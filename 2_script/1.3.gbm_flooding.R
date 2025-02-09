

rm(list = ls())
# HEADER ---------------------------------------------------------------
pacman::p_load(tidyverse, magrittr, data.table, readxl)
pacman::p_load(mlr3verse, iml, DALEX)


library(FSelectorRcpp)
library(praznik)
library(mlr3tuning)
library(parallelMap)


# MODEL ----------------------------------------------------------------


## (0) data ---------------------------------------------------
hs <- read_csv("Data/lightgbm/Book4.csv") %>% dplyr::select(flood)
nomissing <- read_csv("Data/lightgbm/Book4.csv",
                      col_types = "------ddddfdfddddddfdd")
glimpse(hs)
glimpse(nomissing)

# describe(nomissing) %>% round(digits = 2) -> t
# t %>% write.csv("Data/lightgbm/data_description.csv")


# 归一化权重
hs %<>%
  mutate(hs = case_when(flood != 0 ~ 1, flood == 0 ~ 0)) %>%
  mutate(hs = as.factor(hs)) %>%
  # mutate(weights = rescale(flood)) %>%
  dplyr::select(-1) %>% cbind(nomissing)
glimpse(hs)

table(hs$hs) # 239880   1452 
range(hs$weights)



## (1) task -------------------------------------------------
task <- as_task_classif(hs, target = "hs") # for missing data impute and ...
task$set_col_roles("weights", roles = "weight")

# check
task$weights
task$missings()
table(task$truth()) # check original class balance



## (2) engineering -------------------------------------------------


### 1.feature selection ###
drop_features <- c("TWI")
# drop_features <- c("procurvature", "aspect")
task$select(setdiff(task$feature_names, drop_features)) # 删除变量


### 2. undersample majority class ###
prep_under <- po("classbalancing", reference = "major", adjust = "major", 
                 shuffle = F, ratio = 1452 / 239880)

task1 <- prep_under$train(list(task))[[1]]
table(task1$truth())


### 3. factor encoding ###
prep_fac <-
  po("removeconstants", ratio = 0.03) %>>%
  po("collapsefactors", no_collapse_above_prevalence = 0.01) %>>%
  po("encodeimpact", affect_columns = selector_cardinality_greater_than(15)) %>>%
  po("fixfactors") %>>%
  po("imputesample")



## (3) split ---------------------------------------------
set.seed(888)
split <- partition(task1, ratio = 0.7)


## (4) learner ---------------------------------------------
lrn_gbm <- lrn("classif.lightgbm")
graph_learner <- as_learner(prep_fac %>>% lrn_gbm)
# set_threads(graph_learner, n = 10)


## (5) tunning ---------------------------------------------
# lrn_gbm$param_set$ids()

search_space <- ps(
  classif.lightgbm.learning_rate = p_dbl(lower = 0.07, upper = 0.09),
  classif.lightgbm.num_iterations = p_int(lower = 300, upper = 700),
  classif.lightgbm.max_depth = p_int(lower = 3, upper = 5),
  classif.lightgbm.num_leaves = p_int(lower = 2, upper = 31),
  classif.lightgbm.bagging_fraction = p_dbl(lower = 0.7, upper = 1),
  classif.lightgbm.feature_fraction = p_dbl(lower = 0.7, upper = 1),
  classif.lightgbm.lambda_l1 = p_int(lower = 0, upper = 1000),
  classif.lightgbm.lambda_l2 = p_int(lower = 0, upper = 1000)
)

# search_space = ps(
#   learning_rate = p_dbl(lower = 0.07, upper = 0.1),
#   num_iterations = p_int(lower = 300, upper = 700),
#   max_depth = p_int(lower = 3, upper = 5),
#   num_leaves = p_int(lower = 2, upper = 30),
#   bagging_fraction = p_dbl(lower = 0.7, upper = 1),
#   feature_fraction = p_dbl(lower = 0.7, upper = 1),
#   lambda_l1 = p_int(lower = 0, upper = 1000),
#   lambda_l2 = p_int(lower = 0, upper = 1000)
# )


library(future)
library(future.apply)
plan(multisession, workers = 20)

at <- auto_tuner(
  tuner = tnr("grid_search", batch_size = 20),
  learner = graph_learner,
  resampling = rsmp("cv", folds = 5L),
  measure = msr("classif.auc"),
  search_space = search_space,
  term_evals = 100
)
at$train(task1, row_ids = split$train)


# at$tuning_result$learner_param_vals[[1]]
# autoplot(at$tuning_instance, type = "performance")


## (6) train ---------------------------------------------

# tmp = at$tuning_result$learner_param_vals[[1]][c(15:24)]
# names(tmp) = tmp %>% unlist() %>% as.data.frame() %>%
#   row.names.data.frame() %>% str_extract("(?<=lightgbm.).*$")
# lrn_gbm$param_set$values = tmp
# lrn_gbm$train(task, row_ids = split$train)
# lrn_gbm$importance() %>% as.data.frame()


graph_learner$param_set$values <- at$tuning_result$learner_param_vals[[1]]
graph_learner$train(task1, row_ids = split$train)


param_list <- graph_learner$param_set$values[c(11:21)]
param_tibble_hs <- tibble(
  Parameter = names(param_list),
  Value = data.table(param_list)[[1]] %>% unlist()
)



# graph_learner$graph_model$pipeops$
#   classif.lightgbm$learner_model$importance() %>%
#   as.data.frame() %>%
#   rownames_to_column() %>%
#   setnames(c("feature", "value")) -> tmp
# tmp %>%
#   ggplot(aes(x = reorder(feature, value, decreasing = T), 
#              y = value, group = 1)) +
#   geom_point() + geom_line() + theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))


## (7) predict ---------------------------------------------
predictions <- graph_learner$predict(task1, row_ids = split$test)


# graph_learner %>% write_rds("Data/lightgbm/graph_learner_hs.rds")
# graph_learner <- read_rds("Data/lightgbm/graph_learner_hs.rds")



## (8) validation ---------------------------------------------
(predictions$score(msr("classif.auc")) -> cc1)
# classif.auc 
#   0.7976457 
(predictions$score(msr("classif.acc")) -> cc2)
# classif.acc 
#   0.7310345 
(predictions$score(msr("classif.recall")) -> cc3)
# classif.recall 
#      0.6482759
predictions$confusion
#         truth
# response   0   1
#        0 188  54
#        1 102 236

autoplot(predictions, type = "roc") + labs(subtitle = "flood")
ggsave("Output/SpFig/lightgbm/roc_flood.pdf", scale = 2)
# autoplot(predictions, type = "prc")





## (9) newdata ---------------------------------------------
newdata <- task$data()
graph_learner$predict_newdata(newdata)

mypredictprob <- graph_learner$predict_newdata(newdata)
write.csv(
  mypredictprob$prob %>% as_tibble(),
  "data/1-hazard/tmp_dz(new).csv"
)



# FS -------------------------------------------------------------------
### feature selection ###


tsk_hs <- as_task_classif(hs, target = "hs")
tsk_hs$set_col_roles("weights", roles = "weight")
tsk_hs


### Calculating Filter Values ###
flt_gain <- flt("information_gain")
flt_gain$calculate(tsk_hs)
as.data.table(flt_gain)

flt_jmim <- flt("jmim")
flt_jmim$calculate(tsk_hs)
as.data.table(flt_jmim)


### Embedded Methods ###
# lrn_ranger <- lrn("classif.ranger", importance = "impurity")
# flt_importance <- flt("importance", learner = lrn_ranger) # impurity
# flt_importance$calculate(tsk_dz)
# as.data.table(flt_importance)
# autoplot(flt_importance)


lrn_ranger <- lrn("classif.ranger", importance = "permutation")
set_threads(lrn_ranger, n = 5)
flt_importance1 <- flt("importance", learner = lrn_ranger) # permutation
flt_importance1$calculate(tsk_hs)

as.data.table(flt_importance1)
autoplot(flt_importance1) + labs(subtitle = "flood")
ggsave("Output/SpFig/lightgbm/fs_flood.pdf", scale = 2)


# ### Wrapper Methods ###
# library(mlr3fselect)
# set.seed(888)
# split <- partition(tsk_dz, ratio = 0.8)
# tsk_dz$set_row_roles(split$test, "holdout")
# # lrn_ranger = lrn("regr.ranger")
# lrn_gbm <- lrn("classif.lightgbm")
# instance <- fselect(
#   fselector = fs("rfe"), # random_search / rfe / exhaustive_search
#   task = tsk_dz,
#   learner = lrn_gbm,
#   resampling = rsmp("cv", folds = 3),
#   measure = msr("classif.ce"),
#   term_evals = 10,
#   store_models = TRUE
# )
# instance$result # 最佳特征子集
# instance$result_feature_set %>% as.data.frame()
# # fselector$optimize(instance)



## DALEX -------------------------------------


library(DALEX)
library(DALEXtra)

lrn_rf <- lrn("classif.ranger", predict_type = "prob", num.trees = 250)
lrn_rf$train(tsk_dz)
lrn_rf$model

lrn_exp <- explain_mlr3(lrn_rf,
                        data = hs %>% dplyr::select(-c(weights, hs)),
                        y = as.numeric(as.character(hs$hs)),
                        label = "RF", colorize = FALSE
)

fifa_vi <- model_parts(lrn_exp)
plot(fifa_vi, show_boxplots = FALSE)




# DROP -----------------------------------------------------------------

rescale <- function(x, type = "pos") {
  rng <- range(x, na.rm = TRUE)
  switch(type,
         "pos" = 0.5 + ((x - rng[1]) / (rng[2] - rng[1])) * 0.5,
         "neg" = 0.5 + ((rng[2] - x) / (rng[2] - rng[1])) * 0.5
  )
}
rescale <- function(x, type = "pos") {
  rng <- range(x, na.rm = TRUE)
  switch(type,
         "pos" = 0 + ((x - rng[1]) / (rng[2] - rng[1])),
         "neg" = 0 + ((rng[2] - x) / (rng[2] - rng[1]))
  )
}

# # 随机抽样，平衡样本
# dz_0 = hs %>% filter(hs == 0) # 240348
# dz_1 = hs %>% filter(hs != 0) # 8975
#
# set.seed(888)
# dz_new = sample_n(dz_0, nrow(dz_1)) %>% rbind(dz_1)
# table(dz_new$hs)


# feature selection plot
as.data.table(flt_gain) %>% ggplot() +
  geom_point(aes(x = reorder(feature, -score), y = score)) +
  geom_line(aes(x = reorder(feature, -score), y = score, group = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = "features", y = "information gain\n",
    # information gain\n
    # minimal joint mutual information maximization\n
    # impurity-based feature importance\n
    title = "Feature Selection", subtitle = "flooding"
  )
ggsave("Output/SpFig/lightgbm/fs_gain_hs.pdf", scale = 2)


# autoplot(at$tuning_instance,
#          type = "parameter",
#          cols_x = c(
#            "classif.lightgbm.max_depth",
#            "classif.lightgbm.num_leaves"
#          ))




















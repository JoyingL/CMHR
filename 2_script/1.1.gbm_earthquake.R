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
dz <- read_csv("Data/lightgbm/Book4.csv") %>% dplyr::select(earthquake)
nomissing <- read_csv("Data/lightgbm/Book4.csv",
  col_types = "------ddddfdfddddddfdd"
)
glimpse(dz)
glimpse(nomissing)


bruceR::Describe(nomissing) -> t # %>% round(digits = 2)
t$desc %>% write.csv("Data/lightgbm/data_description2.csv")


# plot# 归一化权重
dz %<>%
  mutate(dz = case_when(earthquake != 0 ~ 1, earthquake == 0 ~ 0)) %>%
  mutate(dz = as.factor(dz)) %>%
  mutate(weights = rescale(earthquake)) %>%
  dplyr::select(-1) %>%
  cbind(nomissing)
glimpse(dz)

table(dz$dz) # 232448 / 8884
range(dz$weights)



## (1) task -------------------------------------------------
task <- as_task_classif(dz, target = "dz") # for missing data impute and ...
task$set_col_roles("weights", roles = "weight")

# check
task$weights
task$missings()
table(task$truth()) # check original class balance



## (2) engineering -------------------------------------------------


### 1.feature selection ###
drop_features <- c("aspect")
task$select(setdiff(task$feature_names, drop_features)) # 删除变量


### 2. undersample majority class ###
prep_under <- po("classbalancing",
  id = "undersample", adjust = "major",
  reference = "major", shuffle = FALSE, ratio = 8884 / 232448
)

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


## (5) tunning ---------------------------------------------
# lrn_gbm$param_set$ids()

search_space <- ps(
  classif.lightgbm.learning_rate = p_dbl(lower = 0.05, upper = 0.1),
  classif.lightgbm.num_iterations = p_int(lower = 300, upper = 700),
  classif.lightgbm.max_depth = p_int(lower = 3, upper = 5),
  classif.lightgbm.num_leaves = p_int(lower = 2, upper = 31),
  classif.lightgbm.bagging_fraction = p_dbl(lower = 0.7, upper = 1),
  classif.lightgbm.feature_fraction = p_dbl(lower = 0.7, upper = 1),
  classif.lightgbm.lambda_l1 = p_int(lower = 0, upper = 1000),
  classif.lightgbm.lambda_l2 = p_int(lower = 0, upper = 1000)
)


library(future)
library(future.apply)
plan(multisession, workers = 10)

at <- auto_tuner(
  tuner = tnr("grid_search", batch_size = 30),
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
param_tibble_dz <- tibble(
  Parameter = names(param_list),
  Value = data.table(param_list)[[1]] %>% unlist()
)

writexl::write_xlsx(list("Sheet1" = param_tibble_dz,
                         "Sheet2" = param_tibble_hp,
                         "Sheet3" = param_tibble_hs,
                         "Sheet4" = param_tibble_tf), 
                    "Output/Table/gbm_tunning2.xlsx")


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
# predictions

# graph_learner %>% write_rds("Data/lightgbm/graph_learner_dz.rds")
graph_learner <- read_rds("Data/lightgbm/graph_learner_dz.rds")


## (8) validation ---------------------------------------------
(predictions$score(msr("classif.auc")) -> aa1)
# classif.auc
#     0.89216
(predictions$score(msr("classif.acc")) -> aa2)
# classif.acc
#   0.8165103
(predictions$score(msr("classif.recall")) -> aa3)
# classif.recall
#      0.7846154
predictions$confusion
#         truth
# response    0    1
#        0 2091  404
#        1  574 2261

autoplot(predictions, type = "roc") + labs(subtitle = "Earthquake")
ggsave("Output/SpFig/lightgbm/roc_earthquake.pdf", scale = 2)
# autoplot(predictions, type = "prc")

tribble(
  ~hazard, ~auc, ~acc, ~recall,
  "earthquake", aa1, aa2, aa3,
  "landslide", bb1, bb2, bb3,
  "flooding", bb1, bb2, bb3,
  "cyclone", bb1, bb2, bb3
) %>% write_csv("Output/Table/gbm_validation.csv")




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


tsk_dz <- as_task_classif(dz, target = "dz")
tsk_dz$set_col_roles("weights", roles = "weight")
tsk_dz


### Calculating Filter Values ###
flt_gain <- flt("information_gain")
flt_gain$calculate(tsk_dz)
as.data.table(flt_gain)


# flt_jmim <- flt("jmim")
# flt_jmim$calculate(tsk_dz)
# as.data.table(flt_jmim)


### Embedded Methods ###
# lrn_ranger <- lrn("classif.ranger", importance = "impurity")
# flt_importance <- flt("importance", learner = lrn_ranger) # impurity
# flt_importance$calculate(tsk_dz)
# as.data.table(flt_importance)
# autoplot(flt_importance)


lrn_ranger <- lrn("classif.ranger", importance = "permutation")
set_threads(lrn_ranger, n = 4)
flt_importance1 <- flt("importance", learner = lrn_ranger) # permutation
flt_importance1$calculate(tsk_dz)

as.data.table(flt_importance1)
autoplot(flt_importance1) + labs(subtitle = "earthquake")
ggsave("Output/SpFig/lightgbm/fs_earthquake.pdf", scale = 2)


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
  data = dz %>% dplyr::select(-c(weights, dz)),
  y = as.numeric(as.character(dz$dz)),
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


# # 随机抽样，平衡样本
# dz_0 = dz %>% filter(dz == 0) # 240348
# dz_1 = dz %>% filter(dz != 0) # 8975
#
# set.seed(888)
# dz_new = sample_n(dz_0, nrow(dz_1)) %>% rbind(dz_1)
# table(dz_new$dz)


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
    title = "Feature Selection", subtitle = "earthquake"
  )
ggsave("Output/SpFig/lightgbm/fs_gain_dz.pdf", scale = 2)



# autoplot(at$tuning_instance,
#          type = "parameter",
#          cols_x = c(
#            "classif.lightgbm.max_depth",
#            "classif.lightgbm.num_leaves"
#          ))

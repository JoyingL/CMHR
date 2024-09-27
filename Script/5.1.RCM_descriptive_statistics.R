
rm(list = ls())
# STATS ----------------------------------------------------------------
pacman::p_load(data.table, tidyverse, readxl, writexl, magrittr, naniar)
pacman::p_load(paletteer, ggthemes, patchwork, ggrepel)

library(hrbrthemes)
library(ggforce)
library(urbnthemes)
set_urbn_defaults(style = "print")
library(extrafont)
loadfonts(device = "win", quiet = F)

# library(ggstatsplot)
# urbnthemes::lato_import()




### descriptive statistics ###

RCM <- read_rds("Data/final/MHS_final_comm.rds")
glimpse(RCM)

library(psych)
psych::describe(RCM)
describe(RCM[,2:4]) %>% t() %>% round(digits = 2)

library(epiDisplay)
tableStack(
  dataFrame = data.frame(RCM),
  c(2),
  by = GNS, 
  total.column = T
) -> table1
write.csv(table1, "Output/Tab/descrip_stats_RCM.csv")




# SP -------------------------------------------------------------------


ggplot(RCM)


### filter a quantile ###
ggplot(RCM, aes(R.mean), alpha = .7) + geom_histogram(binwidth = 0.005)
ggplot(RCM, aes(C.mean), alpha = .7) + geom_histogram(binwidth = 0.005)


top10_max <- RCM %>% slice_max(R.mean, n = 10)
top10_min <- RCM %>% slice_min(R.mean, n = 10)
top10_all <- bind_rows(top10_max, top10_min)

top10_max %>% 
  ggplot(aes(x = reorder(COMM, -R.mean), y = R.mean)) + 
  geom_bar(stat = "identity")







# # Fig.2a ----------------------------------------------------

### standard deviation of size ###


dt_test <- RCM %>% filter(GNS != "GS+N") %>%
  mutate(GNS = factor(GNS, levels = c("GS", "GN")))

dt_test %>% group_by(GNS) %>% summarize(Count = n(),
    Mean_Size = mean(n), SD_Size = sd(n), Var_Size = var(n),
    Min_Size = min(n), Max_Size = max(n)) # -> group_summary


## Computes Levene's test for homogeneity of variance across groups.
car::leveneTest(n ~ GNS, data = dt_test)


## direct calculate
val.1 <- dt_test %>% filter(GNS == "GN") %>% mutate(group = 1)
val.2 <- dt_test %>% filter(GNS == "GS") %>% mutate(group = 2)
(t.1 = sd(val.1$n))
(t.2 = sd(val.2$n))
# (t.1 = median(val.1))
# (t.2 = median(val.2))
(obs_diff <- (t.2 - t.1) / t.1 * 100) # [1] 33.6803


## bootstrap function
library(boot)
set.seed(888)

boot_diff <- function(data, indices, var, group, fun, type = 1) {
  d = data[indices, ]
  groups = unique(d[[group]])
  
  val.1 = d[[var]][d[[group]] == groups[1]]
  val.2 = d[[var]][d[[group]] == groups[2]]
  
  t.1 <- fun(val.1)
  t.2 <- fun(val.2)
  
  if (type == 1) {
    return((t.1 - t.2) / t.2 * 100)
  } else {
    return((t.2 - t.1) / t.1 * 100)
  }
}
boot_results <- boot(
  data = rbind(val.1, val.2),
  statistic = boot_diff,
  strata = rbind(val.1, val.2)$group,
  group = "group", # 1 - GN, 2 - GS
  R = 10000,
  var = "n",
  fun = sd,
  type = 2 # 1 大
)
boot_results
boot.ci(boot_results, conf = 0.95, type = "perc")




## permutation test
obs_diff_sd <- boot_results$t0

permute_diff_cv <- function(data) {
  shuffled_GNS = sample(data$GNS)
  GN_sample = data$n[shuffled_GNS == "GN"]
  GS_sample = data$n[shuffled_GNS == "GS"]

  GN_CV = sd(GN_sample)
  GS_CV = sd(GS_sample)

  (GS_CV - GN_CV) / GN_CV * 100
}

perm_results = replicate(10000, permute_diff_cv(dt_test))
p_value = mean(abs(perm_results) >= abs(obs_diff_sd))







# DROP -----------------------------------------------------------------



# 3. Visualization
# ggplot(dt_test, aes(x = GNS, y = n)) + geom_boxplot()


t.test(n ~ GNS, data = dt_test)
wilcox.test(n ~ GNS, data = dt_test)
kruskal.test(n ~ GNS, data = dt_test)






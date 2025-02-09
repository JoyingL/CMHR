

rm(list = ls())
# HEADER ---------------------------------------------------------------
pacman::p_load(data.table, tidyverse, readxl, writexl, magrittr, naniar)


library(MASS)
library(Matrix)
# library(progressr)
# handlers(global = TRUE)
library(progress)


# LOAD -----------------------------------------------------------------


gaul <- read_rds("Data/final/MHS_final_tess.rds") # 183,563
RCM <- read_rds("Data/final/MHS_final_comm.rds") # 916
RCM_exp <- read_rds("Data/final/MHS_final_comm_expand_ISO.rds") # 1,113


# length(unique(gaul$ISO3)) # 129
# length(unique(RCM_exp$ISO3)) # 129
# RCM %>% filter(wop != 0) # 901 × 12


mycountry <- read_rds("Data/studyarea/mycountry.rds") # 127
mycountry %<>% filter(ISO3 != "PSE") # PSE



# MAHA -----------------------------------------------------------------


## construct a matrix
# tibble(ISO3 = mycountry$ISO3) -> ISO #  127^2 = 16129
tibble(ISO3 = mycountry$ISO3) -> ISO #  126^2 = 15876

## all possible country pairs
ISO_pairs <- expand.grid(from = ISO$ISO3, to = ISO$ISO3) %>% tibble()

# ## the covariance matrix 
# (df <- tibble(R = RCM$R.mean, C = RCM$C.mean))
# cov_matrix <- cov(df)
# inv_cov_matrix <- solve(cov_matrix)

## Function: takes a row and returns the min of calculated Maha distance
calc_mahalanobis <- function(row) {
  tryCatch({
    
    # extract data corresponding to the specific country
    cou1 <- RCM_exp %>% filter(ISO3 == row[[1]])
    cou2 <- RCM_exp %>% filter(ISO3 == row[[2]])
    
    # initialize the result matrix
    result_matrix <- matrix(nrow = nrow(cou1), ncol = nrow(cou2))
    
    # caculate the Mahalanobis Distance
    cou1_dt = cou1[c("R.mean", "C.mean")]
    cou2_dt = cou2[c("R.mean", "C.mean")]
    
    for (i in 1:nrow(cou1_dt)) {
      for (j in 1:nrow(cou2_dt)) {
        diff = (cou1_dt[i, ] - cou2_dt[j, ]) %>% t() # 2×1
        result_matrix[i, j] = sqrt(t(diff) %*% inv_cov_matrix %*% diff)
      } # 1×2 * 2×2 * 2×1
    }
    
    # # the average similarity
    # return(average_min = mean(result_matrix, na.rm = TRUE))

    # minimal distance (the most similar RCM pairs)
    row_mines = apply(result_matrix, 1, min, na.rm = TRUE)

    # average for two nations (all constituent RCMs)
    average_min = mean(row_mines, na.rm = TRUE)
    
  }, error = function(e) {
    return(NA)
  })
}

## the progress bar and calculate results
pb <- progress_bar$new(
  format = "[:bar] :current/:total (:percent) Elapsed: :elapsedfull ETA: :eta",
  total = nrow(ISO_pairs), 
  width = 70
)

sim <- apply(ISO_pairs, 1, function(row) {
  result <- calc_mahalanobis(row)
  pb$tick() # 更新进度条
  return(result)
})


# length(sim) # [1] 15876
# ISO_pairs$sim = sim
# miss_var_summary(ISO_pairs)
# # ISO_pairs %>% write_rds("Data/sim/my_maha_sim.rds")
ISO_pairs <- read_rds("Data/sim/my_maha_sim.rds")
# ISO_pairs %>% filter(is.na(sim))

# range(ISO_pairs$sim)
# # 定义区间函数
# define_interval <- function(x) {
#   cut(x, breaks = seq(0, 6, by = 0.2), include.lowest = TRUE, right = FALSE)
# }
# # 计算每个区间的数据分布
# interval_distribution <- ISO_pairs %>%
#   mutate(interval = define_interval(sim)) %>%
#   group_by(interval) %>%
#   summarise(count = n()) %>%
#   mutate(
#     percentage = count / sum(count) * 100,
#     cumulative_percentage = cumsum(percentage)
#   ) %>%
#   arrange(desc(interval))
# # 可视化
# ggplot(interval_distribution, aes(x = interval, y = percentage)) +
#   geom_bar(stat = "identity", fill = "skyblue") +
#   geom_text(aes(label = sprintf("%.1f%%", percentage)),
#             vjust = -0.5, size = 3) +
#   labs(title = "Distribution of my_maha_sim by Intervals",
#        x = "Interval",
#        y = "Percentage of Data") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))




# # 计算马氏距离但取pairwise minimal极值
# length(sim) # [1] 15876
# ISO_pairs$sim = sim
# miss_var_summary(ISO_pairs)
# ISO_pairs %>% write_rds("Data/sim/my_maha_mini_sim.rds")
ISO_pairs0 <- read_rds("Data/sim/my_maha_mini_sim.rds")
hist((ISO_pairs0$sim))


# ## 计算马氏距离但取pairwise mean平均
# length(sim) # [1] 15876
# ISO_pairs$sim = sim
# miss_var_summary(ISO_pairs)
# ISO_pairs %>% write_rds("Data/sim/my_maha_mean_sim.rds")
ISO_pairs1 <- read_rds("Data/sim/my_maha_mean_sim.rds")
ISO_pairs1 %<>% mutate(sim1 = if_else(from == to, 0, sim))
ISO_pairs1 %<>% dplyr::select(-sim)


## 计算闵可夫斯基距离
calc_minkowski <- function(row, p = 2) {
  tryCatch({
    # extract data corresponding to the specific country
    cou1 <- RCM_exp %>% filter(ISO3 == row[[1]])
    cou2 <- RCM_exp %>% filter(ISO3 == row[[2]])
    
    # initialize the result matrix
    result_matrix <- matrix(nrow = nrow(cou1), ncol = nrow(cou2))
    
    # calculate the Minkowski Distance
    cou1_dt = cou1[c("R.mean", "C.mean")]
    cou2_dt = cou2[c("R.mean", "C.mean")]
    
    for (i in 1:nrow(cou1_dt)) {
      for (j in 1:nrow(cou2_dt)) {
        diff = abs(cou1_dt[i, ] - cou2_dt[j, ]) # 2x1 difference vector
        result_matrix[i, j] = (sum(diff^p))^(1/p)
      }
    }
    
    # minimal distance (the most similar RCM pairs)
    row_mines = apply(result_matrix, 1, min, na.rm = TRUE)
    
    # average for two nations (all constituent RCMs)
    average_min = mean(row_mines, na.rm = TRUE)
    
  }, error = function(e) {
    return(NA)
  })
}
sim <- apply(ISO_pairs, 1, function(row) {
  result <- calc_minkowski(row)
  pb$tick() # 更新进度条
  return(result)
})
# length(sim) # [1] 15876
# ISO_pairs$sim = sim
# miss_var_summary(ISO_pairs)
# ISO_pairs %>% write_rds("Data/sim/my_eudic_mini_sim.rds")
ISO_pairs2 <- read_rds("Data/sim/my_eudic_mini_sim.rds")
ISO_pairs2 %<>% mutate(sim2 = if_else(from == to, 0, sim))
ISO_pairs2 %<>% dplyr::select(-sim)


library(ggstatsplot)
ISO_pairs <- ISO_pairs0 %>% left_join(ISO_pairs1)
ISO_pairs <- ISO_pairs0 %>% left_join(ISO_pairs2)
ggcorrmat(ISO_pairs, type = "np")


ISO_pairs <- ISO_pairs %>% 
  pivot_longer(cols = c(sim))
ISO_pairs %>% ggwithinstats(x = from, y = sim1, y1 = sim1, type = "robust")
extract_stats(p)


# t.test(ISO_pairs0$sim, ISO_pairs1$sim, paired = TRUE)
# wilcox.test(ISO_pairs0$sim, ISO_pairs1$sim, paired = TRUE)
cor(ISO_pairs0$sim, ISO_pairs1$sim, method = "spearman") # [1] 0.8475503






# STATS --------------------------------------------------------------
### compare with the traditional taking average measure ###


mysim <- read_rds("Data/sim/my_maha_sim.rds")
sd(mysim$sim) -> s1 # [1] 0.9100305


tmp <- gaul %>% filter(!is.na(ISO3)) %>%
  group_by(ISO3) %>%
  summarise(R = mean(risk), C = mean(GearyC))

gaul_matrix <- tmp %>% dplyr::select(R, C) %>% as.matrix()
dist(gaul_matrix, method = "euclidean") %>% as.matrix() -> gaul_dist

rownames(gaul_dist) <- tmp$ISO3
colnames(gaul_dist) <- tmp$ISO3

dist_long <- reshape2::melt(gaul_dist) %>% tibble()
colnames(dist_long) <- c("from", "to", "distance")

dist_long %<>% filter(from %in% mysim$from & to %in% mysim$to)
dist_long$distance %>% sd() -> s2 # sd [1] 0.1782756

s1 # [1] 0.9100305
s2 # [1] 0.1782756
(s1-s2) / s1 # [1] 0.804099 # 80% lower than me
(s1-s2) / s2 # [1] 4.104627


boot_data <- data.frame(x = mysim$sim, y = dist_long$distance)
boot_result <- boot(
  data = boot_data, statistic = function(data, indices) {
    diff_ratio(data$x[indices], data$y[indices], median)
  }, R = 10000)
boot_result$t0 # [1] 0.8045924
boot.ci(boot_result, type = "perc")


library(ggstatsplot)
long_data <- pivot_longer(boot_data, cols = everything(),
                          names_to = "variable", values_to = "value")
describe(boot_data) %>% round(digits = 3) -> table1
write.csv(table1, "Output/Table/descrip_stats_RPS.csv")

# tableStack(
#   dataFrame = data.frame(long_data),
#   c(1:2),
#   by = variable,
#   total.column = T
# ) -> table1
# write.csv(table1, "Output/Table/descrip_stats_RPS.csv")

# ggbetweenstats(type = "np",
#   data = long_data,
#   x = variable,
#   y = value,
#   plot.type = "violin",
#   pairwise.comparisons = TRUE,
#   pairwise.display = "significant",
#   ggtheme = theme_minimal(),
#   point.args = list(
#     position = ggplot2::position_dodge2(width = 0.5), 
#     alpha = 0.3, size = 0.7, stroke = 0, na.rm = TRUE))
 



### comparing MEDIAN with the taking average method ###

dist_long %>% 
  filter(from %in% mysim$from & to %in% mysim$to) %>% 
  mutate(distance = rescale(distance, type = 2)) %>% 
  mutate_at(vars(distance), ~ ifelse(from == to, NA, .)) %>% 
  pull(distance) -> tmp


# median(tmp, na.rm = T) # [1] 0.6352572
# median(mypairs_res$simi_scale, na.rm = T) # [1] 0.5084894
# (0.6352572-0.5084894)/0.5084894 # [1] 0.2493027 # Me 25% lower than


library(boot)
boot_data <- data.frame(x = tmp, y = mypairs_res$simi_scale)
boot_result <- boot(
  data = boot_data, statistic = function(data, indices) {
    diff_ratio(data$x[indices], data$y[indices], median)
  }, R = 10000)

# boot_result$t0 # [1] 0.2493028
# boot.ci(boot_result, type = "perc")

boot_result$t0 # [1] 0.1995535
boot.ci(boot_result, type = "perc")


















# DROP -----------------------------------------------------------------


diff_ratio <- function(x, y, fun) {
  v1 = fun(x, na.rm = TRUE)
  v2 = fun(y, na.rm = TRUE)
  (v1 - v2) / v1 # (v1 - v2) / v2
}




row = country_pairs[2,]
cou1 = RCM_exp %>% filter(ISO3 == row[[1]])
cou1_dt = cou1[c("R.mean", "C.mean")]
cou2 = RCM_exp %>% filter(ISO3 == row[[2]])
cou2_dt = cou2[c("R.mean", "C.mean")]

for (i in 1:nrow(cou1_dt)) {
  for (j in 1:nrow(cou2_dt)) {
    diff = (cou1_dt[i, ] - cou2_dt[j, ]) %>% t() # 2×1
    result_matrix[i, j] = sqrt(t(diff) %*% inv_cov_matrix %*% diff)
  } # 1×2 * 2×2 * 2×1
}






# sd(tmp, na.rm = TRUE) # [1] 0.2324214
# sd(mypairs_res$simi_scale, na.rm = TRUE) # [1] 0.2440192






























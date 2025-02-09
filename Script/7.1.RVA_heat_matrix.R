

rm(list = ls())
# HEADER ---------------------------------------------------------------
pacman::p_load(data.table, tidyverse, readxl, writexl, magrittr, naniar)
pacman::p_load(ggthemes, patchwork, paletteer, ggrepel)


library(MASS)
library(Matrix)
library(progressr)


RCM_exp <- read_rds("Data/final/MHS_final_comm_expand_ISO.rds")


# LOAD -----------------------------------------------------------------


mycountry <- read_rds("Data/studyarea/mycountry.rds") # 127
mycountry[c(3,5)]

# ISO_pairs %>% write_rds("Data/sim/my_maha_sim.rds")
# ISO_pairs <- read_rds("Data/sim/my_maha_sim.rds") # 127*127 = 16129
ISO_pairs0 <- read_rds("Data/sim/my_maha_mini_sim.rds") # 15,876 × 3
names(ISO_pairs0) <- c("adm0.x", "adm0.y", "simi")

## voting
myagree <- read_rds("Data/sim/voting/agree_newnew.rds") %>% tibble()
names(myagree) <- c("adm0.x", "adm0.y", "voting")

## biblio
mybiblio <- read_rds("Data/sim/biblio/Bibliometric_sim.rds")
names(mybiblio) <- c("adm0.x", "adm0.y", "bibli")


mybiblio %>% filter(adm0.x == "SGP" & adm0.y == "BRA")
mybiblio %>% filter(adm0.x == "BRA" & adm0.y == "SGP")


## sort alphabetically
myrisk <- ISO_pairs0 %>% 
  mutate(across(1:2, as.character)) %>% 
  arrange(adm0.x, adm0.y) # 16,129 × 3



# JOIN -----------------------------------------------------------------

(mypairs <- myrisk %>%
   left_join(myagree, by = c("adm0.x", "adm0.y")) %>%
   left_join(mybiblio, by = c("adm0.x", "adm0.y")))

# ## PSE Gaza Strip Vote all = NA remove
# mypairs %<>% filter(adm0.x != "PSE" & adm0.y != "PSE")
# mypairs %<>% filter(adm0.x != "COM" & adm0.y != "COM")
# # mycountry %>% filter(ISO3 == "PSE")

miss_var_summary(mypairs[-c(1:2)])
sapply(mypairs[-c(1:2)], range, na.rm = T)


## processing: biblio == 0 → NA | adm0.x == adm0.y → NA
mypairs %<>% 
  mutate(bibli = ifelse(bibli == 0, NA, bibli)) %>% # bibli 0 就是NA
  mutate_at(vars(simi, voting, bibli), ~ ifelse(adm0.x == adm0.y, NA, .))


## join GN GS tag
mypairs %<>% 
  left_join(mycountry[c(3,5)], by = c("adm0.x" = "ISO3")) %>%
  rename(NS_tag.x = NS_tag) %>%
  left_join(mycountry[c(3,5)], by = c("adm0.y" = "ISO3")) %>%
  rename(NS_tag.y = NS_tag) %>% mutate(
    NS_tag.x = factor(NS_tag.x, levels = c("GS", "GN")),
    NS_tag.y = factor(NS_tag.y, levels = c("GS", "GN"))
  ) %>% arrange(NS_tag.x, NS_tag.y)
# mypairs %>% write_rds("Data/sim/my_pairs_all.rds")
# mypairs %>% write_rds("Data/sim/my_pairs_all_126.rds")


## normalization
rescale <- function(x, type = 1) {
  # check if all elements are NA
  if (all(is.na(x))) { return(rep(NA, length(x))) }
  # check range
  rng = range(x, na.rm = TRUE)
  if (rng[2] - rng[1] == 0) { return(rep(NA, length(x))) }
  # transformation
  if (type == 1) {
    (x - rng[1]) / (rng[2] - rng[1])
  } else {
    (rng[2] - x) / (rng[2] - rng[1])
  }
}

mypairs %>% group_by(adm0.x) %>%
  mutate(
    simi_scale = rescale(simi, type = 2), # 逆指标正向化 越小越相似
    vote_scale = rescale(voting),
    bibi_scale = rescale(bibli)
  ) -> mypairs_plot

# mypairs_plot %>% write_rds("Data/final/mypairs_126_01标准化.rds")

# mypairs_plot %>% write_rds("Data/sim/my_pairs_all_rescale.rds")
# mypairs_plot %>% write_rds("Data/sim/my_pairs_all_126_rescale.rds")
# mypairs_plot <- read_rds("Data/sim/my_pairs_all_rescale.rds")

# mypairs_plot %>% write_rds("Data/sim/my_pairs_all_rescale_new.rds")
# mypairs_plot <- read_rds("Data/sim/my_pairs_all_rescale_new.rds")
# mypairs_plot <- read_rds("Data/sim/my_pairs_all_126_rescale.rds") # 达咩！！！



# RPS PLOT -------------------------------------------------------------

library(ggprism)

mypairs_res <- read_rds("RVA_01标准化.rds")

# mypairs_tag %>% filter(simi_scale > 0.4) %>% nrow()
# mypairs_tag %>% filter(simi_scale > 0.5) %>% nrow()


# 定义函数来计算给定阈值下的数量
count_above_threshold <- function(data, threshold) {
  data %>% filter(simi_scale > threshold) %>% nrow()
}

# 创建阈值序列，从0到1，步长为0.01
thresholds <- seq(0, 1, by = 0.05)

# 计算每个阈值下的数量
counts <- sapply(thresholds, function(t) count_above_threshold(mypairs_tag, t))

# 计算 75 % 分位数
q75 <- quantile(mypairs_res$simi_scale, probs = c(0.75))[1]
count_at_q75 <- count_above_threshold(mypairs_res, q75)

# # 计算数量的变化
# count_changes <- diff(counts)
# # 找到变化最大的点
# max_change_index <- which.max(abs(count_changes))
# inflection_point <- thresholds[max_change_index + 1]  # +1 因为diff减少了一个元素
# # 创建结果数据框
# results <- data.frame(
#   threshold = thresholds[-1],  # 去掉第一个阈值，因为diff减少了一个元素
#   count = counts[-1],
#   change = count_changes
# )
# # 打印拐点和相关信息
# cat("拐点阈值:", inflection_point, "\n")
# cat("在此阈值下的数量:", counts[max_change_index + 1], "\n")
# cat("最大变化量:", count_changes[max_change_index], "\n")


## 折线图 ##
results <- data.frame(threshold = thresholds, count = counts)

ggplot(results, aes(x = threshold, y = count)) +
  # geom_line(color = "#b35c90") +
  geom_smooth(color = "#041a92", method = "loess", se = FALSE, linewidth = 1) +
  geom_point(color = "#0090a7", size = 2) +
  geom_vline(xintercept = q75, linetype = "dashed", color = "#a0a4fe") +
  annotate("text", x = q75, y = max(counts), 
           label = sprintf("75th Percentile: %.2f", q75),
           vjust = -1, hjust = 1.1, color = "#942193") +
  annotate("point", x = q75, y = count_at_q75, 
           color = "#942193", size = 3, shape = 15) +
  scale_x_continuous(
    limits = c(0, 1), breaks = seq(0, 1, by = 0.1),
    minor_breaks = seq(0, 1, by = 0.02),
    guide = "prism_minor"
  ) +
  scale_y_continuous(
    limits = c(0, max(results$count) * 1.1),
    breaks = seq(0, max(results$count), by = 3000),
    guide = "prism_offset"
  ) +
  scale_shape_prism() +
  theme_prism(palette = "winter_bright", base_size = 16) +
  labs(x = "Threshold", y = "Number of pairs") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(angle = 90, vjust = 2),
        axis.title.x = element_text(vjust = -2))
ggsave("Output/SpFig/RPS_distribution_line2.pdf", height = 5, width = 7)

# y_max <- max(c(max(results$count), 10^4)) * 1.1
# major_breaks <- 10^seq(0, 6, 1)
# minor_breaks <- unlist(lapply(major_breaks, function(x) x * 1:9))

# 定义区间函数
define_interval <- function(x) {
  cut(x, breaks = seq(0, 1, by = 0.05), include.lowest = TRUE, right = FALSE)
}

# 计算每个区间的数据分布
interval_distribution <- mypairs_tag %>%
  mutate(interval = define_interval(simi_scale)) %>%
  group_by(interval) %>%
  summarise(count = n()) %>%
  mutate(
    percentage = count / sum(count) * 100,
    cumulative_percentage = cumsum(percentage)
  ) %>%
  arrange(interval)

# 找到75%分位数所在的区间
q75_interval <- interval_distribution %>%
  filter(cumulative_percentage >= 75) %>%
  slice(1) %>% pull(interval)


## 柱状图 ##
ggplot(interval_distribution, aes(x = interval, y = percentage)) +
  geom_rect(aes(xmin = q75_interval, xmax = Inf, ymin = 0, ymax = Inf),
            fill = "lightyellow", alpha = 0.3) +
  geom_bar(stat = "identity", fill = "#976fae") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5, size = 3) +
  annotate("text", x = q75_interval, y = max(interval_distribution$percentage),
           label = "75% Quantile", hjust = 1, vjust = 1, color = "red") +
  labs(title = "Distribution of RPS by intervals",
       subtitle = paste("75% of data falls within the highlighted area"),
       x = "Interval", y = "Percentage of Data") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Output/SpFig/RPS_distribution_bar.pdf", height = 5, width = 8)


## 密度图 ##
quantiles <- quantile(mypairs_tag$simi_scale, probs = c(0.2, 0.5, 0.8))
ggplot(mypairs_tag, aes(x = simi_scale)) +
  geom_density(fill = "skyblue", alpha = 0.7) +
  geom_rug(alpha = 0.01) +
  geom_vline(xintercept = quantiles[1], color = "red", linetype = "dashed") +
  geom_vline(xintercept = quantiles[2], color = "green", linetype = "dashed") +
  geom_vline(xintercept = quantiles[3], color = "blue", linetype = "dashed") +
  annotate("text", x = quantiles[1], y = 0, label = "20%", color = "red", 
           vjust = -0.5, hjust = 1.1, angle = 90) +
  annotate("text", x = quantiles[2], y = 0, label = "50%", color = "green", 
           vjust = -0.5, hjust = 1.1, angle = 90) +
  annotate("text", x = quantiles[3], y = 0, label = "80%", color = "blue", 
           vjust = -0.5, hjust = 1.1, angle = 90) +
  labs(title = "Density Distribution of simi_scale",
       subtitle = paste("20% quantile:", round(quantiles[1], 2),
                        "| 50% quantile (median):", round(quantiles[2], 2),
                        "| 80% quantile:", round(quantiles[3], 2)),
       x = "value", y = "Density") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> p
# ggsave("Output/Fig/density_RPS.pdf", p, width = 4, height = 3, scale = 1.2)

library(e1071)
skewness(mypairs_tag$simi_scale)
mean(mypairs_tag$simi_scale)
median(mypairs_tag$simi_scale)










# HEATMAP --------------------------------------------------------------

mypairs_plot <- read_rds("Data/sim/my_pairs_all_126_rescale.rds")
mypairs_plot <- read_rds("Data/final/mypairs_126_01标准化.rds")

mypairs_plot %>% 
  # ggplot(aes(x = adm0.y, y = adm0.x, fill = (simi_scale))) +
  ggplot(aes(x = adm0.y, y = adm0.x, fill = (simi_scale))) +
  geom_tile(color = "#f3f3f3", show.legend = T) +
  coord_equal() +
  scale_x_discrete(limits = unique(mypairs_plot$adm0.x) %>% rev(), position = "top") +
  scale_y_discrete(limits = unique(mypairs_plot$adm0.y)) +
  scale_fill_paletteer_c(
    na.value = "white", 
    palette = "grDevices::Spectral", direction = -1,
    trans = "exp", # risk agree biblio
    guide = guide_colorbar(
      barheight = unit(3, units = "mm"),
      barwidth = unit(70, units = "mm"),
      ticks.colour = "transparent")) + 
  labs(x = NULL, y = NULL) + 
  theme_void() +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1.5, size = 10),
    axis.text.y = element_text(angle = 0, hjust = 1, size = 10),
    legend.text = element_text(size = 12),
    legend.position = "top"
  )  # -> myplot


ggsave("Output/SpFig/heatmap_RPS_126.pdf", width = 20, height = 22)
ggsave("Output/SpFig/heatmap_PVP_126.pdf", width = 20, height = 22)
ggsave("Output/SpFig/heatmap_ACI_126.pdf", width = 20, height = 22)
# ggsave("Output/SpFig/heatmap_ACI.pdf", width = 20, height = 22)






# DROP -----------------------------------------------------------------


rescale <- function(x, type = 1) {
  rng <- range(x, na.rm = TRUE)
  if (type == 1) {
    (x - rng[1]) / (rng[2] - rng[1])
  } else {
    (rng[2] - x) / (rng[2] - rng[1])
  }
}

rescale <- function(x, type = 1) {
  # check if all elements are NA
  if (all(is.na(x))) { return(rep(NA, length(x))) }
  # check range
  rng = range(x, na.rm = TRUE)
  if (rng[2] - rng[1] == 0) { return(rep(NA, length(x))) }
  # transformation
  if (type == 1) {
    (x - rng[1]) / (rng[2] - rng[1])
  } else {
    (rng[2] - x) / (rng[2] - rng[1])
  }
}


library(VGAM)
library(bestNormalize)

myboxcox <- function(x) {
  tmp <- x
  obj <- yeojohnson(tmp)
  p <- predict(obj)
  return(p)
}


rescale <- function(x, type = 1) {
  # check if all elements are NA
  if (all(is.na(x))) { return(rep(NA, length(x))) }
  # check range
  rng = range(x, na.rm = TRUE)
  
  if (rng[2] - rng[1] == 0) { return(rep(NA, length(x))) }
  
  if (type == 1) {
    # positive indicators
    transformed_x <- myboxcox(x)
  } else if (type == 2) {
    # negative indicators
    transformed_x <- myboxcox(-x)
  } else {
    stop("Invalid type")
  }
  # normalization
  rng = range(transformed_x, na.rm = TRUE)
  (transformed_x - rng[1]) / (rng[2] - rng[1])
}


# mypairs_plot %>% filter(adm0.y == "COM")
# RCM_exp %>% filter(ISO3 == "COM")


# tess_valid_rm %>% dplyr::select(1) %>% 
#   inner_join(gaul, by = "GRID_ID") %>% 
#   write_rds("Data/final/MHS_final_tess_shp.rds")
mytess <- read_rds("Data/final/MHS_final_tess_shp.rds")
glimpse(mytess)


mapview(filter(mytess, ISO3 == "COM"))
filter(RCM, ISO3 == "COM")






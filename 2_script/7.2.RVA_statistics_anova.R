
rm(list = ls())
# HEADER ---------------------------------------------------------------
pacman::p_load(data.table, tidyverse, readxl, writexl, magrittr, naniar)
pacman::p_load(ggthemes, patchwork, paletteer, ggrepel)


library(psych)
library(epiDisplay)
library(ggstatsplot)

library(boot)
set.seed(886) # 再见吧您

library(urbnthemes)
set_urbn_defaults(style = "print")
urbnthemes::lato_import()

library(extrafont)
loadfonts(device = "win", quiet = F)


library(FSA)



# LOAD -----------------------------------------------------------------


# mypairs <- read_rds("Data/sim/my_pairs_all.rds")
mypairs <- read_rds("Data/sim/my_pairs_all_126.rds")
mypairs %<>% mutate(NS_tag = paste(NS_tag.x, "-", NS_tag.y))



# mypairs_res <- read_rds("Data/sim/my_pairs_all_rescale.rds")
mypairs_res <- read_rds("Data/sim/my_pairs_all_126_rescale.rds")
mypairs_res %<>% mutate(NS_tag = paste(NS_tag.x, "-", NS_tag.y)) %>% ungroup()
mypairs_res %<>% filter(adm0.x != adm0.y) # 15,876-126 = 15,750


glimpse(mypairs_res)
mypairs_res[c(8:11)]
miss_var_summary(mypairs_res)




# ANALYSIS -------------------------------------------------------------


describe(mypairs[c(3:5)]) %>% t() %>% round(digits = 3)

(mypairs_res[c(5:8)] -> dt_test)

tableStack(
  dataFrame = data.frame(dt_test),
  c(1:3),
  by = NS_tag, 
  total.column = T
) -> table1
# write.csv(table1, "Output/Table/descrip_stats_mypairs2.csv")




### anova: GS-GS GN-GN GS-GN GN-GS 4 boxplot ### -------------------------

(mypairs_res[c(5:8)] -> dt_test)
library(ggstatsplot)
ggbetweenstats(
  data = dt_test, x = NS_tag,  y = simi_scale,
  point.args = list(
    position = ggplot2::position_jitterdodge(dodge.width = 0.7), 
    alpha = 0.3, size = 1, stroke = 0, na.rm = TRUE
    ),
  p.adjust.method = "none",
  type = "np",  # 使用参数检验，不进行贝叶斯检验
  xlab = "GN - GS pairwise relationships", 
  ylab = "GearyC",
  title = "Comparision of RPS across regions") +
  theme(text = element_text(size = 12))
ggsave("Output/EDFig/RPS_GNS_anova.pdf", width = 10, height = 5)


pairwise_res <- pairwise.t.test(dt_test$simi_scale, dt_test$NS_tag)
pairwise_pvals <- pairwise_res$p.value
pairwise_sig <- pairwise_res$p.value < 0.05

kruskal_res <- kruskal.test(simi_scale ~ NS_tag, data = dt_test)
dunn_res <- dunnTest(simi_scale ~ NS_tag, data = dt_test, method = "bonferroni")




### box plot ### -------------------------------------

# mypairs_bc <- read_rds("Data/sim/my_pairs_all_126_rescale.rds")
mypairs_res <- read_rds("RVA_01标准化.rds")
# mypairs_res <- read_rds("RVA_01标准化_TAG_UND.rds")


mypairs_long <- mypairs_res %>% filter(bibi_scale != 0) %>% 
  pivot_longer(cols = c(simi_scale, vote_scale, bibi_scale),
               names_to = "variable",
               values_to = "value")
ggplot(mypairs_long, aes(x = value, y = NS_tag, fill = variable)) +
  # geom_violin(scale = "width", trim = TRUE) +
  geom_boxplot(position = position_dodge(width = 0.5), width = 0.3) +
  labs(x = "Segregation index", y = NULL, fill = NULL) +
  theme(legend.position = "top", text = element_text(size = 12))

# ggsave("Output/EDFig/RVA_GNS_boxplot(new).pdf", width = 5, height = 3, scale = 2.2)


library(ggpubr)
library(gghalves)
library(ggrepel)
"#669966" "#6699CC"
"#4F71BE" "#C863A1"
"#ED7B75" "#F8CF7A"
mycolor <- c("#6699CC", "#C863A1", "#82cbc8")
ggplot(mypairs_long, aes(x = NS_tag, y = value, 
                         fill = variable, color = variable)) +
  # geom_half_point_panel(aes(color = variable), side = "l",
  #                       shape = 21, size = 0.5, alpha = 0.1,
  #                       position = position_dodge(width = 0.8)) +
  geom_half_violin(show.legend = F, 
                   side = "r", linewidth = 1, scale = "area", 
                   color = NA, alpha = 0.5,
                   position = position_dodge(width = 0.5)) +
  geom_half_boxplot(aes(colour = variable), 
                    side = "l", errorbar.draw = F,
                    width = 0.2, linewidth = 0.8,
                    position = position_dodge(width = 0.5),
                    outlier.size = 1.3, outlier.alpha = 0.3) +
  scale_fill_manual(values = mycolor) +
  scale_color_manual(values = mycolor) +
  # geom_point(aes(color = variable),
  #            # position = position_jitterdodge(dodge.width = 0.8),
  #            position = position_dodge(width = 0.8),
  #            # position = position_identity(),
  #            size = 1, alpha = 0.5) +

  labs(x = NULL, y = "Segregation index", fill = "Variable") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "top", text = element_text(size = 12))
ggsave("Output/EDFig/RVA_GNS_boxplot(new)2.pdf", 
       width = 3, height = 3, scale = 2.2)


## boots -----------------------------------------------------


### GNN - GSS difference ###

(mypairs_res[c(9,11)] %>% na.omit() -> testdt)
  # filter(NS_tag %in% c("GN - GN", "GS - GS")) -> testdt
  # filter(NS_tag %in% c("GN - GN", "GS - GS")) -> testdt
  # filter(NS_tag %in% c("GS - GS", "GN - GS")) -> testdt

# val.1 <- testdt[[1]][testdt$NS_tag == "GN - GS"]
# val.2 <- testdt[[1]][testdt$NS_tag == "GN - GN"]
# val.1 <- testdt[[1]][testdt$NS_tag %in% c("GN - GS", "GS - GN")]
# val.2 <- testdt[[1]][testdt$NS_tag %in% c("GN - GN", "GS - GS")]

val.1 <- testdt %>% filter(NS_tag == "GN - GN") %>% mutate(group = 1)
val.2 <- testdt %>% filter(NS_tag == "GS - GS") %>% mutate(group = 2)
# val.1 <- testdt %>% filter(NS_tag %in% c("GN - GS", "GS - GN")) %>% mutate(group = 1)
# val.2 <- testdt %>% filter(NS_tag %in% c("GN - GN", "GS - GS")) %>% mutate(group = 2)
(t.1 = sd(val.1[[1]]))
(t.2 = sd(val.2[[1]]))
(t.1 = median(val.1[[1]]))
(t.2 = median(val.2[[1]]))
(obs_diff <- (t.1 - t.2) / t.2 * 100)
(obs_diff <- (t.2 - t.1) / t.1 * 100)


## boot
library(boot)
set.seed(888)

boot_diff <- function(data, indices, var, group, fun, type = 1) {
  d <- data[indices, ]
  groups <- unique(d[[group]])

  val.1 <- d[[var]][d[[group]] == groups[1]]
  val.2 <- d[[var]][d[[group]] == groups[2]]

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
  R = 10000,
  var = "vote_scale",
  group = "group",
  fun = sd,
  type = 1 # 1 大
)
boot_results
boot.ci(boot_results, conf = 0.95, type = "perc")



## vote_scale ##
mypairs_res[c(9, 11)] %>% na.omit() -> testdt
mypairs_res[c(10, 11)] -> testdt
# val.1 <- testdt %>% filter(NS_tag == "GS - GS") %>% mutate(group = 1)
# val.2 <- testdt %>% filter(NS_tag == "GN - GN") %>% mutate(group = 2)
val.1 <- testdt %>% filter(NS_tag == "GS - GS" | NS_tag == "GS - GN") %>% mutate(group = 1)
val.2 <- testdt %>% filter(NS_tag == "GN - GN" | NS_tag == "GN - GS") %>% mutate(group = 2)
# val.1 <- testdt %>% filter(NS_tag == "GS - GS" | NS_tag == "GN - GN") %>% mutate(group = 1)
# val.2 <- testdt %>% filter(NS_tag == "GS - GN" | NS_tag == "GN - GS") %>% mutate(group = 2)
(t.1 <- sd(val.1$vote_scale))
(t.2 <- sd(val.2$vote_scale))
t.1 <- nrow(val.1[which(is.na(val.1$bibi_scale)),]) /nrow(val.1)
t.2 <- nrow(val.2[which(is.na(val.2$bibi_scale)),]) /nrow(val.2)
(t.1 <- median(val.1$vote_scale))
(t.2 <- median(val.2$vote_scale))
(obs_diff <- (t.2 - t.1) / t.1 * 100)
(obs_diff <- (t.1 - t.2) / t.2 * 100)
boot_results <- boot(
  data = rbind(val.1, val.2),
  statistic = boot_diff,
  R = 10000,
  var = "bibi_scale",
  strata = rbind(val.1, val.2)$group,
  group = "group",
  fun = function(x) length(x[is.na(x)])/length(x), 
  type = 1 # 1大
)
boot.ci(boot_results, conf = 0.95, type = "perc")






# biblio ---------------------------------------------------------------


mypairs_res %>%
  group_by(NS_tag) %>%
  summarise(
    total = n(),
    missing = sum(is.na(bibi_scale)),
    missing_pct = missing / total * 100
  ) # -> missing_summary


testdt <- mypairs_res[c(10:11)] %>% 
  # filter(NS_tag %in% c("GN - GN", "GS - GS")) %>% 
  mutate(bibi_scale = ifelse(is.na(bibi_scale), 0, 1))
ggpiestats(
  data         = testdt,
  x            = bibi_scale,
  y            = NS_tag,
  package      = "ggsci",
  palette      = "nrc_npg",
  title        = "Comparision of missing values"
)
# paletteer::palettes_d_names %>% filter(package == "ggsci")
ggsave("Output/SpFig/ACI_missing_pie.pdf", width = 5, height = 5, scale = 1.5)




existing_bibi <- mypairs_res %>% filter(!is.na(bibi_scale))
kruskal.test(bibi_scale ~ NS_tag, data = existing_bibi)

library(dunn.test)
dunn_test <- dunn.test(existing_bibi$bibi_scale, 
                       existing_bibi$NS_tag, 
                       method = "bonferroni")








# column ---------------------------------------------------------------

library(gridExtra)


## 哪些国家和所有国家都不像 按列看都是蓝色
mypairs_res[c(1,2,8)] %>% na.omit() %>% # 16129-127=16002
  group_by(adm0.x) %>% 
  slice_min(simi_scale, n = 5) %>%
  group_by(adm0.y) %>%
  summarise(count = n()) %>% # simi = sum(simi_scale)
  slice_max(count, n = 10) %>%
  arrange(count) -> mytable # %>% pull(adm0.y) -> tmp
mytable <- tableGrob(mytable)

# RCM_adm0 # retrieve from [9.2.RCM_12ISO_demo.R]
ggplot() +
  geom_point(
    data = RCM_adm0, aes(R.m, log(C.m), color = NS_tag),
    shape = 16, alpha = 0.3, show.legend = F
  ) +
  geom_point(
    data = filter(RCM_adm0, ISO3 %in% tmp),
    aes(R.m, log(C.m), color = NS_tag)
  ) +
  geom_text_repel(
    data = filter(RCM_adm0, ISO3 %in% tmp),
    aes(R.m, log(C.m), label = ISO3),
    show.legend = F, vjust = -1
  ) +
  coord_cartesian(clip = "off") +
  labs(title = "Overall characteristics at the national level",
       y = "log(GearyC)\n", x = "Risk") +
  theme_clean() -> p1

grid.arrange(p1, mytable, ncol = 2)
ggsave("Output/SpFig/most_dissimilar2other_scatterplot.pdf", scale = 2)



## 哪些国家和所有国家都像 按列看都是蓝色
mypairs_res[c(1,2,8)] %>% na.omit() %>% # 16129-127=16002
  group_by(adm0.x) %>% 
  slice_max(simi_scale, n = 5) %>%
  group_by(adm0.y) %>%
  summarise(count = n()) %>% # simi = sum(simi_scale)
  slice_max(count, n = 10) %>% 
  arrange(count) -> mytable # %>% pull(adm0.y) -> tmp
mytable <- tableGrob(mytable)

# tmp # [1] "IND" "CHN" "CHL" "RUS" "ARG" "MEX" "AUS" "BRA" "IDN" "USA"

# # retrieve mycowplotfun from [9.1.RCM_map_demo.R]
# mycowplotfun(tmp[3])
# mycowplotfun(tmp[13])
# mycowplotfun(tmp[14])


# RCM_adm0 # retrieve from [9.2.RCM_12ISO_demo.R]
ggplot() +
  geom_point(
    data = RCM_adm0, aes(R.m, log(C.m), color = NS_tag),
    shape = 16, alpha = 0.3, show.legend = F
  ) +
  geom_point(
    data = filter(RCM_adm0, ISO3 %in% tmp),
    aes(R.m, log(C.m), color = NS_tag)
  ) +
  geom_text_repel(
    data = filter(RCM_adm0, ISO3 %in% tmp),
    aes(R.m, log(C.m), label = ISO3),
    show.legend = F, vjust = -1
  ) +
  coord_cartesian(clip = "off") +
  labs(title = "Overall characteristics at the national level",
       y = "log(GearyC)\n", x = "Risk") +
  theme_clean() -> p1

grid.arrange(p1, mytable, ncol = 2)
ggsave("Output/SpFig/most_similar2other_scatterplot.pdf", scale = 2)





RCM_exp %>% filter(ISO3 %in% tmp) %>% 
  group_by(ISO3)

# retrieve mycowplotfun from [9.1.RCM_map_demo.R]
mycowplotfun(tmp[3])
mycowplotfun(tmp[13])


mycowplotfun("PHL")
mycowplotfun("COM")
mycowplotfun("AUS")


mycowplotfun("DNK")








# DROP -----------------------------------------------------------------





t.1 = sd(val.1, na.rm = T) # [1] 0.2177252
t.2 = sd(val.1, na.rm = T) # [1] 0.2177252

(t.1 = mean(val.1, na.rm = T)) # [1] 0.6956518
(t.2 = mean(val.2, na.rm = T)) # [1] 0.5245902




boot_diff <- function(data, indices) {
  
  GN_sample <- data[indices, "n"][data[indices, "GNS"] == "GN"]
  GS_sample <- data[indices, "n"][data[indices, "GNS"] == "GS"]
  
  GN_CV <- sd(GN_sample)
  GS_CV <- sd(GS_sample)
  
  (GS_CV - GN_CV) / GN_CV * 100
}
boot_results <- boot(testdt, boot_diff_cv, R = 10000)
boot.ci(boot_results, conf = 0.95, type = "perc")



mypairs %<>% mutate(NS_tag = paste(NS_tag.x, "-", NS_tag.y),
                    NS_tag = factor(NS_tag))
mypairs_res %<>% mutate(NS_tag = paste(NS_tag.x, "-", NS_tag.y),
                        NS_tag = factor(NS_tag)) %>% ungroup()




ggscatterstats(
  data = RCM_bc, x = R.mean,  y = C.mean,
  type = "np", conf.level = 0.95, p.adjust.method = "none",
  xsidehistogram.args = list(fill = "#7373ff", color = NA, 
                             size = 0.1, binwidth = 0.01),
  ysidehistogram.args = list(fill = "#f75aec", color = NA, 
                             size = 0.1, binwidth = 0.01),
  xlab = "Risk (Skewness = 1.59)", 
  ylab = "GearyC (Skewness = 4.01)",
  smooth.line.args = list(method = "lm", formula = y ~ poly(x, 2), 
                          size = 1, color = "grey70", alpha = .3),
  results.subtitle = T
) + 
  theme(axis.text = element_text(size = 5)) +
  annotate("text", x = 0.8, y = 1.0, 
           label = paste0("R^2 = ", round(summary(model)$r.squared, 3)), 
           size = 4, color = "black")






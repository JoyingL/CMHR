rm(list = ls())
# HEADER ---------------------------------------------------------------
pacman::p_load(data.table, tidyverse, readxl, writexl, magrittr, naniar)
pacman::p_load(ggthemes, patchwork, paletteer, ggrepel)
pacman::p_load(psych, epiDisplay, ggstatsplot, FSA)


library(urbnthemes)
set_urbn_defaults(style = "print")
urbnthemes::lato_import()

library(extrafont)
loadfonts(device = "win", quiet = F)



# LOAD -----------------------------------------------------------------


# # mypairs <- read_rds("Data/sim/my_pairs_all.rds")
# # mypairs_res <- read_rds("Data/sim/my_pairs_all_126_rescale.rds") # box-cox后
# mypairs_res <- read_rds("Data/final/mypairs_126_01标准化.rds")
# mypairs_res %<>% mutate(NS_tag = paste0(NS_tag.x, "-", NS_tag.y))
# mypairs_res %<>% dplyr::select(-c(3:5)) %>% ungroup()
# 
# mypairs_res
# glimpse(mypairs_res)
# miss_var_summary(mypairs_res)



# PROCESS --------------------------------------------------------------


# mypairs_res %<>% filter(adm0.x != adm0.y) # 15,876-126 = 15,750

# mypairs_res %<>% filter(adm0.x != "PSE" & adm0.y != "PSE")
# length(unique(mypairs_res$adm0.x)) # 126^2 = 15876 15876 - 126 = 15750

# mypairs_res %<>% mutate(bibi_scale = replace_na(bibi_scale, 0))
# # mypairs_res %>% write_rds("RVA_01标准化.rds")
mypairs_res <- read_rds("RVA_01标准化.rds")



tableStack(decimal = 
  dataFrame = data.frame(mypairs_res),
  c(5:8),
  by = NS_tag, 
  total.column = T
) -> table1
table1
write.csv(table1, "Output/Table/mypairs_description_stats.csv")



# TYPES ----------------------------------------------------------------


mypairs_res %>% ggplot(aes((simi_scale))) + geom_histogram(bins = 150)
(aa <- quantile(mypairs_res$simi_scale))


mypairs_res %>% ggplot(aes((vote_scale))) + geom_histogram(bins = 150)
(bb <- quantile(mypairs_res$vote_scale))


mypairs_res %>% ggplot(aes((bibi_scale))) + geom_histogram(bins = 150)
mypairs_res %>% filter(bibi_scale != 0) %>% ggplot(aes((bibi_scale))) + geom_histogram(bins = 150)
(cc <- quantile(filter(mypairs_res, bibi_scale != 0)$bibi_scale))


## 分位数打上标记
# mypairs_tag %>% filter(simi_scale > 0.9)
mypairs_tag <- mypairs_res %>% # 7,875
  mutate(R = case_when(
    simi_scale < aa[[2]] ~ -2,
    simi_scale >= aa[[2]] & simi_scale < aa[[3]] ~ -1,
    simi_scale >= aa[[3]] & simi_scale < aa[[4]] ~ 1,
    simi_scale >= aa[[4]] ~ 2
  )) %>%
  mutate(V = case_when(
    vote_scale < bb[[2]] ~ -2,
    vote_scale >= bb[[2]] & vote_scale < bb[[3]] ~ -1,
    vote_scale >= bb[[3]] & vote_scale < bb[[4]] ~ 1,
    vote_scale >= bb[4] ~ 2
  )) %>%
  mutate(A = case_when(
    is.na(bibi_scale) ~ -2,
    bibi_scale < cc[[2]] ~ -2,
    bibi_scale >= cc[[2]] & bibi_scale < cc[[3]] ~ -1,
    bibi_scale >= cc[[3]] & bibi_scale < cc[[4]] ~ 1,
    bibi_scale >= cc[[4]] ~ 2
  ))
mypairs_tag %>% filter(R > 0) # 7,875
mypairs_tag %>% filter(R > 1) # 3,938
# mypairs_tag %>% write_rds("RVA_01标准化_TAG.rds")

## 比例计算
tag_distribution <- mypairs_tag %>%
  count(R, V, A) %>%
  mutate(prob = n / sum(n))

tag_combinations <- expand.grid(
  R = (c(-2, -1, 1, 2)),
  V = (c(-2, -1, 1, 2)),
  A = (c(-2, -1, 1, 2)))

tag_distribution <- left_join(
  tag_combinations, tag_distribution,
  by = c("R", "V", "A")) %>%
  replace_na(list(prob = 0)) %>%
  arrange(desc(prob)) %>% tibble()

## add "high" "low" tag and calculate prob
tag_distribution %>% filter(R > 1) %>% # high RPS
  mutate(
    simi_tag = "high",
    vote_tag = case_when(V > 0 ~ "high", V < 0 ~ "low"),
    bibi_tag = case_when(A > 0 ~ "high", A < 0 ~ "low")
  ) -> tag_distribution


# prob_distribution_tag %>% na.omit() -> finalprob
((tag_distribution)$prob %>% sum() -> mysum) # 0.5  0.25 31.504
tag_distribution %>%
  group_by(vote_tag, bibi_tag) %>%
  summarise(total = sum(prob) / mysum) -> t
t
# t %>% write_csv("Output/Table/RVA_ours_perc(new).csv")






# STATA ----------------------------------------------------------------

mypairs_tag[-c(3:7)] %>% filter(R > 1) %>% 
  mutate(
    R_tag = "H",
    V_tag = case_when(V > 0 ~ "H", V < 0 ~ "L"),
    A_tag = case_when(A > 0 ~ "H", A < 0 ~ "L"),
    Tag = paste0(R_tag, V_tag, A_tag)
  ) -> demo

tableStack(
  dataFrame = data.frame(demo),
  c(3,10),
  by = Tag,
  total.column = T
) -> table1
table1
# write.csv(table1, "Output/Table/descrip_stats_highRPS2.csv")
write.csv(table1, "Output/Table/highRPS_description_stat.csv")


# # demo %>% count(NS_tag, Tag) -> demo
# demo %>% group_by(NS_tag) %>%
#   mutate(percentage = n / sum(n, na.rm = T) * 100) %>%
#   dplyr::select(NS_tag, Tag, percentage) %>%
#   pivot_wider(names_from = Tag, values_from = percentage) -> tmp
# demo %>% group_by(Tag) %>%
#   mutate(percentage = n / sum(n, na.rm = T) * 100) %>%
#   dplyr::select(NS_tag, Tag, percentage) %>%
#   pivot_wider(names_from = Tag, values_from = percentage) -> tmp
# tmp
# sapply(tmp[-1], sum, na.rm = T) -> sum.tmp
# (tmp[2,5] + tmp[3,5]) / sum.tmp[4]



# ROBUST ---------------------------------------------------------------


mypairs_res %>% ggplot(aes((simi_scale))) + geom_histogram(bins = 100)
(aa <- median(mypairs_res$simi_scale)) # [1] 0.5078628
(aa <- mean(mypairs_res$simi_scale)) # [1] 0.518476
(aa <- quantile(mypairs_res$simi_scale)[[4]]) # [1] 0.698986
(aa <- quantile(mypairs_res$simi_scale))


mypairs_res %>% ggplot(aes((vote_scale))) + geom_histogram(bins = 100)
(bb <- median(mypairs_res$vote_scale)) # [1] 0.6544452
(bb <- mean(mypairs_res$vote_scale)) # [1] 0.6375273
(bb <- quantile(mypairs_res$vote_scale)[[4]]) # [1] 0.8136141
(bb <- quantile(mypairs_res$vote_scale))


mypairs_res %>% filter(bibi_scale != 0) %>% ggplot(aes((bibi_scale))) + geom_histogram(bins = 150)
(cc <- filter(mypairs_res, bibi_scale != 0)$bibi_scale %>% median()) # [1] 0.1439576
(cc <- filter(mypairs_res, bibi_scale != 0)$bibi_scale %>% mean()) # [1] 0.2424812
(cc <- quantile(filter(mypairs_res, bibi_scale != 0)$bibi_scale)[[4]])
(cc <- quantile(filter(mypairs_res, bibi_scale != 0)$bibi_scale))


# annotation tags
mypairs_tag <- mypairs_res %>% ungroup() %>% 
  filter(simi_scale >= aa) %>% 
  mutate(R = "high",
         V = ifelse(vote_scale >= bb, "high", "low"),
         A = ifelse(bibi_scale >= cc, "high", "low"))

mypairs_tag %>%
  filter(R == "high") %>%
  count(R, V, A) %>%
  mutate(probability = n / sum(n)) -> dfmean
df75
df50
dfmean
t

## quantile 75%
# A tibble: 4 × 5
#   R     V     A         n probability
#   <chr> <chr> <chr> <int>       <dbl>
# 1 high  high  high    174      0.0442
# 2 high  high  low     878      0.223 
# 3 high  low   high    218      0.0554
# 4 high  low   low    2668      0.678 

## quantile 50% / median
# A tibble: 4 × 5
#   R     V     A         n probability
#   <chr> <chr> <chr> <int>       <dbl>
# 1 high  high  high    617      0.0783
# 2 high  high  low    3363      0.427 
# 3 high  low   high    407      0.0517
# 4 high  low   low    3488      0.443

## mean
# A tibble: 4 × 5
#   R     V     A         n probability
#   <chr> <chr> <chr> <int>       <dbl>
# 1 high  high  high    528      0.0545
# 2 high  high  low    4807      0.496 
# 3 high  low   high    223      0.0230
# 4 high  low   low    4131      0.426


# bind_rows(df75, df50, dfmean) %>% write_csv("Output/Table/robust_RVA(new).csv")

df <- read_csv("Output/Table/robust_RVA(new).csv")
df %<>% mutate(type = paste0(R,V,A))
cont_table <- xtabs(probability ~ type + att, data = df)
# chisq.test(cont_table)
# aov_result <- aov(probability ~ att, data = df)
# summary(aov_result)
# TukeyHSD(aov_result)
# kruskal.test(probability ~ att, data = df)
# library(pgirmess)
# kruskalmc(df$probability, df$att)
cor_matrix <- cor(cont_table, use = "pairwise.complete.obs")
cor_matrix


library(ggstatsplot)
df_wide <- df %>% 
  dplyr::select(type, att, probability) %>%
  pivot_wider(names_from = att, values_from = probability)
ggcorrmat(
  data = df_wide,
  type = "p",
  p.adjust.method = "none",
  colors = c("#4a6bb1", "white", "#976fae"),
  title = "Correlalogram for different definitions"
)
ggsave("Output/SpFig/corr_robustness_RVA(new).pdf", scale = 2)




























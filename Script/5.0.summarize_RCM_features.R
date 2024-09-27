
rm(list = ls())
# HEADER ---------------------------------------------------------------
pacman::p_load(data.table, tidyverse, spdep)


# LOAD -----------------------------------------------------------------

gaul <- read_rds("Data/final/MHS_final_tess.rds")
glimpse(gaul)
miss_var_summary(gaul)

# ISO3 values exist NA because only the mainland is considered 
# and the overseas territories are not taken into account.


# RCMshp <- read_rds("Data/final/MHS_final_tess_shp.rds") %>% 
#   group_by(COMM) %>% summarise(geometry = st_union(geometry))
# RCMshp %>% write_rds("Data/final/MHS_final_comm_shp.rds")


# R mean ------------------------------------------------------------------
(gaul %>% group_by(COMM) %>% summarise(R.mean = mean(risk)) -> dt4RCM)


# C mean ------------------------------------------------------------------

# While not required in the univariate context, 
# the standardized Local Geary is calculated. Unlike the Local Moran, 
# Local Geary uses squared differences to measure dissimilarity.
# Low values of the Local Geary indicate positive spatial autocorrelation 
# and large refers to negative spatial autocorrelation.

# ## Compute Local Geary's C for each Cell
# mylocalC <- localC(tess_valid_rm$risk, lw)
# gaul$GearyC = mylocalC

(gaul %>% group_by(COMM) %>% 
    summarise(n = n(), C.mean = mean(GearyC)) %>%
    left_join(dt4RCM) -> RCM)
# RCM %>% write_rds("Data/final/MHS_final_comm.rds")


# Features ----------------------------------------------------------------

any(is.na(gaul$NS_tag))
miss_var_summary(gaul)

## GNS -------------------------------------------------------------

### RCMs belong to Global North or South ###
gaul %>% group_by(COMM, NS_tag) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = NS_tag, values_from = count) -> tmp_NS

## label: 1-GS, 2-GN, 3-GS+GN
tmp_NS %<>% mutate(GNS = case_when(
  !is.na(GS) & !is.na(GN) ~ 3,
  !is.na(GS) & is.na(GN) ~ 1,
  is.na(GS) & !is.na(GN) ~ 2,
  TRUE ~ NA))

## community_across_GSGN
tmp_NS %>% filter(GNS == 3)

## factor
RCM %<>% left_join(tmp_NS, by = "COMM") %>% 
  mutate(GNS = factor(GNS, levels = c(1:3), labels = c("GS", "GN", "GS+N")))
RCM
# RCM %>% write_rds("Data/final/MHS_final_comm.rds")




## ISO ---------------------------------------------------------------


### RCMs belong to uni / multi Countries ###
length(unique(gaul$ISO3)) # [1] 129 countries

gaul # 183,563
gaul %<>% filter(ISO3 != 'PSE') # 182,312

gaul %>%
  filter(!is.na(ISO3)) %>%
  group_by(COMM) %>%
  summarise(
    n_ISO = n_distinct(ISO3),
    ISO3 = list(unique(ISO3))
  ) -> tmp_adm0

uni_adm0 <- tmp_adm0 %>% filter(n_ISO == 1) # 749, belong to a single nation
mul_adm0 <- tmp_adm0 %>% filter(n_ISO != 1) # 164, belong to multi countries

rbind(
  unnest(uni_adm0, cols = ISO3),
  unnest(mul_adm0, cols = ISO3)
) %>%
  right_join(RCM, by = "COMM") %>% # right join RCM not RCM_bc → no bc!
  arrange(COMM) -> RCM_exp_ISO # A tibble: 1,113 × 9
# RCM_exp_ISO %>% write_rds("Data/final/MHS_final_comm_expand_ISO.rds")








### RCMs across boundaries ###
# mycountry <- read_xlsx("Data/studyarea/countrylist.xlsx")
# mycountry %>% distinct(ISO3, adm0_name)

mul_adm0 # 164 × 3
unnest(mul_adm0, cols = ISO3) %>% 
  left_join(mycountry %>% distinct(ISO3, adm0_name)) %>% 
  group_by(COMM) %>%
  summarize(adm0 = str_c(adm0_name, collapse = "-")) -> tmp

## select the top 10 density
# mylist = c(602,621,663,119,485,723,766,790,754,951,504,546,289,409,89)
mylist = c(119,289,394,409,504,546,602,621,689,704)
RCM %>% filter(COMM %in% mul_adm0$COMM) -> RCMCross
RCMCross %>% # mutate(perc = wop/n) %>% slice_max(perc, n = 10) %>% 
  filter(COMM %in% mylist) %>%
  left_join(tmp) -> RCMSample
RCMSample %>% write_csv("Output/Table/descrip_cross_RCM.csv")

RCMshp %>% inner_join(RCMSample %>% select(COMM, adm0)) %>% 
  na.omit() -> myRCMshp
for (comm in RCMSample$COMM){
  filter_shp = myRCMshp %>% filter(COMM == comm)
  map <- mapview(filter_shp, zcol = "COMM", 
                 legend = TRUE, layer.name = filter_shp$adm0)
  mapshot(map, paste0("map_COMM_max_", comm, ".html"))
}


RCMshp %>% filter(COMM %in% mul_adm0$COMM) %>% mapview(legend = F) -> map
# mapshot(map, file = "RCM_cross_map.pdf")
filter_shp = myRCMshp %>% filter(COMM == 689)
mapview(filter_shp, zcol = "COMM", layer.name = filter_shp$adm0)
# mapshot(mmap, "map_COMM_119.pdf")
# library(knitr)
# kable(RCMCross %>% slice_max(n, n = 10))
# set.seed(888)












# DROP -----------------------------------------------------------------


(tess_gaul %>% st_drop_geometry() %>%
   filter(!is.na(ISO3)) %>% 
   group_by(comm, ISO3) %>%
   summarise(count = n(), .groups = "drop") %>%
   pivot_wider(names_from = ISO3, values_from = count) -> tmp_adm0)

# tess_gaul 应该是由shp信息的gaul


rescale <- function(x, type = 1) {
  rng = range(x, na.rm = TRUE)
  if (type == 1) {
    (x - rng[1]) / (rng[2] - rng[1])
  } else {
    (rng[2] - x) / (rng[2] - rng[1])
  }
}




### RCM R.mean C.mean power-law? ###
library(tidyverse)
library(fitdistrplus)
library(actuar)
RCM_res <- read_rds("Data/final/MHS_final_comm_bc.rds")
RCM <- read_rds("Data/final/MHS_final_comm.rds")
# 对C.mean和R.mean分别拟合帕累托分布
fit_c <- fitdist(RCM$C.mean, "pareto",start = list(shape = 1,scale = 1))
fit_r <- fitdist(RCM$R.mean, "pareto",start = list(shape = 1,scale = 1))
# 创建一个绘图函数
plot_dist <- function(data, fit, title) {
  plt <- ggplot(data, aes(x = value)) +
    geom_histogram(bins = 30, color = "black", fill = "grey") +
    stat_function(fun = dpareto, 
                  args = list(shape = fit$estimate[[1]], 
                              scale = fit$estimate[[2]]), 
                  color = "red", size = 1) +
    labs(title = title, x = "Value", y = "Frequency")
  return(plt)
}
# 可视化C.mean和R.mean的分布拟合情况
plt_c <- plot_dist(data.frame(value = RCM$C.mean), fit_c, 
                   "Distribution of C.mean")
plt_r <- plot_dist(data.frame(value = RCM$R.mean), fit_r, 
                   "Distribution of R.mean")
# 组合两个图
library(patchwork)
plt_c + plt_r
ks.test(RCM$C.mean, "ppareto", fit_c$estimate[1], fit_c$estimate[2])
ks.test(RCM$R.mean, "ppareto", fit_r$estimate[1], fit_r$estimate[2])
# p<0.05 拒绝样本与理论分布相同的零假设→与pareto分布拟合不好


library(poweRlaw)
m <- conpl$new(RCM$C.mean)
est_xmin <- estimate_xmin(m)
est_pars <- estimate_pars(m)
m$setXmin(est_xmin)
m$setPars(est_pars)
plot(m)
lines(m, col = 2)
bs <- bootstrap_p(m, no_of_sims = 100, threads = 100)
bs$p # [1] 0 p值也不好















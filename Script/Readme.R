

pacman::p_load(data.table, tidyverse, readxl, writexl, magrittr, naniar)
pacman::p_load(ggthemes, patchwork, paletteer, ggrepel, ggchicklet)
pacman::p_load(rmapshaper, sf, terra, mapview, ggspatial)
pacman::p_load(igraph, cluster, spdep, ggraph)


library(urbnthemes)
set_urbn_defaults(style = "print")
urbnthemes::lato_import()

library(extrafont)
loadfonts(device = "win", quiet = F)

library(boot)



# SHP ------------------------------------------------------------------


# raw file with 241332 features and 1 field (geometry)
read_sf("Data/studyarea/tess_global_valid_WGS84.gpkg")


# processed file with 184013 features and 28 fields
tess_valid_rm <- read_rds("Data/graph/tess_valid_rm.rds")


## spatial adjacency matrix
# nb_q_rm <- poly2nb(tess_valid_rm, queen = TRUE)
nb_q_rm <- read_rds("Data/graph/nb_q_rm.rds")


## spatial weight matrix
# lw <- nb2listw(nb_q_rm, style = "W")
lw <- read_rds("Data/graph/lw.rds")


## with 183563 features and 26 fields
mytess <- read_rds("Data/final/MHS_final_tess_shp.rds")




# Table ----------------------------------------------------------------


# 合并所有属性的格网: 183,563 × 28
# 社区检测修改后
gaul <- read_rds("Data/final/MHS_final_tess.rds")
glimpse(gaul)


length(unique(gaul$COMM)) # 916 个社区
length(unique(gaul$ISO3)) # 129 个国家


# 格网汇总到社区最全属性: A tibble: 916 × 7 + 4 (LISA) + 1 (worldpop) = 12
read_rds("Data/final/MHS_final_comm.rds")
glimpse(RCM) # 916


# 按照国家ISO拆分社区(跨几个国家拆分为几行): 1,113 × 14
read_rds("Data/final/MHS_final_comm_expand_ISO.rds")


# 经过正态变换和归一化的社区数据集: 916 × 9 象限属性
read_rds("data/final/MHS_final_comm_bc.rds")



# exposure 7 个指标
(myexp = read_xlsx("final/exposure_final.xlsx")) # 209,822
glimpse(myexp)
range(myexp$exp) # 已归一化


# inner_join 后最终一共有的国家
mycountry <- read_rds("Data/studyarea/mycountry.rds") # 127




# 最初的R-V-A合并处理好连接GNS属性数据集: 16,129 × 7
mypairs <- read_rds("Data/sim/my_pairs_all.rds")


# 按行归一化后的pairs绘图加分析用: 16,129 × 7 + 3 = 10
mypairs_plot <- read_rds("Data/sim/my_pairs_all_rescale.rds") 
mypairs_res <- read_rds("Data/sim/my_pairs_all_rescale.rds") 






rescale <- function(x, type = 1) {
  rng = range(x, na.rm = TRUE)
  if (type == 1) {(x - rng[1]) / (rng[2] - rng[1])} 
  else {(rng[2] - x) / (rng[2] - rng[1])}
}


std <- function(x){
  x / max(x, na.rm = T)
}













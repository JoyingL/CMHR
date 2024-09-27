
rm(list = ls())
# HEADER ---------------------------------------------------------------
pacman::p_load(data.table, tidyverse, readxl, writexl, magrittr, naniar)
pacman::p_load(ggthemes, patchwork, paletteer, ggrepel)
pacman::p_load(rmapshaper, sf, terra, mapview, ggspatial)
pacman::p_load(igraph, cluster, spdep, ggraph)



# LOAD -----------------------------------------------------------------

tess_valid_rm <- read_rds("Data/graph/tess_valid_rm.rds")

# nb_q_rm <- poly2nb(tess_valid_rm, queen = TRUE)
nb_q_rm <- read_rds("Data/graph/nb_q_rm.rds")

# lw <- nb2listw(nb_q_rm, style = "W")
lw <- read_rds("Data/graph/lw.rds")

# adjacencies
(nb <- lw$neighbours)
 


# NETWORK --------------------------------------------------------------

### Construct network ###

## local Geary's C for grid-pairs
att = tess_valid_rm$risk
diffs_sq <- lapply(
  seq_along(nb), \(i) (att[i] * att[nb[[i]]]) / (att[i] - att[nb[[i]]])^2
)


## turn to edge weights
edges_df <- mapply(function(i, nb_i, weights_i) {
  data.frame(from = i, to = nb_i, weight = weights_i)
}, seq_along(nb), nb, diffs_sq, SIMPLIFY = FALSE) %>% bind_rows()
# data.table(edges_df)


## construct graph
g.all <- graph_from_data_frame(edges_df, directed = FALSE) %>% simplify()
E(g.all)$weight[is.na(E(g.all)$weight)] <- Inf # what if denominator = NA
g.all %>% write_rds("Data/graph/g_raw.rds")


## add features to grids
V(g.all)$GRID_ID <- tess_valid_rm$GRID_ID
V(g.all)$adm0_name <- tess_valid_rm$adm0_name
V(g.all)$risk <- tess_valid_rm$risk



# RCM ------------------------------------------------------------------

### cluster_leiden community detection ###
set.seed(888)
community <- cluster_leiden(g.all, n_iterations = 100,
                            resolution_parameter = .016,
                            weights = E(g.all)$weight,
                            objective_function = "CPM")

(n = length(unique(membership(community)))) # [1] 1095
(modularity_value = modularity(g.all, membership(community))) # [1] 0.962
sum(table(membership(community)) == 1) # [1] 45

V(g.all)$community <- membership(community)
# g.all %>% write_rds("Data/graph/g_new.rds")
# g.all <- read_rds("Data/graph/g_new.rds")

length(unique(V(g.all)$community))

table((V(g.all)$community)) %>% data.frame() -> tmp
tmp %>% filter(Freq < 6) %>% nrow()
160 / 1090
1090 - 916

# ## update data
# myfac = as.factor(V(g.all)$community)
# (tibble(GRID_ID = V(g.all)$GRID_ID, COMM = myfac) -> tess_tmp)
# (tibble(tess_valid_rm[1], COMM = myfac) -> tess_revised)
# 
# ### modify outliers (n=1/2)
# tess_tmp %>% group_by(COMM) %>% summarise(n = n()) -> comm_count
# tess_revised %<>% left_join(comm_count, by = "COMM")
# # tess_revised %>% write_sf("tess_comm_revised.gpkg")
# 
# ### modified sf file
# tess_tmp_fined <- read_sf("tess_comm_fined.gpkg") %>% st_drop_geometry() # 183,968
# tess_tmp_fined %>% group_by(COMM) %>% summarise(n = n()) -> comm_count_edit
# (filter(comm_count_edit, n >= 7) %>% mutate(COMM = as.numeric(COMM))-> tmp)
# 
# ### 修改后的社区
# glimpse(gaul)
# gaul %<>% left_join(tess_tmp_fined[1:2]) %>% mutate(COMM = as.numeric(COMM))
# gaul %>% filter(COMM %in% tmp$COMM) %>% write_rds("Data/final/MHS_final_tess.rds")



# PLOT --------------------------------------------------------------------

gaul <- read_rds("Data/final/MHS_final_tess.rds")
# glimpse(gaul)
# length(unique(gaul$COMM)) # [1] 916

# tess_plot <- read_sf("Data/studyarea/tess_glo_valid_4_plot_tess.gpkg")
# tess_plot %<>% left_join(gaul[c(1,25)], by = "GRID_ID")

tess_plot <- read_sf("Data/studyarea/tess_glo_valid_4_plot_comm_union.gpkg")
mycomm <- read_rds("Data/final/MHS_final_comm.rds")
tess_plot %<>% left_join(mycomm[c(1:5,8)])
glimpse(tess_plot)

## Crs
target_crs <- st_crs("+proj=robin +datum=WGS84 +no_defs +over")
tess_plot %<>% st_transform(crs = target_crs)

## Palatte
library(paletteer)
set.seed(777)
n = length(unique(gaul$COMM))
colors <- paletteer_d("ggsci::springfield_simpsons")
colors <- rep(colors, ceiling(n / length(colors)))

# 创建一个数据框架，每个颜色一个条目
df <- data.frame(colors = factor(colors), value = 1)
# 绘制圆环图
ggplot(df, aes(x = "", y = value, fill = colors)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") + # 极坐标使其成为环形
  theme_void() + # 移除多余的坐标轴和标签
  theme(legend.position = "none") + # 不显示图例
  scale_fill_identity() # 使用数据框中的颜色值填充
# ggsave("final/color_donut_plot.pdf", width = 1, height = 1)


## GGplot
library(ggthemes)
tess_plot %>% ggplot() +
  geom_sf(aes(fill = COMM), color = NA, size = 0) +
  coord_sf(crs = target_crs) +
  scale_fill_manual(values = colors, na.value = "#f7f7f7") +
  theme_pander() + # theme_map / theme_pander
  theme(
    legend.title = element_text(face = "bold"),
    legend.key.size = unit(1, "lines"),
    legend.position = "right",
    legend.spacing.x = unit(.7, "lines"),
    panel.grid = element_blank(),
    axis.line = element_blank()
  ) +
  labs(subtitle = "Regional risk community structure") +
  guides(alpha = "none", fill = "none") -> mygraphplot

ggsave("Output/Fig/cluster_leiden_CPM_100_016_simpsons.pdf",
       mygraphplot, width = 5, height = 2, scale = 3)







# RESOLUTION --------------------------------------------------------------

# ## Set the resolution search scope and step size
# resolution_values <- seq(from = 0.005, to = 1, by = 0.005)
# 
# ## Function
# get_communities_and_modularity <- function(resolution) {
#   community <- cluster_leiden(g.all, n_iterations = 3,
#                               weights = E(g.all)$weight,
#                               objective_function = "CPM",
#                               resolution_parameter = resolution)
#   # number of communities
#   num_communities = length(unique(membership(community)))
#   # how many communities with 1 constituent
#   num_member_equal_1 = sum(table(membership(community)) == 1)
#   # modularity
#   modularity_value = modularity(g.all, membership(community))
#   
#   return(c(num_communities, num_member_equal_1, modularity_value))
# }
# results <- t(sapply(resolution_values, get_communities_and_modularity))
# 
# ## Get Result
# df <- tibble(
#   Resolution = resolution_values,
#   Communities = results[, 1],
#   equal_1 = results[, 2],
#   Modularity = results[, 3]) %>%
#   mutate(
#     Comm_scale = rescale(Communities),
#     Modu_scale = rescale(Modularity),
#     diff1 = abs(Comm_scale - Modu_scale),
#     diff = abs(Comm_scale - Modularity)
#   )
# 
# df[which.min(df$diff1), ]
# (r <- df[which.min(df$diff1), ][[1]]) # best resolution 0.016
# 
# 
# ## Dual y-axes plot as a function of resolution
# df %>% ggplot(aes(Resolution)) +
#   geom_line(aes(y = Modu_scale, color = "Modularity")) +
#   geom_line(aes(y = Comm_scale, color = "Communities")) +
#   geom_vline(xintercept = r, linetype = "dashed") +
#   scale_y_continuous(sec.axis = sec_axis(~ . * max(df$Comm_scale),
#                                          name = "Communities")) +
#   scale_color_manual(values = c("Modularity" = "#3babc1",
#                                 "Communities" = "#e65335")) +
#   labs(color = "Metrics", x = "Resolution", y = "Modularity",
#        title = sprintf("Best Resolution: %.4f", r)) +
#   theme(text = element_text(family = "serif"))
# # ggsave("Modularity_vs_NumberofCommunities.pdf", width = 7, height = 4)















# DROP -----------------------------------------------------------------


rescale <- function(x, type = 1) {
  rng = range(x, na.rm = TRUE)
  if (type == 1) {(x - rng[1]) / (rng[2] - rng[1])} 
  else {(rng[2] - x) / (rng[2] - rng[1])}
}



library(urbnthemes)
set_urbn_defaults(style = "print")
library(extrafont)
loadfonts(device = "win", quiet = F)

library(ggstatsplot)
urbnthemes::lato_import()





























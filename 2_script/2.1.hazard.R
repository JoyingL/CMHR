# HEADER ------------------------------------------------------------------
rm(list = ls())
pacman::p_load(data.table, tidyverse, readxl, writexl, magrittr, naniar)
pacman::p_load(rmapshaper, sf, terra, mapview, ggspatial)
pacman::p_load(ggthemes, patchwork, paletteer, ggrepel)
pacman::p_load(igraph, cluster, spdep, ggraph)


library(urbnthemes)
set_urbn_defaults(style = "print")
library(extrafont)
extrafont::font_import()


### 已经更新好 ISO3 NS_tag CONT 等所有信息 ###
# gaul <- read_rds("final/MHS_final.rds") # 184,013
gaul <- read_rds("final/MHS_final_comm.rds") # 183,563
glimpse(gaul)

gaul[c(1:11)] -> newgaul
glimpse(newgaul)

## 缺失值
miss_var_summary(newgaul)


# HAZARD ------------------------------------------------------------------
myhaz = read_xlsx("final/hazard_final.xlsx", sheet = 1) %>% dplyr::select(1:5,7)

## haz.raw 归一化
myhaz %<>% mutate(haz.raw = haz.raw / max(haz.raw)) 
miss_var_summary(myhaz) ; sapply(myhaz[-1], range)

## haz 是加权的，但权重不好解释而且计算方法不完全严谨
## haz.raw 是无权重的直接相加，这样会更直接客观好理解

## 连接 hazard 数据
newgaul %<>% left_join(myhaz, by = "GRID_ID")
glimpse(newgaul)


# bruceR::Describe(newgaul$haz)
# # Descriptive Statistics:
# # ────────────────────────────────────────────────────────
# #         N Mean   SD | Median  Min  Max Skewness Kurtosis
# # ────────────────────────────────────────────────────────
# #    199157 0.39 0.23 |   0.39 0.01 1.00     0.18    -0.90
# # ────────────────────────────────────────────────────────
# bruceR::Describe(newgaul$haz.raw)
# # Descriptive Statistics:
# # ────────────────────────────────────────────────────────
# #         N Mean   SD | Median  Min  Max Skewness Kurtosis
# # ────────────────────────────────────────────────────────
# #    199157 0.36 0.23 |   0.34 0.01 1.00     0.42    -0.69
# # ────────────────────────────────────────────────────────



# ANA -----------------------------------------------------------------------------
glimpse(newgaul)

####### 哪种灾害发生概率更高？在每个大洲是怎么分布的？ #######
(colSums(newgaul[12:15] > 0.5) -> tmp)
# prob.dz prob.hp prob.hs prob.tf 
#   50199   50074   87953   65165 

gaul_long <- newgaul[c(1,10,12:15)] %>%
  mutate(across(c(3:6), ~ ifelse(. > 0.5, 1, 0))) %>%
  pivot_longer(names_to = "variable", values_to = "value", - c(GRID_ID, CONT))

gaul_summary <- gaul_long %>%
  group_by(CONT, variable) %>%
  summarise(n = sum(value, na.rm = TRUE), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(total = sum(n), percentage = round(n / total, 2))

# variable_totals <- gaul_summary %>% 
#   group_by(variable) %>% 
#   summarise(total = sum(n)) %>% 
#   mutate(percentage = round(total / sum(total), 4))

# mycolors <- c("#bb2d3d","#f7cd55","#d7d4d4","#b29dd3","#41957e","#5bb7c7")
mycolors <- c("#bb2d3d","#f7cd55","#f291a8","#b29dd3","#41957e","#5bb7c7")
# mycolors <- c("#b13033","#eb9943","#f4c3cd","#845ca9","#3f8148","#90bbc1")

mycolors <- c("#f2a264","#e57057","#9f4676","#581b9a","#4257b2","#20a0bd")
ggplot() +
  geom_bar(data = gaul_summary, 
           aes(x = reorder(variable, -n), y = n, fill = reorder(CONT, -n)),
           stat = "identity", width = .5) +
  geom_text(data = variable_totals, family = "serif",
            aes(x = variable, y = total, label = scales::percent(percentage)),
            vjust = -0.5) +
  scale_fill_manual(values = mycolors) +
  scale_x_discrete(labels = c("flood", "cyclone", "earthquake", "landslide")) +
  # scale_fill_manual(values = c("#0099dc","#2f7f80","#9674b0","#d2d2d2"),
  #                   labels = c("earthquake","landslide","flood","cyclone")) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(x = "Hazard type", y = "Count/103", fill = "Continent") +
  # theme_clean() +
  theme(text = element_text(family = "serif"),
        axis.ticks = element_blank(),
        axis.line.x = element_blank(),
        axis.title = element_text(face = "bold"))
# ggtitle("Number of cells assessed as hazard-prone")
ggsave("final/hazard_type_by_cont.pdf", width = 3, height = 3, scale = 1.5)



####### 四种灾害的耦合情况？在每个大洲是怎么分布的？ #######

# prob大于0.5即有可能发生
# sapply(newgaul[c(12:15)], range)

(haz_tag <- newgaul[c(12:15)] %>% 
   mutate(label = rowSums(. > 0.5)) %>%
   mutate(label = factor(label, ordered = T)))
newgaul$haz_tag = haz_tag$label

haz_tag
table(haz_tag$label)
#   0     1     2     3     4 
# 59004 49776 36233 23051 15499 


## 每个大陆的不同耦合类型的数量 → 每种类型在其所在大陆的占比
gaul_haz_sum <- newgaul %>%
  count(CONT, haz_tag) %>%
  left_join(newgaul %>% count(CONT) %>% rename(total = n), by = "CONT") %>%
  mutate(percentage = round(n / total, 4))

ggplot(gaul_haz_sum, aes(x = reorder(CONT, n), y = percentage, fill = col)) +
  geom_bar(stat = "identity", position = "fill", width = .5) +
  geom_text(family = "serif",
            aes(label = scales::percent(percentage, accuracy = 1)),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_identity() + # 使用实际颜色值作为填充
  theme_classic() +
  theme(
    text = element_text(family = "serif"),
    axis.ticks = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  ) +
  guides(fill = "none") + # 隐藏图例
  labs(x = "Continent", y = "Count", fill = "Hazard Type") +
  coord_flip()
ggsave("final/hazard_type_couple_by_cont.pdf", width = 5, height = 2, scale = 1.5)



# library(ggsci)
# mycolors <- pal_npg()(5) %>% rev()
# mycolors <- c("#5c199c","#9d4edd","#3e5bab","#20a0bd","#8fcbe7") %>% rev()
library(scales)
# mycolors <- c("#bb2d3d","#f7cd55","#f291a8","#b29dd3","#41957e","#5bb7c7")
mycolors <- c("#f2a264","#e57057","#9f4676","#581b9a","#4257b2","#20a0bd")

base_color <- "#f2a264"
color_pal <- colorRampPalette(c(base_color, "#F8D0B1"))
(color_vector <- color_pal(5))
# [1] "#F2A264" "#F5B98A" "#F8D0B1" "#FBE7D8" "#FFFFFF"
# [1] "#E57057" "#EB9381" "#F2B7AB" "#F8DBD5" "#FFFFFF"
# [1] "#9F4676" "#B77498" "#CFA2BA" "#E7D0DC" "#FFFFFF"
# [1] "#581B9A" "#8154B3" "#AB8DCC" "#D5C6E5" "#FFFFFF"
# [1] "#4257B2" "#7181C5" "#A0ABD8" "#CFD5EB" "#FFFFFF"
# [1] "#20A0BD" "#57B7CD" "#8FCFDE" "#C7E7EE" "#FFFFFF"

af_col = c("#4257B2","#596CBB","#7181C4","#8895CE","#A0ABD8") %>% rev()
as_col = c("#F2A264","#F3AD77","#F4B98A","#F6C49D","#F8D0B1") %>% rev()
eu_col = c("#20A0BD","#3BABC5","#57B7CD","#73C3D5","#8FCFDE") %>% rev()
na_col = c("#E57057","#E8816C","#EB9381","#EEA595","#F2B7AB") %>% rev()
oc_col = c("#581B9A","#6C37A6","#8154B3","#9670BF","#AB8DCC") %>% rev()
sa_col = c("#9F4676","#AB5D87","#B77498","#C38BA9","#CFA2BA") %>% rev()
newcol = c(af_col, as_col, eu_col, na_col, oc_col, sa_col)
gaul_haz_sum$col = newcol



## PLOT -------------------------------------------------------------------

## 数值分布
# newgaul %>% ggplot(aes((haz))) + # log
#   geom_histogram(binwidth = .01, alpha = .9, color = "white")

range(newgaul$haz.raw)

newgaul %>% ggplot(aes((x = haz.raw))) + # log
  geom_histogram(aes(y = after_stat(count / sum(count))), 
                 bins = 150, # binwidth = .1, , color = "white", size = .1
                 alpha = .7, fill = "#9f4a4c", color = NA) + 
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(),
        legend.position = c(0.1, 0.9),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.justification = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 1, .2), 
                     expand = expansion(mult = c(0,0))) +
  scale_y_continuous(limits = c(0, .015),
                     breaks = seq(0, .015, .003), 
                     expand = expansion(mult = c(0,0)), 
                     labels = \(x){sprintf("%0.1f",x*100)}) +
  coord_cartesian(clip = "off") + 
  labs(x = ("hazard"), y = "Frequency (%)\n")
ggsave("final/1_haz_freq.pdf", width = 4, height = 2, scale = 1.5)




## plot
read_sf("final/tess_glo_valid_4_plot_new.gpkg") -> tess_plot
tess_plot %<>% left_join(newgaul[c(1,15,16)], by = "GRID_ID")
glimpse(tess_plot)

## crs
target_crs <- st_crs("+proj=robin +datum=WGS84 +no_defs +over")
tess_plot %<>% st_transform(crs = target_crs)

## palette
colors <- colorRampPalette(
  c(paletteer_c("grDevices::Heat", n = 10)) # , "#f9f9f9"
)(100) %>% rev()

## plot
tess_plot %>% ggplot() +
  geom_sf(aes(fill = haz.raw), color = NA, size = 0) +
  coord_sf(crs = target_crs) +
  scale_fill_gradientn(colours = colors,
                       trans = "sqrt",
                       na.value = "#f1f1f1",
                       breaks = c(0.02, 0.93),
                       labels = c("low", "high"),
                       guide = guide_colorbar(
                         direction = "vertical",
                         barheight = unit(60, units = "mm"),
                         barwidth = unit(3, units = "mm"),
                         draw.ulim = FALSE,
                         ticks.colour = "transparent",
                         title.position = "right",
                         title.hjust = 0.5,
                         label.hjust = 0.5)) +
  theme_pander() + # theme_map / theme_pander
  theme(panel.grid = element_blank(),
        axis.line = element_blank(), 
        legend.title = element_text(face = "bold", angle = 90), 
        legend.text = element_text(angle = 90)) +
  labs(fill = "susceptibility") -> haz_plot

## SAVE
ggsave("final/1_susceptibility_Heat_haz.pdf", haz_plot, 
       width = 5, height = 2, scale = 1.7)































# DROP --------------------------------------------------------------------

rescale <- function(x, type = 1) {
  rng <- range(x, na.rm = TRUE)
  if (type == 1) {
    (x - rng[1]) / (rng[2] - rng[1])
  } else {
    (rng[2] - x) / (rng[2] - rng[1])
  }
}
















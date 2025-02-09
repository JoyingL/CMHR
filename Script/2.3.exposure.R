# newgaul %>% write_rds("final/MHS_final.rds")
newgaul <- read_rds("final/MHS_final.rds")
glimpse(newgaul)


# EXPOSURE ----------------------------------------------------------------
(myexp = read_xlsx("final/exposure_final.xlsx")) # 209,822
sapply(myexp, range) # exp 已归一化

## 连接 exposre 数据
newgaul %<>% select(1:20)
newgaul %<>% left_join(myexp[c(1,9)], by = "GRID_ID")
glimpse(newgaul)


# newgaul %>% na.omit()

# PLOT --------------------------------------------------------------------
## 数值分布
newgaul %>% ggplot(aes((exp))) + # log
  geom_histogram(binwidth = .01, alpha = .9, color = "white")


range(newgaul$exp)

newgaul %>% ggplot(aes((x = exp))) + # log
  geom_histogram(aes(y = after_stat(count / sum(count))), 
                 bins = 150, # binwidth = .1, , color = "white"
                 alpha = .7, fill = "#789dbd", color = NA) + 
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
  scale_y_continuous(limits = c(0, .08),
                     breaks = seq(0, .08, .02),
                     expand = expansion(mult = c(0,0)), 
                     labels = \(x){sprintf("%0.1f",x*100)}) +
  coord_cartesian(clip = "off") + 
  labs(x = ("value"), y = "Frequency (%)\n")
ggsave("final/3_exp_freq.pdf", width = 4, height = 2, scale = 1.5)



## 归一化
library(bestNormalize)
myboxcox <- function(x){
  tmp = x
  obj = yeojohnson(tmp)
  p = predict(obj)
  return(p)
}
newgaul %<>% mutate(exp.bc = myboxcox(exp))

bruceR::Describe(newgaul$exp)
# Descriptive Statistics:
# ─────────────────────────────────────────────────────────────
#         N (NA) Mean   SD | Median  Min  Max Skewness Kurtosis
# ─────────────────────────────────────────────────────────────
#    199103   54 0.37 0.08 |   0.35 0.16 1.00     1.69     3.82
# ─────────────────────────────────────────────────────────────
bruceR::Describe(newgaul$exp.bc)
# Descriptive Statistics:
# ──────────────────────────────────────────────────────────────
#         N (NA) Mean   SD | Median   Min  Max Skewness Kurtosis
# ──────────────────────────────────────────────────────────────
#    199103   54 0.00 1.00 |  -0.16 -5.53 3.77     0.36     1.46
# ──────────────────────────────────────────────────────────────
sapply(newgaul[c(19,20)], range, na.rm = T)


## plot
read_sf("final/tess_glo_valid_4_plot_new.gpkg") -> tess_plot
tess_plot %<>% left_join(newgaul[c(1,19,20)], by = "GRID_ID")
glimpse(tess_plot)

## crs
target_crs <- st_crs("+proj=robin +datum=WGS84 +no_defs +over")
tess_plot %<>% st_transform(crs = target_crs)

## palette
pal_c = c('#025259', '#007172', '#f4e2de', '#f29325', '#d94f04')


colors <- colorRampPalette(
  c(paletteer_c("ggthemes::Classic Orange-White-Blue", n = 10))
)(100) %>% rev()

## plot
tess_plot %>% ggplot() +
  geom_sf(aes(fill = exp), color = NA, size = 0) +
  coord_sf(crs = target_crs) +
  scale_fill_gradientn(colours = colors,
                       trans = "log",
                       na.value = "#f1f1f1",
                       breaks = c(.17,.9),
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
  labs(fill = "exposure") -> exp_plot

ggsave("final/3_exposure_OrBu_exp2.pdf", exp_plot, 
       width = 5, height = 2, scale = 1.7)









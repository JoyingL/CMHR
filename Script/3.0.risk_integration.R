
rm(list = ls())
# HEADER ---------------------------------------------------------------
pacman::p_load(data.table, tidyverse, readxl, writexl, magrittr, naniar)
pacman::p_load(ggthemes, patchwork, paletteer, ggrepel)


library(poweRlaw)
library(ggstatsplot)
library(future)


library(urbnthemes)
set_urbn_defaults(style = "print")
urbnthemes::lato_import()
library(extrafont)
loadfonts(device = "win", quiet = F)



# LOAD -----------------------------------------------------------------


gaul <- read_rds("Data/final/MHS_final_tess.rds")
glimpse(gaul) # 183,563 × 26
miss_var_summary(gaul)



# RISK --------------------------------------------------------------------

# sapply(gaul[c(16,18,20)], range)
# gaul %<>% mutate(risk = haz * vul * exp, risk = risk / max(risk))
# glimpse(gaul)

sapply(gaul[c(22,23)], range)
bruceR::Describe(gaul$risk)



# PLOT0 ----------------------------------------------------------------

### 数值分布 ### 

gaul %>% ggplot(aes((risk))) + # log
  geom_histogram(binwidth = .01, alpha = .9, color = "white")
range(gaul$risk)

gaul %>%
  ggplot(aes(x = risk)) +
  geom_density(aes(y = after_stat(count / sum(count))),
               alpha = .7, fill = "grey") + # "#8fcbe7"
  # geom_histogram(aes(y = after_stat(count / sum(count))),
  #                binwidth = 0.0005,  # Adjust the binwidth as needed
  #                alpha = 0.7, fill = "#8fcbe7") + # "#8fcbe7"
  # theme_minimal() +
  scale_x_continuous(breaks = seq(0, 1, .2),
                     expand = expansion(mult = c(0,0))) +
  scale_y_continuous(expand = expansion(mult = c(0,0)),
                     labels = \(x) {sprintf("%0.1f", x*100)}) +
  coord_cartesian(clip = "off") +
  labs(x = "Value", y = "Frequency (%)\n")
ggsave("Output/SpFig/0_risk_freq.pdf", width = 3, height = 2, scale = 1.5)








# Fig.1c ---------------------------------------------------------------


## normalization
myboxcox <- function(x){
  tmp = x
  obj = yeojohnson(tmp)
  p = predict(obj)
  return(p)
}
newgaul %<>% mutate(risk.bc = myboxcox(risk))

bruceR::Describe(newgaul$risk)
# Descriptive Statistics:
# ────────────────────────────────────────────────────────
#         N Mean   SD | Median  Min  Max Skewness Kurtosis
# ────────────────────────────────────────────────────────
#    184146 0.10 0.10 |   0.07 0.00 1.00     1.67     3.84
# ────────────────────────────────────────────────────────
bruceR::Describe(newgaul$risk.bc)
# Descriptive Statistics:
# ──────────────────────────────────────────────────────────
#         N  Mean   SD | Median   Min  Max Skewness Kurtosis
# ──────────────────────────────────────────────────────────
#    184146 -0.00 1.00 |  -0.15 -1.37 2.84     0.45    -0.86
# ──────────────────────────────────────────────────────────
sapply(newgaul[c(21,22)], range)


## plot
read_sf("final/tess_glo_valid_4_plot_new.gpkg") -> tess_plot
tess_plot %<>% left_join(newgaul[c(1,23,24)], by = "GRID_ID")
glimpse(tess_plot)

## crs
target_crs <- st_crs("+proj=robin +datum=WGS84 +no_defs +over")
tess_plot %<>% st_transform(crs = target_crs)

## palette
colors <- colorRampPalette(
  c("#000003", paletteer_c("grDevices::Spectral", n = 10))
)(100) %>% rev()

## plot
tess_plot %>% ggplot() +
  geom_sf(aes(fill = risk.bc), color = NA, size = 0) +
  coord_sf(crs = target_crs) +
  scale_fill_gradientn(colours = colors,
                       # trans = "log",
                       na.value = "#f1f1f1",
                       breaks = c(-1.3, 2.8),
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
  labs(fill = "risk") -> risk_plot

ggsave("final/0_risk_Spectral_risk_bc.pdf", risk_plot, 
       width = 5, height = 2, scale = 1.7)






# Fig.S13 --------------------------------------------------------------

### coupling relationships ###


# glimpse(gaul)
# sapply(gaul[c(16,18,20)], range)


range(gaul$wop)
(gaul_wop <- gaul %>% filter(wop > 100)) # 130,910


## risk, weak, medium, strong
library(classInt)
itvs <- classIntervals(gaul$risk, style = "fisher", n = 3)
print(itvs) ; brks = itvs$brks
gaul %<>% mutate(risk_tag = cut(risk, brks, 1:3, include.lowest = T))
# itvs <- classIntervals(gaul$risk, style = "headtails")
# brks <- c(brks[1], brks[2], brks[7], max(brks)) # 合并


## worldpop, small, big, large
# itvs <- classIntervals(gaul$wop, style = "headtails")
itvs <- classIntervals(gaul$haz.raw, style = "fisher", n = 3)
print(itvs) ; brks = itvs$brks # break points
# brks <- c(brks[1], brks[2], brks[3], max(brks)) # combine
# gaul %<>% mutate(wop_tag = cut(wop, brks, 1:3, include.lowest = T))
gaul %<>% mutate(haz_tag = cut(haz.raw, brks, 1:3, include.lowest = T))
table(gaul$haz_tag)


# color
comb <- tibble(
  "3 - 3" = "#581b9a", # high risk, large city
  "2 - 3" = "#4257b2",
  "1 - 3" = "#20a0bd", # low risk, large city
  "3 - 2" = "#9f4676",
  "2 - 2" = "#9d4edd", # medium risk, medium city
  "1 - 2" = "#8fcbe7",
  "3 - 1" = "#e57057", # high risk, low city
  "2 - 1" = "#f2a264",
  "1 - 1" = "#C8E6E6" # low risk, low city
) %>% gather("group", "color")
# comb <- tibble(
#   "3 - 3" = "#281c86", # high risk, large city
#   "2 - 3" = "#7b3a94",
#   "1 - 3" = "#ce5aa4", # low risk, large city
#   "3 - 2" = "#4663a8",
#   "2 - 2" = "#8e80ba", # medium risk, medium city
#   "1 - 2" = "#d79fc7",
#   "3 - 1" = "#69abcc", # high risk, low city
#   "2 - 1" = "#a5c9df",
#   "1 - 1" = "#e7e6ee" # low risk, low city
# ) %>% gather("group", "color")

dtplot <- gaul %>% select(c("vul", "exp", "risk", "risk_tag", "haz_tag")) %>% 
  mutate(group = paste(as.numeric(risk_tag), "-", as.numeric(haz_tag))) %>%
  left_join(comb, by = "group")

dtplot
miss_var_summary(dtplot)
dtplot %>% slice_min(exp, n = 10)

## plot
library(ggforce)
dtplot %>% arrange(haz_tag) %>% 
  ggplot() + 
  geom_point(data = dtplot %>% filter(!is.na(haz_tag)) %>% arrange(risk_tag), 
             aes(x = (vul), y = (exp), size = (risk), color = color),
             alpha = 0.3, shape = 16) + # alpha = risk
  geom_point(
    data = dtplot %>% slice_max(risk, n = 50),
    aes(x = vul, y = exp), color = "black", size = 3, shape = 18) +
  geom_ellipse(
    data = dtplot %>% slice_max(risk, n = 100),
    aes(x0 = 0.8, y0 = 0.77, a = 0.22, b = 0.18, angle = 70),
    color = "#d2dae5", fill = NA, linetype = "dashed") +
  scale_color_identity() +
  scale_size_continuous(
    range = c(.1, 5), trans = "exp",
    guide = guide_legend(
      direction = "vertical",
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = 0.5)) +
  labs(x = "Vul", y = "Exp", size = "Risk") + 
  scale_x_continuous(expand = expansion(mult = c(0.003, 0)), limits = c(0,1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.003)), limits = c(0,1)) + 
  scatter_grid() +
  coord_cartesian(clip = "off") +
  theme(legend.position = "right", # "right" "none"
        legend.title = element_text(face = "bold", size = 12)) -> risk_plot
# risk_plot

## legend
(comb.plot <- comb %>%
    separate(group, into = c("risk", "hazard"), sep = " - ") %>%
    mutate(risk = as.integer(risk), hazard = as.integer(hazard)))

(comb.plot %>% ggplot() +
    geom_tile(aes(x = risk, y = hazard, fill = color)) +
    scale_fill_identity() +
    labs(x = "Risk", y = "Hazard") +
    theme(axis.title = element_text(size = 10), 
          axis.title.y = element_text(angle = 90)) +
    coord_fixed() + theme_minimal() -> legend)

library(cowplot)
ggdraw() + 
  draw_plot(risk_plot, 0, 0, 1, 1) +
  draw_plot(legend, 0.1, .3, .1, 1) -> mycowplot


ggsave("Output/SpFig/HVE_coupling_cowplot_new3.pdf", 
       width = 5, height = 3, scale = 3)












# DROP -----------------------------------------------------------------



# comb <- tibble(
#   "2 - 2" = "#3F2949", # high vul, high risk
#   "1 - 1" = "#CABED0", # low vul, low risk
#   "1 - 2" = "#4885C1", # low vul, high risk
#   "2 - 1" = "#BC7C8F", # high vul, low risk
# ) %>% gather("group", "color")



# rescale <- function(x, type = 1) {
#   rng <- range(x, na.rm = TRUE)
#   if (type == 1) {
#     (x - rng[1]) / (rng[2] - rng[1])
#   } else {
#     (rng[2] - x) / (rng[2] - rng[1])
#   }
# }





















rm(list = ls())
# HEADER ---------------------------------------------------------------
pacman::p_load(data.table, tidyverse, readxl, writexl, magrittr, naniar)
pacman::p_load(rmapshaper, sf, terra, mapview, ggspatial)
pacman::p_load(paletteer, ggthemes, patchwork, ggrepel)


library(urbnthemes)
set_urbn_defaults(style = "print")
library(extrafont)
loadfonts(device = "win", quiet = F)

library(ggstatsplot)
urbnthemes::lato_import()



# LOAD -----------------------------------------------------------------

gaul <- read_rds("Data/final/MHS_final_tess.rds")
glimpse(gaul)

RCM <- read_rds("Data/final/MHS_final_comm.rds")
glimpse(RCM) # 916




# Fig.2b ---------------------------------------------------------------
### individual RCM to illustrate characteristics of quadrants ###


tess_plot <- read_sf("Data/studyarea/tess_glo_valid_4_plot_tess.gpkg")


## select RCM index ###
(tess_plot %>% filter(COMM == "348") -> tess_tmp)

tess_tmp %>% ggplot() +
  geom_sf(aes(fill = risk), color = NA, size = 0) +
  coord_sf(crs = target_crs) +
  paletteer::scale_fill_paletteer_c(
    "grDevices::Spectral",
    trans = "sqrt", limits = c(0,1), # normalization
    direction = -1, na.value = "#f1f1f1",
    breaks = range(tess_tmp$risk),
    labels = c("low", "high"),
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(3, units = "mm"),
      barwidth = unit(50, units = "mm"),
      ticks.colour = "black",
      label.hjust = 0.7
    )
  ) +
  theme_pander() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_blank(),
    legend.position = "bottom") +
  labs(
    fill = NULL,
    subtitle = paste("RCM:",tess_tmp$COMM[1]," ","ISO:",tess_tmp$ISO3[1]),
  )




# Fig.3a  -----------------------------------------------------------------
### RCM demo plot for individual country ###


tess_plot <- read_sf("Data/studyarea/tess_glo_valid_4_plot_tess.gpkg")


## crs
# target_crs <- st_crs("+proj=robin +datum=WGS84 +no_defs +over")
# tess_plot %<>% st_transform(crs = target_crs)
tess_plot %<>% st_transform(crs = 4326)


## select country demo
mycountry = "U.K. of Great Britain and Northern Ireland"
tess_plot %>% filter(adm0_name == mtcountry) -> tess_tmp
(n = length(unique(tess_tmp$COMM))) # 需要的颜色数量

set.seed(888)
colors = paletteer::paletteer_d("ggsci::springfield_simpsons")
colors = rep(colors, ceiling(n / length(colors)))


## boundary line
border <- read_sf("Data/studyarea/gaul_1_intersect.gpkg")
border %>% filter(adm0_name == mycountry) -> basemap


### plot individual RCMs of a country
tess_tmp %>% ggplot() +
  geom_sf(aes(fill = COMM), color = NA, size = 0) +
  geom_sf(data = basemap, color = "grey99", fill = NA) +
  scale_fill_manual(values = colors, na.value = "#f7f7f7") +
  theme_pander() +
  theme(
    legend.title = element_text(face = "bold"),
    legend.key.size = unit(1, "lines"),
    # panel.grid = element_blank()
  ) + 
  # guides(fill = "none") +
  labs(fill = "RCMs", caption = paste("ISO:", mycountry)) # -> mygraph



### plot risk distribution of a country
tess_tmp %>% ggplot() +
  geom_sf(aes(fill = risk), color = NA, size = 0, show.legend = F) +
  paletteer::scale_fill_paletteer_c(
    "grDevices::Spectral",
    na.value = "#f1f1f1",
    trans = "sqrt", direction = -1
  ) +
  theme_pander() +
  # theme(# panel.grid = element_blank()) +
  labs(caption = paste("Risk map of", mycountry)) # -> myrisk


myrisk | mygraph
ggsave("Output/Demo/DEMO_GN_CHN.pdf", width = 5, height = 2, scale = 3)





# FUNC ----------------------------------------------------------------


tess_plot <- read_sf("Data/studyarea/tess_glo_valid_4_plot_tess.gpkg")
tess_plot %<>% st_transform(crs = 4326)

# comm_plot <- read_sf("Data/studyarea/tess_glo_valid_4_plot_comm_union.gpkg")
# comm_plot %<>% st_transform(crs = 4326) %>% left_join(RCM_exp)

border <- read_sf("Data/studyarea/gaul_1_border.gpkg")
comm_border <- read_rds("Data/studyarea/comm_inter_border.rds")

mycowplotfun <- function(country){
  set.seed(888)
  
  tess_plot %>% filter(ISO3 == country) -> tess_tmp
  comm_border %>% filter(ISO3 == country) -> basemap
  
  # comm_plot %>% filter(ISO3 == country) -> comm_tmp
  # border %>% filter(ISO3 == country) -> basemap
  
  # n = length(unique(tess_tmp$COMM))
  # colors = paletteer_d("ggsci::default_igv", n)
  
  n = length(unique(tess_tmp$COMM))
  colors = paletteer_d("ggsci::springfield_simpsons")
  colors = rep(colors, ceiling(n / length(colors)))
  
  ## mygraph
  tess_tmp %>% ggplot() +
    geom_sf(aes(fill = COMM), color = NA, size = 0) +
    # geom_sf(data = basemap, color = "grey99", fill = NA) +
    scale_fill_manual(values = colors, na.value = "#f7f7f7") +
    theme_pander() +
    theme(
      legend.title = element_text(face = "bold"),
      legend.key.size = unit(1, "lines"),
      panel.grid = element_blank()
    ) + 
    guides(fill = "none") +
    labs(fill = "RCMs", caption = paste("ISO:", country)) -> mygraph
  
  ## myrisk
  tess_tmp %>% ggplot() +
    geom_sf(aes(fill = risk), color = NA, size = 0) +
    geom_sf(data = basemap, color = "black", fill = NA) +
    paletteer::scale_fill_paletteer_c(
      "grDevices::Spectral",
      na.value = "#f1f1f1",
      trans = "sqrt", direction = -1
    ) +
    theme_pander() +
    theme(panel.grid = element_blank()) +
    guides(fill = "none") +
    labs(caption = paste("Risk map of", country)) -> myrisk
  
  return(myrisk | mygraph)
}



mycowplotfun("CHN")
mycowplotfun("GBR")
mycowplotfun("USA")
mycowplotfun("MEX")
ggsave("Output/EDFig/myrisk_mygraph_demo/DEMO_GN_GBR.pdf",
       width = 5, height = 2, scale = 3)












mycowplotfun("Montenegro")
mycowplotfun("Guatemala")
mycowplotfun("Cambodia")
mycowplotfun("Solomon Islands")
mycowplotfun("Turkey")
mycowplotfun("France")
mycowplotfun("Portugal")
mycowplotfun("Sweden") #*
mycowplotfun("Spain") #*
mycowplotfun("United States of America") #*
mycowplotfun("Egypt")
mycowplotfun("Ethiopia")
mycowplotfun("Saudi Arabia")
mycowplotfun("Kenya")
mycowplotfun("Brazil")

mycowplotfun("Yemen")
mycowplotfun("United Republic of Tanzania")
mycowplotfun("Tunisia")
mycowplotfun("Solomon Islands")
mycowplotfun("El Salvador")
mycowplotfun("Sierra Leone")
mycowplotfun("Gaza Strip")
mycowplotfun("Comoros")
mycowplotfun("Ethiopia")
mycowplotfun("Algeria")
mycowplotfun("China")











# DROP -----------------------------------------------------------------


n = length(unique(tess_tmp$COMM))
colors = paletteer_d("ggsci::springfield_simpsons")
colors = rep(colors, ceiling(n / length(colors)))


# coord_sf(crs = target_crs) +
  guide = guide_colorbar(
    direction = "vertical",
    barheight = unit(60, units = "mm"),
    barwidth = unit(3, units = "mm"),
    draw.ulim = FALSE,
    ticks.colour = "transparent",
    title.position = "right",
    title.hjust = 0.5,
    label.hjust = 0.5
  ) + 
  theme(
    # panel.grid = element_blank(),
    legend.title = element_text(face = "bold", angle = 90),
    legend.text = element_text(angle = 90)
  )
  # scale_x_continuous(limits = c(8455401,1561069)) +
  # scale_y_continuous(limits = c(5871883,6778004)) +


# ## RCM intersect 相交线
# comm_inter <- st_intersection(comm_plot)
# comm_border <- comm_inter %>% st_as_sf() %>%
#   filter(st_geometry_type(geom) == "MULTILINESTRING" |
#          st_geometry_type(geom) == "LINESTRING")
# comm_border %>% write_rds("Data/studyarea/comm_inter_border.rds")




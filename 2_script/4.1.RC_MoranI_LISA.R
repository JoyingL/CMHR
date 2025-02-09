

# Moran's I for community datasets -------------------------------

tess_valid_rm <- read_rds("Data/graph/tess_valid_rm.rds")
gaul <- read_rds("data/final/MHS_final_tess.rds")

tess_valid_rm %<>% inner_join(gaul[, c(1, 27)])
tess_valid_rm[c(1, 24, 30)] |> glimpse()
(n <- unique(tess_valid_rm$COMM)) # [1] 916

## calculate Moran's I for each RCM
myMoranI <- function(ID) {
  # data
  tess_valid_rm %>% filter(COMM == ID) -> tess_tmp
  # spatial relations
  nb <- tryCatch(
    {
      poly2nb(tess_tmp, queen = TRUE)
    },
    error = function(e) {
      print(paste("Error in poly2nb for ID:", ID))

      tess_tmp <- tess_tmp[card(nb) != 0, ]
      nb <- poly2nb(tess_tmp, queen = TRUE)

      return(nb)
    }
  )
  if (is.null(nb)) {
    return(tibble(COMM = ID))
  }
  lw <- nb2listw(nb, style = "W")
  # moran.test
  moran <- tryCatch(
    {
      moran.test(tess_tmp$risk, lw)
    },
    error = function(e) {
      print(paste("Error in moran.test for ID:", ID))
      return(NULL)
    }
  )
  if (is.null(moran)) {
    return(NULL)
  }
  # value
  tibble(
    COMM = ID,
    MoranI = moran$estimate[1], alter = moran$alternative,
    statistic = moran$statistic, p.value = moran$p.value
  )
}

myMoranI.result <- map_dfr(n, myMoranI)
myMoranI.result.revised <- myMoranI.result %>%
  mutate(
    COMM = factor(COMM),
    MoranI = ifelse(p.value >= 0.1, 0, MoranI),
    alter = ifelse(p.value >= 0.1, "random", alter)
  ) %>%
  filter(MoranI != 0)


# tess_plot <- read_sf("Data/studyarea/tess_glo_valid_4_plot_new.gpkg") %>%
#   inner_join(gaul[ ,c(1,27)]) %>% group_by(COMM) %>%
#   summarise(geom = st_union(geom))
# tess_plot %>% write_sf("Data/studyarea/tess_glo_valid_4_plot_comm_union.gpkg")


### plot 1 - global map
tess_plot <- read_sf("Data/studyarea/tess_glo_valid_4_plot_comm_union.gpkg")
tess_plot %<>% left_join(myMoranI.result.revised)

target_crs <- st_crs("+proj=robin +datum=WGS84 +no_defs +over")
tess_plot %<>% st_transform(crs = target_crs)

tess_plot %>% # p.value < 0.1
  ggplot() +
  geom_sf(aes(fill = (MoranI)), color = NA, size = 0) +
  paletteer::scale_fill_paletteer_c(
    "grDevices::Purple-Yellow",
    direction = -1,
    # trans = "exp",
    na.value = "grey90",
    guide = guide_colorbar(
      direction = "vertical",
      barheight = unit(70, units = "mm"),
      barwidth = unit(3, units = "mm"),
      draw.ulim = FALSE,
      ticks.colour = "transparent",
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = 0.5
    )
  ) +
  coord_sf(crs = target_crs) +
  theme_pander() +
  theme(
    legend.title = element_text(face = "bold"),
    legend.direction = "vertical",
    legend.position = "right",
    panel.grid = element_blank(),
    axis.line = element_blank()
  ) +
  labs(x = NULL, y = NULL, fill = "Moran's I") -> Moran.plot
ggsave("Output/ExtendDataFig/RCM_MoranI2.pdf", Moran.plot, width = 7, height = 3, scale = 1.5)


### plot 2 - gghistostats

library(ggstatsplot)
bruceR::Describe(myMoranI.result.revised$MoranI)

gghistostats(
  myMoranI.result.revised,
  x = MoranI,
  normal.curve = T, results.subtitle = F
)
ggsave("Output/ExtendDataFig/RCM_MoranI_hist.pdf", width = 5, height = 2, scale = 2)


# shapiro.test(myMoranI.result.revised$MoranI)
# # Shapiro-Wilk normality test
# #
# # data:  myMoranI.result.revised$MoranI
# # W = 0.99128, p-value = 5.341e-05
# # 拒绝服从正态分布的零假设




# LISA within each RCM -------------------------------------------------


myLISAcal <- function(ID) {
  # data
  tess_valid_rm %>% filter(COMM == ID) -> tess_tmp
  # spatial relations
  nb <- tryCatch(
    {
      poly2nb(tess_tmp, queen = TRUE)
    },
    error = function(e) {
      print(paste("Error in poly2nb for ID:", ID))

      tess_tmp <- tess_tmp[card(nb) != 0, ]
      nb <- poly2nb(tess_tmp, queen = TRUE)

      return(nb)
    }
  )
  if (is.null(nb)) {
    return(tibble(COMM = ID))
  }
  lw <- nb2listw(nb, style = "W")
  # localC
  cc <- spdep::localmoran(tess_tmp$risk, lw)
  # FDR
  pr <- p.adjust(data.frame(cc)[, 5], method = "fdr")
  # result
  cbind(tess_tmp[c(1, 24)], lisa = attr(cc, "quadr")$pysal, pr)
}


n <- unique(tess_valid_rm$COMM) # [1] 916
myLISA.result <- map_dfr(n, myLISAcal)


### join LISA results based on each community analysis ###
### how high and low are each group ###
myLISA.result %>% st_drop_geometry() %>% data.table() -> tmp
tmp %>% filter(pr < 0.1) %>% 
  left_join(gaul[,c(1,27)]) %>% 
  group_by(COMM) %>% count(lisa) %>% 
  pivot_wider(names_from = lisa, values_from = n) %>% 
  right_join(RCM) %>% 
  relocate(`High-High`, `Low-Low`, `Low-High`, `High-Low`, 
           .after = GNS) -> RCM
# RCM %>% write_rds("Data/final/MHS_final_comm.rds")
# RCM <- read_rds("Data/final/MHS_final_comm.rds")



## plot
# tess_plot <- read_sf("Data/studyarea/tess_glo_valid_4_plot_new(origin).gpkg")
# tess_plot <- tess_plot %>% inner_join(gaul)
# tess_plot %>% write_sf("data/studyarea/tess_glo_valid_4_plot_tess.gpkg")


tess_plot <- read_sf("data/studyarea/tess_glo_valid_4_plot_tess.gpkg")
tess_plot %<>% left_join(st_drop_geometry(myLISA.result), by = "GRID_ID")

read_sf("data/studyarea/tess_glo_valid_4_plot_comm_union.gpkg") %>% 
  st_intersection() %>% st_as_sf() %>% 
  filter(st_geometry_type(geom) == "MULTILINESTRING" |
         st_geometry_type(geom) == "LINESTRING") -> tess_border


target_crs <- st_crs("+proj=robin +datum=WGS84 +no_defs +over")
tess_plot %<>% st_transform(crs = target_crs)

tess_plot %>%
  mutate(lisa = case_when(pr >= 0.1 ~ NA, TRUE ~ as.factor(lisa))) %>%
  ggplot() +
  geom_sf(aes(fill = lisa), color = NA, size = 0) +
  geom_sf(data = tess_border, color = "white", linewidth = 0.03) +
  coord_sf(crs = target_crs) +
  scale_fill_manual("LISA",
    # values = c("#de3a77", "#3a9261", "#1467d3", "#e88450"),
    # labels = c("H-H", "L-L", "(Other)Positive", "Negative"),
    values = c("#3a9261", "#1467d3", "#e88450", "#de3a77"),
    labels = c("Low-Low", "High-Low", "Low-High", "High-High"),
    na.value = "grey90"
  ) +
  theme_pander() +
  theme(
    legend.title = element_text(face = "bold"),
    legend.key.size = unit(1, "lines"),
    legend.position = "right",
    legend.spacing.x = unit(.7, "lines"),
    panel.grid = element_blank()
  ) -> lisa_within_RCM_plot
ggsave("Output/ExtendDataFig/LISA_withFDR_within_RCM.pdf", 
       lisa_within_RCM_plot, width = 7, height = 3)











# DROP -----------------------------------------------------------------
tess_valid_rm %>% filter(COMM == 3) -> tess_tmp
nb <- poly2nb(tess_tmp, queen = TRUE)
lw <- nb2listw(nb, style = "W")
cc <- spdep::localmoran(tess_tmp$risk, lw)
data.frame(cc)[, 5]

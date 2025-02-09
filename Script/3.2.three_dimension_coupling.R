# Fig.S13 --------------------------------------------------------------

### coupling relationships ###


# glimpse(gaul)
# sapply(gaul[c(16,18,20)], range)



# weak, medium, strong
library(classInt)
itvs <- classIntervals(gaul$exp, style = "fisher", n = 3)
print(itvs) ; brks = itvs$brks
gaul %<>% mutate(exp_tag = cut(exp, brks, 1:3, include.lowest = T))

itvs <- classIntervals(gaul$vul, style = "fisher", n = 3)
print(itvs) ; brks = itvs$brks
gaul %<>% mutate(vul_tag = cut(vul, brks, 1:3, include.lowest = T))


# color
comb <- tibble(
  "3 - 3" = "#581b9a", # high vul, high exp
  "2 - 3" = "#4257b2",
  "1 - 3" = "#20a0bd", # low vul, large exp
  "3 - 2" = "#9f4676",
  "2 - 2" = "#9d4edd", # medium vul, medium exp
  "1 - 2" = "#8fcbe7",
  "3 - 1" = "#e57057", # high vul, low exp
  "2 - 1" = "#f2a264",
  "1 - 1" = "#C8E6E6" # low vul, low exp
) %>% gather("group", "color")


dtplot <- gaul %>% select(c("risk", "haz", "exp_tag", "vul_tag")) %>% 
  mutate(group = paste(as.numeric(vul_tag), "-", as.numeric(exp_tag))) %>%
  left_join(comb, by = "group")


## plot
library(ggforce)
dtplot %>% # arrange(risk) %>% 
  ggplot() + 
  geom_point(aes(x = (haz), y = (risk), color = color, size = (risk)), # size = (risk)
             alpha = 0.3, shape = 16, show.legend = FALSE) + # alpha = risk, size = 1
  geom_point(
    data = dtplot %>% slice_max(risk, n = 50),
    aes(x = haz, y = risk), color = "#511f94", size = 1.7, shape = 18, alpha = 0.5) +
  geom_ellipse(
    data = dtplot %>% slice_max(risk, n = 100),
    aes(x0 = 0.8, y0 = 0.77, a = 0.22, b = 0.18, angle = 70),
    color = "#d2dae5", fill = NA, linetype = "dashed") +
  scale_color_identity() +
  scale_size_continuous(
    range = c(.1, 2), trans = "exp"
    guide = guide_legend(
      direction = "vertical",
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = 0.5)
  ) +
  labs(x = "Haz", y = "Risk") + 
  scale_x_continuous(expand = expansion(mult = c(0.003, 0)), limits = c(0,1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.003)), limits = c(0,1)) + 
  scatter_grid() +
  coord_cartesian(clip = "off") +
  theme(legend.position = "right", # "right" "none"
        legend.title = element_text(face = "bold", size = 12)) -> risk_plot
# risk_plot

## legend
(comb.plot <- comb %>%
    separate(group, into = c("vul", "exp"), sep = " - ") %>%
    mutate(vul = as.integer(vul), exp = as.integer(exp)))

(comb.plot %>% ggplot() +
    geom_tile(aes(x = vul, y = exp, fill = color)) +
    scale_fill_identity() +
    labs(x = "Vulnerability", y = "Exposure") +
    theme(axis.title = element_text(size = 10), 
          axis.title.y = element_text(angle = 90)) +
    coord_fixed() + theme_minimal() -> legend)

library(cowplot)
ggdraw() + 
  draw_plot(risk_plot, 0, 0, 1, 1) +
  draw_plot(legend, 0.1, .3, .1, 1) -> mycowplot


ggsave("Output/SpFig/HVE_coupling_cowplot_new4.pdf", 
       width = 5, height = 3, scale = 3)













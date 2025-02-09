# HEADER ------------------------------------------------------------------
rm(list = ls())
pacman::p_load(
  data.table, tidyverse, magrittr, rmapshaper, 
  sf, terra, mapview, naniar,
  hrbrthemes, ggthemes, readxl, writexl
)



# EM-DAT ------------------------------------------------------------------
EMDAT <- read_csv("D:/GeoData/1_Hazard/EM-DAT/emdat_disaster_2010_now.csv") 
glimpse(EMDAT)

EMDAT.1 = EMDAT %>% filter(Year >= 2010) %>% select(c(1,11:15,35,39,45,47:50))
glimpse(EMDAT.1) # 2010 至今 


##### 计算 "Deaths", "Affected", "Damages" 三个维度的标准化加权和 #####

# weight %>% write_rds("data/2-vulnerability/EMDAT_weight.rds")
weight <- read_rds("data/2-vulnerability/EMDAT_weight.rds")

tmp = EMDAT.1[c(7:9)] %>%
  mutate(across(everything(), \(x) x/max(x, na.rm = T))) # 标准化
names(tmp) = c("Deaths", "Affected", "Damages")
tmp %<>% mutate(Deaths = Deaths * weight[1],
                Affected = Affected * weight[2],
                Damages = Damages * weight[3]) %>%
  mutate(Total = rowSums(across(everything()), na.rm = T))
tmp %<>% rowwise() %>% mutate(Total =
                                if_else(is.na(Deaths) & is.na(Affected) & is.na(Damages), NA, Total))

EMDAT.1 = EMDAT.1[-c(7:9)] %>% cbind(tmp$Total)
glimpse(EMDAT.1)
names(EMDAT.1) = c("Dis_No","Country","ISO","Region","Continent","Location",
                   "admLevel","adm1_code","adm2_code","Geo_Locations","Total")
miss_var_summary(EMDAT.1)

# EMDAT.1 %>% write_rds("data/2-vulnerability/EMDAT_Total.rds")
# EMDAT.1 <- read_rds("data/2-vulnerability/EMDAT_Total.rds")
table(EMDAT.1$admLevel)


####### admLevel == 3 #######
EMDAT.1 %>% filter(admLevel == "1;2") %>% 
  separate_rows(adm1_code, sep = ";", convert = F) %>% 
  group_by(Dis_No) %>% # 每一个事件发生对所有影响地区的平均分数
  mutate(Total.1 = Total / n()) -> EMDAT_sep_3

####### admLevel == 1 ########
EMDAT.1 %>% filter(admLevel == 1) %>%
  separate_rows(adm1_code, sep = ";", convert = F) %>%
  group_by(Dis_No) %>% # 一个事件分配给 n 个地区
  mutate(Total.1 = Total / n()) -> EMDAT_sep_1

EMDAT_sep_1 %>% rbind(EMDAT_sep_3) %>%
  group_by(adm1_code) %>% # 每个地区发生过的所有事件
  summarise(Total.1 = sum(Total.1), n = n()) -> EMDAT_sep_11

EMDAT_sep_11 %<>% mutate(adm1_code = as.numeric(adm1_code))

# EMDAT_sep_11 %>%
#   write_xlsx("D:/GeoData/data/FINAL/Vulnerability/vul_EMDAT_sep_adm1.xlsx")
# (EMDAT_1 <- read_xlsx("D:/GeoData/data/FINAL/Vulnerability/vul_EMDAT_sep_adm1.xlsx"))

####### admLevel == 2 #######
EMDAT.1 %>% filter(admLevel == 2) %>%
  separate_rows(adm2_code, sep = ";", convert = F) %>%
  group_by(Dis_No) %>%
  mutate(Total.1 = Total / n()) -> EMDAT_sep_2

EMDAT_sep_3 %>%
  separate_rows(adm2_code, sep = ";", convert = F) %>%
  rbind(EMDAT_sep_2) %>% group_by(adm2_code) %>%
  summarise(Total.1 = sum(Total.1), n = n()) -> EMDAT_sep_22

EMDAT_sep_22 %<>% mutate(adm2_code = as.numeric(adm2_code))

# EMDAT_sep_22 %>%
#   write_xlsx("D:/GeoData/data/FINAL/Vulnerability/vul_EMDAT_sep_adm2.xlsx")
# (EMDAT_2 <- read_xlsx("D:/GeoData/data/FINAL/Vulnerability/vul_EMDAT_sep_adm2.xlsx"))


####### admLevel == NA #######
EMDAT.1 %>% filter(is.na(admLevel)) %>%
  separate_rows(Location, sep = ";", convert = F) %>%
  separate_rows(Location, sep = ",", convert = F) %>%
  group_by(Dis_No) %>%
  mutate(Total.1 = Total / n()) -> EMDAT_sep_NA

EMDAT_sep_NA %>% group_by(Location) %>%
  summarise(Total.1 = sum(Total.1), n = n()) %>% na.omit() -> EMDAT_sep_NANA

EMDAT_sep_NANA = EMDAT_sep_NANA[-1,]
glimpse(EMDAT_sep_NANA)

# EMDAT_sep_NANA %>% distinct(Location) # 2076
# miss_var_summary(EMDAT_sep_NANA)
# EMDAT_sep_NANA %>% write_xlsx("D:/GeoData/data/FINAL/Vulnerability/EMDAT_sep_NANA2.xlsx")
# EMDAT_NA <- read_xlsx("D:/GeoData/data/FINAL/Vulnerability/EMDAT_sep_NANA2.xlsx")


# ### 补充爬虫得到的经纬度
# (EMDAT_sep_NA_revised <- read_csv("D:/GeoData/data/FINAL/Vulnerability/Google Maps_fuzzy_revised.csv") %>% select(c(1, 11)) %>% distinct(Location, .keep_all = T))
# ### fuzzyjoin 后导出到 GIS 连接行政区划
# library(fuzzyjoin)
# EMDAT_sep_NANA %>% 
#   stringdist_inner_join(EMDAT_sep_NA_revised, by = "Location", max_dist = 1) %>%
#   write_xlsx("D:/GeoData/data/FINAL/Vulnerability/vul_EMDAT_sep_NA2.xlsx")


### 重新连接到 adm1 和 CHN-adm2 合并到adm2
# 每个 location 就相当于每个 event 来看
# EMDAT_NA_1 <- read_csv("D:/GeoData/data/FINAL/Vulnerability/vul_EMDAT_sep_NA_2010_join.csv") 
# glimpse(EMDAT_NA_1)
# (EMDAT_NA_1[c(3,4,11)] %>% relocate(adm1_code, .before = Total_1) %>%
#     rename(Total.1 = Total_1) %>%
#     rbind(EMDAT_11) %>% group_by(adm1_code) %>%
#     summarise(Total.2 = sum(Total.1), n = sum(n)) -> EMDAT_1_final)


# 对应 adm2
EMDAT_22 <- EMDAT_sep_22
EMDAT_NA_JOIN <- read_csv("D:/GeoData/data/FINAL/Vulnerability/vul_EMDAT_sep_NA_2010_join.csv") 
# glimpse(EMDAT_NA_1)
(EMDAT_NA_JOIN[c(3,4,11)] %>% relocate(adm2_code, .before = Total_1) %>%
    rename(Total.1 = Total_1) %>%
    rbind(EMDAT_22) %>% group_by(adm2_code) %>%
    summarise(Total.2 = sum(Total.1), n = sum(n)) -> EMDAT_2_final)

(EMDAT_1_final <- EMDAT_sep_11)
# range(EMDAT_1_final$Total.2, na.rm = T)
# range(EMDAT_2_final$Total.2, na.rm = T)


##### final EMDAT value #####
gaul <- read_rds("MHS-final.rds")
(EMDAT_dt <- gaul[c(1:7)]) # glimpse(EMDAT) # 对应格网的 adm

# 每个 tess 的 adm1 + adm2 + NA = 最终的 EMDAT
(EMDAT_dt %>%
    left_join(EMDAT_1_final, by = "adm1_code") %>%
    left_join(EMDAT_2_final, by = "adm2_code") -> tmp)

## 取出 total 和 n
value <- tmp[c(8,10)] %>% apply(., 1, sum, na.rm = T)
frequ <- tmp[c(9,11)] %>% apply(., 1, sum, na.rm = T)

# value = 0 你不能说是没有发生还是值为0
(tibble(GRID_ID = tmp$GRID_ID, total = value, n = frequ) -> myvul)
# myvul %<>% filter(total != 0 & n != 0)
miss_var_summary(myvul)

## 归一化
(myvul %<>% mutate(total = total/max(total), n = n/max(n)))
sapply(myvul, range)

### vulnerability = total + n ###
myvul <- myvul %>% mutate(EMDAT = total + n) %>% 
  # mutate(EMDAT = ifelse(n != 0, total / n, total)) %>% 
  mutate(EMDAT = EMDAT/max(EMDAT))

myvul %>% # filter(total > 0) %>% 
  ggplot(aes((EMDAT))) + geom_histogram(binwidth = .01, color = "white")

myvul

# (EMDAT_dt <- tibble(GRID_ID = gaul$GRID_ID, myvul) %>% tibble())
# EMDAT_dt %>% write_rds("data/2-vulnerability/vul_EMDAT_add.rds")
# (EMDAT_dt <- read_rds("data/2-vulnerability/vul_EMDAT_new.rds"))


# tibble(total, n) %>% mutate(across(everything(), std)) %>%
#   apply(., 1, \(x) sum(x * weight.vul)) -> exp.val
# EMDAT.1 <- tibble(EMDAT$GRID_ID, exp.val)
# names(EMDAT.1) = c("GRID_ID", "EMDAT")

## rescale 正向指标的归一化
# EMDAT.1 %<>% mutate(EMDAT = rescale(EMDAT))
# range(EMDAT.1$EMDAT)
# EMDAT.1 %>% write_rds("data/2-vulnerability/vul_EMDAT.rds")

## 计算加权和（不要了）
# tibble(total, n) %>% mygrey() -> weight.vul
#      total          n
# 0.98829146 0.01170854


# SHDI --------------------------------------------------------------------
SHDI <- read_csv("D:/GeoData/data/FINAL/Vulnerability/vul_SHDI_tess.csv")
glimpse(SHDI) ## summarize # 一个格子跨两个地方，取平均SHDI
(SHDI %<>% group_by(GRID_ID) %>% summarise(SHDI = mean(SHDI_avg)))
range(SHDI$SHDI, na.rm = T)
miss_var_summary(SHDI)

# ## rescale 负向指标的归一化
# SHDI.1 = SHDI %>% mutate(SHDI = rescale(SHDI, type = 2))
# range(SHDI.1$SHDI, na.rm = T)
# SHDI.1 %>% write.csv("data/2-vulnerability/vul_SHDI_tess_final.csv")
# SHDI.1 %>% write_rds("data/2-vulnerability/vul_SHDI.rds")
# SHDI.1 <- read_rds("data/2-vulnerability/vul_SHDI.rds")
# SHDI[which(SHDI$GRID_ID == "CDB-658"),]
# SHDI.1[which(SHDI.1$GRID_ID == "CDB-658"),]


# 合并 ----------------------------------------------------------------------
vul <- myvul %>% left_join(SHDI, by = "GRID_ID")
glimpse(vul) ; miss_var_summary(vul)
vul %>% write_xlsx("data/2-vulnerability/vulnerability_final.xlsx")


## SHDI 没有数据就是 NA
# 如果忽略NA直接加的话可能会导致某个地方的vul偏低（因为少了个指标呀!）
# vul %<>% na.omit()

# max(vul$EMDAT, na.rm = T)
# max(vul$SHDI, na.rm = T)
# range(vul$EMDAT)
# range(vul$SHDI)

## 计算最终 vulnerability 值
apply(vul[c(2,3)], 1, sum) -> val # 无权重 finalfinal2

(mygrey(vul[c(2,3)] ) -> weight.vul)
apply(vul[c(2,3)], 1, \(x) sum(x * weight.vul)) -> val # 有权重 finalfinal3


vul.1 <- vul %>% cbind(val)
names(vul.1) = c("GRID_ID","EMDAT","SHDI","vul")
head(vul.1)

# vul.1 %>% write_xlsx("data/2-vulnerability/vulnerability_finalfinal2.xlsx")
# vul.1 %>% write_xlsx("data/2-vulnerability/vulnerability_finalfinal3.xlsx")

# vul <- read_excel("data/2-vulnerability/vulnerability_finalfinal2.xlsx")
vul %>% filter(if_any(everything(), \(x) x == 0)) -> vul.tmp

# vul.1[which(vul.1$GRID_ID == "CDB-658"),]
# vul[which(vul$GRID_ID == "CDB-658"),]





# 按国家汇总(drop) -------------------------------------------------------------

rescale = function(x, type = 1) {
  rng = range(x, na.rm = TRUE)
  if (type == 1) {
    (x - rng[1]) / (rng[2] - rng[1])
  } else {
    (rng[2] - x) / (rng[2] - rng[1])
  }
}

# EMDAT.1 %>% group_by(ISO) %>% 
#   summarise(n = n(), value = sum(Total, na.rm = T)) -> EMDAT.2
# # miss_var_summary(EMDAT.2)
# 
# ## 计算最终EMDAT 
# # max(EMDAT.2$n) = 520
# EMDAT.2[c(2,3)] %>% mutate(n = n / 520) %>% # 标准化!!!
#   mutate(EMDAT = n * value) %>% mutate(EMDAT = std(EMDAT)) %>% 
#   select(EMDAT) %>% cbind(EMDAT.2$ISO) -> EMDAT.3
# # head(EMDAT.3)
# # range(EMDAT.3$EMDAT)
# 
# EMDAT.3 %>% write_xlsx("D:/GeoData/data/FINAL/Vulnerability/vul_EMDAT_final.xlsx")





















# newgaul %>% write_rds("final/MHS_final.rds")
newgaul <- read_rds("final/MHS_final.rds")
glimpse(newgaul)


# VULNERABILITY -----------------------------------------------------------
(myvul = read_xlsx("final/vulnerability_final.xlsx"))
miss_var_summary(myvul)
sapply(myvul[4:5], range) # EMDAT 已归一化


myvul %<>% mutate(
  SHDI = rescale(SHDI, type = 2), # SHDI 的归一化
  vul = EMDAT + SHDI, # 计算 vul
  vul = vul / max(vul)) # vul 的归一化
sapply(myvul[4:6], range)


## 连接 vulnerability 数据
newgaul %<>% dplyr::select(1:16)
newgaul %<>% left_join(myvul[c(1,6)], by = "GRID_ID")
glimpse(newgaul)


# PLOT --------------------------------------------------------------------

## 数值分布
newgaul %>% ggplot(aes((vul))) + # log
  geom_histogram(binwidth = .01, alpha = .9, color = "white")


range(newgaul$vul)

newgaul %>% ggplot(aes((x = vul))) + # log
  geom_histogram(aes(y = after_stat(count / sum(count))), 
                 bins = 150, # binwidth = .1, , color = "white"
                 alpha = .7, fill = "#d85ca0", color = NA) + 
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
  scale_y_continuous(limits = c(0, .04),
                     breaks = seq(0, .04, .01),
                     expand = expansion(mult = c(0,0)), 
                     labels = \(x){sprintf("%0.1f",x*100)}) +
  coord_cartesian(clip = "off") + 
  labs(x = ("value"), y = "Frequency (%)\n")
ggsave("final/2_vul_freq.pdf", width = 4, height = 2, scale = 1.5)



## 归一化
myboxcox <- function(x){
  tmp = x
  obj = yeojohnson(tmp)
  p = predict(obj)
  return(p)
}
newgaul %<>% mutate(vul.bc = myboxcox(vul))

bruceR::Describe(newgaul$vul)
# Descriptive Statistics:
# ────────────────────────────────────────────────────────
#         N Mean   SD | Median  Min  Max Skewness Kurtosis
# ────────────────────────────────────────────────────────
#    199157 0.26 0.17 |   0.23 0.00 1.00     0.87     0.27
# ────────────────────────────────────────────────────────
bruceR::Describe(newgaul$vul.bc)
# Descriptive Statistics:
# ─────────────────────────────────────────────────────────
#         N Mean   SD | Median   Min  Max Skewness Kurtosis
# ─────────────────────────────────────────────────────────
#    199157 0.00 1.00 |   0.05 -2.10 2.47     0.09    -0.92
# ─────────────────────────────────────────────────────────
sapply(newgaul[17:18], range)


## plot
read_sf("final/tess_glo_valid_4_plot_new.gpkg") -> tess_plot
tess_plot %<>% left_join(newgaul[c(1,17,18)], by = "GRID_ID")
glimpse(tess_plot)

## crs
target_crs <- st_crs("+proj=robin +datum=WGS84 +no_defs +over")
tess_plot %<>% st_transform(crs = target_crs)

## palette
colors <- colorRampPalette(
  c(paletteer_c("grDevices::PiYG", n = 10))
)(100) %>% rev()

## plot
tess_plot %>% ggplot() +
  geom_sf(aes(fill = vul.bc), color = NA, size = 0) +
  coord_sf(crs = target_crs) +
  scale_fill_gradientn(colours = colors,
                       # trans = "sqrt",
                       na.value = "#f1f1f1",
                       breaks = c(-2, 2.45),
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
  labs(fill = "vulnerability") -> vul_plot

ggsave("final/2_vulnerability_PiYG_vul_bc.pdf", vul_plot, 
       width = 5, height = 2, scale = 1.7)


























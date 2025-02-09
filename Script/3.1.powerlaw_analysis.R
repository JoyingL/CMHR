
rm(list = ls())
# HEADER ---------------------------------------------------------------
pacman::p_load(data.table, tidyverse, readxl, writexl, magrittr, naniar)
pacman::p_load(ggthemes, patchwork, paletteer, ggrepel)


library(poweRlaw)
library(ggstatsplot)
library(future)


# LOAD -----------------------------------------------------------------


val = gaul$risk[gaul$risk > 0]
# sample(tmp, size = 10000, replace = FALSE) -> val_sample


# POWERLAW -------------------------------------------------------------

library(poweRlaw)
### check if power-law distribution ### 


## fit power-law distributions 
m <- poweRlaw::conpl$new(val)
# est = estimate_xmin(m) # 估计截断参数 xmin 幂律指数 alpha
# est %>% write_rds("Data/powerlaw/risk_poweRlaw_est_result.rds")
est = read_rds("Data/powerlaw/risk_poweRlaw_est_result.rds")
m$setXmin(est$xmin)
m$setPars(est$pars)


## KS test power-law
library(future)
plan(multisession, workers = 30)
# availableCores()
# threads = parallel::detectCores()
system.time({
  bs <- bootstrap_p(
    m, no_of_sims = 10000, threads = parallel::detectCores())
})


# bs %>% write_rds("Data/powerlaw/risk_poweRlaw_bs_result.rds")
bs <- read_rds("Data/powerlaw/risk_poweRlaw_bs_result.rds") # str(bs)
bs$p # [1] 0.44 p > 0.05 不能拒绝零假设 即数据遵循幂律分布
bs$gof # [1] 0.01614984 优度拟合统计量 (K-S统计量较小) 拟合优度较好
# print(bs)
# plot(bs, trim = 0.1)


xmin <- m$getXmin()
alpha <- m$getPars()




# PLOT -----------------------------------------------------------------


### ECDF ###
filter(gaul, risk > 0) %>% 
  ggplot(aes(x = risk)) + stat_ecdf(linewidth = 1)


### CCDF ###
gaul$risk[gaul$risk > 0] -> val

# ecdf_val <- ecdf(val)
# ccdf_data <- data.table(
#   risk = sort(val, decreasing = TRUE),
#   ccdf = 1 - ecdf_val(sort(val, decreasing = TRUE))
# )

myccdf <- function(value) {
  ecdf_val = ecdf(value)
  data.table(risk = sort(value, decreasing = TRUE),
             ccdf = 1 - ecdf_val(sort(value, decreasing = TRUE)))
}
ccdf_results <- myccdf(val)


ggplot(ccdf_data, aes(x = risk, y = ccdf)) +
  geom_point(size = 1) +
  scale_x_log10() + 
  scale_y_log10() +  
  labs(x = "Risk", y = "CCDF") +
  theme_minimal()
ggsave("Output/SpFig/powerlaw/CCDF_risk.pdf", scale = 2,
       width = 3, height = 2)



library(ineq)
lorenz_curve <- Lc(val)
lorenz_data <- data.frame(
  p = lorenz_curve$p,
  L = lorenz_curve$L
)
ggplot(lorenz_data, aes(x = p, y = L)) +
  geom_line(size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_minimal()
ggsave("Output/SpFig/powerlaw/LORENZ_risk.pdf", scale = 2, 
       width = 3, height = 2)









# dev.new()
plot(m) ; lines(m, col = 2)
par(
  mar = c(3, 3, 2, 1), mgp = c(2, 0.5, 0),
  tck = -.01, cex.axis = 0.9, las = 1
)
plot(m,
     pch = 4,
     panel.first = grid(col = NULL), xlab = "risk", ylab = "CDF"
)
lines(m, lwd = 2, col = 2)
# dev.off()





library(pracma)



# TRUNCATED ------------------------------------------------------------


## raw data 'val' in log scale
ggplot(tibble(val), aes(x = val)) +
  geom_histogram(binwidth = 0.001, fill = "grey") +
  scale_x_log10() + # scale_y_log10() + 
  theme_minimal()


## split bins in log scale
log_bins <- exp(seq(log(min(val)), log(max(val)), length.out = 10000))
val_bins <- cut(val, breaks = log_bins, include.lowest = TRUE)


## the mean, frequency, and width of each bin
val_stat <- tapply(val, val_bins, \(x) {
  c(mean = mean(x), freq = length(x), width = diff(range(x)))
})


## filter out NULL bins
val_stat <- tibble(val_stat) %>% 
  filter(map_lgl(val_stat, ~ !is.null(.x))) %>%
  unnest_wider(val_stat) # expand the list-column
# ggplot(val_stat, aes(x = mean, y = freq)) +
#   geom_col(width = 0.1, fill = "grey") +
#   scale_x_log10() + scale_y_log10() +
#   geom_errorbar(aes(
#     ymin = freq - width / 2, ymax = freq + width / 2), width = 0.1
#   ) + theme_minimal() +
#   labs(x = "Mean (log)", y = "Frequency (log)", title = "Binned Data Statistics")


## probability density and standard error
expanded_tibble <- val_stat %>%
  mutate(
    prob = freq / sum(freq),
    stderr = sqrt(prob * (1 - prob) / sum(freq)),
    pdf = prob / width,  # 概率密度
    stderr_pdf = stderr / width  # 概率密度的标准误差
  )

## the point of maximum frequency as the turning point
expanded_tibble %>% slice_max(freq, n = 1)
#  mean  freq
# 0.147    72



# UPPER ----------------------------------------------------------------

myXmin = 0.147


## split bins in log scale
log_bins <- exp(seq(log(min(myXmin)), log(max(val)), length.out = 30))
val_bins <- cut(val, breaks = log_bins, include.lowest = TRUE)


## the mean, frequency, and width of each bin
val_stat <- tapply(val, val_bins, \(x) {
  c(mean = mean(x), freq = length(x), width = diff(range(x)))
})


## filter out NULL bins
val_stat <- tibble(val_stat) %>% 
  filter(map_lgl(val_stat, ~ !is.null(.x))) %>%
  unnest_wider(val_stat) # expand the list-column
# ggplot(val_stat, aes(x = mean, y = freq)) +
#   geom_col(width = 0.1, fill = "grey") +
#   scale_x_log10() + scale_y_log10() +
#   geom_errorbar(aes(
#     ymin = freq - width / 2, ymax = freq + width / 2), width = 0.1) +
#   theme_minimal() +
#   labs(x = "Mean (log)", y = "Frequency (log)", title = "Binned Data Statistics")


## probability density and standard error
expanded_tibble <- val_stat %>%
  mutate(
    prob = freq / sum(freq),
    stderr = sqrt(prob * (1 - prob) / sum(freq)),
    pdf = prob / width,  # 概率密度
    stderr_pdf = stderr / width  # 概率密度的标准误差
  )
# ggplot(expanded_tibble, aes(x = mean, y = pdf)) +
#   geom_point(size = 1, color = "#1696d2") +
#   geom_errorbar(aes(ymin = pdf - stderr_pdf, ymax = pdf + stderr_pdf),
#                 width = 0.01, color = "#1696d2") +
#   scale_x_log10(breaks = 10^minor_breaks,
#                 labels = scales::number(10^minor_breaks, 0.1)) +
#   scale_y_log10(labels = sciti_labels) +
#   labs(x = "Risk (r)", y = "Probability f(r)") +
#   theme_bw() +
#   theme(text = element_text(family = "serif"))




# SPLIT ----------------------------------------------------------------


## the log scale
log_tibble <- expanded_tibble %>% 
  mutate(log_mean = log(mean), log_prob = log(prob))
## split point sequence
mysplitpoi <- 2:(nrow(log_tibble) - 1)
## function of computing total error
compute_error <- function(mysplitpoi) {
  fit1 = lm(log_prob ~ log_mean, 
            data = log_tibble[1:mysplitpoi, ])
  fit2 = lm(log_prob ~ log_mean, 
            data = log_tibble[(mysplitpoi + 1):nrow(log_tibble), ])
  total_error = sum(residuals(fit1)^2) + sum(residuals(fit2)^2)
  list(split_index = mysplitpoi,
       coef_fit1 = coef(fit1),
       coef_fit2 = coef(fit2),
       total_error = total_error)
}
## find the best fit
piecewise_fits <- map_dfr(mysplitpoi, compute_error)
best_fit <- piecewise_fits %>% arrange(total_error) %>% slice(1)
## the best split point
best_split <- best_fit$split_index
(best_split_point <- log_tibble$mean[best_split])
# [1] 0.4365047
## fit the lines
final_fit1 <- lm(log_prob ~ log_mean, 
                 data = log_tibble[1:best_split,])
final_fit2 <- lm(log_prob ~ log_mean, 
                 data = log_tibble[(best_split+1):nrow(log_tibble),])
ggplot(log_tibble, aes(x = log_mean, y = log_prob)) +
  geom_point(size = 1, color = "#1696d2") +
  geom_abline(intercept = coef(final_fit1)[1], slope = coef(final_fit1)[2], col = "blue") +
  geom_abline(intercept = coef(final_fit2)[1], slope = coef(final_fit2)[2], col = "red") +
  scale_x_continuous(breaks = 10^minor_breaks, 
                     labels = scales::number(10^minor_breaks, 0.1)) +
  # scale_y_continuous(labels = sciti_labels) +
  labs(x = "Risk (r)", y = "Probability f(r)") +
  theme_bw() +
  theme(text = element_text(family = "serif"))




### using the segmented package ###
library(segmented)
val_log <- log10(expanded_tibble$mean)
fre_log <- log10(expanded_tibble$freq)
lin_mod <- lm(fre_log ~ val_log) # simple linear model

## to estimate the breakpoint and the pis parameter is an initial guess
ini_psi <- median(val_log) # mean(values_log)
seg_mod <- segmented(lin_mod, seg.Z = ~ val_log, 
                     psi = list(val_log = ini_psi))
seg_mod_breaks <- data.frame(seg_mod$psi)$Est.
print(seg_mod_breaks)
(breaks_original_scale <- 10 ^ seg_mod_breaks) 
# on the original scale
# [1] 0.4468473
ggplot(expanded_tibble, aes(x = mean, y = freq)) + geom_point(size = 1) +
  geom_abline(intercept = coef(seg_mod)[1], slope = coef(seg_mod)[2]) +
  scale_x_log10() + scale_y_log10() + theme_minimal()








# PLOT -----------------------------------------------------------------


## coordinates
# major_breaks <- log10(10^(-2:0))
minor_breaks <- round(unique(c(sapply(-2:-1, \(x) log10((2:9) * 10^x)))), 2)
minor_breaks <- c(minor_breaks, 0)
sciti_labels <- \(x) {parse(text = sprintf("%s", scales::math_format()(log10(x))))}


## fitting line
best_split_point = breaks_original_scale
(best_split_point <- log_tibble$mean[best_split])
lm1 <- lm(log10(pdf) ~ log10(mean), data = subset(expanded_tibble, mean <= best_split_point))
lm2 <- lm(log10(pdf) ~ log10(mean), data = subset(expanded_tibble, mean >= best_split_point))


## plot
ggplot(expanded_tibble, aes(x = mean)) +
  geom_point(aes(y = pdf, color = "Empirical Distribution"), size = 1) +
  geom_errorbar(aes(ymin = pdf - stderr_pdf, ymax = pdf + stderr_pdf,
                    color = "Empirical Distribution"), width = 0.01) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.7, 
              aes(y = pdf, color = "Power-law Fit (upper)"), 
              data = subset(expanded_tibble, mean <= best_split_point)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.7,
              aes(y = pdf, color = "Power-law Fit (lower)"), 
              data = subset(expanded_tibble, mean > best_split_point)) +
  annotate("text", color = "#e57057", x = 0.2, y = 0.01,
           label = paste0("y = ", round(coef(lm1)[2], 2), "x + ", round(coef(lm1)[1], 2), "\nR2 = ", round(summary(lm1)$r.squared, 2))) +
  annotate("text", color = "#9d4edd", x = 0.2, y = 0.03,
           label = paste0("y = ", round(coef(lm2)[2], 2), "x + ", round(coef(lm2)[1], 2), "\nR2 = ", round(summary(lm2)$r.squared, 2))) +
  scale_x_log10(
    breaks = 10^minor_breaks,
    labels = scales::number(10^minor_breaks, 0.1)
  ) +
  scale_y_log10(labels = sciti_labels) +
  labs(x = "Risk (r)", y = "Probability f(r)") +
  theme_bw() +
  scale_color_manual(
    values = c("Empirical Distribution" = "#1696d2", 
               "Power-law Fit (upper)" = "#e57057", 
               "Power-law Fit (lower)" = "#9d4edd"),
    labels = c("Empirical Distribution", 
               "Power-law Fit (upper)", 
               "Power-law Fit (lower)"))
ggsave("Output/Fig/log-log-plot.pdf", width = 3, height = 1.5, scale = 3)



































































# DROP -----------------------------------------------------------------


# m <- conpl$new(tmp_sample)
# estimate_pars(m)



















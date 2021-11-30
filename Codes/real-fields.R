# /*=================================================*/
#' # Preparation
# /*=================================================*/

library(here)
library(future.apply)
library(tmap)
library(sp)
library(sf)
library(agricolae)
library(lwgeom)
library(scam)
library(measurements)
library(raster)
library(data.table)
library(tidyverse)
library(gdata)
library(mgcv)
library(MonteCarlo)
library(parallel)
library(gstat)
library(dplyr)
library(R.utils)
library(patchwork)
library(ggpubr)
#--- source functions ---#
source(here("Codes", "functions new.R"))

#--- github ---#
source("https://raw.githubusercontent.com/brittanikedge/DIFM/main/Functions.R")

##### Bohnhoff Schormann #####
data_misalignment <- readRDS(here("Data/Bohnhoff_Schormann_2020/Analysis-Ready", "analysis_data_w_misalignment.rds"))
data_wo_misalignment <- readRDS(here("Data/Bohnhoff_Schormann_2020/Analysis-Ready", "analysis_data_wo_misalignment.rds"))

#### Maps of Data #####
map_mis <- ggplot() +
  geom_sf(data = data_misalignment, aes(fill = s_rate), lwd = 0) +
  scale_colour_distiller(palette = "Blues", 
                         type = "seq",
                         direction = 1,
                         guide = "colourbar",
                         aesthetics = c("fill")) +
  labs(caption = "Field 1 data excluding \n areas with misalignment") + 
  theme(
    plot.caption = element_text(color="black", size=10, hjust = 0.5),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank())

map_no_mis <- ggplot() +
  geom_sf(data = data_wo_misalignment, aes(fill = s_rate), lwd = 0) +
  scale_colour_distiller(palette = "Blues", 
                         type = "seq",
                         direction = 1,
                         guide = "colourbar",
                         aesthetics = c("fill")) +
  labs(caption = "Field 1 data excluding \n areas with misalignment") + 
  theme(
    plot.caption = element_text(color="black", size=10, hjust = 0.5),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank())

(map_mis + map_no_mis)
ggsave("~/Box/Machine_Misalignment/Results/field1_data_map.jpeg")
##### Estimation #####
## Estimate gam and find profit maximizing rate
  
n_price <- .5
s_price <- 4.00
c_price <- 4.00

scam_mis <- scam(yield_vol ~ s(s_rate, k = 5, bs = "cv") +
                   s(n_rate, k = 5, bs = "micv"),
                 data = data_misalignment,
                 optimizer =  "nlm.fd")
# plot(scam_mis)

scam_no_mis <- scam(yield_vol ~ s(s_rate, k = 5, bs = "cv") +
                   s(n_rate, k = 5, bs = "micv"),
                 data = data_wo_misalignment,
                 optimizer =  "nlm.fd")
# plot(scam_no_mis)



#### Optimal Rate Differences ####
opt_n_mis <- data.table(
  n_rate = seq(quantile(data_misalignment$n_rate, probs = .025, na.rm = TRUE),
               quantile(data_misalignment$n_rate, probs = .975, na.rm = TRUE),
               by = 0.5),
  s_rate = rep(median(data_misalignment$s_rate), times = length(seq(quantile(data_misalignment$n_rate, probs = .025, na.rm = TRUE),
                                                                    quantile(data_misalignment$n_rate, probs = .975, na.rm = TRUE),
                                                                    by = 0.5))
  )) %>%
  .[, y_hat := predict(scam_mis, newdata = .)] %>%
  .[, p_hat := y_hat * c_price - n_rate * n_price] %>%
  .[, .SD[which.max(p_hat)]] %>%
  .[, .(n_rate)]

opt_n_wo_mis <- data.table(
  n_rate = seq(quantile(data_wo_misalignment$n_rate, probs = .025, na.rm = TRUE),
               quantile(data_wo_misalignment$n_rate, probs = .975, na.rm = TRUE),
               by = 0.5),
  s_rate = rep(median(data_wo_misalignment$s_rate),
               times = length(seq(quantile(data_wo_misalignment$n_rate, probs = .025, na.rm = TRUE),
                                  quantile(data_wo_misalignment$n_rate, probs = .975, na.rm = TRUE),
                                  by = 0.5))
  )) %>%
  .[, y_hat := predict(scam_no_mis, newdata = .)] %>%
  .[, p_hat := y_hat * c_price - n_rate * n_price] %>%
  .[, .SD[which.max(p_hat)]] %>%
  .[, .(n_rate)]

opt_s_mis <- data.table(
  s_rate = seq(quantile(data_misalignment$s_rate, probs = .025, na.rm = TRUE),
               quantile(data_misalignment$s_rate, probs = .975, na.rm = TRUE),
               by = 0.5),
  n_rate = rep(median(data_misalignment$n_rate), times = length(seq(quantile(data_misalignment$s_rate, probs = .025, na.rm = TRUE),
                                                                    quantile(data_misalignment$s_rate, probs = .975, na.rm = TRUE),
                                                                    by = 0.5))
  )) %>%
  .[, y_hat := predict(scam_mis, newdata = .)] %>%
  .[, p_hat := y_hat * c_price - s_rate * s_price] %>%
  .[, .SD[which.max(p_hat)]] %>%
  .[, .(s_rate)]

opt_s_wo_mis <- data.table(
  s_rate = seq(quantile(data_wo_misalignment$s_rate, probs = .025, na.rm = TRUE),
               quantile(data_wo_misalignment$s_rate, probs = .975, na.rm = TRUE),
               by = .5),
  n_rate = rep(median(data_wo_misalignment$n_rate),
               times = length(seq(quantile(data_wo_misalignment$s_rate, probs = .025, na.rm = TRUE),
                                  quantile(data_wo_misalignment$s_rate, probs = .975, na.rm = TRUE),
                                  by = .5))
  )) %>%
  .[, y_hat := predict(scam_no_mis, newdata = .)] %>%
  .[, p_hat := y_hat * c_price - s_rate * s_price] %>%
  .[, .SD[which.max(p_hat)]] %>%
  .[, .(s_rate)]

diff_n <- opt_n_mis - opt_n_wo_mis
diff_s <- opt_s_mis - opt_s_wo_mis

# profit_diff <- prof_opt - prof_est


### Yield Response to Seed Figures ####
figure_data_s_wo_mis <- data.table(
  s_rate = seq(quantile(data_wo_misalignment$s_rate, probs = .025, na.rm = TRUE),
               quantile(data_wo_misalignment$s_rate, probs = .975, na.rm = TRUE),
               by = .5),
  n_rate = rep(median(data_wo_misalignment$n_rate),
               times = length(seq(quantile(data_wo_misalignment$s_rate, probs = .025, na.rm = TRUE),
                                  quantile(data_wo_misalignment$s_rate, probs = .975, na.rm = TRUE),
                                  by = .5))),
  data = rep("without misalignment"),
  times = length(seq(quantile(data_wo_misalignment$s_rate, probs = .025, na.rm = TRUE),
                     quantile(data_wo_misalignment$s_rate, probs = .975, na.rm = TRUE),
                     by = .5))) %>%
  .[, y_hat := predict(scam_no_mis, newdata = .)]

figure_data_s_w_mis <- data.table(
  s_rate = seq(quantile(data_misalignment$s_rate, probs = .025, na.rm = TRUE),
               quantile(data_misalignment$s_rate, probs = .975, na.rm = TRUE),
               by = .5),
  n_rate = rep(median(data_misalignment$n_rate),
               times = length(seq(quantile(data_misalignment$s_rate, probs = .025, na.rm = TRUE),
                                  quantile(data_misalignment$s_rate, probs = .975, na.rm = TRUE),
                                  by = .5))),
  data = rep("with misalignment"),
  times = length(seq(quantile(data_misalignment$s_rate, probs = .025, na.rm = TRUE),
                     quantile(data_misalignment$s_rate, probs = .975, na.rm = TRUE),
                     by = .5))) %>%
  .[, y_hat := predict(scam_mis, newdata = .)]

figure_data_s <- rbind(figure_data_s_w_mis, figure_data_s_wo_mis)
opt_pt_w <- figure_data_s_w_mis[s_rate == as.numeric(opt_s_mis)]
opt_pt_wo <- figure_data_s_wo_mis[s_rate == as.numeric(opt_s_wo_mis)]

field1_response_seed <- ggplot(figure_data_s, aes(x = s_rate, y = y_hat, color = factor(data))) +
  geom_smooth() +
  xlab("Input Rate") +
  ylab("Yield") +
  labs(color = "Dataset") +
  geom_vline(xintercept = opt_pt_w$s_rate, linetype="dashed", color = "black", size=.5) +
  geom_vline(xintercept = opt_pt_wo$s_rate, linetype="dashed", color = "black", size=.5) +
  annotate("text", x = max(figure_data_s$s_rate - 4),
           y = min(figure_data_s$y_hat) + 0.6,
           label = paste0("Difference = ", as.character(round(abs(diff_s))))) +
  annotate("point", x = opt_pt_w$s_rate, y = opt_pt_w$y_hat, colour = "black") +
  annotate("point", x = opt_pt_wo$s_rate, y = opt_pt_wo$y_hat, colour = "black") +
  theme(legend.position = "bottom")
# ggsave("~/Box/Machine_Misalignment/Results/field1_response_seed.jpeg")

profit_diff_table <- data.table(
  s_rate = as.numeric(c(opt_s_mis, opt_s_wo_mis)),
  n_rate = median(data_wo_misalignment$n_rate)
) %>%
  .[, y_hat := predict(scam_no_mis, newdata = .)] %>%
  .[, p_hat := y_hat * c_price - s_rate * s_price]

##### Yield Response to Nitrogen Figures #####
figure_data_n_wo_mis <- data.table(
  n_rate = seq(quantile(data_wo_misalignment$n_rate, probs = .025, na.rm = TRUE),
               quantile(data_wo_misalignment$n_rate, probs = .975, na.rm = TRUE),
               by = .5),
  s_rate = rep(median(data_wo_misalignment$s_rate),
               times = length(seq(quantile(data_wo_misalignment$n_rate, probs = .025, na.rm = TRUE),
                                  quantile(data_wo_misalignment$n_rate, probs = .975, na.rm = TRUE),
                                  by = .5))),
  data = rep("without misalignment"),
  times = length(seq(quantile(data_wo_misalignment$n_rate, probs = .025, na.rm = TRUE),
                     quantile(data_wo_misalignment$n_rate, probs = .975, na.rm = TRUE),
                     by = .5))) %>%
  .[, y_hat := predict(scam_no_mis, newdata = .)]

figure_data_n_w_mis <- data.table(
  n_rate = seq(quantile(data_misalignment$n_rate, probs = .025, na.rm = TRUE),
               quantile(data_misalignment$n_rate, probs = .975, na.rm = TRUE),
               by = .5),
  s_rate = rep(median(data_misalignment$s_rate),
               times = length(seq(quantile(data_misalignment$n_rate, probs = .025, na.rm = TRUE),
                                  quantile(data_misalignment$n_rate, probs = .975, na.rm = TRUE),
                                  by = .5))),
  data = rep("with misalignment"),
  times = length(seq(quantile(data_misalignment$n_rate, probs = .025, na.rm = TRUE),
                     quantile(data_misalignment$n_rate, probs = .975, na.rm = TRUE),
                     by = .5))) %>%
  .[, y_hat := predict(scam_mis, newdata = .)]

figure_data_n <- rbind(figure_data_n_w_mis, figure_data_n_wo_mis)
opt_pt_w <- figure_data_n_w_mis[n_rate == as.numeric(opt_n_mis)]
opt_pt_wo <- figure_data_n_wo_mis[n_rate == as.numeric(opt_n_wo_mis)]

field1_response_nitrogen <- ggplot(figure_data_n, aes(x = n_rate, y = y_hat, color = factor(data))) +
  geom_smooth() +
  xlab("Input Rate") +
  ylab("Yield") +
  labs(color = "Dataset") +
  geom_vline(xintercept = opt_pt_w$n_rate, linetype="dashed", color = "black", size=.5) +
  geom_vline(xintercept = opt_pt_wo$n_rate, linetype="dashed", color = "black", size=.5) +
  annotate("text", x = max(figure_data_n$n_rate - 15),
           y = min(figure_data_n$y_hat) + 0.6,
           label = paste0("Difference = ", as.character(round(abs(diff_n))))) +
  annotate("point", x = opt_pt_w$n_rate, y = opt_pt_w$y_hat, colour = "black") +
  annotate("point", x = opt_pt_wo$n_rate, y = opt_pt_wo$y_hat, colour = "black") +
  theme(legend.position = "bottom")
# ggsave("~/Box/Machine_Misalignment/Results/field1_response_nitrogen.jpeg")

(field1_response_nitrogen + field1_response_seed)







##### Campbell Goldenrod #####

data_misalignment <- readRDS(here("Data/Campbell_Goldenrod_2020/Analysis-Ready", "analysis_data_w_misalignment.rds"))
data_wo_misalignment <- readRDS(here("Data/Campbell_Goldenrod_2020/Analysis-Ready", "analysis_data_wo_misalignment.rds"))

##### Maps of Data #####
map_mis <- ggplot() +
  geom_sf(data = data_misalignment, aes(fill = s_rate), lwd = 0) +
  scale_colour_distiller(palette = "Blues", 
                         type = "seq",
                         direction = 1,
                         guide = "colourbar",
                         aesthetics = c("fill")) +
  labs(caption = "Field 2 data including \n areas with misalignment") +
  theme(
    plot.caption = element_text(color="black", size=9, hjust = 0.5),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank())

map_no_mis <- ggplot() +
  geom_sf(data = data_wo_misalignment, aes(fill = s_rate), lwd = 0) +
  scale_colour_distiller(palette = "Blues", 
                         type = "seq",
                         direction = 1,
                         guide = "colourbar",
                         aesthetics = c("fill")) +
  labs(caption = "Field 2 data excluding \n areas with misalignment") +
  theme(
    plot.caption = element_text(color="black", size=9, hjust = 0.5),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank())

(map_mis + map_no_mis)
ggsave("~/Box/Machine_Misalignment/Results/field2_data_map.jpeg")
##### Estimation #####
## Estimate gam and find profit maximizing rate

s_price <- 0.321428571
c_price <- 12.00

scam_mis <- scam(yield_vol ~ s(s_rate, k = 5, bs = "cv"),
                 data = data_misalignment,
                 optimizer =  "nlm.fd")
# plot(scam_mis)

scam_no_mis <- scam(yield_vol ~ s(s_rate, k = 5, bs = "cv"),
                    data = data_wo_misalignment,
                    optimizer =  "nlm.fd")
# plot(scam_no_mis)

#### Optimal Rate Differences #####

opt_s_mis <- data.table(
  s_rate = seq(quantile(data_misalignment$s_rate, probs = .025, na.rm = TRUE),
               quantile(data_misalignment$s_rate, probs = .975, na.rm = TRUE),
               by = .5)
) %>%
  .[, y_hat := predict(scam_mis, newdata = .)] %>%
  .[, p_hat := y_hat * c_price - s_rate * s_price] %>%
  .[, .SD[which.max(p_hat)]] %>%
  .[, .(s_rate)]

opt_s_wo_mis <- data.table(
  s_rate = seq(quantile(data_wo_misalignment$s_rate, probs = .025, na.rm = TRUE),
               quantile(data_wo_misalignment$s_rate, probs = .975, na.rm = TRUE),
               by = .5)
) %>%
  .[, y_hat := predict(scam_no_mis, newdata = .)] %>%
  .[, p_hat := y_hat * c_price - s_rate * s_price] %>%
  .[, .SD[which.max(p_hat)]] %>%
  .[, .(s_rate)]

profit_diff_table <- data.table(
  s_rate = as.numeric(c(opt_s_mis, opt_s_wo_mis))
) %>%
  .[, y_hat := predict(scam_no_mis, newdata = .)] %>%
  .[, p_hat := y_hat * c_price - s_rate * s_price]


diff_s <- opt_s_mis - opt_s_wo_mis # no difference in optimal seed rate

### Yield Response to Seed Figures ####
figure_data_s_wo_mis <- data.table(
  s_rate = seq(quantile(data_wo_misalignment$s_rate, probs = .025, na.rm = TRUE),
               quantile(data_wo_misalignment$s_rate, probs = .975, na.rm = TRUE),
               by = .5),
  n_rate = rep(median(data_wo_misalignment$n_rate),
               times = length(seq(quantile(data_wo_misalignment$s_rate, probs = .025, na.rm = TRUE),
                                  quantile(data_wo_misalignment$s_rate, probs = .975, na.rm = TRUE),
                                  by = .5))),
  data = rep("without misalignment"),
  times = length(seq(quantile(data_wo_misalignment$s_rate, probs = .025, na.rm = TRUE),
                     quantile(data_wo_misalignment$s_rate, probs = .975, na.rm = TRUE),
                     by = .5))) %>%
  .[, y_hat := predict(scam_no_mis, newdata = .)]

figure_data_s_w_mis <- data.table(
  s_rate = seq(quantile(data_misalignment$s_rate, probs = .025, na.rm = TRUE),
               quantile(data_misalignment$s_rate, probs = .975, na.rm = TRUE),
               by = .5),
  n_rate = rep(median(data_misalignment$n_rate),
               times = length(seq(quantile(data_misalignment$s_rate, probs = .025, na.rm = TRUE),
                                  quantile(data_misalignment$s_rate, probs = .975, na.rm = TRUE),
                                  by = .5))),
  data = rep("with misalignment"),
  times = length(seq(quantile(data_misalignment$s_rate, probs = .025, na.rm = TRUE),
                     quantile(data_misalignment$s_rate, probs = .975, na.rm = TRUE),
                     by = .5))) %>%
  .[, y_hat := predict(scam_mis, newdata = .)]

figure_data_s <- rbind(figure_data_s_w_mis, figure_data_s_wo_mis)
opt_pt_w <- figure_data_s_w_mis[s_rate == as.numeric(opt_s_mis)]
opt_pt_wo <- figure_data_s_wo_mis[s_rate == as.numeric(opt_s_wo_mis)]

ggplot(figure_data_s, aes(x = s_rate, y = y_hat, color = factor(data))) +
  geom_smooth() +
  xlab("Input Rate") +
  ylab("Yield") +
  labs(color = "Dataset") +
  geom_vline(xintercept = opt_pt_w$s_rate, linetype="dashed", color = "black", size=.5) +
  geom_vline(xintercept = opt_pt_wo$s_rate, linetype="dashed", color = "black", size=.5) +
  annotate("text", x = max(figure_data_s$s_rate - 10),
           y = min(figure_data_s$y_hat) + 0.2,
           label = paste0("Difference = ", as.character(round(abs(diff_s))))) +
  annotate("point", x = opt_pt_w$s_rate, y = opt_pt_w$y_hat, colour = "black") +
  annotate("point", x = opt_pt_wo$s_rate, y = opt_pt_wo$y_hat, colour = "black") +
  theme(legend.position = "bottom")
ggsave("~/Box/Machine_Misalignment/Results/field2_response_seed.jpeg")









##### Hord F17 #####
data_misalignment <- readRDS(here("Data/Hord_F17_2020/Analysis-Ready", "analysis_data_w_misalignment.rds"))
data_wo_misalignment <- readRDS(here("Data/Hord_F17_2020/Analysis-Ready", "analysis_data_wo_misalignment.rds"))

##### Maps of Data #####
map_mis <- ggplot() +
  geom_sf(data = data_misalignment, aes(fill = s_rate), lwd = 0) +
  scale_colour_distiller(palette = "Blues", 
                         type = "seq",
                         direction = 1,
                         guide = "colourbar",
                         aesthetics = c("fill")) +
  labs(caption = "Field 3 data including \n areas with misalignment") +
  theme(
    plot.caption = element_text(color="black", size=10, hjust = 0.5),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank())

map_no_mis <- ggplot() +
  geom_sf(data = data_wo_misalignment, aes(fill = s_rate), lwd = 0) +
  scale_colour_distiller(palette = "Blues", 
                         type = "seq",
                         direction = 1,
                         guide = "colourbar",
                         aesthetics = c("fill")) +
  labs(caption = "Field 3 data excluding \n areas with misalignment") +
  theme(
    plot.caption = element_text(color="black", size=10, hjust = 0.5),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank())

(map_mis + map_no_mis)
ggsave("~/Box/Machine_Misalignment/Results/field3_data_map.jpeg")

##### Estimation #####
## Estimate gam and find profit maximizing rate

n_price <- .5
s_price <- 4.00
c_price <- 4.00

scam_mis <- scam(yield_vol ~ s(s_rate, k = 5, bs = "cv") +
                   s(n_rate, k = 5, bs = "micv"),
                 data = data_misalignment,
                 optimizer =  "nlm.fd")
# plot(scam_mis)

scam_no_mis <- scam(yield_vol ~ s(s_rate, k = 5, bs = "cv") +
                      s(n_rate, k = 5, bs = "micv"),
                    data = data_wo_misalignment,
                    optimizer =  "nlm.fd")
# plot(scam_no_mis)

##### Optimal Rate Differences #####
opt_n_mis <- data.table(
  n_rate = seq(quantile(data_misalignment$n_rate, probs = .025, na.rm = TRUE),
               quantile(data_misalignment$n_rate, probs = .975, na.rm = TRUE),
               by = 1),
  s_rate = rep(median(data_misalignment$s_rate),
               times = length(seq(quantile(data_misalignment$n_rate, probs = .025, na.rm = TRUE),
                                  quantile(data_misalignment$n_rate, probs = .975, na.rm = TRUE),
                                  by = 1))
  )) %>%
  .[, y_hat := predict(scam_mis, newdata = .)] %>%
  .[, p_hat := y_hat * c_price - n_rate * n_price] %>%
  .[, .SD[which.max(p_hat)]] %>%
  .[, .(n_rate)]

opt_n_no_mis <- data.table(
  n_rate = seq(quantile(data_wo_misalignment$n_rate, probs = .025, na.rm = TRUE),
               quantile(data_wo_misalignment$n_rate, probs = .975, na.rm = TRUE),
               by = 1),
  s_rate = rep(median(data_wo_misalignment$s_rate),
               times = length(seq(quantile(data_wo_misalignment$n_rate, probs = .025, na.rm = TRUE),
                                  quantile(data_wo_misalignment$n_rate, probs = .975, na.rm = TRUE),
                                  by = 1))
  )) %>%
  .[, y_hat := predict(scam_no_mis, newdata = .)] %>%
  .[, p_hat := y_hat * c_price - n_rate * n_price] %>%
  .[, .SD[which.max(p_hat)]] %>%
  .[, .(n_rate)]

opt_s_mis <- data.table(
  s_rate = seq(quantile(data_misalignment$s_rate, probs = .025, na.rm = TRUE),
               quantile(data_misalignment$s_rate, probs = .975, na.rm = TRUE),
               by = .5),
  n_rate = rep(median(data_misalignment$n_rate), times = length(seq(quantile(data_misalignment$s_rate, probs = .025, na.rm = TRUE),
                                                                    quantile(data_misalignment$s_rate, probs = .975, na.rm = TRUE),
                                                                    by = .5))
  )) %>%
  .[, y_hat := predict(scam_mis, newdata = .)] %>%
  .[, p_hat := y_hat * c_price - s_rate * s_price] %>%
  .[, .SD[which.max(p_hat)]] %>%
  .[, .(s_rate)]

opt_s_wo_mis <- data.table(
  s_rate = seq(quantile(data_wo_misalignment$s_rate, probs = .025, na.rm = TRUE),
               quantile(data_wo_misalignment$s_rate, probs = .975, na.rm = TRUE),
               by = .5),
  n_rate = rep(median(data_wo_misalignment$n_rate),
               times = length(seq(quantile(data_wo_misalignment$s_rate, probs = .025, na.rm = TRUE),
                                  quantile(data_wo_misalignment$s_rate, probs = .975, na.rm = TRUE),
                                  by = .5))
  )) %>%
  .[, y_hat := predict(scam_no_mis, newdata = .)] %>%
  .[, p_hat := y_hat * c_price - s_rate * s_price] %>%
  .[, .SD[which.max(p_hat)]] %>%
  .[, .(s_rate)]

diff_n <- opt_n_mis - opt_n_no_mis
diff_s <- opt_s_mis - opt_s_wo_mis

profit_diff <- prof_opt - prof_est

### Yield Response to Seed Figures ####
figure_data_s_wo_mis <- data.table(
  s_rate = seq(quantile(data_wo_misalignment$s_rate, probs = .025, na.rm = TRUE),
               quantile(data_wo_misalignment$s_rate, probs = .975, na.rm = TRUE),
               by = .5),
  n_rate = rep(median(data_wo_misalignment$n_rate),
               times = length(seq(quantile(data_wo_misalignment$s_rate, probs = .025, na.rm = TRUE),
                                  quantile(data_wo_misalignment$s_rate, probs = .975, na.rm = TRUE),
                                  by = .5))),
  data = rep("without misalignment"),
  times = length(seq(quantile(data_wo_misalignment$s_rate, probs = .025, na.rm = TRUE),
                     quantile(data_wo_misalignment$s_rate, probs = .975, na.rm = TRUE),
                     by = .5))) %>%
  .[, y_hat := predict(scam_no_mis, newdata = .)]

figure_data_s_w_mis <- data.table(
  s_rate = seq(quantile(data_misalignment$s_rate, probs = .025, na.rm = TRUE),
               quantile(data_misalignment$s_rate, probs = .975, na.rm = TRUE),
               by = .5),
  n_rate = rep(median(data_misalignment$n_rate),
               times = length(seq(quantile(data_misalignment$s_rate, probs = .025, na.rm = TRUE),
                                  quantile(data_misalignment$s_rate, probs = .975, na.rm = TRUE),
                                  by = .5))),
  data = rep("with misalignment"),
  times = length(seq(quantile(data_misalignment$s_rate, probs = .025, na.rm = TRUE),
                     quantile(data_misalignment$s_rate, probs = .975, na.rm = TRUE),
                     by = .5))) %>%
  .[, y_hat := predict(scam_mis, newdata = .)]

figure_data_s <- rbind(figure_data_s_w_mis, figure_data_s_wo_mis)
opt_pt_w <- figure_data_s_w_mis[s_rate == as.numeric(opt_s_mis)]
opt_pt_wo <- figure_data_s_wo_mis[s_rate == as.numeric(opt_s_wo_mis)]

field3_response_seed <- ggplot(figure_data_s, aes(x = s_rate, y = y_hat, color = factor(data))) +
  geom_smooth() +
  xlab("Input Rate") +
  ylab("Yield") +
  labs(color = "Dataset") +
  geom_vline(xintercept = opt_pt_w$s_rate, linetype="dashed", color = "black", size=.5) +
  geom_vline(xintercept = opt_pt_wo$s_rate, linetype="dashed", color = "black", size=.5) +
  annotate("text", x = max(figure_data_s$s_rate - 2),
           y = min(figure_data_s$y_hat) + 0.6,
           label = paste0("Difference = ", as.character(round(abs(diff_s))))) +
  annotate("point", x = opt_pt_w$s_rate, y = opt_pt_w$y_hat, colour = "black") +
  annotate("point", x = opt_pt_wo$s_rate, y = opt_pt_wo$y_hat, colour = "black") +
  theme(legend.position = "bottom")
# ggsave("~/Box/Machine_Misalignment/Results/field3_response_seed.jpeg")

profit_diff_table <- data.table(
  s_rate = as.numeric(c(opt_s_mis, opt_s_wo_mis)),
  n_rate = median(data_wo_misalignment$n_rate)
) %>%
  .[, y_hat := predict(scam_no_mis, newdata = .)] %>%
  .[, p_hat := y_hat * c_price - s_rate * s_price]

##### Yield Response to Nitrogen Figures #####
figure_data_n_wo_mis <- data.table(
  n_rate = seq(quantile(data_wo_misalignment$n_rate, probs = .025, na.rm = TRUE),
               quantile(data_wo_misalignment$n_rate, probs = .975, na.rm = TRUE),
               by = .5),
  s_rate = rep(median(data_wo_misalignment$s_rate),
               times = length(seq(quantile(data_wo_misalignment$n_rate, probs = .025, na.rm = TRUE),
                                  quantile(data_wo_misalignment$n_rate, probs = .975, na.rm = TRUE),
                                  by = .5))),
  data = rep("without misalignment"),
  times = length(seq(quantile(data_wo_misalignment$n_rate, probs = .025, na.rm = TRUE),
                     quantile(data_wo_misalignment$n_rate, probs = .975, na.rm = TRUE),
                     by = .5))) %>%
  .[, y_hat := predict(scam_no_mis, newdata = .)]

figure_data_n_w_mis <- data.table(
  n_rate = seq(quantile(data_misalignment$n_rate, probs = .025, na.rm = TRUE),
               quantile(data_misalignment$n_rate, probs = .975, na.rm = TRUE),
               by = .5),
  s_rate = rep(median(data_misalignment$s_rate),
               times = length(seq(quantile(data_misalignment$n_rate, probs = .025, na.rm = TRUE),
                                  quantile(data_misalignment$n_rate, probs = .975, na.rm = TRUE),
                                  by = .5))),
  data = rep("with misalignment"),
  times = length(seq(quantile(data_misalignment$n_rate, probs = .025, na.rm = TRUE),
                     quantile(data_misalignment$n_rate, probs = .975, na.rm = TRUE),
                     by = .5))) %>%
  .[, y_hat := predict(scam_mis, newdata = .)]

figure_data_n <- rbind(figure_data_n_w_mis, figure_data_n_wo_mis)
opt_pt_w <- figure_data_n_w_mis[n_rate == as.numeric(opt_n_mis)]
opt_pt_wo <- figure_data_n_wo_mis[n_rate == as.numeric(opt_n_no_mis)]

field3_response_nitrogen <- ggplot(figure_data_n, aes(x = n_rate, y = y_hat, color = factor(data))) +
  geom_smooth() +
  xlab("Input Rate") +
  ylab("Yield") +
  labs(color = "Dataset") +
  geom_vline(xintercept = opt_pt_w$n_rate, linetype="dashed", color = "black", size=.5) +
  geom_vline(xintercept = opt_pt_wo$n_rate, linetype="dashed", color = "black", size=.5) +
  annotate("text", x = max(figure_data_n$n_rate - 15),
           y = min(figure_data_n$y_hat) + 0.6,
           label = paste0("Difference = ", as.character(round(abs(diff_n))))) +
  annotate("point", x = opt_pt_w$n_rate, y = opt_pt_w$y_hat, colour = "black") +
  annotate("point", x = opt_pt_wo$n_rate, y = opt_pt_wo$y_hat, colour = "black") +
  theme(legend.position = "bottom")
# ggsave("~/Box/Machine_Misalignment/Results/field3_response_nitrogen.jpeg")

field3_response_nitrogen + field3_response_seed

profit_diff_table <- data.table(
  n_rate = as.numeric(c(opt_n_mis, opt_n_no_mis)),
  s_rate = median(data_wo_misalignment$s_rate)
) %>%
  .[, y_hat := predict(scam_no_mis, newdata = .)] %>%
  .[, p_hat := y_hat * c_price - s_rate * s_price]
profit_diff_table$p_hat[1] - profit_diff_table$p_hat[2] 

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

#--- source functions ---#
source(here("Codes", "functions new.R"))

#--- github ---#
source("https://raw.githubusercontent.com/brittanikedge/DIFM/main/Functions.R")

trial_grids <- readRDS("Results/trial_grids.rds")
opt = 200
rates_ls <- seq(opt - 20, opt + 20, by = 40/5)

exp_design <- assign_rates(
  filter(trial_grids, td_grid_id != "headland"), 
  rates_ls
) %>% 
  data.table() %>% 
  .[, .(td_grid_id, rate)]

headland_design <- trial_grids %>%
  filter(td_grid_id == "headland") %>%
  mutate(rate = rates_ls[[4]]) %>%
  data.table() %>%
.[, .(td_grid_id, rate)]

trial <- rbind(exp_design, headland_design)
trial <- merge(trial_grids, trial, by = "td_grid_id")

ggplot() + 
  geom_sf(data = trial, aes(fill = rate), lwd = 0) +
  labs(fill = "Target Rate") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank())
ggsave("~/Box/Machine_Misalignment/Results/sample_trialdesign.jpeg")

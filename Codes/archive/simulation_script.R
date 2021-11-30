# /*=================================================*/
#' # Preparation
# /*=================================================*/

library(here)
library(tmap)
library(sp)
library(sf)
library(agricolae)
library(lwgeom)
library(measurements)
library(raster)
library(data.table)
library(tidyverse)
library(gdata)
library(mgcv)
library(MonteCarlo)
library(parallel)

#--- source functions ---#
source(here("Codes", "functions.R"))
source(here("Codes", "simulation_functions.R"))

#--- github ---#
source("https://raw.githubusercontent.com/brittanikedge/DIFM/main/Functions.R")

##### Parameters #####
# keep the same setup for the machine, treatments and prices
plot_width <- 60
harvester_width <- 30
rates_ls <- seq(80, 280, by = 40)
n_price <- .5
c_price <- 4.00
subplot_length <- 10

##### Functions #####
y_function_low <- function(x){
  y = 200*(1 - exp(-.006*(140 + x)))
  return(y)}
# graph yield response function
ggplot() +
  xlim(0, 300) +
  geom_function(fun = y_function_low)

p_function_low <- function(x){
  y = (200*(1 - exp(-.006*(140 + x)))*c_price) - x*n_price
  return(y)}
ggplot() +
  xlim(0, 300) +
  geom_function(fun = p_function_low)
optimize(p_function_low, interval=c(50, 300), maximum=TRUE)$maximum

y_function_mid <- function(x){
  y = 280*(1 - exp(-.012*(70 + x)))
  return(y)}
# graph yield response function
ggplot() +
  xlim(0, 300) +
  geom_function(fun = y_function_mid)

p_function_mid <- function(x){
  y = (280*(1 - exp(-.012*(70 + x)))*c_price) - x*n_price
  return(y)}
ggplot() +
  xlim(0, 300) +
  geom_function(fun = p_function_mid)
optimize(p_function_mid, interval=c(50, 300), maximum=TRUE)$maximum

y_function_high <- function(x){
  y = 300*(1 - exp(-.02*(100 + x)))
  return(y)}
# graph yield response function
ggplot() +
  xlim(0, 300) +
  geom_function(fun = y_function_high)

p_function_high <- function(x){
  y = (300*(1 - exp(-.02*(100 + x)))*c_price) - x*n_price
  return(y)}
ggplot() +
  xlim(0, 300) +
  geom_function(fun = p_function_high)
optimize(p_function_high, interval=c(50, 300), maximum=TRUE)$maximum

# make trial grid #
grid <- trial_grid_design(plot_width)

functions <- c(y_function_low, y_function_mid, y_function_high)
function_names <- c("most linear", "more curvature", "most curvature")
function_data <- cbind(functions, function_names)

cases <- c("angle", "misalignment")
errors <- c(5, 10, 15, 20)
  
result_table <- expand.grid(cases, errors, functions) %>%
  rename("case" = "Var1", "error" = "Var2", "y_function" = "Var3") %>%
  tibble() %>%
  rowwise() %>%
  mutate(design_grid = list(grid)) %>%
  mutate(plot_yield = list(ggplot() +
                             xlim(0, 300) +
                             geom_function(fun = y_function))) %>%
  mutate(poly_yield = list(make_harvest(harvester_width, case, error))) %>%
  mutate(opt_n = optimize(function(x) y_function(x)*c_price - x*n_price, interval=c(50, 280), maximum=TRUE)$maximum)

result_table <- result_table %>%
  mutate(sim_estimates = list(function_sim(design_grid, poly_yield, function_type, n_sims = 100)))

first <- result_table[1:10,] %>%
  mutate(sim_estimates = list(function_sim(design_grid, poly_yield, function_type, n_sims = 100)))
second <- result_table[11:21,] %>%
  mutate(sim_estimates = list(function_sim(design_grid, poly_yield, function_type, n_sims = 100)))

saveRDS(result_table, file = here("Codes", "sim_results.rds"))



# Env --------------------------------------------------------------------

rm(list = ls())
set.seed(214561)

# Functions and Packages -------------------------------------------------

library(metafor)
library(dplyr)
library(tidyr)
library(furrr)

# utilities for the simulation
source("simulation/utils.R")

# Simulation Settings ----------------------------------------------------

# number of simulations per conditions
nsim <- 5000

sim <- expand_grid(
  nX = 1, # number of focal predictors
  nZ = 1, # number of nuisance covariates
  b0 = 0, # intercept
  bX = c(0, 0.2), # focal coefficients
  bZ = 0.2, # nuisance coefficients
  rXZ = c(0, 0.5, 0.8), # correlation between focal and nuisance
  tau2 = c(0, 0.05), # residual heterogeneity
  zeta = c(0, log(1.5), log(2.0)), # scale coefficient (related to X)
  k = c(10, 30, 60, 80), # number of studies
  n = c(15, 40, 80), # average number of participants per study per group
  method = c("EE", "REML") # EE: equal-effects, REML: random-effects
)

# removing impossible conditions
sim <- filter(sim, !(tau2 == 0 & zeta != 0))
attributes(sim)$sconds <- names(sim)
sim$nsim <- nsim

# Running the simulation -------------------------------------------------

# interactive vs parallel on slurm cluster
if (!interactive()) {
  workers <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))
  if (is.na(workers) || workers < 1L) {
    workers <- 1L
  }
  future::plan(future::multicore, workers = workers)
} else {
  sim$nsim <- 3
}

sim$res <- future_pmap(
  sim,
  do_sim,
  .options = furrr_options(seed = TRUE, scheduling = 1)
)

saveRDS(sim, file = "simulation/res.rds")

# cleaning for later analysis, focus on x

sim <- unnest_longer(sim, res, indices_to = "sim")
sim <- unnest(sim, res)
sim_clean <- sim |>
  filter(coef == "x1") |>
  select(-starts_with("stat_"), -starts_with("se_"))
saveRDS(sim_clean, "simulation/res-clean.rds")

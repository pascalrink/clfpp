
rm(list = ls())

library(dplyr)
library(doParallel)

n_cores <- 60
par_clust <- parallel::makeCluster(n_cores, outfile = "")
doParallel::registerDoParallel(par_clust)

n_obs_range <- c(400)
sapply(n_obs_range, function(n_obs) {
  t0 <- Sys.time()
  tuneranger_sim_results <- foreach::foreach(
    seed = 1:5000, 
    .packages = c("dplyr", "mlr", "tuneRanger", "ranger")) %dopar% {
      source("5-example/TiltCi-function.R")
      source("5-example/MabtCi-function.R")
      source("6-additions/tuneranger-sim/TuneRangerSim-function.R")
      tuneranger_sim_result <- TuneRangerSim(seed, n_obs)
      saveRDS(tuneranger_sim_result, paste0("1-savefiles/cis/rf/n", n_obs, "/rf-n", n_obs, "-", seed, ".RDS"))
    }
  saveRDS(tuneranger_sim_results, paste0("rf-", n_obs, ".RDS"))
  t1 <- Sys.time()
  t1-t0
})
parallel::stopCluster(par_clust)


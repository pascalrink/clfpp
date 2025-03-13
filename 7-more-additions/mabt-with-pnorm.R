
rm(list = ls())
graphics.off()

library(dplyr)
library(doParallel)

# Specify simulation parameters here
alpha <- 0.05
n_cores <- 60

EstCis <- function(fpath, alpha, n_boots, n_cores) {
  
  filenames <- paste0("1-savefiles/data/", fpath) %>% list.files
  
  par_clust <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(par_clust)
  
  out <- foreach::foreach(fn = filenames, .packages = c("dplyr")) %dopar% {
    
    ProposedTiltBound <- function(inst, boot, rule, alpha, fpath, prefix, suffix) {
      
      select <- inst$select
      params <- inst$params
      preselect <- select$valid[[rule]]$idx
      n_preselected <- length(preselect)
      
      if (n_preselected == 1) {
        return(DefaultTiltBound(inst, boot, rule, alpha, mabt = TRUE))
      }
      
      select_idx <- select$eval[[rule]]$idx
      select_groundtruth <- inst$performs$groundtruth[select_idx]
      measure <- params$measure
      
      labs <- inst$labs
      n_obs <- length(labs)
      
      t0 <- boot$t0[select_idx]
      t <- boot$t[, select_idx]
      
      select_preds <- inst$preds[, select_idx]
      if (measure == "class") {
        select_preds <- ((select_preds > params$cl_thresh) * 1.0 == labs) * 1.0
      }
      
      tau_range <- switch(measure, class = c(-10, 0), auc = c(-20, 0))
      
      eivs <- switch(measure, 
                     class = select_preds %>% as.matrix, 
                     auc = boot::empinf(boot, index = select_idx)) %>% as.matrix
      
      # unif_transfd <- copula::pobs(boot$t, ties.method = "max")
      unif_transfd <- pnorm(boot$t)
      .MaxEcdf <- unif_transfd[, preselect] %>% apply(1, max) %>% (stats::ecdf)
      
      .EstPval <- function(.tau) {
        if (is.na(.tau)) {
          p <- NA
          tilt_weights <- NA
        } else {
          tilt_weights <- boot::exp.tilt(eivs, lambda = .tau * nrow(eivs))$p
          xi <- weighted.mean(select_preds, tilt_weights)
          t0_xi <- (mean(select_preds) - xi) / sd(select_preds)
          imp_weights <- rep(
            tilt_weights * length(select_preds), boot$R)^t(boot::boot.array(boot)) %>% 
            apply(2, prod)
          tilt_perform <- spatstat.univar::ewcdf(t, imp_weights)(t0_xi)
          p <- 1 - .MaxEcdf(tilt_perform)
        }
        return(list(
          tau = .tau, p = p, weights = tilt_weights, success = !is.na(p) * 1.0))
      }
      
      .CalibTau <- function(.tau) {
        p <- .EstPval(.tau)$p
        obj <- p - alpha
        return(obj)
      }
      
      .EstBound <- function(.tilt) {
        if (! .tilt$success) {
          bound <- NA
        } else {
          .weights <- .tilt$weights
          if (measure == "class") {
            bound <- weighted.mean(select_preds, .weights)
          }
          if (measure == "auc") {
            bound_boot <- boot$data[, select_idx] %>% 
              boot::boot(function(d, i) {
                pROC::auc(labs[i], d[i])}, length(t), stype = "i", 
                strata = labs, weights = .weights)
            bound <- mean(bound_boot$t)
          }
        }
        return(bound)
      }
      
      min_tau <- 0  
      min_p <- 1
      while (min_p > alpha/2 & min_tau > tau_range[1]) {
        min_tau <- min_tau - 0.1
        min_p <- .EstPval(min_tau)$p
      }
      tau_range[1] <- min_tau
      
      max_p <- .EstPval(tau_range[2])$p
      feasible <- ifelse(min_p <= alpha/2 & max_p > 2*alpha, 1, 0)
      
      tau <- ifelse(feasible, stats::uniroot(.CalibTau, tau_range)$root, NA)
      tilt <- .EstPval(tau)
      bound <- .EstBound(tilt) 
      alt_method <- NA
      
      if (sd(select_preds) == 0 | feasible == 0) {
        alt_bounds <- StandardBounds(inst, rule, alpha)
        alt_method <- switch(measure, class = "wilson", auc = "delong")
        alt_result <- alt_bounds[which(alt_bounds$method == alt_method), ]
        
        bound <- alt_result$bound
        alpha <- alt_result$alpha
      }
      
      return(data.frame(method = "mabt", 
                        rule = rule, 
                        bound = bound, 
                        groundtruth = select_groundtruth, 
                        est = t0, 
                        alpha = alpha, 
                        n_preselected = n_preselected, 
                        feasible = feasible, 
                        tau = tau, 
                        p = tilt$p, 
                        min_p = min_p, 
                        max_p = max_p, 
                        alt = alt_method))
    }
    
    
    source("3-intervals/bootstrap.R")
    source("3-intervals/default-bounds.R")
    source("3-intervals/default-tilting.R")
    
    fn_length <- nchar(fn)
    fn_split_id <- 9
    prefix <- substr(fn, 1, fn_length - fn_split_id)
    suffix <- substr(fn, fn_length - fn_split_id + 1, fn_length)
    
    t0 <- Sys.time()
    inst <- paste0("1-savefiles/data/", fpath, fn) %>% readRDS
    set.seed(inst$params$seed)
    boot <- Bootstrap(inst, n_boots)
    
    rules <- names(inst$select$eval)
    rules <- rules[rules != "ten"]
    names(rules) <- rules
    
    proposed_tilt <- rules[which(names(rules) != "best")] %>%
      lapply(function(.rule) ProposedTiltBound(
        inst, boot, .rule, alpha, fpath, prefix, suffix)) %>%
      do.call(rbind, .)
    t1 <- Sys.time()
    
    params <- list(full_alpha = alpha, n_boots = n_boots, 
                   ci_runtime = lubridate::second(t1) - lubridate::second(t0))
    inst$params <- c(inst$params, params)
    bounds <- proposed_tilt
    this <- data.frame(bounds, inst$params)
    rownames(this) <- NULL
    this$covers <- this$bound < this$groundtruth
    return(this)
    
    this_fn <- paste0(fpath, prefix, "cis-", suffix)
  }
  
  parallel::stopCluster(par_clust)
  return(out)
}

# system.time(out_200 <- EstCis(fpath = "class/normal/n200/cv/", alpha = alpha, n_boots = 10000, n_cores = n_cores))
# saveRDS(out_200, "7-more-additions/mabt-with-pnorm-out-200.RDS")
# 
system.time(out_400 <- EstCis(fpath = "class/normal/n400/cv/", alpha = alpha, n_boots = 10000, n_cores = n_cores))
saveRDS(out_400, "7-more-additions/mabt-with-pnorm-out-400.RDS")


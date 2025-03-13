
Bootstrap <- function(inst, n_boots) {
  resample_dat <- inst$preds
  measure <- inst$params$measure
  labs <- c(inst$labs)
  
  if (measure == "class") {
    resample_dat <- ((resample_dat > inst$params$cl_thresh) == labs) * 1.0
    .ClassBootStat <- function(d, i) {
      boot_dat <- d[i, ] %>% as.matrix
      sample_size <- length(i)
      mu_0 <- colMeans(d)
      mu <- colMeans(boot_dat)
      std_err <- apply(boot_dat, 2, sd) / sqrt(sample_size)
      std_err[std_err == 0] <- (c(rep(1, sample_size-1), 0) %>% sd) / sqrt(sample_size)
      t_stat <- (mu - mu_0) / std_err
      return(t_stat)
    }
    boot <- boot::boot(resample_dat, .ClassBootStat, n_boots)
  }
  if (measure == "auc") {
    .AucBootStat <- function(d, i) {
      as.matrix(d[i, ]) %>% 
        apply(2, function(.preds) pROC::auc(labs[i], .preds))
    }
    boot <- boot::boot(resample_dat, .AucBootStat, n_boots, strata = labs)
  }
  return(boot)
}

library(lavaan)
library(mvtnorm)
library(dplyr)
library(parallel)
library(data.table)
library(compare)


# generate data from multivariate normal distribution ---------------------


#create all data once(final picked)(insanely fast)
gen_dta <-
  function(nobs,
           reps,
           la1,
           phi1,
           th1,
           tau1,
           fac_mean1,
           la2,
           phi2,
           th2,
           tau2,
           fac_mean2) {
    em_list <- vector(length = reps, mode = "list")
    mclapply(em_list, function(x) {
      pop_co_ma1 <- la1 %*% phi1 %*% t(la1) + th1
      pop_co_ma2 <- la2 %*% phi2 %*% t(la2) + th2
      pop_mean1 <- tau1 + la1 %*% fac_mean1
      pop_mean2 <- tau2 + la2 %*% fac_mean2
      dta1 <-
        data.frame(rmvnorm(
          nobs,
          mean = pop_mean1,
          sigma = pop_co_ma1,
          method = "chol"
        ))#rep(mean,)mean要帶公式,tau+lamda*latent mean
      dta2 <-
        data.frame(rmvnorm(
          nobs,
          mean = pop_mean2,
          sigma = pop_co_ma2,
          method = "chol"
        ))
      rbind(dta1, dta2) %>%
        mutate(group = c(rep(1, nobs), rep(2, nobs)))
    }, mc.cores = 1)
  }


# create difference of lambda dataframe from result of cfa ------------------------------


gen_lam <- function(data, model) {
  mclapply(data, function(x) {
    fit <- cfa(data = x,
               model = model,
               group = "group")
    dlp <- parameterEstimates(fit)[41:45, 10]
    data.frame(v = c("dl2", "dl3", "dl4", "dl5", "dl6"),
               diff_lam_pvalue = dlp)
  }, mc.cores = 12)
}


# check variable is non-invariant or not ----------------------------------


check_non <- function(data, p_value) {
  em_list <- vector(length = reps, mode = "list")
  mclapply(data, function(x) {
    c(x[1, 2] < p_value,
      x[2, 2] < p_value,
      x[3, 2] < p_value,
      x[4, 2] < p_value,
      x[5, 2] < p_value)
  }, mc.cores = 1)
}

# perfect recovery rate:completely detects non-invariant variable ---------


#non_con: non-invariant variable enter TRUE, invariant enter FALSE
det_non <- function(det_list, non_con) {
  sapply(det_list, function(x) {
    ifelse(compare(x, non_con)$result, 1, 0)
  })
}

<<<<<<< HEAD
# model-level Type I error (for baseline model only)------------------------------------------------


det_tyi <- function(det_list) {
  sapply(det_list, function(x) {
    ifelse(x[1] == TRUE, 1, ifelse(x[2] == TRUE, 1, ifelse(
      x[3] == TRUE, 1, ifelse(x[4] == TRUE, 1, ifelse(x[5] == TRUE, 1, 0))
    )))
  })
}

=======
>>>>>>> d707ba2c418d664f88fcbeabbedc2a1336b768e6
# model-level Type I error ------------------------------------------------


det_tyi <- function(det_list) {
  sapply(det_list, function(x) {
    ifelse(x[2] == TRUE, 1, ifelse(x[4] == TRUE, 1, ifelse(x[5] == TRUE, 1, 0)))
  })
}

# model-level Type II error -----------------------------------------------


det_tyii <- function(det_list) {
  sapply(det_list, function(x) {
    ifelse(x[1] == FALSE, 1, ifelse(x[3] == FALSE, 1, 0))
  })
}
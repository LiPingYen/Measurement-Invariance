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



# create tau dataframe from result of cfa ---------------------------------


gen_tau <- function(data, model) {
  mclapply(data, function(x) {
    fit <- cfa(data = x,
               model = model,
               group = "group")
    tau_g1 <- parameterEstimates(fit)[c(8, 16, 17, 18, 19, 20), 7]
    tau_g2 <- parameterEstimates(fit)[c(28, 36, 37, 38, 39, 40), 7]
    data.frame(
      tau_g1 = tau_g1,
      tau_g2 = tau_g2,
      v = c("v1", "v2", "v3", "v4", "v5", "v6")
    )
  }, mc.cores = 1)
}


# check variable is non-invariant or not ----------------------------------


check_non <- function(data, con.int) {
  v1_tau <- data %>% filter(., v == "v1")
  v2_tau <- data %>% filter(., v == "v2")
  v3_tau <- data %>% filter(., v == "v3")
  v4_tau <- data %>% filter(., v == "v4")
  v5_tau <- data %>% filter(., v == "v5")
  v6_tau <- data %>% filter(., v == "v6")
  ci1 <-
    t.test(
      round(v1_tau$tau_g1, digits = 3),
      round(v1_tau$tau_g2, digits = 3),
      alternative = "two.sided",
      paired = TRUE,
      conf.level = con.int
    )$conf.int
  ci2 <-
    t.test(
      v2_tau$tau_g1,
      v2_tau$tau_g2,
      alternative = "two.sided",
      paired = TRUE,
      conf.level = con.int
    )$conf.int
  ci3 <-
    t.test(
      v3_tau$tau_g1,
      v3_tau$tau_g2,
      alternative = "two.sided",
      paired = TRUE,
      conf.level = con.int
    )$conf.int
  ci4 <-
    t.test(
      v4_tau$tau_g1,
      v4_tau$tau_g2,
      alternative = "two.sided",
      paired = TRUE,
      conf.level = con.int
    )$conf.int
  ci5 <-
    t.test(
      v5_tau$tau_g1,
      v5_tau$tau_g2,
      alternative = "two.sided",
      paired = TRUE,
      conf.level = con.int
    )$conf.int
  ci6 <-
    t.test(
      v6_tau$tau_g1,
      v6_tau$tau_g2,
      alternative = "two.sided",
      paired = TRUE,
      conf.level = con.int
    )$conf.int
  c(
    prod(ci1) >= 0,
    prod(ci2) >= 0,
    prod(ci3) >= 0,
    prod(ci4) >= 0,
    prod(ci5) >= 0,
    prod(ci6) >= 0
  )
}

#detect non-invariant variable
detnon <-
  function(reps,
           nobs,
           la1,
           la2,
           phi1,
           phi2,
           th1,
           th2,
           tau1,
           tau2,
           fac_mean1,
           fac_mean2,
           testmd,
           con.int) {
    dta_list <-
      gen_dta(
        reps = reps,
        nobs = nobs,
        la1 = lambda1,
        phi1 = phi1,
        th1 = theta1,
        tau1 = tau1,
        fac_mean1 = fac_mean1,
        la2 = lambda2,
        phi2 = phi2,
        th2 = theta2,
        tau2 = tau2,
        fac_mean2 = fac_mean2
      )
    tau_dta_list <- gen_tau(data = dta_list, model = testmd)
    tau_dta <- rbindlist(tau_dta_list)
    check_non(data = tau_dta, con.int = con.int)
  }

#combine generating data and checking non-invariant variable together
detnon_list <-
  function(reps,
           nobs,
           la1,
           la2,
           phi1,
           phi2,
           th1,
           th2,
           tau1,
           tau2,
           fac_mean1,
           fac_mean2,
           testmd,
           con.int) {
    em_list <- vector(length = reps, mode = "list")
    mclapply(em_list, function(x) {
      detnon(
        reps = reps,
        nobs = nobs,
        la1 = la1,
        la2 = la2,
        phi1 = phi1,
        phi2 = phi2,
        th1 = th1,
        th2 = th2,
        tau1 = tau1,
        tau2 = tau2,
        fac_mean1 = fac_mean1,
        fac_mean2 = fac_mean2,
        testmd = mdconf,
        con.int = con.int
      )
    }, mc.cores = 12)
  }


# perfect recovery rate:completely detects non-invariant variable ---------


#non_con: non-invariant variable enter TRUE, NA enter NA, invariant enter FALSE
det_non <- function(det_list, non_con) {
  sapply(det_list, function(x) {
    ifelse(compare(x, non_con)$result, 1, 0)
  })
}


# model-level Type I error ------------------------------------------------


det_tyi <- function(det_list) {
  sapply(det_list, function(x) {
    ifelse(x[3] == TRUE, 1, ifelse(x[5] == TRUE, 1, ifelse(x[6] == TRUE, 1, 0)))
  })
}


# model-level Type II error -----------------------------------------------


det_tyii <- function(det_list) {
  sapply(det_list, function(x) {
    ifelse(x[2] == FALSE, 1, ifelse(x[4] == FALSE, 1, 0))
  })
}
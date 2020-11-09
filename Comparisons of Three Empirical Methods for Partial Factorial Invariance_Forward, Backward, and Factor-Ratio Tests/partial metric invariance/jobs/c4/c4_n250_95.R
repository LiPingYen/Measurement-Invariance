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


# create lambda dataframe from result of cfa ------------------------------


gen_lam <- function(data, model) {
  mclapply(data, function(x) {
    fit <- cfa(data = x,
               model = model,
               group = "group")
    lam_g1 <- parameterEstimates(fit)[1:6, 7]
    lam_g2 <- parameterEstimates(fit)[21:26, 7]
    data.frame(
      lambda_g1 = lam_g1,
      lambda_g2 = lam_g2,
      v = c("v1", "v2", "v3", "v4", "v5", "v6")
    )
  }, mc.cores = 1)
}


# check variable is non-invariant or not ----------------------------------


check_non <- function(data, con.int) {
  v1_lam <- data %>% filter(., v == "v1")
  v2_lam <- data %>% filter(., v == "v2")
  v3_lam <- data %>% filter(., v == "v3")
  v4_lam <- data %>% filter(., v == "v4")
  v5_lam <- data %>% filter(., v == "v5")
  v6_lam <- data %>% filter(., v == "v6")
  ci1 <-
    t.test(
      v1_lam$lambda_g1,
      v1_lam$lambda_g2,
      alternative = "two.sided",
      paired = TRUE,
      conf.level = con.int
    )$conf.int
  ci2 <-
    t.test(
      v2_lam$lambda_g1,
      v2_lam$lambda_g2,
      alternative = "two.sided",
      paired = TRUE,
      conf.level = con.int
    )$conf.int
  ci3 <-
    t.test(
      v3_lam$lambda_g1,
      v3_lam$lambda_g2,
      alternative = "two.sided",
      paired = TRUE,
      conf.level = con.int
    )$conf.int
  ci4 <-
    t.test(
      v4_lam$lambda_g1,
      v4_lam$lambda_g2,
      alternative = "two.sided",
      paired = TRUE,
      conf.level = con.int
    )$conf.int
  ci5 <-
    t.test(
      v5_lam$lambda_g1,
      v5_lam$lambda_g2,
      alternative = "two.sided",
      paired = TRUE,
      conf.level = con.int
    )$conf.int
  ci6 <-
    t.test(
      v6_lam$lambda_g1,
      v6_lam$lambda_g2,
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
    lam_dta_list <- gen_lam(data = dta_list, model = testmd)
    lam_dta <- rbindlist(lam_dta_list)
    check_non(data = lam_dta, con.int = con.int)
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


det_tyi <- function(det_list, non_con) {
  sapply(det_list, function(x) {
    ifelse(x[3] == TRUE, 1, ifelse(x[5] == TRUE, 1, ifelse(x[6] == TRUE, 1, 0)))
  })
}

# model-level Type II error -----------------------------------------------


det_tyii <- function(det_list, non_con) {
  sapply(det_list, function(x) {
    ifelse(x[2] == FALSE, 1, ifelse(x[4] == FALSE, 1, 0))
  })
}


##mixed-size difference model
options(digits = 4)

#PMI

# n=250 -------------------------------------------------------------------


#CI=.95
#generate population data
reps = 500
nobs = 250
con.int = .95
non_con <- c(NA, TRUE, FALSE, TRUE, FALSE, FALSE)

#group1
lambda1 <- matrix(rep(0.7, 6), nrow = 6)
phi1 <- 1
theta1 <- diag(rep(0.3, 6))
tau1 <- matrix(rep(1, 6), nrow = 6)
fac_mean1 = 0

#group2
lambda2 <- matrix(c(0.7, 0.4, 0.7, 0.2, 0.7, 0.7), nrow = 6)
phi2 <- 1.3
theta2 <- diag(rep(0.3, 6))
tau2 <- matrix(rep(1, 6), nrow = 6)
fac_mean2 = 0.2

#test model
mdconf <- '
fac1=~0.7*X1+X2+X3+X4+X5+X6
fac1~c(0,NA)*1
X1~tau*1
'

#forward method using CI
det_list <-
  detnon_list(
    reps = reps,
    nobs = nobs,
    la1 = lambda1,
    la2 = lambda2,
    phi1 = phi1,
    phi2 = phi2,
    th1 = theta1,
    th2 = theta2,
    tau1 = tau1,
    tau2 = tau2,
    fac_mean1 = fac_mean1,
    fac_mean2 = fac_mean2,
    testmd = mdconf,
    con.int = con.int
  )

#check if the variable is non-invariant or not
non_all <- det_non(det_list = det_list, non_con = non_con)
pe_re_rate <- mean(non_all)

#type I error
tyi_err <- det_tyi(det_list = det_list, non_con = non_con)
tyi_rate <- mean(tyi_err)

#type II error
tyii_err <- det_tyii(det_list = det_list, non_con = non_con)
tyii_rate <- mean(tyii_err)
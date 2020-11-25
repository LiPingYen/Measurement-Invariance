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


gen_tau <- function(data, model) {
  mclapply(data, function(x) {
    fit <- cfa(data = x,
               model = model,
               group = "group")
    dtp <- parameterEstimates(fit)[41:45, 10]
    data.frame(v = c("dt2", "dt3", "dt4", "dt5", "dt6"),
               diff_tau_pvalue = dtp)
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


# model-level Type I error (for baseline model only)------------------------------------------------


det_tyi <- function(det_list) {
  sapply(det_list, function(x) {
    ifelse(x[1] == TRUE, 1, ifelse(x[2] == TRUE, 1, ifelse(
      x[3] == TRUE, 1, ifelse(x[4] == TRUE, 1, ifelse(x[5] == TRUE, 1, 0))
    )))
  })
}


# model-level Type II error -----------------------------------------------


det_tyii <- function(det_list) {
  sapply(det_list, function(x) {
    ifelse(x[1] == FALSE, 1, ifelse(x[3] == FALSE, 1, 0))
  })
}


##baseline model
options(digits = 4)

#PSI

# n=1000 ------------------------------------------------------------------


#CI=.95
#generate population data
reps = 1000
nobs = 1000
p_value = 0.05
non_con <- c(FALSE, FALSE, FALSE, FALSE, FALSE)#dt2,dt3,dt4,dt5,dt6

#group1
lambda1 <- matrix(rep(0.7, 6), nrow = 6)
phi1 <- 1
theta1 <- diag(rep(0.3, 6))
tau1 <- matrix(rep(1, 6), nrow = 6)
fac_mean1 = 0

#group2
lambda2 <- matrix(rep(0.7, 6), nrow = 6)
phi2 <- 1.3
theta2 <- diag(rep(0.3, 6))
tau2 <- matrix(rep(1, 6), nrow = 6)
fac_mean2 = 0.2

#test model
mdconf <- '
fac1=~0.7*X1+lm2*X2+lm3*X3+lm4*X4+lm5*X5+lm6*X6
fac1~c(0,NA)*1
X1~tau*1
X2~c(t21,t22)*1
X3~c(t31,t32)*1
X4~c(t41,t42)*1
X5~c(t51,t52)*1
X6~c(t61,t62)*1
dt2:=t21-t22
dt3:=t31-t32
dt4:=t41-t42
dt5:=t51-t52
dt6:=t61-t62
'

#forward method using CI
dta <- gen_dta(
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
  fac_mean2 = fac_mean2
)

tau_list <- gen_tau(data = dta, model = mdconf)

non_v_list <- check_non(data = tau_list, p_value = p_value)

#check if the variable is non-invariant or not
non_all <- det_non(det_list = non_v_list, non_con = non_con)
pe_re_rate <- mean(non_all)

#type I error
tyi_err <- det_tyi(det_list = non_v_list)
tyi_rate <- mean(tyi_err)
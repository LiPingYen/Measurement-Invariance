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
    converge <- lavInspect(fit, what = "converged")
    z <- data.frame(v = c("dl2", "dl3", "dl4", "dl5", "dl6"),
                    diff_lam_pvalue = dlp)
    list(z, converge)
  }, mc.cores = 12)
}


# check variable is non-invariant or not ----------------------------------


check_non <- function(data, p_value) {
  dif_lam_p <- lapply(data, function(x) {
    x[[1]]
  })
  mclapply(dif_lam_p, function(y) {
    c(y[1, 2] < p_value,
      y[2, 2] < p_value,
      y[3, 2] < p_value,
      y[4, 2] < p_value,
      y[5, 2] < p_value)
  }, mc.cores = 1)
}


#convergence rate


conv_rate <-
  function(non_v_li) {
    mean(sapply(lapply(non_v_li, function(x) {
      x[[2]]
    }), function(y) {
      y
    }))
  }

# perfect recovery rate:completely detects non-invariant variable ---------


#non_con: non-invariant variable enter TRUE, invariant enter FALSE
det_non <- function(det_list, non_con) {
  sapply(det_list, function(x) {
    ifelse(compare(x, non_con)$result, 1, 0)
  })
}

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


##mixed-size difference model
options(digits = 4)

#PMI

# n=250 -------------------------------------------------------------------


#CI=.99
#generate population data
reps = 1000
nobs = 250
p_value = 0.01
non_con <- c(TRUE, FALSE, TRUE, FALSE, FALSE)#dl2,dl3,dl4,dl5,dl6

#group1
lambda1 <- matrix(rep(0.7, 6), nrow = 6)
phi1 <- 1
theta1 <- diag(rep(0.3, 6))
tau1 <- matrix(rep(1, 6), nrow = 6)
fac_mean1 = 0

#group2
lambda2 <- matrix(c(0.7,0.4,0.7,0.2,0.7,0.7), nrow = 6)
phi2 <- 1.3
theta2 <- diag(rep(0.3, 6))
tau2 <- matrix(rep(1, 6), nrow = 6)
fac_mean2 = 0.2

#test model
mdconf <- '
fac1=~0.7*X1+c(l21,l22)*X2+c(l31,l32)*X3+c(l41,l42)*X4+c(l51,l52)*X5+c(l61,l62)*X6
fac1~c(0,NA)*1
X1~tau*1
dl2:=l21-l22
dl3:=l31-l32
dl4:=l41-l42
dl5:=l51-l52
dl6:=l61-l62
'

#forward method using CI
seed<-sample(1:100000,1)
set.seed(seed)

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

lam_list <- gen_lam(data = dta, model = mdconf)

non_v_list <- check_non(data = lam_list, p_value = p_value)

#check if the variable is non-invariant or not
non_all <- det_non(det_list = non_v_list, non_con = non_con)
pe_re_rate <- mean(non_all)

#type I error
tyi_err <- det_tyi(det_list = non_v_list)
tyi_rate <- mean(tyi_err)

#type II error
tyii_err <- det_tyii(det_list = non_v_list)
tyii_rate <- mean(tyii_err)

#convergence rate
convergence_rate <-conv_rate(non_v_li = lam_list)
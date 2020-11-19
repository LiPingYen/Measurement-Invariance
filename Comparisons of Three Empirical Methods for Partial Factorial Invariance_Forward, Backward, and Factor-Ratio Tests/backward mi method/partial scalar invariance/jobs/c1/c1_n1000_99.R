library(lavaan)
library(mvtnorm)
library(dplyr)
library(parallel)
library(data.table)
library(compare)
library(stringr)


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


# 產出noninvariant的變項，使用平行運算 ------------------------------------------------


det_non_v <- function(md, dta) {
  mclapply(dta, function(x) {
    fit <- cfa(
      model = md,
      data = x,
      group = "group",
      group.equal = c("loadings", "intercepts")
    )
    lavp <-
      lavTestScore(fit)$uni %>% subset(!lhs %in% c(".p2.", ".p3.", ".p4.", ".p5.", ".p6.", ".p8.")) %>% arrange(p.value)#移除lam恆等的那列
    lavp <- lavp[1, ]
    fre_va <- vector()
    non_int_each <- vector()
    n <- 1
    while (lavp[, 6] < p_value) {
      non_int_each[n] <- lavp$lhs
      X <-
        str_extract_all(lavp$lhs, "(\\d)+")[[1]] %>% as.numeric() %>% -14 %>% as.character()
      fre_va[n] <- paste0("X", X, "~1")
      fit_i <-
        cfa(
          model = mdconf,
          data = x,
          group = "group",
          group.equal = c("loadings", "intercepts"),
          group.partial = fre_va
        )
      lavp <- lavTestScore(fit_i)$uni %>% subset(!lhs %in% c(".p2.", ".p3.", ".p4.", ".p5.", ".p6.", ".p8.")) %>% arrange(p.value)
      lavp <- lavp[1, ]
      n = n + 1
    }
    non_int_each
  }, mc.cores = 12)
}


# perfect recovery rate:completely detects non-invariant variable ---------


#non_con: non-invariant variable enter TRUE, invariant enter FALSE
det_non <- function(det_list, non_con) {
  sapply(det_list, function(x) {
    ifelse(compare(x, non_con, ignoreOrder = TRUE)$result, 1, 0)
  })
}


# model-level Type I error (only for baseline model) ----------------------


det_tyi <- function(det_list) {
  sapply(det_list, function(x) {
    ifelse(any(x %in% c(".p16.", ".p17.", ".p18.", ".p19.", ".p20.")), 1, 0)
  })
}


# model-level Type II error -----------------------------------------------


det_tyii <- function(det_list, non_con) {
  sapply(det_list, function(x) {
    ifelse(any(x %in% ".p16."), ifelse(any(x %in% ".p18."), 0, 1), 1)
  })
}


##baseline model
options(digits = 4)

#PMI

# n=1000 ------------------------------------------------------------------


#CI=.99
#generate population data
reps = 1000
nobs = 1000
p_value = 0.01
non_con <- NA

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
fac1=~0.7*X1+X2+X3+X4+X5+X6
fac1~c(0,NA)*1
X1~tau*1
'

#backward method using MI
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

non_v_list<-det_non_v(md = mdconf, dta = dta)

#perfect recovery rate
non_all <- det_non(det_list = non_v_list, non_con = non_con)
pe_re_rate <- mean(non_all)

#type I error
tyi_err <- det_tyi(det_list = non_v_list)
tyi_rate <- mean(tyi_err)
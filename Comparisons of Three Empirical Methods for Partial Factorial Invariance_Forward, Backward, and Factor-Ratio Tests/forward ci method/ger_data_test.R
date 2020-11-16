library(lavaan)
library(mvtnorm)
library(dplyr)
library(semPlot)

####Generating data####
##用simulateData
#group1
ge_md1 <- '
fac1=~0.7*x1+0.7*x2+0.7*x3+0.7*x4+0.7*x5+0.7*x6
x1+x2+x3+x4+x5+x6~1*1
fac1~~1*fac1
fac1~0*1
x1~~0.3*x1
x2~~0.3*x2
x3~~0.3*x3
x4~~0.3*x4
x5~~0.3*x5
x6~~0.3*x6
'

mdconf <- '
fac1=~1*x1+x2+x3+x4+x5+x6
'
reps = 1000
nobs = 1000
dta <-
  simulateData(
    model = ge_md1,
    model.type = "cfa",
    return.type = "data.frame",
    sample.nobs = nobs
  )
dta_list <-
  replicate(
    n = reps,
    simulateData(
      model = ge_md1,
      model.type = "cfa",
      return.type = "data.frame",
      sample.nobs = nobs
    ),
    simplify = FALSE
  )



#用前一篇論文寫的code
gen_dta_test <- function(nobs, la1, phi1, th1, tau1, fac_mean) {
  pop_co_ma1 <- la1 %*% phi1 %*% t(la1) + th1
  pop_mean <- tau1 + lambda1 %*% fac_mean
  dta <-
    data.frame(rmvnorm(
      nobs,
      mean = pop_mean,
      sigma = pop_co_ma1,
      method = "chol"
    ))#rep(mean,)mean要帶公式,tau+lamda*latent mean
  dta <- dta %>%
    rename(
      x1 = X1,
      x2 = X2,
      x3 = X3,
      x4 = X4,
      x5 = X5,
      x6 = X6
    )
  dta
}

nobs = 1000
lambda1 <- matrix(rep(0.7, 6), nrow = 6)
phi1 <- 1
theta1 <- diag(rep(0.3, 6))
tau1 <- matrix(rep(1, 6), nrow = 6)
fac_mean = 0
dta2 <-
  data.frame(
    gen_dta_test(
      nobs = nobs,
      la1 = lambda1,
      phi1 = phi1,
      th1 = theta1,
      tau1 = tau1,
      fac_mean = fac_mean
    )
  )
dta_list2 <-
  replicate(n = reps, data.frame(
    gen_dta_test(
      nobs = nobs,
      la1 = lambda1,
      phi1 = phi1,
      th1 = theta1,
      tau1 = tau1,
      fac_mean = fac_mean
    )
  ), simplify = FALSE)

#兩種方式產生資料比較
apply(dta, 2, mean)
apply(dta2, 2, mean)

cov(dta)
cov(dta2)

#cfa plot
fit1 <- cfa(model = mdconf, data = dta)
fit2 <- cfa(model = mdconf, data = dta2)
semPaths(fit1, what = "std")
semPaths(fit2, what = "std")

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
    })
  }

reps = 100
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
lambda2 <- matrix(c(0.7, 0.5, 0.7, 0.5, 0.7, 0.7), nrow = 6)
phi2 <- 1.3
theta2 <- diag(rep(0.3, 6))
tau2 <- matrix(c(1, 0.8, 1, 0.8, 1, 1), nrow = 6)
fac_mean2 = 0.2

#test model
mdconf <- '
fac1=~c(v1,v1)*X1+X2+X3+X4+X5+X6
X1~c(1,1)*1
'

dta_test_list <-
  gen_dta(
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

fit <- cfa(data = dta_test_list[[1]],
           model = mdconf,
           group = "group",
           group.equal=c("loadings"))

summary(fit)

tau_test<-gen_tau(data = dta_test_list,model = mdconf)
tau_dta_test <- rbindlist(tau_test)

check_lsit<-check_non(data = tau_dta_test,con.int = con.int)

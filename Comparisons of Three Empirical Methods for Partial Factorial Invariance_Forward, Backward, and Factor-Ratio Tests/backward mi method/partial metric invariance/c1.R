options(digits = 4)
##baseline model

#PMI

# n=250 -------------------------------------------------------------------


#CI=.95
#generate population data
reps = 1000
nobs = 250
p_value = 0.05
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

#convergence rate
convergence_rate <-conv_rate(non_v_li = non_v_list)
convergence_rate


#CI=.99
p_value = 0.01

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

#convergence rate
convergence_rate <-conv_rate(non_v_li = non_v_list)
convergence_rate


# n=500 -------------------------------------------------------------------


#CI=.95
#generate population data
reps = 1000
nobs = 500
p_value = 0.05

#test model
mdconf <- '
fac1=~0.7*X1+X2+X3+X4+X5+X6
fac1~c(0,NA)*1
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

#convergence rate
convergence_rate <-conv_rate(non_v_li = non_v_list)
convergence_rate


#CI=.99
p_value = 0.01

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


# n=1000 ------------------------------------------------------------------


#CI=.95
#generate population data
reps = 1000
nobs = 1000
p_value = 0.05

#test model
mdconf <- '
fac1=~0.7*X1+X2+X3+X4+X5+X6
fac1~c(0,NA)*1
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

#convergence rate
convergence_rate <-conv_rate(non_v_li = non_v_list)
convergence_rate


#CI=.99
p_value = 0.01

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

#convergence rate
convergence_rate <-conv_rate(non_v_li = non_v_list)
convergence_rate
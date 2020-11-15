options(digits = 4)
##baseline model

#PSI

# n=250 -------------------------------------------------------------------


#CI=.95
#generate population data
reps = 1000
nobs = 250
p_value = 0.05
non_con <- c(TRUE, FALSE, TRUE, FALSE, FALSE)#dt2,dt3,dt4,dt5,dt6

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


#CI=.99
p_value = 0.01

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


# n=500 -------------------------------------------------------------------


#CI=.95
#generate population data
reps = 1000
nobs = 500
p_value = 0.05

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


#CI=.99
p_value = 0.01

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


# n=1000 ------------------------------------------------------------------


#CI=.95
#generate population data
reps = 1000
nobs = 1000
p_value = 0.05

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


#CI=.99
p_value = 0.01

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
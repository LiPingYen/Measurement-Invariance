options(digits = 4)
##mixed-size difference model

#PMI

# n=250 -------------------------------------------------------------------


#CI=.95
#generate population data
reps = 1000
nobs = 250
p_value = 0.05
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

lam_list <- gen_lam(data = dta, model = mdconf)

non_v_list <- check_non(data = lam_list, p_value = p_value)

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

lam_list <- gen_lam(data = dta, model = mdconf)

non_v_list <- check_non(data = lam_list, p_value = p_value)

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

lam_list <- gen_lam(data = dta, model = mdconf)

non_v_list <- check_non(data = lam_list, p_value = p_value)

#check if the variable is non-invariant or not
non_all <- det_non(det_list = non_v_list, non_con = non_con)
pe_re_rate <- mean(non_all)

#type I error
tyi_err <- det_tyi(det_list = non_v_list)
tyi_rate <- mean(tyi_err)
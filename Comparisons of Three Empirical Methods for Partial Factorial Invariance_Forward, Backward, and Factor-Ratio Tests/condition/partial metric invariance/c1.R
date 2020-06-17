options(digits = 4)
##baseline model

####PMI####
####n=250####
####CI=.95####
#generate population data
reps = 1000
nobs = 250
con.int = .95
non_con <- c(NA, FALSE, FALSE, FALSE, FALSE, FALSE)

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
fac1=~c(v1,v1)*X1+X2+X3+X4+X5+X6
'

####forward method using CI####
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
mean(non_all)

#type I error
tyi_err <- det_tyi(det_list = det_list, non_con = non_con)
mean(tyi_err)

#type II error
tyii_err <- det_tyii(det_list = det_list, non_con = non_con)
mean(tyii_err)


####CI=.99####
con.int = .99
####forward method using CI####
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
mean(non_all)

#type II error
tyii_err <- det_tyii(det_list = det_list, non_con = non_con)
mean(tyii_err)


####n=500####
####CI=.95####
#generate population data
reps = 1000
nobs = 500
con.int = .95

#test model
mdconf <- '
fac1=~c(v1,v1)*x1+x2+x3+x4+x5+x6
'

####forward method using CI####
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
mean(non_all)

#type I error
tyi_err <- det_tyi(det_list = det_list, non_con = non_con)
mean(tyi_err)

#type II error
tyii_err <- det_tyii(det_list = det_list, non_con = non_con)
mean(tyii_err)


####CI=.99####
con.int = .99
####forward method using CI####
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
mean(non_all)

#type I error
tyi_err <- det_tyi(det_list = det_list, non_con = non_con)
mean(tyi_err)

#type II error
tyii_err <- det_tyii(det_list = det_list, non_con = non_con)
mean(tyii_err)


####n=1000####
####CI=.95####
#generate population data
reps = 1000
nobs = 1000
con.int = .95

#test model
mdconf <- '
fac1=~c(v1,v1)*x1+x2+x3+x4+x5+x6
'

####forward method using CI####
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
mean(non_all)

#type I error
tyi_err <- det_tyi(det_list = det_list, non_con = non_con)
mean(tyi_err)

#type II error
tyii_err <- det_tyii(det_list = det_list, non_con = non_con)
mean(tyii_err)

####CI=.99####
con.int = .99
####forward method using CI####
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
mean(non_all)

#type I error
tyi_err <- det_tyi(det_list = det_list, non_con = non_con)
mean(tyi_err)

#type II error
tyii_err <- det_tyii(det_list = det_list, non_con = non_con)
mean(tyii_err)
options(digits = 4)
##con:high, LD: large, nonpropotional

####n=200,v=6####
####generate population data####
reps = 100
nobs = 200
lambda1_1 <- matrix(c(0.7, 0.9, 0.5, 0.6, 0.8, 0.3), nrow = 6)
phi1 <- 1
lambda2_8 <- matrix(c(0.25, 0.65, 0.5, 0.45, 0.45, 0.3), nrow = 6)
phi2 <- 1.3
theta2_8 <- diag(c(0.5, 1.2, 0.4, 0.4, 0.9, 0.2))

dta_list <-
  replicate(n = reps, data.frame(
    gen_dta_v6(
      nobs = nobs,
      la1 = lambda1_1,
      la2 = lambda2_8,
      phi1 = phi1,
      phi2 = phi2,
      th2 = theta2_8
    )
  ), simplify = FALSE)

md <- "
fac1=~NA*v1+v2+v3+v4+v5+v6
fac1~~c(1,NA)*fac1
"

####gain original model RMSEA####
cpu.cores <- detectCores()
cl <- makeCluster(cpu.cores, type = "FORK")
rmsea_all_c8_n200 <- rmfun_papl(md = md, dta = dta_list)
data.frame(mean = mean(rmsea_all_c8_n200),
           sd = sd(rmsea_all_c8_n200))

stopCluster(cl)

####perfect recovery rate####
non_var <- non_var_papl(md = md, dta = dta_list)
non_all <-
  det_non_pl(non_var = non_var,
             non_con = c(".p1.", ".p2.", ".p4.", ".p5."))
mean(non_all)

####true detection####
true_det_pl(non_var = non_var,
            non_con = c(".p1.", ".p2.", ".p4.", ".p5."))

####false detection####
fal_det_pl(non_var = non_var, inv_con = c(".p3.", ".p6."))


####n=500,v=6####
####generate population data####
reps = 100
nobs = 500
lambda1_1 <- matrix(c(0.7, 0.9, 0.5, 0.6, 0.8, 0.3), nrow = 6)
phi1 <- 1
lambda2_8 <- matrix(c(0.25, 0.65, 0.5, 0.45, 0.45, 0.3), nrow = 6)
phi2 <- 1.3
theta2_8 <- diag(c(0.5, 1.2, 0.4, 0.4, 0.9, 0.2))

dta_list <-
  replicate(n = reps, data.frame(
    gen_dta_v6(
      nobs = nobs,
      la1 = lambda1_1,
      la2 = lambda2_8,
      phi1 = phi1,
      phi2 = phi2,
      th2 = theta2_8
    )
  ), simplify = FALSE)

md <- "
fac1=~NA*v1+v2+v3+v4+v5+v6
fac1~~c(1,NA)*fac1
"

####gain original model RMSEA####
cpu.cores <- detectCores()
cl <- makeCluster(cpu.cores, type = "FORK")
rmsea_all_c8_n500 <- rmfun_papl(md = md, dta = dta_list)
data.frame(mean = mean(rmsea_all_c8_n500),
           sd = sd(rmsea_all_c8_n500))

stopCluster(cl)

####perfect recovery rate####
non_var <- non_var_papl(md = md, dta = dta_list)
non_all <-
  det_non_pl(non_var = non_var,
             non_con = c(".p1.", ".p2.", ".p4.", ".p5."))
mean(non_all)

####true detection####
true_det_pl(non_var = non_var,
            non_con = c(".p1.", ".p2.", ".p4.", ".p5."))

####false detection####
fal_det_pl(non_var = non_var, inv_con = c(".p3.", ".p6."))
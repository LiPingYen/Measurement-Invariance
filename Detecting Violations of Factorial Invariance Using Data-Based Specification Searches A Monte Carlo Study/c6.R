options(digits = 4)
##con:high, LD: large

####n=200,v=6####
####generate population data####
reps=100
nobs=200
lambda1_1<-matrix(c(0.7,0.9,0.5,0.6,0.8,0.3),nrow= 6)
phi1<-1
lambda2_6<-matrix(c(0.4,0.51,0.5,0.34,0.46,0.3),nrow = 6)
phi2<-1.3
theta2_6<-diag(c(0.7,1.2,0.4,0.5,0.9,0.2))

for(i in 1:reps){
  assign(paste0("dta6_", i), data.frame(gen_dta_v6(nobs = nobs,la1=lambda1_1,la2 = lambda2_6,
                                                phi1 = phi1,phi2 = phi2,
                                                th2 = theta2_6)))
}

dta_list = mget(apropos("dta6_"))
md<-"
fac1=~NA*v1+v2+v3+v4+v5+v6
fac1~~c(1,NA)*fac1
"

####gain original model RMSEA####
cpu.cores <- detectCores()
cl <- makeCluster(cpu.cores,type = "FORK")
rmsea_all_c6_n200<-rmfun_papl(md=md,dta = dta_list)
data.frame(mean=mean(rmsea_all_c6_n200),sd=sd(rmsea_all_c6_n200))

stopCluster(cl)

####perfect recovery rate####
non_var<-non_var_papl(md=md,dta=dta_list)
non_all<-det_non_pl(non_var = non_var,non_con = c(".p1.",".p2.",".p4.",".p5."))
mean(non_all)

####true detection####
true_det_pl(non_var = non_var,non_con = c(".p1.",".p2.",".p4.",".p5."))

####false detection####
fal_det_pl(non_var = non_var,inv_con = c(".p3.",".p6."))


####n=500,v=6####
####generate population data####
reps=100
nobs=500
lambda1_1<-matrix(c(0.7,0.9,0.5,0.6,0.8,0.3),nrow= 6)
phi1<-1
lambda2_6<-matrix(c(0.4,0.51,0.5,0.34,0.46,0.3),nrow = 6)
phi2<-1.3
theta2_6<-diag(c(0.7,1.2,0.4,0.5,0.9,0.2))

for(i in 1:reps){
  assign(paste0("dta6_", i), data.frame(gen_dta_v6(nobs = nobs,la1=lambda1_1,la2 = lambda2_6,
                                                phi1 = phi1,phi2 = phi2,
                                                th2 = theta2_6)))
}

dta_list = mget(apropos("dta6_"))
md<-"
fac1=~NA*v1+v2+v3+v4+v5+v6
fac1~~c(1,NA)*fac1
"

####gain original model RMSEA####
cpu.cores <- detectCores()
cl <- makeCluster(cpu.cores,type = "FORK")
rmsea_all_c6_n500<-rmfun_papl(md=md,dta = dta_list)
data.frame(mean=mean(rmsea_all_c6_n500),sd=sd(rmsea_all_c6_n500))

stopCluster(cl)

####perfect recovery rate####
non_var<-non_var_papl(md=md,dta=dta_list)
non_all<-det_non_pl(non_var = non_var,non_con = c(".p1.",".p2.",".p4.",".p5."))
mean(non_all)

####true detection####
true_det_pl(non_var = non_var,non_con = c(".p1.",".p2.",".p4.",".p5."))

####false detection####
fal_det_pl(non_var = non_var,inv_con = c(".p3.",".p6."))


####n=200,v=12####
####generate population data####
reps=100
nobs=200
lambda1_1<-matrix(c(0.7,0.9,0.5,0.6,0.8,0.3,0.7,0.9,0.5,0.6,0.8,0.3),nrow= 12)
phi1<-1
lambda2_6<-matrix(c(0.4,0.51,0.5,0.34,0.46,0.3,0.4,0.51,0.5,0.34,0.46,0.3),nrow = 12)
phi2<-1.3
theta2_6<-diag(c(0.7,1.2,0.4,0.5,0.9,0.2,0.7,1.2,0.4,0.5,0.9,0.2))

for(i in 1:reps){
  assign(paste0("dta6_", i), data.frame(gen_dta_v12(nobs = nobs,la1=lambda1_1,la2 = lambda2_6,
                                                phi1 = phi1,phi2 = phi2,
                                                th2 = theta2_6)))
}

dta_list = mget(apropos("dta6_"))
md<-"
fac1=~NA*v1+v2+v3+v4+v5+v6+v7+v8+v9+v10+v11+v12
fac1~~c(1,NA)*fac1
"

####gain original model RMSEA####
cpu.cores <- detectCores()
cl <- makeCluster(cpu.cores,type = "FORK")
rmsea_all_c6_n200<-rmfun_papl(md=md,dta = dta_list)
data.frame(mean=mean(rmsea_all_c6_n200),sd=sd(rmsea_all_c6_n200))

stopCluster(cl)

####perfect recovery rate####
non_var<-non_var_papl(md=md,dta=dta_list)
non_all<-det_non_pl(non_var = non_var,non_con = c(".p1.",".p2.",".p4.",".p5.",".p7.",".p8.",".p10.",".p11."))
mean(non_all)

####true detection####
true_det_pl(non_var = non_var,non_con = c(".p1.",".p2.",".p4.",".p5.",".p7.",".p8.",".p10.",".p11."))

####false detection####
fal_det_pl(non_var = non_var,inv_con = c(".p3.",".p6."))


####n=500,v=12####
####generate population data####
reps=100
nobs=500
lambda1_1<-matrix(c(0.7,0.9,0.5,0.6,0.8,0.3,0.7,0.9,0.5,0.6,0.8,0.3),nrow= 12)
phi1<-1
lambda2_6<-matrix(c(0.4,0.51,0.5,0.34,0.46,0.3,0.4,0.51,0.5,0.34,0.46,0.3),nrow = 12)
phi2<-1.3
theta2_6<-diag(c(0.7,1.2,0.4,0.5,0.9,0.2,0.7,1.2,0.4,0.5,0.9,0.2))

for(i in 1:reps){
  assign(paste0("dta6_", i), data.frame(gen_dta_v12(nobs = nobs,la1=lambda1_1,la2 = lambda2_6,
                                                phi1 = phi1,phi2 = phi2,
                                                th2 = theta2_6)))
}

dta_list = mget(apropos("dta6_"))
md<-"
fac1=~NA*v1+v2+v3+v4+v5+v6+v7+v8+v9+v10+v11+v12
fac1~~c(1,NA)*fac1
"

####gain original model RMSEA####
cpu.cores <- detectCores()
cl <- makeCluster(cpu.cores,type = "FORK")
rmsea_all_c6_n500<-rmfun_papl(md=md,dta = dta_list)
data.frame(mean=mean(rmsea_all_c6_n500),sd=sd(rmsea_all_c6_n500))

stopCluster(cl)

####perfect recovery rate####
non_var<-non_var_papl(md=md,dta=dta_list)
non_all<-det_non_pl(non_var = non_var,non_con = c(".p1.",".p2.",".p4.",".p5.",".p7.",".p8.",".p10.",".p11."))
mean(non_all)

####true detection####
true_det_pl(non_var = non_var,non_con = c(".p1.",".p2.",".p4.",".p5.",".p7.",".p8.",".p10.",".p11."))

####false detection####
fal_det_pl(non_var = non_var,inv_con = c(".p3.",".p6."))
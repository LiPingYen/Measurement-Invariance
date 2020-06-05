options(digits = 4)
##con:low, LD: small

####n=200,v=6####
####generate population data####
reps=100
nobs=200
lambda1_1<-matrix(c(0.7,0.9,0.5,0.6,0.8,0.3),nrow= 6)
phi1<-1
lambda2_1<-matrix(c(0.6,0.77,0.5,0.6,0.8,0.3),nrow = 6)
phi2<-1.3
theta2_1<-diag(c(0.8,1.3,0.4,0.5,0.9,0.2))


dta_list<-replicate(n=reps,data.frame(gen_dta_v6(nobs = nobs,la1=lambda1_1,la2 = lambda2_1,
                                       phi1 = phi1,phi2 = phi2,
                                       th2 = theta2_1)),simplify = FALSE)

md<-"
fac1=~NA*v1+v2+v3+v4+v5+v6
fac1~~c(1,NA)*fac1
"

####gain all RMSEA from original####
cpu.cores <- detectCores()
cl <- makeCluster(cpu.cores,type = "FORK")
rmsea_all_c1_n200<-rmfun_papl(md=md,dta = dta_list)
data.frame(mean=mean(rmsea_all_c1_n200),sd=sd(rmsea_all_c1_n200))

stopCluster(cl)

####perfect recovery rate####
non_var<-non_var_papl(md=md,dta=dta_list_test)
non_all<-det_non_pl(non_var = non_var,non_con = c(".p1.",".p2."))
mean(non_all)

####true detection####
true_det_pl(non_var = non_var,non_con = c(".p1.",".p2."))

####false detection####
fal_det_pl(non_var = non_var,inv_con = c(".p3.",".p4.",".p5.",".p6."))


####n=500,v=6####
####generate population data####
reps=100
nobs=500
lambda1_1<-matrix(c(0.7,0.9,0.5,0.6,0.8,0.3),nrow= 6)
phi1<-1
lambda2_1<-matrix(c(0.6,0.77,0.5,0.6,0.8,0.3),nrow = 6)
phi2<-1.3
theta2_1<-diag(c(0.8,1.3,0.4,0.5,0.9,0.2))

dta_list<-replicate(n=reps,data.frame(gen_dta_v6(nobs = nobs,la1=lambda1_1,la2 = lambda2_1,
                                                phi1 = phi1,phi2 = phi2,
                                                th2 = theta2_1)),simplify = FALSE)

md<-"
fac1=~NA*v1+v2+v3+v4+v5+v6
fac1~~c(1,NA)*fac1
"

####gain all RMSEA from original####
cpu.cores <- detectCores()
cl <- makeCluster(cpu.cores,type = "FORK")
rmsea_all_c1_n500<-rmfun_papl(md=md,dta = dta_list)
data.frame(mean=mean(rmsea_all_c1_n500),sd=sd(rmsea_all_c1_n500))

stopCluster(cl)

####perfect recovery rate####
non_var<-non_var_papl(md=md,dta=dta_list)
non_all<-det_non_pl(non_var = non_var,non_con = c(".p1.",".p2."))
mean(non_all)

####true detection####
true_det_pl(non_var = non_var,non_con = c(".p1.",".p2."))

####false detection####
fal_det_pl(non_var = non_var,inv_con = c(".p3.",".p4.",".p5.",".p6."))


####n=200,v=12####
####generate population data####
reps=100
nobs=200
lambda1_1<-matrix(c(0.7,0.9,0.5,0.6,0.8,0.3,0.7,0.9,0.5,0.6,0.8,0.3),nrow= 12)
phi1<-1
lambda2_1<-matrix(c(0.6,0.77,0.5,0.6,0.8,0.3,0.6,0.77,0.5,0.6,0.8,0.3),nrow = 12)
phi2<-1.3
theta2_1<-diag(c(0.8,1.3,0.4,0.5,0.9,0.2,0.8,1.3,0.4,0.5,0.9,0.2))

dta_list<-replicate(n=reps,data.frame(gen_dta_v12(nobs = nobs,la1=lambda1_1,la2 = lambda2_1,
                                                phi1 = phi1,phi2 = phi2,
                                                th2 = theta2_1)),simplify = FALSE)

md<-"
fac1=~NA*v1+v2+v3+v4+v5+v6+v7+v8+v9+v10+v11+v12
fac1~~c(1,NA)*fac1
"

####gain all RMSEA from original####
cpu.cores <- detectCores()
cl <- makeCluster(cpu.cores,type = "FORK")
rmsea_all_c1_n200<-rmfun_papl(md=md,dta = dta_list)
data.frame(mean=mean(rmsea_all_c1_n200),sd=sd(rmsea_all_c1_n200))

stopCluster(cl)

####perfect recovery rate####
non_var<-non_var_papl(md=md,dta=dta_list)
non_all<-det_non_pl(non_var = non_var,non_con = c(".p1.",".p2.",".p7.",".p8."))
mean(non_all)

####true detection####
true_det_pl(non_var = non_var,non_con = c(".p1.",".p2.",".p7.",".p8."))

####false detection####
fal_det_pl(non_var = non_var,inv_con = c(".p3.",".p4.",".p5.",".p6.",".p9.",".p10.",".p11.",".p12."))



####n=500,v=12####
####generate population data####
reps=100
nobs=500
lambda1_1<-matrix(c(0.7,0.9,0.5,0.6,0.8,0.3,0.7,0.9,0.5,0.6,0.8,0.3),nrow= 12)
phi1<-1
lambda2_1<-matrix(c(0.6,0.77,0.5,0.6,0.8,0.3,0.6,0.77,0.5,0.6,0.8,0.3),nrow = 12)
phi2<-1.3
theta2_1<-diag(c(0.8,1.3,0.4,0.5,0.9,0.2,0.8,1.3,0.4,0.5,0.9,0.2))

dta_list<-replicate(n=reps,data.frame(gen_dta_v12(nobs = nobs,la1=lambda1_1,la2 = lambda2_1,
                                                phi1 = phi1,phi2 = phi2,
                                                th2 = theta2_1)),simplify = FALSE)

md<-"
fac1=~NA*v1+v2+v3+v4+v5+v6+v7+v8+v9+v10+v11+v12
fac1~~c(1,NA)*fac1
"

####gain all RMSEA from original####
cpu.cores <- detectCores()
cl <- makeCluster(cpu.cores,type = "FORK")
rmsea_all_c1_n500<-rmfun_papl(md=md,dta = dta_list)
data.frame(mean=mean(rmsea_all_c1_n500),sd=sd(rmsea_all_c1_n500))

stopCluster(cl)

####perfect recovery rate####
non_var<-non_var_papl(md=md,dta=dta_list)
non_all<-det_non_pl(non_var = non_var,non_con = c(".p1.",".p2.",".p7.",".p8."))
mean(non_all)

####true detection####
true_det_pl(non_var = non_var,non_con = c(".p1.",".p2.",".p7.",".p8."))

####false detection####
fal_det_pl(non_var = non_var,inv_con = c(".p3.",".p4.",".p5.",".p6.",".p9.",".p10.",".p11.",".p12."))
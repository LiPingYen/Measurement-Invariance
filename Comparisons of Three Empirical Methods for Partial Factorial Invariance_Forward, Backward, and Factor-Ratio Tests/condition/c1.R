options(digits = 4)
##baseline model

####PMI####
####n=250####
####CI=.95####
#generate population data
nobs=250

#group1
lambda1<-matrix(rep(0.7,6),nrow= 6)
phi1<-1
theta1<-diag(rep(0.3,6))
tau1<-matrix(rep(1,6),nrow= 6)
fac_mean1=0

#group2
lambda2<-matrix(rep(0.7,6),nrow= 6)
phi2<-1.3
theta2<-diag(rep(0.3,6))
tau2<-matrix(rep(1,6),nrow= 6)
fac_mean2=0.2

dta_list<-replicate(n=reps,gen_dta(nobs=nobs,la1 = lambda1,phi1 = phi1,th1 = theta1,tau1 = tau1,fac_mean1 = fac_mean1,
                                   la2 = lambda2,phi2 = phi2,th2 = theta2,tau2 = tau2,fac_mean2 = fac_mean2),simplify =FALSE )

lam_dta_list<-gen_lam(data = dta_list,model = mdconf)
lam_dta<-rbindlist(lam_dta_list)
check_non(data = lam_dta,con.int = con.int)

#test model
mdconf<-'
fac1=~c(v1,v1)*x1+x2+x3+x4+x5+x6
'

reps=1000
nobs=250
con.int=.95
####forward method using CI####
det_list<-replicate(n=reps,detnon_list(reps = reps,nobs = nobs,md1 =ge_md1,md2=ge_md2,testmd = mdconf,con.int = con.int),simplify = FALSE)

#check if the variable is non-invariant or not
non_all<-det_non(det_list = det_list,non_con =c(NA,FALSE,FALSE,FALSE,FALSE,FALSE))
mean(non_all)

####CI=.99####
con.int=.99
####forward method using CI####
det_list<-replicate(n=reps,detnon_list(reps = reps,nobs = nobs,md1 =ge_md1,md2=ge_md2,testmd = mdconf,con.int = con.int),simplify = FALSE)

#check if the variable is non-invariant or not
non_all<-det_non(det_list = det_list,non_con =c(NA,FALSE,FALSE,FALSE,FALSE,FALSE))
mean(non_all)


####n=500####
####CI=.95####
#generate population data
ge_md1<-'
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
ge_md2<-'
fac1=~0.7*x1+0.7*x2+0.7*x3+0.7*x4+0.7*x5+0.7*x6
x1+x2+x3+x4+x5+x6~1*1
fac1~~1.3*fac1
fac1~0.2*1
x1~~0.3*x1
x2~~0.3*x2
x3~~0.3*x3
x4~~0.3*x4
x5~~0.3*x5
x6~~0.3*x6
'
mdconf<-'
fac1=~c(v1,v1)*x1+x2+x3+x4+x5+x6
'

reps=1000
nobs=500
con.int=.95
####forward method using CI####
det_list<-replicate(n=reps,detnon_list(reps = reps,nobs = nobs,md1 =ge_md1,md2=ge_md2,testmd = mdconf,con.int = con.int),simplify = FALSE)

#check if the variable is non-invariant or not
non_all<-det_non(det_list = det_list,non_con =c(NA,FALSE,FALSE,FALSE,FALSE,FALSE))
mean(non_all)

####CI=.99####
con.int=.99
####forward method using CI####
det_list<-replicate(n=reps,detnon_list(reps = reps,nobs = nobs,md1 =ge_md1,md2=ge_md2,testmd = mdconf,con.int = con.int),simplify = FALSE)

#check if the variable is non-invariant or not
non_all<-det_non(det_list = det_list,non_con =c(NA,FALSE,FALSE,FALSE,FALSE,FALSE))
mean(non_all)


####n=1000####
####CI=.95####
#generate population data
ge_md1<-'
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
ge_md2<-'
fac1=~0.7*x1+0.7*x2+0.7*x3+0.7*x4+0.7*x5+0.7*x6
x1+x2+x3+x4+x5+x6~1*1
fac1~~1.3*fac1
fac1~0.2*1
x1~~0.3*x1
x2~~0.3*x2
x3~~0.3*x3
x4~~0.3*x4
x5~~0.3*x5
x6~~0.3*x6
'
mdconf<-'
fac1=~c(v1,v1)*x1+x2+x3+x4+x5+x6
'

reps=1000
nobs=1000
con.int=.95
####forward method using CI####
det_list<-replicate(n=reps,detnon_list(reps = reps,nobs = nobs,md1 =ge_md1,md2=ge_md2,testmd = mdconf,con.int = con.int),simplify = FALSE)

#check if the variable is non-invariant or not
non_all<-det_non(det_list = det_list,non_con =c(NA,FALSE,FALSE,FALSE,FALSE,FALSE))
mean(non_all)

####CI=.99####
con.int=.99
####forward method using CI####
det_list<-replicate(n=reps,detnon_list(reps = reps,nobs = nobs,md1 =ge_md1,md2=ge_md2,testmd = mdconf,con.int = con.int),simplify = FALSE)

#check if the variable is non-invariant or not
non_all<-det_non(det_list = det_list,non_con =c(NA,FALSE,FALSE,FALSE,FALSE,FALSE))
mean(non_all)
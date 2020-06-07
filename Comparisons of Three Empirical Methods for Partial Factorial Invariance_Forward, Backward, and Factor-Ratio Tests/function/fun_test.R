####test gen_lam####
ge_md1<-'
fac1=~0.7*x1+0.7*x2+0.7*x3+0.7*x4+0.7*x5+0.7*x6
x1+x2+x3+x4+x5+x6~1*1
fac1~~1*fac1
fac1~0.2*1
x1~~0.3*x1
x2~~0.3*x2
x3~~0.3*x3
x4~~0.3*x4
x5~~0.3*x5
x6~~0.3*x6
'
ge_md2<-'
fac1=~0.7*x1+0.5*x2+0.7*x3+0.5*x4+0.7*x5+0.7*x6
x1+x3+x5+x6~1*1
x2~0.8*1
x4~0.8*1
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

reps=50
nobs=250
con.int=.95
dta_list<-replicate(n=reps,gen_dta(nobs=nobs,md1=ge_md1,md2 =ge_md2),simplify = FALSE)

####forward method using CI####
lam_dta_list<-gen_lam(data=dta_list,model = mdconf)
lam_dta<-rbindlist(lam_dta_list)
ch_non<-check_non(data = lam_dta,con.int = con.int)

#check if the variable is non-invariant or not
det_list<-replicate(n=reps,detnon_list(reps = reps,nobs = nobs,md1 =ge_md1,md2=ge_md2,testmd=testmd,con.int),simplify = FALSE)
non_all<-det_non(det_list = det_list,non_con =c(NA,TRUE,FALSE,TRUE,FALSE,FALSE))
mean(non_all)

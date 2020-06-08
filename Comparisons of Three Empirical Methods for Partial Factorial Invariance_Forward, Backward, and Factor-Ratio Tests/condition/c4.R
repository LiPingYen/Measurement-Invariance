options(digits = 4)
##mixed-size difference

####PMI####
####n=250,CI=.95####
####generate population data####
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
fac1=~0.7*x1+0.4*x2+0.7*x3+0.2*x4+0.7*x5+0.7*x6
x1+x3+x5+x6~1*1
x2~0.7*1
x4~0.5*1
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
nobs=250
con.int=.95
####forward method using CI####
det_list<-replicate(n=reps,detnon_list(reps = reps,nobs = nobs,md1 =ge_md1,md2=ge_md2,testmd = mdconf,con.int = con.int),simplify = FALSE)

#check if the variable is non-invariant or not
non_all<-det_non(det_list = det_list,non_con =c(NA,TRUE,FALSE,TRUE,FALSE,FALSE))
mean(non_all)

####n=500####
####generate population data####
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
fac1=~0.7*x1+0.4*x2+0.7*x3+0.2*x4+0.7*x5+0.7*x6
x1+x3+x5+x6~1*1
x2~0.7*1
x4~0.5*1
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
non_all<-det_non(det_list = det_list,non_con =c(NA,TRUE,FALSE,TRUE,FALSE,FALSE))
mean(non_all)


####n=1000####
####generate population data####
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
fac1=~0.7*x1+0.4*x2+0.7*x3+0.2*x4+0.7*x5+0.7*x6
x1+x3+x5+x6~1*1
x2~0.7*1
x4~0.5*1
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
non_all<-det_non(det_list = det_list,non_con =c(NA,TRUE,FALSE,TRUE,FALSE,FALSE))
mean(non_all)
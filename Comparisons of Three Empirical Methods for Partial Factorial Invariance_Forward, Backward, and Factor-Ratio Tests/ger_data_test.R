library(lavaan)
library(mvtnorm)
library(dplyr)

####Generating data####
##用simulateData
#group1
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
dta<-simulateData(model =ge_md1,model.type = "cfa",return.type = "data.frame",sample.nobs = 250)


#用前一篇論文寫的code
gen_dta_test<-function(nobs,la1,phi1,th1){
  pop_co_ma1<-la1%*%phi1%*%t(la1)+th1
  dta<-data.frame(rmvnorm(nobs,rep(0,nrow(pop_co_ma1)),sigma = pop_co_ma1,method ="chol"))#rep(mean,)mean要帶公式,tau+lamda*latent mean
  dta<-dta%>%
    rename(x1=X1,x2=X2,x3=X3,x4=X4,x5=X5,x6=X6)
  dta
}

nobs=250
lambda1<-matrix(rep(0.7,6),nrow= 6)
phi1<-1
theta1<-diag(rep(0.3,6))
dta2<-data.frame(gen_dta_test(nobs = nobs,la1=lambda1,phi1=phi1,th1 = theta1))


#兩種方式產生資料比較

cov(dta)
cov(dta2)

library(lavaan)
library(mvtnorm)
library(dplyr)
library(parallel)
library(data.table)
library(compare)
library(future)

####generate data from multivariate normal distribution####
#use self-defined function(faster option)
gen_dta<-function(nobs,la1,phi1,th1,tau1,fac_mean1,la2,phi2,th2,tau2,fac_mean2){
  pop_co_ma1<-la1%*%phi1%*%t(la1)+th1
  pop_co_ma2<-la2%*%phi2%*%t(la2)+th2
  pop_mean1<-tau1+la1%*%fac_mean1
  pop_mean2<-tau2+la2%*%fac_mean2
  dta1<-data.frame(rmvnorm(nobs,mean = pop_mean1,sigma = pop_co_ma1,method ="chol"))#rep(mean,)mean要帶公式,tau+lamda*latent mean
  dta2<-data.frame(rmvnorm(nobs,mean = pop_mean2,sigma = pop_co_ma2,method ="chol"))
  dta<-rbind(dta1,dta2)%>%
    mutate(group=c(rep(1,nobs),rep(2,nobs)))
  dta
}

####create lambda dataframe from result of cfa####
gen_lam<-function(data,model){
  mclapply(data,function(x){
    fit<-cfa(data=x,model=model,group="group")
    lam_g1<-parameterEstimates(fit)[1:6,7]
    lam_g2<-parameterEstimates(fit)[21:26,7]
    data.frame(lambda_g1=lam_g1,lambda_g2=lam_g2,v=c("v1","v2","v3","v4","v5","v6"))
  },mc.cores =availableCores()-5 )
}

####check variable is non-invariant or not####
check_non<-function(data,con.int){
  v1_lam<-data%>%filter(.,v=="v1")
  v2_lam<-data%>%filter(.,v=="v2")
  v3_lam<-data%>%filter(.,v=="v3")
  v4_lam<-data%>%filter(.,v=="v4")
  v5_lam<-data%>%filter(.,v=="v5")
  v6_lam<-data%>%filter(.,v=="v6")
  ci1<-t.test(v1_lam$lambda_g1,v1_lam$lambda_g2,alternative = "two.sided",paired = TRUE,conf.level = con.int)$conf.int
  ci2<-t.test(v2_lam$lambda_g1,v2_lam$lambda_g2,alternative = "two.sided",paired = TRUE,conf.level = con.int)$conf.int
  ci3<-t.test(v3_lam$lambda_g1,v3_lam$lambda_g2,alternative = "two.sided",paired = TRUE,conf.level = con.int)$conf.int
  ci4<-t.test(v4_lam$lambda_g1,v4_lam$lambda_g2,alternative = "two.sided",paired = TRUE,conf.level = con.int)$conf.int
  ci5<-t.test(v5_lam$lambda_g1,v5_lam$lambda_g2,alternative = "two.sided",paired = TRUE,conf.level = con.int)$conf.int
  ci6<-t.test(v6_lam$lambda_g1,v6_lam$lambda_g2,alternative = "two.sided",paired = TRUE,conf.level = con.int)$conf.int
  dta_inv<-c(prod(ci1)>=0,prod(ci2)>=0,prod(ci3)>=0,prod(ci4)>=0,prod(ci5)>=0,prod(ci6)>=0)
  dta_inv
}

####perfect recovery rate:completely detects non-invariant variable####
#combine generating data and checking non-invariant variable together
detnon_list<-function(reps,nobs,la1,la2,phi1,phi2,th1,th2,tau1,tau2,fac_mean1,fac_mean2,testmd,con.int){
  dta_list<-replicate(n=reps,gen_dta(nobs=nobs,la1 = lambda1,phi1 = phi1,th1 = theta1,tau1 = tau1,fac_mean1 = fac_mean1,
                                     la2 = lambda2,phi2 = phi2,th2 = theta2,tau2 = tau2,fac_mean2 = fac_mean2),simplify =FALSE )
  lam_dta_list<-gen_lam(data=dta_list,model = testmd)
  lam_dta<-rbindlist(lam_dta_list)
  ch_non<-check_non(data = lam_dta,con.int = con.int)
  ch_non
}
#non_con: non-invariant variable enter TRUE, NA enter NA, invariant enter FALSE 
det_non<-function(det_list,non_con){
  sapply(det_list,function(x){
    non_inv<-vector()
    ifelse(compare(x,non_con)$result,non_inv<-1,non_inv<-0)
    non_inv})
}
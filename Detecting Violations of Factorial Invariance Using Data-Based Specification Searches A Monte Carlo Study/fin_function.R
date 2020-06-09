library(dplyr)
library(MASS)
library(mvtnorm)
library(lavaan)
library(semTools)
library(simsem)
library(stringr)
library(reshape2)
library(compare)
library(parallel)
library(future)

####產生population資料####
gen_dta_v6<-function(nobs,la1,phi1,la2,phi2,th2){
  pop_co_ma1<-la1%*%phi1%*%t(la1)+th2
  pop_co_ma2<-la2%*%phi2%*%t(la2)+th2
  dta_1<-data.frame(rmvnorm(nobs,rep(0,nrow(pop_co_ma1)),sigma = pop_co_ma1,method ="chol"))
  dta_2<-data.frame(rmvnorm(nobs,rep(0,nrow(pop_co_ma2)),sigma = pop_co_ma2,method ="chol"))
  dta<-rbind(dta_1,dta_2)%>%
    rename(v1=X1,v2=X2,v3=X3,v4=X4,v5=X5,v6=X6)%>%
    mutate(group=c(rep(1,nobs),rep(2,nobs)))
  dta
}
gen_dta_v12<-function(nobs,la1,phi1,la2,phi2,th2){
  pop_co_ma1<-la1%*%phi1%*%t(la1)+th2
  pop_co_ma2<-la2%*%phi2%*%t(la2)+th2
  dta_1<-data.frame(rmvnorm(nobs,rep(0,nrow(pop_co_ma1)),sigma = pop_co_ma1,method ="chol"))
  dta_2<-data.frame(rmvnorm(nobs,rep(0,nrow(pop_co_ma2)),sigma = pop_co_ma2,method ="chol"))
  dta<-rbind(dta_1,dta_2)%>%
    rename(v1=X1,v2=X2,v3=X3,v4=X4,v5=X5,v6=X6,v7=X7,v8=X8,v9=X9,v10=X10,v11=X11,v12=X12)%>%
    mutate(group=c(rep(1,nobs),rep(2,nobs)))
  dta
}

####產生所有資料的rmsea####
#cpu.cores <- availableCores()
#cl <- makeCluster(cpu.cores,type = "FORK")
rmfun_papl<-function(md,dta){
  parSapply(cl=cl,dta,function(x){
    fit<-cfa(model = md,data = x,group = "group",group.equal=c("loadings"))
    fitMeasures(fit,"rmsea")})
}
#stopCluster(cl)
#showConnections() 

####產出noninvariant的變項，使用平行分析####
non_var_papl<-function(md,dta){
  mclapply(dta,function(x){
    fit<-cfa(model=md,data=x,group="group",group.equal=c("loadings"))
    lavp<-lavTestScore(fit,cumulative = TRUE)$cumulative[1,]
    fre_va<-vector()
    non_int_each<-vector()
    n<-1
    while(lavp[,6]<.05){
      non_int_each[n]<-lavp$lhs
      v<-str_extract_all(lavp$lhs, "(\\d)+")[[1]]
      fre_va[n]<-paste0("fac1=~v",v)
      fit_i<-cfa(model=md,data=x,group="group",group.equal=c("loadings"),group.partial=fre_va)
      lavp<-lavTestScore(fit_i,cumulative = TRUE)$cumulative[1,]
      n=n+1
    }
    non_int_each
  },mc.cores =availableCores()-5)
}

#偵測模型中noninvariant variable 和資料中的有沒有相符,perfect recovery rate
det_non_pl<-function(non_var,non_con){
  sapply(non_var,function(x){
    non_inv<-vector()
    ifelse(compare(x,non_con,ignoreOrder = TRUE)$result,non_inv<-1,non_inv<-0)#解決ignorOrder對於c()沒有用
    non_inv})
}  

####true detection####
true_det_pl<-function(non_var,non_con){
  a<-mclapply(non_var,function(x){
    x%in%non_con
  },mc.cores =availableCores()-5)
  b<-mclapply(a,function(x){
    table(x)
  },mc.cores =availableCores()-5)
  names(b)<-NULL
  c<-melt(b)
  c$L1 <- factor(c$L1, seq_along(b))
  d<-dcast(c, L1 ~ x, fill = 0, drop = FALSE)
  colnames(d)<-c("L1","F","T")
  count(d,F,T)%>%
    mutate(freq=n/reps)
}

#false detection
fal_det_pl<-function(non_var,inv_con){
  a<-mclapply(non_var,function(x){
    x%in%inv_con
  },mc.cores =availableCores()-5)
  b<-mclapply(a,function(x){
    table(x)
  },mc.cores =availableCores()-5)
  names(b)<-NULL
  c<-melt(b)
  c$L1 <- factor(c$L1, seq_along(b))
  d<-dcast(c, L1 ~ x, fill = 0, drop = FALSE)
  colnames(d)<-c("L1","F","T")
  count(d,F,T)%>%
    mutate(freq=n/reps)
}
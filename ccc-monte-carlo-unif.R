#unif
library(MASS)
set.seed(100)

m=5000
n=10

g_1<-numeric(m)
g_2<-numeric(m)

for(i in 1:m){
  bivariate_data <- as.data.frame(
    mvrnorm(n=10,
            mu=c(0, 0),
            Sigma=matrix(c(1, 0.95, 0.95, 1), ncol=2)))
  bivariate_data <- data.frame(V1 = pnorm(bivariate_data$V1), V2 = pnorm(bivariate_data$V1))

  y1=c(bivariate_data$V1)
  mean1=mean(y1)
  var1=var(y1)
  std1=sqrt(var1)
  
  y2=c(bivariate_data$V2)
  mean2=mean(y2)
  var2=var(y2)
  std2=sqrt(var2)
  
  covar=cov(y1,y2)
  
  g_1[i] <- 2*covar/(var1+var2+(mean1-mean2)^2)
  rho=covar/(std1*std2)
  rhoc=(2*covar)/(var1+var2+(mean1-mean2)^2)
  u=(mean1-mean2)/sqrt(std1*std2)
  g_2[i] <- sqrt(1/(n-2)*(((1-rho^2)*rhoc^2*(1-rhoc^2))/rho^2+(4*rhoc^3*(1-rhoc)*u^2)/rho-(2*rhoc^4*u^4)/rho^2))
}
#g_1
#g_2
# mean of data
esti_mean1<-mean(g_1)
esti_mean1


est_var=var(g_1)
est_var

# standard error of data
est_std=sqrt(est_var)
est_std

# asymptotic standard error
esti_mean2<-mean(g_2)
esti_mean2
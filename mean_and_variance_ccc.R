#library(JAGS)
library(rjags)
library(agRee)
data(judgeRatings)
agree.ccc(judgeRatings[,2:3])

#case1
library(MASS)
m=5000
n=10
g_1<-numeric(m)
g_2<-numeric(m)
for(i in 1:m){
bivariate_data <- as.data.frame(mvrnorm(n=10,
                                        mu=c(0, 0),
                                        Sigma=matrix(c(1, 0.95, 0.95, 1), ncol=2)))

y1=c(bivariate_data$V1)
mean1=mean(y1)
var1=var(y1)
y2=c(bivariate_data$V2)
mean2=mean(y2)
var2=var(y2)
var1=var(y1)
covar=cov(y1,y2)
g_1[i] <- 2*covar/(var1+var2+(mean1-mean2)^2)
rho=covar/var1*var2
rhoc=2*covar/(var1+var2+(mean1-mean2)^2)
u=(mean1-mean2)/sqrt(var1*var2)
g_2[i] <- 1/(n-2)*(((1-rho^2)*rhoc^2*(1-rhoc^2))/rho^2+(4*rhoc^3*(1-rhoc)*u^2)/rho-(2*rhoc^4*u^4)/rho^2)
}
g_1
g_2
esti_mean1<-mean(g_1)
esti_mean1

es_de=sd(g_1)
es_de
esti_mean2<-mean(g_2)
esti_mean2
#esti_var<-var(g)
#esti_var

esti_variance<-sum(g_1-mean(g_1))^2/n
esti_variance

esti_std=sqrt(esti_var)
esti_std

esti_err=esti_std/sqrt(n)
esti_err


esti_error<-mean(sqrt(sum((g - mean(g))^2)) / n)
esti_error

esti_error1=1/sqrt(n)*sqrt((1/(n-1)*(sum(g-mean(g))^2))
esti_error1



m <- 1000
g <- numeric(m)
for (i in 1:m) {
  x <- rnorm(2)
  g[i] <- abs(x[1] - x[2])
}
est <- mean(g)

x <- rnorm(2)
x


#bootstrap
library(bootstrap) 
bivariate_data <- as.data.frame(mvrnorm(n=10,
                                        mu=c(0, 0),
                                        Sigma=matrix(c(1, 0.95, 0.95, 1), ncol=2)))

print(cor(law$LSAT, law$GPA))
[1] 0.7763745
print(cor(law82$LSAT, law82$GPA))
[1] 0.7599979

library(bootstrap) 
last1=c(law$LSAT)
last1
last2=(law82$LSAT )
last2



#case2
library(MASS)
bivariate_data <- as.data.frame(mvrnorm(n=100,
                                        mu=c(-1*sqrt(0.1)/2,1*sqrt(0.1)/2 ),
                                        Sigma=matrix(c(1, 0.95, 0.95, 1), ncol=2)))
head(bivariate_data)

#case3
library(MASS)
bivariate_data <- as.data.frame(mvrnorm(n=100,
                                        mu=c(-1*sqrt(0.1)/2,1*sqrt(0.1)/2),
                                        Sigma=matrix(c(1.1^2, 0.95*1.1*0.9, 0.95*1.1*0.9, 0.9*0.9), ncol=2)))
head(bivariate_data)

#case4
library(MASS)
bivariate_data <- as.data.frame(mvrnorm(n=100,
                                        mu=c(-1*sqrt(0.1)/2,1*sqrt(0.1)/2),
                                        Sigma=matrix(c(0.9*0.9, 0.8*0.9*1.1, 0.8*0.9*1.1, 1.1*1.1), ncol=2)))
head(bivariate_data)

#case5
library(MASS)
bivariate_data <- as.data.frame(mvrnorm(n=100,
                                        mu=c(-1*sqrt(0.25)/2,1*sqrt(0.25)/2),
                                        Sigma=matrix(c((4/3)*(4/3), 0.5*(4/3)*(2/3), 0.5*(4/3)*(2/3), (2/3)*(2/3)), ncol=2)))
head(bivariate_data)

#bootstrap
set.seed(10)
library(MASS)
library(boot)
m=10
n=10
g<-numeric(m)
for(i in 1:m){
  bivariate_data <- as.data.frame(mvrnorm(n=10,
                                          mu=c(0, 0),
                                          Sigma=matrix(c(1, 0.95, 0.95, 1), ncol=2)))
  
  y1=c(bivariate_data$V1)
  mean1=mean(y1)
  var1=var(y1)
  y2=c(bivariate_data$V2)
  mean2=mean(y2)
  var2=var(y2)
  covar=cov(y1,y2)
  g[i] <- 2*covar/(var1+var2+(mean1-mean2)^2)
}
x=c(g)
x
xlength=length(x)
xlength
standard_error=mean(replicate(5000, sd(sample(x, replace=T))/sqrt(length(x))))
standard_error

x <- c(12, 14, 14, 15, 18, 21, 25, 29, 32, 35)
x


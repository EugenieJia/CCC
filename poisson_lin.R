#case1
library(MASS)
set.seed(334)

m=5000
n=50

g_1<-numeric(m)
g_2<-numeric(m)
z_1<-numeric(m)
var_z<-numeric(m)
std_z<-numeric(m)
rhoboot <- numeric(m)

for(i in 1:m){
  #case1 lambda1=lambda2=0.45, lambda3=8.55
  #case2 lambda1=lambda2=1.017, lambda3=7.983
  #case3 lambda1=lambda2=5.76, lambda3=3.24
  X1 <- rpois(n, 5.76)
  X2 <- rpois(n, 5.76)
  X3 <- rpois(n, 3.24)
  X <- X1 + X3
  Y <- X2 + X3
  #condition 1

  #Standardization
  Stand_X=(X-9)/3
  Stand_Y=(Y-9)/3
  
  y1=c(Stand_X)
  mean1=mean(y1)
  var1=var(y1)
  std1=sqrt(var1)
  
  y2=c(Stand_Y)
  mean2=mean(y2)
  var2=var(y2)
  std2=sqrt(var2)
  
  covar=cov(y1,y2)
  
  g_1[i] <- 2*covar/(var1+var2+(mean1-mean2)^2)
  z_1[i] <- (1/2) * log((1 + g_1[i]) / (1 - g_1[i]))
  rho=covar/(std1*std2)
  rhoc=(2*covar)/(var1+var2+(mean1-mean2)^2)
  u=(mean1-mean2)/sqrt(std1*std2)
  g_2[i] <- sqrt(1/(n-2)*(((1-rho^2)*rhoc^2*(1-rhoc^2))/rho^2+(4*rhoc^3*(1-rhoc)*u^2)/rho-(2*rhoc^4*u^4)/rho^2))
  var_z[i] <- sqrt(1 / (n - 2) * (((1 - rho^2) * rhoc^2 / ((1 - rhoc^2)) * rho^2) + (4 * rhoc^3 * (1 - rhoc) * u^2) / (rho * (1 - rhoc^2)^2) - (2 * rhoc^4 * u^4) / (rho^2 * (1 - rhoc^2)^2)))
  std_z[i] <-sqrt((g_2[i])^2/(1-rhoc^2)^2)
}
#g_1
#g_2
# mean of data
#y1
#y2
#g_1
#g_2
#var_z
#(idx <- which(rhoc == 1))
#length(idx)
#mean(std_z)
#mean(var_z)

#rhoc
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


#(idx <- which(g_1 == 1))
#length(idx)
#mean(z_1[-idx])

#z_1 <- z_1[-idx]

# mean of data_z
esti_mean1_z<-mean(z_1)
esti_mean1_z


#(idx <- which(g_1 == 1))
#length(idx)
#var(z_1[-idx])

#z_1 <- z_1[-idx]

est_var_z=var(z_1)
est_var_z

# standard error of data_z
est_std_z=sqrt(est_var_z)
est_std_z


# asymptotic standard error_z
esti_mean2_z<-mean(var_z)
esti_mean2_z



#Normality
sample1=c(g_1)
ks_result <- ks.test(sample1, "pnorm", mean(sample1), sd(sample1))
print(ks_result)

#Normality_z
#(idx <- which(g_1 == 1))
#length(idx)
#mean(z_1[-idx])
#z_1 <- z_1[-idx]
sample2=c(z_1)
ks_result <- ks.test(sample2, "pnorm", mean(sample2), sd(sample2))
print(ks_result)


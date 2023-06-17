#unif
library(MASS)
set.seed(100)


dist_gen <- function(n,dist){
  bivariate_data <- as.data.frame(
    mvrnorm(n=n,
            mu=c(0, 0),
            Sigma=matrix(c(1, 0.95, 0.95, 1), ncol=2)))
  
  if (dist != "norm"){
    bivariate_data <- data.frame(V1 = pnorm(bivariate_data$V1), V2 = pnorm(bivariate_data$V1))
    
    if (dist == "pois"){
      bivariate_data <- data.frame(V1 = qpois(bivariate_data$V1, 9)/3 - 3, V2 = qpois(bivariate_data$V1, 9)/3 - 3)
    }
  }
  bivariate_data
}


infer <- function(value,m,n,dist){
  if(!dist %in% c("norm","unif","pois")) {
        return()
  }

  f_gen <- function(n){dist_gen(n,dist)}
  g_1<-numeric(m)
  g_2<-numeric(m)

  for(i in 1:m){
    
    bivariate_data<-f_gen(n)
    
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
  
  if(val == "real"){
    return(g_1)
  }
  if(val == "infer"){
    return(g_2)
  }
}


m <- 5000
n <- 10
dist <- "norm"

val <- "real"
x <- infer(val,m,n,dist)
mean(x)
sd(x)

val <- "infer"
x <- infer(val,n,dist)
mean(x)




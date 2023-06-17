# initializing
library(MASS)
set.seed(100)

# the function that selects what random variable to generate 
dist_gen <- function(n,dist,covar_g){
  # only accept supported distributions
  if(!dist %in% c("norm","unif","pois")) {
        return()
  }
  
  # generate the correlated MVN
  bivariate_data <- as.data.frame(
    mvrnorm(n=n,
            mu=c(0, 0),
            Sigma=covar_g))
  
  # transform normal to uniform
  if (dist != "norm"){
    bivariate_data <- data.frame(V1 = pnorm(bivariate_data$V1), V2 = pnorm(bivariate_data$V1))
    
    # transform uniform to poisson, normalized
    if (dist == "pois"){
      bivariate_data <- data.frame(
        V1 = qpois(bivariate_data$V1, 9)/3 - 3, 
        V2 = qpois(bivariate_data$V1, 9)/3 - 3)
    }
  }
  # output data
  bivariate_data
}

# ccc estimating function
infer <- function(m,n,dist,covar_g){
  # select distribution
  f_gen <- function(n){dist_gen(n,dist,covar_g)}
  

  # initialize storage of pc and the asymptotic std
  g_1<-numeric(m)
  g_2<-numeric(m)
  
  # make all runs
  for(i in 1:m){
    
    # generate data
    bivariate_data<-f_gen(n)
    
    # basic statistics
    y1=c(bivariate_data$V1)
    mean1=mean(y1)
    var1=var(y1)
    std1=sqrt(var1)
    
    y2=c(bivariate_data$V2)
    mean2=mean(y2)
    var2=var(y2)
    std2=sqrt(var2)
    
    covar=cov(y1,y2)
    
    # calculations
    g_1[i] <- 2*covar/(var1+var2+(mean1-mean2)^2)
    rho=covar/(std1*std2)
    rhoc=(2*covar)/(var1+var2+(mean1-mean2)^2)
    u=(mean1-mean2)/sqrt(std1*std2)
    g_2[i] <- sqrt(1/(n-2)*(((1-rho^2)*rhoc^2*(1-rhoc^2))/rho^2+(4*rhoc^3*(1-rhoc)*u^2)/rho-(2*rhoc^4*u^4)/rho^2))
    
  }
  data.frame(pc = g_1, est = g_2)
}

# set parameters
m <- 5000
n <- 10
dist <- "norm"
covar_g <- matrix(c(1, 0.95, 0.95, 1),ncol=2)

# collect data
dat <- infer(m,n,dist,covar_g)
  pc <- dat$pc
  mean(pc)
  sd(pc)
  
  z <- atanh(pc)
  mean(z)
  sd(z)
  
  Sp <- dat$est
  mean(Sp)
  Sz <- (1/(1-pc^2))*(Sp)
  mean(Sz)

x1 <- c(mean(pc),mean(Sp),mean(z),mean(Sz))
y1 <- c(sd(pc),NA,sd(z),NA)

# normality test
u <- ks.test(pc,"pnorm",mean(pc),sd(pc))

v <- ks.test(z,"pnorm",mean(z),sd(z))


j1 <- c(u$statistic,NA,v$statistic,NA)
k1  <- c(u$p.value,NA,v$p.value,NA)

# produce table
data.frame(mean = x1, std = y1, D = j1, p = k1)






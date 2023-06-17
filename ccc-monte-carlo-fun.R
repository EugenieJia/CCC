# terminal command cd /c/temp/stat/project

# initializing
library(MASS)
mean_g <- c(0,0)
##### Setup #####
{

  # function shortcuts ####
  {
  # asymptotic standard deviation
  ccc_asy_sd <- function(n, rho, u, rhoc){
    1/(n-2)*(((1-rho^2)*rhoc^2*(1-rhoc^2))/rho^2 +
            (4*rhoc^3*(1-rhoc)*u^2)/rho -
            (2*rhoc^4*u^4)/rho^2)
    }
  # location shift
  ccc_shift_loc <- function(mean1,mean2,std1,std2){
    (mean1-mean2)/sqrt(std1*std2)
    }
  # ccc
  ccc_calc <- function(mean1,mean2,var1,var2,covar){
    (2*covar)/(var1+var2+(mean1-mean2)^2)
  }
  # linear correlation
  corr <- function(std1,std2,covar){
    covar/(std1*std2)
  }
  }
  # function selecting a random variable ####
  dist_gen <- function(n,dist,mean_g,covar_g){
    # only accept supported distributions
    if(!dist %in% c("norm","unif","pois")) {
          return()
    }
    
    # generate the correlated MVN
    bivariate_data <- as.data.frame(
      mvrnorm(n=n,
              mu=mean_g,
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

  # ccc estimating function ####
  infer <- function(m,n,dist,mean_g,covar_g){
    # select distribution
    f_gen <- function(n){dist_gen(n,dist,mean_g,covar_g)}
    
  
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
      rhoc=ccc_calc(mean1,mean2,var1,var2,covar)
      g_1[i] <- rhoc
      rho=corr(std1,std2,covar)
      u=ccc_shift_loc(mean1,mean2,std1,std2)
      g_2[i] <- sqrt(ccc_asy_sd(n, rho, u, rhoc))
      
    }
    data.frame(pc = g_1, est = g_2)
  }

  # analysis function for table ####
  insta_table <- function(dat){
    pc <- dat$pc
    z <- atanh(pc)
    Sp <- dat$est
    Sz <- (1/(1-pc^2))*(Sp)
    
    x1 <- c(mean(pc),mean(Sp),mean(z),mean(Sz))
    y1 <- c(sd(pc),NA,sd(z),NA)
    
    # normality test
    u <- ks.test(pc,"pnorm",mean(pc),sd(pc))
    
    v <- ks.test(z,"pnorm",mean(z),sd(z))
    
    
    j1 <- c(u$statistic,NA,v$statistic,NA)
    k1  <- c(u$p.value,NA,v$p.value,NA)
    
    # produce table
    data.frame(val = c('pc', 's_pc', 'z', 's_z'), mean = x1, std = y1, D = j1, p = k1)
  }
}









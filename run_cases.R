source("ccc-monte-carlo-fun.R")

library(tidyverse)
library(MASS)


##### Simulation #####

# create analysis function

insta_sim <- function(m, dist, mean_g, covar_g){
  m <- m
  dist <- dist
  mean_g <- mean_g
  covar_g <- covar_g
  
# collect data
n <- 10
dat <- infer(m,n,dist,mean_g,covar_g)
n10 <- insta_table(dat)

n <- 20
dat <- infer(m,n,dist,mean_g,covar_g)
n20 <- insta_table(dat)

n <- 50
dat <- infer(m,n,dist,mean_g,covar_g)
n50 <- insta_table(dat)

# create table
list <- lst(n10,n20,n50)

f <- list %>%
  imap(function(x, y) x %>% 
  rename_with(~paste(., y, sep = '_'), -val)) %>%
  reduce(full_join, by = 'val')
f
}

# create data generator for table
lin_run <- function(dist, sel){
  
  m <- 5000
  sel <- sel
  # means
  m1 <- -sqrt(.1)/2
  m2 <- -sqrt(.25)/2
  
  mu0 <- c(0,0)
  mu1 <- c(-m1,m1)
  mu2 <- c(-m2,m2)
  
  # covariance matrices
  mat_gen <- function(a,b,sca){
    matrix(c(a^2, sca*a*b, sca*a*b, b^2),ncol=2)
  }
  
  mat1 <- mat_gen(1,1,0.95)
  mat2 <- mat_gen(1.1,0.9,0.95)
  mat3 <- mat_gen(0.9,1.1,0.8)
  mat4 <- mat_gen(4/3,2/3,0.5)
  
  if(1 %in% sel){
    mean_g <- mu0
    covar_g <- mat1
    case_1 <- insta_sim(m,dist,mean,covar_g)
  }
  if(2 %in% sel){
    mean_g <- mu1
    covar_g <- mat1
    case_2 <- insta_sim(m,dist,mean,covar_g)
  }
  if(3 %in% sel){
    mean_g <- mu1
    covar_g <- mat2
    case_3 <- insta_sim(m,dist,mean,covar_g)
  }
  if(4 %in% sel){
    mean_g <- mu1
    covar_g <- mat3
    case_4 <- insta_sim(m,dist,mean,covar_g)
  }
  if(5 %in% sel){
    mean_g <- mu2
    covar_g <- mat4
    case_5 <- insta_sim(m,dist,mean,covar_g)
  }
}

source("ccc-monte-carlo-fun.R")

library(tidyverse)
library(MASS)


##### Simulation #####

# set parameters
set.seed(100)

m <- 5000
dist <- "norm"
covar_g <- matrix(c(1, 0.95, 0.95, 1),ncol=2)

# collect data
n <- 10
dat <- infer(m,n,dist,covar_g)
n10 <- insta_table(dat)

n <- 20
dat <- infer(m,n,dist,covar_g)
n20 <- insta_table(dat)

n <- 50
dat <- infer(m,n,dist,covar_g)
n50 <- insta_table(dat)

# create table
list <- lst(n10,n20,n50)

f <- list %>%
  imap(function(x, y) x %>% 
  rename_with(~paste(., y, sep = '_'), -val)) %>%
  reduce(full_join, by = 'val')


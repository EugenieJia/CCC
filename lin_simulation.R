# dependencies
source("ccc-monte-carlo-fun.R")
source("run_cases.R")

# libraries
library(MASS)
library(tidyverse)

# set seed
set.seed(100)

# generate data
lin_run("norm",1:5)

lin_run("unif",c(1,3,5))

lin_run("norm",c(1,3,5))

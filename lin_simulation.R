# dependencies
source("ccc-monte-carlo-fun.R")
source("run_cases.R")

# libraries
library(MASS)
library(tidyverse)

# set seed
set.seed(100)

# generate data and save to .csv
# to do: create function that produces file
dist <-'norm'
lin_run("norm",1:2)
full_tb <- mget(ls(pattern ="case_\\d+")) %>% map_df(I, .id = "src") %>% add_column(.before = "src", "dist" = dist)
write_csv(full_tb, paste(dist,"ccc","sim.csv", sep = "_"))
rm(list = ls(pattern ="case_\\d+")) # clear data

dist <- 'unif'
lin_run("unif",c(1,3,5))
full_tb <- mget(ls(pattern ="case_\\d+")) %>% map_df(I, .id = "src") %>% add_column(.before = "src", "dist" = dist)
write_csv(full_tb, paste(dist,"ccc","sim.csv", sep = "_"))
rm(list = ls(pattern ="case_\\d+"))

dist <- 'pois'
lin_run("pois",c(1,3,5))
full_tb <- mget(ls(pattern ="case_\\d+")) %>% map_df(I, .id = "src") %>% add_column(.before = "src", "dist" = dist)
write_csv(full_tb, paste(dist,"ccc","sim.csv", sep = "_"))
rm(list = ls(pattern ="case_\\d+"))


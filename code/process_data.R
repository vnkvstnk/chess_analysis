# Load packages
source("./code/moves_breakdown.R")
require(dplyr)

# Reading in the games data
rds_file <- "./data/2018-05.1.rds"
games <- readRDS(rds_file)


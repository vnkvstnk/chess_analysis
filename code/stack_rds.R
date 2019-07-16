require(dplyr)
path = "./data/processed"
files <- list.files(path = path, pattern = "\\.rds$", full.names = TRUE)
r <- lapply(files, readRDS) %>% bind_rows()

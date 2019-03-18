source("./code/functions.R")
require(stringr)

for (file in dir("./data/raw")) {
    filename <- str_split(file, "\\.pgn$")[[1]][1]
    rds_name <- paste("./data/processed/", filename, ".rds", sep = "")
    pgn_name <- paste("./data/raw/", file, sep = "")
    if (file.exists(rds_name)) {
        cat("File ", rds_name, " has already been processed. Moving forward\n")
        next
    }
    cat("Processing ", pgn_name, "\n")
    x <- import_pgn(pgn_name)
    saveRDS(x, rds_name)
    cat(rds_name, " has been saved.\n")
}
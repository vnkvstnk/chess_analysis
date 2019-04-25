# Let's try and gather hearmap data for some number of rds files, say 10
source("./code/pieces_heatmap.R")

files <- dir("./data/processed/")
path <- "./data/processed/"



for (i in 11:20) {
    file <- paste(path, files[i], sep = "")
    games <- readRDS(file)
    movetext <- paste(games$Whitemoves, collapse = " ")
    heat <- piece_heatmap(movetext)
    heat_data <- heat_data + heat
    n_games <- n_games + nrow(games)
    cat("File", i, "processed.\n")
}
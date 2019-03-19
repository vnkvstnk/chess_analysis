# Load packages
library(bigchess)
library(dplyr)
library(tidyr)
source("./code/functions.R")
source("./code/popular_squares.R")

# Importing games data
rds_file <- "./data/processed/2018-05.1.rds"
games <- readRDS(rds_file)


# Getting popular squares
squares <- list(pawn_squares=NULL, queen_squares=NULL, king_squares=NULL,
                           bishop_squares=NULL, knight_squares=NULL, rook_squares=NULL)

for (i in 1:nrow(games)) {
    game_squares <- popular_squares(games$Movetext[i])
    for (field in names(squares)) {
        squares[[field]] <- update_table(squares[[field]], game_squares[[field]])
    }
    if (i %% 1000 == 0) print(i)
}

# Transforming to board matrices
squares <- lapply(squares, board_matrix)

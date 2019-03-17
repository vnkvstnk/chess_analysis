# Load packages
library(bigchess)
library(dplyr)
library(tidyr)
source("./code/functions.R")

# Load pgn files (for now, I will use only one file with 97339 games)
rds_file <- "./data/processed/2018-05.4.rds"
pgn_file <- "./data/raw/2018-05.4.pgn"

if (!file.exists(rds_file)) {
    games <- read.pgn(pgn_file,
                      n.moves = TRUE,
                      extract.moves = FALSE, 
                      stat.moves = FALSE,
                      add.tags = c("WhiteElo", "BlackElo","ECO", "WhiteTitle", "BlackTitle",
                                   "TimeControl", "Termination")) %>%
        as_tibble() %>%
        mutate_at(c("Termination", "ECO", "WhiteTitle", "BlackTitle"),
                  list(factor)) %>%
        mutate(Date = as.Date(Date, "%Y.%m.%d"),
               Tournament = grepl("tournament",
                                  Event,
                                  ignore.case = TRUE),
               Type = factor(sapply(strsplit(Event, " "), function(x) x[2]))) %>%
        separate(col = TimeControl,
                 into = c("BaseTime", "Increment"),
                 convert = TRUE) %>%
        select(-c(Round, Event)) %>%
        filter(NMoves >= 3)
    saveRDS(games, rds_file)
} else {
    games <- readRDS(rds_file)
}

# Now let's try to get all the squares from all the games

squares <- list(pawn_squares=NULL, queen_squares=NULL, king_squares=NULL,
                           bishop_squares=NULL, knight_squares=NULL, rook_squares=NULL)
# profvis::profvis({
for (i in 1:nrow(games)) {
    # if (i == 36931) next
    game_squares <- popular_squares(games$Movetext[i])
    for (field in names(squares)) {
        squares[[field]] <- update_table(squares[[field]], game_squares[[field]])
    }
    if (i %% 1000 == 0) print(i)
}
# })
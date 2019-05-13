source("./code/functions.R")
source("./code/moves_breakdown.R")
require(bigchess)
require(dplyr)
require(tidyr)


import_pgn <- function (filename,
                        tags = c("WhiteElo", "BlackElo", "ECO", "Opening", "WhiteTitle",
                                 "BlackTitle", "TimeControl", "Termination",
                                 "WhiteRatingDiff", "BlackRatingDiff", "UTCTime")) {
    
    # Usung read.pgn function of "bigchess" package to read in a pgn file
    games <- read.pgn(filename,
                      n.moves = TRUE,
                      extract.moves = FALSE, 
                      stat.moves = FALSE,
                      add.tags = tags) %>%
        
        as_tibble() %>%
        
        # Converting the following columns to factors
        mutate_at(c("Termination", "ECO", "WhiteTitle", "BlackTitle", "Opening"),
                  list(factor)) %>%
        
        # Converting Date column to Date data type
         mutate(Date = paste(Date, UTCTime) %>% as.POSIXct(format = "%Y.%m.%d %H:%M:%S", tz = "UTC"),
               
        # If a game was played in a tournament or not       
               Tournament = grepl("tournament",
                                  Event,
                                  ignore.case = TRUE),
        
        # Type of the game (rapid, blitz, etc)
               Type = factor(sapply(strsplit(Event, " "),
                                    function(x) x[2])),
        
        # Character vector of white's moves in order
               Whitemoves = sapply(Movetext,
                                   break_moves, side = "white"),
        
        # Character vector of black's moves in order
               Blackmoves = sapply(Movetext,
                                   break_moves, side = "black"),
        
        # White's rating change after the game
               WhiteRatingDiff = as.integer(WhiteRatingDiff),
        
        # Black's rating change after the game
               BlackRatingDiff = as.integer(BlackRatingDiff)) %>%
        
        # Creating two columns: BaseTime and Increment from TimeControl
        separate(col = TimeControl,
                 into = c("BaseTime", "Increment"),
                 convert = TRUE) %>%
        
        # Removing the following columns
        select(-c(Round, Event, Movetext, UTCTime)) %>%
        
        # Removing games with less than three moves
        filter(NMoves > 3)
    
    games
    
}
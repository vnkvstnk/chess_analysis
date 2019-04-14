source("./code/functions.R")
source("./code/moves_breakdown.R")
require(bigchess)
require(dplyr)
require(tidyr)


import_pgn <- function (filename,
                        tags = c("WhiteElo", "BlackElo", "ECO", "Opening", "WhiteTitle",
                                 "BlackTitle", "TimeControl", "Termination")) {
    games <- read.pgn(filename,
                      n.moves = TRUE,
                      extract.moves = FALSE, 
                      stat.moves = FALSE,
                      add.tags = tags) %>%
        as_tibble() %>%
        mutate_at(c("Termination", "ECO", "WhiteTitle", "BlackTitle", "Opening"),
                  list(factor)) %>%
        mutate(Date = as.Date(Date, "%Y.%m.%d"),
               Tournament = grepl("tournament",
                                  Event,
                                  ignore.case = TRUE),
               Type = factor(sapply(strsplit(Event, " "),
                                    function(x) x[2]))) %>%
        separate(col = TimeControl,
                 into = c("BaseTime", "Increment"),
                 convert = TRUE) %>%
        select(-c(Round, Event)) %>%
        filter(NMoves >= 3)
    games
}
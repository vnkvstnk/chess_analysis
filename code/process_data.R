# Load packages
library(bigchess)
library(dplyr)
library(tidyr)
source("./code/functions.R")

# Load pgn files (for now, I will use only one file with 97339 games)
games <- read.pgn("./data/raw/2018-05.1.pgn",
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


pieces <- c("p", "k", "q", "b", "n", "r")
popularity <- vector("list", length(pieces)) %>% setNames(pieces)
for (i in names(popularity)) popularity[[i]] <- matrix(0, 8, 8)

subset <- games[1:300, ]

for (i in 1:nrow(subset)) {
    output <- popular_squares(subset[i, ]$Movetext)
    popularity <- Map("+", popularity, output)
}

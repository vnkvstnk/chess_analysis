# Let's represent a chessboard as an 8-by-8 matrix with columns being files (A-H equals 1-8)
# and rows being ranks (1-8). We'll be looking at the board from white side, which means that
# first row of the matrix will correspond to the 8th rank and the 8th row of the matrix
# will correspond to the first rank.

# Help functions

# Converts square to numbers. "e4" - c(4, 5), a8 - c(8, 1)
square_to_index <- function (square) {
    square <- strsplit(square, "")[[1]]
    j <- which(letters == square[1])   # File
    i <- as.integer(square[2])         # Rank
    c(i, j)
}

# Updates a table with another table
update_table <- function (this, with_this) {
    n <- intersect(names(this), names(with_this))
    updated <- c(this[!names(this) %in% n], with_this[!names(with_this) %in% n], this[n] + with_this[n])
    updated
}

# Loads a pgn file
import_pgn <- function (filename) {
    games <- read.pgn(filename,
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
    games
}





# 1. What are the most popular squares for each piece?
# (rchess package turned out to be extremely slow)



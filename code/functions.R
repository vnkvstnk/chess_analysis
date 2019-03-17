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


# 1. What are the most popular squares for each piece?
# (rchess package turned out to be extremely slow)
popular_squares <- function (pgn, pieces = c("p", "k", "q", "b", "n", "r")) {
    require(rchess)
    require(dplyr)
    
    # Importing a game
    game <- Chess$new()
    game$load_pgn(pgn)
    data <- game$history(verbose = TRUE)  # data frame with information about each move
    
    # Preallocating output variable
    output <- vector("list", length(pieces)) %>% setNames(pieces)
    for (i in names(output)) output[[i]] <- matrix(0, 8, 8)
    
    # Populating the output
    for (i in 1:nrow(data)) {
        piece <- as.character(data[i, "piece"])
        index <- square_to_index(as.character(data[i, "to"]))
        output[[piece]][index[1], index[2]] <- output[[piece]][index[1], index[2]] + 1
    }
    output
}


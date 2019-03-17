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


# 1. What are the most popular squares for each piece?
# (rchess package turned out to be extremely slow)
popular_squares <- function (pgn, pieces = c("p", "k", "q", "b", "n", "r")) {
    require(stringr)
    require(dplyr)
    
    # Preallocating output variable
    output <- vector("list", length(pieces))
    names(output) <- pieces
    for (i in names(output)) output[[i]] <- matrix(0, 8, 8)
    
    # Split moves into white and black moves
    # a. Split move string at digits with dots.
    spl_pgn <- strsplit(pgn, "\\d+\\. ")[[1]][-1] %>%  # First element is empty
        
    # b. Squish each move (remove unneded spaces) and remove punctuation
    str_squish() %>% str_remove_all("[!\\?\\+#]") %>%
        
    # c. Split at space
    str_split(" ")
    
    moves <- list(white_moves = sapply(spl_pgn, function(x) x[1]),
                  black_moves = sapply(spl_pgn, function(x) x[2]))
    
    # Create a list, that stores squares of each piece ****(Should account for castling in the future)****
    squares <- list(pawn_squares=NULL, queen_squares=NULL, king_squares=NULL,
                    bishop_squares=NULL, knight_squares=NULL, rook_squares=NULL)
    
    # a. Pawn moves
    squares$pawn_squares <- moves$white_moves[grepl("^[a-h]\\d{1}$", moves$white_moves)]
    
    # b. Queen moves
    squares$queen_squares <- moves$white_moves[grepl("^Q", moves$white_moves)]
    
    # c. King moves
    squares$king_squares <- moves$white_moves[grepl("^K", moves$white_moves)]
    
    # d. Bishop moves
    squares$bishop_squares <- moves$white_moves[grepl("^B", moves$white_moves)]
    
    # e. Knight moves
    squares$knight_squares <- moves$white_moves[grepl("^N", moves$white_moves)]
    
    # f. Rook moves
    squares$rook_squares <- moves$white_moves[grepl("^R", moves$white_moves)]
    
    # Take only last two characters and present each set of squares as a table
    squares <- sapply(squares, str_sub, -2, -1)
    squares <- sapply(squares, table)
    
    squares
}


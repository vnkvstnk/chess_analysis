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
    squares <- lapply(squares, str_sub, -2, -1)
    squares <- lapply(squares, table)
    
    squares
}
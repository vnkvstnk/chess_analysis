# Input:
# 1. "movetext" - a character vector of unnumbered moves, i.e. "e4 Bc4 Qf3 g3 Qxf6 d3 Be3 Kd2 Nc3 Ne2 Nxc3"
# 2. "side" - side that played the moves. Important for castling handling. Default is "white".

# Output - 64-by-6 matrix with rows as squares and columns as pieces.
# Values represent number of times a piece went to the square

piece_heatmap <- function(movetext, side = "white") {
    source("./code/functions.R")
    
    # 0. Initializing output matrix
    pieces <- c("k", "q", "r", "b", "n", "p")
    files <- letters[1:8]
    ranks <- as.character(1:8)
    squares <- paste(rep(files, each = 8), ranks, sep = "")
    output <- matrix(0, 64, 6)
    colnames(output) <- pieces
    rownames(output) <- squares
    
    # 1. Removing [!?#+] and splitting movetext into separate moves
    movetext <- stringr::str_remove_all(movetext, "[!\\?#\\+]")
    moves <- strsplit(movetext, split = " ", fixed = TRUE)[[1]]
    
    # 2. Handle castling
    # 2.1 Short castle
    idx <- grepl("^O-O$", moves, fixed = TRUE)  # logical vector of castling moves
    n_castle <- sum(idx)        # Number of castling moves
    if (side == "white") king_sq <- "g1"; rook_sq <- "f1"
    if (side == "black") king_sq <- "g8"; rook_sq <- "f8"
    output[king_sq, "k"] <- n_castle
    output[rook_sq, "r"] <- n_castle
    moves <- moves[!idx]
    
    # 2.2 Long castle
    idx <- grepl("O-O-O", moves, fixed = TRUE)
    n_castle <- sum(idx) 
    if (side == "white") king_sq <- "c1"; rook_sq <- "d1"
    if (side == "black") king_sq <- "c8"; rook_sq <- "d8"
    output[king_sq, "k"] <- n_castle
    output[rook_sq, "r"] <- n_castle
    moves <- moves[!idx]
    
    # 3. Pawn moves
    # 3.1 Regular moves, no captures
    pattern <- "^[a-h]{1}\\d{1}"
    idx <- grepl(pattern, moves)  # logical vector of pawn moves
    pawn_squares <- sapply(moves[idx], substr, start = 1, stop = 2)  # taking first two characters
    output[pawn_squares, "p"] <- 1
    moves <- moves[!idx] # removing pawn moves
    
    # 3.2 Captures
    pattern <- "^[a-h]{1}x[a-h]{1}\\d{1}"
    idx <- grepl(pattern, moves)
    pawn_squares <- sapply(moves[idx], substr, start = 3, stop = 4)
    for (sq in pawn_squares) output[sq, "p"] <- output[sq, "p"] + 1 # Adding one for each square
    moves <- moves[!idx]
    
    # 4. King moves
    pattern <- "^K"
    idx <- grepl(pattern, moves)
    king_squares <- sapply(moves[idx], substr_right, n = 2)
    for (sq in king_squares) output[sq, "k"] <- output[sq, "k"] + 1
    moves <- moves[!idx]
    
    # 4. Queen squares
    pattern <- "^Q"
    idx <- grepl(pattern, moves)
    queen_squares <- sapply(moves[idx], substr_right, n = 2)
    for (sq in queen_squares) output[sq, "q"] <- output[sq, "q"] + 1
    moves <- moves[!idx]
    
    # 5. Rook squares
    pattern <- "^R"
    idx <- grepl(pattern, moves)
    rook_squares <- sapply(moves[idx], substr_right, n = 2)
    for (sq in rook_squares) output[sq, "r"] <- output[sq, "r"] + 1
    moves <- moves[!idx]
    
    # 6. Bishop squares
    pattern <- "^B"
    idx <- grepl(pattern, moves)
    bishop_squares <- sapply(moves[idx], substr_right, n = 2)
    for (sq in bishop_squares) output[sq, "b"] <- output[sq, "b"] + 1
    moves <- moves[!idx]
    
    # 7. Knight squares
    pattern <- "^N"
    idx <- grepl(pattern, moves)
    knight_squares <- sapply(moves[idx], substr_right, n = 2)
    for (sq in knight_squares) output[sq, "n"] <- output[sq, "n"] + 1
    moves <- moves[!idx]
    
    cat(length(moves), " moves left.\n")
    
    output
}
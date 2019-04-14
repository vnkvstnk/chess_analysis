# Input variables:
# movetext - text to process
# side - defines the side for which the moves are returned

# Output:
# Vector of moves (as character strings)


break_moves <- function(movetext, side = "white") {
    require(stringr)
    require(dplyr)
    
    # 1. Split the movetext at digits with dots
    movetext <- str_split(movetext, "\\d+\\. ")[[1]][-1] %>%  # Leaving the first empty element out
        
    # 2. Removing whitespaces from start and end of each move
        str_squish() %>%
        
    # 3. Split into white and black moves
        str_split(" ")
    
    # 4. Choose white or black and handle castling
    if (side == "white") {
        movetext <- sapply(movetext, function (x) x[1])
        
        # 5.1.a Adding "Kg1" and "Rf1" if "O-O" is present
        #       We don't mind if "O-O" stays in the movelist
        if ("O-O" %in% movetext) movetext <- c(movetext, c("Kg1", "Rf1"))
        
        # 5.2.a Adding "Kc1" and "Rd1" if "O-O-O" is present
        if ("O-O-O" %in% movetext) movetext <- c(movetext, c("Kc1", "Rd1"))

    } else if (side == "black") {
        movetext <- sapply(movetext, function (x) x[2])
        
        # 5.1.b Adding "Kg8" and "Rf8" if "O-O" is present
        if ("O-O" %in% movetext) movetext <- c(movetext, c("Kg8", "Rf8"))
        
        # 5.2.b Adding "Kc8" and "Rd8" if "O-O-O" is present
        if ("O-O-O" %in% movetext) movetext <- c(movetext, c("Kc8", "Rd8"))
    }
    
    # Returning the movelist without NAs that may sometimes occur
    movetext[!is.na(movetext)]
    
}
# Input variables:
# movetext - text to process
# side - defines the side for which the moves are returned

# Output:
# Character vector of moves separated by space


break_moves <- function(movetext, side = "white") {
    
    # 1. Split the movetext at whitespaces
    movetext <- strsplit(movetext, split = " ", fixed = TRUE)
    
    # 2. Remove empty elements that arise if a game was analysed with the engine
    movetext <- movetext[[1]][movetext[[1]] != ""]
     
    # 3. Choose white or black
    if (side == "white") movetext <- movetext[c(FALSE, TRUE, FALSE)]
    if (side == "black") movetext <- movetext[c(FALSE, FALSE, TRUE)]
    
    paste(movetext, collapse = " ")
    
}
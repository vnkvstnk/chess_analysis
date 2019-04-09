plot_board <- function(board) {
    require(ggplot2)
    require(tidyr)
    require(dplyr)
    # Reshaping "board" to make it suitable for plotting
    board <- as_tibble(board) %>%           # convert to dataframe
        mutate(rank = rownames(board)) %>%  # adding rank numbers as separate variable
        gather(file, count, -rank)          # leaving three variables in the table
    
    # Plotting
    g <- ggplot(data = board,
                aes(x = file,
                    y = rank,
                    fill = count)) + 
        geom_tile()
    
    print(g)

}
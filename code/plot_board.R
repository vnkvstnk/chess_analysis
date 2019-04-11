plot_board <- function(board) {
    require(ggplot2)
    require(tidyr)
    require(dplyr)
    require(RColorBrewer)
    # Reshaping "board" to make it suitable for plotting
    board <- as_tibble(board) %>%           # convert to dataframe
        mutate(rank = rownames(board)) %>%  # adding rank numbers as separate variable
        gather(file, count, -rank)          # leaving three variables in the table
    
    # Plotting
    g <- ggplot(data = board,
                aes(x = file,
                    y = rank,
                    fill = count)) + 
        geom_tile(colour="white",
                  size=0.25) + 
        coord_fixed() + 
        labs(x = "",
             y = "",
             fill = "Counts") + 
        theme(text = element_text(size = 22,
                                  face = "bold"),
              axis.ticks = element_blank(),
              legend.text = element_text(size = 12,
                                         face = "plain"),
              legend.title = element_text(size = 12,
                                          face = "plain"),
              panel.background = element_blank())
    
    print(g)

}
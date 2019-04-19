plot_board <- function(board) {
    require(ggplot2)
    require(tidyr)
    require(dplyr)
    require(RColorBrewer)
    
    # Reshaping "board" object to make it suitable for plotting
    board <- as_tibble(board, title = "Default title") %>%           # convert to data frame
        mutate(rank = rownames(board)) %>%  # adding rank numbers as a separate variable
        gather(file, count, -rank)          # leaving three variables in the table
    
    # Plotting
    g <- ggplot(data = board,
                aes(x = file,
                    y = rank,
                    fill = count)) + 
        geom_tile(colour="white",
                  size=0.25,
                  alpha = 0.8) + 
        coord_fixed() + 
        labs(x = "",
             y = "",
             title = title,
             fill = "Counts") + 
        theme(text = element_text(size = 22,
                                  face = "bold"),
              axis.ticks = element_blank(),

              panel.background = element_blank(),
              # Anjusting legend
              legend.margin = margin(),  # Zero-width margin
              legend.title = element_text(size = 12,
                                          face = "plain"),  # Font adjustment
              legend.text = element_text(size = 12,
                                         face = "plain"),   # Font anjustment
              legend.key.height = grid::unit(0.8, "cm"),
              legend.key.width=grid::unit(0.4,"cm")) 
        
    
    print(g)

}
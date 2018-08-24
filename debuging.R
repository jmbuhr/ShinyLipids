# Debugging notes and scripts

# Notes -----------------------------------------------------------------------------------------------------------


# Scripts ---------------------------------------------------------------------------------------------------------

input
input %>% names()

dev.off()
names(df)
df


plt <- ggplot(df) +
    aes(x = !!sym(input$aes_x),
        y = !!sym(input$aes_color),
        fill = !!sym(input$aes_y)) +
    geom_raster() +
    mainTheme +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = input$heatLabSize, colour = "black"),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_blank(),
        plot.background = element_blank(),
        panel.grid = element_blank()
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_fill_viridis_c(option = input$heatColor, name = "mol%") +
    NULL

plt






# Loading data




# * PCA -----------------------------------------------------------------------------------------------------------




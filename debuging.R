# Debugging notes and scripts

# Notes -----------------------------------------------------------------------------------------------------------


# Scripts ---------------------------------------------------------------------------------------------------------

input
input %>% names()

names(df)

df <- rawData()






names(df)

dev.off()

plt <- df %>%
    ggplot()

plt <- plt +
    aes(x = !!sym(input$aes_x), y = !!sym(input$aes_y))+
    geom_point()+
    labs(title = "Alpha Version")

plt






# Loading data




# * PCA -----------------------------------------------------------------------------------------------------------




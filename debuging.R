# Debugging notes and scripts

# Notes -----------------------------------------------------------------------------------------------------------


# Scripts ---------------------------------------------------------------------------------------------------------

input
input %>% names()

names(df)

df

is.discrete(df["oh"])
is.factor(df[[input$aes_facet1]])

class(df[[input$aes_color]])

input$aes_color
input$aes_facet1


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




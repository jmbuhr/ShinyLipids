# Debugging notes and scripts

# Notes -----------------------------------------------------------------------------------------------------------


# Scripts ---------------------------------------------------------------------------------------------------------

input
input %>% names()

names(df)

df

dev.off()

plt <- df %>%
    ggplot()

plt <- plt +
    aes(x = !!sym(input$aes_x), y = !!sym(input$aes_y))+
    geom_point()+
    labs(title = "Alpha Version")

plt


df

if(!is.null(input$filter_cat)){
    df <- df %>% filter(category %in% input$filter_cat)
}
if(!is.null(input$filter_class)){
    df <- df %>% filter(class %in% input$filter_class)
}
if(!is.null(input$filter_func)){
    df <- df %>% filter(func_cat %in% input$filter_func)
}
if(!is.null(input$filter_length)){
    df <- df %>% filter(length %>% between(input$filter_length[1], input$filter_length[2]))
}
if(!is.null(input$filter_db)){
    df <- df %>% filter(db %>% between(input$filter_db[1], input$filter_db[2]))
}
if(!is.null(input$filter_oh)){
    df <- df %>% filter(oh %>% between(input$filter_oh[1], input$filter_oh[2]))
}






# Loading data




# * PCA -----------------------------------------------------------------------------------------------------------




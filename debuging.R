# Debugging notes and scripts

# Notes -----------------------------------------------------------------------------------------------------------


# Scripts ---------------------------------------------------------------------------------------------------------


hmap <- gplots::heatmap.2(out$data,
          col = out$colors,
          dendrogram = "both",
          density.info = "none",
          trace = "none",
          breaks = out$break.points,
          cexCol  = input$heatlabsz,
          cexRow = input$heatlabsz,
          margins = c(input$heatmarx,input$heatmary))


out$data %>% as_tibble(rownames = "sample") %>%
    gather(-sample, key = "species", value = "value") %>%
    ggplot()+
    aes(x = sample, y = species, fill = value)+
    geom_raster()+
    coord_flip()+
    .theme

input$heatlabsz

SubPlotData()[c('sample','sample_replicate','xval','standardizedSum')] %>%
    spread(key = 'xval', value = 'standardizedSum')

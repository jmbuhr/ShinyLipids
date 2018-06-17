# Debugging notes and scripts

# Notes -----------------------------------------------------------------------------------------------------------


# Scripts ---------------------------------------------------------------------------------------------------------

### Old heatmap
# hmap <- gplots::heatmap.2(out$data,
#           col = out$colors,
#           dendrogram = "both",
#           density.info = "none",
#           trace = "none",
#           breaks = out$break.points,
#           cexCol  = input$heatlabsz,
#           cexRow = input$heatlabsz,
#           margins = c(input$heatmarx,input$heatmary))

# New heatmap
sink()
SubPlotData()[c('sample','sample_replicate','xval','standardizedSum')] %>%
    group_by(sample, xval) %>% summarise(
        value = mean(standardizedSum)
    ) %>%
    ggplot()+
    aes(x = xval, y = sample, fill = value)+
    geom_raster()+
    scale_fill_viridis()+
    .theme+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    NULL

dev.off()

# from bevore PCA functions ---------------------------------------------------------------------------------------

test <- function(){
    data(mtcars)
    data<-mtcars

    tmp<-list()
    tmp$pca.algorithm<-"svd"
    tmp$pca.components<-2
    tmp$pca.center<-TRUE
    tmp$pca.scaling<-"uv"
    tmp$pca.data<-data
    pca.inputs<-tmp

    res <- devium.pca.calculate(pca.inputs,return="list",plot=FALSE)

    results<-"biplot"
    color<-data.frame(am=mtcars$am,vs=mtcars$vs)
    color<-data.frame(color=join.columns(color))
    color=NULL
    xaxis<-1
    yaxis=2
    group.bounds="polygon"
    plot.PCA(pca=res,results=results,yaxis=yaxis,xaxis=xaxis,size=1,color=color, label=TRUE, legend.name = NULL,font.size=.1,group.bounds,alpha=.75)
}

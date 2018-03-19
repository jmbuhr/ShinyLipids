function(input, output, session) { 
  
  .theme<- theme(
    axis.line = element_line(colour = 'gray', size = .75), 
    panel.background = element_blank(),  
    plot.background = element_blank()
  )
  # Ranges for zooming
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush 
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })

  
  # Load the rawdata
  rawdata <- reactive({
    print("Raw data loaded")
    collect(getraw(input$ID, "data2"))
    # getraw(input$ID, "data2")
  })
  
  ### Preparation of Metadatatable
  # clean data
  observeEvent(input$refresh,{
    # Note: The following lines are identical to the ones concerning cleandata in 
    #       global.R
    sql_data <- paste("SELECT * FROM id_info")
    rawmetadata <- collect(tbl(database_connection, sql(sql_data)))
    cleandata <- rawmetadata
    # Some formatting of the columns that contain temporal information
    cleandata$date_upload <- as.Date(cleandata$date_upload, format = '%y%m%d')
    cleandata$date_sample <- as.Date(cleandata$date_sample, format = '%y%m%d')
    cleandata$date_extraction <- as.Date(cleandata$date_extraction, format = '%y%m%d')
    cleandata$date_measured <- as.Date(cleandata$date_measured, format = '%y%m%d')
    cleandata <<- cleandata[order(cleandata$id),]
    
    # Update Names and Tables
    datanames <- as.list(cleandata$id)
    names(datanames) <- paste(cleandata$id, cleandata$title)
    datanames <<- datanames
    updateSelectInput(session, "ID", choices = datanames)
    updateSelectInput(session, "downid", choices = datanames)
  })
  # Initialization
  output$metadataTable <- DT::renderDataTable({
    datatable(
      if (input$fulltable) {
        cleandata
      } else {
        cleandata[c("id", "title", "date_upload", "status", "sample_from")]
      },
      options = list(orderClasses = TRUE, pageLength = 10,
                     paging = FALSE, searching = TRUE, dom = 'C<"clear">lfrtip',
                     colVis = list(exclude = c(0,1), activate = 'mouseover'),
                     order = list(0,'desc'),
                     deferRender = TRUE,
                     scrollY = 500,
                     scrollCollapse = TRUE
      ), 
      rownames = FALSE, 
      selection = 'single'
    ) 
  })  
  #Updating
  observeEvent(input$refresh,{
    output$metadataTable <- DT::renderDataTable({
      datatable(
        if (input$fulltable) {
          cleandata
        } else {
          cleandata[c("id", "title", "date_upload", "status", "sample_from")]
        },
        options = list(orderClasses = TRUE, pageLength = 10,
                       paging = FALSE, searching = TRUE, dom = 'C<"clear">lfrtip',
                       colVis = list(exclude = c(0,1), activate = 'mouseover'),
                       order = list(0,'desc')
        ), 
        rownames = FALSE, 
        extensions = 'ColVis'
      ) 
    })
  })
  
  
  
  ### Select plotting choices (var:plotchoices)
  # Choices: "Plot Values", "Errorbar (Mean +/- 1SD)", "Nr of Datapoints", "Mean Values", 
  #          "Show Legend", "Ttest", "Median"
  observe({
    if (input$plottype == 'barplot') {
      updateCheckboxGroupInput(session, "checkGroup", 
                               choices = plotchoices,
                               selected = plotchoices[c("Show Legend")])
    } else if (input$plottype == 'boxplot') {
      updateCheckboxGroupInput(session, "checkGroup",
                               choices = plotchoices[c("Nr of Datapoints", "Show Legend",
                                                       "Log10-Transformation")],
                               selected = plotchoices[c("Show Legend")])
    } else if (input$plottype == 'rawplot') {
      updateCheckboxGroupInput(session, "checkGroup",
                               choices = plotchoices[c("Plot Values", "Errorbar (Mean +/- 1SD)", "Nr of Datapoints",
                                                       "Show Legend", "Median", "Identify Individuals",
                                                       "Log10-Transformation")],
                               selected = plotchoices[c("Plot Values", "Errorbar (Mean +/- 1SD)", 
                                                        "Errorbar (Mean +/- 1SE)","Show Legend",
                                                        "Log10-Transformation")])
    } else if (input$plottype == "pointrange") {
      updateCheckboxGroupInput(session, "checkGroup",
                               choices = plotchoices[c("Nr of Datapoints", "Show Legend",
                                                       "Log10-Transformation")],
                               selected = plotchoices[c("Show Legend")])
    }
  }) 
  
  
  ############################################################
  observeEvent(input$metadataTable_row_last_clicked,{
    #*****************************************
    # I do no know if I need all this until the next Stars... 
    sql_data <- paste("SELECT * FROM id_info")
    rawmetadata <- collect(tbl(database_connection, sql(sql_data)))
    cleandata <- rawmetadata
    # Some formatting of the columns that contain temporal information
    cleandata$date_upload <- as.Date(cleandata$date_upload, format = '%y%m%d')
    cleandata$date_sample <- as.Date(cleandata$date_sample, format = '%y%m%d')
    cleandata$date_extraction <- as.Date(cleandata$date_extraction, format = '%y%m%d')
    cleandata$date_measured <- as.Date(cleandata$date_measured, format = '%y%m%d')
    cleandata <<- cleandata[order(cleandata$id),]
    
    # Update Names and Tables
    datanames <- as.list(cleandata$id)
    names(datanames) <- paste(cleandata$id, cleandata$title)
    datanames <<- datanames
    #*****************************************
    
    clicked <- as.integer(input$metadataTable_row_last_clicked)
    vectorOf_datanames <-as.vector(unlist(datanames )) 
    indexFor_datanames <- match(clicked,vectorOf_datanames)
    indexFor_datanames <- clicked # Possible Fix as of Febr 2018
    updateSelectInput(session, "ID", choices = datanames, selected=datanames[indexFor_datanames] )
  })
  ############################################################
  
  
  
  ### Select sample subset to plot
  observe({
    input$ID
    choice <- rawdata()
    choicesam <- as.factor(choice$sample)
    updateSelectInput(session, "samplesub", choices = levels(choicesam))
    updateSelectInput(session, "samplebase", choices = levels(choicesam))
  })
  
  ### Select data subset to plot
  observe({
    input$ID
    choice <- rawdata()
    if (input$what == "class") {
      choicewhat <- as.factor(choice$class)
    } else if (input$what == "category") {
      choicewhat <- as.factor(choice$category)
    } else if (input$what == "func_cat") {
      choicewhat <- as.factor(choice$func_cat)
    } else if (input$what == "chains") {
      choicewhat <- as.factor(choice$chains) 
    } else if (input$what == "chain_sums") {
      choicewhat <- as.factor(choice$chain_sums)    
    } else if (input$what == "length") {
      choicewhat <- as.factor(choice$length)
    } else if (input$what == "db") {
      choicewhat <- as.factor(choice$db)
    } else if (input$what == "oh") {
      choicewhat <- as.factor(choice$oh)
    }
    updateSelectInput(session, "whatsub", choices = levels(choicewhat),
                      label = paste("Which", revwhatnames[input$what]))    
    
  })
  ### Select within sub to plot
  observe({
    input$ID
    choice <- rawdata()
    choicewith <- NA
    if (input$within == "Category") {
      choicewith <- as.factor(choice$category)
    } else if (input$within == "Functional Category") {
      choicewith <- as.factor(choice$func_cat)
    } else if (input$within == "Class") {
      choicewith <- as.factor(choice$class)
    }
    if (input$what %in% c("chains", "chain_sums") & input$within == "Class") {
      updateSelectInput(session, "withsub", choices = levels(choicewith),
                        selected = levels(choicewith)[1])
    } else {
      updateSelectInput(session, "withsub", choices = levels(choicewith))
      
    }
  })  
  ### Select replicate sub to plot and the same for technical replicates 
  observe({
    input$ID
    choice <- rawdata()
    # choicesam <- as.factor(choice$sample_identifier) # AVERAGING
    choicesam <- as.factor(choice$sample_replicate)
    choicetech <- as.factor(choice$sample_replicate_technical)
    updateSelectInput(session, "repsub", choices = levels(choicesam))
    updateSelectInput(session, "techsub", choices = levels(choicetech))
  })
  ### If Species/Sum Species selected update to summarized: Class and select one
  observe({
    if(input$what %in% c("chains", "chain_sums")) {
      updateSelectInput(session, "within", choices = withinnames, 
                        selected = "Class")
      choice <- rawdata()
      choicewith <- as.factor(choice$class)
      updateSelectInput(session, "withsub", choices = levels(choicewith),
                        selected = levels(choicewith)[1])
    }
  })
  
  observe({
    if(input$what %in% c("class", "category", "func_cat")) {
      updateSelectInput(session, "within", choices = withinnames,
                        selected = "Sample")
    }
  })
  
  FirstData <- reactive({
    collect(prepareData2(rawdata(), what = input$what, 
                         within = input$within,
                         standard = input$standard, 
                         ID = input$ID))
  })
  
  
  ### Create Plot
  PlotData <- reactive({
    if(input$stdSub || input$add_rem) {
      collect(prepareData(rawdata(), what = input$what, 
                          within = input$within,
                          standard = input$standard, 
                          ID = input$ID,
                          standardSubset = input$stdSub, 
                          add = input$add_rem,
                          samplesub = input$samplesub,
                          withsub = input$withsub,
                          whatsub = input$whatsub,
                          repsub = input$repsub,
                          techsub = input$techsub))      
    } else {
      FirstData()
    }
  
  })
  
  SubPlotData <- reactive({
    # Note: You need to use shiny::validate instead of just validate for those 
    #   operations, since there is a second validate in jsonlite
    shiny::validate(
      need(check_ttest(PlotData(), input), 
           "Please ensure that you have selected exactly two samples!")
    )
   
    # TODO: Clean advplotoptions
    advplot <<- c(input$symbolchoice)
    if (input$add_rem) {
      advplot <<- c(advplot, "add")
    } 
    if (input$highlight) {
      advplot <<- c(advplot, "highlight")
    }
    
    prepareSubset(PlotData(), advanced = advplot,
                  samplesub = input$samplesub, 
                  whatsub = input$whatsub, 
                  withsub = input$withsub, 
                  within = input$within,
                  repsub = input$repsub,
		              base = input$samplebase)
  })

  
  
  PCA.results<-reactive({
    print("started new")
      # list(data=read.csv(input$files$datapath, header=T, stringsAsFactors =T),
      # data2=rnorm(10))
      
      # }
      #adapted from another devium
      pca.inputs<-list()
      # start.data<<-read.csv(input$files$datapath, header=T, stringsAsFactors =T)
      A <<- SubPlotData()
      B <- A[c('sample','sample_replicate','xval','standardizedSum')]
      C <- spread(B, key = 'xval', value = 'standardizedSum')
      # print(C)
      # print(input$samplebase)
      if (length(input$samplebase) != 0) {
        unchanged <- data.frame(C[,-c(1)],row.names = 1)
        print(unchanged)
        start.data <- data.frame(C[,-c(1)],row.names = 1)
      } else {
        start.data <- data.frame(C[,-c(1)],row.names = 1)
      }
      # print()
      # start.data <- as.matrix(C,row.names=1)
      
      # print(dim(start.data))
      # print(dim(C[,1]))
      # print(row.names(start.data))
      # row.names(start.data) <- C[,1]
      # D <- D[rowSums(D)>0, ]
      # D <- t(t(D)/colSums(D)) * 100
      # print("start.data")
      # print(start.data)
      pca.inputs$pca.data<-start.data
      pca.inputs$pca.algorithm<-input$method
      pca.inputs$pca.components<-input$PCs
      pca.inputs$pca.center<-input$center
      pca.inputs$pca.scaling<-input$scaling
      pca.inputs$pca.cv<-input$cv # currently not used
      pca.inputs$pca.groups<-C[[1]]
      # print(pca.inputs)
      devium.pca.calculate(pca.inputs,return="list",plot=F)
      # devium.pca.calculate(pca.inputs,return="model",plot=F)
    
  })
  
  plotInput <- reactive({
    # A and B are only used for debugging purposes TODO: Delete later
    print("started")
    A <<- SubPlotData()
    B <<- preparePlots(SubPlotData(), checkGroup = input$checkGroup,
                       plottype = input$plottype,
                       what = input$what,
                       within = input$within,
                       standard = input$standard,
                       advanced = advplot)
    # print(B)
    print(ranges$x)
    print(ranges$y)
    print("ended")
    # B <- ggplotly(B)
    B  + coord_cartesian(xlim = ranges$x, ylim = ranges$y)
  })
  
  prepareHeatmap <- reactive({
    library("gplots")
    A <<- SubPlotData()
    B <- A[c('sample','sample_replicate','xval','standardizedSum')]
    C <- spread(B, key = 'xval', value = 'standardizedSum')
    # print(C)
    out <- list()
    out$data <- as.matrix(data.frame(C[,-c(1)],row.names = 1))
    # print(data)
    if (input$heatcolscheme == "heatcolors") {
      out$colors <- "heat.colors"
    } else if (input$heatcolscheme == "bluewhiteviolet") {
      out$colors <- colorRampPalette(c("blue","blue","white","blueviolet","blueviolet"))(399)
    } else if (input$heatcolscheme == "whitegreenblack") {
      out$colors <- colorRampPalette(c("white","green","black"))(399)
    } else if (input$heatcolscheme == "bluewhitered") {
      out$colors <- colorRampPalette(c("blue","white","red"))(399)
    }
    if (input$heatscale) {
      out$break.points <- c(seq(input$heatmin,input$heatmedmin,length=101)[-101],seq(input$heatmedmin,input$heatmedium,length=101)[-101],seq(input$heatmedium,input$heatmedmax,length=101)[-101],seq(input$heatmedmax,input$heatmax,length=101)[-101])
    } else {
      break.points <- NA
    }
    return(out)
  })
  
  plotScores <- reactive({
    if (is.null(PCA.results())) {
      return(NULL)
    } else {
      tmp<-PCA.results()
      scores<-data.frame(tmp$pca.scores)
      if(nrow(tmp$pca.diagnostics)==nrow(scores))
      {
        if(any(tmp$pca.diagnostics$DmodX=="NaN")){tmp$pca.diagnostics$DmodX<-1}
        scores<-data.frame(leverage=tmp$pca.diagnostics$leverage, dmodx=tmp$pca.diagnostics$DmodX,scores)
      } else {
        scores<-data.frame(leverage=1, dmodx=1,scores)
      }
      # theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
      # circle <- cbind(cos(theta), sin(theta))
      # df.u <- data.frame(xvar=scores[,1], yvar=scores[,2],groups=tmp$pca.groups,dmodx=tmp$pca.diagnostics$DmodX)
      # ell <- ddply(df.u, "groups", function(x) {
      #   if (nrow(x) <= 2) {
      #     return(NULL)
      #   }
      #   sigma <- var(cbind(x$xvar, x$yvar))
      #   mu <- c(mean(x$xvar), mean(x$yvar))
      #   ed <- sqrt(qchisq(input$prob, df = 2))
      #   data.frame(sweep(circle %*% chol(sigma) * ed, 2,
      #                    mu, FUN = "+"), groups = x$groups[1])
      # })
      # # print(ell)
      # names(ell)[1:2] <- c("PC1", "PC2")
      # print(row.names(tmp$pca.scores))
      p<-ggplot(scores,mapping = aes_string(x = names(scores)[3], y = names(scores)[4]
                                            ,color="samples"
                                            # ,color=factor(as.numeric(tmp$pca.groups))
                                            # ,color="red"
                                            # ,size=names(scores)[2]
      ))
      p <- p + scale_size_continuous("DmodX", range = c(4, 10))
      p <- p + geom_point(alpha=0.75,aes(colour=tmp$pca.groups,group=tmp$pca.groups),size=input$size)
      if (input$labels) {
        p <- p + geom_text(aes(label = row.names(tmp$pca.scores), color = tmp$pca.groups), size = 3,nudge_y=0.3)
      }
      # p <- p + geom_path(data = ell, aes(colour = ell$groups, group = ell$groups)) + coord_equal()
      p <- p +.theme
      p
      # print(p)
    }
  })
  
  plotLoadings <- reactive({
    if (is.null(PCA.results())) { 
      return(NULL)
    } else {
      tmp<-PCA.results()
      loadings<-data.frame(tmp$pca.loadings,names=rownames(tmp$pca.loadings))
      
      #plot
      p<-ggplot(loadings,mapping = aes_string(x = names(loadings)[1], y = names(loadings)[2], label = "names")) + 
        geom_text(size=4,alpha=0.75) +.theme
      # print(p)
    }
  })
  
  plotScree1 <- reactive({
    library(ggplot2)
    library(reshape2)
    if (is.null(PCA.results())) { 
      return(NULL)
    } else {
      x<-PCA.results()
      eigenvalues<-data.frame(x$pca.eigenvalues)
      
      # make.scree.plot(x)	
      # p <- make.scree.plot.bar(x)
      library("gridExtra")
      .theme<- theme(
        axis.line = element_line(colour = 'gray', size = .75), 
        panel.background = element_blank(),  
        plot.background = element_blank()
      )	
      
      tmp<-data.frame(melt(eigenvalues$eigenvalue),PCs=rep(1:nrow(eigenvalues)))
      tmp$value<-tmp$value*100
      p1<-ggplot(tmp, aes(y=value, x = as.factor(PCs)))+geom_bar( fill="gray",stat="identity",position=position_dodge())+
        .theme + geom_hline(yintercept=1,linetype=2) + ylab("% variance explained") + xlab("Principal Component")
    }
    return(p1)
  })
  
  plotScree2 <- reactive({
    library(ggplot2)
    library(reshape2)
    if (is.null(PCA.results())) { 
      return(NULL)
    } else {
      x<-PCA.results()
      eigenvalues<-data.frame(x$pca.eigenvalues)
      
      # make.scree.plot(x)	
      # p <- make.scree.plot.bar(x)
      library("gridExtra")
      .theme<- theme(
        axis.line = element_line(colour = 'gray', size = .75), 
        panel.background = element_blank(),  
        plot.background = element_blank()
      )	
      
      #cumulative	
      eigenvalues$eigenvalues<-cumsum(eigenvalues$eigenvalues)
      tmp<-data.frame(melt(eigenvalues),PCs=rep(1:nrow(eigenvalues)))
      p2<-ggplot(tmp, aes(y=value, x = as.factor(PCs), fill=variable))+geom_bar( stat="identity",position=position_dodge())+
        .theme + geom_hline(yintercept=.8,linetype=2) +xlab("Principal Component")
    }
    return(p2)
  })
  
  plotPCA <- reactive({
    # A and B are only used for debugging purposes TODO: Delete later
    print("PCA started")
    A <<- SubPlotData()
    B <- A[c('sample_replicate','xval','standardizedSum')]
    # print(B)
    C <- spread(B, key = 'xval', value = 'standardizedSum')
    
    D <- data.frame(C[,-1])
    # D <- D[rowSums(D)>0, ]
    # D <- t(t(D)/colSums(D)) * 100
    # print(D)
    # D <- t(D)
    # print(D)
    pc.data <- prcomp(D, scale = T,center=T)
    # print(attributes(pc.data))
    # print(pc.data$x)
    print("prcomp complete")
    # ggbiplot(pc.data, obs.scale = 1, var.scale = 1,
    #          groups = colnames(C[,-1]), ellipse = input$ellipse, 
    #          circle = T,pc.biplot=T,var.axes = F)
    # ,labels=C$sample_replicate
    # g <- plot(pc.data,type="l")
    library(ggbiplot)
    g <- ggbiplot(pc.data, obs.scale = 1, var.scale = 1,
             groups = D['xval'], ellipse = TRUE,
             circle = T, pc.biplot=F,var.axes = T,alpha=1)
    # print(g)
    print("PCA ended")
    # B <- ggplotly(g)
    # B  + coord_cartesian(xlim = ranges$x, ylim = ranges$y)
  })
  output$plot1 <- renderPlot({
    plotInput()
    # plotInput()
  })
  # output$plotPCA <- renderPlot({
  #   plotPCA()
  #   # plotInput()
  # })
  output$heatmap <- renderPlot({
    out <- prepareHeatmap()
    heatmap.2(out$data
              ,col = out$colors
              ,dendrogram="both",density.info="none",trace="none"
              ,breaks=out$break.points
              ,cexCol=input$heatlabsz,cexRow = input$heatlabsz,margins=c(input$heatmarx,input$heatmary))
  })
  output$pathway <- renderPlot({
    library("gplots")
    A <<- SubPlotData()
    print(A)
    B <- A[c('sample','sample_replicate','xval','standardizedSum')]
    C <- spread(B, key = 'xval', value = 'standardizedSum')
    Ttestdata = Ttest(A, within = input$within)
    # print(C)
    data <- as.matrix(data.frame(C[,-c(1)],row.names = 1))
    print(data)
    library(topGO)
    library("hgu95av2.db")
    library(genefilter)
    data(geneList)
    data <- read.csv("C://Users/bbadm/Desktop/protGroups_dani_erweitert.csv",header = T, sep=",",dec=".")[,c(52,13,15)]
    geneList <- 10^(-data[,3])
    names(geneList) <- data[,1]
    geneList <- geneList[!is.na(geneList)]
    head(geneList)
    
    BPterms <- ls(GOBPTerm)
    # selProbes <- genefilter(geneList, filterfun(pOverA(0.20, log2(100)), function(x) (IQR(x) > 0.25)))
    # eset <- ALL[selProbes, ]
    
    
    # affyLib <- paste(annotation(geneList), "db", sep = ".")
    sum(topDiffGenes(geneList))
    sampleGOdata <- new("topGOdata",description = "Simple session", ontology = "CC",allGenes = geneList, geneSel = topDiffGenes,nodeSize = 10,annot = annFUN.org, mapping="org.Hs.eg.db", ID = "symbol")
    resultFisher <- runTest(sampleGOdata, algorithm = "classic", statistic = "fisher")
    resultKS <- runTest(sampleGOdata, algorithm = "classic", statistic = "ks")
    resultKS.elim <- runTest(sampleGOdata, algorithm = "elim", statistic = "ks")
    allRes <- GenTable(sampleGOdata, classicFisher = resultFisher,classicKS = resultKS, elimKS = resultKS.elim,orderBy = "elimKS", ranksOf = "classicFisher")
    pValue.classic <- score(resultKS)
    pValue.elim <- score(resultKS.elim)[names(pValue.classic)]
    gstat <- termStat(sampleGOdata, names(pValue.classic))
    gSize <- gstat$Annotated / max(gstat$Annotated) * 4
    gCol <- colMap(gstat$Significant)
    plot(pValue.classic, pValue.elim, xlab = "p-value classic", ylab = "p-value elim",pch = 19, cex = gSize, col = gCol)
    sel.go <- names(pValue.classic)[pValue.elim < pValue.classic]
    cbind(termStat(sampleGOdata, sel.go),elim = pValue.elim[sel.go],classic = pValue.classic[sel.go])
    # printGraph(sampleGOdata, resultKS.elim, firstSigNodes = 5, fn.prefix = "sampleFile2", useInfo = "all", pdfSW = TRUE)
    showSigOfNodes(sampleGOdata, score(resultKS.elim), firstSigNodes = 5, useInfo = "all")
    # printGraph(GOdata, resultFisher, firstSigNodes = 5, fn.prefix = "sampleFile", useInfo = "all", pdfSW = TRUE)
    # y <- as.integer(sapply(eset$BT, function(x) return(substr(x, 1, 1) =='T')))
    # table(y)
    write.csv(allRes,file="C://Users/bbadm/Documents/Dani_GO_CC.csv",sep=",")
  })
  output$scores <- renderPlot({
    print(plotScores())
  })
  output$loadings <- renderPlot({
    print(plotLoadings())
  })
  
  #make screeplot
  output$screeplot1 <- renderPlot({
    print(plotScree1())
  })	
  output$screeplot2 <- renderPlot({
    print(plotScree2())
  })	
  
  # output$plot1 <- renderPlotly({
    # plotInput()
  # })
  
  output$sum_muMol <- DT::renderDataTable({
    outtab <- collect(rawdata())
    # Averaging over the technical replicates
    outtab <- outtab %>% 
      group_by(id, lipid, category, func_cat, class, length, 
               db, oh, chains, chain_sums, sample, sample_replicate) %>% 
      summarize(value = mean(value, na.rm = T))
    # Summing the relevant parts
    outtab <- outtab %>% group_by(sample, sample_replicate) %>% 
      summarise(sum = sum(value, na.rm = T))
    names(outtab) <- c("Sample", "Sample Replicate", "µM")
    datatable(outtab,
              options = list(orderClasses = TRUE, pageLength = 10,
                             paging = FALSE, searching = TRUE, dom = 'C<"clear">lfrtip',
                             # colVis = list(exclude = c(0,1), activate = 'mouseover'),
                             order = list(0,'desc'),
                             deferRender = TRUE,
                             scrollY = 500,
                             scrollCollapse = TRUE
              ), 
              rownames = FALSE,
              selection = "none"
              # extensions = 'ColVis'
              # selection = 'single'          
    )
  })
  
  
  ##################################################  
  ### Saving
  
  # Saving a dataset as a .RData
  output$downdata <- downloadHandler(
    filename = function() {
      paste0(input$downid, ".RData")
    },
    content = function(file) {
      dataset <- collect(getraw(input$downid,"data2"))
      save(dataset, file = file)
    }
  )
  
  # Saving the current plot as a pdf
  output$saveplot <- downloadHandler(
    filename = function() {
      paste0(input$ID, "_", Sys.Date(),".pdf") 
    },
    content = function(file) {
      pdf(file, height = 10, width = 20)
      print(plotInput())
      dev.off()
    }
  )
  output$saveheatmap <- downloadHandler(
    filename = function() {
      paste0(input$ID, "_", Sys.Date(),"_heatmap.pdf") 
    },
    content = function(file) {
      out <- prepareHeatmap()
      pdf(file, height = input$heightheat, width = input$widthheat)
      heatmap.2(out$data,col = out$colors,dendrogram="both",density.info="none",trace="none",breaks=out$break.points,cexCol=1,margins=c(input$heatmarx,input$heatmary))
      dev.off()
    }
  )
  output$saveScores <- downloadHandler(
    filename = function() {
      paste0(input$ID, "_", Sys.Date(),"_Scores.pdf") 
    },
    content = function(file) {
      pdf(file, height = input$heightPCA, width = input$widthPCA)
      print(plotScores())
      dev.off()
    }
  )
  output$saveLoadings <- downloadHandler(
    filename = function() {
      paste0(input$ID, "_", Sys.Date(),"_Loadings.pdf") 
    },
    content = function(file) {
      pdf(file, height = input$heightPCA, width = input$widthPCA)
      print(plotLoadings())
      dev.off()
    }
  )
  output$savescree1 <- downloadHandler(
    filename = function() {
      paste0(input$ID, "_", Sys.Date(),"_Scree.pdf") 
    },
    content = function(file) {
      pdf(file, height = input$heightPCA, width = input$widthPCA)
      print(plotScree1())
      dev.off()
    }
  )
  output$savescree2 <- downloadHandler(
    filename = function() {
      paste0(input$ID, "_", Sys.Date(),"_Scree.pdf") 
    },
    content = function(file) {
      pdf(file, height = input$heightPCA, width = input$widthPCA)
      print(plotScree2())
      dev.off()
    }
  )
  # Save the corresponding ggplot data for further computations
  output$saveplotR <- downloadHandler(
    filename = function() {
      paste0(input$ID, "_", Sys.Date(),".RData") 
    },
    content = function(file) {    
      lipidplot <- plotInput()
      save(lipidplot, lipidsubset, file = file)
    }
  )
  # General plot saving
  output$save <- downloadHandler(
    filename = function() {
      paste0(input$ID, "_", Sys.Date(),".pdf") 
    },
    content = function(file) {
      pdf(file, height = input$height, width = input$width)
      print(plotInput())
      dev.off()
    }
  )  
  
  ### saves the current selection of metadata columns in an csv file
  output$savemeta <- downloadHandler(
    filename = function() {
      paste0('metadatatable.csv')
    },
    content = function(file) {
      write.csv(cleandata, row.names = FALSE, col.names = FALSE, file = file)
    }
  )
  # Save standardized Data as a File
  output$savestddata <- downloadHandler(
    filename = function() {
      paste0(input$ID, "_", Sys.Date(),".csv") 
    },
    content = function(file) {
      write.csv(collect(tidySave), row.names = FALSE, 
                file = file)
    })
  ##################################################
  # create the standardisation possibilities
  observe({
    if (input$what != "class")
    {
      if (input$within == "Sample")
      {
        updateSelectInput(session, "standard", choices = "Sample")
      }
      else if (input$within == "Category")
      {
        updateSelectInput(session, "standard", choices = c("Sample", "Category", 
                                                           "Functional Category"))
      } 
      else if (input$within == "Functional Category")
      {
        updateSelectInput(session, "standard", choices = c("Sample", "Category", 
                                                           "Functional Category"))
      } 
      else if (input$within == "Class")
      {
        updateSelectInput(session, "standard", choices = c("Sample", "Category",
                                                           "Functional Category", "Class"))
      }
    } else {
      updateSelectInput(session, "standard", choices = c("Sample", "Category",
                                                         "Functional Category"))
    }
  })  
  
  ###  ShinyJS stuff
  # advanced saving options
  observe({
    shinyjs::onclick("toggleAdvanced",
                     shinyjs::toggle(id = "advanced", anim = TRUE))
  })
  observe({
    shinyjs::onclick("toggleAdvancedPCA",
                     shinyjs::toggle(id = "advanced", anim = TRUE))
  })
  
}

# Server function -------------------------------------------------------------------------------------------------
function(input, output, session) {
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
    })


    # * Metadata ------------------------------------------------------------------------------------------------------
    ### Preparation of Metadatatable
    # clean data
    observeEvent(input$refresh, {
        # Note: The following lines are identical to the ones concerning cleandata in
        #       global.R
        # They are... so wtf is the redundancy for?
        sql_data <- paste("SELECT * FROM id_info")
        rawmetadata <-
            collect(tbl(database_connection, sql(sql_data)))
        cleandata <- rawmetadata

        # Some formatting of the columns that contain temporal information
        cleandata$date_upload <-
            as.Date(cleandata$date_upload, format = '%y%m%d')
        cleandata$date_sample <-
            as.Date(cleandata$date_sample, format = '%y%m%d')
        cleandata$date_extraction <-
            as.Date(cleandata$date_extraction, format = '%y%m%d')
        cleandata$date_measured <-
            as.Date(cleandata$date_measured, format = '%y%m%d')
        cleandata <<- cleandata[order(cleandata$id), ]

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
                cleandata[c("id",
                            "title",
                            "date_upload",
                            "status",
                            "sample_from")]
            },
            extensions = 'Buttons',
            options = list(
                orderClasses = TRUE,
                pageLength = 10,
                paging = FALSE,
                searching = TRUE,
                dom = 'C<"clear">lfrtip',
                buttons = list(list(
                    extend = 'colvis', exclude = c(0, 1)
                )),
                order = list(0, 'desc'),
                deferRender = TRUE,
                scrollY = 500,
                scrollCollapse = TRUE
            ),
            rownames = FALSE,
            selection = 'single'
        )
    })

    #Updating
    observeEvent(input$refresh, {
        output$metadataTable <- DT::renderDataTable({
            datatable(
                if (input$fulltable) {
                    cleandata
                } else {
                    cleandata[c("id",
                                "title",
                                "date_upload",
                                "status",
                                "sample_from")]
                },
                extensions = 'Buttons',
                options = list(
                    orderClasses = TRUE,
                    pageLength = 10,
                    paging = FALSE,
                    searching = TRUE,
                    dom = 'C<"clear">lfrtip',
                    buttons = list(list(
                        extend = 'colvis', exclude = c(0, 1)
                    )),
                    order = list(0, 'desc')
                ),
                rownames = FALSE
            )
        })
    })



    ### Select plotting choices (var:plotchoices)
    # Choices: "Plot Values", "Errorbar (Mean +/- 1SD)", "Nr of Datapoints", "Mean Values",
    #          "Show Legend", "Ttest", "Median"
    observe({
        if (input$plottype == 'barplot') {
            updateCheckboxGroupInput(session,
                                     "checkGroup",
                                     choices = plotchoices,
                                     selected = plotchoices[c("Show Legend")])
        } else if (input$plottype == 'boxplot') {
            updateCheckboxGroupInput(session,
                                     "checkGroup",
                                     choices = plotchoices[c("Nr of Datapoints",
                                                             "Show Legend",
                                                             "Log10-Transformation")],
                                     selected = plotchoices[c("Show Legend")])
        } else if (input$plottype == 'rawplot') {
            updateCheckboxGroupInput(session,
                                     "checkGroup",
                                     choices = plotchoices[c(
                                         "Plot Values",
                                         "Errorbar (Mean +/- 1SD)",
                                         "Nr of Datapoints",
                                         "Show Legend",
                                         "Median",
                                         "Identify Individuals",
                                         "Log10-Transformation"
                                     )],
                                     selected = plotchoices[c(
                                         "Plot Values",
                                         "Errorbar (Mean +/- 1SD)",
                                         "Errorbar (Mean +/- 1SE)",
                                         "Show Legend",
                                         "Log10-Transformation"
                                     )])
        } else if (input$plottype == "pointrange") {
            updateCheckboxGroupInput(session,
                                     "checkGroup",
                                     choices = plotchoices[c("Nr of Datapoints",
                                                             "Show Legend",
                                                             "Log10-Transformation")],
                                     selected = plotchoices[c("Show Legend")])
        }
    })



    #** weird section about metadata ---------------------------------------------------------------------------------------------------
    observeEvent(input$metadataTable_row_last_clicked, {
        #*****************************************
        # I do no know if I need all this until the next Stars...
        sql_data <- paste("SELECT * FROM id_info")
        rawmetadata <-
            collect(tbl(database_connection, sql(sql_data)))
        cleandata <- rawmetadata
        # Some formatting of the columns that contain temporal information
        cleandata$date_upload <-
            as.Date(cleandata$date_upload, format = '%y%m%d')
        cleandata$date_sample <-
            as.Date(cleandata$date_sample, format = '%y%m%d')
        cleandata$date_extraction <-
            as.Date(cleandata$date_extraction, format = '%y%m%d')
        cleandata$date_measured <-
            as.Date(cleandata$date_measured, format = '%y%m%d')
        cleandata <<- cleandata[order(cleandata$id), ]

        # Update Names and Tables
        datanames <- as.list(cleandata$id)
        names(datanames) <- paste(cleandata$id, cleandata$title)
        datanames <<- datanames
        #*****************************************

        clicked <- as.integer(input$metadataTable_row_last_clicked)
        vectorOf_datanames <- as.vector(unlist(datanames))
        indexFor_datanames <- match(clicked, vectorOf_datanames)
        indexFor_datanames <- clicked # Possible Fix as of Febr 2018
        updateSelectInput(session,
                          "ID",
                          choices = datanames,
                          selected = datanames[indexFor_datanames])
    })

    # selecting data to plot ------------------------------------------------------------------------------------------
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
        updateSelectInput(
            session,
            "whatsub",
            choices = levels(choicewhat),
            label = paste("Which", revwhatnames[input$what])
        )

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
        if (input$what %in% c("chains", "chain_sums") &
            input$within == "Class") {
            updateSelectInput(
                session,
                "withsub",
                choices = levels(choicewith),
                selected = levels(choicewith)[1]
            )
        } else {
            updateSelectInput(session, "withsub", choices = levels(choicewith))

        }
    })
    ### Select replicate sub to plot and the same for technical replicates
    observe({
        input$ID
        choice <- rawdata()
        choicesam <- as.factor(choice$sample_replicate)
        choicetech <- as.factor(choice$sample_replicate_technical)
        updateSelectInput(session, "repsub", choices = levels(choicesam))
        updateSelectInput(session, "techsub", choices = levels(choicetech))
    })
    ### If Species/Sum Species selected update to summarized: Class and select one
    observe({
        if (input$what %in% c("chains", "chain_sums")) {
            updateSelectInput(session,
                              "within",
                              choices = withinnames,
                              selected = "Class")
            choice <- rawdata()
            choicewith <- as.factor(choice$class)
            updateSelectInput(
                session,
                "withsub",
                choices = levels(choicewith),
                selected = levels(choicewith)[1]
            )
        }
    })

    observe({
        if (input$what %in% c("class", "category", "func_cat")) {
            updateSelectInput(session,
                              "within",
                              choices = withinnames,
                              selected = "Sample")
        }
    })


    # advanced options-list -------------------------------------------------------------------------------------------
    advplot <-
        reactive({
        res <- c(input$symbolchoice)
        if (input$add_rem) {
            res <- c(res, "add")
        }
        if (input$highlight) {
            res <- c(res, "highlight")
        }
        return(res)
    })


    # Creating main plot ----------------------------------------------------------------------------------------------
    ### Create Plot
    PlotData <- reactive({
        collect(
            prepareData(
                rawdata(),
                what = input$what,
                within = input$within,
                standard = input$standard,
                ID = input$ID,
                standardSubset = input$stdSub,
                add = input$add_rem,
                samplesub = input$samplesub,
                withsub = input$withsub,
                whatsub = input$whatsub,
                repsub = input$repsub,
                techsub = input$techsub
            )
        )

    })

    SubPlotData <- reactive({
        prepareSubset(
            PlotData(),
            advanced = advplot(),
            samplesub = input$samplesub,
            whatsub = input$whatsub,
            withsub = input$withsub,
            within = input$within,
            repsub = input$repsub,
            base = input$samplebase
        )
    })



    # PCA calculations ------------------------------------------------------------------------------------------------
    PCA.results <- reactive({
        print("started new PCA Results")
        #adapted from another devium
        pca.inputs <- list()
        pca_df <- SubPlotData()
        pca_df <-
            pca_df[c('sample',
                     'sample_replicate',
                     'xval',
                     'standardizedSum')] %>%
            spread(key = 'xval', value = 'standardizedSum')

        if (length(input$samplebase) != 0) {
            unchanged <- data.frame(pca_df[, -c(1)], row.names = 1)
            print(unchanged)
            start.data <- data.frame(pca_df[, -c(1)], row.names = 1)
        } else {
            start.data <- data.frame(pca_df[, -c(1)], row.names = 1)
        }

        pca.inputs$pca.data <- start.data
        pca.inputs$pca.algorithm <- input$method
        pca.inputs$pca.components <- input$PCs
        pca.inputs$pca.center <- input$center
        pca.inputs$pca.scaling <- input$scaling
        pca.inputs$pca.cv <- input$cv # currently not used
        pca.inputs$pca.groups <- pca_df[[1]]

        devium.pca.calculate(pca.inputs, return = "list", plot = F)
    })


    # Main Plot zoom --------------------------------------------------------------------------------------------------
    plotInput <- reactive({
        print("started plot Input")
        plt_main <-
            preparePlots(
                SubPlotData(),
                checkGroup = input$checkGroup,
                plottype = input$plottype,
                what = input$what,
                within = input$within,
                standard = input$standard,
                advanced = advplot()
            )
        print(ranges$x)
        print(ranges$y)
        print("ended")
        plt_main  + coord_cartesian(xlim = ranges$x, ylim = ranges$y)
    })


    # Heatmap ---------------------------------------------------------------------------------------------------------
    heatmap_df <- reactive({
        SubPlotData()[c('sample',
                        'sample_replicate',
                        'xval',
                        'standardizedSum')] %>%
            group_by(sample, xval) %>% summarise(value = mean(standardizedSum))
    })

    plotScores <- reactive({
        if (is.null(PCA.results())) {
            return(NULL)
        } else {
            tmp <- PCA.results()
            scores <- data.frame(tmp$pca.scores)
            if (nrow(tmp$pca.diagnostics) == nrow(scores))
            {
                if (any(tmp$pca.diagnostics$DmodX == "NaN")) {
                    tmp$pca.diagnostics$DmodX <- 1
                }
                scores <-
                    data.frame(
                        leverage = tmp$pca.diagnostics$leverage,
                        dmodx = tmp$pca.diagnostics$DmodX,
                        scores
                    )
            } else {
                scores <- data.frame(leverage = 1,
                                     dmodx = 1,
                                     scores)
            }

            p <-
                ggplot(scores,
                       mapping = aes_string(
                           x = names(scores)[3],
                           y = names(scores)[4],
                           color = "samples"
                       ))
            p <-
                p + scale_size_continuous("DmodX", range = c(4, 10))
            p <-
                p + geom_point(
                    alpha = 0.75,
                    aes(
                        colour = tmp$pca.groups,
                        group = tmp$pca.groups
                    ),
                    size = input$size
                )
            if (input$labels) {
                p <-
                    p + geom_text(
                        aes(
                            label = row.names(tmp$pca.scores),
                            color = tmp$pca.groups
                        ),
                        size = 3,
                        nudge_y = 0.3
                    )
            }
            p <- p + .theme
            p
        }
    })

    plotLoadings <- reactive({
        if (is.null(PCA.results())) {
            return(NULL)
        } else {
            tmp <- PCA.results()
            loadings <-
                data.frame(tmp$pca.loadings, names = rownames(tmp$pca.loadings))

            #plot
            p <-
                ggplot(loadings,
                       mapping = aes_string(
                           x = names(loadings)[1],
                           y = names(loadings)[2],
                           label = "names"
                       )) +
                geom_text(size = 4, alpha = 0.75) + .theme
            # print(p)
        }
    })

    plotScree1 <- reactive({
        if (is.null(PCA.results())) {
            return(NULL)
        } else {
            x <- PCA.results()
            eigenvalues <- data.frame(x$pca.eigenvalues)
            tmp <-
                data.frame(melt(eigenvalues$eigenvalue), PCs = rep(1:nrow(eigenvalues)))
            tmp$value <- tmp$value * 100
            p1 <-
                ggplot(tmp, aes(y = value, x = as.factor(PCs))) + geom_bar(fill = "gray",
                                                                           stat = "identity",
                                                                           position = position_dodge()) +
                .theme + geom_hline(yintercept = 1, linetype = 2) + ylab("% variance explained") + xlab("Principal Component")
        }
        return(p1)
    })

    plotScree2 <- reactive({
        if (is.null(PCA.results())) {
            return(NULL)
        } else {
            x <- PCA.results()
            eigenvalues <- data.frame(x$pca.eigenvalues)

            #cumulative
            eigenvalues$eigenvalues <- cumsum(eigenvalues$eigenvalues)
            tmp <-
                data.frame(melt(eigenvalues), PCs = rep(1:nrow(eigenvalues)))
            p2 <-
                ggplot(tmp, aes(
                    y = value,
                    x = as.factor(PCs),
                    fill = variable
                )) +
                geom_bar(stat = "identity", position = position_dodge()) +
                .theme +
                geom_hline(yintercept = .8, linetype = 2) +
                xlab("Principal Component")
        }
        return(p2)
    })

    plotPCA <- reactive({
        # A and B are only used for debugging purposes TODO: Delete later
        print("PCA started")
        A <-
            SubPlotData()[c('sample_replicate', 'xval', 'standardizedSum')] %>%
            spread(B, key = 'xval', value = 'standardizedSum')
        A <- data.frame(A[, -1])
        pc.data <- prcomp(A, scale = T, center = T)
        print("prcomp complete")
        g <- ggbiplot(
            pc.data,
            obs.scale = 1,
            var.scale = 1,
            groups = A['xval'],
            ellipse = TRUE,
            circle = T,
            pc.biplot = F,
            var.axes = T,
            alpha = 1
        )
        g
        print("PCA ended")

    })
    output$plot1 <- renderPlot({
        plotInput()
    })

    hmap <- reactive({
        heatmap_df() %>%
            ggplot() +
            aes(x = xval,
                y = sample,
                fill = value) +
            geom_raster() +

            .theme +
            theme(
                axis.text.x = element_text(angle = 45, hjust = 1),
                axis.text = element_text(size = input$heatlabsz, colour = "black"),
                axis.title.y = element_text(size = 20),
                axis.title.x = element_blank(),
                plot.background = element_blank(),
                panel.grid = element_blank()
            ) +
            scale_x_discrete(expand = c(0, 0)) +
            scale_y_discrete(expand = c(0, 0)) +
            labs(x = "species") +
            scale_fill_viridis(option = input$hmap_color) +
            NULL
    })

    output$heatmap <- renderPlot({
        hmap()
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

    output$sum_muMol <- DT::renderDataTable({
        outtab <- collect(rawdata())
        # Averaging over the technical replicates
        outtab <- outtab %>%
            group_by(
                id,
                lipid,
                category,
                func_cat,
                class,
                length,
                db,
                oh,
                chains,
                chain_sums,
                sample,
                sample_replicate
            ) %>%
            summarize(value = mean(value, na.rm = T))
        # Summing the relevant parts
        outtab <- outtab %>% group_by(sample, sample_replicate) %>%
            summarise(sum = sum(value, na.rm = T))
        names(outtab) <- c("Sample", "Sample Replicate", "µM")
        datatable(
            outtab,
            options = list(
                orderClasses = TRUE,
                pageLength = 10,
                paging = FALSE,
                searching = TRUE,
                dom = 'C<"clear">lfrtip',
                order = list(0, 'desc'),
                deferRender = TRUE,
                scrollY = 500,
                scrollCollapse = TRUE
            ),
            rownames = FALSE,
            selection = "none"
        )
    })


    #** Saving data and plots -----------------------------------------------------------------------------------------------------

    # Saving a dataset as a .RData
    output$downdata <- downloadHandler(
        filename = function() {
            paste0(input$downid, ".RData")
        },
        content = function(file) {
            dataset <- collect(getraw(input$downid, "data2"))
            save(dataset, file = file)
        }
    )

    # Saving the current plot as a pdf
    output$saveplot <- downloadHandler(
        filename = function() {
            paste0(input$ID, "_", Sys.Date(), ".pdf")
        },
        content = function(file) {
            ggsave(
                file,
                plotInput(),
                device = "pdf",
                width = 20,
                height = 10
            )
        }
    )


    #Saving heatmap
    output$saveheatmap <- downloadHandler(
        filename = function() {
            paste0(input$ID, "_", Sys.Date(), "_heatmap.pdf")
        },
        content = function(file) {
            ggsave(
                file,
                hmap(),
                device = "pdf",
                width = input$widthheat,
                height = input$heightheat
            )
        }
    )
    output$saveScores <- downloadHandler(
        filename = function() {
            paste0(input$ID, "_", Sys.Date(), "_Scores.pdf")
        },
        content = function(file) {
            pdf(file,
                height = input$heightPCA,
                width = input$widthPCA)
            print(plotScores())
            dev.off()
        }
    )
    output$saveLoadings <- downloadHandler(
        filename = function() {
            paste0(input$ID, "_", Sys.Date(), "_Loadings.pdf")
        },
        content = function(file) {
            pdf(file,
                height = input$heightPCA,
                width = input$widthPCA)
            print(plotLoadings())
            dev.off()
        }
    )
    output$savescree1 <- downloadHandler(
        filename = function() {
            paste0(input$ID, "_", Sys.Date(), "_Scree.pdf")
        },
        content = function(file) {
            pdf(file,
                height = input$heightPCA,
                width = input$widthPCA)
            print(plotScree1())
            dev.off()
        }
    )
    output$savescree2 <- downloadHandler(
        filename = function() {
            paste0(input$ID, "_", Sys.Date(), "_Scree.pdf")
        },
        content = function(file) {
            pdf(file,
                height = input$heightPCA,
                width = input$widthPCA)
            print(plotScree2())
            dev.off()
        }
    )
    # Save the corresponding ggplot data for further computations
    output$saveplotR <- downloadHandler(
        filename = function() {
            paste0(input$ID, "_", Sys.Date(), ".RData")
        },
        content = function(file) {
            lipidplot <- plotInput()
            save(lipidplot, lipidsubset, file = file)
        }
    )
    # General plot saving
    output$save <- downloadHandler(
        filename = function() {
            paste0(input$ID, "_", Sys.Date(), ".pdf")
        },
        content = function(file) {
            ggsave(
                file,
                plotInput(),
                device = "pdf",
                width = input$width,
                height = input$height
            )
        }
    )

    ### saves the current selection of metadata columns in an csv file
    output$savemeta <- downloadHandler(
        filename = function() {
            paste0('metadatatable.csv')
        },
        content = function(file) {
            write.csv(
                cleandata,
                row.names = FALSE,
                col.names = FALSE,
                file = file
            )
        }
    )
    # Save standardized Data as a File
    output$savestddata <- downloadHandler(
        filename = function() {
            paste0(input$ID, "_", Sys.Date(), ".csv")
        },
        content = function(file) {
            write.csv(collect(tidySave),
                      row.names = FALSE,
                      file = file)
        }
    )

    # *** create the standardisation possibilities --------------------------------------------------------------------
    observe({
        if (input$what != "class")
        {
            if (input$within == "Sample")
            {
                updateSelectInput(session, "standard", choices = "Sample")
            }
            else if (input$within == "Category")
            {
                updateSelectInput(
                    session,
                    "standard",
                    choices = c("Sample", "Category",
                                "Functional Category")
                )
            }
            else if (input$within == "Functional Category")
            {
                updateSelectInput(
                    session,
                    "standard",
                    choices = c("Sample", "Category",
                                "Functional Category")
                )
            }
            else if (input$within == "Class")
            {
                updateSelectInput(
                    session,
                    "standard",
                    choices = c(
                        "Sample",
                        "Category",
                        "Functional Category",
                        "Class"
                    )
                )
            }
        } else {
            updateSelectInput(
                session,
                "standard",
                choices = c("Sample", "Category",
                            "Functional Category")
            )
        }
    })


    # ShinyJS stuff ---------------------------------------------------------------------------------------------------
    # advanced saving options
    observe({
        shinyjs::onclick("toggleAdvanced",
                         shinyjs::toggle(id = "advanced", anim = TRUE))
    })
    observe({
        shinyjs::onclick("toggleAdvancedPCA",
                         shinyjs::toggle(id = "advanced", anim = TRUE))
    })


    # End -------------------------------------------------------------------------------------------------------------
    # End session when window is closed
    session$onSessionEnded(stopApp)
}

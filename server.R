# Server function -------------------------------------------------------------------------------------------------
function(input, output, session) {

    # Metadata / Datasets ---------------------------------------------------------------------------------------------

    ## Debugging code for a reactive database_connection set from the UI
    # database_connection <- reactive({
    #     req(input$database_connection)
    #     src_sqlite(input$database_connection["datapath"] %>% as.character())
    # })

    # Reading in table of datasets
    metaData <- reactive({
        meta <- collect(tbl(database_connection, sql(sqlQueryMeta))) %>%
            mutate(
                date_upload = as.Date(date_upload, format = "%y%m%d"),
                date_sample = as.Date(date_sample, format = "%y%m%d"),
                date_extraction = as.Date(date_extraction, format = "%y%m%d"),
                date_measured = as.Date(date_measured, format = "%y%m%d")
            ) %>%
            arrange(id)
        return(meta)
    })

    # Rendering datasets as a table to send to UI
    output$metaDataTable <- DT::renderDT({
        req(metaData())

        if (input$showFullMeta == TRUE){
            meta <- metaData()
        } else{
            meta <- metaData()[c("id","title", "date_upload", "status", "sample_from")]
        }
        meta
    },
    server = FALSE, selection = list(mode = 'single', selected = 1),
    options = list(orderClasses = TRUE,
                   pageLength = 10,
                   order = list(0, 'desc'),
                   scrollX = TRUE,
                   deferRender = TRUE,
                   scrollY = 500,
                   scrollCollapse = TRUE)
    )

    ## Update SelectInput for datasets based on sets loaded and row selected
    # select clicked row in table
    observe({
        choices <- metaData()$id
        names(choices) <- metaData()$title
        selection <- input$metaDataTable_rows_selected
        updateSelectInput(session, "ID",
                          choices = choices,
                          selected = choices[selection]
        )
    }, label = "updatingDataSelect")

    # Data ------------------------------------------------------------------------------------------------------------

    # * Reading in raw data based on dataset selected -----------------------------------------------------------------

    rawData <- reactive({
        # Only runs if a dataset is selected
        validate(
            need(input$ID, "Please select a dataset first.")
        )

        query <- sqlQueryData(input$ID)
        raw <- collect(tbl(database_connection, sql(query)))
        df <- raw %>%
            filter(!is.na(value))
        df <- df %>%
            mutate(
                sample_identifier = factor(sample_identifier),
                lipid = factor(lipid),
                func_cat = factor(func_cat),
                class = forcats::fct_relevel(class, input$custom_class_order_order),
                category = factor(category),
                sample = factor(sample),
                sample_replicate = factor(sample_replicate),
                sample_replicate_technical = factor(sample_replicate_technical),
                oh = if_else(is.na(oh), 0, oh)
            ) %>%
            select(id, sample_identifier, lipid, value, everything())
        df %>%
            return()
    })


    # * mainData from rawData -------------------------------------------------------------------------
    # standardization, then filtering

    mainData <- reactive({
        req(rawData())

        # Temporary dataframe in the scope of this function
        df <- rawData()

        # Standardization based on input$std_feature
        if(input$std_feature != ""){
            df <- df %>% group_by(!!sym(input$std_feature)) %>%
                mutate(
                    value = value / sum(value) * 100
                ) %>%
                ungroup()
        }

        # Base level substraction
        if(input$base_sample != ""){
            baseline <- df %>%
                filter(sample == input$base_sample) %>%
                group_by(lipid) %>%
                summarize(baseline = mean(value, na.rm = TRUE))
            df <- df %>% left_join(baseline) %>%
                mutate(
                    baseline = if_else(is.na(baseline),0,baseline)
                ) %>%
                mutate(
                    value = value - baseline
                ) %>%
                ungroup()
        }


        # Filtering
        # Category
        if(!is.null(input$filter_cat)){
            df <- df %>% filter(category %in% input$filter_cat)
        }
        # Class
        if(!is.null(input$filter_class)){
            df <- df %>% filter(class %in% input$filter_class)
        }
        # Functional category
        if(!is.null(input$filter_func)){
            df <- df %>% filter(func_cat %in% input$filter_func)
        }
        # Total length of sidechains
        if(!is.null(input$filter_length)){
            df <- df %>% filter(length %>% between(input$filter_length[1], input$filter_length[2]))
        }
        # Total number of double bounds
        if(!is.null(input$filter_db)){
            df <- df %>% filter(db %>% between(input$filter_db[1], input$filter_db[2]))
        }
        # Total number of hydroxyl groups
        if(!is.null(input$filter_oh)){
            df <- df %>% filter(oh %>% between(input$filter_oh[1], input$filter_oh[2]))
        }
        # explicitly demanding sample
        if(!is.null(input$sample_select)){
            df <- df %>% filter(sample %in% input$sample_select)
        }
        # removing sample
        if(!is.null(input$sample_remove)){
            df <- df %>% filter(!(sample %in% input$sample_remove))
        }
        # demanding replicate
        if(!is.null(input$rep_select)){
            df <- df %>% filter(sample_replicate %in% input$rep_select)
        }
        # removing replicate
        if(!is.null(input$rep_remove)){
            df <- df %>% filter(!(sample_replicate %in% input$rep_remove))
        }
        # removing technical replicate
        if(!is.null(input$tecRep_remove)){
            df <- df %>% filter(!(sample_replicate_technical %in% input$tecRep_remove))
        }

        df %>%
            return()
    })


    # Download handlers  --------------------------------------------------------------

    # * metadata and raw datasets ------------------------------------------------------------------------------------
    # Metadata - .csv
    output$saveMeta <- downloadHandler(
        filename = function() {
            paste0("datasets_info.csv")
        },
        content = function(file) {
            write_csv(x = metaData(), path = file)
        }
    )

    # Raw Main Data - .csv
    output$saveRawCSV <- downloadHandler(
        filename = function() {
            tmp <- metaData() %>% filter(id == input$ID) %>% select(title)
            tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
            paste0(Sys.Date(), "_", tmp, "-raw" ,".csv")
        },
        content = function(file) {
            write_csv(x = rawData(), path = file)
        }
    )

    #  Main Data - .csv
    output$saveMainCSV <- downloadHandler(
        filename = function() {
            tmp <- metaData() %>% filter(id == input$ID) %>% select(title)
            tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
            paste0(Sys.Date(), "_", tmp, "-filtered" ,".csv")
        },
        content = function(file) {
            write_csv(x = mainData(), path = file)
        }
    )


    # * Plot Data -----------------------------------------------------------------------------------------------------
    #  Plot Data - .csv
    output$main_saveData <- downloadHandler(
        filename = function() {
            tmp <- metaData() %>% filter(id == input$ID) %>% select(title)
            tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
            paste0(Sys.Date(), "_", tmp, "-plot" ,".csv")
        },
        content = function(file) {
            write_csv(x = plotData(), path = file)
        }
    )

    #  Plot mean Data (bars) - .csv
    output$main_saveMeans <- downloadHandler(
        filename = function() {
            tmp <- metaData() %>% filter(id == input$ID) %>% select(title)
            tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
            paste0(Sys.Date(), "_", tmp, "-means" ,".csv")
        },
        content = function(file) {
            write_csv(x = meanPlotData() %>% rename(mean = value), path = file)
        }
    )


    # * Plots ---------------------------------------------------------------------------------------------------------

    # Main Plot
    output$main_savePlot <- downloadHandler(
        filename = function() {
            tmp <- metaData() %>% filter(id == input$ID) %>% select(title)
            tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
            paste0(Sys.Date(), "_", tmp, "-plot" ,".pdf")
        },
        content = function(file) {
            ggsave(file, plot = mainPlt(),
                   width = input$mainWidth, height = input$mainHeight)
        }
    )

    # Heatmapt
    output$heatSave <- downloadHandler(
        filename = function() {
            tmp <- metaData() %>% filter(id == input$ID) %>% select(title)
            tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
            paste0(Sys.Date(), "_", tmp, "-heatmap" ,".pdf")
        },
        content = function(file) {
            ggsave(file, plot = heatPlt(),
                   width = input$heatWidth, height = input$heatHeight)
        }
    )

    # PCA
    output$pca_saveScores <- downloadHandler(
        filename = function() {
            tmp <- metaData() %>% filter(id == input$ID) %>% select(title)
            tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
            paste0(Sys.Date(), "_", tmp, "-PCA_scores" ,".pdf")
        },
        content = function(file) {
            ggsave(file, plot = pca_ScoresPlt(),
                   width = input$pca_Width, height = input$pca_Height)
        }
    )

    output$pca_saveLoadings <- downloadHandler(
        filename = function() {
            tmp <- metaData() %>% filter(id == input$ID) %>% select(title)
            tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
            paste0(Sys.Date(), "_", tmp, "-PCA_loadings" ,".pdf")
        },
        content = function(file) {
            ggsave(file, plot = pca_LoadingsPlt(),
                   width = input$pca_Width, height = input$pca_Height)
        }
    )

    # UMAP
    output$umapSave <- downloadHandler(
        filename = function() {
            tmp <- metaData() %>% filter(id == input$ID) %>% select(title)
            tmp <- as.character(tmp) %>% gsub("[[:space:]]", "_", .)
            paste0(Sys.Date(), "_", tmp, "-UMAP" ,".pdf")
        },
        content = function(file) {
            ggsave(file, plot = umap_plt(),
                   width = input$umapWidth, height = input$umapHeight)
        }
    )


    # Updating filtering options by dataset --------------------------------------------------------

    observe({
        choices <- rawData()$sample %>%
            unique()
        updateSelectizeInput(session, "sample_select",
                             choices = choices
        )
        updateSelectizeInput(session, "base_sample",
                             choices = choices,
                             selected = ""
        )
        sample_IDs <- rawData()$sample_identifier %>% unique()
        updateSelectizeInput(session, "sample_remove",
                             choices = choices
        )
        choices <- rawData()$sample_replicate %>%
            unique()
        updateSelectizeInput(session, "rep_select",
                             choices = choices
        )
        updateSelectizeInput(session, "rep_remove",
                             choices = choices
        )
        choices <- rawData()$sample_replicate_technical %>%
            unique()
        updateSelectizeInput(session, "tecRep_remove",
                             choices = choices
        )
        choices <- rawData()$category %>%
            unique()
        updateSelectizeInput(session, "filter_cat",
                             choices = choices
        )
        choices <- rawData()$func_cat %>%
            unique()
        updateSelectizeInput(session, "filter_func",
                             choices = choices
        )
        choices <- rawData()$class %>%
            unique()
        updateSelectizeInput(session, "filter_class",
                             choices = choices
        )
        ls <- rawData()$length %>%
            range(na.rm = TRUE)
        updateSliderInput(session, "filter_length",
                          min = ls[1], max = ls[2],
                          value = c(ls[1], ls[2])
        )
        dbs <- rawData()$db %>%
            range(na.rm = TRUE)
        updateSliderInput(session, "filter_db",
                          min = dbs[1], max = dbs[2],
                          value = c(dbs[1], dbs[2])
        )
        ohs <- rawData()$oh %>%
            range(na.rm = TRUE)
        updateSliderInput(session, "filter_oh",
                          min = 0, max = ohs[2],
                          value = c(ohs[1], ohs[2])
        )
    })

    # Updating selectizeOptions of samples based dataset and sample_remove based on selected samples
    observe({
        if(!is.null(input$sample_select)){
            updateSelectizeInput(session,
                                 "sample_remove",
                                 choices = input$sample_select
            )
        }
        if (is.null(input$sample_select)){
            updateSelectizeInput(session,
                                 "sample_remove",
                                 choices =  unique(rawData()$sample)
            )
        }
    })

    # Displaying main Dataset as a table ----------------------------------------------------------------------------

    # Rendering selected dataset as a table to send to UI
    output$mainDataTable <- DT::renderDT({
        mainData()
    },
    filter = 'none',
    rownames = FALSE,
    options = list(orderClasses = TRUE,
                   pageLength = 10,
                   order = list(0, 'desc'),
                   scrollX = TRUE,
                   deferRender = TRUE,
                   scrollCollapse = TRUE)
    )



    # plotData from mainData based on sidebar inputs ---------------------------------------------------------------

    # with apropriate summarize functions based on selecte plot type, standards and aes
    plotData <- reactive({
        req(mainData())
        df <- mainData()

        # Validations, friendly error messages
        validate(
            need( !(input$tecRep_average & ( input$aes_color == "sample_replicate_technical" |
                                                 input$aes_x == "sample_replicate_technical" |
                                                 input$aes_y == "sample_replicate_technical" |
                                                 input$aes_facet1 == "sample_replicate_technical" |
                                                 input$aes_facet2 == "sample_replicate_technical"  )
            ),
            "You are currently averaging over technical replicates (see the samples tab in the sidebar)
                  and thus can't use this feature in your plots."
            ),
            need( input$aes_x != "",
                  "Please select a feature to display on the x-axis"),
            need( input$aes_y != "",
                  "Please select a feature to display on the y-axis")
        )

        # Averaging over the technical replicates
        if (input$tecRep_average){
            df <- df %>%
                group_by_at(vars(-sample_identifier,-sample_replicate_technical,-value)) %>%
                summarize(value = mean(value, na.rm = T)) %>%
                ungroup()
        }

        # Filter any NA in features used for aesthetics (x-axis, y-axis, color, facet1, facet2)
        df <- df %>% filter(!is.na(!!sym(input$aes_x)),
                            !is.na(!!sym(input$aes_y)))
        if (input$aes_color != ""){
            df <- df %>% filter(!is.na(!!sym(input$aes_color)))
        }
        if (input$aes_facet1 != ""){
            df <- df %>% filter(!is.na(!!sym(input$aes_facet1)))
        }
        if (input$aes_facet2 != ""){
            df <- df %>% filter(!is.na(!!sym(input$aes_facet2)))
        }

        # Summation of values within the displayed aesthetics
        # TODO show individual tec. sample reps.
        # By features mapped to aesthetics, always by sample rep
        df <- df %>% ungroup()
        if (input$aes_x != ""){
            df <- df %>% group_by(!!sym(input$aes_x), add = TRUE)
        }
        if (input$aes_color != ""){
            df <- df %>% group_by(!!sym(input$aes_color), add = TRUE)
        }
        if (input$aes_facet1 != ""){
            df <- df %>% group_by(!!sym(input$aes_facet1), add = TRUE)
        }
        if (input$aes_facet2 != ""){
            df <- df %>% group_by(!!sym(input$aes_facet2), add = TRUE)
        }
        if (input$tecRep_average){
            df <- df %>% group_by(sample_replicate, add = TRUE)
        } else {
            df <- df %>% group_by(sample_replicate_technical, add = TRUE)
        }

        # Sums for each group
        df <- df %>% summarize(
            value = sum(value, na.rm = TRUE)
        )
        # This will remove only the last layer of grouping (sample_replicate or sample_replicate_technical)
        # and keep the other groups, in either case, ase_x will still be a group
        cat(file=stderr(), "\n plotData still grouped by", paste(groups(df), collapse = ", "), "\n")

        df
    })


    # meanPlotData for bars/averages ----------------------------------------------------------------------------------

    meanPlotData <- reactive({
        req(plotData())
        df <- plotData()

        df <- df %>% summarize(
            SD = sd(value, na.rm = TRUE),
            SEM = sd(value, na.rm = TRUE)/n(),
            N = n(),
            value = mean(value, na.rm = TRUE)
            # CI_lower = value - qt(1 - (0.05 / 2), N - 1) * SEM,
            # CI_upper = value + qt(1 - (0.05 / 2), N - 1) * SEM
        )# %>%
        # Assumption: we are 100% sure that no lipid has a value smaller than 0
        # mutate(
        #     CI_lower = if_else(CI_lower < 0, 0,CI_lower)
        # )

        cat(file=stderr(), "\n meanPlotData still grouped by", paste(groups(df), collapse = ", "), "\n")

        df
    })


    # Main Plot output ------------------------------------------------------------------------------------------------

    # Ranges for zooming by clicking on the plot
    ranges <- reactiveValues(x = NULL, y = NULL)
    observeEvent(input$mainPlot_dblclick, {
        brush <- input$mainPlot_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
    })


    # * Plot Object ----------------------------------------------------------------------------------------
    mainPlt <- reactive({
        req(plotData())
        req(meanPlotData())
        # temporary dataframe inside this function
        df <- plotData()

        # basic plot object
        plt <- df %>%
            ggplot()

        # main plot definition
        plt <- plt +
            aes(x = !!sym(input$aes_x),
                y = !!sym(input$aes_y)
            )

        # add color/fill if requested
        if(input$aes_color != ""){
            # number of colors needed, if any
            colorCount <- df[,input$aes_color] %>% unique() %>% as_vector() %>% length()

            plt <- plt +
                aes(color = factor(!!sym(input$aes_color)),
                    fill = factor(!!sym(input$aes_color))
                )
        }

        # Add bars
        if ("bars" %in% input$main_add){
            plt <- plt +
                geom_col(data = meanPlotData(), position = position_dodge2(width = 0.9))
        }

        # Add points
        if ("points" %in% input$main_add){
            plt <- plt +
                geom_point(position = position_dodge(width = 0.9), pch = 21, alpha = 1,
                           color = "black", show.legend = F)
        }

        # Error bars and mean
        if (input$main_error != "None"){
            plt <- plt +
                geom_errorbar(
                    data = meanPlotData(), position = position_dodge2(width = 0.2, padding = 0.8),
                    aes(ymin = switch(input$main_error,
                                      "SD" = value - SD,
                                      "SEM" = value - SEM
                                      #"CI" = CI_lower
                    ), ymax = switch(input$main_error,
                                     "SD" = value + SD,
                                     "SEM" = value + SEM
                                     #"CI" = CI_upper
                    )
                    ), alpha = .8
                )
        }

        # Hightlight Average
        if ("mean" %in% input$main_add){
            plt <- plt +
                geom_errorbar(data = meanPlotData(),
                              aes(ymin = value, ymax = value),
                              position = position_dodge2(width = 0.9),
                              color = "black", size = 1.2)
        }

        # facetting
        if (input$aes_facet1 != "" | input$aes_facet2 != ""){
            facet_col <- vars(!!sym(input$aes_facet1))
            facet_row <- vars(!!sym(input$aes_facet2))

            if(input$aes_facet1 == ""){facet_col = NULL}
            if(input$aes_facet2 == ""){facet_row = NULL}

            plt <- plt+
                facet_grid(cols = facet_col,
                           rows = facet_row,
                           scales = "free_x", space = "free_x"
                )
        }

        # Display Values as text
        if ("values" %in% input$main_add){
            plt <- plt +
                geom_text(data = meanPlotData(), aes(label = round(value, 2)),
                          vjust = 0, color = "black", position = position_dodge(width = 0.9))
        }

        if ("ind_values" %in% input$main_add){
            plt <- plt +
                geom_text(aes(label = round(value, 2)),
                          vjust = 0, color = "black", position = position_dodge(width = 0.9))
        }

        # add theme and scale (defined in global.R) includes titles and formatting
        plt <- plt +
            mainTheme +
            mainScale(colorCount)+
            guides(color = guide_legend(ncol = 12,
                                        nrow = as.integer(colorCount/12)+1,
                                        title = input$aes_color),# usefull if way to many values of color
                   fill = guide_legend(ncol = 12,
                                       nrow = as.integer(colorCount/12)+1,
                                       title = input$aes_color)
            )

        if ("log" %in% input$main_add){
            if (input$std_feature != ""){
                y_name <- "amount + 1 [ Mol % ], log scale"
                y_breaks <- scales::trans_breaks("log1p", function(x) 10^x)
                y_labels <- scales::percent_format(scale = 1)
                y_trans <- "log1p"
            } else {
                y_name <- "amount + 1 [ µM ], log scale"
                y_breaks <- scales::trans_breaks("log1p", function(x) 10^x)
                y_labels <- scales::trans_format("log1p", scales::math_format(10^.x))
                y_trans <- "log1p"
            }
        } else {
            if (input$std_feature != ""){
                y_name <- "amount [ Mol % ]"
                y_breaks <- waiver()
                y_labels <- scales::percent_format(scale = 1)
                y_trans <- "identity"
            } else {
                y_name <- "amount [ µM ]"
                y_breaks <- waiver()
                y_labels <- scales::number_format()
                y_trans <- "identity"
            }
        }

        # Percent scaling if something is standardized
        plt <- plt +
            scale_y_continuous(name = y_name,
                               labels = y_labels,
                               #breaks = y_breaks,
                               trans = y_trans
            )

        # Zooming
        plt <- plt + coord_cartesian(xlim = ranges$x, ylim = ranges$y)

        # Swap X and Y
        if ("swap" %in% input$main_add){
            validate(
                need(!("log" %in% input$main_add),
                     "Swapped X and Y Axis are currently not supported for a logarithmic Y-Axis")
            )
            plt <- plt +
                coord_flip()
        }

        # return final plot
        plt
    })

    #output$mainPlot_ly <- plotly::renderPlotly(plotly::ggplotly(mainPlt()))

    # ** Plot Render --------------------------------------------------------------------------------------------
    # create actual rendered plot output from mainPlt
    output$mainPlot <- renderPlot({
        plt <- mainPlt()
        plt
    })



    # meanPlotDataTable -----------------------------------------------------------------------------------------------

    output$meanPlotDataTable <- DT::renderDT({
        req(meanPlotData())

        df <- meanPlotData()
        df %>% select(!!sym(input$aes_x),
                      sample,
                      Average = value, everything())
    },
    filter = 'none',
    rownames = FALSE,
    options = list(orderClasses = TRUE,
                   pageLength = 10,
                   order = list(0, 'desc'),
                   scrollX = TRUE,
                   deferRender = TRUE,
                   scrollCollapse = TRUE)
    )


    # Heatmap ---------------------------------------------------------------------------------------------------------

    # * Plot Object ---------------------------------------------------------------------------------------------------
    heatPlt <- reactive({
        # dataframe
        # df <- plotData()
        df <- meanPlotData()

        # plot
        plt <- ggplot(df) +
            aes(x = factor(!!sym(input$aes_x)),
                y = factor(!!sym(input$aes_color)),
                fill = !!sym(input$aes_y)) +
            geom_raster() +
            mainTheme +
            theme(
                axis.text.y = element_text(size = input$heatLabSize, colour = "black"),
                plot.background = element_blank(),
                panel.grid = element_blank(),
                panel.background = element_rect(colour = NA, fill = "grey80")
            ) +
            scale_x_discrete(expand = c(0, 0)) +
            scale_y_discrete(expand = c(0, 0)) +
            scale_fill_viridis_c(option = input$heatColor)+
            labs(y = input$aes_color)
        NULL

        # facetting
        if (input$aes_facet1 != "" & input$aes_facet2 != ""){
            plt <- plt+
                facet_grid(rows = vars(!!sym(input$aes_facet1)),
                           cols = vars(!!sym(input$aes_facet2)),
                           scales = "free"
                )
        }
        if (input$aes_facet1 != "" & input$aes_facet2 == ""){
            plt <- plt+
                facet_wrap(facets = vars(!!sym(input$aes_facet1)), scales = "free"
                )
        }
        plt
    })


    # * Plot Render ---------------------------------------------------------------------------------------------------

    output$heatPlot <-renderPlot({
        plt <- heatPlt()
        plt
    })

    # PCA -------------------------------------------------------------------------------------------------------------

    # ** Updating pca-options --------------------------------------------------------------------------------------------------
    # update nPCs, they should not exceed the dimensions of the data
    observe({
        req(pcaData())
        updateSliderInput(session,
                          "pca_nPC",
                          max = min(dim(pcaData()))
        )}
    )

    # * pcaData -------------------------------------------------------------------------------------------------------
    pcaData <- reactive({
        req(plotData())

        validate(
            need( input$aes_color == "sample", "To perform a PCA, please set color to sample in the mappings"),
            need( (input$aes_x != "sample" & input$aes_x != "sample_replicate" & input$aes_x != "sample_replicate_technical"),
                  "To perform a PCA, please select a feature other than sample as your x-axis in the mappings"),
            need( input$aes_facet1 == "", "To perform a PCA, please remove any facetting in the mappings"),
            need( input$aes_facet2 == "", "To perform a PCA, please remove any facetting in the mappings")
        )

        df <- plotData() %>% ungroup()

        df <- df %>%
            select(!!sym(ifelse(input$tecRep_average,
                                "sample_replicate",
                                "sample_replicate_technical")),
                   !!sym(input$aes_x), value)

        df <- df %>%
            spread(key = input$aes_x, value = "value") %>%
            data.frame(row.names = TRUE) %>%
            as.matrix()

        df # not a dataframe but a matrix in this case
    })

    # * pcaObject -----------------------------------------------------------------------------------------------------
    pcaObject <- reactive({
        df <- pcaData() # needs to be a numeric matrix with samples in rows, variables in columns

        # returns pcaRes object
        res <- pcaMethods::pca(
            as.matrix(df),
            method = input$pca_method,
            nPcs = input$pca_nPC,
            center = input$pca_center,
            scale = input$pca_scaling,
            cv = input$pca_cv,
            seed = 123
        )

        res
    })


    # Scaling factor for orignial data dimenision vectors in principal component space

    sample_names <- reactive({
        if (input$tecRep_average){
            res <- plotData() %>% ungroup() %>%
                select(sample, sample_replicate) %>% distinct() %>%
                mutate(sample = as.character(sample),
                       sample_replicate = as.character(sample_replicate))
        } else {
            res <- plotData() %>% ungroup() %>%
                select(sample, sample_replicate_technical) %>% distinct() %>%
                mutate(sample = as.character(sample),
                       sample_replicate_technical = as.character(sample_replicate_technical))
        }
        res
    })

    scaled_loadings <- reactive({
        req(pcaObject())
        p <- pcaObject()

        loadings <- p@loadings %>% as_tibble(rownames = input$aes_x)

        scores <- p@scores %>%
            as_tibble(rownames = if_else(input$tecRep_average, "sample_replicate", "sample_replicate_technical")) %>%
            left_join(sample_names())

        scaler <- min(max(abs(scores[, "PC1"]))/max(abs(loadings[,"PC1"])),
                      max(abs(scores[, "PC2"]))/max(abs(loadings[, "PC2"]))
        )
        loadings[, c("PC1", "PC2")] <- loadings[, c("PC1", "PC2")] * scaler * 0.8
        loadings
    })


    # * pcaOutputs ------------------------------------------------------------------------------------------------------
    # Info
    output$pca_info <- renderPrint({
        req(pcaObject())

        pca <- pcaObject()
        pca %>% summary()
    })

    # ** Scores -----------------------------------------------------------------------------------------------------
    pca_ScoresPlt <- reactive({
        req(pcaData(), pcaObject())
        p <- pcaObject()

        colorCount <- rownames(pcaData()) %>% length()
        scores <- p@scores %>%
            as_tibble(rownames = if_else(input$tecRep_average, "sample_replicate", "sample_replicate_technical")) %>%
            left_join(sample_names())

        scores$sample <- factor(scores$sample)

        plt <- scores %>%
            ggplot(aes(PC1, PC2, fill = sample))

        if (input$pca_hull){
            plt <- plt +
                stat_chull(alpha = .15, show.legend = FALSE)
        }

        plt <- plt+
            geom_point(pch = 21, alpha = 1, size = input$pca_pointSize / 2)+
            mainTheme +
            mainScale(colorCount = colorCount)

        if (input$pca_labels){
            plt <- plt +
                ggrepel::geom_text_repel(aes(label = !!sym(ifelse(input$tecRep_average,
                                                                  "sample_replicate",
                                                                  "sample_replicate_technical"))
                ), show.legend = FALSE
                )
        }

        # Add scaled orginal vectors as arrows
        if(input$pca_vectors){
            plt <- plt+
                geom_segment(data = scaled_loadings(),
                             aes(x = 0, y = 0, xend = PC1, yend = PC2, group = !!sym(input$aes_x)),
                             inherit.aes = FALSE, arrow = arrow(), alpha = .3
                )+
                ggrepel::geom_label_repel(data = scaled_loadings(), aes(x = PC1, y = PC2, label = !!sym(input$aes_x)),
                                          inherit.aes = FALSE, alpha = .3, show.legend = FALSE)
        }

        plt
    })

    output$pca_scores <- renderPlot({
        pca_ScoresPlt()
    })


    # ** Loadings -----------------------------------------------------------------------------------------------------

    pca_LoadingsPlt <- reactive({
        req(pcaObject())

        loadings <- pcaObject()@loadings %>% as_tibble(rownames = input$aes_x)

        plt <- loadings %>%
            ggplot(aes(PC1, PC2))+
            geom_point(pch = 19 ,size = input$pca_pointSize / 3)+
            mainTheme+
            ggrepel::geom_text_repel(aes(label = !!sym(input$aes_x)),
                                     show.legend = FALSE)
        plt
    })


    output$pca_loadings <- renderPlot({
        pca_LoadingsPlt()
    })


    # UMAP ------------------------------------------------------------------------------------------------------------

    umap_wide <- reactive({
        req(rawData())
        rawData() %>%
            select(sample_identifier, lipid, value) %>%
            spread(lipid, value) %>%
            replace(is.na(.), 0)
    })

    umap_data <- reactive({
        umap_wide() %>% select(-sample_identifier) %>% as.data.frame()
    })

    umap_labels <- reactive({
        umap_wide() %>% select(sample_identifier)
    })

    umap_samples <- reactive({
        req(rawData())
        rawData() %>%
            select(sample_identifier, sample) %>%
            distinct()
    })

    umap_model <- reactive({
        req(umap_data())
        umap::umap(umap_data())
    })

    umap_annotated <- reactive({
        req(umap_model())
        umap_model()$layout %>%
            as_tibble() %>%
            bind_cols(umap_labels()) %>%
            inner_join(umap_samples())
    })

    umap_plt <- reactive({
        req(umap_annotated())
        umap_annotated() %>% ggplot(aes(V1, V2, color = sample))+
            geom_point(size = 3.4)+
            mainTheme +
            ggrepel::geom_text_repel(aes(label = sample), show.legend = FALSE)
    })

    output$umap_plot <- renderPlot({
        umap_plt()
    })

    output$umap_summary <- renderText({
        #m <- umap_model()
    })


    # End -------------------------------------------------------------------------------------------------------------
    # End session when window is closed
    session$onSessionEnded(stopApp)
}

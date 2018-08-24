# Server function -------------------------------------------------------------------------------------------------
function(input, output, session) {


    # Metadata / Datasets --------------------------------------------------------------------------------------------------------

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
    output$metaDataTable <- renderDT({
        if (input$showFullMeta == TRUE){
            meta <- metaData()
        } else{
            meta <- metaData()[c("id","title", "date_upload", "status", "sample_from")]
        }
        meta
    },
    server = FALSE, selection = 'single',
    options = list(orderClasses = TRUE,
                   pageLength = 10,
                   order = list(0, 'desc'),
                   scrollX = TRUE,
                   deferRender = TRUE,
                   scrollY = 500,
                   scrollCollapse = TRUE)
    )

    # Update SelectInput for datasets based on sets loaded and row selected
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
        req(input$ID)

        query <- sqlQueryData(input$ID)
        raw <- collect(tbl(database_connection, sql(query)))
        df <- raw %>%
            filter(!is.na(value))
        df <- df %>%
            mutate(
                sample_identifier = factor(sample_identifier),
                lipid = factor(lipid),
                func_cat = factor(func_cat),
                class = factor(class),
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


    # ** Filtering rawData to create mainData -------------------------------------------------------------------------

    mainData <- reactive({
        req(rawData())

        # Temporary dataframe in the scope of this function
        df <- rawData()

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
        if(!is.null(input$sample_remove)){
            df <- df %>% filter(!(sample %in% input$sample_remove))
        }
        if(!is.null(input$rep_select)){
            df <- df %>% filter(sample_replicate %in% input$rep_select)
        }
        if(!is.null(input$rep_remove)){
            df <- df %>% filter(!(sample_replicate %in% input$rep_remove))
        }
        if(!is.null(input$tecRep_remove)){
            df <- df %>% filter(!(sample_replicate_technical %in% input$tecRep_remove))
        }

        df %>%
            return()
    })


    # ** Download handlers for metadata and raw datasets --------------------------------------------------------------

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


    # * Updating select options for filtering based on dataset --------------------------------------------------------

    observe({
        choices <- rawData()$sample %>%
            unique()
        updateSelectizeInput(session, "sample_select",
                             choices = choices
        )
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
                          value = c(ls[1], max = ls[2])
        )
        dbs <- rawData()$db %>%
            range(na.rm = TRUE)
        updateSliderInput(session, "filter_db",
                          min = dbs[1], max = dbs[2],
                          value = c(dbs[1], max = dbs[2])
        )
        ohs <- rawData()$oh %>%
            range(na.rm = TRUE)
        updateSliderInput(session, "filter_oh",
                          min = 0, max = ohs[2],
                          value = c(ohs[1], max = ohs[2])
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

    # * Displaying main Dataset as a table ----------------------------------------------------------------------------

    # Rendering selected dataset as a table to send to UI
    output$mainDataTable <- renderDT({
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



    # * plotData from mainData based on sidebar inputs ---------------------------------------------------------------

    # TODO add apropriate summarize functions based on selecte plot type and aes
    plotData <- reactive({
        df <- mainData()
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
        # temorary dataframe inside this function
        df <- plotData()

        # basic plot object
        plt <- df %>%
            ggplot()

        # Validations, friendly error messages
        shiny::validate(
            need( input$aes_x != "",
                  "Please select a feature to display on the x-axis"),
            need( input$aes_y != "",
                  "Please select a feature to display on the y-axis")
        )

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
                    fill = factor(!!sym(input$aes_color)),
                    group = factor(!!sym(input$aes_color))
                )
        }

        # Add points
        plt <- plt +
            geom_point()

        # facetting
        if (input$aes_facet1 != "" & input$aes_facet2 != ""){
            plt <- plt+
                facet_grid(rows = vars(!!sym(input$aes_facet1)),
                           cols = vars(!!sym(input$aes_facet2)),
                           scales = "free_x"
                )+
                theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
        }
        if (input$aes_facet1 != "" & input$aes_facet2 == ""){
            plt <- plt+
                facet_wrap(facets = vars(!!sym(input$aes_facet1)), scales = "free_x"
                )+
                theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
        }

        # add theme and scale (defined in global.R) includes titles and formatting
        plt <- plt +
            mainTheme +
            mainScale(colorCount)+
            guides(color = guide_legend(ncol = 12, nrow = as.integer(colorCount/12)+1 ),# usefull if way to many values of color
                   fill = guide_legend(ncol = 12, nrow = as.integer(colorCount/12)+1 )
            )

        # Zooming
        plt <- plt + coord_cartesian(xlim = ranges$x, ylim = ranges$y)

        # return final plot
        plt
    })


    # ** Plot Render --------------------------------------------------------------------------------------------
    # create actual rendered plot output from mainPlt
    output$mainPlot <- renderPlot({
        plt <- mainPlt()
        plt
    })



    # PCA -------------------------------------------------------------------------------------------------------------

    # TODO



    # Heatmap ---------------------------------------------------------------------------------------------------------

    # Change choices for color value in the sidebar selectInput
    # TODO

    # * Plot Object ---------------------------------------------------------------------------------------------------
    heatPlt <- reactive({
        # dataframe
        df <- plotData()

        # plot
        plt <- ggplot(df) +
            aes(x = !!sym(input$aes_x),
                y = factor(!!sym(input$aes_color)),
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
            scale_fill_viridis_c(option = input$heatColor) +
            NULL
    })


    # * Plot Render ---------------------------------------------------------------------------------------------------

    output$heatPlot <-renderPlot({
        plt <- heatPlt()
        plt
    })



    # End -------------------------------------------------------------------------------------------------------------
    # End session when window is closed
    session$onSessionEnded(stopApp)
}

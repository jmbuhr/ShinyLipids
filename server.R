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

        df <- rawData()

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

    # Updating select options for filtering based on dataset

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



    # ** plotData from mainData based on sidebar inputs ---------------------------------------------------------------

    plotData <- reactive({
        df <- mainData()
        df %>%
            return()
    })


    # main Plot output ------------------------------------------------------------------------------------------------

    mainPlt <- reactive({
        df <- plotData()
        plt <- df %>%
            ggplot()

        plt <- plt +
            aes(x = !!sym(input$aes_x), y = !!sym(input$aes_y))+
            geom_point()+
            labs(title = "Alpha Version")

        plt <- plt +
            aes(color = NULL)


        plt %>%
            return()
    })

    output$mainPlot <- renderPlot({
        plt <- mainPlt()

        plt %>%
            return()
    })




    # End -------------------------------------------------------------------------------------------------------------
    # End session when window is closed
    session$onSessionEnded(stopApp)
}

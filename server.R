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

    mainData <- reactive({
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
                sample_replicate_technical = factor(sample_replicate_technical)
            ) %>%
            select(id, sample_identifier, lipid, value, everything())
        df
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



    # TODO Save buttons for data





    # End -------------------------------------------------------------------------------------------------------------
    # End session when window is closed
    session$onSessionEnded(stopApp)
}

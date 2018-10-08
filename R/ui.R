# Header ----------------------------------------------------------------------------------------------------------
header <- dashboardHeader(title = span(tagList(icon("flask"), span("ShinyLipids"))))

# Sidebar ---------------------------------------------------------------------------------------------------------
sidebar <- dashboardSidebar(

    # * Visual fixes ------------------------------------------------------------------------------------------------
    tags$head(
        # tags$style(HTML(".sidebar {height: 90vh; overflow-y: auto; }" )
        # ),
        # As well as moving labels closer to their fields
        tags$style(HTML("label { margin-bottom: 3px; }")),
        tags$style(HTML(".form-group, .selectize-control {margin-bottom: 0px;}"))
    ),
    tags$head(tags$style(".wrapper {overflow: visible !important;}")),  # This part fixes the issue of having 2 scroll bars on one side:)

    shinyjs::useShinyjs(), # needed to load java script functions

    sidebarMenu(
        # * MetaData ---------------------------------------------------------------------------------------------------
        id = "tab",
        menuItem(
            "Database info", tabName = "metadata", icon = icon("th"),
            startExpanded = TRUE
        ),

        # * Visualization ----------------------------------------------------------------------------------------------
        menuItem(
            "Visualization",
            icon = icon("bar-chart"),
            menuSubItem("Main plot", tabName = "main"),
            menuSubItem("PCA", tabName = "PCA"),
            menuSubItem("Heatmap", tabName = "heatmap"),
            startExpanded = TRUE
        )
    ),

    # * Tables/Plot Options -------------------------------------------------------------------------------------------
    tabsetPanel(type = "pills",
                tabPanel("Mapping",
                         selectInput("aes_x", label = HTML("Feature to display on x-Axis /<br>use in the PCA"),
                                     choices = features[-c(4)],
                                     selected = "class"),
                         selectizeInput("aes_color", label = HTML("Feature to display by color /<br> y-axis of heatmap"),
                                        choices = features[-c(3,4,8,11,12)],
                                        selected = "sample"),
                         selectizeInput("aes_facet1", label = "Feature to use for facetting in columns",
                                        choices = features[-c(3,4,8,11,12,14)],
                                        selected = NULL),
                         selectizeInput("aes_facet2", label = "Feature to use for facetting in rows",
                                        choices = features[-c(3,4,8,11,12,14)],
                                        selected = NULL),
                         selectizeInput(
                             'std_feature', label = "Standardize to 100% within:",
                             choices = features[-4],
                             selected = NULL
                         ),
                         selectizeInput(
                             'base_sample', label = "Substract sample as baseline",
                             choices = "",
                             selected = NULL
                         )
                ),
                tabPanel("Defaults",
                         # Future Defaults panel
                         selectInput("aes_y", label = "Feature to display on y-Axis / color value of heatmap",
                                     choices = features[4],# fixed to "value" for now, other choices seem not helpful to user
                                     selected = "value"),
                         # limit options for now, dodge2 is the most usefull
                         selectInput("aes_pos", label = "Position of colored datapoints",
                                     choices = list(
                                         #"stacked" = "stack",
                                         "besides" = "dodge2"
                                         #"filled to 100%" = "fill",
                                         #"old besides" = "dodge",
                                         #"jitter" = "jitter"
                                     ),
                                     selected = "dodge2"
                         )
                ),
                tabPanel("Samples",
                         selectizeInput(
                             'sample_select',
                             label = 'Select samples',
                             options = list(placeholder = "Explicitly demand sample"),
                             choices = NULL,
                             multiple = TRUE
                         ),
                         selectizeInput(
                             'sample_remove',
                             label = 'Remove samples',
                             options = list(placeholder = "Explicitly remove sample"),
                             choices = NULL,
                             multiple = TRUE
                         ),
                         selectizeInput(
                             'rep_select',
                             label = "Select replicates",
                             options = list(placeholder = "Explicitly demand replicate"),
                             choices = NULL,
                             multiple = TRUE
                         ),
                         selectizeInput(
                             'rep_remove',
                             label = "Remove replicates",
                             options = list(placeholder = "Explicitly remove replicate"),
                             choices = NULL,
                             multiple = TRUE
                         ),
                         selectizeInput(
                             'tecRep_remove',
                             label = "Remove technical replicates",
                             options = list(placeholder = "Explicitly remove tec. replicate"),
                             choices = NULL,
                             multiple = TRUE
                         ),
                         checkboxInput(
                             "tecRep_average",
                             label = "average over technical replicates", value = TRUE
                         )
                ),
                tabPanel("Filters",
                         selectizeInput(
                             'filter_class',
                             label = "Select classes",
                             options = list(placeholder = "empty field means no filtering based on this feature"),
                             choices = NULL,
                             multiple = TRUE
                         ),
                         selectizeInput(
                             'filter_cat',
                             label = "Select categories",
                             options = list(placeholder = "empty field means no filtering based on this feature"),
                             choices = NULL,
                             multiple = TRUE
                         ),
                         selectizeInput(
                             'filter_func',
                             label = "Select functional categories",
                             options = list(placeholder = "empty field means no filtering based on this feature"),
                             choices = NULL,
                             multiple = TRUE
                         ),
                         sliderInput(
                             'filter_length',
                             label = "Filter length",
                             min = 1, max = 100,
                             value = c(1,100)
                         ),
                         sliderInput(
                             'filter_db',
                             label = "Filter double bonds",
                             min = 1, max = 10,
                             value = c(1,10)
                         ),
                         sliderInput(
                             'filter_oh',
                             label = "Filter hydroxylation",
                             min = 1, max = 10,
                             value = c(1,10)
                         )
                )
    ),

    # * Impressum -----------------------------------------------------------------------------------------------------
    menuItem("About ", href = "http://bzh.db-engine.de/default.asp?lfn=2241&fg=4289"),
    div(
        strong("ShinyLipids"),
        p("Rewritten by"),
        em("Jannik Buhr"),
        p("SL 2.1.0 -- 2018-10-03"),
        p("Based on the original Shiny Lipids by:"),
        em("Mathias Gerl"),
        br(),
        em("Manuel Haußmann"),
        br(),
        em("Sebastian Bender")
    )
)


# Body ------------------------------------------------------------------------------------------------------------
body <- dashboardBody(
    shinyjs::useShinyjs(),
    # ** Database Info / Meta ------------------------------------------------------------------------
    tabItems(
        tabItem(
            tabName = "metadata",
            fluidRow(
                fileInput("database_connection",
                          label = "Load a local Sqlite database (a .db file)",
                          accept = c(".db")
                          )
            ),
            fluidRow(
                box(
                    title = NULL,
                    status = "primary",
                    width = 12,
                    DTOutput("metaDataTable"),
                    # Save buttons
                    checkboxInput("showFullMeta", label = "Show all columns", value = FALSE),
                    selectInput("ID",
                                label = "Select dataset by clicking on the table or use this dropdown list",
                                choices = "",
                                width = "50%"),
                    downloadButton("saveMeta", label = "Save metadata as .csv"),
                    downloadButton("saveRawCSV", label = "Save selected dataset as .csv (unfiltered)"),
                    downloadButton("saveMainCSV", label = "Save selected dataset as .csv (filtered)")
                )
            ),

            # *** Dataset Table ----------------------------------------------------------------------
            fluidRow(
                box(
                    title = "Selected Dataset",
                    status = "primary",
                    width = NULL,
                    DTOutput("mainDataTable")
                )
            )
        ),

        # ** Main plot -------------------------------------------------------------------------------
        tabItem(tabName = "main",
                fluidRow(
                    box(
                        title = NULL,
                        width = 12,
                        height = 620,
                        status = "primary",
                        plotOutput("mainPlot",height = 600,
                                   dblclick = "mainPlot_dblclick",
                                   brush = brushOpts(id = "mainPlot_brush",
                                                     resetOnNew = TRUE)
                        ) %>% withSpinner(type = 4, color = "#605ca8")
                    )
                ),
                fluidRow(
                    box(width = 4,
                        checkboxGroupInput("main_add", label = NULL,
                                           choices = list(
                                               "Log10 Y-Axis" = "log",
                                               "Display value of means" = "values",
                                               "Display value of points" = "ind_values",
                                               "Show Points" = "points",
                                               "Show bars" = "bars",
                                               "Swap X- and Y-Axis" = "swap",
                                               "Highlite average" = "mean"
                                           ),
                                           selected = list("points", "bars")
                        ),
                        selectInput("main_error", "Type of error bars",
                                    choices = list("SD", "SEM")
                        )
                    ),
                    box(width = 4,
                        downloadButton("main_savePlot", label = "Save plot as .pdf"),
                        br(),
                        downloadButton("main_saveData", label = "Save datapoints as .csv"),
                        downloadButton("main_saveMeans", label = "Save means as .csv"),
                        numericInput("mainWidth", label = "width", value = 20),
                        numericInput("mainHeight", label = "height", value = 10)
                    )
                ),
                fluidRow(
                    box(title = "Summary",
                        width = NULL,
                        DTOutput("meanPlotDataTable")
                    )
                )
        ),

        # ** PCA -------------------------------------------------------------------------------------
        tabItem(
            tabName = "PCA",
            fluidRow(
                box(
                    title = NULL,
                    width = 6,
                    status = "primary",
                    plotOutput("pca_scores") %>% withSpinner(type = 4, color = "#605ca8")
                ),
                box(
                    title = NULL,
                    width = 6,
                    status = "primary",
                    plotOutput("pca_loadings") %>% withSpinner(type = 4, color = "#605ca8")
                )
            ),
            fluidRow(
                column(width = 6,
                       box(width = NULL,
                           verbatimTextOutput("pca_info")
                       ),
                       box(width = NULL,
                           downloadButton("pca_saveScores", label = "Save Score Plot as .pdf"),
                           downloadButton("pca_saveLoadings", label = "Save Loadings Plot as .pdf"),
                           numericInput("pca_Width", label = "width", value = 20),
                           numericInput("pca_Height", label = "height", value = 20)
                       )
                ),
                column(width = 6,
                       box(width = NULL,
                           checkboxInput("pca_center", "Center", TRUE),
                           checkboxInput("pca_labels", "Sample Labels", TRUE),
                           checkboxInput("pca_vectors", "Show original dimensions in principal component space", TRUE),
                           checkboxInput("pca_hull", "Draw convex hull", FALSE),
                           selectInput(
                               "pca_scaling",
                               "Scale",
                               list(
                                   "none" = "none",
                                   "unit variance" = "uv",
                                   "pareto" = "pareto"
                               )
                           ),
                           selectInput("pca_method", "Method",
                                       listPcaMethods()[1:7],# The last three don't work for now
                                       selected = "svd"),
                           selectInput("pca_cv", "cross-validation",
                                       list ("none" = "none", "Q2" =  "q2")),
                           sliderInput(
                               "pca_nPC",
                               label = "Number of PCs",
                               min = 2,
                               max = 8,
                               value = 2,
                               ticks = TRUE,
                               animate = FALSE
                           ),
                           sliderInput(
                               "pca_pointSize",
                               label = "Point Size",
                               min = 1,
                               max = 10,
                               value =5,
                               ticks = TRUE,
                               animate = FALSE
                           )
                       )
                )
            )
        ),

        # ** heatmap ---------------------------------------------------------------------------------
        tabItem(tabName = "heatmap",
                fluidRow(
                    box(
                        title = NULL,
                        width = 12,
                        status = "primary",
                        plotOutput("heatPlot") %>% withSpinner(type = 4, color = "#605ca8")
                    )
                ),
                fluidRow(
                    box(
                        downloadButton("heatSave", label = "Save as .pdf"),
                        numericInput("heatWidth", label = "width", value = 20),
                        numericInput("heatHeight", label = "height", value = 10)
                    ),
                    box(
                        selectInput(
                            "heatColor",
                            "Color scheme",
                            choices = list("viridis",
                                           "magma",
                                           "plasma",
                                           "inferno",
                                           "cividis")
                        ),
                        sliderInput(
                            "heatLabSize",
                            label = "Size of labels",
                            min = 1,
                            max = 30,
                            value = 9,
                            ticks = TRUE,
                            animate = FALSE
                        )
                    )
                )
        )
    )
)

# Initiate Dashboard page ----------------------------------------------------------------------------------------------
dashboardPage(skin = "purple",
              title = "ShinyLipids",
              header,
              sidebar,
              body)

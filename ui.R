# Header ----------------------------------------------------------------------------------------------------------
header <- shinydashboard::dashboardHeader(
    title = span(tagList(icon("flask"),
                         span("ShinyLipids"))),
    shinydashboard::dropdownMenu(type       = "notification", badgeStatus = NULL,
                                 icon       = icon("question-circle"),
                                 headerText = "Need Help?",
                                 shinydashboard::notificationItem(text   = "Documentation",
                                                                  href   = "https://jmbuhr.de/project/shinylipids/",
                                                                  icon   = icon("book"),
                                                                  status = "success"
                                 ),
                                 shinydashboard::notificationItem(text   = "Contact author",
                                                                  href   = "https://jmbuhr.de/#contact",
                                                                  status = "success",
                                                                  icon   = icon("envelope")
                                 )
    )
)

# Sidebar ---------------------------------------------------------------------------------------------------------
sidebar <- shinydashboard::dashboardSidebar(

    # * Visual fixes ------------------------------------------------------------------------------------------------
    tags$head(
        # tags$style(HTML(".sidebar {height: 90vh; overflow-y: auto; }" )
        # ),
        # As well as moving labels closer to their fields
        tags$style(HTML("label { margin-bottom: 3px; }")),
        tags$style(HTML(".form-group, .selectize-control {margin-bottom: 0px;}"))
    ),
    # This part fixes the issue of having 2 scroll bars on one side :)
    tags$head(tags$style(".wrapper {overflow: visible !important;}")),

    shinyjs::useShinyjs(), # needed to load java script functions

    shinydashboard::sidebarMenu(
        id = "tab",
        selectInput(
            "ID",
            label   = "Select dataset by clicking on the table or use this dropdown list",
            choices = ""),
        shinydashboard::menuItem(
            "Database info",
            tabName       = "metadata",
            icon          = icon("th"),
            startExpanded = TRUE
        ),

        # * Visualization ----------------------------------------------------------------------------------------------
        shinydashboard::menuItem(
            "Visualization",
            icon = icon("bar-chart"),
            shinydashboard::menuSubItem("Main plot", tabName = "main"),
            shinydashboard::menuSubItem("Heatmap", tabName = "heatmap"),
            shinydashboard::menuSubItem("PCA", tabName = "PCA"),
            startExpanded = TRUE
        )
    ),

    # * Tables/Plot Options -------------------------------------------------------------------------------------------
    tabsetPanel(type = "pills", id = "tabs",
                tabPanel(title = "Mapping",
                    selectInput(
                        "aes_x",
                        label       = HTML("Feature to display on x-Axis /<br>use in the PCA"),
                        choices     = features[!features %in% c("value",
                                                                "sample_replicate",
                                                                "sample_replicate_technical")],
                        selected    = "class"),
                    selectizeInput(
                        "aes_color",
                        label    = HTML("Feature to display by color /<br> y-axis of heatmap"),
                        choices  = features[!features %in% c("lipid",
                                                             "value",
                                                             "length",
                                                             "chains",
                                                             "chain_sums",
                                                             "sample_replicate",
                                                             "sample_replicate_technical")],
                        selected = "sample"),
                    selectizeInput(
                        "aes_facet1",
                        label    = "Feature to use for facetting in columns",
                        choices  = features[!features %in% c("lipid",
                                                             "value",
                                                             "length",
                                                             "chains",
                                                             "chain_sums",
                                                             "sample_replicate",
                                                             "sample_replicate_technical")],
                        selected = NULL),
                    selectizeInput(
                        "aes_facet2",
                        label    = "Feature to use for facetting in rows",
                        choices  = features[!features %in% c("lipid",
                                                             "value",
                                                             "length",
                                                             "chains",
                                                             "chain_sums",
                                                             "sample_replicate",
                                                             "sample_replicate_technical")],
                        selected = NULL),
                    selectizeInput(
                        "std_feature",
                        label    = "Standardize to 100% within:",
                        choices  = features[!features %in% c("value")],
                        selected = "sample_replicate_technical"
                    ),
                    selectizeInput(
                        "base_sample",
                        label    = "Substract sample as baseline",
                        choices  = "",
                        selected = NULL
                    )
                ),
                tabPanel(title = "Samples",
                         selectizeInput(
                             'sample_select',
                             label    = 'Select samples',
                             options  = list(placeholder = "Explicitly demand sample"),
                             choices  = NULL,
                             multiple = TRUE
                         ),
                         selectizeInput(
                             'sample_remove',
                             label    = 'Remove samples',
                             options  = list(placeholder = "Explicitly remove sample"),
                             choices  = NULL,
                             multiple = TRUE
                         ),
                         selectizeInput(
                             'rep_select',
                             label    = "Select replicates",
                             options  = list(placeholder = "Explicitly demand replicate"),
                             choices  = NULL,
                             multiple = TRUE
                         ),
                         selectizeInput(
                             'rep_remove',
                             label    = "Remove replicates",
                             options  = list(placeholder = "Explicitly remove replicate"),
                             choices  = NULL,
                             multiple = TRUE
                         ),
                         selectizeInput(
                             'tecRep_remove',
                             label    = "Remove technical replicates",
                             options  = list(placeholder = "Explicitly remove tec. replicate"),
                             choices  = NULL,
                             multiple = TRUE
                         ),
                         checkboxInput(
                             "tecRep_average",
                             label    = "average over technical replicates", value = TRUE
                         )
                ),
                tabPanel(title = "Filters",
                    actionButton(
                        'filter_apply',
                        label    = "Apply filter",
                        icon     = icon("rocket")
                    ),
                    selectizeInput(
                        'filter_class',
                        label    = "Select classes",
                        options  = list(placeholder = "empty field means no filtering based on this feature"),
                        choices  = NULL,
                        multiple = TRUE
                    ),
                    selectizeInput(
                        'filter_cat',
                        label    = "Select categories",
                        options  = list(placeholder = "empty field means no filtering based on this feature"),
                        choices  = NULL,
                        multiple = TRUE
                    ),
                    selectizeInput(
                        'filter_func',
                        label    = "Select functional categories",
                        options  = list(placeholder = "empty field means no filtering based on this feature"),
                        choices  = NULL,
                        multiple = TRUE
                    ),
                    sliderInput(
                        'filter_length',
                        label    = "Filter length",
                        min      = 1, max = 100,
                        value    = c(1,100)
                    ),
                    sliderInput(
                        'filter_db',
                        label    = "Filter double bonds",
                        min      = 1, max = 10,
                        value    = c(1,10)
                    ),
                    sliderInput(
                        'filter_oh',
                        label    = "Filter hydroxylation",
                        min      = 1, max = 10,
                        value    = c(1,10)
                    )
                )
    ),

    # * Impressum -----------------------------------------------------------------------------------------------------
    shinydashboard::menuItem("About ", href = "http://bzh.db-engine.de/default.asp?lfn=2241&fg=4289"),
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
body <- shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    # ** Database Info / Meta ------------------------------------------------------------------------
    shinydashboard::tabItems(
        shinydashboard::tabItem(
            "metadata",
            fluidRow(
                shinydashboard::box(
                    status = "primary",
                    width = 12,
                    DT::DTOutput("metaDataTable"),
                    # Save buttons
                    checkboxInput("showFullMeta", label = "Show all columns", value = FALSE),
                    downloadButton("saveMeta", label = "Save metadata as .csv"),
                    downloadButton("saveRawCSV", label = "Save selected dataset as .csv (unfiltered)"),
                    downloadButton("saveMainCSV", label = "Save selected dataset as .csv (filtered)")
                )
            ),

            # *** Dataset Table ----------------------------------------------------------------------
            fluidRow(
                shinydashboard::box(
                    title = "Selected Dataset",
                    status = "primary",
                    width = NULL,
                    DT::DTOutput("mainDataTable")
                )
            )
        ),

        # ** Main plot -------------------------------------------------------------------------------
        shinydashboard::tabItem(tabName = "main",
                                fluidRow(
                                    shinydashboard::box(
                                        width                                   = 12,
                                        height                                  = 620,
                                        status                                  = "primary",
                                        plotOutput(
                                            "mainPlot",
                                            height   = 600,
                                            dblclick = "mainPlot_dblclick",
                                            brush    = brushOpts(id = "mainPlot_brush", resetOnNew = TRUE)
                                        ) %>% shinycssloaders::withSpinner(type = 4, color = "#605ca8")
                                    )
                                ),
                                fluidRow(
                                    shinydashboard::box(
                                        "Options",
                                        width = 3,
                                        checkboxGroupInput("main_add", label = NULL,
                                                           choices = list(
                                                               "Show points"                       = "points",
                                                               "Show bars"                         = "bars",
                                                               "Show average"                      = "mean",
                                                               "Show value of means"               = "values",
                                                               "Show value of points"              = "ind_values",
                                                               "Transform y-axis log1p"            = "log",
                                                               "Show N per sample"                 = "N",
                                                               "Label points"                      = "label",
                                                               "Swap x- and y-axis"                = "swap",
                                                               "Free y scale for facets"           = "free_y",
                                                               "Mark groups with significant hits" = "signif"
                                                           ),
                                                           selected = list("points", "bars")
                                        ),
                                        selectInput("main_error", "Type of error bars",
                                                    choices = list("SD", "SEM", "CI", "None"),
                                                    selected = "None"
                                        )
                                    ),
                                    shinydashboard::box(
                                        "Download",
                                        width = 3,
                                        downloadButton("main_savePlot", label = "Save plot as .pdf"),
                                        br(),
                                        downloadButton("main_saveData", label = "Save datapoints as .csv"),
                                        downloadButton("main_saveMeans", label = "Save means as .csv"),
                                        numericInput("mainWidth", label = "width", value = 20),
                                        numericInput("mainHeight", label = "height", value = 10)
                                    ),
                                    shinydashboard::box(
                                        "Pairwise Comparisons",
                                        width = 6,
                                        DT::DTOutput("pairwiseComparisonsTable")
                                    )
                                ),
                                fluidRow(
                                    shinydashboard::box(
                                        "Summary",
                                        width = NULL,
                                        DT::DTOutput("meanPlotDataTable")
                                    )
                                ),
                                fluidRow(
                                    shinydashboard::box(
                                        "Order",
                                        width = NULL,
                                        shinyjqui::orderInput('custom_class_order',
                                                              label = 'Custom Class Order',
                                                              items = class_levels,
                                                              item_class = 'primary',
                                                              width = '100%') #,
                                        # shinyjqui::orderInput('custom_sample_order',
                                        #                       label = 'Custom Sample Order',
                                        #                       items = letters,
                                        #                       item_class = 'primary',
                                        #                       width = '100%')
                                    )
                                )
        ),

        # ** PCA -------------------------------------------------------------------------------------
        shinydashboard::tabItem(
            tabName = "PCA",
            fluidRow(
                shinydashboard::box(
                    title  = NULL,
                    width  = 6,
                    status = "primary",
                    plotOutput("pca_scores") %>% shinycssloaders::withSpinner(type = 4, color = "#605ca8")
                ),
                shinydashboard::box(
                    title  = NULL,
                    width  = 6,
                    status = "primary",
                    plotOutput("pca_loadings") %>% shinycssloaders::withSpinner(type = 4, color = "#605ca8")
                )
            ),
            fluidRow(
                column(
                    width = 6,
                    shinydashboard::box(width = NULL,
                                        verbatimTextOutput("pca_info")
                    ),
                    shinydashboard::box(width = NULL,
                                        downloadButton("pca_saveScores", label = "Save Score Plot as .pdf"),
                                        downloadButton("pca_saveLoadings", label = "Save Loadings Plot as .pdf"),
                                        numericInput("pca_Width", label = "width", value = 20),
                                        numericInput("pca_Height", label = "height", value = 20)
                    )
                ),
                column(
                    width = 6,
                    shinydashboard::box(width = NULL,
                                        checkboxInput("pca_center", "Center", TRUE),
                                        checkboxInput("pca_labels", "Sample Labels", TRUE),
                                        checkboxInput("pca_vectors", "Show original dimensions in principal component space", TRUE),
                                        checkboxInput("pca_hull", "Draw convex hull", FALSE),
                                        selectInput(
                                            "pca_scaling",
                                            "Scale",
                                            list(
                                                "none"          = "none",
                                                "unit variance" = "uv",
                                                "pareto"        = "pareto"
                                            )
                                        ),
                                        selectInput(
                                            "pca_method",
                                            "Method",
                                            pcaMethods::listPcaMethods()[1:7],# The last three don't work for now
                                            selected = "svd"),
                                        selectInput("pca_cv",
                                                    "cross-validation",
                                                    list("none" = "none", "Q2" =  "q2")),
                                        sliderInput(
                                            "pca_nPC",
                                            label   = "Number of PCs",
                                            min     = 2,
                                            max     = 8,
                                            value   = 2,
                                            ticks   = TRUE,
                                            animate = FALSE
                                        ),
                                        sliderInput(
                                            "pca_pointSize",
                                            label   = "Point Size",
                                            min     = 1,
                                            max     = 10,
                                            value   = 5,
                                            ticks   = TRUE,
                                            animate = FALSE
                                        )
                    )
                )
            )
        ),

        # ** heatmap ---------------------------------------------------------------------------------
        shinydashboard::tabItem(tabName = "heatmap",
                                fluidRow(
                                    shinydashboard::box(
                                        width  = 12,
                                        status = "primary",
                                        plotOutput("heatPlot") %>% shinycssloaders::withSpinner(type = 4, color = "#605ca8")
                                    )
                                ),
                                fluidRow(
                                    shinydashboard::box(
                                        downloadButton("heatSave", label = "Save as .pdf"),
                                        numericInput("heatWidth", label = "width", value = 20),
                                        numericInput("heatHeight", label = "height", value = 10)
                                    ),
                                    shinydashboard::box(
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
                                            label   = "Size of labels",
                                            min     = 1,
                                            max     = 30,
                                            value   = 9,
                                            ticks   = TRUE,
                                            animate = FALSE
                                        )
                                    )
                                )
        )
    )
)

# Initiate Dashboard page ----------------------------------------------------------------------------------------------
shinydashboard::dashboardPage(
    skin    = "purple",
    title   = "ShinyLipids",
    header  = header,
    sidebar = sidebar,
    body    = body
)

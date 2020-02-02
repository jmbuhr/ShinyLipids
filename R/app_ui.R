#' @import shiny
app_ui <- function() {
  shinyjs::useShinyjs()
  # Header ####
  header <- shinydashboard::dashboardHeader(
    title = span(tagList(icon("flask"),
                         span("ShinyLipids"))),
    shinydashboard::dropdownMenu(
      type       = "notification",
      badgeStatus = NULL,
      icon       = icon("question-circle"),
      headerText = "Need Help?",
      shinydashboard::notificationItem(text   = "Documentation",
                                       href   = "https://jmbuhr.de/project/shinylipids/",
                                       icon   = icon("book"),
                                       status = "success"),
      shinydashboard::notificationItem(text   = "Contact author",
                                       href   = "https://jmbuhr.de/#contact",
                                       status = "success",
                                       icon   = icon("envelope")),
      tags$li(
        a(
          p("Rewritten by", strong("Jannik Buhr")),
          p("Based on the original Shiny Lipids by:"),
          strong("Mathias Gerl"),
          br(),
          strong("Manuel Hau\u00dfmann"),
          br(),
          strong("Sebastian Bender")
          )
        )
    )
  )
  
  # Sidebar ####
  sidebar <- shinydashboard::dashboardSidebar(
    # * Visual fixes ####
    tags$head(
      # Move labels closer to their fields
      tags$style(HTML("label { margin-bottom: 3px; }")),
      tags$style(HTML(".form-group, .selectize-control {margin-bottom: 0px;}"))
    ),
    # Fix issue of having 2 scroll bars on one side :)
    tags$head(tags$style(".wrapper {overflow: visible !important;}")),
    shinydashboard::sidebarMenu(
      id = "tab",
      selectInput("ID",
                  label   = "Select dataset by clicking on the table or use this dropdown list",
                  choices = ""),
      shinydashboard::menuItem(
        "Database info",
        tabName       = "metadata",
        icon          = icon("th"),
        startExpanded = TRUE
      ),
      
      # * Visualization ####
      shinydashboard::menuItem(
        "Visualization",
        icon = icon("bar-chart"),
        shinydashboard::menuSubItem("Main plot", tabName = "main"),
        shinydashboard::menuSubItem("Heatmap", tabName = "heatmap"),
        shinydashboard::menuSubItem("PCA", tabName = "PCA"),
        startExpanded = TRUE
      )
    ),
    
    # * Tables/Plot Options ####
    tabsetPanel(type = "pills", id = "tabs",
                tabPanel(title = "Quickoptions",
                         actionButton(
                           "quickClassProfile",
                           label = "Class Profile"
                         ),
                         selectizeInput(
                           "quickSpeciesProfileClass",
                           label = "Species Profile for:",
                           options  = list(placeholder = "Select a class to automatically filter and display"),
                           choices  = NULL
                         ),
                ),
                tabPanel(title = "Mapping",
                         selectInput(
                           "aesX",
                           label       = HTML("Feature to display on x-Axis /<br>use in the PCA"),
                           choices     = features[!features %in% c("value",
                                                                   "sample_replicate",
                                                                   "sample_replicate_technical")],
                           selected    = "class"),
                         selectizeInput(
                           "aesColor",
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
                           "aesFacetCol",
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
                           "aesFacetRow",
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
                           "standardizationFeatures",
                           label    = "Standardize to 100% within:",
                           multiple = TRUE,
                           choices  = features[!features %in% c("value", "sample_replicate_technical")],
                           selected = c("")
                         ),
                         selectizeInput(
                           "baselineSample",
                           label    = "Substract sample as baseline",
                           choices  = "",
                           selected = NULL
                         )
                ),
                tabPanel(title = "Samples",
                         selectizeInput(
                           'samplesToSelect',
                           label    = 'Select samples',
                           options  = list(placeholder = "Explicitly demand sample"),
                           choices  = NULL,
                           multiple = TRUE
                         ),
                         selectizeInput(
                           'samplesToRemove',
                           label    = 'Remove samples',
                           options  = list(placeholder = "Explicitly remove sample"),
                           choices  = NULL,
                           multiple = TRUE
                         ),
                         selectizeInput(
                           'replicatesToSelect',
                           label    = "Select replicates",
                           options  = list(placeholder = "Explicitly demand replicate"),
                           choices  = NULL,
                           multiple = TRUE
                         ),
                         selectizeInput(
                           'replicatesToRemove',
                           label    = "Remove replicates",
                           options  = list(placeholder = "Explicitly remove replicate"),
                           choices  = NULL,
                           multiple = TRUE
                         ),
                         selectizeInput(
                           'technicalReplicatesToRemove',
                           label    = "Remove technical replicates",
                           options  = list(placeholder = "Explicitly remove tec. replicate"),
                           choices  = NULL,
                           multiple = TRUE
                         ),
                         checkboxInput(
                           "summariseTechnicalReplicates",
                           label    = "average over technical replicates",
                           value = TRUE
                         ),
                         checkboxInput(
                           "standardizeWithinTechnicalReplicate",
                           label = "Standardize within technical replicates",
                           value = TRUE
                         )
                ),
                tabPanel(title = "Filters",
                         selectizeInput(
                           'lipidClassToSelect',
                           label    = "Select classes",
                           options  = list(placeholder = "empty field means no filtering based on this feature"),
                           choices  = NULL,
                           multiple = TRUE
                         ),
                         selectizeInput(
                           'categoryToSelect',
                           label    = "Select categories",
                           options  = list(placeholder = "empty field means no filtering based on this feature"),
                           choices  = NULL,
                           multiple = TRUE
                         ),
                         selectizeInput(
                           'functionalCategoryToSelect',
                           label    = "Select functional categories",
                           options  = list(placeholder = "empty field means no filtering based on this feature"),
                           choices  = NULL,
                           multiple = TRUE
                         ),
                         sliderInput(
                           'filterLengthRange',
                           label    = "Filter length",
                           min      = 1, max = 100,
                           value    = c(1,100)
                         ),
                         sliderInput(
                           'filterDoubleBondsRange',
                           label    = "Filter double bonds",
                           min      = 0,
                           max = 10,
                           value    = c(0,10)
                         ),
                         sliderInput(
                           'filterOhRange',
                           label    = "Filter hydroxylation",
                           min      = 0,
                           max = 10,
                           value    = c(0,10)
                         )
                )
    )
  )
  
  # Body ####
  body <- shinydashboard::dashboardBody(
    # ** Database Info / Meta ####
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
        
        # *** Dataset Table ####
        fluidRow(
          shinydashboard::box(
            title = "Selected Dataset",
            status = "primary",
            width = NULL,
            DT::DTOutput("mainDataTable")
          )
        )
      ),
      
      # ** Main plot ####
      shinydashboard::tabItem(tabName = "main",
                              fluidRow(
                                shinydashboard::box(
                                  width  = 12,
                                  height = 620,
                                  status = "primary",
                                  plotOutput(
                                    "mainPlot",
                                    height   = 600,
                                    dblclick = "mainPlotDoubleClick",
                                    brush    = brushOpts(id = "mainPlotBrush", resetOnNew = TRUE)
                                  ) %>%
                                    shinycssloaders::withSpinner(type = 4, color = "#605ca8")
                                )
                              ),
                              fluidRow(
                                shinydashboard::box(
                                  title = "Options",
                                  width = 3,
                                  checkboxGroupInput("mainPlotAdditionalOptions", label = NULL,
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
                                  selectInput("errorbarType", "Type of error bars",
                                              choices = list("SD", "SEM", "CI", "None"),
                                              selected = "None"
                                  )
                                ),
                                shinydashboard::box(
                                  title = "Download",
                                  width = 3,
                                  downloadButton("saveMainPlot", label = "Save plot as .pdf"),
                                  downloadButton("savePlotData", label = "Save datapoints as .csv"),
                                  downloadButton("saveMeanPlotData", label = "Save means as .csv"),
                                  numericInput("mainWidth", label = "width", value = 20),
                                  numericInput("mainHeight", label = "height", value = 10)
                                ),
                                shinydashboard::box(
                                  title = "Pairwise comparisons",
                                  footer = "Calculated via t-tests on log-transformed data. \
                                  P-values corrected with the Benjamini-Hochberg procedure.",
                                  width = 6,
                                  DT::DTOutput("pairwiseComparisonsTable")
                                )
                              ),
                              fluidRow(
                                shinydashboard::box(
                                  title = "Summary",
                                  width = NULL,
                                  DT::DTOutput("meanPlotDataTable")
                                )
                              )
      ),
      
      # ** PCA ####
      shinydashboard::tabItem(
        tabName = "PCA",
        fluidRow(
          shinydashboard::box(
            title  = NULL,
            width  = 6,
            status = "primary",
            plotOutput("pcaScoresPlot") %>% shinycssloaders::withSpinner(type = 4, color = "#605ca8")
          ),
          shinydashboard::box(
            title  = NULL,
            width  = 6,
            status = "primary",
            plotOutput("pcaLoadingsPlot") %>% shinycssloaders::withSpinner(type = 4, color = "#605ca8")
          )
        ),
        fluidRow(
          column(
            width = 6,
            shinydashboard::box(width = NULL,
                                verbatimTextOutput("pcaInfo")
            ),
            shinydashboard::box(width = NULL, title = "Download",
                                downloadButton("savePCAScores", label = "Save Score Plot as .pdf"),
                                downloadButton("savePCALoadings", label = "Save Loadings Plot as .pdf"),
                                numericInput("pcaWidth", label = "width", value = 20),
                                numericInput("pcaHeight", label = "height", value = 20)
            )
          ),
          column(
            width = 6,
            shinydashboard::box(width = NULL,
                                checkboxInput("pcaCenter", "Center", TRUE),
                                checkboxInput("pcaLabels", "Sample Labels", TRUE),
                                checkboxInput("pcaVectors", "Show original dimensions in principal component space", TRUE),
                                checkboxInput("drawPcaConvexHull", "Draw convex hull", FALSE),
                                selectInput(
                                  "pcaScalingMethod",
                                  "Scaling method",
                                  list(
                                    "none"          = "none",
                                    "unit variance" = "uv",
                                    "pareto"        = "pareto"
                                  )
                                ),
                                selectInput(
                                  "pcaMethod",
                                  "Method",
                                  pcaMethods::listPcaMethods()[1:7],# The last three don't work for now
                                  selected = "svd"),
                                selectInput("pcaCrossValidationMethod",
                                            "Cross validation method",
                                            list("none" = "none", "Q2" =  "q2")),
                                sliderInput(
                                  "pcaNumberPrincipalComponents",
                                  label   = "Number of PCs",
                                  min     = 2,
                                  max     = 8,
                                  value   = 2,
                                  ticks   = TRUE,
                                  animate = FALSE
                                ),
                                sliderInput(
                                  "pcaPointSize",
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
      
      # ** heatmap ####
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
                                  downloadButton("saveHeatmap", label = "Save as .pdf"),
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
  
  # Initiate Dashboard page ####
  shinydashboard::dashboardPage(
    skin    = "purple",
    title   = "ShinyLipids",
    header  = header,
    sidebar = sidebar,
    body    = body
  )
}


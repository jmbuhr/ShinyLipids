features <- c(
  "",
  "value",
  "Sample"                     = "sample",
  "Sample replicate"           = "sample_replicate",
  "Sample replicate technical" = "sample_replicate_technical",
  "Class"                      = "class",
  "Lipid species"              = "lipid",
  "Category"                   = "category",
  "Functional category"        = "func_cat",
  "Double bonds"               = "db",
  "Hydroxylation state"        = "oh",
  "Chain Length"               = "length",
  "Chains"                     = "chains",
  "Chain sums"                 = "chain_sums"
)

# Header ####
uiHeader <- shinydashboard::dashboardHeader(
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
        p("Written by", strong("Jannik Buhr")),
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
uiSidebar <- shinydashboard::dashboardSidebar(
  # * Visual fixes ####
  tags$head(
    # Move labels closer to their fields
    tags$style(HTML("label { margin-bottom: 3px; }")),
    tags$style(HTML(".form-group, .selectize-control {margin-bottom: 0px;}"))
  ),
  # Fix issue of having 2 scroll bars on one side
  tags$head(tags$style(".wrapper {overflow: visible !important;}")),
  shinydashboard::sidebarMenu(
    id = "tab",
    selectInput("ID",
                label   = "Select dataset by clicking on the table or use this dropdown list",
                choices = ""),
    shinydashboard::menuItem(
      "Tables",
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
              tabPanel(title = "Quickmenu",
                       actionButton(
                         "quickClassProfile",
                         label = "Show class profile"
                       ),
                       selectizeInput(
                         "quickSpeciesProfileClass",
                         label = "Show Species Profile:",
                         options  = list(placeholder = "Select a class to filter and display"),
                         choices  = NULL
                       ),
              ),
              tabPanel(title = "Features \u2192 Plot",
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
                         selected = ""),
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
                         selected = "")),
              tabPanel(title = "Data",
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
                         selected = ""
                       ),
                       checkboxInput(
                         "standardizeWithinTechnicalReplicate",
                         label = "Standardize within technical replicates",
                         value = TRUE
                       ),
                       checkboxInput(
                         "summariseTechnicalReplicates",
                         label    = "average technical replicates",
                         value = TRUE
                       ),
                       checkboxInput(
                         "imputeMissingAs0",
                         label = "Impute missing values as 0",
                         value = TRUE
                       )
              ),
              tabPanel(title = "Samples",
                       selectizeInput(
                         "samplesToSelect",
                         label    = "Select samples",
                         options  = list(placeholder = "Explicitly demand sample"),
                         choices  = NULL,
                         multiple = TRUE
                       ),
                       selectizeInput(
                         "samplesToRemove",
                         label    = "Remove samples",
                         options  = list(placeholder = "Explicitly remove sample"),
                         choices  = NULL,
                         multiple = TRUE
                       ),
                       selectizeInput(
                         "replicatesToSelect",
                         label    = "Select replicates",
                         options  = list(placeholder = "Explicitly demand replicate"),
                         choices  = NULL,
                         multiple = TRUE
                       ),
                       selectizeInput(
                         "replicatesToRemove",
                         label    = "Remove replicates",
                         options  = list(placeholder = "Explicitly remove replicate"),
                         choices  = NULL,
                         multiple = TRUE
                       ),
                       selectizeInput(
                         "technicalReplicatesToRemove",
                         label    = "Remove technical replicates",
                         options  = list(placeholder = "Explicitly remove tec. replicate"),
                         choices  = NULL,
                         multiple = TRUE
                       )
              ),
              tabPanel(title = "Filters",
                       selectizeInput(
                         "lipidClassToSelect",
                         label    = "Select classes",
                         options  = list(placeholder = "empty field means no filtering based on this feature"),
                         choices  = NULL,
                         multiple = TRUE
                       ),
                       selectizeInput(
                         "categoryToSelect",
                         label    = "Select categories",
                         options  = list(placeholder = "empty field means no filtering based on this feature"),
                         choices  = NULL,
                         multiple = TRUE
                       ),
                       selectizeInput(
                         "functionalCategoryToSelect",
                         label    = "Select functional categories",
                         options  = list(placeholder = "empty field means no filtering based on this feature"),
                         choices  = NULL,
                         multiple = TRUE
                       ),
                       sliderInput(
                         "filterLengthRange",
                         label    = "Filter length",
                         min      = 0L, max = 1L,
                         step     = 1L,
                         value    = c(1L, 100L)
                       ),
                       sliderInput(
                         "filterDoubleBondsRange",
                         label    = "Filter double bonds",
                         min      = 0L, max = 1L,
                         step     = 1L,
                         value    = c(0L, 10L)
                       ),
                       sliderInput(
                         "filterOhRange",
                         label    = "Filter hydroxylation",
                         min      = 0L, max = 1L,
                         step     = 1L,
                         value    = c(0L, 10L)
                       )
              )
  )
)

# Body ####
uiBody <- shinydashboard::dashboardBody(
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
                                )
                              )
                            ),
                            fluidRow(
                              shinydashboard::box(
                                title = "Options",
                                width = 3,
                                checkboxGroupInput("mainPlotAdditionalOptions", label = NULL,
                                                   choices = list(
                                                     "Show points"                = "points",
                                                     "Show bars"                  = "bars",
                                                     "Show average"               = "mean",
                                                     "Show value of means"        = "values",
                                                     "Show value of points"       = "ind_values",
                                                     "Transform y-axis log1p"     = "log",
                                                     "Show N per sample"          = "N",
                                                     "Label points"               = "label",
                                                     "Swap x- and y-axis"         = "swap",
                                                     "Free y scale for facets"    = "free_y",
                                                     "Run pairwise t-tests"       = "signif"
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
                                downloadButton("saveMainPlot", label = "Save plot (.pdf)"),
                                downloadButton("savePlotData", label = "Save points (.csv)"),
                                downloadButton("saveMeanPlotData", label = "Save means (.csv)"),
                                numericInput("mainWidth", label = "width", value = 20),
                                numericInput("mainHeight", label = "height", value = 10)
                              ),
                              shinydashboard::box(
                                title = "Pairwise comparisons",
                                footer = "This table only shows when you tick \"Run pairwise t-tests\" in the plot options next to this. Calculated via t-tests on log-transformed data. \
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
          plotOutput("pcaScoresPlot")
        ),
        shinydashboard::box(
          title  = NULL,
          width  = 6,
          status = "primary",
          plotOutput("pcaLoadingsPlot")
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
                                pcaMethods::listPcaMethods()[1:7], # The last three don't work for now
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
                                plotOutput("heatPlot")
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

app_ui <- function() {
  shinydashboard::dashboardPage(
    skin    = "purple",
    title   = "ShinyLipids",
    header  = uiHeader,
    sidebar = uiSidebar,
    body    = uiBody
  )
}

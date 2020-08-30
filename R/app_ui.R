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
uiHeader <- function() {
  shinydashboard::dashboardHeader(
    title = span(tagList(icon("flask"),
                         span("ShinyLipids"))),
    shinydashboard::dropdownMenu(
      type       = "notification",
      badgeStatus = NULL,
      icon       = icon("question-circle"),
      headerText = "Need Help?",
      shinydashboard::notificationItem(text   = "Documentation",
                                       href   = "https://jmbuhr.de/ShinyLipids/",
                                       icon   = icon("book"),
                                       status = "success"),
      shinydashboard::notificationItem(text   = "Report bug",
                                       href   = "https://github.com/jmbuhr/ShinyLipids/issues/new",
                                       status = "success",
                                       icon   = icon("bug")),
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
  )}

# Sidebar ####
uiSidebar <- function() {
  defaultInput <- defaultInput()
  shinydashboard::dashboardSidebar(
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
        shinydashboard::menuSubItem("Dimensionality reduction", tabName = "dimensionalityReduction"),
        startExpanded = TRUE
      )
    ),
    
    # * Tables/Plot Options ####
    tabsetPanel(type = "pills",
                id = "tabs",
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
                         actionButton(
                           "resetEverything",
                           "Reset everything"
                         )
                ),
                tabPanel(title = "Features \u2192 Plot",
                         selectInput(
                           inputId = "aesX",
                           label    = HTML("Feature to display on x-Axis /<br>use in the PCA"),
                           choices  = features[!features %in% c("value",
                                                                "sample_replicate",
                                                                "sample_replicate_technical")],
                           selected = defaultInput$aesX),
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
                           selected = defaultInput$aesColor),
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
                           selected = defaultInput$aesFacetCol),
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
                           selected = defaultInput$aesFacetRow)),
                tabPanel(title = "Data",
                         selectizeInput(
                           "standardizationFeatures",
                           label    = "Standardize to 100% within:",
                           multiple = TRUE,
                           choices  = features[!features %in% c("value", "sample_replicate_technical")],
                           selected = defaultInput$standardizationFeatures
                         ),
                         selectizeInput(
                           "baselineSample",
                           label    = "Substract sample as baseline",
                           choices  = "",
                           selected = defaultInput$baselineSample
                         ),
                         checkboxInput(
                           "standardizeWithinTechnicalReplicate",
                           label = "Standardize to 100% within technical replicates",
                           value = defaultInput$standardizeWithinTechnicalReplicate
                         ),
                         checkboxInput(
                           "summariseTechnicalReplicates",
                           label    = "Average technical replicates",
                           value = defaultInput$summariseTechnicalReplicates
                         ),
                         checkboxInput(
                           "imputeMissingAs0",
                           label = "Impute missing values as 0",
                           value = defaultInput$imputeMissingAs0
                         )
                ),
                tabPanel(title = "Samples",
                         selectizeInput(
                           "samplesToSelect",
                           label    = "Select samples",
                           options  = list(placeholder = "Explicitly demand sample"),
                           choices  = defaultInput$samplesToSelect,
                           multiple = TRUE
                         ),
                         selectizeInput(
                           "samplesToRemove",
                           label    = "Remove samples",
                           options  = list(placeholder = "Explicitly remove sample"),
                           choices  = defaultInput$samplesToRemove,
                           multiple = TRUE
                         ),
                         selectizeInput(
                           "replicatesToSelect",
                           label    = "Select replicates",
                           options  = list(placeholder = "Explicitly demand replicate"),
                           choices  = defaultInput$replicatesToSelect,
                           multiple = TRUE
                         ),
                         selectizeInput(
                           "replicatesToRemove",
                           label    = "Remove replicates",
                           options  = list(placeholder = "Explicitly remove replicate"),
                           choices  = defaultInput$replicatesToRemove,
                           multiple = TRUE
                         ),
                         selectizeInput(
                           "technicalReplicatesToRemove",
                           label    = "Remove technical replicates",
                           options  = list(placeholder = "Explicitly remove tec. replicate"),
                           choices  = defaultInput$technicalReplicatesToRemove,
                           multiple = TRUE
                         )
                ),
                tabPanel(title = "Filters",
                         selectizeInput(
                           "lipidClassToSelect",
                           label    = "Select classes",
                           options  = list(placeholder = "empty field means no filtering based on this feature"),
                           choices  = defaultInput$lipidClassToSelect,
                           multiple = TRUE
                         ),
                         selectizeInput(
                           "categoryToSelect",
                           label    = "Select categories",
                           options  = list(placeholder = "empty field means no filtering based on this feature"),
                           choices  = defaultInput$categoryToSelect,
                           multiple = TRUE
                         ),
                         selectizeInput(
                           "functionalCategoryToSelect",
                           label    = "Select functional categories",
                           options  = list(placeholder = "empty field means no filtering based on this feature"),
                           choices  = defaultInput$functionalCategoryToSelect,
                           multiple = TRUE
                         ),
                         sliderInput(
                           "filterLengthRange",
                           label    = "Filter length",
                           min      = 0L, max = 1L,
                           step     = 1L,
                           value    = defaultInput$filterLengthRange
                         ),
                         sliderInput(
                           "filterDoubleBondsRange",
                           label    = "Filter double bonds",
                           min      = 0L, max = 1L,
                           step     = 1L,
                           value    = defaultInput$filterDoubleBondsRange
                         ),
                         sliderInput(
                           "filterOhRange",
                           label    = "Filter hydroxylation",
                           min      = 0L, max = 1L,
                           step     = 1L,
                           value    = defaultInput$filterOhRange
                         )
                )
    )
  )}

# Body ####
uiBody <- function() {
  defaultInput <- defaultInput()
  shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
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
            downloadButton("saveMetaCSV", label = "Save metadata as .csv"),
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
                                  status = "primary",
                                  footer = "Brush and doubleclick to zoom",
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
                                                       "Show mean as line"          = "mean",
                                                       "Show value of means"        = "values",
                                                       "Show value of points"       = "ind_values",
                                                       "Transform y-axis log1p"     = "log",
                                                       "Show N per sample"          = "N",
                                                       "Label points"               = "label",
                                                       "Swap x- and y-axis"         = "swap",
                                                       "Free y scale for facets"    = "free_y",
                                                       "Run pairwise t-tests"       = "signif"
                                                     ),
                                                     selected = defaultInput$mainPlotAdditionalOptions
                                  ),
                                  selectInput("errorbarType", "Type of error bars",
                                              choices = list("SD", "SEM",  "95% CI" = "CI", "None"),
                                              selected = defaultInput$errorbarType 
                                  )
                                ),
                                shinydashboard::box(
                                  title = "Download",
                                  width = 3,
                                  downloadButton("saveMainPlot", label = "Save ggplot as .pdf"),
                                  downloadButton("saveMainPlotRDS", label = "Save ggplot as .RDS"),
                                  downloadButton("savePlotData", label = "Save points as .csv"),
                                  downloadButton("saveMeanPlotData", label = "Save means as .csv"),
                                  numericInput("mainWidth", label = "width", value = defaultInput$mainWidth),
                                  numericInput("mainHeight", label = "height", value = defaultInput$mainHeight)
                                ),
                                shinydashboard::box(
                                  title = "Pairwise comparisons",
                                  footer = "",
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
      
      # ** Dimensionality reduction ####
      shinydashboard::tabItem(
        tabName = "dimensionalityReduction",
        fluidRow(
          shinydashboard::box(
            title  = NULL,
            width  = 6,
            status = "primary",
            plotly::plotlyOutput("pcaScoresPlot")
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
                                plotOutput("pcaInfo")
            ),
            shinydashboard::box(width = NULL, title = "Download",
                                downloadButton("savePCAScores", label = "Save Score Plot as .pdf"),
                                downloadButton("savePCAScoresRDS", label = "Save Score Plot as .RDS"),
                                downloadButton("savePCALoadings", label = "Save Loadings Plot as .pdf"),
                                downloadButton("savePCALoadingsRDS", label = "Save Loadings Plot as .RDS"),
                                numericInput("pcaWidth", label = "width", value = defaultInput$pcaWidth),
                                numericInput("pcaHeight", label = "height", value = defaultInput$pcaHeight)
            )
          ),
          column(
            width = 6,
            shinydashboard::box(width = NULL,
                                checkboxInput("pcaCenter", "Center",  defaultInput$pcaCenter),
                                checkboxInput("pcaScale", "Scale dimensions before pca", defaultInput$pcaScale),
                                checkboxInput("pcaLabels", "Sample Labels", defaultInput$pcaLabels),
                                checkboxInput("pcaVectors", "Show original dimensions in principal component space",
                                              defaultInput$pcaVectors),
                                checkboxInput("drawPcaConvexHull", "Draw convex hull", FALSE),
                                sliderInput(
                                  "pcaNumberPrincipalComponents",
                                  label   = "Number of PCs",
                                  min     = 2,
                                  max     = 8,
                                  value   = defaultInput$pcaNumberPrincipalComponents,
                                  ticks   = TRUE,
                                  animate = FALSE
                                ),
                                sliderInput(
                                  "pcaPointSize",
                                  label   = "Point Size",
                                  min     = 1,
                                  max     = 10,
                                  value   = defaultInput$pcaPointSize,
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
                                  downloadButton("saveHeatmapRDS", label = "Save as .RDS"),
                                  numericInput("heatWidth", label = "width", value = defaultInput$heatWidth),
                                  numericInput("heatHeight", label = "height", value = defaultInput$heatHeight)
                                ),
                                shinydashboard::box(
                                  selectInput(
                                    "heatColor",
                                    "Color scheme",
                                    choices = list("viridis",
                                                   "magma",
                                                   "plasma",
                                                   "inferno",
                                                   "cividis"),
                                    selected = defaultInput$heatColor
                                  ),
                                  checkboxInput(
                                    "heatLogScale",
                                    "Logarithmic colorscale",
                                    value = defaultInput$heatLogScale
                                  ),
                                  sliderInput(
                                    "heatLabSize",
                                    label   = "Size of labels",
                                    min     = 1,
                                    max     = 30,
                                    value   = defaultInput$heatLabSize,
                                    ticks   = TRUE,
                                    animate = FALSE
                                  )
                                ))
      )
    )
  )
}

app_ui <- function() {
  shinydashboard::dashboardPage(
    skin    = "purple",
    title   = "ShinyLipids",
    header  = uiHeader(),
    sidebar = uiSidebar(),
    body    = uiBody()
  )
}

# Header ----------------------------------------------------------------------------------------------------------
header <- dashboardHeader(title = p("ShinyLipids", em("alpha")))


# Sidebar ---------------------------------------------------------------------------------------------------------
sidebar <- dashboardSidebar(
    # This part fixes the issue of having 2 scroll bars on one side:)
    tags$head(
        tags$style(HTML("
                      .sidebar {height: 90vh; overflow-y: auto; }
                      " )
        ),
        # As well as moving labels closer to their fields
        tags$style(HTML(
            "label { margin-bottom: 5px; }"
        ))
    ),

    shinyjs::useShinyjs(), # needed to load java script functions
    # The sidebar
    sidebarMenu(
        id = "menu",
        menuItem(
            "Database info", tabName = "metadata", icon = icon("th")
        ),
        selectInput("ID", label = "Dataset:", choices = ""),
        menuItem(
            "Visualization",
            tabName = "plotmenu",
            icon = icon("bar-chart"),
            menuSubItem("Main plot", tabName = "main"),
            menuSubItem("PCA", tabName = "PCA"),
            menuSubItem("Heatmap", tabName = "heatmap"),
            startExpanded = TRUE
        )
    ),


    # * Tables/Plot Options -------------------------------------------------------------------------------------------
    p("Visual mappings of your data:"),
    selectInput("aes_x", label = "Feature to display on x-Axis",
                choices = ""),
    selectInput("aes_y", label = "Feature to display on y-Axis / color value of heatmap",
                choices = "",
                selected = ),
    selectInput("aes_color", label = "Feature to display by color / y-axis of heatmap",
                choices = ""),
    selectInput("aes_pos", label = "Position of colored datapoints",
                choices = list(
                    "stacked" = "stacked",
                    "besides" = "dodge2",
                    "filled to 100%" = "fill"
                ),
                selected = "dodge2"
    ),
    selectInput("aes_facet1", label = "Feature to use for facetting 1",
                choices = ""),
    selectInput("aes_facet2", label = "Feature to use for facetting 2",
                choices = ""),
    selectizeInput(
        'std_feature', label = "Standardize to 100% within:",
        choices = list()
    ),
    p("Filtering data:"),
    selectizeInput(
        'sample_select',
        label = 'Select samples',
        choices = list(),
        multiple = TRUE
    ),
    selectizeInput(
        'sample_remove',
        label = 'Remove samples',
        choices = list(),
        multiple = TRUE
    ),
    selectizeInput(
        'rep_select',
        label = "Select replicates",
        choices = list(),
        multiple = TRUE
    ),
    selectizeInput(
        'rep_remove',
        label = "Remove replicates",
        choices = list(),
        multiple = TRUE
    ),
    selectizeInput(
        'filter_x',
        label = "Filter X",
        choices = list(),
        multiple = TRUE
    ),


    # * Impressum -----------------------------------------------------------------------------------------------------
    menuItem("About ", href = "http://bzh.db-engine.de/default.asp?lfn=2241&fg=4289"),
    div(
        strong("ShinyLipids 2015"),
        "by",
        br(),
        em("Mathias Gerl"),
        br(),
        em("Manuel Haußmann"),
        br(),
        em("Sebastian Bender"),
        br(),
        p("Rewritten by"),
        br(),
        em("Jannik Buhr"),
        br(),
        p("Version: 2.0"),
        br(),
        p("2018-08-04")
    )
)


# Body ------------------------------------------------------------------------------------------------------------
body <- dashboardBody(shinyjs::useShinyjs(),

                      # ** Database Info / Meta ---------------------------------------------------------------------------------------------------------
                      tabItems(
                          tabItem(
                              tabName = "metadata",
                              fluidRow(
                                  box(
                                      title = NULL,
                                      status = "primary",
                                      width = 12,
                                      DTOutput("metaDataTable")
                                  )
                              ),
                              fluidRow(
                                  box(
                                      title = NULL,
                                      checkboxInput("showFullMeta", label = "Show all columns", value = FALSE),
                                      downloadButton("saveMeta", label = "Save metadata as .csv"),
                                      downloadButton("saveRawRD", label = "Save raw data as .RData"),
                                      downloadButton("saveRawCSV", label = "Save raw data as .csv")
                                  )
                              ),

                              # *** Dataset Table -----------------------------------------------------------------------------------------------
                              fluidRow(
                                  box(
                                      title = "Selected Dataset",
                                      status = "primary",
                                      width = NULL,
                                      DTOutput("mainDataTable")
                                  )
                              )
                          ),

                          # ** Main plot -------------------------------------------------------------------------------------------------------
                          tabItem(tabName = "main",
                                  fluidRow(
                                      column(
                                          12,
                                          box(
                                              title = NULL,
                                              width = 12,
                                              height = 7,
                                              status = "primary",
                                              plotOutput(
                                                  "mainPlot",
                                                  dblclick = "mainPlot_dblclick",
                                                  brush = brushOpts(id = "mainPlot_brush",
                                                                    resetOnNew = TRUE)
                                              )
                                          )
                                      )
                                  ),
                                  fluidRow(
                                      column (
                                          width = 6,
                                          box(
                                              title = "Plotting details",
                                              id = "pdetails",
                                              width = 12,
                                              collapsible = TRUE,
                                              collapsed = FALSE,
                                              checkboxGroupInput("checkGroup", label = NULL,
                                                                 choices = "")
                                          )
                                      ),
                                      column (width = 6,
                                              fluidRow(
                                                  column(width = 6,
                                                         box(
                                                             title = NULL,
                                                             width = NULL,
                                                             selectInput(
                                                                 'plottype',
                                                                 label = 'Plottype',
                                                                 choices = list(
                                                                     "Barplot" = 'barplot',
                                                                     "Boxplot" = 'boxplot',
                                                                     "Points" = 'points',
                                                                     "Pointrange" = 'pointrange'
                                                                 )
                                                             )
                                                         )),
                                                  column(
                                                      width = 6,
                                                      box(
                                                          title = NULL,
                                                          width = NULL,
                                                          downloadButton("savePlotR", label = "Save .RData"),
                                                          downloadButton("savePlot", label = "Save as .pdf"),
                                                          downloadButton("savestddata", label = "Save as .csv"),
                                                          br(),
                                                          numericInput("mainWidth", label = "width", value = 20),
                                                          numericInput("mainHeight", label = "height", value = 10),
                                                          downloadButton("mainSave", label = "Save pdf")
                                                      )
                                                  )
                                              ),

                                              # *** summary ------------------------------------------------------------------------------------------------------
                                              fluidRow(column(
                                                  12,
                                                  box(
                                                      title = "Summary",
                                                      width = 12,
                                                      collapsible = TRUE,
                                                      collapsed = FALSE,
                                                      DT::dataTableOutput("sum_muMol")
                                                  )
                                              )))
                                  )),

                          # ** PCA ----------------------------------------------------------------------------------------------------------
                          tabItem(
                              tabName = "PCA",
                              fluidRow(
                                  box(
                                      title = NULL,
                                      width = 6,
                                      status = "primary",
                                      plotOutput("screeplot1", height = 140 * 2, width = 250 *
                                                     2)
                                  ),
                                  box(
                                      title = NULL,
                                      width = 6,
                                      status = "primary",
                                      plotOutput("screeplot2", height = 140 * 2, width = 250 *
                                                     2)
                                  ),
                                  box(
                                      title = NULL,
                                      width = 6,
                                      status = "primary",
                                      plotOutput("scores")
                                  ),
                                  box(
                                      title = NULL,
                                      width = 6,
                                      status = "primary",
                                      plotOutput("loadings")
                                  )
                              ),
                              fluidRow(
                                  box(title = NULL, width = 6,
                                      checkboxInput("center", "Center", TRUE),
                                      checkboxInput("labels", "Sample Labels", FALSE),
                                      selectInput(
                                          "scaling",
                                          "Scale",
                                          list(
                                              "none" = "none",
                                              "unit variance" = "uv",
                                              "pareto" = "pareto"
                                          )
                                      ),
                                      selectInput("method", "Method",
                                                  c("PCA Stuff")),
                                      selectInput("cv", "cross-validation",
                                                  list ("none" = "none", "Q2" =  "q2")),
                                      sliderInput(
                                          "PCs",
                                          label = "Number of PCs",
                                          min = 2,
                                          max = 8,
                                          value = 2,
                                          ticks = TRUE,
                                          animate = FALSE
                                      ),
                                      sliderInput(
                                          "size",
                                          label = "Point Size",
                                          min = 1,
                                          max = 8,
                                          value = 3,
                                          ticks = TRUE,
                                          animate = FALSE
                                      )
                                  ),
                                  column(
                                      width = 6,
                                      box(
                                          title = NULL,
                                          width = NULL,
                                          downloadButton("savescree1", label = "Save Scree Plot as .pdf"),
                                          downloadButton("savescree2", label = "Save Scree Plot as .pdf"),
                                          downloadButton("saveScores", label = "Save Score Plot as .pdf"),
                                          downloadButton("saveLoadings", label = "Save Loadings Plot as .pdf"),
                                          numericInput("pcaWidth", label = "width", value = 20),
                                          numericInput("pcaHeight", label = "height", value = 10)
                                      )
                                  )
                              )
                          ),

                          # ** heatmap ------------------------------------------------------------------------------------------------------
                          tabItem(tabName = "heatmap",
                                  fluidRow(column(
                                      12,
                                      box(
                                          title = NULL,
                                          width = 12,
                                          status = "primary",
                                          plotOutput("heatmap")
                                      )
                                  ),
                                  column(
                                      width = 6,
                                      box(
                                          title = NULL,
                                          width = NULL,
                                          downloadButton("heatSave", label = "Save as .pdf"),
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
                                              max = 50,
                                              value = 20,
                                              ticks = TRUE,
                                              animate = FALSE
                                          ),
                                          numericInput("heatWidth", label = "width", value = 20),
                                          numericInput("heatHeight", label = "height", value = 10)
                                      )
                                  )))
                      )
)


# Initiate Dashboard page ----------------------------------------------------------------------------------------
dashboardPage(skin = "green",
              title = "ShinyLipids",
              header,
              sidebar,
              body)

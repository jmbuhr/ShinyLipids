# Header ----------------------------------------------------------------------------------------------------------
header <- dashboardHeader(title = p("ShinyLipids", em("alpha")))


# Sidebar ---------------------------------------------------------------------------------------------------------
sidebar <- dashboardSidebar(

    # *** Visual fixes ------------------------------------------------------------------------------------------------
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


    sidebarMenu(
        # * MetaData ------------------------------------------------------------------------------------------------------
        id = "menu",
        menuItem(
            "Database info", tabName = "metadata", icon = icon("th"),
            startExpanded = TRUE
        ),

        # * Visualization -------------------------------------------------------------------------------------------------
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
    tabsetPanel(type = "pills",
                tabPanel("Mapping",
                         selectInput("aes_x", label = "Feature to display on x-Axis",
                                     choices = features),
                         selectInput("aes_y", label = "Feature to display on y-Axis / color value of heatmap",
                                     choices = features,
                                     selected = ),
                         selectInput("aes_color", label = "Feature to display by color / y-axis of heatmap",
                                     choices = features),
                         selectInput("aes_pos", label = "Position of colored datapoints",
                                     choices = list(
                                         "stacked" = "stacked",
                                         "besides" = "dodge2",
                                         "filled to 100%" = "fill"
                                     ),
                                     selected = "dodge2"
                         ),
                         selectInput("aes_facet1", label = "Feature to use for facetting 1",
                                     choices = features),
                         selectInput("aes_facet2", label = "Feature to use for facetting 2",
                                     choices = features),
                         selectizeInput(
                             'std_feature', label = "Standardize to 100% within:",
                             choices = features
                         )
                ),
                tabPanel("Defaults"),
                tabPanel("Samples",
                         selectizeInput(
                             'sample_select',
                             label = 'Select samples',
                             options = list(placeholder = "Not selecting a sample here will keep all samples"),
                             choices = NULL,
                             multiple = TRUE
                         ),
                         selectizeInput(
                             'sample_remove',
                             label = 'Remove samples',
                             choices = NULL,
                             multiple = TRUE
                         ),
                         selectizeInput(
                             'rep_select',
                             label = "Select replicates",
                             options = list(placeholder = "Not selecting a replicate here will keep them all"),
                             choices = NULL,
                             multiple = TRUE
                         ),
                         selectizeInput(
                             'rep_remove',
                             label = "Remove replicates",
                             choices = NULL,
                             multiple = TRUE
                         )
                ),
                tabPanel("Filters",
                         selectizeInput(
                             'filter_cat',
                             label = "Filter category",
                             options = list(placeholder = "empty field means no filtering based on this feature"),
                             choices = NULL,
                             multiple = TRUE
                         ),
                         selectizeInput(
                             'filter_func',
                             label = "Filter functional category",
                             options = list(placeholder = "empty field means no filtering based on this feature"),
                             choices = NULL,
                             multiple = TRUE
                         ),
                         selectizeInput(
                             'filter_class',
                             label = "Filter class",
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
                             label = "Filter double bounds",
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
        strong("ShinyLipids alpha"),
        p("Rewritten by"),
        em("Jannik Buhr"),
        p("Version: 2.0"),
        p("2018-08-04"),
        p("includes code from:"),
        br(),
        em("Mathias Gerl"),
        br(),
        em("Manuel Haußmann"),
        br(),
        em("Sebastian Bender"),
        br()

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
                                      DTOutput("metaDataTable"),
                                      # Save buttons
                                      checkboxInput("showFullMeta", label = "Show all columns", value = FALSE),
                                      selectInput("ID",
                                                  label = "Select dataset by clicking on the table or use this dropdown list",
                                                  choices = "",width = "50%"),
                                      downloadButton("saveMeta", label = "Save metadata as .csv"),
                                      downloadButton("saveRawCSV", label = "Save selected dataset as .csv (unfiltered)"),
                                      downloadButton("saveMainCSV", label = "Save selected dataset as .csv (filtered)")
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
                                              status = "primary",
                                              plotOutput("mainPlot",
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
                                              checkboxGroupInput("main_checkGroup", label = NULL,
                                                                 choices = "")
                                          )
                                      ),
                                      column(width = 6,
                                             fluidRow(
                                                 column(width = 6,
                                                        box(
                                                            title = NULL,
                                                            width = NULL,
                                                            selectInput(
                                                                'main_plottype',
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
                                                         downloadButton("main_savePlot", label = "Save as .pdf"),
                                                         downloadButton("main_saveStd", label = "Save as .csv"),
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
                                      plotOutput("pca_screeplot1", height = 140 * 2, width = 250 *
                                                     2)
                                  ),
                                  box(
                                      title = NULL,
                                      width = 6,
                                      status = "primary",
                                      plotOutput("pca_screeplot2", height = 140 * 2, width = 250 *
                                                     2)
                                  ),
                                  box(
                                      title = NULL,
                                      width = 6,
                                      status = "primary",
                                      plotOutput("pca_scores")
                                  ),
                                  box(
                                      title = NULL,
                                      width = 6,
                                      status = "primary",
                                      plotOutput("pca_loadings")
                                  )
                              ),
                              fluidRow(
                                  box(title = NULL, width = 6,
                                      checkboxInput("pca_center", "Center", TRUE),
                                      checkboxInput("pca_labels", "Sample Labels", FALSE),
                                      selectInput(
                                          "scaling",
                                          "Scale",
                                          list(
                                              "none" = "none",
                                              "unit variance" = "uv",
                                              "pareto" = "pareto"
                                          )
                                      ),
                                      selectInput("pca_method", "Method",
                                                  c("PCA Stuff")),
                                      selectInput("pca_cv", "cross-validation",
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
                                          "pca_pointSize",
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
                                          downloadButton("pca_saveScree1", label = "Save Scree Plot as .pdf"),
                                          downloadButton("pca_saveScree2", label = "Save Scree Plot as .pdf"),
                                          downloadButton("pca_saveScores", label = "Save Score Plot as .pdf"),
                                          downloadButton("pca_saveLoadings", label = "Save Loadings Plot as .pdf"),
                                          numericInput("pca_Width", label = "width", value = 20),
                                          numericInput("pca_Height", label = "height", value = 10)
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
                                          plotOutput("heatPlot")
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
dashboardPage(skin = "purple",
              title = "ShinyLipids",
              header,
              sidebar,
              body)

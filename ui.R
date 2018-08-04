# Header ----------------------------------------------------------------------------------------------------------
header <- dashboardHeader(title = p("ShinyLipids", em("alpha")))


# Sidebar ---------------------------------------------------------------------------------------------------------
sidebar <- dashboardSidebar(
    shinyjs::useShinyjs(), # needed to load the necessary java script functions
    sidebarMenu(
        id = "menu",
        menuItem(
            "Database info", tabName = "metadata", icon = icon("th")
        ),
        selectInput("ID", label = "Database:", choices = ""),
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
    selectInput("aes_x", label = "Feature to display on x-Axis",
                choices = ""),
    selectInput("aes_y", label = "Feature to display on y-Axis",
                choices = list("µM / mol%" = "xval"),
                selected = "xval"),
    selectInput("aes_color", label = "Feature to display by color",
                choices = ""),
    selectInput("aes_pos", label = "Feature to display by color",
                choices = ""),
    selectInput("aes_facet", label = "Feature to use for facetting",
                choices = ""),
    selectizeInput(
        'samplebase',
        label = 'Sample to use as base level',
        choices = list(),
        multiple = TRUE
    ),
    selectizeInput(
        'samplesub',
        label = 'Subset Samples',
        choices = list(),
        multiple = TRUE
    ),
    selectizeInput(
        'withsub',
        label = 'Subset Within',
        choices = list(),
        multiple = TRUE
    ),
    selectizeInput(
        'repsub',
        label = "Select Replicate",
        choices = list(),
        multiple = TRUE
    ),
    selectizeInput(
        'techsub',
        label = "Remove Technical Replicate",
        choices = list(),
        multiple = TRUE
    ),


    # * Impressum -----------------------------------------------------------------------------------------------------
    menuItem("About ", href = "http://bzh.db-engine.de/default.asp?lfn=2241&fg=4289"),
    div(
        strong("ShinyLipids 2015"),
        "by",
        br(),
        em("Mathias Gerl,"),
        br(),
        em("Manuel Haußmann"),
        br(),
        em("Sebastian Bender"),
        br(),
        p("Rewritten by"),
        em("Jannik Buhr"),
        br(),
        ("Version: 2.0"),
        ("2018-08-04")
    )
)


# Body ------------------------------------------------------------------------------------------------------------
body <-
    dashboardBody(shinyjs::useShinyjs(),

                  # ** Database Info / Meta ---------------------------------------------------------------------------------------------------------
                  tabItems(
                      tabItem(
                          tabName = "metadata",
                          DT::dataTableOutput("metadataTable"),
                          fluidRow(
                              box(
                                  title = NULL,
                                  width = 7,
                                  selectInput("downid", label = "Select dataset to download",
                                              choices = ""),
                                  downloadButton("savemeta", label = "Save metadata as .csv"),
                                  downloadButton("saveRawRD", label = "Save raw data as .RData"),
                                  downloadButton("saveRawCSV", label = "Save raw data as .csv")
                              )
                          )
                      ),

                      # ** Main plot -------------------------------------------------------------------------------------------------------
                      tabItem(tabName = "main",
                              fluidRow(column(
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
                              )),
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
                                                      downloadButton("saveplotR", label = "Save .RData"),
                                                      # &#39; is needed for now to avoid a bug when using single ' in shinyBS::bsTooltip
                                                      shinyBS::bsTooltip(
                                                          id = "saveplotR",
                                                          placement = "top",
                                                          title = paste0(
                                                              "Save the current plot as &#39;lipidplot&#39; ",
                                                              "and the subset to create ",
                                                              "it as &#39;lipidsubset&#39; in an .RData file"
                                                          )
                                                      ),
                                                      downloadButton("saveplot", label = "Save as .pdf"),
                                                      shinyBS::bsTooltip(
                                                          id = "saveplot",
                                                          placement = "top",
                                                          title = "Save the current plot as a .pdf file"
                                                      ),
                                                      downloadButton("savestddata", label = "Save as .csv"),
                                                      shinyBS::bsTooltip(
                                                          id = "savestddata",
                                                          placement = "top",
                                                          title = "Save the data used to compute the plot as a .csv file"
                                                      ),
                                                      br(),
                                                      a(id = "toggleAdvanced", "Show/hide advanced options"),
                                                      shinyjs::hidden(
                                                          div(
                                                              id = "advanced",
                                                              numericInput("width", label = "width", value = 20),
                                                              numericInput("height", label = "height", value = 10),
                                                              downloadButton("save", label = "Save"),
                                                              shinyBS::bsTooltip(
                                                                  id = 'save',
                                                                  placement = "top",
                                                                  title = "Save plot with custom width/height parameters"
                                                              )
                                                          )
                                                      )
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
                                                  collapsed = TRUE,
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
                                      shinyBS::bsTooltip(
                                          id = "savescree1",
                                          placement = "top",
                                          title = "Save the current Scree Plot as a .pdf file"
                                      ),
                                      downloadButton("savescree2", label = "Save Scree Plot as .pdf"),
                                      shinyBS::bsTooltip(
                                          id = "savescree2",
                                          placement = "top",
                                          title = "Save the current Scree Plot as a .pdf file"
                                      ),
                                      downloadButton("saveScores", label = "Save Score Plot as .pdf"),
                                      shinyBS::bsTooltip(
                                          id = "saveScores",
                                          placement = "top",
                                          title = "Save the current Score Plot as a .pdf file"
                                      ),
                                      downloadButton("saveLoadings", label = "Save Loadings Plot as .pdf"),
                                      shinyBS::bsTooltip(
                                          id = "saveLoadings",
                                          placement = "top",
                                          title = "Save the current Loadings Plot as a .pdf file"
                                      ),
                                      numericInput("widthPCA", label = "width", value = 20),
                                      numericInput("heightPCA", label = "height", value = 10)
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
                                      downloadButton("saveheatmap", label = "Save as .pdf"),
                                      shinyBS::bsTooltip(
                                          id = "saveheatmap",
                                          placement = "top",
                                          title = "Save the current heatmap as a .pdf file"
                                      ),
                                      selectInput(
                                          "hmap_color",
                                          "Color scheme",
                                          choices = list("viridis",
                                                         "magma",
                                                         "plasma",
                                                         "inferno",
                                                         "cividis")
                                      ),
                                      sliderInput(
                                          "heatlabsz",
                                          label = "Size of labels",
                                          min = 1,
                                          max = 50,
                                          value = 20,
                                          ticks = TRUE,
                                          animate = FALSE
                                      ),
                                      numericInput("widthheat", label = "width", value = 20),
                                      numericInput("heightheat", label = "height", value = 10)
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

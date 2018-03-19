library(shinydashboard)

##### Specification of the Header
header <- dashboardHeader(title = "ShinyLipids")

##### Specification of the Sidebar
sidebar <- dashboardSidebar(
  shinyjs::useShinyjs(), # needed to load the necessary java script functions
  sidebarMenu(id = "menu",
    menuItem("Metadata", tabName = "metadata", icon = icon("th")
    ),
    menuItem("Plots", tabName = "plotmenu", icon = icon("bar-chart"),
             menuSubItem("Plots", tabName = "plots"),
             menuSubItem("PCA", tabName = "PCA"),
             menuSubItem("heatmap", tabName = "heatmap"),
             # menuSubItem("Pathway Analysis", tabName = "pathway"),
             menuSubItem("Advanced Options", tabName = "advanced")
    )
  ),

  ### The Options for tables/plots
    selectInput("ID", label = "Which database?", choices = rev(datanames)),
    selectInput("what", label = "What should be displayed?",
                 choices = whatnames),
    conditionalPanel("input.what == 'chains' | input.what == 'chain_sums' |
                      input.what == 'length' | 
                      input.what == 'db'     | input.what == 'oh'",
      selectInput("within", label = "Within which group should be summarized?",
                  choices = withinnames)
    ),
    selectInput("standard", label = "Standardized to which group?",
                  choices = standardnames),
    selectInput('samplebase', label = 'Sample to use as base level',  
                  choices = list(), multiple = TRUE),
    selectInput('samplesub', label = 'Subset Samples',  
                  choices = list(), multiple = TRUE),
    selectInput('whatsub', label = 'Subset What',  
                choices = list(), multiple = TRUE),
    selectInput('withsub', label = 'Subset Within', 
                choices = list(), multiple = TRUE),
    selectInput('repsub', label = "Select Replicate",
                choices = list(), multiple = TRUE),
    selectInput('techsub', label = "Remove Technical Replicate",
                choices = list(), multiple = TRUE),
  
  

  
  menuItem("About ", href = "http://bzh.db-engine.de/default.asp?lfn=2241&fg=4289"),
  div(strong("ShinyLipids 2015"), "by",
      br(),
      em("Mathias Gerl,"),
      br(),
      em("Manuel Haußmann"),
      br(),
      em("Sebastian Bender"),
      br(),
      ("Version: 1.0"),
      ("Last Update: 23.08.2017"))
  
)
##### Specification of the Body
body <- dashboardBody(
  shinyjs::useShinyjs(), # needed to load the necessary java script functions
  
  tabItems(
    tabItem(tabName = "metadata",
            DT::dataTableOutput("metadataTable"),
      fluidRow(
        box(title = NULL, width = 3,
            downloadButton("savemeta", label = "Save as .csv"),
            checkboxInput("fulltable", label = "View the complete table", value = FALSE)
        ),
        box(title = NULL, width = 3,
            selectInput("downid", label = "Select dataset to download",
                        choices = datanames),
            downloadButton("downdata", label = "Save as .RData"),
            bsTooltip(id = "downdata", placement = "top",
                      "Note: The downloaded object is called &#39;dataset&#39;")),
            actionButton("refresh", "refresh Metadata", icon = icon("refresh"))
      )
    ),
    tabItem(tabName = "advanced",
            fluidRow(
              box(title = "Advanced Plotting Options",
                  "Do you want to choose the subset to be plotted by adding or removing?",
                  checkboxInput('add_rem', label = "Add in subsetselection",
                                value = TRUE),
                  checkboxInput('stdSub', label = "Standardize on Visible",
                                value = FALSE),
                  checkboxInput('highlight', label = "Highlight value points in barplot",
                                value = TRUE),
#                   checkboxInput('compSpec', label = "Allow complete (Sum) Species Plot",
#                                 value = FALSE),
                  selectInput('symbolchoice', label = "Identify individuals in the plot",
                              choices = list("via sample replicate" = "letters",
                                             "via symbols" = "symbols"))
              )
            )
    ),
    tabItem(tabName = "PCA",
      fluidRow(
        column(12,
          # box(title = NULL, width = 12, status = "primary",
          #   conditionalPanel("input.what == 'class' && input.standard == 'Class'",
          #                      "Why would you want to do this?"),
          #   plotOutput("plotPCA",
          #              dblclick = "plot1_dblclick",
          #              brush = brushOpts(
          #                id = "plot1_brush",
          #                resetOnNew = TRUE
          #              )
          #   )
          # ),
          box(title = NULL, width = 6, status = "primary",
            checkboxInput("center","Center",TRUE),
            checkboxInput("labels","Sample Labels",FALSE),
            selectInput("scaling","Scale",
                        list(none = "none", "unit variance" = "uv", pareto = "pareto")
            ),
            
            selectInput("method","Method",
                        namel(listPcaMethods())
            ),
            selectInput("cv","cross-validation",
                        list (none = "none", Q2 =  "q2")
            ),
            sliderInput("PCs", label="Number of PCs", min=2, max=8, value=2, ticks = TRUE, animate = FALSE),
            # sliderInput("prob", label="Ellipse Probability", min=0.01, max=1, value=0.68, ticks = TRUE, animate = FALSE),
            sliderInput("size", label="Point Size", min=1, max=8, value=3, ticks = TRUE, animate = FALSE)
            # selectInput("refsample","Reference Sample",samplenames)
            
          ),
          # box(title = NULL, width = 3,
          #     selectInput('samplebase', label = 'Sample to use as base level',  
          #                 choices = list(), multiple = TRUE)
          # ),
          box(title = NULL, width = 6, status = "primary",
              # conditionalPanel("input.what == 'class' && input.standard == 'Class'",
              #                  "Why would you want to do this?"),
              plotOutput("screeplot1",height = 140*2, width = 250*2)
          ),
          box(title = NULL, width = 6, status = "primary",
              # conditionalPanel("input.what == 'class' && input.standard == 'Class'",
              #                  "Why would you want to do this?"),
              plotOutput("screeplot2",height = 140*2, width = 250*2)
          ),
          box(title = NULL, width = 6, status = "primary",
              # conditionalPanel("input.what == 'class' && input.standard == 'Class'",
              #                  "Why would you want to do this?"),
              plotOutput("scores")
          ),
          box(title = NULL, width = 6, status = "primary",
              # conditionalPanel("input.what == 'class' && input.standard == 'Class'",
              #                  "Why would you want to do this?"),
              plotOutput("loadings")
          )
        ),
        column(width = 6,
               box(title = NULL, width = NULL, 
                   downloadButton("savescree1", label = "Save Scree Plot as .pdf"),
                   bsTooltip(id = "savescree1",  placement = "top",
                             title = "Save the current Scree Plot as a .pdf file"),
                   downloadButton("savescree2", label = "Save Scree Plot as .pdf"),
                   bsTooltip(id = "savescree2",  placement = "top",
                             title = "Save the current Scree Plot as a .pdf file"),
                   downloadButton("saveScores", label = "Save Score Plot as .pdf"),
                   bsTooltip(id = "saveScores",  placement = "top",
                             title = "Save the current Score Plot as a .pdf file"),
                   downloadButton("saveLoadings", label = "Save Loadings Plot as .pdf"),
                   bsTooltip(id = "saveLoadings",  placement = "top",
                             title = "Save the current Loadings Plot as a .pdf file"),
                   numericInput("widthPCA", label = "width", value = 20),
                   numericInput("heightPCA", label = "height", value = 10)
               )
               
        )
      )
    ),
    
tabItem(tabName = "heatmap",
        fluidRow(
          column(12,
                 # box(title = NULL, width = 12, status = "primary",
                 #   conditionalPanel("input.what == 'class' && input.standard == 'Class'",
                 #                      "Why would you want to do this?"),
                 #   plotOutput("plotPCA",
                 #              dblclick = "plot1_dblclick",
                 #              brush = brushOpts(
                 #                id = "plot1_brush",
                 #                resetOnNew = TRUE
                 #              )
                 #   )
                 # ),
                 checkboxInput("heatscale","Scale color key",FALSE),
                 sliderInput("heatmin", label="Minimum", min=-10, max=10, value=-7, ticks = TRUE, animate = FALSE,step=0.1),
                 sliderInput("heatmedmin", label="Size of labels", min=-10, max=10, value=-4, ticks = TRUE, animate = FALSE,step=0.1),
                 sliderInput("heatmedium", label="Medium", min=-10, max=10, value=0, ticks = TRUE, animate = FALSE,step=0.1),
                 sliderInput("heatmedmax", label="Size of labels", min=-10, max=10, value=4, ticks = TRUE, animate = FALSE,step=0.1),
                 sliderInput("heatmax", label="Maximum", min=-10, max=10, value=7, ticks = TRUE, animate = FALSE,step=0.1),
                 selectInput('heatcolscheme', label = "Heatmap Color Scheme",
                            choices = list("Heat Colors" = "heatcolors",
                                           "Blue White Violet" = "bluewhiteviolet",
                                           "White Green Black" = "whitegreenblack",
                                           "Blue White Red" = "bluewhitered")),
                 box(title = NULL, width = 12, status = "primary",
                     # conditionalPanel("input.what == 'class' && input.standard == 'Class'",
                     #                  "Why would you want to do this?"),
                     plotOutput("heatmap")
                 )
          ),
          column(width = 6,
                 box(title = NULL, width = NULL, 
                     downloadButton("saveheatmap", label = "Save as .pdf"),
                     bsTooltip(id = "saveheatmap",  placement = "top",
                               title = "Save the current heatmap as a .pdf file"),
                     sliderInput("heatlabsz", label="Size of labels", min=0.1, max=5, value=2, ticks = TRUE, animate = FALSE),
                     numericInput("widthheat", label = "width", value = 20),
                     numericInput("heightheat", label = "height", value = 10),
                     numericInput("heatmarx", label = "bottom Margin", value = 10),
                     numericInput("heatmary", label = "side Margin", value = 10)
                 ) 
          )
          
        )
),

# tabItem(tabName = "pathway",
#         fluidRow(
#           column(12,
#                  # box(title = NULL, width = 12, status = "primary",
#                  #   conditionalPanel("input.what == 'class' && input.standard == 'Class'",
#                  #                      "Why would you want to do this?"),
#                  #   plotOutput("plotPCA",
#                  #              dblclick = "plot1_dblclick",
#                  #              brush = brushOpts(
#                  #                id = "plot1_brush",
#                  #                resetOnNew = TRUE
#                  #              )
#                  #   )
#                  # ),
#                  box(title = NULL, width = 3, status = "primary",
#                      checkboxInput("center","Center",TRUE),
#                      checkboxInput("labels","Sample Labels",FALSE),
#                      selectInput("scaling","Scale",
#                                  list(none = "none", "unit variance" = "uv", pareto = "pareto")
#                      ),
#                      
#                      selectInput("method","Method",
#                                  namel(listPcaMethods())
#                      ),
#                      selectInput("cv","cross-validation",
#                                  list (none = "none", Q2 =  "q2")
#                      ),
#                      sliderInput("PCs", label="Number of PCs", min=2, max=8, value=2, ticks = TRUE, animate = FALSE),
#                      sliderInput("prob", label="Ellipse Probability", min=0.01, max=1, value=0.68, ticks = TRUE, animate = FALSE)
#                      
#                  ),
#                  box(title = NULL, width = 9, status = "primary",
#                      conditionalPanel("input.what == 'class' && input.standard == 'Class'",
#                                       "Why would you want to do this?"),
#                      plotOutput("pathway")
#                  )
#           )
#         )
# ),
            
      
    tabItem(tabName = "plots", 
      # fluidRow(
      #   box(title = NULL, width = 2,
      #     selectInput('samplesub', label = 'Subset Samples',  
      #               choices = list(), multiple = TRUE)
      #   ),
      #   box(title = NULL, width = 2,
      #       selectInput('whatsub', label = 'Subset What',  
      #                   choices = list(), multiple = TRUE)
      #   ),
      #   box(title = NULL, width = 2,
      #       conditionalPanel("input.within == 'Category' | input.within == 'Functional Category' | input.within == 'Class'",
      #           selectInput('withsub', label = 'Subset Within', 
      #                        choices = list(), multiple = TRUE)
      #       )  
      #   ),
      #   box(title = NULL, width = 2,
      #     selectInput('repsub', label = "Select Replicate",
      #                 choices = list(), multiple = TRUE)
      #   ),
      #   box(title = NULL, width = 3,
      #       selectInput('techsub', label = "Remove Technical Replicate",
      #                   choices = list(), multiple = TRUE)
      #       )
      # ),
      fluidRow(
        column(12,
               # status = "primary" sets darkblue background
          box(title = NULL, width = 12, status = "primary",
              # A bunch of error messages, due to bad selections
              # conditionalPanel("input.what == 'class' && input.standard == 'Class'",
              #                   "Why would you want to do this?"),
              plotOutput("plot1",
                         dblclick = "plot1_dblclick",
                         brush = brushOpts(
                           id = "plot1_brush",
                           resetOnNew = TRUE
                         ),
                         height = "600px"
              )
              # plotlyOutput("plot1")
        )
        )
      ),
      fluidRow(
        column (width = 6,
            box(title = "Plotting details", id = "pdetails", width = 12,
                collapsible = TRUE, collapsed = FALSE,
                checkboxGroupInput("checkGroup", label = NULL,
                    choices = plotchoices)
            )
        ),
        column (width = 6,
          fluidRow(
            column(width = 6,
                   box(title = NULL, width = NULL,
                       selectInput('plottype', label = 'Plottype',
                                   choices = list("Barplot" = 'barplot',
                                                  "Boxplot" = 'boxplot',
                                                  "Rawplot" = 'rawplot',
                                                  "Pointrange" = 'pointrange')),
                       bsTooltip('plottype', placement = "top",
                                 title = "Construct a barplot, boxplot or plot the raw values")
                   )
            ),
            column(width = 6,
                   box(title = NULL, width = NULL, 
                       downloadButton("saveplotR", label = "Save .RData"),
                       # &#39; is needed for now to avoid a bug when using single ' in bsTooltip
                       bsTooltip(id = "saveplotR",  placement = "top",
                                 title = paste0("Save the current plot as &#39;lipidplot&#39; ",
                                                "and the subset to create ",
                                                "it as &#39;lipidsubset&#39; in an .RData file")),
                       downloadButton("saveplot", label = "Save as .pdf"),
                       bsTooltip(id = "saveplot",  placement = "top",
                                 title = "Save the current plot as a .pdf file"),
                       downloadButton("savestddata", label = "Save as .csv"),
                       bsTooltip(id = "savestddata",  placement = "top",
                                 title = "Save the data used to compute the plot as a .csv file"),
                       br(),
                       a(id = "toggleAdvanced", "Show/hide advanced options"),
                       shinyjs::hidden(
                         div(id = "advanced",
                             numericInput("width", label = "width", value = 20),
                             numericInput("height", label = "height", value = 10),
                             downloadButton("save", label = "Save"),
                             bsTooltip(id = 'save',  placement = "top",
                                       title = "Save plot with custom width/height parameters")
                         )              
                       )  
                   ) 
            )
          ), fluidRow(
            column(12,
                   box(title = "Summary", width = 12, collapsible = TRUE,
                       collapsed = TRUE, 
                       DT::dataTableOutput("sum_muMol"))
            )
        )
        )
      )
    )
  )
)

dashboardPage( skin = "green",
  header,
  sidebar,
  body
)

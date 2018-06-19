# Header ----------------------------------------------------------------------------------------------------------
header <- dashboardHeader(title = "ShinyLipids")


# Sidebar ---------------------------------------------------------------------------------------------------------
sidebar <- dashboardSidebar(
    shinyjs::useShinyjs(), # needed to load the necessary java script functions
    sidebarMenu(id = "menu",
                menuItem("Metadata", tabName = "metadata", icon = icon("th")
                ),
                menuItem("Plots", tabName = "plotmenu", icon = icon("bar-chart"),
                         menuSubItem("Plots", tabName = "plots"),
                         menuSubItem("PCA", tabName = "PCA"),
                         menuSubItem("heatmap", tabName = "heatmap"),
                         menuSubItem("Advanced Options", tabName = "advanced")
                )
    ),


    # * Tables/Plot Options -------------------------------------------------------------------------------------------
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


    # * Impressum -----------------------------------------------------------------------------------------------------
    menuItem("About ", href = "http://bzh.db-engine.de/default.asp?lfn=2241&fg=4289"),
    div(strong("ShinyLipids 2015"), "by",
        br(),
        em("Mathias Gerl,"),
        br(),
        em("Manuel Haußmann"),
        br(),
        em("Sebastian Bender"),
        br(),
        em("Jannik Buhr"),
        br(),
        ("Version: 1.0.5"),
        ("Last Update: 15.06.2018"))
)



# Body ------------------------------------------------------------------------------------------------------------
body <- dashboardBody(
    shinyjs::useShinyjs(), # needed to load the necessary java script functions

    # ** Meta ---------------------------------------------------------------------------------------------------------
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

        # ** Advanced plotting options ------------------------------------------------------------------------------------
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
                        selectInput('symbolchoice', label = "Identify individuals in the plot",
                                    choices = list("via sample replicate" = "letters",
                                                   "via symbols" = "symbols"))
                    )
                )
        ),

        # ** PCA ----------------------------------------------------------------------------------------------------------
        tabItem(tabName = "PCA",
                fluidRow(
                    column(12,
                           box(title = NULL, width = 6, status = "primary",
                               checkboxInput("center","Center",TRUE),
                               checkboxInput("labels","Sample Labels",FALSE),
                               selectInput("scaling","Scale",
                                           list("none" = "none", "unit variance" = "uv", "pareto" = "pareto")
                               ),

                               selectInput("method","Method",
                                           namel(listPcaMethods())
                               ),
                               selectInput("cv","cross-validation",
                                           list ("none" = "none", "Q2" =  "q2")
                               ),
                               sliderInput("PCs", label="Number of PCs", min=2, max=8, value=2, ticks = TRUE, animate = FALSE),
                               sliderInput("size", label="Point Size", min=1, max=8, value=3, ticks = TRUE, animate = FALSE)
                           ),
                           box(title = NULL, width = 6, status = "primary",
                               plotOutput("screeplot1",height = 140*2, width = 250*2)
                           ),
                           box(title = NULL, width = 6, status = "primary",
                               plotOutput("screeplot2",height = 140*2, width = 250*2)
                           ),
                           box(title = NULL, width = 6, status = "primary",
                               plotOutput("scores")
                           ),
                           box(title = NULL, width = 6, status = "primary",
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


        # ** heatmap ------------------------------------------------------------------------------------------------------
        tabItem(tabName = "heatmap",
                fluidRow(
                    column(12,
                           box(title = NULL, width = 12, status = "primary",
                               plotOutput("heatmap")
                           )
                    ),
                    column(width = 6,
                           box(title = NULL, width = NULL,
                               downloadButton("saveheatmap", label = "Save as .pdf"),
                               bsTooltip(id = "saveheatmap",  placement = "top",
                                         title = "Save the current heatmap as a .pdf file"),
                               selectInput("hmap_color", "Color scheme",
                                           choices = list(
                                               "viridis",
                                               "magma",
                                               "plasma",
                                               "inferno",
                                               "cividis"
                                           )
                               ),
                               sliderInput("heatlabsz", label="Size of labels", min= 1,
                                           max= 50, value= 20, ticks = TRUE, animate = FALSE),
                               numericInput("widthheat", label = "width", value = 20),
                               numericInput("heightheat", label = "height", value = 10)
                           )
                    )
                )
        ),

        # ** plots -------------------------------------------------------------------------------------------------------
        tabItem(tabName = "plots",
                fluidRow(
                    column(12,
                           box(title = NULL, width = 12, status = "primary",
                               plotOutput("plot1",
                                          dblclick = "plot1_dblclick",
                                          brush = brushOpts(
                                              id = "plot1_brush",
                                              resetOnNew = TRUE
                                          ),
                                          height = "600px"
                               )
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
                            ),

                            # ** summary ------------------------------------------------------------------------------------------------------
                            fluidRow(
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



# Initiatde Dashboard page ----------------------------------------------------------------------------------------
dashboardPage( skin = "green",
               header,
               sidebar,
               body
)

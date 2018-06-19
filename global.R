# Preamble --------------------------------------------------------------------------------------------------------
# Packages
## Data management
library(jsonlite)
library(RSQLite)
library(lazyeval)
library(dbplyr)  # newly inserted
library(plyr)
library(dplyr) # dplyr needs to be loaded after plyr
library(tidyr)
library(reshape2)
## Shiny
library(shinyBS)
library(shinyjs)
library(shiny)
library(DT) # needs to be loaded after shiny
library(shinythemes)
library(shinydashboard)
## Plotting
library(gridExtra)
library(ggplot2)
library(ggbiplot)
library(RColorBrewer)
library(viridis)
library(ggsignif)
## Bioconductor Packages
# source("http://bioconductor.org/biocLite.R")
# biocLite("pcaMethods")
library(Biobase)
library(BiocGenerics)
library(pcaMethods)

# sessionInfo()
# R version 3.4.4 (2018-03-15)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows >= 8 x64 (build 9200)
#
# Matrix products: default
#
# locale:
#     [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252
# [4] LC_NUMERIC=C                           LC_TIME=English_United States.1252
#
# attached base packages:
#     [1] parallel  grid      stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:
#     [1] bindrcpp_0.2.2       pcaMethods_1.70.0    Biobase_2.38.0       BiocGenerics_0.24.0  ggsignif_0.4.0
# [6] viridis_0.5.1        viridisLite_0.3.0    RColorBrewer_1.1-2   ggbiplot_0.55        scales_0.5.0
# [11] ggplot2_2.2.1        gridExtra_2.3        shinydashboard_0.7.0 shinythemes_1.1.1    DT_0.4
# [16] shinyjs_1.0          shinyBS_0.61         reshape2_1.4.3       tidyr_0.8.1          dplyr_0.7.5
# [21] plyr_1.8.4           dbplyr_1.2.1         lazyeval_0.2.1       RSQLite_2.1.1        jsonlite_1.5

# Database Connection functions -----------------------------------------------------------------------------------

# from Database.R
# A collection of functions that prepare the data for the plotting

samplecols <- list("sample", "sample_replicate")


#* Get Raw Data ----------------------------------------------------------------------------------------------------
# This function takes an ID and a string representing a table
# and returns a "link" to the corresponding data.frame()
# Note: This data.frame still needs to be collected later for plotting
getraw <- function(ID, data) {
    sql_data <- paste("SELECT * FROM", data, "WHERE ID =", ID)
    data <- tbl(database_connection, sql(sql_data))
    return(data)
}


# * Data preparation ------------------------------------------------------------------------------------------------
# TODO redo this whole part
prepareData <-
    function (rawdata,
              what = "",
              within = "",
              standard = "",
              ID,
              standardSubset = FALSE,
              add = FALSE,
              samplesub = input$samplesub,
              withsub = input$withsub,
              whatsub = input$whatsub,
              repsub = input$repsub,
              techsub = input$techsub) {
        cat(file = stderr(), "prepareData started")

        # Filter NAs
        rawdata <-
            rawdata %>% filter(!is.na(!!what)) %>% filter(!is.na(value))

        # Remove technical replicates
        if (!is.null(techsub)) {
            rawdata <-
                rawdata %>% filter(!(sample_replicate_technical %in% techsub))
        }

        # Averaging over the technical replicates
        rawdata <- rawdata %>%
            group_by_at(vars(-sample_identifier,-sample_replicate_technical,-value)) %>%
            summarize(value = mean(value, na.rm = T))

        # Onle here for debugging purposes
        print("###########################################################")
        print(what)
        print(within)
        print(standard)
        print(ID)
        print(samplesub)
        print(withsub)
        print(whatsub)
        print(repsub)
        print("###########################################################")

        if (standardSubset)
        {
            if (add) {
                # Filter on those samples explicitely desired
                if (length(samplesub) != 0) {
                    if (length(samplesub) == 1) {
                        rawdata <- rawdata %>% filter(sample == samplesub)
                    } else {
                        rawdata <- rawdata %>% filter(sample %in% samplesub)
                    }
                }
                #Filter on those whats explicitely desired
                if (length(whatsub) != 0) {
                    if (length(whatsub) == 1) {
                        filter_crit <- interp( ~ what == whatsub, what = as.name(what))
                        rawdata <- rawdata %>% filter_(filter_crit)
                    } else {
                        filter_crit <- interp( ~ what %in% whatsub, what = as.name(what))
                        rawdata <- rawdata %>% filter_(filter_crit)
                    }
                }
                # Filter on those categories / classes explicitely desired
                if (length(withsub) != 0) {
                    if (length(withsub) == 1) {
                        if (within == "Category") {
                            rawdata <- rawdata %>% filter(category == withsub)
                        } else if (within == "Functional Category") {
                            rawdata <- rawdata %>% filter(func_cat == withsub)
                        } else if (within == "Class") {
                            rawdata <- rawdata %>% filter(class == withsub)
                        }
                    } else {
                        if (within == "Category") {
                            rawdata <- rawdata %>% filter(category %in% withsub)
                        } else if (within == "Functional Category") {
                            rawdata <- rawdata %>%  filter(func_cat %in% withsub)
                        } else if (within == "Class") {
                            rawdata <- rawdata %>% filter(class %in% withsub)
                        }
                    }
                }
                # Filter on those replicates explicitely desired
                if (length(repsub) != 0) {
                    if (length(repsub) == 1) {
                        rawdata <- rawdata %>% filter((sample_replicate == repsub))

                    } else {
                        rawdata <- rawdata %>% filter((sample_replicate %in% repsub))
                    }
                }
            } else {
                # Filter on those samples explicitely desired
                if (length(samplesub) != 0) {
                    if (length(samplesub) == 1) {
                        rawdata <- rawdata %>% filter(!(sample == samplesub))
                    } else {
                        rawdata <- rawdata %>% filter(!(sample %in% samplesub))
                    }
                }
                #Filter on those whats explicitely desired
                if (length(whatsub) != 0) {
                    if (length(whatsub) == 1) {
                        filter_crit <- interp( ~ !(what == whatsub), what = as.name(what))
                        rawdata <- rawdata %>% filter_(filter_crit)
                    }
                    else {
                        filter_crit <- interp( ~ !(what %in% whatsub), what = as.name(what))
                        rawdata <- rawdata %>% filter_(filter_crit)
                    }
                }
                # Filter on those categories / classes explicitely desired
                if (length(withsub) != 0) {
                    if (length(withsub) == 1) {
                        if (within == "Category") {
                            rawdata <- rawdata %>% filter(!(category == withsub))
                        } else if (within == "Functional Category") {
                            rawdata <- rawdata %>% filter(!(func_cat == withsub))
                        } else if (within == "Class") {
                            rawdata <- rawdata %>% filter(!(class == withsub))
                        }
                    } else {
                        if (within == "Category") {
                            rawdata <- rawdata %>% filter(!(category %in% withsub))
                        } else if (within == "Functional Category") {
                            rawdata <- rawdata %>% filter(!(func_cat %in% withsub))
                        } else if (within == "Class") {
                            rawdata <- rawdata %>% filter(!(class %in% withsub))
                        }
                    }
                }
                # Filter on those replicates explicitely desired
                if (length(repsub) != 0) {
                    if (length(repsub) == 1) {
                        rawdata <- rawdata %>% filter(!(sample_replicate == repsub))
                    } else {
                        rawdata <- rawdata %>% filter(!(sample_replicate %in% repsub))
                    }
                }
            }
        }


        if (what %in% c("class", "category", "func_cat")) {
            within <- standard
        }

        # Prepare the grouping of the variables used throughout the function
        if (within == "Sample") {
            aggreGroup <- c(samplecols, what)
            smasGroup  <- list("sample", what)
        } else if (within == "Category") {
            aggreGroup <- c(samplecols, "category", what)
            smasGroup  <- list("sample", "category", what)
        } else if (within == "Functional Category") {
            aggreGroup <- c(samplecols, "func_cat", what)
            smasGroup  <- list("sample", "func_cat", what)
        } else if (within == "Class") {
            aggreGroup <- c(samplecols, "category", "class", what)
            smasGroup  <- list("sample", "category", "class", what)
        }

        # Prepares the standardization of the variables used throughout the function
        if (standard == "Sample") {
            stdGroup   <- list("sample_replicate")
        } else if (standard == "Category") {
            stdGroup   <- c(samplecols, "category")
        } else if (standard == "Functional Category") {
            stdGroup   <- c(samplecols, "func_cat")
        } else if (standard == "Class") {
            stdGroup   <- c(samplecols, "category", "class")
        } else {
            return (-1)
        }

        Aggregated <- rawdata %>%
            group_by(.dots = aggreGroup) %>%
            summarize(sum = sum(value, na.rm = T))

        print("group_by was used 1")

        Standard <- rawdata %>%
            group_by(.dots = stdGroup) %>%
            summarize(standardSum = sum(value, na.rm = T))

        print("group_by was used 2")

        Standardized <-
            left_join(Aggregated, Standard, type = "left") %>%
            mutate(standardizedSum = (sum / standardSum) * 100)


        StandardizedNrSamp <- Standardized %>%
            group_by(.dots = aggreGroup) %>%
            # summarise(MeanOfTechnicalReplicates = mean(standardizedSum, na.rm = T)) %>%
            group_by(.dots = smasGroup) %>%
            summarise(
                Mean = mean(standardizedSum, na.rm = T),
                Median = median(standardizedSum, na.rm = T),
                SD = sd(standardizedSum, na.rm = T),
                SE = se(standardizedSum, na.rm = T),
                NrSamp = n()
            )

        print("group_by was used 3")

        Standardized <- left_join(Standardized, StandardizedNrSamp)

        print("standardized created")

        Standardized <- rename_(Standardized, .dots = list(xval = what))
        StandardizedNrSamp <<- StandardizedNrSamp %>%
            rename_(.dots = list(xval = what))
        samplenames <- as.list(unique(rawdata$sample))

        names(samplenames) <- unique(rawdata$sample)
        print("Dataprep successful with prepareData")
        return(Standardized)
    }



# Plotting functions ----------------------------------------------------------------------------------------------

### plotter.R

# Prepares the subset of the data for the plots
prepareSubset <-
    function(data,
             advanced,
             samplesub = NA,
             whatsub = NA,
             withsub = NA,
             within = within,
             repsub = NA,
             base = NA) {
        if ("add" %in% advanced) {
            # Filter on those samples explicitely desired
            if (length(samplesub) != 0) {
                data <- data %>%
                    filter(sample %in% samplesub)
            }
            #Filter on those whats explicitely desired
            if (length(whatsub) != 0) {
                data <- data %>%
                    filter(xval %in% whatsub)
            }
            # Filter on those categories / classes explicitely desired
            if (length(withsub) != 0) {
                if (within == "Category") {
                    data <- data %>%
                        filter(category %in% withsub)
                } else if (within == "Functional Category") {
                    data <- data %>%
                        filter(func_cat %in% withsub)
                } else if (within == "Class") {
                    data <- data %>%
                        filter(class %in% withsub)
                }
            }
            # Filter on those replicates explicitely desired
            if (length(repsub) != 0) {
                data <- data %>% filter((sample_replicate %in% repsub))
            }
        } else {
            # Filter on those samples explicitely desired
            if (length(samplesub) != 0) {
                data <- data %>%
                    filter(!(sample %in% samplesub))
            }
            #Filter on those whats explicitely desired
            if (length(whatsub) != 0) {
                data <- data %>%
                    filter(!(xval %in% whatsub))
            }
            # Filter on those categories / classes explicitely desired
            if (length(withsub) != 0) {
                if (within == "Category") {
                    data <- data %>%
                        filter(!(category %in% withsub))
                } else if (within == "Functional Category") {
                    data <- data %>%
                        filter(!(func_cat %in% withsub))
                } else if (within == "Class") {
                    data <- data %>%
                        filter(!(class %in% withsub))
                }
            }
            # Filter on those replicates explicitely desired
            if (length(repsub) != 0) {
                data <- data %>% filter(!(sample_replicate %in% repsub))
            }
            if (length(base) != 0) {
                basedata <- data %>%
                    filter(sample %in% base)
            }
        }

        # ** Data saving functions ----------------------------------------------------------------------------------------
        # The subset the ggplot code works with
        lipidsubset <<- data
        # The source for the .csv file
        # Concat for facetted data
        csvdata <- data
        if (within == "Category") {
            csvdata <- csvdata %>%
                mutate(xval = paste(category, xval, sep = '-'))
        } else if (within == "Functional Category") {
            csvdata <- csvdata %>%
                mutate(xval = paste(func_cat, xval, sep = '-'))
        } else if (within == "Class") {
            csvdata <- csvdata %>%
                mutate(xval = paste(class, xval, sep = '-'))
        }
        tidySave <<- csvdata %>%
            ungroup() %>%
            mutate(biolSample = paste(sample, sample_replicate)) %>%
            group_by(biolSample, xval) %>%
            summarize(value = sum(standardizedSum)) %>%
            select(Sample = biolSample, Feature = xval, value) %>%
            spread("Sample", "value")

        return(data)
    }


# * Main Plot -------------------------------------------------------------------------------------------------------
preparePlots <-
    function(data,
             checkGroup,
             plottype,
             what,
             within = within,
             standard = standard,
             advanced,
             ...) {
        print("Plotting called")

        # Order of the x-axis
        if (what == "class") {
            plotorder <- class_order
        } else {
            plotorder <- levels(as.factor(data$xval))
        }
        data$xval <- factor(data$xval, levels = plotorder)

        # Color related options (remove yellow (i.e. value 6))
        colorCount <- length(unique(data$sample))
        getPalette <- colorRampPalette(brewer.pal(9, "Set1")[-6])

        # Specifying the dodging
        dodge <- position_dodge(width = 0.9)

        ## Decide on Plottype
        if (plottype == "barplot") {
            graphic <- ggplot(data = data, aes(
                x = xval,
                y = Mean,
                fill = sample,
                group = sample
            )) +
                geom_bar(position = dodge, stat = "identity")

            if (colorCount < 9) {
                graphic <- graphic +
                    scale_fill_manual(values = brewer.pal(9, "Set1")[-6]) +
                    scale_color_manual(values = brewer.pal(9, "Set1")[-6])
            } else {
                graphic <- graphic +
                    scale_fill_manual(values = getPalette(colorCount)) +
                    scale_color_manual(values = getPalette(colorCount))
            }
        } else if (plottype == "boxplot") {
            graphic <- ggplot(data = data, aes(
                x = xval,
                y = standardizedSum,
                fill = sample
            )) +
                geom_boxplot()
            if (colorCount < 9) {
                graphic <- graphic +
                    scale_fill_manual(values = brewer.pal(9, "Set1")[-6]) +
                    scale_color_manual(values = brewer.pal(9, "Set1")[-6])
            } else {
                graphic <- graphic +
                    scale_fill_manual(values = getPalette(colorCount)) +
                    scale_color_manual(values = getPalette(colorCount))
            }
        } else if (plottype == "pointrange") {
            graphic <-
                ggplot(
                    data = data,
                    aes(
                        x = xval,
                        y = Mean,
                        ymin = Mean - SD,
                        ymax = Mean + SD,
                        group = sample,
                        color = sample
                    )
                ) +
                geom_pointrange(position = dodge, size = 0.1) +
                geom_point(
                    aes(
                        x = xval,
                        y = standardizedSum,
                        color = sample,
                        group = sample
                    ),
                    alpha = 0.2,
                    position = dodge
                )
            if (colorCount < 9) {
                graphic <- graphic +
                    scale_fill_manual(values = brewer.pal(9, "Set1")[-6]) +
                    scale_color_manual(values = brewer.pal(9, "Set1")[-6])
            } else {
                graphic <- graphic +
                    scale_fill_manual(values = getPalette(colorCount)) +
                    scale_color_manual(values = getPalette(colorCount))
            }
        } else {
            graphic <- ggplot(data = data,
                              aes(
                                  x = xval,
                                  y = standardizedSum,
                                  fill = sample,
                                  group = sample
                              ))
            if (colorCount < 9) {
                graphic <- graphic +
                    scale_fill_manual(values = brewer.pal(9, "Set1")[-6]) +
                    scale_color_manual(values = brewer.pal(9, "Set1")[-6])
            } else {
                graphic <- graphic +
                    scale_fill_manual(values = getPalette(colorCount)) +
                    scale_color_manual(values = getPalette(colorCount))
            }
        }

        if (what %in% c("class", "category", "func_cat")) {
            within <- standard
        }
        if (within == "Category") {
            if (what == "chains")
            {
                graphic <- graphic + facet_grid(~ category, scales = "free_x")
            } else {
                graphic <- graphic + facet_grid(~ category, scales = "free_x")
            }
        } else if (within == "Functional Category") {
            #        stddata <- stddata %>% filter(func_cat %in% data$func_cat)
            graphic <-
                graphic + facet_grid(~ func_cat, scales = "free_x")
        } else if (within == "Class") {
            graphic <- graphic + facet_grid(~ class, scales = "free_x")
        }

        # Plot Legend
        if (!("legend?" %in% checkGroup)) {
            graphic <- graphic + theme(legend.position = "none")
        }

        # Plot values / maybe shaped
        if ("sample_ident" %in% checkGroup) {
            if ("symbols" %in% advanced) {
                graphic <- graphic +
                    geom_point(
                        data = data,
                        aes(
                            x = xval,
                            y = standardizedSum,
                            colour = sample,
                            shape = sample_replicate
                        ),
                        position = dodge,
                        alpha = 0.9
                    ) +
                    scale_shape_manual(values = c(0:25, 33:127), name = "")
            } else if ("letters" %in% advanced) {
                graphic <- graphic +
                    geom_text(
                        data = data,
                        aes(
                            x = xval,
                            y = standardizedSum,
                            colour = sample,
                            label = sample_replicate
                        ),
                        size = 2.5,
                        position = dodge,
                        alpha = 0.9,
                        color = "black"
                    )
            }
        } else if ("values" %in% checkGroup) {
            if ("highlight" %in% advanced) {
                graphic <- graphic +
                    geom_point(
                        data = data,
                        aes(
                            x = xval,
                            y = standardizedSum,
                            fill = sample
                        ),
                        position = dodge,
                        colour = "white",
                        alpha = 0.8,
                        pch = 21
                    )
            } else {
                graphic <- graphic +
                    geom_point(
                        data = data,
                        aes(
                            x = xval,
                            y = standardizedSum,
                            colour = sample
                        ),
                        position = dodge,
                        alpha = 0.9
                    )
            }
        }

        # Plot Median
        if ("median" %in% checkGroup) {
            graphic <- graphic +
                geom_errorbar(aes(ymin = Median, ymax = Median), position = dodge)
        }
        # Plot Errorbars
        if ("errsd" %in% checkGroup) {
            graphic <- graphic +
                geom_errorbar(aes(
                    color = sample,
                    ymin = Mean - SD,
                    ymax = Mean + SD
                ),
                position = dodge)
        }
        if ("errse" %in% checkGroup) {
            graphic <- graphic +
                geom_errorbar(aes(
                    color = sample,
                    ymin = Mean - SE,
                    ymax = Mean + SE
                ),
                position = dodge)
        }
        # Plot Nr of Datapoints
        data <- data[!duplicated(data[c("sample", "xval")]), ]
        if ("nrdat" %in% checkGroup) {
            graphic <- graphic +
                geom_text(
                    data = data,
                    aes(
                        x = xval,
                        y = -0.7,
                        label = NrSamp,
                        group = sample,
                        fontsize = "bold"
                    ),
                    size = 4,
                    colour = "darkgrey",
                    position = dodge,
                    check_overlap = TRUE
                )
        }
        if ("mean" %in% checkGroup) {
            if ("errsd" %in% checkGroup) {
                graphic <- graphic +
                    geom_text(
                        data = data,
                        aes(
                            color = sample,
                            y = Mean + SD,
                            label = round(Mean, 2)
                        ),
                        position = dodge,
                        vjust = -0.5,
                        size = 3
                    )
            } else if ("errse" %in% checkGroup) {
                graphic <- graphic +
                    geom_text(
                        data = data,
                        aes(
                            color = sample,
                            y = Mean + SE,
                            label = round(Mean, 2)
                        ),
                        position = dodge,
                        vjust = -0.5,
                        size = 3
                    )
            } else {
                graphic <- graphic +
                    geom_text(
                        data = data,
                        aes(
                            color = sample,
                            x = xval,
                            y = Mean,
                            label = round(Mean, 2)
                        ),
                        position = dodge,
                        vjust = -0.5,
                        size = 3
                    )
            }
        }
        graphic <-
            graphic + labs(x = "", y = paste("mol % of measured lipid"))
        if (what %in% c("chains", "chain_sums")) {
            graphic <-
                graphic + theme(axis.text.x = element_text(
                    angle = 45,
                    size = 08,
                    hjust = 1
                ))
        }
        if ("logtrans" %in% checkGroup)
            graphic <- graphic + scale_y_log10()
        if ("ttest" %in% checkGroup){
            # TODO this
            graphic
        }

        print("Plot successful")
        return(graphic)
    }



# PCA Plots -------------------------------------------------------------------------------------------------------

# *DeviumPCA -------------------------------------------------------------------------------------------------------

# TODO add control for polygon plot order (https://github.com/hadley/ggplot2/wiki/plotting-polygon-shapefiles)
# open addition of pallets, themes,add group labels to plot, ...

#trying to debug issue in shiny, pca.data is now an object instead of its name as a character
devium.pca.calculate <-
    function(pca.inputs,
             args.list = TRUE,
             return = "list",
             plot = TRUE)
    {
        #port of imDEV source code optimized for GUI use
        #accepts list with the following arguments
        #pca.data<- data object (samples as rows)
        #pca.components number of principal components
        #pca.algorithm see pcaMethods::pca for options
        #pca.center logical, mean center the data
        #pca.scaling character describing scaling method, see pcaMethods::prep for options

        #check for text or factor and remove (add to subset)
        tmp <- pca.inputs
        data.obj <-
            afixln(tmp$pca.data) # converts factors or characters to numeric

        if (is.null(tmp$pca.cv)) {
            pca.cv <- "none"
        } else {
            pca.cv <- tmp$pca.cv
        } #avoid issues elsewhere

        #adjust PCS if > than data
        PCs <- tmp$pca.components
        if (PCs > min(dim(data.obj))) {
            PCs <-
                min(dim(data.obj))
        } # this should be done internally in the PCa fxn
        pca.results <-
            pcaMethods::pca(
                as.matrix(data.obj),
                method = tmp$pca.algorithm,
                nPcs = PCs,
                center = tmp$pca.center,
                scale = tmp$pca.scaling,
                cv = pca.cv,
                seed = 123
            )

        #results
        scores <- as.data.frame(pca.results@scores)
        loadings <- as.data.frame(pca.results@loadings)
        eigenvalues <- data.frame(eigenvalues = pca.results@R2)

        if (pca.cv == "q2") {
            # account for unequal r2 and q2 lengths
            q2 <-
                tryCatch(
                    pcaMethods:::Q2(pca.results),
                    error = function(e) {
                        0
                    }
                )#some versions of pcaMethods don't have this?
            q2 <- c(q2, rep(q2[length(q2)], nrow(eigenvalues) - length(q2)))
            eigenvalues <- data.frame(eigenvalues, q2 = q2)
        }

        #add leverage and dmodX
        #bind between scores and loadings
        lev <-
            tryCatch(
                as.matrix(pcaMethods:::leverage(pca.results)),
                error = function(e) {
                    "can not calculate"
                }
            )
        dmodx <-
            tryCatch(
                as.matrix(pcaMethods:::DModX(pca.results)),
                error = function(e) {
                    "can not calculate"
                }
            )
        diagnostics <-
            tryCatch(
                data.frame(leverage = lev, DmodX = dmodx),
                error = function(e) {
                    data.frame(Error = "not applicable")
                }
            )

        #scree plot
        if (plot == TRUE) {
            make.scree.plot.bar(eigenvalues)
        }

        #get the name of the data
        if (return == "list") {
            return(
                list(
                    pca.scores = scores,
                    pca.loadings =  loadings,
                    pca.eigenvalues = eigenvalues,
                    pca.diagnostics = diagnostics,
                    pca.groups = tmp$pca.groups,
                    pca.model = pca.results
                )
            )
        }

        if (return == "model") {
            return(pca.results)
        }
    }

# *** Screee plot -------------------------------------------------------------------------------------------------
# generate a scree plot base
make.scree.plot <- function(eigenvalues)
{
    pcaeigen <- eigenvalues
    par(mar = c(4, 4, 4, 4.25))
    total <- sum(matrix(unlist(pcaeigen)) * 100)
    plot(
        c(1:nrow(pcaeigen)),
        matrix(unlist(pcaeigen)) * 100,
        type = "l",
        main = paste(
            "PCA Screeplot showing",
            round(total, 0),
            "% explained variance"
        ),
        lwd = 2,
        xaxt = "n",
        frame.plot = TRUE,
        xlab = paste("Principal components (n =", nrow(pcaeigen) , ")"),
        ylab = ""
    )
    abline(v = seq(1, nrow(pcaeigen), by = 1),
           lty = 2,
           col = "gray40")
    points(
        as.matrix(pcaeigen) * 100,
        col = "black",
        pch = 21,
        bg = "red",
        cex = 2
    )
    mtext(
        "% Explained Variance",
        side = 2,
        line = 3,
        col = "red"
    )
    abline(h = 1, lty = 2)
    total.var <- round(cumsum(matrix(unlist(pcaeigen))) * 100, 0)
    par(new = TRUE)
    plot(
        c(1:nrow(pcaeigen)),
        total.var,
        type = "l",
        lwd = 2,
        xaxt = "n",
        yaxt = "n",
        lty = 1,
        ylab = "",
        xlab = ""
    )
    abline(v = c(1:nrow(pcaeigen))[total.var > 79][1], lty = 2)
    points(
        total.var,
        col = "black",
        pch = 21,
        bg = "blue",
        cex = 2
    )
    axis(4)
    mtext(
        "Total % explained variance",
        side = 4,
        line = 3,
        col = "blue"
    )
    axis(1, seq(1, nrow(pcaeigen), by = 1))
}

make.scree.plot.bar <- function(eigenvalues) {
    check.get.packages("gridExtra")
    .theme <- theme(
        axis.line = element_line(colour = 'gray', size = .75),
        panel.background = element_blank(),
        plot.background = element_blank()
    )

    tmp <-
        data.frame(melt(eigenvalues$eigenvalue), PCs = rep(1:nrow(eigenvalues)))
    tmp$value <- tmp$value * 100
    p1 <-
        ggplot(tmp, aes(y = value, x = as.factor(PCs))) + geom_bar(fill = "gray",
                                                                   stat = "identity",
                                                                   position = position_dodge()) +
        .theme + geom_hline(yintercept = 1, linetype = 2) + ylab("% variance explained") + xlab("Principal Component")

    #cumulative
    eigenvalues$eigenvalues <- cumsum(eigenvalues$eigenvalues)
    tmp <- data.frame(melt(eigenvalues), PCs = rep(1:nrow(eigenvalues)))
    p2 <-
        ggplot(tmp, aes(
            y = value,
            x = as.factor(PCs),
            fill = variable
        )) + geom_bar(stat = "identity", position = position_dodge()) +
        .theme + geom_hline(yintercept = .8, linetype = 2) + xlab("Principal Component")

    #multiple plot out put
    grid.arrange(p1, p2, ncol = 1)
}

# replace with generic plotting which supports custom ggplot calls
plot.PCA <-
    function(pca,
             xaxis = 1,
             yaxis = 2,
             results = c("screeplot", "scores", "loadings", "biplot"),
             group.bounds = "ellipse",
             size = 3,
             color = NULL,
             label = TRUE,
             color.legend.name =  NULL,
             size.legend.name =  NULL,
             font.size = 5,
             alpha = .75,
             g.alpha = 0.2,
             theme = NULL,
             point.labels = NULL,
             extra = NULL,
             plot = TRUE,
             ...) {
        local <- switch(
            results[1],

            "screeplot" 	= function(pca, ...) {
                make.scree.plot.bar(pca$pca.eigenvalues)
            },
            "scores"		= function(pca, color, size, extra, ...) {
                obj <- pca$pca.scores[, c(xaxis, yaxis)]
                tmp <- data.frame(obj, id = rownames(obj))

                if (is.null(theme)) {
                    .theme2 <- theme(
                        axis.line = element_line(colour = 'gray', size = .75),
                        panel.background = element_blank(),
                        plot.background = element_blank(),
                        legend.background = element_rect(fill = 'white'),
                        legend.key = element_blank()
                    )
                } else {
                    .theme2 <- theme
                }

                if (is.null(color)) {
                    tmp$color <- "gray"
                } else{
                    tmp$color <- as.factor(color[, ])
                    if (is.null(color.legend.name)) {
                        color.legend.name <- colnames(color)
                    }
                }

                points <- if (all(tmp$color == "gray")) {
                    if (!is.data.frame(size)) {
                        geom_point(
                            color = "gray",
                            size = size,
                            alpha = alpha,
                            show_guide = FALSE
                        )
                    } else {
                        tmp$size <- size[, 1]
                        if (is.null(size.legend.name)) {
                            size.legend.name <- colnames(size)
                        }
                        geom_point(aes(size = size),
                                   color = "gray",
                                   alpha = alpha)
                    }
                } else {
                    if (!is.data.frame(size)) {
                        geom_point(aes(color = color),
                                   size = size,
                                   alpha = alpha)
                    } else {
                        tmp$size <- size[, 1]
                        if (is.null(size.legend.name)) {
                            size.legend.name <- colnames(size)
                        }
                        geom_point(aes(color = color, size = size), alpha =
                                       alpha)
                    }
                }
                #labels
                tmp$lab.offset <-
                    tmp[, 2] - abs(range(obj[, 2])[1] - range(obj[, 2])[2]) / 50
                labels <- if (label == TRUE) {
                    if (is.null(point.labels)) {
                        geom_text(
                            size = font.size,
                            aes_string(
                                x = colnames(tmp)[1],
                                y = "lab.offset",
                                label = "id"
                            ),
                            color = "black",
                            show_guide = FALSE
                        )
                    } else {
                        tmp$custom.label <- point.labels
                        geom_text(
                            size = font.size,
                            aes_string(
                                x = colnames(tmp)[1],
                                y = "lab.offset",
                                label = "custom.label"
                            ),
                            color = "black",
                            show_guide = FALSE
                        )
                    }
                } else {
                    NULL
                }

                #grouping visualizations
                polygons <- NULL
                #Hoettellings T2 ellipse
                if (group.bounds == "ellipse") {
                    ell <-
                        get.ellipse.coords(cbind(obj[, 1], obj[, 2]), group = tmp$color)# group visualization via
                    polygons <- if (is.null(color)) {
                        geom_polygon(
                            data = data.frame(ell$coords),
                            aes(x = x, y = y),
                            fill = "gray",
                            color = "gray",
                            linetype = 2,
                            alpha = g.alpha,
                            show_guide = FALSE
                        )
                    } else {
                        geom_polygon(
                            data = data.frame(ell$coords),
                            aes(
                                x = x,
                                y = y,
                                fill = group
                            ),
                            linetype = 2,
                            alpha = g.alpha,
                            show_guide = FALSE
                        )
                    }
                }
                #convex hull
                if (group.bounds == "polygon") {
                    ell <-
                        get.polygon.coords(data.frame(cbind(obj[, 1], obj[, 2])), tmp$color)# group visualization via
                    polygons <- if (is.null(color)) {
                        geom_polygon(
                            data = data.frame(ell),
                            aes(x = x, y = y),
                            fill = "gray",
                            color = "gray",
                            linetype = 2,
                            alpha = g.alpha,
                            show_guide = FALSE
                        )
                    } else {
                        geom_polygon(
                            data = data.frame(ell),
                            aes(
                                x = x,
                                y = y,
                                fill = group
                            ),
                            linetype = 2,
                            alpha = g.alpha,
                            show_guide = FALSE
                        )
                    }
                }

                #making the actual plot
                p <-
                    ggplot(data = tmp, aes_string(x = colnames(tmp)[1], y = colnames(tmp)[2])) +
                    geom_vline(
                        xintercept = 0,
                        linetype = 2,
                        size = .5,
                        alpha = .5
                    ) +
                    geom_hline(
                        yintercept = 0,
                        linetype = 2,
                        size = .5,
                        alpha = .5
                    ) +
                    polygons +
                    points +
                    labels +
                    .theme2 +
                    scale_x_continuous(paste(colnames(tmp)[1], sprintf(
                        "(%s%%)", round(pca$pca.eigenvalues[xaxis, 1], digits = 2) * 100
                    ), sep = " ")) +
                    scale_y_continuous(paste(colnames(tmp)[2], sprintf(
                        "(%s%%)", round(pca$pca.eigenvalues[yaxis, 1], digits = 2) * 100
                    ), sep = " "))
                if (!is.null(color.legend.name)) {
                    if (is.factor(unlist(color))) {
                        p <- p + scale_colour_discrete(name = color.legend.name)
                    } else {
                        p <- p + scale_fill_continuous(name = color.legend.name)
                    }
                }
                if (!is.null(size.legend.name)) {
                    if (is.factor(unlist(size))) {
                        p <- p + scale_size_discrete(name = size.legend.name)
                    } else {
                        p <- p + scale_size_continuous(name = size.legend.name)
                    }
                }
                p <- p + extra
                if (plot)
                    print(p)
                else
                    return(p)
            },

            "loadings"		= function(pca, color, size, ...) {
                obj <- pca$pca.loadings[, c(xaxis, yaxis)]
                tmp <- data.frame(obj, id = rownames(obj))
                #plot
                if (is.null(theme)) {
                    .theme2 <- theme(
                        axis.line = element_line(colour = 'gray', size = .75),
                        panel.background = element_blank(),
                        plot.background = element_blank(),
                        legend.background = element_rect(fill = 'white'),
                        legend.key = element_blank()
                    )
                } else {
                    .theme2 <- theme
                }

                if (is.null(color)) {
                    tmp$color <- "gray"
                } else{
                    tmp$color <- as.factor(color[, ])
                    if (is.null(color.legend.name)) {
                        color.legend.name <- colnames(color)
                    }
                }

                points <- if (all(tmp$color == "gray")) {
                    geom_point(
                        color = "gray",
                        size = size,
                        alpha = alpha,
                        show_guide = FALSE
                    )
                } else {
                    if (is.null(size.legend.name)) {
                        size.legend.name <- colnames(color)
                    }
                    geom_point(aes(color = color),
                               size = size,
                               alpha = alpha)
                }
                #labels
                tmp$lab.offset <-
                    tmp[, 2] - abs(range(obj[, 2])[1] - range(obj[, 2])[2]) / 50
                labels <-
                    if (label == TRUE) {
                        geom_text(
                            size = font.size,
                            aes_string(
                                x = colnames(tmp)[1],
                                y = "lab.offset",
                                label = "id"
                            ),
                            color = "black",
                            show_guide = FALSE
                        )
                    } else {
                        NULL
                    }

                #grouping visualizations
                polygons <- NULL
                #Hoettellings T2 ellipse
                if (group.bounds == "ellipse") {
                    ell <-
                        get.ellipse.coords(cbind(obj[, 1], obj[, 2]), group = tmp$color)# group visualization via
                    polygons <- if (is.null(color)) {
                        geom_polygon(
                            data = data.frame(ell$coords),
                            aes(x = x, y = y),
                            fill = "gray",
                            color = "gray",
                            linetype = 2,
                            alpha = g.alpha,
                            show_guide = FALSE
                        )
                    } else {
                        geom_polygon(
                            data = data.frame(ell$coords),
                            aes(
                                x = x,
                                y = y,
                                fill = group
                            ),
                            linetype = 2,
                            alpha = g.alpha,
                            show_guide = FALSE
                        )
                    }
                }
                #convex hull
                if (group.bounds == "polygon") {
                    ell <-
                        get.polygon.coords(data.frame(cbind(obj[, 1], obj[, 2])), tmp$color)# group visualization via
                    polygons <- if (is.null(color)) {
                        geom_polygon(
                            data = data.frame(ell),
                            aes(x = x, y = y),
                            fill = "gray",
                            color = "gray",
                            linetype = 2,
                            alpha = g.alpha,
                            show_guide = FALSE
                        )
                    } else {
                        geom_polygon(
                            data = data.frame(ell),
                            aes(
                                x = x,
                                y = y,
                                fill = group
                            ),
                            linetype = 2,
                            alpha = g.alpha,
                            show_guide = FALSE
                        )
                    }
                }

                #making the actual plot
                p <-
                    ggplot(data = tmp, aes_string(x = colnames(tmp)[1], y = colnames(tmp)[2])) +
                    geom_vline(
                        xintercept = 0,
                        linetype = 2,
                        size = .5,
                        alpha = .5
                    ) +
                    geom_hline(
                        yintercept = 0,
                        linetype = 2,
                        size = .5,
                        alpha = .5
                    ) +
                    points +
                    .theme2 +
                    labels +
                    polygons +
                    scale_x_continuous(paste(colnames(tmp)[1], sprintf(
                        "(%s%%)", round(pca$pca.eigenvalues[xaxis, 1], digits = 2) * 100
                    ), sep = " ")) +
                    scale_y_continuous(paste(colnames(tmp)[2], sprintf(
                        "(%s%%)", round(pca$pca.eigenvalues[yaxis, 1], digits = 2) * 100
                    ), sep = " "))
                if (!is.null(color.legend.name)) {
                    p <- p + scale_colour_discrete(name = color.legend.name)
                }
                if (!is.null(size.legend.name)) {
                    p <- p + scale_size_discrete(name = size.legend.name)
                }
                if (plot)
                    print(p)
                else
                    p
            },
            "biplot" = function(pca, ...) {
                #rescaling based on: http://cran.r-project.org/doc/contrib/Lemon-kickstart/rescale.R
                rescale <- function(x, newrange) {
                    if (nargs() > 1 && is.numeric(x) && is.numeric(newrange)) {
                        # if newrange has max first, reverse it
                        if (newrange[1] > newrange[2]) {
                            newmin <- newrange[2]
                            newrange[2] <- newrange[1]
                            newrange[1] <- newmin
                        }
                        xrange <- range(x)
                        if (xrange[1] == xrange[2])
                            stop("can't rescale a constant vector!")
                        mfac <-
                            (newrange[2] - newrange[1]) / (xrange[2] - xrange[1])
                        invisible(newrange[1] + (x - xrange[1]) *
                                      mfac)
                    }
                    else {
                        cat("Usage: rescale(x,newrange)\n")
                        cat(
                            "\twhere x is a numeric object and newrange is the min and max of the new range\n"
                        )
                    }
                }
                if (is.null(theme)) {
                    .theme2 <- theme(
                        axis.line = element_line(colour = 'gray', size = .75),
                        panel.background = element_blank(),
                        plot.background = element_blank()
                    )
                } else {
                    .theme2 <- theme
                }

                #based on https://groups.google.com/forum/#!topic/ggplot2/X-o2VXjDkQ8
                scores <- pca$pca.scores[, c(xaxis, yaxis)]
                loadings <-
                    tmp.loadings <- pca$pca.loadings[, c(xaxis, yaxis)]
                tmp.loadings[, 1] <-
                    rescale(loadings[, 1], range(scores[, 1]))
                tmp.loadings[, 2] <-
                    rescale(loadings[, 2], range(scores[, 2]))
                tmp.loadings <-
                    data.frame(tmp.loadings, label = rownames(loadings))

                #Adding grouping visualizations
                tmp <- scores[, 1:2]
                if (is.null(color)) {
                    tmp$color <- "gray"
                } else{
                    tmp$color <- as.factor(color[, ])
                    if (is.null(legend.name)) {
                        legend.name <- colnames(color)
                    }
                }
                #grouping visualizations
                polygons <- NULL
                #Hoettellings T2 ellipse
                if (group.bounds == "ellipse") {
                    ell <-
                        get.ellipse.coords(cbind(tmp[, 1], tmp[, 2]), group = tmp$color)# group visualization via
                    polygons <- if (is.null(color)) {
                        geom_polygon(
                            data = data.frame(ell$coords),
                            aes(x = x, y = y),
                            fill = "gray",
                            color = "gray",
                            linetype = 2,
                            alpha = g.alpha,
                            show_guide = FALSE
                        )
                    } else {
                        geom_polygon(
                            data = data.frame(ell$coords),
                            aes(
                                x = x,
                                y = y,
                                fill = group
                            ),
                            linetype = 2,
                            alpha = g.alpha,
                            show_guide = FALSE
                        )
                    }
                }
                #convex hull
                if (group.bounds == "polygon") {
                    ell <-
                        get.polygon.coords(cbind(tmp[, 1], tmp[, 2]), tmp$color)# group visualization via
                    polygons <- if (is.null(color)) {
                        geom_polygon(
                            data = data.frame(ell),
                            aes(x = x, y = y),
                            fill = "gray",
                            color = "gray",
                            linetype = 2,
                            alpha = g.alpha,
                            show_guide = FALSE
                        )
                    } else {
                        geom_polygon(
                            data = data.frame(ell),
                            aes(
                                x = x,
                                y = y,
                                fill = group
                            ),
                            linetype = 2,
                            alpha = g.alpha,
                            show_guide = FALSE
                        )
                    }
                }

                points <- if (all(tmp$color == "gray")) {
                    geom_point(
                        data = tmp,
                        aes_string(x = colnames(tmp)[1], y = colnames(tmp)[2]),
                        color = "gray",
                        size = size,
                        alpha = alpha,
                        show_guide = FALSE
                    )
                } else {
                    if (!is.data.frame(size)) {
                        geom_point(
                            data = tmp,
                            aes_string(
                                x = colnames(tmp)[1],
                                y = colnames(tmp)[2],
                                color = "color"
                            ),
                            size = size,
                            alpha = alpha
                        )
                    } else {
                        tmp$size <- size[, 1]
                        geom_point(
                            data = tmp,
                            aes_string(
                                x = colnames(tmp)[1],
                                y = colnames(tmp)[2],
                                color = "color",
                                size = "size"
                            ),
                            alpha = alpha
                        )
                    }
                }

                p <- ggplot() +
                    points +
                    polygons +
                    geom_segment(
                        data = tmp.loadings,
                        aes_string(
                            x = 0,
                            y = 0,
                            xend = colnames(tmp.loadings)[1],
                            yend = colnames(tmp.loadings)[2]
                        ),
                        arrow = arrow(length = unit(0.05, "cm")),
                        alpha = 0.25
                    ) +
                    geom_text(
                        data = tmp.loadings,
                        aes_string(
                            x = colnames(tmp.loadings)[1],
                            y = colnames(tmp.loadings)[2],
                            label = "label"
                        ),
                        alpha = 0.5,
                        size = font.size
                    ) +
                    scale_colour_discrete("Variety") +
                    scale_x_continuous(paste(colnames(tmp)[1], sprintf(
                        "(%s%%)", round(pca$pca.eigenvalues[xaxis, 1], digits = 2) * 100
                    ), sep = " ")) +
                    scale_y_continuous(paste(colnames(tmp)[2], sprintf(
                        "(%s%%)", round(pca$pca.eigenvalues[yaxis, 1], digits = 2) * 100
                    ), sep = " ")) +
                    .theme2
                if (!is.null(legend.name)) {
                    p <- p + scale_colour_discrete(name = legend.name)
                }
                p <- p + extra
                if (plot)
                    print(p)
                else
                    p
            }
        )
        local(
            pca = pca,
            color = color,
            size = size,
            alpha = alpha,
            group.bounds = group.bounds,
            extra,
            ...
        )
    }


# Global options  ------------------------------------------------------------------------------------------------

# * ggplot options --------------------------------------------------------------------------------------------------
##### general options for the ggplot theme
theme_set(
    theme_bw(18)
    + theme(
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 10),
        panel.border = element_rect(colour = "black"),
        panel.background = element_blank()
    )
)

.theme <- theme(
    axis.line = element_line(colour = 'black', size = .75),
    text = element_text(color = "black"),
    panel.background = element_blank(),
    plot.background = element_blank()
)


# * Database Connections --------------------------------------------------------------------------------------------
# To postgres
# database_connection <- src_postgres(dbname = "ldb", host = "129.206.154.238",
#                                    user = "mathias")
#                                    # user = "monkel")
# To SQLite
database_connection = src_sqlite("database/Sqlite.db")

##### Initial Rawdata
rawdata <- data.frame()

##### Initial Metadatatable generation
sql_data <- paste("SELECT * FROM id_info")
rawmetadata <- collect(tbl(database_connection, sql(sql_data)))

cleandata <- rawmetadata

# Some formatting of the columns that contain temporal information
cleandata$date_upload <-
    as.Date(cleandata$date_upload, format = '%y%m%d')
cleandata$date_sample <-
    as.Date(cleandata$date_sample, format = '%y%m%d')
cleandata$date_extraction <-
    as.Date(cleandata$date_extraction, format = '%y%m%d')
cleandata$date_measured <-
    as.Date(cleandata$date_measured, format = '%y%m%d')
cleandata <- cleandata[order(cleandata$id),]


# * Names for plots -------------------------------------------------------------------------------------------------
datanames <- as.list(cleandata$id)
names(datanames) <- paste(cleandata$id, cleandata$title)
whatnames <-
    list(
        "Classes" = "class",
        "Categories" = "category",
        "Functional Categories" = "func_cat",
        "Species" = "chains",
        "Sum Species" = "chain_sums",
        "Length" = "length",
        "Double bonds" = "db",
        "OH" = "oh"
    )
revwhatnames <-
    list(
        "class" = "Classes",
        "category" = "Categories",
        "func_cat" = "Functional Categories",
        "chains" = "Species",
        "chain_sums" = "Sum Species",
        "length" = "Length",
        "db" = "Double bonds",
        "oh" = "OH"
    )

samplenames <- list()

withinnames <-
    c("Sample", "Category", "Functional Category", "Class")
standardnames <-
    c("Sample", "Category", "Functional Category", "Class")
plotchoices <- list(
    "Plot Values" = "values",
    "Errorbar (Mean +/- 1SD)" = "errsd",
    "Errorbar (Mean +/- 1SE)" = "errse",
    "Median" = "median",
    "Nr of Datapoints" = "nrdat",
    "Mean Values" = "mean",
    "Log10-Transformation" = "logtrans",
    "Show Legend" = "legend?",
    "Identify Individuals" = "sample_ident",
    "Ttest" = "ttest"
)


#  ** Order of factor(class) ---------------------------------------------------------------------
# by the database:
sql_statement <-
    paste('SELECT class FROM lipid_class_order_complete order by class_order')
class_order_database <-
    tbl(database_connection, sql(sql_statement)) %>% data.frame()
class_order <- class_order_database$class

# By hand
# class_order <- c("PC", "PC O-", "PE", "PE O-", "PE P-", "PS", "PS O-", "PI", "PI O-",
#                  "PG", "PG O-", "PA", "PA O-", "DAG", "CL", "MLCL", "Cer", "SM",
#                  "HexCer", "GM3", "Sulf", "SGalCer", "diHexCer", "For", "Chol", "Desm",
#                  "CE", "TAG")


# * General functions needed for plotting ---------------------------------------------------------------------------

# Some functions needed for the summary plottings
se <- function(X, na.rm = T)
    sqrt(var(X, na.rm = na.rm) / length(X))
meanplussd <- function(X)
    mean(X) + sd(X)
meanminussd <- function(X)
    mean(X) - sd(X)
meanplusse <- function(X)
    mean(X) + se(X)
meanminusse <- function(X)
    mean(X) - se(X)

mval <- function(X) {
    return(data.frame(y = mean(X), label = paste0(round(mean(
        X
    ), 3))))
}
mvalsd <- function(X) {
    return(data.frame(y = meanplussd(X), label = paste0(round(mean(
        X
    ), 3))))
}
mvalse <- function(X) {
    return(data.frame(y = meanplusse(X), label = paste0(round(mean(
        X
    ), 3))))
}

namel <- function (vec) {
    tmp <- as.list(vec)
    names(tmp) <- as.character(unlist(vec))
    tmp
}

colMap <- function(x) {
    .col <- rep(rev(heat.colors(length(unique(
        x
    )))), time = table(x))
    return(.col[match(1:length(x), order(x))])
}

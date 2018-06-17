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
#     [1] bindrcpp_0.2.2       pcaMethods_1.70.0    Biobase_2.38.0       BiocGenerics_0.24.0  RColorBrewer_1.1-2
# [6] ggbiplot_0.55        scales_0.5.0         ggplot2_2.2.1        gplots_3.0.1         gridExtra_2.3
# [11] shinydashboard_0.7.0 shinythemes_1.1.1    DT_0.4               shinyjs_1.0          shinyBS_0.61
# [16] reshape2_1.4.3       tidyr_0.8.0          dplyr_0.7.4          plyr_1.8.4           dbplyr_1.2.1
# [21] lazyeval_0.2.1       jsonlite_1.5         shiny_1.0.5          RSQLite_2.1.0
#
# loaded via a namespace (and not attached):
#     [1] gtools_3.5.0       tidyselect_0.2.4   purrr_0.2.4        colorspace_1.3-2   htmltools_0.3.6    yaml_2.1.18
# [7] blob_1.1.1         rlang_0.2.0        pillar_1.2.1       glue_1.2.0         DBI_0.8            bit64_0.9-7
# [13] bindr_0.1.1        stringr_1.3.0      munsell_0.4.3      gtable_0.2.0       htmlwidgets_1.0    caTools_1.17.1
# [19] memoise_1.1.0      labeling_0.3       Cairo_1.5-9        crosstalk_1.0.0    httpuv_1.3.6.2     Rcpp_0.12.16
# [25] KernSmooth_2.23-15 xtable_1.8-2       gdata_2.18.0       mime_0.5           bit_1.1-12         digest_0.6.15
# [31] stringi_1.1.7      tools_3.4.4        bitops_1.0-6       magrittr_1.5       tibble_1.4.2       pkgconfig_2.0.1
# [37] rsconnect_0.8.8    assertthat_0.2.0   R6_2.2.2           compiler_3.4.4


# Database Connection functions -----------------------------------------------------------------------------------

# from Database.R
# A collection of functions that prepare the data for the plotting
# they are called by server.R and sourced by global.R

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


# Data preparation ------------------------------------------------------------------------------------------------
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


# T.Test ----------------------------------------------------------------------------------------------------------
# I should replace this with ggsignif -- JB
#
# Ttest <- function(data, within) {
#     Res <- data.frame()
#     if (within == "Sample") {
#         for (sub in unique(data$xval)) {
#             tmpsub <- filter(data, xval == sub)
#             try ({
#                 ttest <- t.test(standardizedSum ~ sample, data = tmpsub)
#                 yval <- max(ttest$estimate)
#                 Res <-
#                     rbind(Res,
#                           data.frame(
#                               xval = sub,
#                               yval = yval,
#                               pval = round(ttest$p.value, 4),
#                               sample = unique(data$sample)[1]
#                           ))
#             })
#         }
#     } else if (within == "Category") {
#         for (cat in unique(data$category)) {
#             tmp <- filter(data, category == cat)
#             for (sub in unique(tmp$xval)) {
#                 tmpsub <- filter(tmp, xval == sub)
#                 try({
#                     ttest <- t.test(standardizedSum ~ sample, data = tmpsub)
#                     yval <- max(ttest$estimate)
#                     Res <-
#                         rbind(
#                             Res,
#                             data.frame(
#                                 xval = sub,
#                                 yval = yval,
#                                 pval = round(ttest$p.value, 4),
#                                 sample = unique(data$sample)[1],
#                                 category = cat
#                             )
#                         )
#                 })
#             }
#         }
#     } else if (within == "Functional Category") {
#         for (cat in unique(data$func_cat)) {
#             tmp <- filter(data, func_cat == cat)
#             for (sub in unique(tmp$xval)) {
#                 tmpsub <- filter(tmp, xval == sub)
#                 try({
#                     ttest <- t.test(standardizedSum ~ sample, data = tmpsub)
#                     yval <- max(ttest$estimate)
#                     Res <-
#                         rbind(
#                             Res,
#                             data.frame(
#                                 xval = sub,
#                                 yval = yval,
#                                 pval = round(ttest$p.value, 4),
#                                 sample = unique(data$sample)[1],
#                                 func_cat = cat
#                             )
#                         )
#                 })
#             }
#         }
#     } else if (within == "Class") {
#         for (cat in unique(data$class)) {
#             print(cat)
#             tmp <- filter(data, class == cat)
#             for (sub in unique(tmp$xval)) {
#                 print(sub)
#                 tmpsub <- filter(tmp, xval == sub)
#                 try({
#                     ttest <- t.test(standardizedSum ~ sample, data = tmpsub)
#                     yval <- max(ttest$estimate)
#                     Res <-
#                         rbind(
#                             Res,
#                             data.frame(
#                                 xval = sub,
#                                 yval = yval,
#                                 pval = round(ttest$p.value, 4),
#                                 sample = unique(data$sample)[1],
#                                 class = cat
#                             )
#                         )
#                 })
#             }
#         }
#     }
#
#     Res$Significant <- ifelse(Res$pval < 0.001,
#                               '***',
#                               ifelse(Res$pval < 0.01, '**',
#                                      ifelse(Res$pval < 0.05, '*', '')))
#     Res$x_bar <- ifelse(Res$pval < 0.05, '_____', '')
#     Res$StarHeight <- max(Res$yval / 12.5)
#     return(Res)
# }


# Plotting functions ----------------------------------------------------------------------------------------------

### plotter.R
# The functions that prepares the actual ggplots
###########################################################
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


        ### Create the two saveable dataframes
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



# Main Plot -------------------------------------------------------------------------------------------------------
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

        #    stddata <- StandardizedNrSamp

        # Order of the x-axis
        if (what == "class") {
            plotorder <- class_order
        } else {
            plotorder <- levels(as.factor(data$xval))
        }
        data$xval <- factor(data$xval, levels = plotorder)
        #    stddata$xval <- factor(stddata$xval, levels = plotorder)

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
            #        stddata <- stddata %>% filter(category %in% data$category)
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
            #stddata <- stddata %>% filter(class %in% data$class)
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
        #pca.components numeric number of number of principal components
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

# browser()

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
    # x11()
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


# PCA functions (DeviumCommon.R) ----------------------------------------------------------------------------------

#convert to merged data column and row meta data format
binbase.format.output <- function(data, col.meta, row.meta) {
    # 'glue' col. meta on top of data and row.meta to the left
    tmp <- rbind(t(col.meta), "", data)
    space <- dim(tmp)[1] - 1
    tmp2 <-
        rbind("", matrix("", space - dim(data)[1], ncol(row.meta)), as.matrix(row.meta))
    tmp2[nrow(tmp2) - nrow(row.meta), ] <- colnames(tmp2)
    #add meta data for rows
    names <- colnames(col.meta)
    tmp2[1:length(names), ncol(tmp2)] <- names
    colnames(tmp2) <- NULL
    #combine
    tmp3 <- cbind(tmp2, tmp)
    colnames(tmp3) <- rep("", ncol(tmp3))
    return(tmp3)
}

#list to character
fixlc <- function(obj) {
    as.character(unlist(obj))
}

#list to numeric
fixln <-
    function(obj) {
        as.numeric(as.character(unlist(obj)))
    } # should encode text as factors firstƒ

#factor to numeric
fixlf <-
    function(obj) {
        as.numeric(as.factor(unlist(obj)))
    } # should encode text as factors first

#convert all columns of a data.frame or matrix to numeric (encoding characters)
afixln <- function(a, keep.factors = FALSE) {
    obj <- do.call("cbind", lapply(1:ncol(a), function(i) {
        tmp <- a[, i]
        if (is.factor(tmp) | is.character(tmp)) {
            as.numeric(as.factor(tmp))
        } else {
            tmp
        }
    }))
    #maintain object class and dimnames
    pclass <- paste0("as.", class(a))
    obj <- do.call(pclass, list(obj))
    dimnames(obj) <- dimnames(a)
    return(obj)
}

#convert all columns of a data.frame or matrix to numeric (encoding characters) optional preserving factors
afixlnf <-function(a, factors = TRUE) {
    tmp <- a
    is.num <-
        sapply(1:ncol(a), function(i) {
            all(is.na(as.numeric(as.character(a[, i]))))
        })
    tmp <-
        data.frame(do.call("cbind", lapply(tmp, function(i) {
            as.numeric(as.character(i))
        }))) # make sure dims are maintained
    if (factors) {
        tmp2 <- a[, is.num, drop = FALSE]
        tmp[, is.num] <-
            data.frame(do.call("cbind", lapply(1:ncol(tmp2), function(i) {
                data.frame(factor(tmp2[, i]))
            })))
    } else {
        tmp[, is.num] <-
            data.frame(sapply(a[, is.num, drop = FALSE], function(i) {
                as.numeric(factor(i))
            }))
    }

    #maintain object class and dimnames
    pclass <- paste0("as.", class(a))
    obj <- do.call(pclass, list(tmp))
    dimnames(obj) <- dimnames(a)
    return(obj)
}

#convert all columns of a data.frame or matrix to character
afixlc <- function(a) {
    obj <- do.call("cbind", lapply(1:ncol(a), function(i) {
        fixlc(as.factor(a[, i]))
    }))
    #maintain object class and dimnames
    pclass <- paste0("as.", class(a))
    obj <- do.call(pclass, list(obj))
    dimnames(obj) <- dimnames(a)
    return(obj)
}

#check data.frame and remove or return factor or character columns
fixlr <- function(a, .remove = TRUE) {
    id <- sapply(a, is.character) | sapply(a, is.factor)
    if (.remove) {
        a[, !id, drop = FALSE]
    } else {
        a[, id, drop = FALSE]
    }
}

#transpose data and fix numeric/factors
fixlt <- function(obj) {
    tmp <- t(obj)
    #check what can be numeric
    # fct<-apply(apply(apply(tmp,2,as.numeric),2,is.na),2,all) # a better way must exist?
    fct <-
        sapply(1:ncol(tmp), function(i) {
            all(is.na(as.numeric(tmp[, i])))
        })

    #set everything else to factor
    tmp2 <-
        as.data.frame(apply(sapply(data.frame(tmp), as.character), 2, as.numeric))
    tmp2[, fct] <- data.frame(tmp[, fct, drop = FALSE])
    return(tmp2)
}

#remove unused levels in factors
#before droplevels
fixfactors <- function(obj) {
    #check what can be numeric
    # fct<-apply(apply(apply(tmp,2,as.numeric),2,is.na),2,all) # a better way must exist?
    fct <- sapply(obj, is.factor)
    tmp <- obj[, fct, drop = FALSE]
    #set everything else to factor
    tmp2 <-
        data.frame(apply(sapply(data.frame(tmp), as.character), 2, as.factor))
    obj[, fct] <- data.frame(tmp2)
    return(obj)
}

#import from clipboard
read.excel <- function(type = "with.dimnames") {
    #assume row and colnames are included but may not be unique
    tmp <- read.table("clipboard-128", sep = "\t")
    devium.data.format(tmp, type)
}

#write to clipboard
write.to.clipboard <- function(obj, type = "no.name") {
    #obj = to be written to clipboard
    #type determines dimensions and their names

    switch(
        type,
        with.dimnames = .local <- function(obj)
        {
            obj.names <- list(rownames = rownames(obj), colnames = colnames(obj))
            obj <- obj
            is.error <- NULL
            is.error <- tryCatch(
                dimnames(obj) <- obj.names,
                error = function(e)
                {
                    NULL
                }
            )#need to set as protected
            if (is.null(is.error)) {
                list(
                    data = obj,
                    rownames = obj.names$rownames,
                    colnames = obj.names$colnames
                )
            } else {
                obj
            }
        },

        with.name = .local <- function(obj)
        {
            obj.names <-
                list(1:((length(
                    as.character(unlist(obj))
                )) - 1), as.character(unlist(obj)[1]))
            obj <- as.data.frame(as.character(unlist(obj))[-1])
            is.error <- NULL
            is.error <- tryCatch(
                dimnames(obj) <- obj.names,
                error = function(e)
                {
                    NULL
                }
            )#need to set as protected
            if (is.null(is.error)) {
                list(
                    data = obj,
                    rownames = obj.names$rownames,
                    colnames = obj.names$colnames
                )
            } else {
                obj
            }
        },
        no.name = .local <- function(obj)
        {
            obj.names <- list(1:((length(
                as.character(unlist(obj))
            ))), "NA")
            obj <- as.data.frame(as.character(unlist(obj)))
            is.error <- NULL
            is.error <- tryCatch(
                dimnames(obj) <- obj.names,
                error = function(e)
                {
                    NULL
                }
            )#need to set as protected
            if (is.null(is.error)) {
                list(
                    data = obj,
                    rownames = obj.names$rownames,
                    colnames = obj.names$colnames
                )
            } else {
                obj
            }
        }
    )

    tmp <- .local(obj)
    #bind row names and col names with obj
    tmp.obj <- cbind(c("", rownames(tmp)), rbind(colnames(tmp), tmp))
    write.table(
        as.matrix(tmp.obj),
        "clipboard-128",
        sep = "\t",
        row.names = FALSE,
        col.names = FALSE
    )
}

#format data objects: identify headers and meta data, and give sepearately
devium.data.format <- function(obj, type) {
    #type can be with.dimnames
    # with.meta.data

    switch(
        type,
        with.dimnames = .local <- function(obj)
        {
            obj.names <-
                list(rownames = as.character(unlist(obj[-1, 1])),
                     colnames = as.character(unlist(obj[1, -1])))
            #obj<-as.data.frame(matrix(as.numeric(as.character(unlist(obj[-1,-1]))),nrow=length(obj.names$rownames), ncol=length(obj.names$colnames))) # as.numeric
            obj <-
                as.data.frame(matrix(
                    as.character(unlist(obj[-1, -1])),
                    nrow = length(obj.names$rownames),
                    ncol = length(obj.names$colnames)
                ))

            is.error <- NULL
            is.error <- tryCatch(
                dimnames(obj) <- obj.names,
                error = function(e)
                {
                    NULL
                }
            )#need to set as protected
            if (is.null(is.error)) {
                list(
                    data = obj,
                    rownames = obj.names$rownames,
                    colnames = obj.names$colnames
                )
            } else {
                obj
            }
        },

        with.name = .local <- function(obj)
        {
            obj.names <-
                list(1:((length(
                    as.character(unlist(obj))
                )) - 1), as.character(unlist(obj)[1]))
            obj <- as.data.frame(as.character(unlist(obj))[-1])
            is.error <- NULL
            is.error <- tryCatch(
                dimnames(obj) <- obj.names,
                error = function(e)
                {
                    NULL
                }
            )#need to set as protected
            if (is.null(is.error)) {
                list(
                    data = obj,
                    rownames = obj.names$rownames,
                    colnames = obj.names$colnames
                )
            } else {
                obj
            }
        },
        no.name = .local <- function(obj)
        {
            obj.names <- list(1:((length(
                as.character(unlist(obj))
            ))), "NA")
            obj <- as.data.frame(as.character(unlist(obj)))
            is.error <- NULL
            is.error <- tryCatch(
                dimnames(obj) <- obj.names,
                error = function(e)
                {
                    NULL
                }
            )#need to set as protected
            if (is.null(is.error)) {
                list(
                    data = obj,
                    rownames = obj.names$rownames,
                    colnames = obj.names$colnames
                )
            } else {
                obj
            }
        },
        with.meta.data = .local <- function(obj)
        {
            #look for "" and set headers based on first non "" row and column (add other patterns here)
            object <- obj

            #identify boundaries of data the rest is meta data
            sample.start <- max(which(object[1, ] == "") + 2)
            variable.start <- max(which(object[, 1] == "") + 2)

            #data
            drows <- nrow(object)
            dcols <-
                max(c(1:ncol(object))[!sapply(1:ncol(object), function(i) {
                    sum(is.na(object[, i])) == drows
                })])
            tmp.data <-
                as.data.frame(as.matrix(object[c(variable.start:drows), c(sample.start:dcols)]))
            .names <- dimnames(tmp.data)

            #make sure all factors converted to numeric
            tmp.data <-
                as.data.frame(matrix(
                    as.numeric(as.character(unlist(tmp.data))),
                    nrow = nrow(tmp.data),
                    ncol = ncol(tmp.data)
                ))
            dimnames(tmp.data) <- .names

            #get variable and sample meta data
            row.meta <-
                as.data.frame(t(as.matrix(object[c(1:(variable.start - 2)), c(sample.start:dcols)])))
            col.meta <-
                as.data.frame(as.matrix(object[c(variable.start:drows), c(1:(sample.start -
                                                                                 1))]))

            #attempt to set colnames
            tryCatch(
                colnames(row.meta) <-
                    as.character(unlist(obj[1:(variable.start - 2), (sample.start - 1)])),
                errro = function(e) {
                }
            )
            tryCatch(
                colnames(col.meta) <-
                    as.character(unlist(obj[(variable.start - 1), 1:(sample.start - 1)])),
                errro = function(e) {
                }
            )


            #convert data to all numeric prior to return
            data <-
                do.call("cbind", unclass(tmp.data))
            dimnames(tmp.data) <- .names # need to break factors

            #return results as a list
            list(
                data = tmp.data,
                row.metadata = row.meta,
                col.metadata = col.meta
            )
        }
    )
    .local(obj)

}

#format binbase output (data with meta data structure)
format.binbase.output <- function(data) {
    #first row must have some ID
    #data = name as string
    object <-
        get(data)#need to adjust for first row taken as column names
    #format data object
    #use row 1 and column 1 to figure out meta data space
    row.id <- max(c(1:ncol(object))[is.na(object[1, ]) |
                                        object[1, ] == ""])
    col.id <- max(c(1:nrow(object))[is.na(object[, 1]) |
                                        object[, 1] == ""])

    # sample.start<-which(colnames(object)=="file.id")+1
    # variable.start<-which(object[,1]=="BinBase name")+1

    sample.start <- row.id + 2
    variable.start <- col.id + 2

    #data
    drows <- nrow(object)
    dcols <-
        max(c(1:ncol(object))[!sapply(1:ncol(object), function(i) {
            sum(is.na(object[, i])) == drows
        })])
    tmp.data <-
        as.data.frame(t(as.matrix(object[c(variable.start:drows), c(sample.start:dcols)])))
    row.names <- rownames(tmp.data)
    #make sure all factors are converted to numeric
    tmp.data <-
        as.data.frame(matrix(
            as.numeric(as.character(unlist(tmp.data))),
            nrow = nrow(tmp.data),
            ncol = ncol(tmp.data)
        ))
    tmp.data <- do.call("cbind", unclass(tmp.data))
    dimnames(tmp.data) <- list(1:nrow(tmp.data), 1:ncol(tmp.data))

    #get variable and sample meta data
    #row meta (everything is transposed ... bad)
    row.meta <-
        as.data.frame(t(as.matrix(object[c(1:(variable.start - 2)), c(sample.start:dcols)]))) # skip empty line above data
    colnames(row.meta) <-
        object[c(1:(variable.start - 2)), sample.start - 1]

    #column meta
    col.meta <-
        as.data.frame(as.matrix(object[c(variable.start:drows), c(1:(sample.start -
                                                                         1))]))
    # colnames(col.meta)<-object[row.id,1:(variable.start)]
    colnames(col.meta) <-
        object[variable.start - 1, 1:sample.start - 1]# why does above work some times?

    #convert data to all numeric prior to return
    # need to break factors

    #return results as a list
    list(
        data = data.frame(tmp.data),
        row.metadata = data.frame(row.meta),
        col.metadata = data.frame(col.meta)
    )
}


#return objects to excel
return.to.Excel <-
    function(workbook.path = "new",
             return.obj.list,
             return.name = names(return.obj.list),
             workbook.name = NULL)
    {
        check.get.packages(c("XLConnect"))

        #load workbok
        if (!workbook.path == "new")
        {
            #connect to worksheet
            old.dir <- getwd()
            wd <- dirname(workbook.path)
            workbook.name <- basename(workbook.path)
            setwd(wd)
            wb = loadWorkbook(workbook.name, create = FALSE)

        } else{
            if (is.null(workbook.name)) {
                workbook.name <-
                    paste('Workbook',
                          format(Sys.time(), "%Y.%m.%d_%I_%M_%p"),
                          "xls",
                          sep = ".")
            }
            wb = loadWorkbook(workbook.name, create = TRUE)
        }

        #place objects in workbook
        sapply(1:length(return.name), function(i)
        {
            obj.name <- rename(x = return.name[i], pattern = c(" ", "\\$"))
            #create sheet
            createSheet(wb, name = obj.name)

            #delete name is if exists
            tryCatch(
                removeName(wb, obj.name),
                error = function(e) {
                }
            )
            createName(wb,
                       name = obj.name,
                       formula = paste(obj.name, "$A$1", sep = "!"))

            #bind rownames else not visible
            return <-
                cbind(rownames(return.obj.list[[i]]), return.obj.list[[i]])
            colnames(return)[1] <- "Variables"
            writeNamedRegion(wb,
                             data = return,
                             name = obj.name,
                             header = TRUE)

            #add excel auto filters need to avoid text and mean +/- stdev column else all all broken
            corners = getReferenceCoordinates(wb, obj.name)
            #don't include first column of variable names
            corners[1, 2] <- (corners[2, 2] - (corners[2, 2] - 2))
            corners[2, 2] <- corners[2, 2] - 1
            setAutoFilter(wb,
                          sheet = obj.name,
                          reference = aref(corners[1, ], corners[2, ]))
        })

        saveWorkbook(wb)
    }

#functions to calculate ranges for excel placement
list.placement.full <-
    function(data.list,
             list.names,
             direction,
             start.col,
             start.row,
             spacer) {
        set.1 <- list.object.dim.full(data.list, list.names)
        col.i <-
            matrix(paste(rbind(
                matrix(LETTERS, ncol = 1), matrix(paste(
                    rep(LETTERS, each = length(LETTERS)), rep(LETTERS, length(LETTERS)), sep =
                        ""
                ), ncol = 1)
            ), "$", sep = ""))
        row.i <- matrix(1:1e6, ncol = 1)
        place.row <- matrix()
        place.col <- matrix()
        place.range <- matrix()
        columns <- matrix()
        rows <- matrix()
        n <- dim(set.1)[1]
        i <- 1
        for (i in 1:n) {
            place.row[i] <-
                start.row + sum(unlist(set.1[1:i, 3])) + spacer * (i - 1) - unlist(set.1[i, 3])
            place.col[i] <-
                col.i[start.col + sum(unlist(set.1[1:(i), 2])) + spacer * (i - 1) - unlist(set.1[i, 2])]
            if (direction == "vertical") {
                place.range[i] <-
                    matrix(paste(col.i[start.col], place.row[i], sep = ""), ncol = 1)
            } else{
                if (direction == "horizontal") {
                    place.range[i] <-
                        matrix(paste(place.col[i], start.row, sep = ""), ncol = 1)
                }
            }
        }
        ex.range <- as.data.frame(cbind(set.1, place.range))
        ex.range
    }

# accesory fxn to list.placement.full
list.object.dim.full <- function(data.list, list.names)
{
    l.dim <- list()
    n <- length(data.list)
    i <- 1
    for (i in 1:n)
    {
        tmp.list <- as.data.frame(data.list[[i]])
        height <- dim(tmp.list)[1]
        width <- dim(as.data.frame(tmp.list))[2]
        l.dim[[i]] <-
            as.data.frame(matrix(cbind(width, height), ncol = 2))
    }
    out <- do.call("rbind", l.dim)
    out <- cbind(list.names, out)
    colnames(out) <- c("objects", "width", "height")
    out
}

#write object to an excel sheet based on positional location of top left corner
multi.object.XL <-
    function(workbook.path = "new",
             placement.list,
             workbook.name = NULL,
             sheet.name = "Output")
    {
        check.get.packages(c("XLConnect"))

        #load workbok
        if (!workbook.path == "new")
        {
            #connect to worksheet
            old.dir <- getwd()
            wd <- dirname(workbook.path)
            workbook.name <- basename(workbook.path)
            setwd(wd)
            wb = loadWorkbook(workbook.name, create = FALSE)

        } else{
            if (is.null(workbook.name)) {
                workbook.name <-
                    paste('Workbook',
                          format(Sys.time(), "%Y.%m.%d_%I_%M_%p"),
                          "xls",
                          sep = ".")
            }
            wb = loadWorkbook(workbook.name, create = TRUE)
        }

        #create worksheet
        createSheet(wb, name = sheet.name)

        #create named regions and return objects
        sapply(1:nrow(placement.list), function(i)
        {
            print(i)
            name <- fixlc(placement.list[i, 1])
            #delete name is if exists
            tryCatch(
                removeName(wb, name),
                error = function(e) {
                }
            )
            #fix formula for colukn position
            name.location <-
                paste(sheet.name, paste("$", unlist(placement.list[i, 4]), sep = ""), sep =
                          "!") # too lazy too rewrite depnedancy must accept uglness for now
            createName(wb, name = name, formula = name.location)

            #bind rownames else not visible
            return <- get(name)
            writeNamedRegion(wb,
                             data = return,
                             name = name,
                             header = TRUE)

        })

        saveWorkbook(wb)
    }

#get object from EXCEL
get.from.Excel <-
    function(workbook.path = NULL,
             get.object.sheet = NULL,
             get.obj.name = NULL,
             return = TRUE,
             environment = .GlobalEnv)
    {
        check.get.packages(c("XLConnect"))
        old.dir <- getwd() # original working directory

        #check to see if workbook exists
        get.workbook <- function(workbook.path)
        {
            wd <- dirname(workbook.path)
            setwd(wd)
            loadWorkbook(basename(workbook.path))
        }
        cat("Loading workbook", "\n")
        workbook <-
            tryCatch(
                get.workbook(workbook.path),
                error = function(e) {
                    NULL
                }
            )

        if (is.null(workbook))
        {
            return(cat("Could not load workbook", "\n"))
        } else {
            #check if named range exists
            obj <-
                tryCatch(
                    readNamedRegion(workbook, name =  get.obj.name , header = FALSE),
                    error = function(e) {
                        NULL
                    }
                )

            if (is.null(obj))
            {
                #check if worksheet exists
                obj <-
                    tryCatch(
                        readWorksheet(workbook, sheet = get.object.sheet),
                        error = function(e) {
                            NULL
                        }
                    )
                if (is.null(obj))
                {
                    return(cat("the object can't be loaded", "\n"))
                } else {
                    #assign sheet to environment
                    tmp.obj <- get.object.sheet
                }
            } else {
                #assign named range to environment
                tmp.obj <- get.obj.name
            }

        }
        setwd(old.dir)
        #return obj
        #assign
        assign(tmp.obj, obj, envir = environment)
        if (return) {
            get(tmp.obj, envir = environment)
        }
    }

#collapse columns as strings (why am I not using paste collapse?)
join.columns <-
    function(obj = formatted$row.metadata[, 4:6],
             char = "|",
             quote.last = FALSE)
    {
        if (class(obj) == "list") {
            obj <- as.matrix(obj[[1]])
        } else {
            obj <- as.matrix(obj)
        }
        #used to binf 2 columns try to generalize to more
        .local <- function(obj, char, quote.last)
        {
            if (length(obj) == 0)
            {
                return(NULL)
            } else{
                if (ncol(as.matrix(obj)) >= 2)
                {
                    n <- ncol(obj)
                    out <- data.frame()
                    sobj <- obj
                    i <- 1
                    for (i in 1:(n - 1))
                    {
                        if (quote.last == TRUE)
                        {
                            sobj[, i] <-
                                paste(
                                    as.character(obj[, i]),
                                    paste("'", as.character(obj[, i + 1]), "'", sep = ""),
                                    sep = char
                                )
                        } else {
                            sobj[, i] <-
                                paste(as.character(obj[, i]),
                                      as.character(obj[, i + 1]),
                                      sep = char)
                        }
                    }
                    sobj[, -n]
                } else{
                    obj
                }
            }
        }
        if (ncol(obj) > 2)
        {
            tmp <- obj[, 1:2]
            for (i in 1:(ncol(obj) - 1)) {
                tmp2 <- .local(tmp, char = char, quote.last = quote.last)
                if (i < (ncol(obj) - 1)) {
                    tmp <- cbind(tmp2, obj[, (i + 2), drop = FALSE])
                }
            }
            tmp2

        } else {
            .local(obj = obj,
                   char = char,
                   quote.last = quote.last)
        }
    }

#accesory function to return position of first instance of unique object
unique.id <- function(obj)
{
    tmp <- as.factor(obj)
    id <- seq(along = obj)
    sapply(1:nlevels(tmp), function(i)
    {
        id[tmp == levels(tmp)[i]][1]
    })
}

#function to check for packages and attempt to download if not found
check.get.packages <- function(pkg) {
    options(warn = -1)

    # #make sure bio conductor is one of the repositories
    # #will need a mechanism to make sure this stays upto date
    # if(!all(names(getOption("repos"))%in%"BioCsoft"))
    # {
    # r<-getOption("repos")
    # r["BioCsoft"]<-"http://www.bioconductor.org/packages/2.11/bioc" # needs to be specific version
    # options(repos = r)

    # #add layer to call
    # #source("http://bioconductor.org/biocLite.R")
    # #biocLite("package to install")

    # if install fails
    # #r.version<-paste(sessionInfo()$R.version$major,sessionInfo()$R.version$minor, sep=".")
    # #bioc.url<-paste("http://www.bioconductor.org/packages/",r.version,"/bioc", sep="")
    # #r["BioCsoft"]<-bioc.url # needs to be specific to R version
    # #options(repos = r)
    # }

    res <- character()

    need <- as.matrix(sapply(1:length(pkg), function(i)
    {
        if (require(pkg[i], character.only = TRUE) == FALSE)
        {
            res <- c(res, pkg[i])
        }
    }))

    need <- as.character(unlist(need[!is.null(need)]))
    if (length(need) > 0)
    {
        x <- sapply(need, install.packages, dependencies = TRUE)
        lib.fun <- function(need) {
            sapply(1:length(need), function(i) {
                out <-
                    tryCatch(
                        library(need[i], character.only = TRUE),
                        error = function(e) {
                            need[i]
                        }
                    )
                if (all(out == need[i])) {
                    need[i]
                }
            })
        }

        out <- as.character(unlist(lib.fun(need)))

        #try bioconductor
        if (length(out) > 0) {
            cat(paste("Package not found, trying Bioconductor..."),
                "\n")
            source("http://bioconductor.org/biocLite.R")
            lib.fun.bioc <- function(need) {
                sapply(1:length(need), function(i) {
                    tryCatch(
                        biocLite(need[i], ask = FAlSE),
                        error = function(e) {
                            need[i]
                        }
                    )
                })
            }

            tmp <- lib.fun.bioc(out)
            final <- lib.fun(tmp)
            if (length(final) > 0) {
                cat(paste(
                    "could not find package: ",
                    paste(as.character(unlist(
                        final
                    )), collapse = ", "),
                    sep = ""
                ),
                "\n")
            }
        }

    }

}

#function to extract objects based on reference
extract.on.index <-
    function(database,
             index = database[, 1, drop = FALSE],
             what,
             extract.on = "row")
    {
        # the merge function should be used instead
        if (extract.on == "col") {
            database <- t(database)
        }
        #assume top row are column names
        col.names <- database[1, ]

        #first column of what and database are the index for extractions
        # cbind objects based on matching index values
        index.w <- as.character(what)
        ref <- as.data.frame(index)

        out <- lapply(1:ncol(ref), function(j)
        {
            index.d <- as.character(ref[, j])#as.character(database[,1])
            index.w <- index.w[index.w %in% index.d]
            tmp.database <- database
            out <- matrix(NA, length(index.w), ncol(tmp.database), 1)
            e.index <- c(1:nrow(database))
            i <- 1
            for (i in 1:length(index.w))
            {
                if (any(index.w[i] %in% index.d))
                {
                    ext <- unique(e.index[index.w[i] == index.d])[1]
                    out[i, ] <- tmp.database[ext, ]
                }

            }

            # placing rownames in a column to avoid duplicate error
            out <- as.matrix(cbind(index.w, out))

            if (extract.on == "col")
            {
                out <- cbind(col.names, as.data.frame(t(out)))
                #fix column names
                col.names <- as.character(unlist(out[1, ]))
                out <- out[-1, ]
                colnames(out) <- col.names
            } else{
                out <- as.data.frame(out)
                colnames(out) <- col.names
            }
        })
        return(out)
    }

#check object value or set default on condition
if.or <- function(object,
                  if.value = NULL,
                  default,
                  environment = devium)
{
    obj <-
        tryCatch(
            svalue(get(object, envir = environment)),
            error = function(e) {
                NA
            }
        )
    if (!any(obj %in% c(if.value, NA))) {
        return(obj)
    } else{
        return(default)
    }
}

#get unassigned variables from within data frame
gget <- function(obj)
{
    #break obj on $
    # [1] = data frame
    # [2] = variable name
    # return object
    tmp <- unlist(strsplit(obj, "\\$"))
    if (!length(tmp) == 0)
        get(tmp[1])[, tmp[2]]
    else
        NULL
}

#function to connect to google docs
# GetGoogleDoc<-function(account,password,connection="new")
# 	{
# 		#returns list
# 		# [1] = connection name
# 		# [2] = names of documents
# 		#  connection = as.character connection name if already made using this function
# 		# and stored in the envir= googDocs
#
# 		#install RGoogleDocs if not available
# 		if(require("RGoogleDocs")==FALSE)
# 			{
# 				install.packages("RGoogleDocs", repos = "http://www.omegahat.org/R", type="source")
# 				library("RGoogleDocs")
# 			}
#
# 		if(connection == "new")
# 			{
# 					#make time stampped name for connection
# 					con.name<-con.name.txt<-paste('connection',format(Sys.time(), "%Y.%m.%d_%I_%M_%p"), sep = ".")
#
# 					#set options to avoid ssl error
# 					options(RCurlOptions = list(capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))
#
# 					#assign to new envir
# 					assign("googDocs",new.env(),envir=.GlobalEnv)
# 					assign(con.name,getGoogleDocsConnection(getGoogleAuth(account, password, service ="wise")), envir = googDocs )
# 			} else {
# 					con.name<-con.name.txt<-connection
# 					}
#
# 		docs<-getDocs(tryCatch(get(con.name,envir=googDocs),error=function(e){stop(paste(connection, " does not exist","\n"))}))
# 		dnames<-names(docs)
# 		return(list(connection = con.name.txt , names = dnames))
# 	}

#function to view excel objects
viewExcelObject <- function(obj.path)
{
    #connect to file and view:
    #worksheet names
    #named ranges


    #load workbook
    old.dir <- getwd()
    wd <- dirname(obj.path)
    workbook <- basename(obj.path)
    setwd(wd)
    wb = loadWorkbook(workbook, create = FALSE)

    #get sheet names
    all.worksheets <- getSheets(wb)

    #get all valid named ranges
    all.named.ranges <- getDefinedNames(wb, validOnly = TRUE)
    setwd(old.dir)
    return(list(worksheets = all.worksheets, named.ranges = all.named.ranges))
    1
}


# ***accesory functions based/from package pmg --------------------------------------------------------------------

Paste <- function (..., sep = "", collapse = "")
{
    x = unlist(list(...))
    x = x[!is.na(x)]
    x = x[x != "NA"]
    paste(x, sep = sep, collapse = collapse)
}

is.gWindow <- function (obj)
{
    is(obj, "gWindowRGtk")
}

rpel <- function (string, envir = .GlobalEnv)
{
    eval(parse(text = string), envir = envir)
}

#function to get gwidget svalues for assigned widgets
d.get <-
    function(object,
             main.object = "devium.pca.object",
             envir = devium)
    {
        #check to see if main object exists else create
        sapply(1:length(object), function(i)
        {
            tmp <- svalue(get(object[i], envir = envir))
            d.assign(
                add.obj = object[i],
                value = tmp,
                main.object,
                envir = envir
            )
        })
    }

#function to get gwidget svalues for assigned widgets
#main.object and its envir as string
check.get.obj <-
    function(object,
             main.object = "devium.pca.object",
             envir = "devium")
    {
        #check to see if main object exists else create
        check.get.envir(main.object, envir)
        env <- get(envir)
        sapply(1:length(object), function(i)
        {
            tmp <- svalue(get(object[i], envir = get(envir)))
            d.assign(
                add.obj = object[i],
                value = tmp,
                main.object,
                envir = get(envir)
            )
        })
    }

#check to see if environment exists else create one
check.get.envir <- function(main.object, envir)
{
    if (!exists(envir)) {
        assign(envir, new.env(), envir = .GlobalEnv)
    }
    if (!exists(main.object, envir = get(envir))) {
        assign(main.object, list(), envir = get(envir))
    }
}

#fxn to create the environment "devium" if it does not exist
create.devium.env <- function()
{
    if (!exists("devium"))
    {
        if (!is.environment("devium")) {
            assign("devium", new.env(), envir = .GlobalEnv)
        }

        #check for devium objects and set to null if they don't exist
        for (i in c("devium.helpBrowser.window",
                    "devium.plotnotebook.window"))
        {
            if (!exists(i))
            {
                assign(i, NULL, envir = devium)
            }
        }
    }
}

#function to make assignments to storage object
d.assign <-
    function(add.obj, value, main.object, envir = devium)
        #main.object="devium.pca.object"
    {
        .local <- function()
        {
            tmp <- get(main.object, envir = envir)
            tmp[[add.obj]] <- value
            assign(get("main.object"), tmp, envir = devium)
        }
        tryCatch(
            .local(),
            error = function(e) {
            }
        )
    }

#from plyr: get as text
. <- function (..., .env = parent.frame())
{
    structure(as.list(match.call()[-1]), env = .env, class = "quoted")
}

#fix character symbols
check.fix.names <-
    function(names,
             ok.chars = c(".", "_", ",", "(", ")", ":", " "),
             replace.with = ".") {
        obj <- as.list(names)
        fixlc(lapply(1:length(obj), function(i)
        {
            #check and replace bad (not allowed as range names in Excel) characters
            tmp <- unlist(strsplit(as.character(obj[i]), ""))
            check.index <- c(1:length(tmp))
            OK.char <- c(LETTERS, letters, ok.chars)
            replace.index <- check.index[!tmp %in% OK.char]
            is.number <- (check.index)[!is.na(tmp == fixln(tmp))]
            if (length(is.number) == 0) {
                is.number <- 0
            }
            replace <- replace.index[!replace.index %in% is.number]
            tmp[replace] <- replace.with
            tmp <- paste(tmp, collapse = "")
            obj <- paste(fixlc(strsplit(tmp, " ")), collapse = replace.with)
        }))
    }

#get top triangle of possible pairwise interactions
#accessory function
all.pairs <- function(r, type = "one") {
    switch(
        type,
        one = list(first = rep(1:r, rep(r, r))[lower.tri(diag(r))], second = rep(1:r, r)[lower.tri(diag(r))]),
        two = list(first = rep(1:r, r)[lower.tri(diag(r))], second = rep(1:r, rep(r, r))[lower.tri(diag(r))])
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


# Database Connections --------------------------------------------------------------------------------------------
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


# Names for plots -------------------------------------------------------------------------------------------------
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



# General functions needed for plotting ---------------------------------------------------------------------------
# Verification that the ttest gets the desired two samples
# check_ttest <- function(data, input) {
#     if ("ttest" %in% input$checkGroup)
#     {
#         if (length(unique(data$sample)) == 2 |
#             length(unique(input$samplesub)) == 2) {
#             return(TRUE)
#         }
#         else {
#             return(FALSE)
#         }
#     }
#     return(TRUE)
# }

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

##### Plot the factors in the order specified
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

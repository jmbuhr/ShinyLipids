### Database.R
# A collection of functions that prepare the data for the plotting
# they are called by server.R and sourced by global.R

samplecols <- list("sample", "sample_replicate")

###########################################################
# This function takes an ID and a string representing a table
# and returns a "link" to the corresponding data.frame()
# Note: This data.frame still needs to be collected later for plotting
getraw <- function(ID,data) {
    sql_data <- paste("SELECT * FROM",data,"WHERE ID =", ID)
    data <- tbl(database_connection, sql(sql_data))
    return(data)
}



# Data preparation ------------------------------------------------------------------------------------------------
prepareData <- function (rawdata, what = "", within = "", standard = "", ID,
                         standardSubset = FALSE, add = FALSE,
                         samplesub = input$samplesub,
                         withsub = input$withsub,
                         whatsub = input$whatsub,
                         repsub = input$repsub,
                         techsub = input$techsub){
    filter_crit <- interp(~ !is.na(filter_var), filter_var = as.name(what))
    rawdata <- rawdata %>% filter_(filter_crit) %>% filter(!is.na(value))

    # Remove technical replicates
    bar <<- techsub
    if (!is.null(techsub)) {
        rawdata <- rawdata %>% filter(!(sample_replicate_technical %in% techsub))
    }

    # Averaging over the technical replicates
    rawdata <- rawdata %>%
        group_by(id, lipid, category, func_cat, class, length,
                 db, oh, chains, chain_sums, sample, sample_replicate) %>%
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
                    filter_crit <- interp(~ what == whatsub, what = as.name(what))
                    rawdata <- rawdata %>% filter_(filter_crit)
                } else {
                    filter_crit <- interp(~ what %in% whatsub, what = as.name(what))
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
                    filter_crit <- interp(~ !(what == whatsub), what = as.name(what))
                    rawdata <- rawdata %>% filter_(filter_crit)
                }
                else {
                    filter_crit <- interp(~ !(what %in% whatsub), what = as.name(what))
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

    Standardized <- left_join(Aggregated, Standard, type = "left") %>%
        mutate(standardizedSum = (sum / standardSum) * 100)


    StandardizedNrSamp <- Standardized %>%
        group_by(.dots = aggreGroup) %>%
        # summarise(MeanOfTechnicalReplicates = mean(standardizedSum, na.rm = T)) %>%
        group_by(.dots = smasGroup) %>%
        summarise(Mean = mean(standardizedSum, na.rm = T),
                  Median = median(standardizedSum, na.rm = T),
                  SD = sd(standardizedSum, na.rm = T),
                  SE = se(standardizedSum, na.rm = T),
                  NrSamp = n())

    print("group_by was used 3")


    Standardized <- left_join(Standardized, StandardizedNrSamp)

    print("standardized created")

    Standardized <- rename_(Standardized, .dots = list(xval = what))
    StandardizedNrSamp <<- StandardizedNrSamp %>%
        rename_(.dots = list(xval = what))
    # print(unique(rawdata$sample))
    samplenames <- as.list(unique(rawdata$sample))
    # print("samplenames")
    # print(samplenames)
    names(samplenames) <- unique(rawdata$sample)
    # print(samplenames)

    print("Dataprep successful with prepareData")
    return(Standardized)
}


# T.Test ----------------------------------------------------------------------------------------------------------
# I should replace this with ggsignif -- JB

Ttest <- function(data, within) {
    Res <- data.frame()
    if (within == "Sample") {
        for (sub in unique(data$xval)) {
            tmpsub <- filter(data, xval == sub)
            try ({
                ttest <- t.test(standardizedSum ~ sample, data = tmpsub)
                yval <- max(ttest$estimate)
                Res <- rbind(Res, data.frame(xval = sub, yval = yval,
                                             pval = round(ttest$p.value,4),
                                             sample = unique(data$sample)[1] ))
            })
        }
    } else if (within == "Category") {
        for (cat in unique(data$category)) {
            tmp <- filter(data, category == cat)
            for (sub in unique(tmp$xval)) {
                tmpsub <- filter(tmp, xval == sub)
                try({
                    ttest <- t.test(standardizedSum ~ sample, data = tmpsub)
                    yval <- max(ttest$estimate)
                    Res <- rbind(Res, data.frame(xval = sub, yval = yval,
                                                 pval = round(ttest$p.value,4),
                                                 sample = unique(data$sample)[1],
                                                 category = cat))
                })
            }
        }
    }else if (within == "Functional Category") {
        for (cat in unique(data$func_cat)) {
            tmp <- filter(data, func_cat == cat)
            for (sub in unique(tmp$xval)) {
                tmpsub <- filter(tmp, xval == sub)
                try({
                    ttest <- t.test(standardizedSum ~ sample, data = tmpsub)
                    yval <- max(ttest$estimate)
                    Res <- rbind(Res, data.frame(xval = sub, yval = yval,
                                                 pval = round(ttest$p.value,4),
                                                 sample = unique(data$sample)[1],
                                                 func_cat = cat))
                })
            }
        }
    } else if (within == "Class") {
        for (cat in unique(data$class)) {
            print(cat)
            tmp <- filter(data, class == cat)
            for (sub in unique(tmp$xval)) {
                print(sub)
                tmpsub <- filter(tmp, xval == sub)
                try({
                    ttest <- t.test(standardizedSum ~ sample, data = tmpsub)
                    yval <- max(ttest$estimate)
                    Res <- rbind(Res, data.frame(xval = sub, yval = yval,
                                                 pval = round(ttest$p.value,4),
                                                 sample = unique(data$sample)[1],
                                                 class = cat))
                })
            }
        }
    }

    Res$Significant <- ifelse(Res$pval < 0.001, '***',
                              ifelse(Res$pval < 0.01, '**',
                                     ifelse(Res$pval < 0.05, '*', '')))
    Res$x_bar <- ifelse(Res$pval<0.05,'_____','')
    Res$StarHeight <- max(Res$yval/12.5)
    return(Res)
}


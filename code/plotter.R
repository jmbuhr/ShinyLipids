### plotter.R
# The functions that prepares the actual ggplots
# library(ggthemes)
###########################################################
prepareSubset <- function(data, advanced, samplesub = NA, whatsub = NA, 
                          withsub = NA, within = within, repsub = NA, base = NA) {
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
  if(within == "Category"){
    csvdata <- csvdata %>% 
      mutate(xval = paste(category, xval, sep='-'))
  } else if (within == "Functional Category") {
    csvdata <- csvdata %>% 
      mutate(xval = paste(func_cat, xval, sep='-'))
  }else if (within == "Class") {
    csvdata <- csvdata %>% 
      mutate(xval = paste(class, xval, sep='-'))
  }
  tidySave <<- csvdata %>%    
    ungroup() %>%   
    mutate(biolSample = paste(sample, sample_replicate)) %>% 
    group_by(biolSample, xval) %>%    
    summarize(value = sum(standardizedSum))%>% 
    select(Sample = biolSample, Feature =xval, value) %>%   
    spread("Sample", "value")
  
  
  # print("Been there, done that!") 
  return(data)
}




###########################################################
preparePlots <- function(data, checkGroup, plottype, what, within = within, 
                         standard = standard, advanced, ...) {
  print("Plotting called")
  
  stddata <- StandardizedNrSamp 

  # Order of the x-axis
  if (what == "class") {
    plotorder <- class_order
  } else {
    plotorder <- levels(as.factor(data$xval))
  }
  data$xval <- factor(data$xval, levels = plotorder)
  stddata$xval <- factor(stddata$xval, levels = plotorder)
  
  # Color related options (remove yellow (i.e. value 6))
  colorCount <- length(unique(data$sample))
  getPalette <- colorRampPalette(brewer.pal(9, "Set1")[-6]) 
  
  # Specifying the dodging
  dodge <- position_dodge(width = 0.9)
  
  ## Decide on Plottype
  if (plottype == "barplot") {
    graphic <- ggplot(data = data, aes(x = xval, 
                                       y = Mean, 
                                       fill = sample, group = sample)) +
      geom_bar(position = dodge, stat = "identity")
      # stat_summary(fun.y = mean, geom = "bar", position = dodge)
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
    graphic <- ggplot(data = data, aes(x = xval, y = standardizedSum, 
                                       fill = sample)) +
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
    graphic <- ggplot(data = data, aes(x = xval, y = Mean, ymin = Mean - SD,
                                       ymax = Mean + SD, group = sample,
                                       color = sample)) +
      geom_pointrange(position = dodge, size = 0.1) +
      geom_point(aes(x = xval, y = standardizedSum, color = sample,
                               group = sample), alpha = 0.2,
                 position = dodge) 
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
    graphic <- ggplot(data = data, aes(x = xval, y = standardizedSum, 
                                       fill = sample, group = sample))
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
    stddata <- stddata %>% filter(category %in% data$category)
    if (what == "chains")
    {
      graphic <- graphic + facet_grid( ~ category, scales = "free_x")
    } else {    
      graphic <- graphic + facet_grid( ~ category, scales = "free_x")
    }
  } else if (within == "Functional Category") {
    stddata <- stddata %>% filter(func_cat %in% data$func_cat)
    graphic <- graphic + facet_grid( ~ func_cat, scales = "free_x")
  } else if (within == "Class") {
    stddata <- stddata %>% filter(class %in% data$class)
    graphic <- graphic + facet_grid( ~ class, scales = "free_x") 
  }
  
  # Plot Legend
  if (!("legend?" %in% checkGroup)) {
    graphic <- graphic + theme(legend.position = "none")
  } 
  # Plot Ttest
  if ("ttest" %in% checkGroup) {
    Ttestdata <- Ttest(data, within = within)
    graphic <- graphic + 
      geom_text(data = Ttestdata, aes(x = as.factor(xval), 
                                      y = yval + StarHeight,
                                      label = Significant),
                vjust = -0.5, fontface = "bold", size = 3) +
      geom_text(data = Ttestdata, aes(x = as.factor(xval), 
                                      y = yval + 0.97 * StarHeight,
                                      label = x_bar), 
                vjust = -0.5, fontface = "bold", size = 3)
  }
  # Plot values / maybe shaped
  if ("sample_ident" %in% checkGroup) {
    if("symbols" %in% advanced) {
      graphic <- graphic + 
        geom_point(data = data, aes(x = xval, y = standardizedSum,
                                    colour = sample, shape = sample_replicate),
                   position = dodge, alpha = 0.9) + 
        scale_shape_manual(values = c(0:25, 33:127), name = "")
    } else if ("letters" %in% advanced) {
      graphic <- graphic + 
        geom_text(data = data, aes(x = xval, y = standardizedSum,
                                   colour = sample, label = sample_replicate),
                  size = 2.5, position = dodge, alpha = 0.9, color="black")
    } 
  } else if ("values" %in% checkGroup) {
    if ("highlight" %in% advanced) {
      graphic <- graphic + 
        geom_point(data = data, aes(x = xval, y = standardizedSum, fill = sample),
                   position=dodge, colour="white", alpha=0.8, pch=21)      
    } else {
      graphic <- graphic +
        geom_point(data = data, 
                   aes(x = xval, y = standardizedSum, colour = sample),
                   position = dodge, alpha = 0.9)
    }


    
    
  }
  # Plot Median
  if ("median" %in% checkGroup) {
    graphic <- graphic +
      geom_errorbar(aes(ymin = Median, ymax = Median), position = dodge)
#       stat_summary(fun.ymin = median, fun.ymax = median, geom = "errorbar", 
#                    position = dodge)
  }  
  
  # Plot Errorbars
  if ("errsd" %in% checkGroup) {
    graphic <- graphic + 
      geom_errorbar(aes(color = sample, ymin = Mean - SD, ymax = Mean + SD),
                    position = dodge)
#       stat_summary(aes(color = sample), fun.ymax = meanplussd, fun.ymin = meanminussd, 
#                    geom = "errorbar", position = dodge)
  }
  if ("errse" %in% checkGroup) {
    graphic <- graphic + 
      geom_errorbar(aes(color = sample, ymin = Mean - SE, ymax= Mean + SE),
                    position = dodge)
#       stat_summary(aes(color = sample), fun.ymax = meanplusse, fun.ymin = meanminusse, 
#                    geom = "errorbar", position = dodge)
  }
  # Plot Nr of Datapoints
  stddata <- data[!duplicated(data[c("sample","xval")]),]
  if ("nrdat" %in% checkGroup) {
    graphic <- graphic + 
      geom_text(data = stddata, 
                  aes(x = xval, y = -0.7, label = NrSamp, group = sample,
                    fontsize = "bold"), size = 4, colour = "darkgrey",
                position = dodge, check_overlap = TRUE)
  }
  
  if ("mean" %in% checkGroup) {
    if ("errsd" %in% checkGroup) {
      graphic <- graphic + 
        geom_text(data = stddata, aes(color = sample, y = Mean + SD, 
                                                 label = round(Mean, 2)), 
                  position = dodge, vjust = -0.5, size = 3)
#         stat_summary(aes(color = sample), fun.data = mvalsd, geom = "text",
#                      position = dodge, vjust = -0.5, size = 3)
    } else if ("errse" %in% checkGroup) {
      graphic <- graphic + 
        geom_text(data = stddata, aes(color = sample, y = Mean + SE, 
                                                 label = round(Mean, 2)), 
                  position = dodge, vjust = -0.5, size = 3)
#         stat_summary(aes(color = sample), fun.data = mvalse, geom = "text",
#                      position = dodge, vjust = -0.5, size = 3)
    } else {
      graphic <- graphic + 
        geom_text(data = stddata, aes(color = sample, x = xval, y = Mean, 
                                                 label = round(Mean, 2)), 
                  position = dodge, vjust = -0.5, size = 3)
#         stat_summary(aes(color = sample), fun.data = mval, geom = "text",
#                      position = dodge, vjust = -0.5, size = 3)
    }
  }
  graphic <- graphic + labs(x = "", y = paste("mol % of measured lipid"))
  if (what %in% c("chains", "chain_sums")) {
    graphic <- graphic + theme(axis.text.x = element_text(angle = 45, size=08, hjust=1))
  }
  
  if ("logtrans" %in% checkGroup)
    graphic <- graphic + scale_y_log10()
  
  # if (what == "class") {
  #   graphic <- graphic + labs(x = "Classes", y = paste("mol % of", standard))
  #                             # title = paste("Classes", "per", standard))    
  # } else if (what == "category") {
  #   graphic <- graphic + labs(x = "Categories", y = paste("mol % of", standard))
  #                             # title = paste("Categories", "per", standard))    
  # } else if (what == "chains") {
  #   graphic <- graphic + labs(x = "Species", y = paste("mol % of", standard)) 
  #                             # title = paste("Species", "per", standard)) +
  #     # theme(axis.text.x = element_text(angle = 45, hjust = 1, size=08))
  #   
  # } else if (what == "chain_sums") {
  #   graphic <- graphic + labs(x = "Chain Sums", y = paste("mol % of", standard)) 
  #                             # title = paste("Chain Sums", "per", standard)) +
  #     # theme(axis.text.x = element_text(angle = 45, hjust = 1, size=08))
  #   
  # } else if (what == "length") {
  #   graphic <- graphic + labs(x = "Length", y = paste("mol % of", standard))
  #                             # title = paste("Length", "per", standard))
  # } else if (what == "db") {
  #   graphic <- graphic + labs(x = "Double Bonds", y = paste("mol % of", standard))
  #                             # title = paste("Double Bonds", "per", standard))
  # } else if (what == "oh") {
  #   graphic <- graphic + labs(x = "OH-Bounds", y = paste("mol % of", standard))
  #                             # title = paste("OH-Bounds", "per", standard))
  # }
  # graphic <- graphic + guides(fill = guide_legend(nrow = 15))
  print("Plot successful")
  
  # gg <- ggplotly(graphic)
  # return(gg)
  return(graphic)
}


preparePCAPlots <- function(data) {
  graphic = plot(data,type="l")
  # graphic <- graphic + guides(fill = guide_legend(nrow = 15))
  print("Plot successful")
  
  # gg <- ggplotly(graphic)
  # return(gg)
  return(graphic)
}

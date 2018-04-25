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
library(gplots)
library(ggplot2)
library(ggbiplot)
library(RColorBrewer)
## Bioconductor Packages
# source("http://bioconductor.org/biocLite.R")
# biocLite("pcaMethods")
library(Biobase)
library(BiocGenerics)
library(pcaMethods)

# Listing current packages:

# sess <- sessionInfo()
# other <- sess$otherPkgs
# loaded <- sess$loadedOnly
# otherpkgs <- purrr::map_df(other, ~ tibble(pkg = .x$Package, version = .x$Version)) %>% mutate(state = "other")
# loadedpkgs <- purrr::map_df(other, ~ tibble(pkg = .x$Package, version = .x$Version)) %>% mutate(state = "loaded only")
# rbind(otherpkgs, loadedpkgs) %>% clipr::write_clip()

# pkg	version	state
# dbplyr	1.2.1	other
# tidyr	0.8.0	other
# bindrcpp	0.2.2	other
# pcaMethods	1.70.0	other
# Biobase	2.38.0	other
# BiocGenerics	0.24.0	other
# RColorBrewer	1.1-2	other
# ggbiplot	0.55	other
# scales	0.5.0	other
# ggplot2	2.2.1	other
# gplots	3.0.1	other
# gridExtra	2.3	other
# shinydashboard	0.7.0	other
# shinythemes	1.1.1	other
# DT	0.4	other
# shinyjs	1.0	other
# shinyBS	0.61	other
# reshape2	1.4.3	other
# dplyr	0.7.4	other
# plyr	1.8.4	other
# lazyeval	0.2.1	other
# RSQLite	2.1.0	other
# jsonlite	1.5	other
# shiny	1.0.5	other
# dbplyr	1.2.1	loaded only
# tidyr	0.8.0	loaded only
# bindrcpp	0.2.2	loaded only
# pcaMethods	1.70.0	loaded only
# Biobase	2.38.0	loaded only
# BiocGenerics	0.24.0	loaded only
# RColorBrewer	1.1-2	loaded only
# ggbiplot	0.55	loaded only
# scales	0.5.0	loaded only
# ggplot2	2.2.1	loaded only
# gplots	3.0.1	loaded only
# gridExtra	2.3	loaded only
# shinydashboard	0.7.0	loaded only
# shinythemes	1.1.1	loaded only
# DT	0.4	loaded only
# shinyjs	1.0	loaded only
# shinyBS	0.61	loaded only
# reshape2	1.4.3	loaded only
# dplyr	0.7.4	loaded only
# plyr	1.8.4	loaded only
# lazyeval	0.2.1	loaded only
# RSQLite	2.1.0	loaded only
# jsonlite	1.5	loaded only
# shiny	1.0.5	loaded only


# Sourcing --------------------------------------------------------------------------------------------------------
## Code for loading the Data
source('code/database.R')
## Code for plotting the Data
source('code/plotter.R')
## Code for PCA
source('code/DeviumPCA.R')
source('code/DeviumCommon.R')



# ggplot options --------------------------------------------------------------------------------------------------
##### general options for the ggplot theme
theme_set(
    theme_bw(18)
    + theme(
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10),
        # axis.text.x= element_text(angle = 90,hjust = 1, vjust = 0.5),
        axis.title.y = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 10),
        panel.border = element_rect(colour = "black"),
        panel.background = element_blank()
    )
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
cleandata <- cleandata[order(cleandata$id), ]


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
check_ttest <- function(data, input) {
    if ("ttest" %in% input$checkGroup)
    {
        if (length(unique(data$sample)) == 2 |
            length(unique(input$samplesub)) == 2) {
            return(TRUE)
        }
        else {
            return(FALSE)
        }
    }
    return(TRUE)
}

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

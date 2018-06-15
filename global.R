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

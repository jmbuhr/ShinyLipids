# Preamble --------------------------------------------------------------------------------------------------------
# Packages

# install.packages(pkgs = c("tidyverse", "jsonlite", "RSQLite",
#                  "shiny", "shinyjs", "DT", "shinythemes", "shinydashboard",
#                  "RColorBrewer", "scales", "ggthemes", "ggsignif", "BiocManager"),
#                  quiet = TRUE)
#
#
# BiocManager::install(pkgs = c("Biobase", "BiocGenerics", "pcaMethods"))

# Database
#library(jsonlite)
library(RSQLite)
## Plotting
library(RColorBrewer)
library(scales)
library(ggthemes)
library(ggsignif)
## Bioconductor Packages, install via
# BiocManager::install("<package>")
library(Biobase)
library(BiocGenerics)
library(pcaMethods)
##
library(tidyverse)
library(ggrepel)
# contains: ggplot, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
## Shiny
library(shiny)
library(shinyjs) # for UI hiding
library(DT)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)


# (.packages())
# sessionInfo()
# R version 3.5.1 (2018-07-02)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows >= 8 x64 (build 9200)
#
# Matrix products: default
#
# locale:
#     [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252
# [3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C
# [5] LC_TIME=English_United States.1252
#
# attached base packages:
#     [1] parallel  stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:
#     [1] pcaMethods_1.72.0    Biobase_2.40.0       BiocGenerics_0.26.0  ggsignif_0.4.0       ggthemes_4.0.0
# [6] scales_0.5.0         RColorBrewer_1.1-2   shinydashboard_0.7.0 shinythemes_1.1.1    DT_0.4
# [11] shinyjs_1.0          shinyBS_0.61         RSQLite_2.1.1        jsonlite_1.5         forcats_0.3.0
# [16] stringr_1.3.1        dplyr_0.7.6          purrr_0.2.5          readr_1.1.1          tidyr_0.8.1
# [21] tibble_1.4.2         ggplot2_3.0.0        tidyverse_1.2.1      shiny_1.1.0
#
# loaded via a namespace (and not attached):
#     [1] Rcpp_0.12.18     lubridate_1.7.4  lattice_0.20-35  assertthat_0.2.0 digest_0.6.15    mime_0.5
# [7] R6_2.2.2         cellranger_1.1.0 plyr_1.8.4       backports_1.1.2  httr_1.3.1       pillar_1.3.0
# [13] rlang_0.2.1      lazyeval_0.2.1   readxl_1.1.0     rstudioapi_0.7   blob_1.1.1       htmlwidgets_1.2
# [19] bit_1.1-14       munsell_0.5.0    broom_0.5.0      compiler_3.5.1   httpuv_1.4.5     modelr_0.1.2
# [25] pkgconfig_2.0.1  htmltools_0.3.6  tidyselect_0.2.4 crayon_1.3.4     withr_2.1.2      later_0.7.3
# [31] grid_3.5.1       nlme_3.1-137     xtable_1.8-2     gtable_0.2.0     DBI_1.0.0        magrittr_1.5
# [37] cli_1.0.0        stringi_1.1.7    promises_1.0.1   bindrcpp_0.2.2   xml2_1.2.0       tools_3.5.1
# [43] bit64_0.9-7      glue_1.3.0       crosstalk_1.0.0  hms_0.4.2        yaml_2.2.0       colorspace_1.3-2
# [49] rvest_0.3.2      memoise_1.1.0    bindr_0.1.1      haven_1.1.2


# Debugging -------------------------------------------------------------------------------------------------------

options(shiny.fullstacktrace = FALSE,
        shiny.error = "default")


# * Database connection ---------------------------------------------------------------------------------------------

# connection
database_connection = src_sqlite("database/Sqlite.db")

# SQL queries
sqlQueryMeta <- paste("SELECT * FROM id_info")
sqlQueryData <- function(dataset_ID){
    query <- paste("SELECT * FROM data2", "WHERE ID =", dataset_ID)
    return(query)
}


# ggplot options --------------------------------------------------------------------------------------------------

# Features that can serve as aesthetics (visual mappings, short aes) in plots
features <- list("", #1
                 "sample", #2
                 "lipid", #3
                 "value", #4
                 "category", #5
                 "functional category" = "func_cat", #6
                 "class",#7
                 "length",#8
                 "double bound" ="db",#9
                 "hydroxylation" = "oh",#10
                 "chains",#11
                 "chain_sums",#12
                 "sample_replicate",#13
                 "sample_replicate_technical")#14

# Global theme definition to add to ggplots
mainTheme <- list(
    theme_minimal(),
    theme(
        axis.line = element_line(colour = 'grey70', size = .75),
        text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_blank(),
        legend.position = "bottom",
        panel.background = element_rect(color = "grey70", fill = NA, size = 1),
        strip.background = element_rect(fill = "grey80", color = "black"),
        strip.text = element_text(color = "black")
    )
)

# Returns a function that takes an interger and creates a color palette
getPalette <- colorRampPalette(brewer.pal(9, "Set1")[-6])

# Color scale
mainScale <- function(colorCount){
    list(
        scale_fill_manual(values = getPalette(colorCount)),
        scale_color_manual(values = getPalette(colorCount)),
        labs(title = "Alpha Version")
    )
}


# helper functions -------------------------------------------------------------------------------------------------------

# borrowed from plyr (https://github.com/hadley/plyr/):
is.discrete <- function(x) is.factor(x) || is.character(x) || is.logical(x)

# Preamble --------------------------------------------------------------------------------------------------------
# Packages

# install.packages(pkgs = c("tidyverse", "jsonlite", "RSQLite",
#                  "shiny", "shinyjs", "DT", "shinydashboard",
#                  "RColorBrewer", "scales", "ggthemes", "BiocManager"),
#                  quiet = TRUE)
#
# BiocManager::install(pkgs = c("Biobase", "BiocGenerics", "pcaMethods"))
# Packages that are not attached here by library() are explicitely called via <packagename>::<function>(),
# you still need to install them.

# (.packages())
# sessionInfo()
# NCmisc:::list.functions.in.file("./R/global.R")
# writeLines(capture.output(sessionInfo()), "./doc/sessionInfo.txt")

# Attaching packages ----------------------------------------------------------------------------------------------
library(tidyverse) # contains: ggplot, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
library(shiny)

# Debugging -------------------------------------------------------------------------------------------------------

options(shiny.fullstacktrace = FALSE,
        shiny.error = "default")


# Database Connection ---------------------------------------------------------------------------------------------

## uncomment this to read from a local file in the folder of this shiny app (outcomment the other line!)
database_connection = src_sqlite("./database/Sqlite.db")

## uncomment this to read from a serverside database (and fill in credentials as necessary)
# database_connection = src_postgres(dbname = "ldb",
#                                    host = "129.206.154.238",
#                                    port = 5432,
#                                    user = "mathias")

## uncomment this to read from a database on the same server this App is running (and fill in credentials as necessary)
# database_connection = src_postgres(dbname = "ldb",
#                                    host = "localhost",
#                                    user = "mathias")

# SQL queries -----------------------------------------------------------------------------------------------------
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
getPalette <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1")[-6])

# Color scale
mainScale <- function(colorCount){
  list(
    scale_fill_manual(values = getPalette(colorCount)),
    scale_color_manual(values = getPalette(colorCount))
  )
}


# helper functions -------------------------------------------------------------------------------------------------------

# borrowed from plyr (https://github.com/hadley/plyr/):
is.discrete <- function(x) is.factor(x) || is.character(x) || is.logical(x)

# Convex hull for PCA plots
# borrowed from https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html
StatChull <- ggproto("StatChull", Stat,
                     compute_group = function(data, scales) {
                       data[chull(data$x, data$y), , drop = FALSE]
                     },

                     required_aes = c("x", "y")
)

stat_chull <- function(mapping = NULL, data = NULL, geom = "polygon",
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatChull, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

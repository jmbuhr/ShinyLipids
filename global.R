# Preamble --------------------------------------------------------------------------------------------------------
# Packages
## From global.R
# install.packages(c("BiocManager", "ggplot2", "RColorBrewer", "shiny", "tidyr", "dbplyr"))

## From server.R
# install.packages(c("DT", "ggrepel", "RSQLite", "umap", "readr"))

## From ui.RRSQLite
# install.packages(c("DT", "shinycssloaders", "shinydashboard", "shinyjs"))

## deprecated or installed as dependency automatically:
# "shinyjs", "ggthemes", "jsonlite"

## For deployment from RStudio
# install.packages(c("bitops", "RCurl", "openssl", "rstudioapi"))

# BiocManager::install() # installs core packages
# BiocManager::install(pkgs = c("pcaMethods"))

# Packages that are not attached here by library() are explicitely called via <packagename>::<function>(),
# you still need to install them.

# Run this before deployment
options(repos = c(BiocManager::repositories()))

## If deployment fails, run this:
# BiocManager::install("rsconnect", update = TRUE, ask = FALSE)

## Debug
options(shiny.fullstacktrace = FALSE, shiny.error = browser)

# Attaching packages ----------------------------------------------------------------------------------------------
# library(tidyverse, quietly = TRUE) # contains: ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
library(shiny, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(tidyr, quietly = TRUE)
library(purrr, quietly = TRUE)
library(RSQLite, quietly = TRUE)


# Database Connection ---------------------------------------------------------------------------------------------
## uncomment this to read from a local file in the folder of this shiny app (outcomment the other line!)
database_connection = DBI::dbConnect(RSQLite::SQLite(), "database/Sqlite.db")

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
sqlQueryData <- function(dataset_ID) {
  query <- paste("SELECT * FROM data2", "WHERE ID =", dataset_ID)
  return(query)
}


# ggplot options --------------------------------------------------------------------------------------------------

# Features that can serve as aesthetics (visual mappings, short aes) in plots
features <- c(
  "",
  "value",
  "Sample" = "sample",
  "Sample replicate" = "sample_replicate",
  "Sample replicate technical" = "sample_replicate_technical",
  "Class" = "class",
  "Lipid species" = "lipid",
  "Category" = "category",
  "Functional category" = "func_cat",
  "Double bonds" = "db",
  "Hydroxylation state" = "oh",
  "Chain Length" = "length",
  "Chains" = "chains",
  "Chain sums" = "chain_sums"
)


# Lipid class order -----------------------------------------------------------------------------------------------
if ("LIPID_CLASS_ORDER_COMPLETE" %in% DBI::dbListTables(database_connection)) {
  class_levels <-
    collect(tbl(database_connection, "LIPID_CLASS_ORDER_COMPLETE")) %>%
    arrange(class_order) %>%
    pull(class)
} else {
  class_levels <- c("PC", "PC O-", "LPC", "PE", "PE O-", "PE P-", "LPE",
                    "PS", "PS O-", "PI", "PI O-", "PG", "PG O-", "LPG", "PA",
                    "PA O-", "LPA", "CL", "MLCL", "Cer", "SM", "HexCer", "SGalCer",
                    "GM3", "Sulf", "diHexCer", "Hex2Cer", "For", "IPC", "MIPC",
                    "M(IP)2C", "Chol", "Desm", "Erg", "CE", "EE", "DAG", "TAG",
                    "PIP", "PIP2", "PIP3", "GM1Cer", "GD1Cer", "MAG", "Epi", "PGP", "WE", "FA")
}

# Global theme definition to add to ggplots
mainTheme <- list(
  theme_minimal(),
  theme(
    axis.line = element_line(colour = 'grey70', size = .75),
    text = element_text(
      color = "black",
      face = "bold",
      family = "sans"
    ),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.background = element_blank(),
    legend.position = "bottom",
    panel.background = element_rect(color = "grey70", fill = NA, size = 1),
    strip.background = element_rect(fill = "grey80", color = "black"),
    strip.text = element_text(color = "black")
  )
)

# Returns a function that takes an interger and creates a color palette
getPalette <-
  colorRampPalette(RColorBrewer::brewer.pal(n = 9, name = "Set1")) #[-6]

# Color scale
mainScale <- function(colorCount) {
  list(scale_fill_manual(values = getPalette(colorCount)),
       scale_color_manual(values = getPalette(colorCount)))
}


# helper functions -------------------------------------------------------------------------------------------------------

# borrowed from plyr (https://github.com/hadley/plyr/):
is.discrete <-
  function(x) {
    is.factor(x) || is.character(x) || is.logical(x)
  }

# Convex hull for PCA plots
# borrowed from https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html
StatChull <- ggproto(
  "StatChull",
  Stat,
  compute_group = function(data, scales) {
    data[chull(data$x, data$y), , drop = FALSE]
  },

  required_aes = c("x", "y")
)

stat_chull <-
  function(mapping = NULL,
           data = NULL,
           geom = "polygon",
           position = "identity",
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE,
           ...) {
    layer(
      stat = StatChull,
      data = data,
      mapping = mapping,
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  }


# Debugging -------------------------------------------------------------------------------------------------------

options(shiny.fullstacktrace = FALSE,
        shiny.error = "default")

# (.packages())
# sessionInfo()
# NCmisc:::list.functions.in.file("./R/global.R")
# writeLines(capture.output(sessionInfo()), "./docs/sessionInfo.txt")
# write.table(rsconnect::appDependencies(), "./docs/appDependencies.txt")

# Debugging options -------------------------------------------------------
# options(shiny.error = recover)

# Attaching packages ----------------------------------------------------------------------------------------------
# library(dbplyr) # only needed for deployment
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(RSQLite)

# Sourcing Functions
source("./R/functions.R")

# Database Connection ---------------------------------------------------------------------------------------------
## uncomment this to read from a local file in the folder of this shiny app (outcomment the other line!)
database_connection <- "database/Sqlite_dev.db"
# database_connection <- "R/exampleDatabase/exampleDatabase2.db"

## uncomment this to read from a serverside database (and fill in credentials as necessary)
# database_connection <- src_postgres(dbname = "ldb",
#                                    host    = "",
#                                    port    = 5432,
#                                    user    = "")

## uncomment this to read from a database on the same server this App is running (and fill in credentials as necessary)
# database_connection <- src_postgres(dbname = "ldb",
#                                    host    = "localhost",
#                                    user    = "")

# ggplot options --------------------------------------------------------------------------------------------------

# Features that can serve as aesthetics (visual mappings, short aes) in plots
features <- c(
  "",
  "value",
  "Sample"                     = "sample",
  "Sample replicate"           = "sample_replicate",
  "Sample replicate technical" = "sample_replicate_technical",
  "Class"                      = "class",
  "Lipid species"              = "lipid",
  "Category"                   = "category",
  "Functional category"        = "func_cat",
  "Double bonds"               = "db",
  "Hydroxylation state"        = "oh",
  "Chain Length"               = "length",
  "Chains"                     = "chains",
  "Chain sums"                 = "chain_sums"
)


# Lipid class order -----------------------------------------------------------------------------------------------
class_levels <- get_lipid_class_order(database_connection)

# Global theme definition to add to ggplots
mainTheme <- list(
  theme_minimal(),
  theme(
    axis.line        = element_line(colour = "grey70", size = .75),
    text             = element_text(
      color          = "black",
      face           = "bold",
      family         = "sans"
    ),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    plot.background  = element_blank(),
    legend.position  = "bottom",
    panel.background = element_rect(color = "grey70", fill = NA, size = 1),
    strip.background = element_rect(fill = "grey80", color = "black"),
    strip.text       = element_text(color = "black")
  )
)


# Preamble --------------------------------------------------------------------------------------------------------
# Packages
## tidyverse
library(tidyverse)
# contains: ggplot, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
library(jsonlite)
library(RSQLite)
## Shiny
library(shiny)
library(shinyBS) # for tooltips
library(shinyjs) # for UI hiding
library(DT) # needs to be loaded after shiny
library(shinythemes)
library(shinydashboard)
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

# sessionInfo()

# Preamble --------------------------------------------------------------------------------------------------------
install.packages(c("BiocManager", "tidyverse", "RColorBrewer", "shiny",
                    "dbplyr", "DT", "ggrepel", "RSQLite",
                    "DT", "shinycssloaders", "shinydashboard", "shinyjs", "shinyjqui"))

BiocManager::install(pkgs = c("pcaMethods"))
# For deployment from RStudio
install.packages(c("bitops", "RCurl", "openssl", "rstudioapi"))

# Run this before deployment
options(repos = c(BiocManager::repositories()))

## If deployment fails, run this:
# BiocManager::install("rsconnect", update = TRUE, ask = FALSE)

#' @import shiny
#' @import dplyr
#' @import forcats
#' @import ggplot2
#' @import tidyr
#' @import purrr
#' @import RSQLite
#' @importFrom stats qt
#' @importFrom rlang .data
#' @importFrom utils write.csv
#' @importFrom grDevices chull
#' @importFrom graphics title
#' @importFrom stats p.adjust pairwise.t.test sd
#' @importFrom utils data
#' 
NULL

# quiets concerns of R CMD CHECK re: the .'s that appear in pipelines
# and also names of tibble columns used in tidy evaluation / NSE.
utils::globalVariables(
  c(".",
    "CI_lower", "CI_upper", "N", "PC1", "PC2", "SD", "SEM", "category",
    "class_order", "datasets", "date_extraction", "date_measured",
    "date_sample", "date_upload", "db", "func_cat", "lipid", "oh",
    "p.value", "pairwise", "sample_identifier", "sample_replicate",
    "sample_replicate_technical", "value", "chains", "individualChains")
)


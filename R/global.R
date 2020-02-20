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

dtOptions <- list(
  orderClasses   = TRUE,
  pageLength     = 10,
  order          = list(0, "desc"),
  scrollX        = TRUE,
  deferRender    = TRUE,
  scrollCollapse = TRUE
)

# Functions

# Libraries loaded in global.R
# library(shiny)
# library(dplyr, quietly = TRUE)
# library(ggplot2)
# library(tidyr)
# library(purrr)
# library(RSQLite)


# helper functions -------------------------------------------------------------------------------------------------------

# borrowed from plyr (https://github.com/hadley/plyr/):
is.discrete <-
    function(x) {
        is.factor(x) || is.character(x) || is.logical(x)
    }

# Suppress warning that not all factor levels for lipid class order are used:
quiet_fct_relevel <- purrr::quietly(forcats::fct_relevel)

safe_qt <- possibly(qt, otherwise = NA_real_)

# Convex hull for PCA plots
# borrowed from https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html
StatChull <- ggproto(
    "StatChull",
    Stat,
    compute_group = function(data, scales) {
        data[chull(data$x, data$y),, drop = FALSE]
    },
    required_aes = c("x", "y")
)

stat_chull <- function(mapping       = NULL,
                       data        = NULL,
                       geom        = "polygon",
                       position    = "identity",
                       na.rm       = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE,
                       ...) {
    layer(
        stat        = StatChull,
        data        = data,
        mapping     = mapping,
        geom        = geom,
        position    = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params      = list(na.rm = na.rm, ...)
    )
}


# Data Cleaning Functions -------------------------------------------------

collect_meta_data <- function(con, query) {
    meta <- collect(tbl(con, sql(query))) %>%
        mutate(
            date_upload     = as.Date(date_upload, format = "%y%m%d"),
            date_sample     = as.Date(date_sample, format = "%y%m%d"),
            date_extraction = as.Date(date_extraction, format = "%y%m%d"),
            date_measured   = as.Date(date_measured, format = "%y%m%d")
        ) %>%
        arrange(id)
    return(meta)
}


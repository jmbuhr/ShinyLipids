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

# Lipid class order -----------------------------------------------------------------------------------------------
get_lipid_class_order <- function(con) {
    con <- DBI::dbConnect(RSQLite::SQLite(), con)
    if ("LIPID_CLASS_ORDER_COMPLETE" %in% DBI::dbListTables(con)) {
        res <- collect(tbl(con, "LIPID_CLASS_ORDER_COMPLETE")) %>%
            arrange(class_order) %>%
            pull(class)
    } else {
        res <- c(
            "PC", "PC O-", "LPC", "PE", "PE O-", "PE P-", "LPE",
            "PS", "PS O-", "PI", "PI O-", "PG", "PG O-", "LPG", "PA",
            "PA O-", "LPA", "CL", "MLCL", "Cer", "SM", "HexCer", "SGalCer",
            "GM3", "Sulf", "diHexCer", "Hex2Cer", "For", "IPC", "MIPC",
            "M(IP)2C", "Chol", "Desm", "Erg", "CE", "EE", "DAG", "TAG",
            "PIP", "PIP2", "PIP3", "GM1Cer", "GD1Cer", "MAG", "Epi", "PGP", "WE", "FA"
        )
    }
    dbDisconnect(con)
    return(res)
}

# Data Cleaning Functions -------------------------------------------------

collect_meta_data <- function(con, query) {
    con <- DBI::dbConnect(RSQLite::SQLite(), con)
    meta <- collect(tbl(con, sql(query))) %>%
        mutate(
            date_upload     = as.Date(date_upload, format = "%y%m%d"),
            date_sample     = as.Date(date_sample, format = "%y%m%d"),
            date_extraction = as.Date(date_extraction, format = "%y%m%d"),
            date_measured   = as.Date(date_measured, format = "%y%m%d")
        ) %>%
        arrange(id)
    dbDisconnect(con)
    return(meta)
}


collect_raw_data <- function(con, query, custom_class_order = class_levels) {
    con <- DBI::dbConnect(RSQLite::SQLite(), con)
    df <- collect(tbl(con, sql(query))) %>%
        filter(!is.na(value)) %>%
        mutate(
            sample_identifier          = factor(sample_identifier),
            lipid                      = factor(lipid),
            func_cat                   = factor(func_cat),
            class                      = quiet_fct_relevel(class, custom_class_order)$result,
            category                   = factor(category),
            sample                     = factor(sample),
            sample_replicate           = factor(sample_replicate),
            sample_replicate_technical = factor(sample_replicate_technical),
            oh                         = if_else(is.na(oh), 0, oh)
        ) %>%
        select(id, sample_identifier, lipid, value, everything())
    dbDisconnect(con)
    return(df)
}

standardize_rawData <- function(df, input) {
    # Standardization based on input$std_feature
    if (input$std_feature != "") {
        df <- df %>%
            group_by(id, !!sym(input$std_feature)) %>%
            mutate(value = value / sum(value) * 100) %>%
            ungroup()
    }

    # Base level substraction
    if (input$base_sample != "") {
        baseline <- df %>%
            filter(sample == input$base_sample) %>%
            group_by(lipid) %>%
            summarize(baseline = mean(value, na.rm = TRUE))
        df <- df %>%
            left_join(baseline) %>%
            mutate(baseline = if_else(is.na(baseline), 0, baseline)) %>%
            mutate(value = value - baseline) %>%
            ungroup()
    }
    return(df)
}


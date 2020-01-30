#' @importFrom stats qt
#' @import dplyr
#' @import forcats
#' @import ggplot2
#' @import tidyr
#' @import purrr
#' @import RSQLite
#' @importFrom rlang .data
#' @importFrom grDevices chull
#' @importFrom graphics title
#' @importFrom stats p.adjust pairwise.t.test sd
#' @importFrom utils data
#' 
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(
    c(".",
      "CI_lower", "CI_upper", "N", "PC1", "PC2", "SD", "SEM", "category", "class_order",
      "datasets", "date_extraction", "date_measured", "date_sample", "date_upload", "db", 
      "func_cat", "lipid", "oh", "p.value", "pairwise", "sample_identifier", "sample_replicate",
      "sample_replicate_technical", "value")
)

#' Generate SQL Query for the selected dataset
#'
#' @param dataset_ID numeric
#'
#' @return
#' SQL Query as a string
#' @export
createQueryForID <- function(dataset_ID) {
    query <- paste("SELECT * FROM data2", "WHERE id =", dataset_ID)
    return(query)
}

# helper functions --------------------------------------------------------

#' Test if x is discrete
#'
#' @param x vector
#'
#' @return boolean
#' @note 
#' borrowed from plyr (https://github.com/hadley/plyr/)
is.discrete <- function(x) {
    is.factor(x) || is.character(x) || is.logical(x)
}

# Suppress warning that not all factor levels for lipid class order are used:
quiet_fct_relevel <- purrr::quietly(forcats::fct_relevel)

safe_qt <- possibly(stats::qt, otherwise = NA_real_)

# Returns a function that takes an interger and creates a color palette
getPalette <- colorRampPalette(RColorBrewer::brewer.pal(n = 9, name = "Set1"))

#' Create color scale
#'
#' @param colorCount integer
#'
#' @return A list with scale_color_ and scale_fill_
mainScale <- function(colorCount) {
    list(
        scale_fill_manual(values  = getPalette(colorCount)),
        scale_color_manual(values = getPalette(colorCount))
    )
}

testAllMoreThanOneReplicate <- function(df, aesX, aesColor) {
    df %>%
        group_by(!!sym(aesX), !!sym(aesColor)) %>%
        count() %>%
        pull(n) %>%
        {all(. > 1)}
}

#' Convex hull for PCA plots
#'
#' Borrowed from https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html
#'
#' @param mapping aesthetic mapping
#' @param data data
#' @param geom geometric element
#' @param position default = "identity"
#' @param na.rm remove NAs
#' @param show.legend show legend
#' @param inherit.aes inherit aesthetics
#' @param ... passed to layer
#'
#' @return a state for ggplot
stat_chull <- function(mapping     = NULL,
                       data        = NULL,
                       geom        = "polygon",
                       position    = "identity",
                       na.rm       = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE,
                       ...) {
    StatChull <- ggproto(
        "StatChull",
        Stat,
        compute_group = function(data, scales) {
            data[chull(data$x, data$y),, drop = FALSE]
        },
        required_aes = c("x", "y")
    )
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

# Significance Tests ------------------------------------------------------

#' Pairwise t-tests
#' 
#' Because people like p-values and little stars.
#'
#' @param df a subset of the data
#'
#' @return
#' A tidy representation of the significance
#' test build with \code{broom::tidy}
test_pairwise <- function(df) {
    response <- df$value
    group    <- df$sample
    pairwise.t.test(
        x = log(response), g = group,
        paired = FALSE, alternative = "two.sided"
    ) %>%
        broom::tidy()
}

#' Test all possible pairwise t-tests
#' 
#' Because people like p-values and little stars.
#'
#' @param df the full dataset
#' @param x_axis the feature mapped to the x-axis
#'
#' @return
#' A tidy representation of the significance
#' tests build with \code{broom::tidy} combinde
#' into one tibble.
doAllPairwiseComparisons <- function(df, x_axis) {
    comparisons <- df %>%
        group_by(!!sym(x_axis)) %>%
        nest() %>%
        mutate(
            pairwise = map(data, possibly(test_pairwise, tibble()))
        ) %>%
        unnest(pairwise)
    
    comparisons %>%
        mutate(p.value = p.adjust(p.value, "BH")) %>%
        select(-data)
}

#' Lipid Class Order
#' 
#' Get or create the order in which the different lipid
#' classes are displayed on their axis.
#'
#' @param con A database connection
#' If the databse does not have the table
#' LIPID_CLASS_ORDER_COMPLETE, the order
#' is generated here instead.
#'
#' @return A vector of lipid classes
#' @export
collectLipidClassOrder <- function(con) {
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
    return(res)
}

# Data Cleaning Functions -------------------------------------------------
#' Make a date
#'
#' @param col A character vector 
#' (Column in a dataframe)
#' @return A Date vector
#' @export
make_date <- function(col) {
    as.Date(col, format = "%y%m%d")
}

#' Collect metadata from database
#'
#' @param con database conncetion object.
#' Create one yourself with e.g.
#' \code{DBI::dbConnect(RPostgres::Postgres(), ...)} or
#' \code{DBI::dbConnect(RSQLite::SQLite(), "<data/exampleDatabase.db>")}
#' 
#' @return a tibble with the meta data
#' @export
collectMetaData <- function(con) {
    meta <- collect(tbl(con, sql("SELECT * FROM id_info"))) %>%
        mutate_at(vars(date_upload, date_sample, date_extraction, date_measured),
                  possibly(make_date, NA_real_)
        ) %>%
        arrange(id)
    return(meta)
}

#' Collect raw data from database
#'
#' @param con A database connection
#' @param query The sql query to get the dataset.
#' Create it yourself with \code{\link{createQueryForID}}.
#' @param lipidClassOrder The order for the lipid classes
#'
#' @return Raw data as a tibble
#' @export
collectRawData <- function(con, query, lipidClassOrder) {
    collect(tbl(con, sql(query))) %>%
        filter(!is.na(value)) %>%
        mutate(
            sample_identifier          = factor(sample_identifier),
            lipid                      = factor(lipid),
            func_cat                   = factor(func_cat),
            class                      = quiet_fct_relevel(class, lipidClassOrder)$result,
            category                   = factor(category),
            sample                     = factor(sample),
            sample_replicate           = factor(sample_replicate),
            sample_replicate_technical = factor(sample_replicate_technical),
            oh                         = as.integer(oh),
            oh                         = if_else(is.na(oh), 0L, oh)
        ) %>%
        select(id, sample_identifier, lipid, value, everything())
}

standardizeWithinTechnicalReplicatesIf <- function(df, do_it) {
  if (do_it) {
    df <- df %>%
      group_by(id, sample_replicate_technical) %>%
      mutate(value = value / sum(value) * 100) %>%
      ungroup()
  }
  df
}

#' Filter raw data
#'
#' @param df Data as a tibble 
#' @param input a list.
#' This list can contain
#' strings with the following names,
#' non existent ones will be ignored:
#' \itemize{
#' \item \code{categoryToSelect}
#' \item \code{lipidClassToSelect}
#' \item \code{functionalCategoryToSelect}
#' \item \code{filter_length}
#' \item \code{filter_db}
#' \item \code{filter_oh}
#' \item \code{samplesToSelect}
#' \item \code{samplesToRemove}
#' \item \code{technicalReplicatesToRemove}
#' }
#'
#' @return Lovely filtered data
#' @export
filterRawDataFor <- function(df, input) {
    # Category
    if (!is.null(input$categoryToSelect)) {
        df <- df %>% filter(category %in% input$categoryToSelect)
    }
    # Class
    if (!is.null(input$lipidClassToSelect)) {
        df <- df %>% filter(class %in% input$lipidClassToSelect)
    }
    # Functional category
    if (!is.null(input$functionalCategoryToSelect)) {
        df <- df %>% filter(func_cat %in% input$functionalCategoryToSelect)
    }
    # Total length of sidechains
    if (!is.null(input$filter_length)) {
        df <-
            df %>%
            filter(length %>% between(input$filter_length[1], input$filter_length[2]))
    }
    # Total number of double bounds
    if (!is.null(input$filter_db)) {
        df <-
            df %>%
            filter(db %>% between(input$filter_db[1], input$filter_db[2]))
    }
    # Total number of hydroxyl groups
    if (!is.null(input$filter_oh)) {
        df <-
            df %>%
            filter(oh %>% between(input$filter_oh[1], input$filter_oh[2]))
    }
    # explicitly demanding sample
    if (!is.null(input$samplesToSelect)) {
        df <- df %>% filter(sample %in% input$samplesToSelect)
    }
    # removing sample
    if (!is.null(input$samplesToRemove)) {
        df <- df %>% filter(!(sample %in% input$samplesToRemove))
    }
    # demanding replicate
    if (!is.null(input$replicatesToSelect)) {
        df <- df %>% filter(sample_replicate %in% input$replicatesToSelect)
    }
    # removing replicate
    if (!is.null(input$replicatesToRemove)) {
        df <- df %>% filter(!(sample_replicate %in% input$replicatesToRemove))
    }
    # removing technical replicate
    if (!is.null(input$technicalReplicatesToRemove)) {
        df <-
            df %>%
            filter(!(
                sample_replicate_technical %in% input$technicalReplicatesToRemove
            ))
    }
    return(df)
}

#' Standardize raw data
#'
#' @param df Raw data as a tibble
#' @param baselineSample Sample to use as a baseline
#' @param standardizationFeatures Features to standardize on
#'
#' @return Standardized data as a tibble
#' @export
standardizeRawDataWithin <- function(df, baselineSample, standardizationFeatures) {
  # Standardization
  if (!is.null(standardizationFeatures)) {
    df <- df %>%
      group_by(id, !!!syms(standardizationFeatures)  ) %>%
      mutate(value = value / sum(value) * 100) %>%
      ungroup()
  }

  # Base level substraction
  if (baselineSample != "") {
    baseline <- df %>%
      filter(sample == baselineSample) %>%
      group_by(lipid) %>%
      summarize(baseline = mean(value, na.rm = TRUE))
    df <- df %>%
      left_join(baseline) %>%
      mutate(baseline = if_else(is.na(baseline), 0, baseline)) %>%
      mutate(value = value - baseline) %>%
      ungroup()
  }
  df
}

#' Create the data ready for plotting
#' 
#' Based on the aesthetic mappings.
#'
#' @param df Input tibble
#' @param input a list with the aesthetic mappings.
#' This list can contain
#' strings with the following names,
#' non existent ones will be ignored:
#' \itemize{
#' \item \code{summariseTechnicalReplicates}
#' \item \code{aesX}
#' \item \code{aesColor}
#' \item \code{aesFacetCol}
#' \item \code{aesFacetRow}
#' }
#' 
#' @return neat data
#' @export
createPlotData <- function(df, input) {
    # Averaging over the technical replicates
    if (input$summariseTechnicalReplicates) {
        df <- df %>%
            group_by_at(vars(
                -sample_identifier,
                -sample_replicate_technical,
                -value
            )) %>%
            summarize(value = mean(value, na.rm = TRUE)) %>%
            ungroup()
    }
    # Filter any NA in features used for aesthetics (x-axis, y-axis, color, facet1, facet2)
    df <- df %>% filter(
        !is.na(!!sym(input$aesX)),
        !is.na(value)
    )
    if (input$aesColor != "") {
        df <- df %>% filter(!is.na(!!sym(input$aesColor)))
    }
    if (input$aesFacetCol != "") {
        df <- df %>% filter(!is.na(!!sym(input$aesFacetCol)))
    }
    if (input$aesFacetRow != "") {
        df <- df %>% filter(!is.na(!!sym(input$aesFacetRow)))
    }
    
    # Summation of values within the displayed aesthetics
    df <- df %>% ungroup()
    if (input$aesX != "") {
        df <- df %>% group_by(!!sym(input$aesX), add = TRUE)
    }
    if (input$aesColor != "") {
        df <- df %>% group_by(!!sym(input$aesColor), add = TRUE)
    }
    if (input$aesFacetCol != "") {
        df <- df %>% group_by(!!sym(input$aesFacetCol), add = TRUE)
    }
    if (input$aesFacetRow != "") {
        df <- df %>% group_by(!!sym(input$aesFacetRow), add = TRUE)
    }
    if (input$summariseTechnicalReplicates) {
        df <- df %>% group_by(sample_replicate, add = TRUE)
    } else {
        df <- df %>% group_by(sample_replicate_technical, add = TRUE)
    }
    
    df <- df %>% summarize(value = sum(value, na.rm = TRUE))
    # This will remove only the last layer of grouping (sample_replicate or sample_replicate_technical)
    # and keep the other groups, in either case, ase_x will still be a group
    # this group will then be summarized in meanPlotData
    return(df)
}

#' Summarise the plot data
#' 
#' @param df input tibble
#'
#' @return data ready for summmary plots like barplots
#' @export
summarisePlotData <- function(df) {
    df <- df %>% summarize(
        SD       = sd(value, na.rm = TRUE),
        SEM      = sd(value, na.rm = TRUE) / n(),
        N        = n(),
        value    = mean(value, na.rm = TRUE),
        CI_lower = value - safe_qt(1 - (0.05 / 2), N - 1) * SEM,
        CI_upper = value + safe_qt(1 - (0.05 / 2), N - 1) * SEM
    ) %>%
        # assumption: we are 100% sure that no lipid has a value smaller than 0
        mutate(
            CI_lower = if_else(CI_lower < 0, 0, CI_lower)
        )
    return(df)
}


#' Collect metadata from database
#'
#' @param con database conncetion object.
#' Create one yourself with e.g.
#' \code{DBI::dbConnect(RPostgres::Postgres(), ...)} or
#' \code{DBI::dbConnect(RSQLite::SQLite(), "<data/exampleDatabase.db>")}
#' @return tibble. Meta data
#' @export
collectMetaData <- function(con) {
  collect(tbl(con, sql("SELECT * FROM id_info"))) %>%
    mutate_at(vars(date_upload, date_sample, date_extraction, date_measured),
              possibly(parseDate, NA_real_)
    ) %>%
    arrange(id)
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

#' Generate SQL Query for the selected dataset
#'
#' @param datasetID numeric. ID of the dataset
#'
#' @return string. SQL Query
#' @export
createQueryForID <- function(datasetID) {
  paste("SELECT * FROM data2", "WHERE id =", datasetID)
}

#' Collect raw data from database
#'
#' @param con A database connection
#' @param id numeric. ID of dataset to collect
#'
#' @return tibble. Raw data
#' @export
collectRawData <- function(id = 1, con) {
  con %>% 
    tbl(sql(createQueryForID(id))) %>% 
    collect() %>%
    filter(!is.na(value),
           !is.na(lipid)) %>% 
    mutate_at(vars(sample_identifier, sample_replicate,
                   sample_replicate_technical,
                   category, func_cat),
              replace_na, "other")
}

#' Add Lipid properties
#' 
#' Based on lipid column
#'
#' @param data tibble. rawData
#' @param lipidClassOrder character vector, order of lipid classes
#'
#' @return tibble. enhanced data
#' @export
addLipidProperties <-
  function(data,
           lipidClassOrder = c(
             "PC", "PC O-", "LPC", "PE", "PE O-", "PE P-", "LPE",
             "PS", "PS O-", "PI", "PI O-", "PG", "PG O-", "LPG",
             "PA", "PA O-", "LPA", "CL", "MLCL", "Cer", "SM",
             "HexCer", "SGalCer", "GM3", "Sulf", "diHexCer",
             "Hex2Cer", "For", "IPC", "MIPC", "M(IP)2C", "Chol",
             "Desm", "Erg", "CE", "EE", "DAG", "TAG", "PIP",
             "PIP2", "PIP3", "GM1Cer", "GD1Cer", "MAG", "Epi",
             "PGP", "WE", "FA")) {
  data %>% 
    separate(col = lipid,
             into = c("class", "chains"),
             sep = " ",
             fill = "right",
             remove = FALSE
    ) %>%
    mutate(
      class = stringr::str_trim(paste0(class,
                                       " ",
                                       stringr::str_replace_na(stringr::str_extract(chains, ".-"), ""))),
      class = quietly(forcats::fct_relevel)(class, lipidClassOrder)$result,
      chains           = replace_na(chains, "0:0"),
      individualChains = stringr::str_split(chains, "/"),
      length           = stringr::str_extract_all(chains, "\\d+(?=:)") %>% 
        map_int(~ sum(as.integer(.x))),
      db               = stringr::str_extract_all(chains, "(?<=:)\\d+") %>% 
        map_int(~ sum(as.integer(.x))),
      oh               = stringr::str_extract_all(chains, "(?<=;)\\d+") %>% 
        map_int(~ sum(as.integer(.x)))
    ) %>% 
    mutate(
      chain_sums = paste0(length, ":", db,
                          if_else(oh > 0, paste0(";", oh), ""))
    ) %>% 
    mutate_if(is.character, factor) %>% 
    select(id, sample_identifier, lipid, value, everything()) %>% 
    select(-individualChains)
}

#' Impute implicit missing values as 0
#' 
#' Imputes if \code{input$imputeMissingAs0 == TRUE},
#' else returns input data.
#'
#' @param data tibble. rawData
#' @param input list. Input list from shiny ui. Uses
#' - imputeMissingAs0 :: logical
#'
#' @return tibble. imputed data
#' @export
imputeMissingIf <- function(data, input) {
  if (input$imputeMissingAs0) {
    data %>% 
      complete(id,
               nesting(lipid, category, func_cat),
               nesting(sample, sample_replicate, sample_replicate_technical, sample_identifier),
               fill = list(value = 0))
  } else {
    data
  }
}

#' Standardize data within technical replicates
#' 
#' 
#'
#' @param data tibble. data
#' @param input list. Input list from shiny ui. Uses
#' - standardizeWithinTechnicalReplicate :: logical
#'
#' @return tibble. standardized data
#' @export
standardizeWithinTechnicalReplicatesIf <- function(data, input) {
  if (input$standardizeWithinTechnicalReplicate) {
    group_by(data, id, sample_replicate_technical) %>%
      mutate(value = value / sum(value) * 100) %>%
      ungroup()
  } else {
    data
  }
}

#' Filter if not null
#' 
#' Filter data based on condition
#' only if an input var is not NULL
#'
#' @param data tibble. data
#' @param var character vector | NULL
#' @param condition an expression to evaluate in
#' the context of the data
#'
#' @return tibble. filtered data
filterIfNotNull <- function(data, var, condition) {
  if (!is.null(var)) {
    filter(data, {{condition}})
  } else {
    data
  }
}

#' Filter raw data
#' 
#' @param rawData :: tibble. rawData
#' @param input :: list. Input list from shiny ui. Uses
#' - standardizeWithinTechnicalReplicate
#' - categoryToSelect :: string | NULL
#' - lipidClassToSelect :: string | NULL
#' - functionalCategoryToSelect :: string | NULL
#' - filterLengthRange :: string | NULL
#' - filterDoubleBondsRange :: string | NULL
#' - filterOhRange :: string | NULL
#' - samplesToSelect :: string | NULL
#' - samplesToRemove :: string | NULL
#' - replicatesToSelect :: string | NULL
#' - replicatesToRemove :: string | NULL
#' - technicalReplicatesToRemove :: string | NULL
#'
#' @return tibble. Lovely filtered data
#' @export
filterRawDataFor <- function(rawData, input) {
  rawData %>% 
    filterIfNotNull(input$categoryToSelect, category %in% input$categoryToSelect) %>% 
    filterIfNotNull(input$lipidClassToSelect, class %in% input$lipidClassToSelect) %>% 
    filterIfNotNull(input$functionalCategoryToSelect, func_cat %in% input$functionalCategoryToSelect) %>% 
    filterIfNotNull(input$filterLengthRange, between(length, input$filterLengthRange[1], input$filterLengthRange[2])) %>%
    filterIfNotNull(input$filterDoubleBondsRange, between(db, input$filterDoubleBondsRange[1], input$filterDoubleBondsRange[2])) %>%
    filterIfNotNull(input$filterOhRange, between(oh, input$filterOhRange[1], input$filterOhRange[2])) %>%
    filterIfNotNull(input$samplesToSelect, sample %in% input$samplesToSelect) %>% 
    filterIfNotNull(input$samplesToRemove, !(sample %in% input$samplesToRemove)) %>% 
    filterIfNotNull(input$replicatesToSelect, sample_replicate %in% input$replicatesToSelect) %>% 
    filterIfNotNull(input$replicatesToRemove, !(sample_replicate %in% input$replicatesToRemove)) %>% 
    filterIfNotNull(input$technicalReplicatesToRemove, !(sample_replicate_technical %in% input$technicalReplicatesToRemove))
}


#' Standardize raw data
#'
#' @param data tibble. Raw data
#' @param input list. Input list from shiny ui. Uses
#' - baselineSample
#' - standardizationFeatures :: tibble. rawData
#'
#' @return tibble. Standardized data
#' @export
standardizeRawDataWithin <- function(data, input) {
  # Standardization
  if (!is.null(input$standardizationFeatures)) {
    data <- data %>%
      group_by(id, !!!syms(input$standardizationFeatures)) %>%
      mutate(value = value / sum(value) * 100) %>%
      ungroup()
  }
  
  # Base level subtraction
  if (input$baselineSample != "") {
    baseline <- data %>%
      filter(sample == input$baselineSample) %>%
      group_by(lipid) %>%
      summarize(baseline = mean(value, na.rm = TRUE))
    data <- data %>%
      left_join(baseline) %>%
      mutate(baseline = if_else(is.na(baseline), 0, baseline)) %>%
      mutate(value = value - baseline) %>%
      ungroup()
  }
  data
}

#' Create the data ready for plotting
#' 
#' Based on the aesthetic mappings.
#'
#' @param data :: tibble
#' @param input :: list. Input list from shiny ui. Uses
#' - summariseTechnicalReplicates :: boolean
#' - aesX :: string
#' - aesColor :: string
#' - aesFacetCol :: string | ""
#' - aesFacetRow :: string | ""
#' 
#' @return :: tibble. neat data
#' @export
createPlotData <- function(data, input) {
  # Averaging over the technical replicates
  if (input$summariseTechnicalReplicates) {
    data <- data %>%
      group_by_at(vars(-sample_identifier,
                       -sample_replicate_technical,
                       -value)) %>%
      summarize(value = mean(value, na.rm = TRUE))
  }
  
  cols <- c("value", input$aesX, input$aesColor, input$aesFacetCol, input$aesFacetRow,
            if_else(input$summariseTechnicalReplicates,
                    "sample_replicate", "sample_replicate_technical"))
  cols <- cols[cols != ""]
  
  data %>% 
    group_by(!!!syms(cols[cols != "value"])) %>% 
    summarise(value = sum(value, na.rm = TRUE)) %>% 
    ungroup()
}

#' Summarise the plot data
#' 
#' @param data tibble. plotData
#' @param input :: list. Input list from shiny ui. Uses
#' - aesX :: string
#' - aesColor :: string
#' - aesFacetCol :: string | ""
#' - aesFacetRow :: string | ""
#'
#' @return tibble. data ready for summmary plots like barplots
#' @export
summarisePlotData <- function(data, input) {
  possiblyQt <- possibly(stats::qt, otherwise = NA_real_)
  
  cols <- c(input$aesX, input$aesColor, input$aesFacetCol, input$aesFacetRow)
  cols <- cols[cols != ""]
  data %>%
    group_by(!!!syms(cols)) %>% 
    summarise(
      SD       = sd(value, na.rm = TRUE),
      SEM      = sd(value, na.rm = TRUE) / n(),
      N        = n(),
      value    = mean(value, na.rm = TRUE),
      CI_lower = value - possiblyQt(1 - (0.05 / 2), N - 1) * SEM,
      CI_upper = value + possiblyQt(1 - (0.05 / 2), N - 1) * SEM) %>%
    mutate(
      CI_lower = if_else(CI_lower < 0, 0, CI_lower)
    ) %>% 
    ungroup()
}

# Significance Tests ####

#' Pairwise t-tests
#' 
#' Because people like p-values and little stars.
#'
#' @param data tibble. a subset of the data
#'
#' @return
#' A tidy representation of the significance
#' test build with \code{broom::tidy}
testPairwise <- function(data) {
  response <- data$value
  group    <- data$sample
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
#' @param data tibble. The full dataset
#' @param input :: list. Input list from shiny ui. Uses
#' - aesX :: string
#'
#' @return :: tibble.
#' A tidy representation of the significance
#' tests build with \code{broom::tidy} combined
#' into one tibble.
doAllPairwiseComparisons <- function(data, input) {
  data %>%
    group_by(!!sym(input$aesX)) %>%
    nest() %>%
    mutate(pairwise = map(data, possibly(testPairwise, tibble()))) %>%
    unnest(pairwise) %>%
    mutate(p.value = p.adjust(p.value, "BH")) %>%
    select(-data)
}

#' Collect metadata from database
#'
#' @param con database conncetion object.
#' Create one yourself with e.g.
#' \code{DBI::dbConnect(RPostgres::Postgres(), ...)} or
#' \code{DBI::dbConnect(RSQLite::SQLite(), "<data/exampleDatabase.db>")}
#' @return tibble. Meta data
#' @export
collectMetaData <- function(con) {
  meta <- collect(tbl(con, sql("SELECT * FROM id_info"))) %>%
    mutate_at(vars(date_upload, date_sample, date_extraction, date_measured),
              possibly(parseDate, NA_real_)
    ) %>%
    arrange(id)
  return(meta)
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
  collect(tbl(con, sql(createQueryForID(id)))) %>%
    filter(!is.na(value),
           !is.na(lipid)) %>% 
    mutate_at(vars(sample_identifier, sample_replicate, sample_replicate_technical,
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
addLipidProperties <- function(data,
                               lipidClassOrder = c(
                                 "PC", "PC O-", "LPC", "PE", "PE O-", "PE P-", "LPE",
                                 "PS", "PS O-", "PI", "PI O-", "PG", "PG O-", "LPG",
                                 "PA", "PA O-", "LPA", "CL", "MLCL", "Cer", "SM",
                                 "HexCer", "SGalCer", "GM3", "Sulf", "diHexCer",
                                 "Hex2Cer", "For", "IPC", "MIPC", "M(IP)2C", "Chol",
                                 "Desm", "Erg", "CE", "EE", "DAG", "TAG", "PIP",
                                 "PIP2", "PIP3", "GM1Cer", "GD1Cer", "MAG", "Epi",
                                 "PGP", "WE", "FA")) {
  # legacy version that relies on columns in the data
  # data %>% 
  #   mutate(
  #     sample_identifier          = factor(sample_identifier),
  #     lipid                      = factor(lipid),
  #     func_cat                   = factor(func_cat),
  #     class                      = quiet_fct_relevel(class, lipidClassOrder)$result,
  #     category                   = factor(category),
  #     sample                     = factor(sample),
  #     sample_replicate           = factor(sample_replicate),
  #     sample_replicate_technical = factor(sample_replicate_technical),
  #     oh                         = as.integer(oh),
  #     oh                         = if_else(is.na(oh), 0L, oh)
  #   )
  data %>% 
    separate(col = lipid,
             into = c("class", "chains"),
             sep = " ",
             fill = "right",
             remove = FALSE
    ) %>% 
    mutate(
      class = quiet_fct_relevel(class, lipidClassOrder)$result,
      chains           = replace_na(chains, "0:0"),
      individualChains = str_split(chains, "/"),
      length           = str_extract_all(chains, "\\d+(?=:)") %>% 
        map_int(~ sum(as.integer(.x))),
      db               = str_extract_all(chains, "(?<=:)\\d+") %>% 
        map_int(~ sum(as.integer(.x))),
      oh               = str_extract_all(chains, "(?<=;)\\d+") %>% 
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


#' Standardize data within technical replicates
#' 
#' 
#'
#' @param data tibble. data
#' @param doIt boolean, if TRUE runs standardization,
#' else returns the data unchanged
#'
#' @return tibble. standardized data
#' @export
standardizeWithinTechnicalReplicatesIf <- function(data, doIt = TRUE) {
  if (doIt) {
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
#' @export
filterIfNotNull <- function(data, var, condition) {
  if (!is.null(var)) {
    filter(data, {{condition}})
  } else {
    data
  }
}

#' Filter raw data
#'
#' @param rawData tibble. rawData
#' @param categoryToSelect string | NULL
#' @param lipidClassToSelect string | NULL
#' @param functionalCategoryToSelect string | NULL
#' @param filterLengthRange string | NULL
#' @param filterDoubleBondsRange string | NULL
#' @param filterOhRange string | NULL
#' @param samplesToSelect string | NULL
#' @param samplesToRemove string | NULL
#' @param replicatesToSelect string | NULL
#' @param replicatesToRemove string | NULL
#' @param technicalReplicatesToRemove string | NULL
#'
#' @return tibble. Lovely filtered data
#' @export
filterRawDataFor <- function(rawData,
                             categoryToSelect            = NULL,
                             lipidClassToSelect          = NULL,
                             functionalCategoryToSelect  = NULL,
                             filterLengthRange           = NULL,
                             filterDoubleBondsRange      = NULL,
                             filterOhRange               = NULL,
                             samplesToSelect             = NULL,
                             samplesToRemove             = NULL,
                             replicatesToSelect          = NULL,
                             replicatesToRemove          = NULL,
                             technicalReplicatesToRemove = NULL) {
  rawData %>% 
    filterIfNotNull(categoryToSelect, category %in% categoryToSelect) %>% 
    filterIfNotNull(lipidClassToSelect, class %in% lipidClassToSelect) %>% 
    filterIfNotNull(functionalCategoryToSelect, func_cat %in% functionalCategoryToSelect) %>% 
    filterIfNotNull(filterLengthRange, between(length, filterLengthRange[1], filterLengthRange[2])) %>%
    filterIfNotNull(filterDoubleBondsRange, between(db, filterDoubleBondsRange[1], filterDoubleBondsRange[2])) %>%
    filterIfNotNull(filterOhRange, between(oh, filterOhRange[1], filterOhRange[2])) %>%
    filterIfNotNull(samplesToSelect, sample %in% samplesToSelect) %>% 
    filterIfNotNull(samplesToRemove, !(sample %in% samplesToRemove)) %>% 
    filterIfNotNull(replicatesToSelect, sample_replicate %in% replicatesToSelect) %>% 
    filterIfNotNull(replicatesToRemove, !(sample_replicate %in% replicatesToRemove)) %>% 
    filterIfNotNull(technicalReplicatesToRemove, !(sample_replicate_technical %in% technicalReplicatesToRemove))
}


#' Standardize raw data
#'
#' @param data tibble. Raw data
#' @param baselineSample string | "", Sample to use as a baseline
#' @param standardizationFeatures character vector, Features to standardize on
#'
#' @return tibble. Standardized data
#' @export
standardizeRawDataWithin <- function(data,
                                     baselineSample          = "",
                                     standardizationFeatures = c("")) {
  # Standardization
  if (!is.null(standardizationFeatures)) {
    data <- data %>%
      group_by(id, !!!syms(standardizationFeatures)) %>%
      mutate(value = value / sum(value) * 100) %>%
      ungroup()
  }
  
  # Base level substraction
  if (baselineSample != "") {
    baseline <- data %>%
      filter(sample == baselineSample) %>%
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
#' @param data Input tibble
#' @param summariseTechnicalReplicates boolean
#' @param aesX string
#' @param aesColor string
#' @param aesFacetCol string | ""
#' @param aesFacetRow string | ""
#' 
#' @return tibble. neat data
#' @export
createPlotData <- function(data,
                           summariseTechnicalReplicates = TRUE,
                           aesX = "class",
                           aesColor = "sample",
                           aesFacetCol = "",
                           aesFacetRow = "") {
  # Averaging over the technical replicates
  if (summariseTechnicalReplicates) {
    data <- data %>%
      group_by_at(vars(-sample_identifier,
                       -sample_replicate_technical,
                       -value)) %>%
      summarize(value = mean(value, na.rm = TRUE))
  }
  
  cols <- c("value", aesX, aesColor, aesFacetCol, aesFacetRow,
            if_else(summariseTechnicalReplicates,
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
#' @param aesX string
#' @param aesColor string
#' @param aesFacetCol string | ""
#' @param aesFacetRow string | ""
#'
#' @return tibble. data ready for summmary plots like barplots
#' @export
summarisePlotData <- function(data,
                              aesX = "class",
                              aesColor = "sample",
                              aesFacetCol = "",
                              aesFacetRow = "") {
  cols <- c(aesX, aesColor, aesFacetCol, aesFacetRow)
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
    )
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
#' @param aesX string. the feature mapped to the x-axis
#'
#' @return tibble.
#' A tidy representation of the significance
#' tests build with \code{broom::tidy} combinde
#' into one tibble.
doAllPairwiseComparisons <- function(data, aesX) {
  data %>%
    group_by(!!sym(aesX)) %>%
    nest() %>%
    mutate(pairwise = map(data, possibly(testPairwise, tibble()))) %>%
    unnest(pairwise) %>%
    mutate(p.value = p.adjust(p.value, "BH")) %>%
    select(-data)
}

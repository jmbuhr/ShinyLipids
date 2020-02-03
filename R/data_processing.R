#' Collect metadata from database
#'
#' @param con database conncetion object.
#' Create one yourself with e.g.
#' \code{DBI::dbConnect(RPostgres::Postgres(), ...)} or
#' \code{DBI::dbConnect(RSQLite::SQLite(), "<data/exampleDatabase.db>")}
#' @return a tibble with the meta data
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
#' @param datasetID numeric
#'
#' @return
#' SQL Query as a string
#' @export
createQueryForID <- function(datasetID) {
  query <- paste("SELECT * FROM data2", "WHERE id =", datasetID)
  return(query)
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

standardizeWithinTechnicalReplicatesIf <- function(data, doIt) {
  if (doIt) {
    group_by(data, id, sample_replicate_technical) %>%
      mutate(value = value / sum(value) * 100) %>%
      ungroup()
  } else {
    data
  }
}

filterIfNotNull <- function(data, var, expression) {
  if (!is.null(var)) {
    filter(data, {{ expression }})
  } else {
    data
  }
}


#' Filter raw data
#'
#' @param rawData Data as a tibble 
#'
#' @return Lovely filtered data
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
    filterIfNotNull(categoryToSelect,
                    category %in% categoryToSelect) %>% 
    filterIfNotNull(lipidClassToSelect,
                    class %in% lipidClassToSelect) %>% 
    filterIfNotNull(functionalCategoryToSelect,
                    func_cat %in% functionalCategoryToSelect) %>% 
    filterIfNotNull(filterLengthRange,
                    between(length, filterLengthRange[1], filterLengthRange[2])) %>% 
    filterIfNotNull(filterDoubleBondsRange,
                    between(db, filterDoubleBondsRange[1], filterDoubleBondsRange[2])) %>% 
    filterIfNotNull(filterOhRange,
                    between(oh, filterOhRange[1], filterOhRange[2])) %>% 
    filterIfNotNull(samplesToSelect,
                    sample %in% samplesToSelect) %>% 
    filterIfNotNull(samplesToRemove,
                    !(sample %in% samplesToRemove)) %>% 
    filterIfNotNull(replicatesToSelect,
                    sample_replicate %in% replicatesToSelect) %>% 
    filterIfNotNull(replicatesToRemove,
                    !(sample_replicate %in% replicatesToRemove)) %>% 
    filterIfNotNull(technicalReplicatesToRemove,
                    !(sample_replicate_technical %in% technicalReplicatesToRemove))
}


#' Standardize raw data
#'
#' @param data Raw data as a tibble
#' @param baselineSample Sample to use as a baseline
#' @param standardizationFeatures Features to standardize on
#'
#' @return Standardized data as a tibble
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
#' 
#' @return neat data
#' @export
createPlotData <- function(data,
                           summariseTechnicalReplicates = TRUE,
                           aesX = "class",
                           aesColor = "sample",
                           aesFacetCol = NULL,
                           aesFacetRow = NULL) {
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
  
  # This will remove only the last layer of grouping
  # (sample_replicate or sample_replicate_technical)
  # and keep the other groups, in either case, aseX will still be a group
  # this group will then be summarized as meanPlotData in summarisePlotData
  data %>% 
    filter_at(cols, negate(is.na)) %>% 
    group_by(!!!syms(cols[cols != "value"])) %>% 
    summarise(value = sum(value, na.rm = TRUE))
}

#' Summarise the plot data
#' 
#' @param data input tibble
#'
#' @return data ready for summmary plots like barplots
#' @export
summarisePlotData <- function(data) {
  data %>%
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
#' @param data a subset of the data
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
#' @param data the full dataset
#' @param aesX the feature mapped to the x-axis
#'
#' @return
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

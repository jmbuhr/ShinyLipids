utils::globalVariables(c("set", "name"))

#' Read SDF file from Lipid Maps
#'
#' @param path string. File path to SDF file
#'
#' @return tibble. Tidy tibble of LM data
readLipidMapsSDF <- function(path) {
  if (!requireNamespace("ChemmineR", quietly = TRUE)) {
    stop("Package \"ChemmineR\" needed for this function to work. Please install it.",
         call. = FALSE)
  } else {
    ChemmineR::read.SDFset(path) %>% 
      tibble::enframe(set@SDF) %>% 
      mutate(body = map(value, "datablock")) %>% 
      unnest_wider(body) %>% 
      select(-value) %>% 
      rename(ID = name)
  }
}

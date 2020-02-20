# utils::globalVariables(c("set", "name"))

## Not currently used
# readLipidMapsSDF <- function(path) {
#   if (!requireNamespace("ChemmineR", quietly = TRUE)) {
#     stop("Package \"ChemmineR\" needed for this function to work. Please install it.",
#          call. = FALSE)
#   } else {
#     ChemmineR::read.SDFset(path) %>% 
#       tibble::enframe(set@SDF) %>% 
#       mutate(body = map(value, "datablock")) %>% 
#       unnest_wider(body) %>% 
#       select(-value) %>% 
#       rename(ID = name)
#   }
# }

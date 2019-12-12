context("Backend Functions")

# pkgload::load_all()

# test_that(
#   "Standardization for Species Profile of PC works as intended",
# )
# 
# database_connection <- DBI::dbConnect(RSQLite::SQLite(), "database/test.db")
# 
# input <- list(
#   ID = 168,
#   custom_class_order = get_lipid_class_order(database_connection),
#   std_tec_rep = TRUE,
#   std_feature = c("class", "sample_replicate"),
#   tecRep_average = TRUE,
#   base_sample = "",
#   filter_class = "PC",
#   aes_x = "lipid",
#   aes_facet1 = "class",
#   aes_facet2 = "",
#   aes_color = "sample"
# )
# 
# query <- sqlQueryData(input$ID)
# rawData <- collect_raw_data(database_connection,
#                  query,
#                  custom_class_order = input$custom_class_order)
# 
# plotData <- rawData %>%
#   standardize_technical_replicates(input$std_tec_rep) %>% 
#   filter_rawData(input) %>% 
#   standardize_rawData(base_sample = input$base_sample,
#                       std_features = input$std_feature) %>% 
#   create_plotData(input)







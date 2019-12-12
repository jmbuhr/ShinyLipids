context("Backend Functions")

# pkgload::load_all()
pkgload::load_code()

test_that(
  "Standardization for Species Profile of PC works as intended",
  
  
  
  
  
)

db_con <- DBI::dbConnect(RSQLite::SQLite(), "database/test.db")
database_connection <- db_con

input <- list(
  ID = 168,
  custom_class_order = get_lipid_class_order(database_connection),
  std_tec_rep = TRUE,
  base_sample = "",
  filter_class = "PC",
  aes_facet1 = "class",
  aes_x = "lipid",
  std_feature = c("class", "sample_replicate")
)

query <- sqlQueryData(input$ID)
rawData <- collect_raw_data(database_connection,
                 query,
                 custom_class_order = input$custom_class_order)

rawData %>%
  standardize_technical_replicates(input$std_tec_rep) %>% 
  filter_rawData(input) %>% 
  standardize_rawData(base_sample = input$base_sample,
                      std_features = input$std_feature)





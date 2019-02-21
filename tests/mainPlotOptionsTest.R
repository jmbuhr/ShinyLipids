app <- ShinyDriver$new("../")
app$snapshotInit("mainPlotOptionsTest")

# Input 'mainDataTable_rows_current' was set, but doesn't have an input binding.
# Input 'mainDataTable_rows_all' was set, but doesn't have an input binding.
app$setInputs(ID = "148")
# Input 'metaDataTable_rows_selected' was set, but doesn't have an input binding.
# Input 'metaDataTable_row_last_clicked' was set, but doesn't have an input binding.
# Input 'metaDataTable_cell_clicked' was set, but doesn't have an input binding.
# Input 'mainDataTable_rows_current' was set, but doesn't have an input binding.
# Input 'mainDataTable_rows_all' was set, but doesn't have an input binding.
app$setInputs(tab = "main")
# Input 'pairwiseComparisonsTable_rows_current' was set, but doesn't have an input binding.
# Input 'pairwiseComparisonsTable_rows_all' was set, but doesn't have an input binding.
app$setInputs(main_add = c("points", "bars", "mean"))
app$setInputs(main_add = c("points", "bars", "mean", "values"))
app$setInputs(main_add = c("points", "bars", "mean", "values", "ind_values"))
app$setInputs(main_add = c("points", "bars", "mean", "values", "ind_values", "log"))
app$setInputs(main_add = c("points", "bars", "mean", "values", "ind_values", "log", "N"))
app$setInputs(main_add = c("points", "bars", "mean", "values", "ind_values", "log", "N", "label"))
app$setInputs(main_add = c("points", "bars", "mean", "values", "ind_values", "N", "label"))
app$setInputs(main_add = c("points", "bars", "mean", "values", "ind_values", "N", "label", "swap"))
app$setInputs(main_add = c("points", "bars", "mean", "values", "ind_values", "N", "label", "swap", "free_y"))
app$setInputs(main_add = c("points", "bars", "mean", "values", "ind_values", "N", "label", "swap", "free_y", "signif"))
app$setInputs(main_error = "CI")
app$setInputs(aes_facet1 = "category")
# Input 'pairwiseComparisonsTable_rows_current' was set, but doesn't have an input binding.
# Input 'pairwiseComparisonsTable_rows_all' was set, but doesn't have an input binding.
app$snapshot()
app$snapshot()

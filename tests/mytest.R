app <- ShinyDriver$new("../", seed = 42)
app$snapshotInit("mytest")

# Input 'mainDataTable_rows_current' was set, but doesn't have an input binding.
# Input 'mainDataTable_rows_all' was set, but doesn't have an input binding.
app$setInputs(tab = "main")
# Input 'pairwiseComparisonsTable_rows_current' was set, but doesn't have an input binding.
# Input 'pairwiseComparisonsTable_rows_all' was set, but doesn't have an input binding.
app$setInputs(tab = "heatmap")
app$setInputs(tab = "PCA")
app$setInputs(tab = "main")
app$snapshot()
app$setInputs(ID = "2")
app$setInputs(tab = "heatmap")
app$setInputs(tab = "PCA")
app$setInputs(tab = "heatmap")
app$snapshot()
app$setInputs(tab = "main")
app$setInputs(tab = "metadata")
# Input 'mainDataTable_rows_current' was set, but doesn't have an input binding.
# Input 'mainDataTable_rows_all' was set, but doesn't have an input binding.
app$setInputs(tab = "main")
app$setInputs(main_add = c("points", "bars", "label"))
app$setInputs(tab = "metadata")
# Input 'mainDataTable_rows_all' was set, but doesn't have an input binding.
# Input 'mainDataTable_rows_current' was set, but doesn't have an input binding.
# Input 'mainDataTable_rows_all' was set, but doesn't have an input binding.
app$snapshot()

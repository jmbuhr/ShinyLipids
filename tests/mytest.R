app <- ShinyDriver$new("../", loadTimeout = 1e+05)
app$snapshotInit("mytest")

# Input 'mainDataTable_rows_current' was set, but doesn't have an input binding.
# Input 'mainDataTable_rows_all' was set, but doesn't have an input binding.
app$setInputs(tab = "main")
# Input 'pairwiseComparisonsTable_rows_current' was set, but doesn't have an input binding.
# Input 'pairwiseComparisonsTable_rows_all' was set, but doesn't have an input binding.
app$setInputs(tab = "heatmap")
app$setInputs(tab = "PCA")
app$snapshot()

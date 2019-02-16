app <- ShinyDriver$new("../")
app$snapshotInit("mytest")

# Input 'mainDataTable_rows_current' was set, but doesn't have an input binding.
# Input 'mainDataTable_rows_all' was set, but doesn't have an input binding.
app$setInputs(tab = "main")
# Input 'meanPlotDataTable_rows_current' was set, but doesn't have an input binding.
# Input 'meanPlotDataTable_rows_all' was set, but doesn't have an input binding.
# Input 'pairwiseComparisonsTable_rows_current' was set, but doesn't have an input binding.
# Input 'pairwiseComparisonsTable_rows_all' was set, but doesn't have an input binding.
# Input 'mainPlot_brush' was set, but doesn't have an input binding.
# Input 'mainPlot_dblclick' was set, but doesn't have an input binding.
# Input 'mainPlot_dblclick' was set, but doesn't have an input binding.
app$setInputs(main_add = c("points", "bars", "mean"))
app$setInputs(main_add = c("points", "bars", "mean", "values"))
app$setInputs(main_add = c("points", "bars", "mean", "values", "ind_values"))
app$setInputs(main_add = c("points", "bars", "mean", "values", "ind_values", "log"))
app$setInputs(main_add = c("points", "bars", "mean", "values", "ind_values", "log", "free_y"))
app$setInputs(aes_facet1 = "func_cat")
# Input 'pairwiseComparisonsTable_rows_current' was set, but doesn't have an input binding.
# Input 'pairwiseComparisonsTable_rows_all' was set, but doesn't have an input binding.
app$setInputs(main_add = c("points", "bars", "mean", "values", "ind_values", "log", "free_y", "signif"))
app$setInputs(ID = "149")
app$setInputs(main_add = c("points", "bars", "mean", "values", "ind_values", "log", "signif"))
# Input 'mainPlot_dblclick' was set, but doesn't have an input binding.
# Input 'mainPlot_brush' was set, but doesn't have an input binding.
# Input 'mainPlot_dblclick' was set, but doesn't have an input binding.
# Input 'mainPlot_dblclick' was set, but doesn't have an input binding.
# Input 'mainPlot_brush' was set, but doesn't have an input binding.
# Input 'mainPlot_dblclick' was set, but doesn't have an input binding.
app$setInputs(main_add = c("points", "bars", "mean", "values", "ind_values", "log", "free_y", "signif"))
app$setInputs(main_add = c("points", "bars", "mean", "values", "ind_values", "log", "signif"))
# Input 'mainPlot_dblclick' was set, but doesn't have an input binding.
# Input 'mainPlot_brush' was set, but doesn't have an input binding.
# Input 'mainPlot_dblclick' was set, but doesn't have an input binding.
# Input 'mainPlot_dblclick' was set, but doesn't have an input binding.
# Input 'mainPlot_brush' was set, but doesn't have an input binding.
# Input 'mainPlot_dblclick' was set, but doesn't have an input binding.
# Input 'mainPlot_dblclick' was set, but doesn't have an input binding.
app$snapshot()

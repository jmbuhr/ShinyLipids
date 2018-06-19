# Structure

## Data flow

### Meta Data
_contains information about the dataset e.g. who measured what when_  
raw_meta() -> meta

#### Main Data
_the µM values for each lipid from the sql database_  
raw_df() -> main_df -> sub_df -> sum_df():  
(database -> filtering -> normalizing


filtering, creates **sub_df**:  
options: sample, sample_replicate, category, func_category, class, length, sum_length, db, oh

## IO
### Input



### Output

#### Download Buttons
- whole dataset
- Subset of data
- Summary

(each as .Rds, .csv, .xls)
with checkbox for long/wide-format

#### Plots
- main_plt (bar, point, range etc.), uses sum_df()
- PCA, value always µM, 
    - PC1,2 + sample
    - PC1,2 + Lipid
- heatmap_plt, uses sum_df()



Display options, creates sum_df and plot:
x-axis, color, facet 1, facet 2


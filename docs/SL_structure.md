# Structure
- [Structure](#structure)
    - [User Interface](#user-interface)
    - [The internals](#the-internals)
        - [global.R](#globalr)
        - [ui.R](#uir)
        - [server.R](#serverr)
            - [Reactive dependencies](#reactive-dependencies)


This document provides a quick outline of the internal datastructure of the new Shiny Lipids as well as some thoughts behind the UI-Changes.

## User Interface

The user interface is based on the popular R-package `shinydashboard`. It features:

- a header (only includes title for now, might host e-mail and help buttons in the future)
- a sidebar
    - Contains most of the control elements that affect all outputs
    - Divided into tabs: "Datasets" opens the datasets tab in the body, "Visualization" hosts the tabs for "main Plot, PCA and Heatmap"
- a body
    - contains the main elements. Plots are always at the top of each panel with additional options below.

## The internals

Shiny Lipid _alpha_ makes full use of `shiny`'s reactive functions. Those keep track of their status and only update if they find themselves outdated.
The overwriting of global variables from inside observer-functions as it was the case in previous versions was abandoned for the sake of a cleaner data-structure (and more elegant code).

### global.R

Only a handful of global variables and functions are used:

- `database_connection` points to the local `src_sqlite("database/Sqlite.db")` database but could be changed to point to a database on the server
- `features` is a list of features present in the data and is used to populate the choices for selectInputs in _ui.R_
- SQL-Queries to retrieve the list of datasets and one specific dataset from the database
- Definitions for a `mainTheme` and `mainScale(<number of colors>)` to add to ggplots
- the helper function `is.discrete()` borrowed from the `plyr`-package

### ui.R

Mostly explained in [User Interface](#User-Interface). For more documentation see comments in the code.

### server.R

This is where the magic happens.

#### Reactive dependencies

A flowchart is created with [mermaid.js](https://mermaidjs.github.io/) from the following code and can be
previewed in VSCode with [this extension](https://github.com/mjbvz/vscode-markdown-mermaid). The final version will contain a rendered image. 

[comment]: <> (TODO)

``` mermaid
graph TB
subgraph Datasets
M[metaData] -->|output| MT(metaDataTable)
MT-->|update selectInput|ID(ID)
MT -->saveMeta(( saveMeta))
end
subgraph mainData
ID-->|select ID|RD(rawData)
RD-->SR((saveRawCSV))
RD -->|stanndardization and filtering|MD(mainData)
MD-->saveMainCSV((saveMainCSV))
FI{filter_/sample_}--> |filtering| MD
MD-->|output|mDT((mainDataTable))
end
subgraph main Plot
MD-->|summarize|PD(plotData)
AES{input$aes_}-->|define|PD
PD-->|average replicates|mPD(meanPlotData)
AES{input$aes_}-->|define|mP
PD-->|ggplot|mP(mainPlt)
mPD-->|ggplot|mP(mainPlt)
mP-->|outout|mPL((mainPlot))
mPL-->|click/brush|ranges
ranges-->|update|mP
end
subgraph PCA
PD-->|spread to matrix|pcaDATA
AES-->|define|pcaDATA
pcaDATA -->|calculate|pcaObject
pcaObject -->|output summary|PCAI((pca_info))
pcaObject -->|ggplot|pca_scores
pcaObject -->|ggplot|pca_loadings
pca_scores -->|render plot|PCAS((pca_scores))
pca_loadings -->|render plot|PCAL((pca_loadings))
end
subgraph heatmap
AES-->|define|HP(heatPlt)
PD-->|ggplot|HP
HP-->|output|HPO((heatPlot))
end
```
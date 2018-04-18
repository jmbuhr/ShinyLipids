# Packages
library(ggplot2)
library(jsonlite)
library(lazyeval)
library(dbplyr)  # newly inserted
library(plyr)
library(dplyr) # needs to be loaded after plyr
library(RColorBrewer)
library(shinyBS)
library(shinyjs)
library(shiny)
library(DT) # needs to be loaded after shiny
library(shinythemes)
library(tidyr)
library(RSQLite)
library(gplots)
library(reshape2)
library(gridExtra)
library(shinydashboard)
library(ggbiplot)
# library(XLConnect) # Do we need this?

# source("http://bioconductor.org/biocLite.R")
# biocLite("BiocGenerics")

library(Biobase)
library(BiocGenerics)
library(pcaMethods)



##### Code for loading the Data
source('code/database.R')
##### Code for plotting the Data
source('code/plotter.R')
##### Code for PCA
source('code/DeviumPCA.R')
source('code/DeviumCommon.R')


##### general options for the ggplot theme
theme_set(
  theme_bw(18)
  + theme(
    axis.line = element_line(colour = "black"),
    axis.text = element_text(size = 10),
    # axis.text.x= element_text(angle = 90,hjust = 1, vjust = 0.5),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10, face="bold"),
    legend.text = element_text(size = 10),
    panel.border = element_rect(colour = "black"),
    panel.background = element_blank()
  )
)


##### Database Connections
# To postgres
# database_connection <- src_postgres(dbname = "ldb", host = "129.206.154.238",
#                                    user = "mathias")
#                                    # user = "monkel")
# To SQLite
database_connection = src_sqlite("database/Sqlite.db")

##### Initial Rawdata
rawdata <- data.frame()

##### Initial Metadatatable generation
sql_data <- paste("SELECT * FROM id_info")
rawmetadata <- collect(tbl(database_connection, sql(sql_data)))

cleandata <- rawmetadata

# Some formatting of the columns that contain temporal information
cleandata$date_upload <- as.Date(cleandata$date_upload, format = '%y%m%d')
cleandata$date_sample <- as.Date(cleandata$date_sample, format = '%y%m%d')
cleandata$date_extraction <- as.Date(cleandata$date_extraction, format = '%y%m%d')
cleandata$date_measured <- as.Date(cleandata$date_measured, format = '%y%m%d')
cleandata <- cleandata[order(cleandata$id),]


##### Names for the Plot informations
datanames <- as.list(cleandata$id)
names(datanames) <- paste(cleandata$id, cleandata$title)
whatnames <-
  list(
    "Classes" = "class", "Categories" = "category",
    "Functional Categories" = "func_cat", "Species" = "chains",
    "Sum Species" = "chain_sums",
    "Length" = "length", "Double bonds" = "db", "OH" = "oh"
  )
revwhatnames <-
  list("class" = "Classes", "category" = "Categories",
       "func_cat" = "Functional Categories", "chains" = "Species",
       "chain_sums" = "Sum Species", "length" = "Length",
       "db" = "Double bonds", "oh" = "OH")

samplenames <- list()




withinnames <-
  c("Sample", "Category", "Functional Category", "Class")
standardnames <-
  c("Sample", "Category", "Functional Category", "Class")
plotchoices <- list(
  "Plot Values" = "values",
  "Errorbar (Mean +/- 1SD)" = "errsd",
  "Errorbar (Mean +/- 1SE)" = "errse",
  "Median" = "median",
  "Nr of Datapoints" = "nrdat",
  "Mean Values" = "mean",
  "Log10-Transformation" = "logtrans",
  "Show Legend" = "legend?",
  "Identify Individuals" = "sample_ident",
  "Ttest" = "ttest"
)


##### General functions needed for plotting
  # Verification that the ttest gets the desired two samples
check_ttest <- function(data, input) {
  if ("ttest" %in% input$checkGroup)
  {
    if (length(unique(data$sample)) == 2 | length(unique(input$samplesub)) == 2) {
      return(TRUE)
    }
    else {
      return(FALSE)
    }
  }
  return(TRUE)
}

  # Some functions needed for the summary plottings
se <- function(X, na.rm = T) sqrt(var(X, na.rm = na.rm)/length(X))
meanplussd <- function(X) mean(X) + sd(X)
meanminussd <- function(X)  mean(X) - sd(X)
meanplusse <- function(X) mean(X) + se(X)
meanminusse <- function(X) mean(X) - se(X)

mval <- function(X) {
  return(data.frame(y = mean(X), label = paste0(round(mean(X),3))))
}
mvalsd <- function(X) {
  return(data.frame(y = meanplussd(X), label = paste0(round(mean(X),3))))
}
mvalse <- function(X) {
  return(data.frame(y = meanplusse(X), label = paste0(round(mean(X),3))))
}

namel<-function (vec){
  tmp<-as.list(vec)
  names(tmp)<-as.character(unlist(vec))
  tmp
}

colMap <- function(x) {
  .col <- rep(rev(heat.colors(length(unique(x)))), time = table(x))
  return(.col[match(1:length(x), order(x))])
}

##### Plot the factors in the order specified
  # by the database:
sql_statement <- paste('SELECT class FROM lipid_class_order_complete order by class_order')
class_order_database <- tbl(database_connection, sql(sql_statement)) %>% data.frame()
class_order <- class_order_database$class

  # By hand
# class_order <- c("PC", "PC O-", "PE", "PE O-", "PE P-", "PS", "PS O-", "PI", "PI O-",
#                  "PG", "PG O-", "PA", "PA O-", "DAG", "CL", "MLCL", "Cer", "SM",
#                  "HexCer", "GM3", "Sulf", "SGalCer", "diHexCer", "For", "Chol", "Desm",
#                  "CE", "TAG")

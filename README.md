
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ShinyLipids

<!-- [![Travis build Status](https://travis-ci.com/jannikbuhr/ShinyLipids.svg?token=czApY9arsWuqNrp7yAYj&branch=master)](https://travis-ci.com/jannikbuhr/ShinyLipids) -->

A remake of ShinyLipids with consistent coding, documentation and
improved functionality.

## What is ShinyLipids?

ShinyLipids is a visualization tool for lipidomcis data built in R with
the `shiny` package. The original version was developed in 2015 by
*Mathias Gerl* and further enhanced by *Manuel Haußmann* and *Sebastian
Bender* until August, 2017. It serves as the primary visualization
dashboard for the [Brügger Lipidomics
Group](https://bzh.db-engine.de/default.asp?lfn=2241&fg=4289) at the
Heidelberg Biochemistry Center (BZH). There is also a great deal of
backend infrastructure, built by Mathias, to support a server with a
database, upload tools and a version of this App for internal use at the
BZH.

## What changed?

In 2018 I started working on a new version of ShinyLipids. The idea is
still very much based on the original, but the internal coding and
datastructure is designed to be more consistend and host a safer
environment for the implementation of new features within the R code.
More information can be found on [my
website](https://jmbuhr.de/project/shinylipids/).

## How do I use ShinyLipids?

Make sure you have the latest version of R installed. Then run this
piece of R code to install ShinyLipds with all it’s dependecies\[1\].
from github:

``` r
if ( !("devtools" %in% installed.packages()) ) install.packages("devtools")
devtools::install_github("jannikbuhr/ShinyLipids")
```

ShinyLipids gets the data it needs from a datbase. This database needs
to contain at least the two tables **id\_info** and **data2** \[2\]. Of
course, you might not have your data in a database-dump file. To get
your tabular data (preferably *.csv*) into such a file, you can use a
little helper function in ShinyLipids:

``` r
if ( !("readr" %in% installed.packages()) ) install.packages("readr")
myMedatdata <- readr::read_csv("myMetadata.csv")
myDataset   <- readr::read_csv("myDataset.csv")
ShinyLipids::createDatabase("./database/exampleDatabase.db", myMedatdata, myDataset)
```

Now you are ready to look at your lipids\! Run this code for a
quickstart:

``` r
db_con <- DBI::dbConnect(RSQLite::SQLite(), "./database/exampleDatabase.db")
ShinyLipids::run_app(db = db_con)
```

### What should my tables look like?

**id\_info**

| Column            |
| :---------------- |
| id                |
| title             |
| date\_upload      |
| status            |
| sample\_from      |
| date\_sample      |
| extracted\_by     |
| date\_extraction  |
| measured\_by      |
| date\_measured    |
| distinct\_samples |
| data\_lines       |
| file              |
| instruments       |

**data2**

| Column                       |
| :--------------------------- |
| id                           |
| sample\_identifier           |
| lipid                        |
| category                     |
| func\_cat                    |
| class                        |
| length                       |
| db                           |
| oh                           |
| chains                       |
| chain\_sums                  |
| sample                       |
| sample\_replicate            |
| sample\_replicate\_technical |
| value                        |

Optionally, your database can contain a table named
“LIPID\_CLASS\_ORDER\_COMPLETE” with a numeric *class\_order* column
associated with a *class* column. You can also reorder your lipid
classes in the app on bottom of the main plotting page.

## For advanced use

If you want to run ShinyLipids on your own server connceted to a
database or post your findings to shinyapps.io, you will have to
download the sourcecode:

``` bash
git clone https://github.com/jannikbuhr/ShinyLipids.git
```

Open the file `app.R`, read the comments and uncomment the specified
lines. Then you will be able to run the app or use the buttons in the
top right corner of your RStudio code panel.

## Notes

Please note that the ‘ShinyLipids’ project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to
this project, you agree to abide by its terms.

1.  If you want to publish the ShinyApp on RStudio-connect, you will
    need a couple of addictional packages that are listed in `app.R`.

2.  the naming is due to backend compatibility


<!-- README.md is generated from README.Rmd. Please edit that file -->

# ShinyLipids

<!-- badges: start -->

![Travis build
status](https://travis-ci.com/jannikbuhr/ShinyLipids.svg?token=czApY9arsWuqNrp7yAYj&branch=master)
<!-- badges: end -->

A remake of ShinyLipids with a focus on consistent coding,
documentation, improved functionality and above all, user-friendlyness.

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

Make sure you have the latest version of R and RStudio installed.+

If you are new to R, just head over to

<https://cran.r-project.org/> and follow the download link suitable for
your operating system. Then, head over to
<https://rstudio.com/products/rstudio/download/#download> and install
RStudio. You can think of R as the engine that runs the Code for
ShinyLipids and of RStudio as a car with a nice dashboard around the
engine to help you the raw power. Don’t worry, ther won’t be any coding
necessary to get startet with your data in ShinyLipids.

Then run this piece of R code to install ShinyLipds with all it’s
dependecies\[1\] from github:

``` r
if ( !("devtools" %in% installed.packages()) ) install.packages("devtools")
devtools::install_github("jannikbuhr/ShinyLipids")
```

(To run the code, copy and paste it into the R console that pops up in
the bottom right corner when you first open RStudio. Then hit Enter.)

ShinyLipids gets the data it needs from a datbase. This database needs
to contain at least the two tables **id\_info** and **data2** \[2\]. Of
course, you might not have your data in a database-dump file. To get
your tabular data (preferably *.csv*) into such a file, you can use a
little helper function in ShinyLipids, which you can run from an R
script. Before you do so, make your your tables contain the columns
listed in section [What should my tables look like?](#tables).

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

This is `myMetadata.csv`. Not all columns are needed.

| Column            | Description                   |
| :---------------- | ----------------------------- |
| id                | whole number:                 |
| title             | text: title of dataset        |
| date\_upload      | text: YYYY-MM-DD              |
| status            | text: e.g. in progress        |
| sample\_from      | text: owner of sample         |
| date\_sample      | text: YYYY-MM-DD              |
| extracted\_by     | text: name                    |
| date\_extraction  | text: YYYY-MM-DD              |
| measured\_by      | text: name                    |
| date\_measured    | text: YYYY-MM-DD              |
| distinct\_samples | whole number: n of samples    |
| data\_lines       | whole number: n of datapoints |
| file              | text: name of raw datafile    |
| instruments       | text: instruments             |

**data2**

This is `myDataset.csv`.

| Column                       | Description                      |
| :--------------------------- | -------------------------------- |
| id                           | whole number: matches metadata   |
| sample\_identifier           | text: e.g. “1Aa”                 |
| lipid                        | text: format: “Cer 34:1;2”       |
| category                     | text: e.g. SP                    |
| func\_cat                    | text: e.g. SP                    |
| class                        | text: e.g. SP Cer                |
| length                       | whole number: 34                 |
| db                           | whole number: n of double bounds |
| oh                           | whole number: n of OH groups     |
| chains                       | text: format: “34:1;2”           |
| chain\_sums                  | text: format: “34:1;2”           |
| sample                       | text: e.g. “1 - 0min Stim”       |
| sample\_replicate            | text: e.g. “1 - 0min Stim\_A”    |
| sample\_replicate\_technical | text: e.g. “1 - 0min Stim\_A\_a” |
| value                        | number                           |

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

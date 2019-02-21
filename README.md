# ShinyLipids

[![Build Status](https://travis-ci.com/jannikbuhr/ShinyLipids.svg?token=czApY9arsWuqNrp7yAYj&branch=dev)](https://travis-ci.com/jannikbuhr/ShinyLipids)

A remake of ShinyLipids with consistent coding and documentation as well as improved functionality.

## What is Shiny Lipids?

ShinyLipids is a visualization tool for lipidomcis data built in R with the `shiny` package. The original
version was developed in 2015 by _Mathias Gerl_ and further enhanced by _Manuel Haußmann_ and _Sebastian Bender_
until August, 2017. It serves as the primary visualization dashboard for the [Brügger Lipidomics Group](https://bzh.db-engine.de/default.asp?lfn=2241&fg=4289) at the Heidelberg Biochemistry Center (BZH).
There is also a great deal of backend infrastructure, built by Mathias, to support a server with a database,
upload tools and a version of this App for internal use at the BZH.

## What changed?

In 2018 I started working on a new version of ShinyLipids. The idea is still very much based on the original,
but the internal coding and datastructure is designed to be more consistend and host a safer environment for the
implementation of new features within R. A documentation can be found on [my website](https://jannikbuhr.github.io/doc/shinylipids/).

## How do I use ShinyLipids?

You can download or clone this repository from GitHub. Make sure you have the latest version of R installed.
Then, open the `ShinyLipids.Rproj`-file in in R-Studio and have a look at the file `global.R`. From line 4 onwards
you find a list of R-packages you need in order for ShinyLipids to run smoothly. You can also have a look
at _"docs/sessionInfo.txt"_ to see my version of R, operating system and installed packages ^[If you want to publish
the ShinyApp on RStudio-connect, you will need a couple of addictional packages.].
Once all packages are installed, make sure line 31 in `global.R` is active and place an approriate SQLite-database
file under _/database/Sqlite.db_. This database file needs to contain the two tables "id_info" and "data2"
(the naming is due to backend compatibility, because I did not touch the original server- and database-structure).

**You should have the following columns in the respective tables:**

Table: id_info

| id | title | date_upload | status | sample_from| date_sample | extracted_by | date_extraction | measured_by | date_measured | distinct_samples | data_lines | file | instruments |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|

Table: data2

id | sample_identifier | lipid | category | func_cat | class | length | db | oh | chains | chain_sums | sample | sample_replicate | sample_replicate_technical | value
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|

Optionally, your database can contain a table named "LIPID_CLASS_ORDER_COMPLETE" (name like this for backwards compatibility) with
a numeric _class_order_ column associated with a _class_ column. You can also reorder your lipid classes in the app.

Once you are ready, press _run App_ in the top right corner of your R-Studio editor panel with e.g. global.R open, or
if you prefer typing over clicking, run `shiny::runApp()` in the R-console or use the R-Studio-Shortcut `ctrl+shift+Enter` (with `cmd` instead of `ctrl` for mac users).

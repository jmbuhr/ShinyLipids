# Example Script to create your
# own .db Database-dump file
library(DBI)

# Create a connection to a file
# "exampleDatabase.db" in the folder
# "exampleDatabase"
# Note that this is a relative path to the folder, starting from
# the working directory of your R-session (or the root of an RStudio project folder)
con <- dbConnect(RSQLite::SQLite(), "R/exampleDatabase/exampleDatabase2.db")

# Read in tabular data with the columns specified in the README.
# Info about the dataset(s):
id_info <- read.csv("R/exampleData/datasets_info.csv", stringsAsFactors = FALSE)

# The actual data (this step can be repeated for multiple datasets):
data2 <- read.csv("R/exampleData/2019-09-07_Eliot_Morrison-raw.csv", stringsAsFactors = FALSE)

# If you have excel files instead of .csv files, feel free to
# use the next line instead of read.csv
# name_for_your_data <- readxl::read_excel("path_to_your_file.xlsx", sheet = 1)

# Write the data to the database connection
dbWriteTable(con, "id_info", id_info)
dbWriteTable(con, "data2", data2)

# Close the connection
dbDisconnect(con)

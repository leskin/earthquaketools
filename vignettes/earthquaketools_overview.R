## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
library(earthquaketools)
# First read in the raw data file into a data frame from its compressed tab-separated variable file
earthquakes_raw <- readr::read_delim("earthquakes.tsv.gz", delim = "\t")
# After we have the raw data, clean it up for use by the visualization functions
eq_data <- eq_clean_data(earthquakes_raw)
# and summarize the cleaned data
str(eq_data)


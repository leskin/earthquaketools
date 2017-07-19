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

## ---- eval = TRUE, fig.width=7-------------------------------------------
library(earthquaketools)
library(grid)
library(ggmap)
library(magrittr)

eq_data <- readr::read_delim("earthquakes.tsv.gz", delim = "\t") %>%
  eq_clean_data() %>% dplyr::filter(COUNTRY == "USA" | COUNTRY == "CHINA")

ggplot(eq_data) +
  geom_timeline(aes(x = DATE, colour = TOTAL_DEATHS, size = EQ_PRIMARY), 
                alpha = 0.5, xmindate = 2000, xmaxdate = 2017) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title.y = element_blank(), 
        axis.line.y = element_blank(), 
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey", size = 0.5)) +
  labs(size = "Richter scale value ", colour = "# deaths ")


## ---- eval = TRUE, fig.width=7-------------------------------------------

ggplot(eq_data) +
  geom_timeline(aes(x = DATE, y = COUNTRY, colour = TOTAL_DEATHS, size = EQ_PRIMARY), 
                alpha = 0.5, xmindate = 2000, xmaxdate = 2017) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title.y = element_blank(), 
        axis.line.y = element_blank(), 
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey", size = 0.5)) +
  labs(size = "Richter scale value ", colour = "# deaths ")


## ---- eval = FALSE, fig.width=7------------------------------------------
#  
#  ggplot(eq_data) +
#    geom_timeline(aes(x = DATE, y = COUNTRY, colour = TOTAL_DEATHS, size = EQ_PRIMARY),
#                  alpha = 0.5, xmindate = 2000, xmaxdate = 2017) +
#    geom_timeline_label(aes(label = LOCATION_NAME), n_max = 5) +
#    theme_classic() +
#    theme(legend.position = "bottom",
#          axis.title.y.left = element_blank(),
#          axis.line.y.left = element_blank(),
#          axis.ticks.y.left = element_blank(),
#          panel.grid.major.y = element_line(colour = "grey", size = 0.5)) +
#    labs(size = "Richter scale value ", colour = "# deaths ")
#  

## ---- eval = TRUE, fig.width=7-------------------------------------------

  readr::read_delim("earthquakes.tsv.gz", delim = "\t") %>%
  eq_clean_data() %>%
  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
  eq_map(annot_col = "DATE")


## ---- eval = TRUE, fig.width=7-------------------------------------------

readr::read_delim("earthquakes.tsv.gz", delim = "\t") %>%
  eq_clean_data() %>%
  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
  dplyr::mutate(popup_text = eq_create_label(.)) %>%
  eq_map(annot_col = "popup_text")



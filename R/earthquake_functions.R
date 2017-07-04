#'
#' Create a data frame containing a cleaned version of the NOAA Significant Earthquakes Dataset
#'
#' This function manipulates and cleans the earthquake data from the U.S. National Oceanographic
#' and Atmospheric Administration (NOAA) on significant earthquakes around the world.
#' This dataset contains information about 5,933 earthquakes over an approximately 4,000 year time span,
#' and is avaialable at the URL:
#' https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1
#'
#'
#' The function reads the raw NOAA data and returns a clean data frame.
#' The clean data frame has the following:
#'
#' 1.	A date column created by uniting the year, month, day and converting it to the Date class
#' 2.	LATITUDE and LONGITUDE columns converted to numeric class
#' 3.	A LOCATION_NAME column which has been cleaned by stripping out the country name (including the colon)
#'    and converting the names to title case (as opposed to all caps).
#'
#' @param earthquakes_raw The raw NOAA data frame to be cleaned.
#'
#' @return This function returns a data.frame containing the cleaned NOAA earthquake data.
#' A DATE column is created by uniting the year, month day and converting it to Date class.
#' LATITUDE and LONGITUDE columns are converted to numeric class and
#' the LOCATION_NAME column has been cleaned by stripping out the country name (including the colon)
#' and converting the names to title case (as opposed to all caps).
#'
#' @importFrom lettercase str_title_case
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na unite
#' @importFrom dplyr mutate filter select
#'
#' @examples
#' \dontrun{eq_clean_data(earthquakes.tsv)}
#'
#' @export
eq_clean_data <- function(earthquakes_raw) {

  earthquakes_clean <- earthquakes_raw %>%
    #dplyr::select(YEAR, MONTH, DAY, EQ_PRIMARY, COUNTRY, LOCATION_NAME, LATITUDE, LONGITUDE, TOTAL_DEATHS) %>%  # Select only the needed columns
    dplyr::filter(YEAR >= 0) %>%  # the Date class only pertains positive year values
    tidyr::replace_na(list(MONTH = 1, DAY = 1)) %>% # replace any NA values with 1 for MONTH and DAY
    tidyr::unite(datetime, YEAR, MONTH, DAY, remove = FALSE, sep = "-") %>%  # create a datetime character variable for processing
    dplyr::mutate(DATE = as.POSIXct(datetime, format = "%Y-%m-%d")) %>% # convert datetime to the Date class
    dplyr::mutate(clean_loc = gsub(".*:","", LOCATION_NAME)) %>% # remove the country name(s) from the LOCATION_NAME
    dplyr::mutate(LOCATION_NAME = lettercase::str_title_case(tolower(clean_loc))) %>% # and convert LOCATION_NAME to title case
    dplyr::mutate(plot_magnitude = EQ_PRIMARY) %>%
    tidyr::replace_na(list(plot_magnitude = 1)) %>%
    dplyr::select(-clean_loc, -datetime) # remove unneeded intermediate columns from the data.frame

  return(earthquakes_clean)
}



#' This code is based on input from the Extending ggplot2 vignette:
#' https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html

library(grid)
library(ggmap)
#library(geosphere)

#'
#' The following code creates the geom_timeline geom that is used to visualize some of the
#' information in the NOAA earthquakes dataset.
#'
#' Create a new ggproto object (GeomEarthquake) for a new geom (geom_timeline) for ggplot2
#'
#' This ggproto object inherits from the basic Geom.  Default values are provided for the
#' aesthetics size, linetype and alpha.  A null ggproto opject is created if there are no
#' earthquakes in the desired time range.
#'
GeomEarthquake <- ggproto("GeomEarthquake", Geom,
                          required_aes = c("x"),
                          default_aes = aes(y = NULL, shape = 19, colour = "black", fill = NA, size = 0.8, linetype = 1, alpha = 0.6),
                          draw_key = draw_key_point,

                          draw_panel = function(data, panel_scales, coord) {
                            # return a nullGrob if no point info
                            n <- nrow(data)
                            if (n < 1) return(grid::nullGrob())

                            ## Transform the data first
                            coords <- coord$transform(data, panel_scales)

                            # Use the color information provided in the first row
                            #first_row <- coords[1, , drop = FALSE]

                            grid::pointsGrob(
                              coords$x, coords$y,
                              default.units = "native",
                              pch = coords$shape,
                              gp = grid::gpar(
                                col = coords$colour,
                                size = coords$size,
                                alpha = coords$alpha
                              )
                            )
                          })

#' Create a new geom (geom_timeline) for ggplot2
#'
#' This function creates a new geom which will create a linear timeline for a
#' specified time range, and will display each earthquake as a point on the timeline.
#'
#' This geom makes use of the GeomEarthquake ggproto object, inherits from the basic Geom.
#' The GeomEarthquake object defines default values for the optional
#' aesthetics color, size, and alpha.  A null ggproto opject is created if there are no
#' earthquakes in the desired time range.
#'
#' This geom is based on (but does not inherit from) the point geom (geom_point).  Many of the input
#' parameters are the same as for the geom_point.  Parameter descriptions for identical parameters
#' are taken from the geom_point help file to minimize confusion.
#'
#' @param mapping A set of aesthetic mappins created by aes or aes_.  If specified and inherit.aes = TRUE (the default),
#' it is combined with the default mappint at the top level of the plot.  You must supply mapping if there is no plot mapping.
#' @param data The datat to be displayed in this layer.  There are three options:
#'
#' If NULL, the default, the data is inherited from the plot data as specified in the call to ggplot.
#'
#' A data.frame, or other object, will override the plot data.  All objects will be fortified to produce a data frame.  See fortify for which variables will be created.
#'
#' A function will be called wiht a single argument, the plot data. The return value must be a data.frame, and will be used as the layer data.
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param ... Other arguments passed on layer.  Thes are often aesthetics, used to set an aesthetic to a fixed value, like color = "red" or size = 3.
#' They may also be parameters to the paired geom/stat.
#' @param na.rm If FALSE, the default, missing values are mremoved with a warning.  If TRUE, missing values are silently removed.
#' @param show.legend logical.  Should this layer be included in the legends?  NA, the default, incldes if any aesthetics are mapped.  False never includes,
#' and TRUE always includes.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them.  This is most usefule for helper functions that
#' define both data and aesthetics and shouldn't inherit behavior from the default plot specification, e.t. borders.
#' @param xmin Minimum year to display on the timeline
#' @param xmax Maximum year to display on the timeline
#'
#' @return This function has no return value
#'
#' @importFrom ggplot2 layer
#'
#' @examples
#' \dontrun{geom_earthquake(data = earthquakes_clean, aes(x = DATE, y = "COUNTRY", color = "black", size = 4, alpha = 0.6), xmin = 2000, xmax = 2017)}
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE,
                          y = NULL, xmin = 2000, xmax = 2015, ...) {

  # call the layer() function to add this layer to the map
  ggplot2::layer(
    geom = GeomEarthquake,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


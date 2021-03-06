% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/earthquake_functions.R
\name{eq_create_label}
\alias{eq_create_label}
\title{Create an HTML label that can be used as an annotation text in the leaflet map}
\usage{
eq_create_label(data = NULL)
}
\arguments{
\item{data}{The data to be used to create the HTML label that will be used as annotation text in the map.

If NULL, the default, the data is inherited from the plot data as specified in the call to ggplot.

A data.frame, or other object, will override the plot data.  All objects will be fortified to produce a data frame.  See fortify for which variables will be created.

A function will be called with a single argument, the plot data. The return value must be a data.frame, and will be used as the layer data.}
}
\value{
This function returns a character vector
}
\description{
This function creates an HTML character string for each earthquake in a dataset
that will show the cleaned location name (as cleaned by the eq_location_clean() function),
the magnitude (EQ_PRIMARY), and the total number of deaths (TOTAL_DEATHS),
with boldface labels for each ("Location", "Total deaths", and "Magnitude").
If an earthquake is missing values for any of these, both the label and the value are
skipped for that element of the tag.
}
\examples{
\dontrun{eq_create_label(data = eq_data)}

}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fdir_helper.R
\name{asp2extent}
\alias{asp2extent}
\title{Recalculate extent values from a given aspect ratio}
\usage{
asp2extent(xmin, xmax, ymin, ymax, asp = 1)
}
\arguments{
\item{xmin, xmax, ymin, ymax}{Numeric Vectors of the same length specifying the x/y extents}

\item{aps}{Aspect ratio calculated as specified in the description. A single numeric value or a vector in the
same length as xmin etc.}

\item{return}{Returns a dataframe with four columns named \code{xmin}, \code{xmax}, \code{ymin}, \code{ymax}}
}
\description{
Takes four vectors (xmin, xmax, ymin and ymax) and returns the new values based on a given aspect ratio
Aspect ratio is calculated as \eqn{\frac{\Delta y\ }{\Delta x\ }}. Values > 1 therefore means portrait,
values < 1 means landscape.
}
\details{
At the moment, this function can only enlargen the extent in order to match the aspect ratio. At one point,
it might be neccessary to add the option to shrink the extent accordingly.
}

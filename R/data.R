#' landesgebiet
#'
#' Swissboundries3D Country data .
#'
#' @format An \code{sf} object containing polygon data with four features.
#' \describe{
#'   \item{NAME}{Name of the country}
#'   \item{geometry}{sfc_POLYGON  data in EPSG 2056}

#' }
#' @source \url{https://shop.swisstopo.admin.ch/de/products/landscape/boundaries3D}
"landesgebiet"


#' Sample Polygons
#'
#' Polygons of the 10 largest muncipalities ("Gemeinden") in Switzerland.
#'
#' @format An \code{sf} object containing polygon data
#' \describe{
#'   \item{NAME}{Name of the muncipality in its main language}
#'   \item{EINWOHNERZ}{Number of inhabitants}
#'   \item{geometry}{sfc_POLYGON  data in EPSG 2056}

#' }
#' @source \url{https://shop.swisstopo.admin.ch/de/products/landscape/boundaries3D}
"gemeinden_top_poly"



#' Sample Points
#'
#' Centroids of the 10 largest muncipalities ("Gemeinden") in Switzerland.
#' This dataset is based on \code{\link{sample_points}}
#'
#' @format An \code{sf} object containing point data
#' \describe{
#'   \item{NAME}{Name of the muncipality in its main language}
#'   \item{EINWOHNERZ}{Number of inhabitants}
#'   \item{geometry}{sfc_POINT data in EPSG 2056}

#' }
#' @source \url{https://shop.swisstopo.admin.ch/de/products/landscape/boundaries3D}
"sample_points"

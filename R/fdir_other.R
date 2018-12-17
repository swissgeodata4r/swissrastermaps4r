
#' Import an existing fdir
#'
#' Avoid rescanning all your files (\code{fdir_init}) by
#' exporting an existing \{fdir} (with \code{fdir_export})
#' after initializing and importing it again in the next session
#'
#' The function enables importing an existing "File Directory"
#' (\code{fdir}) from an ".Rda" File after running \code{fdir_init}
#' and \code{fdir_export}. This avoids having to rescan all the files
#' with \code{fdir_init}. Handle with care, changes in the source files
#' are not registered with this method. Use \code{fdir_init} if unsure.

#' @param name Path to an .Rda File
fdir_import <- function(path){
  fdir <- get(load(path))
  assign("fdir",fdir,envir = swissrastermapEnv)
}

#' Export an existing fdir to an Rda-File
#'
#' Avoid rescanning all your files (\code{fdir_init}) by
#' exporting an existing \{fdir} (with \code{fdir_export})
#' after initializing and importing it again (with \code{fdir_import})
#' in the next session
#'
#' The function enables exporting an existing "File Directory"
#' (\code{fdir}) to an ".Rda" File after running \code{fdir_init}.
#' This avoids having to rescan all the files with \code{fdir_init}.
#' Handle with care, changes in the source files are not registered with
#' this method. Use \code{fdir_init} if unsure.
#'
fdir_export <- function(path){
  if(exists("fdir", envir = swissrastermapEnv)){
    fdir <- get("fdir",envir = swissrastermapEnv)
    save(fdir,file = path)
  } else{
    stop("Please run fdir_init() first.")
  }
}

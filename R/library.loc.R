#' Load package (and dependencies) from specified directory.
#'
#' For the requested package check that all dependencies as well as the package
#' itself are installed to the given directory. If they are not then the
#' package will not be loaded. This is to prevent a package being loaded from
#' one directory (e.g. within a project folder) but the dependencies being
#' loaded from another (e.g. default R install locations) - as can be the case
#' if \code{library(pkg, lib = directory)} is used.
#'
#' @param package name of package to load.
#' @param loc directory to load from.
#' @param ... other arguments to pass to library.
#'
#' @examples
#' library.loc(pkg = gbm, loc = "/Users/richardangell/Projects/test")
#'
#' @export
library.loc <- function(pkg, loc, ...) {
  
  # check loc directory exists
  if (!dir.exists(loc)) {
    
    stop(gettextf("loc (%s) does not exist.", sQuote(loc)))
    
  }
  
  # quote package if it is not passed quoted already
  pkg <- as.character(substitute(pkg))
  
  # record current libPaths
  prev_libPaths <- .libPaths()
  
  cat(gettextf(".libPaths() currently set to %s.", 
               paste(prev_libPaths, collapse = " ")),
      "\n")
  
  # check if .libPaths() is already set to the supplied loc
  if (length(prev_libPaths) == 1 && prev_libPaths == loc) {
    
    cat("not reassigning .libPaths as it is already equal to loc.\n")
    
  # if .libPaths() is not equal to loc and needs to be set
  } else {
    
    cat("reassigning .libPaths()\n")

    assign(".lib.loc", loc, envir = environment(.libPaths))
    
    post_libPaths <- .libPaths()
    
    cat(gettextf(".libPaths() set to %s.", 
                 sQuote(post_libPaths)),
        "\n")
    
    if (post_libPaths != loc) {
      
      stop(".libPaths() was not reassigned successfully.")
      
    }
    
  }
  
  cat(gettextf("attempting to load package %s from %s.", 
               sQuote(pkg),
               sQuote(loc)),
      "\n")
  
  library(pkg, lib.loc = loc, character.only = TRUE, ...)
  
}










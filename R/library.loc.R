#' Load package (and dependencies) from specified directory.
#'
#' This function reassigns .libPaths() to the specified directory then tries to
#' load the package using \code{base::library}. This is to ensure that package
#' depenedencies (and the package itself) are not loaded from other directories
#' in the search tree (\code{.libPaths()}).
#'
#' @param package name of package to load.
#' @param loc directory to load from.
#' @param ... other arguments to pass to \code{base::library}.
#'
#' @examples
#' cwd <- getwd()
#' 
#' dir1 <- paste0(cwd, "/dir1")
#' dir2 <- paste0(cwd, "/dir2")
#' 
#' dir.create(dir1)
#' dir.create(dir2)
#' 
#' # install gbm package with and without dependencies
#' install.packages("gbm", lib = dir1, dependencies = c("Depends", "Imports"))
#' install.packages("gbm", lib = dir2, dependencies = FALSE)
#' 
#' # try to load from the directory without dependencies
#' library.loc(gbm, loc = dir2)
#' 
#' # now try to load from the directory with dependencies installed
#' library.loc(gbm, loc = dir1)
#' 
#' # to see behaviour of base::library - restart R before running
#' .libPaths(c(dir1, dir2))
#' library(gbm, lib.loc = dir2)
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





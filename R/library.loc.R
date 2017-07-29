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
#'
#' @examples
#' library.loc(gbm, )
#'
#' @export
library.loc <- function(package, loc, ...) {
  
  if (!dir.exists(loc)) {
    
    stop(gettextf("loc (%s) does not exist.", sQuote(loc)))
    
  }
  
  # quote package if it is not passed quoted already
  package <- as.character(substitute(package))
  
  # record current libPaths
  prev_libPaths <- .libPaths()
  
  print(gettextf(".libPaths() currently set to %s.", sQuote(prev_libPaths)))
  
  if (prev_libPaths == loc) {
    
    print("not reassigning .libPaths as it is already equal to loc.")
    
  } else {
    
    print("reassigning .libPaths()")
    
    assign(".lib.loc", loc, envir = environment(.libPaths))
    
    post_libPaths <- .libPaths()
    
    print(gettextf(".libPaths() set to %s.", sQuote(post_libPaths)))
    
    
    
  }
  
  
  

  library(package, lib.loc = loc, ...)
  
}














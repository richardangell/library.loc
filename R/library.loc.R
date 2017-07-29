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
library.loc <- function(package, loc) {
  
  # quote package if it is not passed quoted already
  package <- as.character(substitute(package))
  
  installed_packages <- data.frame(installed.packages(lib = loc),
                                   stringsAsFactors = FALSE)
  
  session_info <- sessionInfo()
  
  #-----------------------------------------------------------------------------#
  # Section 1. Check if package in installed in loc ----
  #-----------------------------------------------------------------------------#
  
  pkg_installed <- package_in_dir(pkg = package,
                                  dir = loc,
                                  version = NULL)
  
  pkg_info <- installed_packages[installed_packages$Package == package, ]
  
  #-----------------------------------------------------------------------------#
  # Section 2. Check if depends for package are installed in loc ----
  #-----------------------------------------------------------------------------#
  
  if (!is.na(pkg_info$Depends)) {
    
    check_pkg_dependencies(pkg_info$Depends)
    
  }
  
  #-----------------------------------------------------------------------------#
  # Section 3. Check if imports for package are installed in loc ----
  #-----------------------------------------------------------------------------#
  
  if (!is.na(pkg_info$Imports)) {
    
    check_pkg_dependencies(pkg_info$Imports)
    
  }
  
  #-----------------------------------------------------------------------------#
  # Section 4. Load package ----
  #-----------------------------------------------------------------------------#
  
  library(package, loc = loc)
  
}
















# split out each dependancy
pkg_depends <- strsplit(pkg_info$Depends, ",")[[1]]

pkg_depends <- trimws(pkg_depends, which = "both")

for (i in 1:length(pkg_depends)) {
  
  # skip over dependancy if it is R
  if (pkg_to_check_split[1] != "R") {
    
    is_package_in_loc(pkg_to_check[1], loc)
    
    depends_info <- installed_packages[installed_packages$Package == pkg_to_check_split[1], ]
    
    # if there is a version number specified for the dependancy
    if (length(pkg_to_check_split) > 1) {
      
      # check that the version of the depend package is sufficient
      pkg_version_check()
      
      
      
    }
    
  }
  
}

}



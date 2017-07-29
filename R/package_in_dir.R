

package_in_dir <- function(pkg, dir, required_version = NULL) {
  
  package_installed <- FALSE
  
  installed_packages <- data.frame(installed.packages(lib.loc = dir),
                                   stringsAsFactors = FALSE)
  
  if (!any(installed_packages$Package == package)) {
    
    stop(gettextf("there is no package called %s installed in %s",
                  sQuote(package),
                  dir))
    
  } else {
    
    package_installed <- TRUE
    
  }
  
  
  if (!is.null(version)) {
    
    pkg_info <- installed_packages[installed_packages$Package == package, ]
    
    installed_version <- strsplit(pkg_version, " ")[[1]]
    
    pkg_version_comparison <- compareVersion(installed_version, version)
    
    
    
    if (length(pkg_version) == 1) {
      
      if (pkg_version_comparison != 0) {
        
        stop("%s package (%s) installed in %s does not match required version (%s)",
             sQuote(pkg),
             sQuote(pkg_version),
             dir,
             sQuote(version))
        
      }
      
    } else if (length(pkg_version) == 2) {
      
      
      
    } else {
      
      stop(gettextf("unable to parse package version requirement for %s from %s",
                    sQuote(pkg),
                    sQuote(version))
      
    }
    
    
  }
  
  
 
  
}





a <- data.frame(installed.packages(lib.loc = "/Users/richardangell/Projects/R-examples/R_Packages/Install"),
                stringsAsFactors = F)

equality <- ">="

v1 <- "3.4"
v2 <- "3.5"

eval(parse(text = paste(sQuote(v1),
                        equality,
                        sQuote(v2))))


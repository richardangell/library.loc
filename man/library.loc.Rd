% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/library.loc.R
\name{library.loc}
\alias{library.loc}
\title{Load package (and dependencies) from specified directory.}
\usage{
library.loc(pkg, loc, ...)
}
\arguments{
\item{loc}{directory to load from.}

\item{...}{other arguments to pass to \code{base::library}.}

\item{package}{name of package to load.}
}
\description{
This function reassigns .libPaths() to the specified directory then tries to
load the package using \code{base::library}. This is to ensure that package
depenedencies (and the package itself) are not loaded from other directories
in the search tree (\code{.libPaths()}).
}
\examples{
cwd <- getwd()

dir1 <- paste0(cwd, "/dir1")
dir2 <- paste0(cwd, "/dir2")

dir.create(dir1)
dir.create(dir2)

# install gbm package with and without dependencies
install.packages("gbm", lib = dir1, dependencies = c("Depends", "Imports"))
install.packages("gbm", lib = dir2, dependencies = FALSE)

# try to load from the directory without dependencies
library.loc(gbm, loc = dir2)

# now try to load from the directory with dependencies installed
library.loc(gbm, loc = dir1)

# to see behaviour of base::library - restart R before running
.libPaths(c(dir1, dir2))
library(gbm, lib.loc = dir2)

}

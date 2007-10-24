\name{gWidgetsRGtk2-package}
\alias{gWidgetsRGtk2-package}
\alias{gWidgetsRGtk2}
\docType{package}
\title{
Toolkit implementation of gWidgets for RGtk2
}
\description{
Port of gWidgets API to RGtk2
}
\details{
  This package allows the gWidgets API to use the RGtk2 package allowing
  the use of the GTK libraries within R. The documentation for
  the functions in this package are contained in the gWidgets package.

  As gWidgets is meant to be multi-toolkit, this file documents
  differences from the API.

  \bold{Containers:}\cr

  The \code{glayout} container is not dynamic. First you load the
  variables, then you need to call \code{visible()<- TRUE}. Once this is
  set, no new widgets can be added to the container.
  \cr
  

  \bold{Widgets:}\cr

  In the data frame editor (gdf()) the subset= option only works if the
  column names have not been changed.
  \cr

  
  \bold{Methods:}\cr
  
  The \code{font} method is not implemented.
  \cr

  
  


  
}
\author{
Michael Lawrence, John Verzani

Maintainer: John Verzani <gwidgetsrgtk@gmail.com>
}
% \references{}
\keyword{ package }
\seealso{gWidgets}

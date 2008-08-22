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
  differences from the API, as defined by the man pages of the
  \pkg{gWidgets} package.



  \bold{Containers:}\cr

  To access the underlying gtk container from a \code{gframe} object
  one uses \code{getToolkitWidget(obj)$getParent()} \cr

  \bold{Widgets:}\cr

  In the data frame editor \code{gdf} the \code{subset} option only works if the
  column names have not been changed.
  \cr

  
  \bold{Methods:}\cr
  
  The \code{font} method is not implemented. 
  \cr

  For widgets which allow markup (\code{gframe}, \code{glabel}) PANGO
  markup is used. This is not HTML, but is similar to basic HTML.\cr


  \bold{gWidgetsRGtk2 and the RGtk2 package:}\cr	
  
  The \pkg{RGtk2} package is imported only so its namespace, which is
  large, is not loaded by default. To access its functions, load the
  package. \cr

  The \pkg{RGtk2} package and \pkg{gWidgetsRGtk2} can be used together
  in the following ways. First, an \pkg{RGtk2} object can be added to
  a \pkg{gWidgetsRGtk2} through the \code{add} method of the
  container. This works for most objects. If you find one that doesn't
  work, simply place it inside a \code{gtkHBox} container, then add
  that container. Second, a \pkg{gWidgetsRGtk2} object can be added to
  to a \pkg{RGtk2} container by adding the return value of the
  \code{getToolkitWidget} method of the object. Again, this should
  work, but if not, the \pkg{gWidgetsRGtk2} can be added to a
  \code{ggroup} container first. In either case, the
  \pkg{gWidgetsRGtk2} object should not be attached to a container, so
  in particular the constructor should be called with its
  \code{container} argument as \code{NULL} (the default).


  
}
\author{
Michael Lawrence, John Verzani

Maintainer: John Verzani <gwidgetsrgtk@gmail.com>
}
% \references{}
\keyword{ package }
\seealso{gWidgets}

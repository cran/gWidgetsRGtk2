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

  If using a \code{ggraphics} device, one should call \code{gwindow}
  with the argument \code{visible=FALSE} then after the device is
  added call the \code{visible<-} method to show the window.


  To access the underlying gtk container from a \code{gframe} object
  one uses \code{getToolkitWidget(obj)$getParent()} \cr

  The gnotebook changed handler has component \code{pageno} to
  indicate the newly selected page number, as the \code{svalue} method
  returns the page before the change.\cr

  \bold{Widgets:}\cr
  
  The \code{gbutton} constructor can not be called before
  \pkg{gWidgetsRGtk2} is loaded. This means that an initial call like
  \code{gbutton("label", cont=gwindow())} won't work. Instead, either
  load directly \pkg{gWidgetsRGtk2} (not just \pkg{gWidgets}) or
  create another widget, like a top-level window. Something similar is the case
  for the \code{gdfedit} widget.
  \cr

  The \code{gradio} widget can now have its items shortened or lengthened via \code{[<-}.\cr

  For the data frame viewer \code{gtable} when no filtering is
  requested -- the default -- the column headers can be clicked to sort
  the values. Setting the index to 0 will clear the selection.


  In the data frame editor \code{gdf} the \code{subset} option only
  works if the column names have not been changed. One can suppress
  the creation of keyboard navigation and the right click popup on the
  column headers. The hiddern argument \code{diy} (for do it yourself)
  if left empty will place in both. A value of \code{"suppress.key"}
  or \code{"suppress.popup"} (or both) will suppress the respective
  handler.  \cr

  The \code{gaction} constructor produces action objects. The
  \code{enabled<-} method can be used to set their sensitivity. The
  objects can be used with \code{gbutton} through the \code{action}
  argument, and in the lists defining menubars and toolbars. The
  \code{key.accel} argument (for assigning a keyboard accelerator) of the constructor is ignored for now. The
  \code{tooltip} is OS sensitive, as it depends on the event loop
  implementation. \cr

  The \code{gtoolbar} list can have components that are a) lists with
  a handler componented, b) lists with a separator component, c)
  gaction instances d) gWidgets, in which case the widget appears in
  the toolbar. The latter is not portable to other gWidgets
  implementations.\cr
  
  The \code{gvarbrowser} constructor depends on a variable
  \code{knownTypes}. A default is provided in the package, but this can
  be overridden by a) providing a hidden argument \code{knownTypes} to
  the constructor or b) setting an option \code{knownTypes}. In each
  case this is a named list whose components are character vectors
  listing classes of a similar nature. For example, the default value
  for \code{knownTypes} included \code{"data sets"=
  c("numeric","logical","factor","character","integer",
  "data.frame","matrix","list", "table","xtabs",
  "nfnGroupedData","nffGroupedData","nmGroupedData",
  "POSIXct","POSIXlt","POSIXt" )}.  \cr

  The function used to map a class to an icon is by default
  \code{getStockIconFromClass}. This can be changed by assigning a
  function to the option \code{gWidgetsStockIconFromClass}. This function should take a class and return a stock icon name. (The class passed is the first value only.) 

  The \code{gfile} constructor has the argument \code{multiple}, which if
  TRUE will allow for multiple selections of files. This feature
  should be merged into the gWidgets API, but for now is passed in via
  \code{...}.
  \cr

  The \code{ggraphics} constructor provides a means to embed a graphics
  window inside a GUI. A right mouse popup allows one to copy the
  graphic to the clipboard or save it to a file. The different file
  types are limited by the function \code{gdkPixbufSave} whose manual page states
  that jpg, png, ico and bmp are permissable.

  A few quirks exist.
  \enumerate{
    \item Drawing a graphic too soon may result in a message about
    \code{plot.margins too small}. This comes from trying to draw the
    first graphic before the window is fully realized.
    \cr

    One workaround is to initially set the window not visible then when the GUI is done, make the window visible. That is, try: \code{w <- gwindow(visible=FALSE); ggraphics(cont=w); visible(w) <- TRUE; hist(rnorm(100))}

    \item When there are multiple
  devices, the standard means of setting a device via \code{dev.set}
  are supplemented by mouse handlers. Clicking in the graphics window
  sets the window as the current device.
  \item The handler for \code{addHandlerClicked} responds to a  mouse
  click. The components \code{x} and \code{y} give the coordinates in
  "usr" coordinates.
  \item The handler for \code{addHandlerChanged} responds to the
  "rubber-banding" effect that comes from trying to trace out a
  rectangle in the graphic window. The components \code{x} and \code{y}
  give the coordinates in "usr" coordinates. (These each have two
  values.) The functions \code{grconvertX} and \code{grconvertY} can
  convert to other coordinate systems for you. See the \code{ggraphics}
  help page for an example of how
  this can be used to update a data frame.
}

The \code{gbasicdialog} constructor can be used both ways. The hidden
argument \code{buttons} can take values \code{ok}, \code{yes},
\code{cancel}, \code{close}, \code{no}, with a default of \code{c("ok","cancel")}.

	
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
  \pkg{gWidgetsRGtk2} object should not be previsously attached to a container, so
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

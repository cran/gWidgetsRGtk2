setClass("gSliderRGtk",
         contains="gComponentRGtk",
         prototype=prototype(new("gComponentRGtk"))
         )

setMethod(".gslider",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   from=0, to=100, by = 1,
                   value=from,
                   horizontal=TRUE,
                   handler=NULL, action=NULL,
                   container=NULL, ...) {

            force(toolkit)
            
            if (horizontal)
              scale <- gtkHScaleNewWithRange(from, to, by)
            else scale <- gtkVScaleNewWithRange(from, to, by)
            scale$setValue(value)

            obj <- as.gWidgetsRGtk2(scale)

            
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj,...)
            }
            
            if (!is.null(handler))  {
              id = addhandlerchanged(obj, handler, action)
            }
            
            invisible(obj)
          })

as.gWidgetsRGtk2.GtkHScale <- as.gWidgetsRGtk2.GtkVScale <-
  function(widget,...) {
    obj <- new("gSliderRGtk",block=widget, widget=widget,
               toolkit=guiToolkit("RGtk2"))
    return(obj)
  }

### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gSliderRGtk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            obj@widget$getValue()
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitRGtk2",obj="gSliderRGtk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   obj@widget$setValue(value)
                   return(obj)
                 })


### handlers
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gSliderRGtk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            addhandler(obj, "value-changed", handler, action)
          })

##################################################
## add a separator to a container. Needs the container

setClass("gSeparatorRGtk",
         contains="gComponentRGtk",
         prototype=prototype(new("gComponentRGtk"))
         )

## should this return object?
setMethod(".gseparator",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   horizontal = TRUE, container = NULL, ...) {

            force(toolkit)
            
            if(horizontal) {
              separator = gtkHSeparatorNew()
            } else {
              separator = gtkVSeparatorNew()
            }

            
            obj = new("gSeparatorRGtk", block=separator, widget=separator, toolkit=toolkit)

            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj,...)
            }

            invisible(obj)
            
          })



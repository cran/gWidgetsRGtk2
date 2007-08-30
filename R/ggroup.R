## class in aaaClasses.R
## constructor
setMethod(".ggroup",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   horizontal = TRUE, spacing = 5,
                   use.scrollwindow = FALSE, 
                   container = NULL, ... 
                   ) {

            force(toolkit)
            
            theArgs = list(...)                   # raise.on.dragmotion
            
            if(is.null(spacing))
              spacing = 0
            if (horizontal)
              group <- gtkHBoxNew(spacing=spacing)
            else
              group <- gtkVBoxNew(spacing=spacing)
            
            ## let breath a little
            group$SetBorderWidth(2)

            ## do we pack into a scroll window?
            theArgs = list(...)
            if(use.scrollwindow == TRUE) {
              ## put into a scroll window
              sw = gtkScrolledWindowNew()
              sw$SetPolicy("GTK_POLICY_AUTOMATIC","GTK_POLICY_AUTOMATIC")
              sw$AddWithViewport(group)
              
              obj = new("gGroupRGtk", block=sw, widget=group, toolkit=toolkit)
            } else {
              obj = new("gGroupRGtk", block=group, widget=group, toolkit=toolkit)
            }
            
            
            if(!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE, toolkit=toolkit)
              add(container, obj, ...)
            }

            ## raise if we drag across
            if(!is.null(theArgs$raise.on.dragmotion)) {
              ## we tried Raise and Focus here, but still have bug
              ## with windows causing the drop value to flutter away
              ## after the window is raised. So we cop out and avoid
              ## this on Window

              if(.Platform$OS.type != "windows") {
                ## need drop target before a drag motion!! 
                adddroptarget(obj, handler = function(h,...) {})
                ##              adddropmotion(obj, handler = function(h,...) getWidget(h$obj)$GetWindow()$Raise())
                ## some bug in windows, try focus
                adddropmotion(obj, handler = function(h,...) focus(obj) <- TRUE) ##getWidget(h$obj)$GetParentWindow()$Focus())
              }
            }
            return(obj)
          })


##################################################
## methods

## for gGroup
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gGroupRGtk", value="RGtkObject"),
          function(obj, toolkit, value, ...) {

            theArgs = list(...)
            expand = if(is.null(theArgs$expand)) FALSE else theArgs$expand
  
            getWidget(obj)$packStart(value, expand, TRUE, 0) # expand to fill if TRUE
          })



setReplaceMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gGroupRGtk"),
          function(obj, toolkit, index=NULL, drop=NULL, ..., value) {
            ## adds some breathing room to object
            ## value is pixels
            getWidget(obj)$SetBorderWidth(as.numeric(value))
            return(obj)
          })


setReplaceMethod(".size",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gGroupRGtk"),
          function(obj, toolkit, ...,value) {
            width = value[1]; height = value[2]
            block = obj@block           # use block not widget here in case its a sw
            block$SetSizeRequest(width, height)

            return(obj)
          })

##################################################
## handlers

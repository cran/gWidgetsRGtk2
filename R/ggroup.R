## class in aaaClasses.R
## constructor
setMethod(".ggroup",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   horizontal = TRUE, spacing = 5, container = NULL, ... 
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
            
            obj = new("gGroupRGtk", block=group, widget=group, toolkit=toolkit)

            ## attach to container if there
            if(!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE, toolkit=toolkit)
              add(container, obj)
            }

            ## raise if we drag across
            if(!is.null(theArgs$raise.on.dragmotion)) {
              adddroptarget(obj, handler = function(h,...) {})
              adddropmotion(obj, handler = function(h,...) getWidget(h$obj)$GetWindow()$Raise())
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

##################################################
## handlers

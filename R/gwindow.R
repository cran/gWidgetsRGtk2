setClass("gWindowRGtk",
         contains="gContainerRGtk",
         prototype=prototype(new("gContainerRGtk"))
         )

## constructor
setMethod(".gwindow",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   title="Window", visible=TRUE,
                   width = NULL, height = NULL,
                   handler=NULL, action = NULL,
                   ...
                   ) {
            force(toolkit)
            
            window <- gtkWindowNew("toplevel", show = FALSE)
            ## set default size
            if(!is.null(width)) {
              if(is.null(height)) height = .7*width
              window$SetDefaultSize(width, height)
            }

            
            obj = new("gWindowRGtk",block=window, widget=window, toolkit=toolkit)

            window$SetTitle(title)
            
            if (!is.null(handler)) {
              ## handler can't refer to h$obj, as it is already <invalid>
              ## by the time it gets here.
              id <- addhandlerdestroy(obj, handler=handler, action=action)
            }

            if(visible)
              window$Show()

            return(obj)
          })
##################################################
## Methods

setMethod(".add",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gWindowRGtk", value="RGtkObject"),
          function(obj, toolkit, value, ...) {
            getWidget(obj)$Add(value)
          })


## methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gWindowRGtk"),
          function(obj, toolkit, index=NULL, drop=NULL, ..) {
            getWidget(obj)$GetTitle()
          })

setMethod(".svalue<-",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gWindowRGtk"),
          function(obj, toolkit, index=NULL,..., value) {
            ## set the title
            getWidget(obj)$SetTitle(value)
            return(obj)
          })

## no visible() method
setMethod(".visible<-",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gWindowRGtk"),
          function(obj, toolkit, ..., value) {
            value = as.logical(value)
            if(value == TRUE)
              getWidget(obj)$Show()
            else
              getWidget(obj)$Hide()

            return(obj)
          })


setMethod(".size",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gWindowRGtk"),
          function(obj, toolkit, ...) {
            theSize = getWidget(obj)$GetSize()
            return(unlist(theSize[2:3]))
          })

setMethod(".dispose",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gWindowRGtk"),
          function(obj, toolkit, ...) {
            obj@widget$Destroy()
          })

##################################################
## handlers
## THis intercepts the windowmanager delete-event, destroy does not
setMethod(".addhandlerunrealize",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gWindowRGtk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            theArgs = list(...)
            try(connectSignal(obj@widget,
                          signal="delete-event",
                          f = function(...) {
                            val = handler(...)
                            if(is.logical(val))
                              return(val)
                            else
                              return(TRUE)
                          },
                          data=list(obj=if(!is.null(theArgs$actualobj))
                            theArgs$actualobj
                          else
                            obj, action=action,...),
                          user.data.first = TRUE,
                          
                          after=FALSE),
                silent=TRUE)
          })

setMethod(".addhandlerdestroy",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gWindowRGtk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addHandler(obj, toolkit, signal="destroy", handler, action, ...)
          })

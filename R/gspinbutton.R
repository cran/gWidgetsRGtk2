## Could make spinbutton slider, subclass as methods are identical
setClass("gSpinbuttonRGtk",
         contains="gComponentRGtk",
         prototype=prototype(new("gComponentRGtk"))
         )

setMethod(".gspinbutton",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   from=0,to=10,by=1,value=from,digits=0,
                   handler=NULL, action=NULL,
                   container=NULL, ...) {

            force(toolkit)
                        
            adjustment = gtkAdjustmentNew(value=value, lower=from,
              upper=to,step.incr=by)
            spin = gtkSpinButtonNew(adjustment,climb.rate=0.6, digits=digits)
  
            obj = new("gSpinbuttonRGtk", block=spin, widget=spin, toolkit=toolkit)

            svalue(obj) <- value                  # wasn't working as desired
  

            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj)
            }
            
            if (!is.null(handler))  {
              id = addhandlerchanged(obj, handler, action)
            }
            invisible(obj)
          })

### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gSpinbuttonRGtk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            obj@widget$GetValue()
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitRGtk2",obj="gSpinbuttonRGtk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   obj@widget$SetValue(value)
                   return(obj)
                 })

### handlers
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gSpinbuttonRGtk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            addhandler(obj,"value-changed",handler, action)
          })


setClass("gCheckboxRGtk",
         contains="gComponentRGtk",
         prototype=prototype(new("gComponentRGtk"))
         )

## constructor
setMethod(".gcheckbox",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   text, checked=FALSE,
                   handler=NULL, action=NULL,
                   container=NULL,...) {
            
            check <- gtkCheckButtonNewWithLabel(text)
            check$SetActive(checked)

            obj = new("gCheckboxRGtk",block=check, widget=check,
              toolkit=toolkit)
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj)
            }
            
            
            if (!is.null(handler)) {
              id = addhandler(obj, "toggled",handler, action=action)
            }
  
            invisible(obj)
          })

### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gCheckboxRGtk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            obj@widget$getActive()
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitRGtk2",obj="gCheckboxRGtk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   obj@widget$setActive(value)
                   return(obj)
                 })

## [
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitRGtk2",x="gCheckboxRGtk"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            x@widget[[1]]$GetText()
          })
            
setMethod("[",
          signature(x="gCheckboxRGtk"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitRGtk2",x="gCheckboxRGtk"),
          function(x, toolkit, i, j, ..., value) {
            x@widget[[1]]$SetText(value)
            return(x)
          })

setReplaceMethod("[",
                 signature(x="gCheckboxRGtk"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

### no method to change the value of text???

### handlers
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gCheckboxRGtk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            addhandler(obj, "toggled", handler, action=action,...)
          })

setClass("gButtonRGtk",
         contains="gComponentRGtk",
         prototype=prototype(new("gComponentRGtk"))
         )


setMethod(".gbutton",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   text="", handler=NULL, action=NULL, container=NULL,...
                   ) {
            force(toolkit)
            
            iconname = getstockiconname(tolower(text))
            if(!is.na(iconname)) {
              button <- gtkButtonNewFromStock(iconname)
              button$Show()
            } else {
              button <- gtkButtonNewWithLabel(text)
            }

            obj = new("gButtonRGtk",
              block=button, widget=button, toolkit=toolkit)

            ## add to container
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE, toolkit=toolkit)
              add(container, obj)
            }

            ## add handler
            if (!is.null(handler)) {
              id = addhandlerclicked(obj,handler,action)
            }
            
            invisible(obj)
          })
          
### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gButtonRGtk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            return(obj@widget$GetLabel())
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitRGtk2",obj="gButtonRGtk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   button = obj@widget
                   image = gtkImageNew()
                   if(is.gImage(value)) {
                     filename = tag(value,"filename")
                     if(!is.na(getstockiconname(filename))) {
                       ## stock
                       image$SetFromStock(getstockiconname(filename),size=obj$size)
                     } else {
                       image$SetFromFile(filename)
                     }
                     button$SetImage(image)
                   } else if(is(value,"gLabelRGtk")) {
                     button$SetLabel(value@widget)
                   } else {
                     button$SetLabel(value)
                   }
                   return(obj)
                 })

### handlers
setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gButtonRGtk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            addhandler(obj,"pressed",handler, action)
          })
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gButtonRGtk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            addhandlerclicked(obj, handler, action)
          })

## for popup menu
setMethod(".addpopupmenu",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gButtonRGtk"),
          function(obj, toolkit, menulist, action=NULL, ...) {
            addPopupMenuWithSignal(obj, toolkit, menulist, signal="clicked",...)
})

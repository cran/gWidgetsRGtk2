setClass("gButtonRGtk",
         contains="gComponentRGtk",
         prototype=prototype(new("gComponentRGtk"))
         )


setMethod(".gbutton",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   text="", border=TRUE, handler=NULL, action=NULL, container=NULL,...
                   ) {
            force(toolkit)
            
            iconname = getstockiconname(tolower(text))
            if(!is.na(iconname)) {
              button <- gtkButtonNewFromStock(iconname)
              button$Show()
            } else {
              button <- gtkButtonNewWithLabel(text)
            }

            ## look for border request
            if(border == FALSE) {
              button$SetRelief(as.integer(2))
            }


            obj = as.gWidgetsRGtk2(button)
#            obj = new("gButtonRGtk",
#              block=button, widget=button, toolkit=toolkit)

            ## add to container
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE, toolkit=toolkit)
              add(container, obj,...)
            }

            ## add handler
            if (!is.null(handler)) {
              id = addhandlerclicked(obj,handler,action)
            }
            
            invisible(obj)
          })

## coerce gtk object
as.gWidgetsRGtk2.GtkButton <- function(widget,...) {
  button = widget
  obj = new("gButtonRGtk",
    block=button, widget=button, toolkit=guiToolkit("RGtk2"))
  return(obj)
}

## constructor for actions
## proper call is gbutton(action = gaction_instnace, cont = ...)
## setMethod(".gbutton",
##           signature(toolkit="guiWidgetsToolkitRGtk2",
##                     text = "guiComponent"),
##           function(toolkit,
##                    text="", border=TRUE, handler=NULL, action=NULL, container=NULL,...
##                    ) {
##             .gbutton(toolkit, "", border, handler, text@widget, container, ...)
##           })

setMethod(".gbutton",
          signature(toolkit="guiWidgetsToolkitRGtk2",
                    action = "guiComponent"),
          function(toolkit,
                   text="", border=TRUE, handler=NULL, action=NULL, container=NULL,...
                   ) {
            .gbutton(toolkit,  "", border, handler, action@widget, container, ...)
          })

setMethod(".gbutton",
          signature(toolkit="guiWidgetsToolkitRGtk2",
                    action = "gActionRGtk"),
          function(toolkit,
                   text="", border=TRUE, handler=NULL, action=NULL, container=NULL,...
                   ) {
            force(toolkit)

            action <- getWidget(action)
            
            button <- gtkButton()
            obj <- new("gButtonRGtk",
                       block=button, widget=button, toolkit=guiToolkit("RGtk2"))

            action$connectProxy(button)
            ## icon
            icon <- action['stock-id']
            if(!is.null(icon)) {
              image <- action$createIcon(GtkIconSize[4])
              button$setImage(image)
            }

            if(!is.null(container)) {
              if(is.logical(container) && container) {
                container <- gwindow()
                add(container, obj)
              } else {
                add(container, obj, ...)
              }
            }
                
            
            return(obj)
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
            addhandler(obj,"clicked",handler, action)
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

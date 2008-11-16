## reusuabel chunk of code
setClass("gActionRGtk",
         contains="gComponentRGtk",
         prototype=prototype(new("gComponentRGtk"))
         )


setMethod(".gaction",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   label,
                   tooltip = NULL,
                   icon = NULL,
                   key.accel = NULL,
                   handler = NULL, action = NULL, 
                   ...) {
            
            force(toolkit)

            if(!is.null(icon))
              icon <- getstockiconname(icon)
            
            act <- gtkAction(name = make.names(label),
                             label = label,
                             tooltip = tooltip,
                             stock.id = icon)


            obj = new("gActionRGtk", block=act, widget=act, toolkit=toolkit)

            ## add for later use
            ## should be defined when used in a menu bar.
            tag(obj,"key.accel") <- key.accel
            
            if(!is.null(handler))
              addHandlerChanged(obj, handler, action)
            
            return(obj)
          })

## svalue -- get label
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gActionRGtk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            widget <- getWidget(obj)
            return(widget['label'])
          })



## svalue<- set label
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitRGtk2",obj="gActionRGtk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   cat("svalue<- not defined\n")

                   ## this sets label, but does not update GUI
                   ##                   widget <- getWidget(obj)
                   #                   widget['label'] <- value

                   return(obj)
                 })

## enabled -- inherited
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gActionRGtk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            widget <- getWidget(obj)

            ID <- gSignalConnect(widget, signal="activate",
                           f = handler,
                           data = list(action = action),
                           user.data.first = TRUE)

            return(ID)
          })

                             
## helper functions

.isgAction <- function(lst) {
  is(lst,"guiComponent") && is(lst@widget, "gActionRGtk") ||
  is(lst,"gActionRGtk")
}

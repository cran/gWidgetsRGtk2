setClass("gImageRGtk",
         representation(filename="character"),
         contains="gComponentRGtk",
         prototype=prototype(new("gComponentRGtk"))
         )

## image use 

setMethod(".gimage",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   filename = "", dirname="",
                   size="",
                   handler=NULL, action=NULL, 
                   container=NULL, ...) {

            force(toolkit)
            
            ## size if for Stock: one of MENU, SMALL_TOOLBAR, LARGE_TOOLBAR, BUTTON, DND, DIALOG
            image = gtkImageNew()
            if(dirname == "stock") {
              if(is.null(size)) {
                size =  GtkIconSize["menu"]
              } else {
                size = switch(toupper(size),
                  "MENU"= GtkIconSize["menu"],
                  "SMALL_TOOLBAR"= GtkIconSize["small-toolbar"],
                  "LARGE_TOOLBAR"= GtkIconSize["large-toolbar"],
                  "BUTTON"= GtkIconSize["button"],
                  "DND"= GtkIconSize["dnd"],
                  "DIALOG"= GtkIconSize["dialog"],
                  GtkIconSize["menu"]
                  )
              }
              filename = getstockiconname(filename) # in icons.R
              image$SetFromStock(filename,size=size)
            } else {
              if(nchar(dirname) >0 )
                filename = Paste(dirname,"/",filename) # / works for windows and unix?
              if(!missing(filename) && file.exists(filename))
                image$SetFromFile(filename)
            }
            
            ## pack into an event box so that we can get signals
            evb = gtkEventBoxNew()
            evb$Add(image)
            
            obj = new("gImageRGtk", block=evb, widget=image, toolkit=toolkit,
              filename=filename)

            tag(obj,"doStock") <- dirname=="stock"
            if(dirname == "stock") {
              tag(obj,"size") <- size
            }
            if(!is.null(handler)) {
              id = addhandlerclicked(obj, handler=handler, action=action)
            }

            ## attach?
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj,...)
            }
            
            invisible(obj)
          })
          
### methods
### need to fuss with evb vs. label
setMethod(".adddroptarget",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gImageRGtk"),
          function(obj, toolkit, targetType="text", handler=NULL, action=NULL, ...) {
            ## problem -- we want to add drop target to obj@block evb,
            ## but have handler refer to obj@widgeg=label. 
            addDropTarget(obj@block, toolkit, targetType, handler, action, overrideobj=obj)
            
          })
          
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gImageRGtk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            ## return name?
            return(obj@filename)
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitRGtk2",obj="gImageRGtk"),
                 function(obj, toolkit, index=NULL,  ..., value) {
                   ## value is a full filename
                   if(value != "" & file.exists(value))  {
                     obj@widget$SetFromFile(value)
                     obj@filename=value
                   } else if(value != "" & tag(obj,"doStock")) {
                     iconname = getstockiconname(value)
                     obj@widget$SetFromStock(iconname,size=tag(obj,"size"))
                   } else {
                     cat("File",value,"does not exist nor is a stock name.\n")
                   }
                   return(obj)
                 })


### handlers
## put onto block
setMethod(".addHandler",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gImageRGtk"),
          function(obj, toolkit,
                   signal, handler, action=NULL, ...) {
            .addHandler(obj@block, toolkit, signal, handler, action, ...)
          })


setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gImageRGtk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            addhandler(obj,"button-press-event", handler=handler, action=action)
          })

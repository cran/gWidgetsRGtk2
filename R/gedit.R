## class defined in aaaClasses for inheritance
## constructor
setMethod(".gedit",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   text="", width=25,
                   coerce.with = NULL, 
                   handler=NULL, action=NULL,
                   container=NULL,
                   ...
                   ) {
            
            force(toolkit)

            if (is.null(text)) text<-""
  

            entry <- gtkEntryNew()

            obj <- as.gWidgetsRGtk2(entry)

            tag(obj, "coerce.with") <- coerce.with
  
            ## this adds completion fields to this widget. To *add* to the list
            ## of values that can be completed use gEditobject[]<- values
            
            completion = gtkEntryCompletionNew()
            ## set model
            ##  model = gtkListStoreNew("gchararray")
            ## this caps out at 1000 -- is this a speed issue?
#            model = rGtkDataFrame(hack.as.data.frame(matrix("",nrow=1000,ncol=1)))
            model <- rGtkDataFrame(data.frame(character(1000),stringsAsFactors=FALSE))
            completion$SetModel(model)
            completion$SetTextColumn(0)           # Columns count from 0 -- not 1

            ## set properties
            if("inline-completion" %in% names(completion))
              completion['inline-completion'] <- TRUE
            if("inline-selection" %in% names(completion))
              completion['inline-selection'] <- TRUE

            entry$SetCompletion(completion)
            
            ##  entry$setMaxLength(max(width,length(unlist(strsplit(text,"")))))
            entry$SetText(text)
            tag(obj,"value")  <- text        # store for later
            tag(obj,"completion") <- completion

            ## width -- ths sets minimum -- it ay expand to fill space
            if(!is.null(width))
              entry$SetSizeRequest(as.numeric(width) * 8, -1)
            
            
  
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow()
              add(container, obj,...)
            }

            if (!is.null(handler)) {
              id = addhandlerchanged(obj,handler,action)
            }
            
            invisible(obj)
            
            
          })


as.gWidgetsRGtk2.GtkEntry <- function(widget, ...) {

  obj = new("gEditRGtk",block=widget, widget=widget,
    toolkit=guiToolkit("RGtk2"))

  ## Drag and drop
  dropHandler =   function(h,...) {
    theName = id(h$dropdata)
    if(is.null(theName)) theName == ""
    svalue(h$obj) <- ""                 # funny, why isn't this svalue(h$obj)<-theName?
    ## override value -- in case it is a widget
    tag(h$obj, "value") <- h$dropdata
    return(TRUE)
  }
  handler.ids = list()
  id = adddroptarget(obj, targetType="object",handler=dropHandler)
  handler.ids[['dnd']] = id
  

  return(obj)
}

## methods
setMethod("svalue", signature(obj="GtkEntry"),
          function(obj, index=NULL, drop=NULL, ...) {
            .svalue(obj,guiToolkit("RGtk2"), index, drop, ...)
          })
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gEditRGtk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            val = obj@widget$getText()
            if(!is.null(tag(obj,"coerce.with")))
              val = do.call(tag(obj,"coerce.with"), list(val))

            return(val)
          })
## trouble here -- no coerce.with info available in obj
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="GtkEntry"),
          function(obj, toolkit, index=NULL, drop=NULL,  ...) {
            val = obj$getText()
            return(val)
          })

## svalue<-
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitRGtk2",obj="gEditRGtk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   obj@widget$SetText(value)
                   tag(obj, "value") <- value
                   return(obj)
          })
## want to replace "value" but can't
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitRGtk2",obj="GtkEntry"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   obj$SetText(value)
                   return(obj)
          })


setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitRGtk2",x="gEditRGtk"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
          })
            
setMethod("[",
          signature(x="gEditRGtk"),
          function(x, i, j, ..., drop=TRUE) {
            store = obj@widget$GetCompletion()$GetModel()
            nrows = dim(store)[1]
            if(missing(i))
              i = 1:nrows

            return(store[i , ])
            
          })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitRGtk2",x="gEditRGtk"),
          function(x, toolkit, i, j, ..., value) {
            store = x@widget$GetCompletion()$GetModel()
            nrows = dim(store)[1]
            n =length(value)
            if(n > nrows)
              values = values[1:nrows]            # truncate
            if(missing(i))
              i = 1:n
            store[i , ]<- value

            ## all done
            return(x)
          })

setReplaceMethod("[",
                 signature(x="gEditRGtk"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

##################################################
## handlers

setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gEditRGtk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            f = function(h,widget,event,...) {
              keyval = event$GetKeyval()
              if(keyval == GDK_Return) {
                handler(h,widget,event,...)
                return(TRUE)
              } else {
                return(FALSE)
              }
            }
            id = addhandler(obj,signal="key-release-event",handler=f, action=action)

            return(id)
          })

setMethod(".addhandlerkeystroke",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gEditRGtk"),
          function(obj,toolkit, handler=NULL, action=NULL,...) {
            widget <- getWidget(obj)
            ID <-
              gSignalConnect(widget,signal = "key-press-event",
                             f = function(d,widget,event,...) {
                               h <- list(obj=d$obj,action=d$action)
                               key <- event$GetString()
                               h$key <- key
                               if(!is.null(d$handler) &&
                                  is.function(d$handler))
                                 d$handler(h,...)
                               return(FALSE) # propogate
                             },
                             user.data.first = TRUE,
                             data = list(obj=obj,handler=handler, action=action)
                             )
            return(ID)
##            addhandler(obj,"changed",handler,action)
          })


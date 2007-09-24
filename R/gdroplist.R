## editable has entry widget that can be edited
setClass("gDroplistRGtk",
         contains="gComponentRGtk",
         prototype=prototype(new("gComponentRGtk"))
         )
setMethod(".gdroplist",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   items, selected = 1, # use 0 for blank
                   editable=FALSE,
                   coerce.with = NULL,
                   handler=NULL, action=NULL,
                   container=NULL,
                   ...               # do.quote = TRUE for quote of answer
                   ) {

            force(toolkit)
            
            ## items must be a vector here
            items = as.vector(items)              # undoes factor
            items = unique(items)                 # unique
            
            theArgs = list(...)

            ## keep this, but don't advertise
            if(!is.null(theArgs$do.quote)) {
              coerce.with = function(x) paste("'",x,"'",sep="",collapse="")
            }
            
            
            ## droplist is not happy with datastore class
            ## droplist was not happy with numeric vectors! seems strange
            
            if(editable) {
              store = gtkTreeStoreNew("gchararray")
              combo <- gtkComboBoxEntryNewWithModel(store, 0)
              entry = combo$GetChild()
              entry$SetEditable(TRUE)
              ## add in drop target to entry
              ## we can't pass in obj here, so we find via scoping
              dropHandler =   function(h,...) {
                theName = id(h$dropdata)
                ## override value -- in case it is a widget
                tag(obj, "value") <- h$dropdata # find obj via scoping
                svalue(obj) <- "" 
                return(TRUE)
              }
              .adddroptarget(entry, toolkit, targetType="object",handler=dropHandler)
#              .adddroptarget(entry, toolkit, targetType="object")
              
            } else {
              ##    store = gtkTreeStoreNew(RtoGObjectConversion(items))
              store = gtkTreeStoreNew("gchararray")
              combo <- gtkComboBoxNewWithModel(store,0)
              cell = gtkCellRendererTextNew()
              combo$PackStart(cell, TRUE)
              combo$AddAttribute(cell,"text",0)
            }

            
            obj = new("gDroplistRGtk",block=combo,widget=combo, toolkit=toolkit)

#            if(editable)
#              obj@widget = combo$GetChild()
            
            tag(obj,"store") <- store
            tag(obj,"combo") <- combo
            tag(obj,"editable") <- editable
            tag(obj,"items") <- items
            tag(obj, "coerce.with") = coerce.with
            
            ## load up the store
            if(length(items) > 0) {
              obj[] <- items
            }
            ## should I have actiirst be blank? Use 0 (to make -1) for this
            combo$Show()
            combo$SetActive(selected-1)
            
            ## add drophandler -- switch if drop matches
            adddroptarget(obj, handler = function(h,...) {
              name = id(h$dropdata)
              theValues = obj[]
              if(!is.na(name) && !is.null(name) && name %in% theValues) {
                svalue(obj) <- name
              }
            })
            
            
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj,...)
            }
            
            if (!is.null(handler)) {
              id <- addhandlerchanged(obj, handler, action)
              tag(obj, "handler.id") <- id
            }
            
            invisible(obj)
          })
          
### methods
## value is for getting/setting the selected value
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gDroplistRGtk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            ## add in an as.numeric flag, getwidget when editable
            theArgs = list(...)         # deprecated
            coerce.with = tag(obj, "coerce.with")
  
            ## do things depending on whether there is an entry or not
            ## if editable, then entry is widget and combo may be found by tag("combo")
            if(tag(obj,"editable")) {
              if(is.null(index) || index==FALSE) {
##                entry = obj@widget      # entry is widget
                entry = obj@widget$GetChild()
                if(!is.null(theArgs$getwidget)) {
                  cat("DEBUG: getwidget is deprecated\n")
                }
                if(!is.null(theArgs$as.numeric)) {
                  cat("DEBUG: as.numeric as an argument is deprected. Use coerce.with\n")
                }
                
                ## else we return text
                val = entry$GetText()

                coerce.with<-tag(obj,"coerce.with")
                if(is.null(coerce.with))
                  return(val)
                else if(is.function(coerce.with))
                  return(coerce.with(val))
                else if(is.character(coerce.with))
                  return(do.call(coerce.with,list(val)))
                else
                  warning("Error: coerce.with is a function or character")
              } else {
                ## return the index or NA
                combobox = tag(obj,"combo") # of obj@widget$GetParent()
                active = combobox$GetActive()
                if(active < 0)
                  return(NA)
                else
                  return(active+1)
              }
            } else {
              ## from pygtk manual
              combobox = obj@widget
              model = combobox$GetModel()
              selected = combobox$GetActive()
              items = obj[]

              ## selected is the index. It is 0 based
              if(selected < 0) {
                return(NULL)                      # none selected
              } else {
                ## do we return the index?
                if(!is.null(index) && index==TRUE) {
                  return(selected + 1)
                } else {
                  val = items[selected+1]
                  coerce.with<-tag(obj,"coerce.with")
                  if(is.null(coerce.with))
                    return(val)
                  else if(is.function(coerce.with))
                    return(coerce.with(val))
                  else if(is.character(coerce.with))
                    return(do.call(coerce.with,list(val)))
                  else
                    warning("Error: coerce.with is a function or character")
                }
              }
            }
          })

## set the displayed value to value
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitRGtk2",obj="gDroplistRGtk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   theArgs = list(...)

                   ##  if editable do differently
                   if(tag(obj,"editable")) {
                     if(is.null(index) || index == FALSE)  {
##                       entry = obj@widget
                       entry = obj@widget$GetChild()
                       entry$SetText(value)              # gtk Call
                     } else {
                       ## set the index
                       combobox = tag(obj,"combo") # or obj@widget$GetParent()
                       combobox$SetActive(value-1)
                     }
                   } else {
                     combobox = obj@widget
                     items = obj[]
                     if(!is.null(index)) { # either value or index is non-null
                       combobox$SetActive(value-1)
                     } else {
                       if(any(value == items)) {
                         combobox$SetActive(min(which(value==items)) - 1)
                       } else {
                         combobox$AppendText(value)
                         combobox$SetActive(length(items))
                       }
                     }
                   }
                   return(obj)
                 })

## the methods [ and [<- refer to the pre-defined values in the drop list.
## [
setMethod("[",
          signature(x="gDroplistRGtk"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitRGtk2",x="gDroplistRGtk"),
          function(x, toolkit, i, j, ..., drop=TRUE) {

            items = tag(x,"items")
            if(missing(i))
              return(items)
            else
              return(items[i])
          })


## replaces the values in droplist
## values is a vector of values -- not a dataframe
#set.values.gDropList = function(obj, values, ...) {
setReplaceMethod("[",
                 signature(x="gDroplistRGtk"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitRGtk2",x="gDroplistRGtk"),
          function(x, toolkit, i, j, ..., value) {

            olditems = tag(x,"items")
            if(missing(i)) {
              items = value
              n = length(items)
              i = 1:n
            } else {
              items = olditems
              items[i] = value
            }
            ## update items
            tag(x,"items") <- items
            
            ## now update widget
            store = tag(x,"store")
            store$Clear()
            n = length(items)
            
            if(n  > 0)  {
              for(j in 1:n) {
                iter = store$Append(parent=NULL)
                store$SetValue(iter$iter, column = 0, items[j])
              }
            }
            
            return(x)
          })

###################################################
  
### handlers
setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gDroplistRGtk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            addhandler(obj,"changed",handler,action)
          })

## want changed by activate -- or arrow for editable -- not keystroke
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gDroplistRGtk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            if(tag(obj,"editable")) {
              id = addhandler(obj,"changed",handler = function(h,...) {
                if(obj@widget$GetActive() != -1) {
                  handler(h,...)
                }
              },action)  # clicked -- not keystroke
              ## put handler on entry too
              try(connectSignal(obj@widget$GetChild(),
                            signal="activate",
                            f=handler,
                            data=list(obj=obj, action=action,...),
                            user.data.first = TRUE,
                            after = FALSE),
                  silent=TRUE)
              invisible(id)
            } else {
              addhandler(obj,"changed",handler,action)
            }
          })

setMethod(".addhandlerkeystroke",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gDroplistRGtk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            ## put handler on entry 
            try(connectSignal(obj@widget$GetChild(),
                          signal="changed",
                          f=handler,
                          data=list(obj=obj, action=action,...),
                          user.data.first = TRUE,
                          after = FALSE),
                silent=TRUE)
          })

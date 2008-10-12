## build widget based on gcheckbox
setClass("gCheckboxgroupRGtk",
         contains="gComponentRGtk",
         prototype=prototype(new("gComponentRGtk"))
         )

setMethod(".gcheckboxgroup",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   items, checked = FALSE,
                   horizontal=FALSE, 
                   handler = NULL, action = NULL, container = NULL, ...) {

            force(toolkit)

            if(missing(items))
              stop(gettext("Need items to be defined"))

            if(is.data.frame(items))
              items <- items[, 1, drop=TRUE]

            
            checked = rep(checked, length(items))

            group = ggroup(horizontal = horizontal, container=container, ...)
            
            lst = list()
            n = length(items)
            for(i in 1:n) {
              newItem = gcheckbox(items[i], checked=checked[i])
              lst[[ as.character(items[i]) ]] = newItem
              add(group, newItem)
            }
  

            ## make combination widget with all the values
            obj = new("gCheckboxgroupRGtk",block=group, widget=group, toolkit=toolkit)
  
            tag(obj, "items") <- items
            tag(obj, "itemlist") <- lst
            tag(obj, "handlerList") <- list()
            tag(obj, "handlerCount") <- 0

            ## add handler
            if(!is.null(handler))
              ID = addhandlerchanged(obj, handler=handler, action=action, ...)
            
            return(obj)
          })


### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gCheckboxgroupRGtk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            theArgs = list(...)
            
            lst = tag(obj, "itemlist")
            vals = sapply(lst, svalue)         # logicals
            
            if(!is.null(index) && index == TRUE) {
              return(which(vals))
            } else {
              return(tag(obj,"items")[vals])
            }
          })

## toggles state to be T or F
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitRGtk2",obj="gCheckboxgroupRGtk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   if(is.data.frame(value))
                     value <- value[,1,drop=TRUE]
                   
                   items = tag(obj,"items")
                   lst = tag(obj,"itemlist")
                   values = rep(value, length.out=length(items)) ## recycle

                   sapply(1:length(items), function(i) svalue(lst[[i]]) <- values[i])
                   
                   return(obj)
                 })

## [ and [<- refer to the names -- not the TF values

setMethod("[",
          signature(x="gCheckboxgroupRGtk"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitRGtk2",x="gCheckboxgroupRGtk"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            items = tag(x,"items")
            if(missing(i))
              return(items)
            else
              return(items[i])
          })

## assigns names
setReplaceMethod("[",
                 signature(x="gCheckboxgroupRGtk"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitRGtk2",x="gCheckboxgroupRGtk"),
          function(x, toolkit, i, j, ..., value) {
            items = tag(x,"items")
            lst = tag(x,"itemlist")
            n = length(items)

            ## if i is missing, we can relabel if length the same
            ## otherwise we delete and start again
            ## We will need to add the handlers back
            
            if(missing(i)) {
              if(length(value) != n) {
                group <- x@widget
                ## delete
                sapply(rev(lst), function(child)
                       delete(group, child))
                ## add
                lst <- list()
                for(i in 1:length(value)) {
                  newItem = gcheckbox(value[i], checked=FALSE)
                  lst[[ as.character(value[i]) ]] = newItem
                  add(group, newItem)
                }
                tag(x, "items") <- value
                tag(x, "itemlist") <- lst

                ## addhandlers
                handlerList <- tag(x,"handlerList")
                if(length(handlerList) > 0) {
                  for(j in handlerList) {
                    sapply(lst, function(i)
                           addhandlerchanged(i,
                                             handler=j$handler, action=j$action,
                                             actualobj=x, ...))
                  }
                }
                ## return
                return(x)
              } else {
                ## back to our regularly scheduled programming
                i = 1:n
              }
            }
  
            if(is.logical(i))
              i = which(i)

            items[i] = value
            sapply(1:n, function(i) 
                   lst[[i]][] <- items[i]
                   )
            tag(x,"items") <- items
            tag(x,"itemlist") <- lst
  
             return(x)
          })

## length
setMethod(".length",
          signature(toolkit="guiWidgetsToolkitRGtk2",x="gCheckboxgroupRGtk"),
          function(x,toolkit) {
            length(tag(x,"items"))
          })


## handlers
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gCheckboxgroupRGtk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            handlerList <- tag(obj,"handlerList")
            ct <- tag(obj,"handlerCount")
            ID <- as.character(ct+1)
            handlerList[[ID]] <- list(
                                      handler=handler,
                                      action=action
                                      )
            tag(obj,"handlerList") <- handlerList
            ## now call on each
            lst = tag(obj,"itemlist")
            IDs <- lapply(lst, function(i)
                   addhandlerchanged(i,handler=handler, action=action, actualobj=obj, ...))
            return(IDs)
          })
          

setMethod(".removehandler",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gCheckboxgroupRGtk"),
          function(obj, toolkit, ID=NULL, ...) {
            tag(obj,"handlerList") <- NULL
            lst <- tag(obj,"itemlist")
            sapply(1:length(lst), function(i)
                   removehandler(lst[[i]], ID[[i]])
                 )
          })

setMethod(".blockhandler",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gCheckboxgroupRGtk"),
          function(obj, toolkit, ID=NULL, ...) {

            lst <- tag(obj,"itemlist")
            sapply(1:length(lst), function(i)
                   blockhandler(lst[[i]], ID[[i]])
                   )
          })

setMethod(".unblockhandler",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gCheckboxgroupRGtk"),
          function(obj, toolkit, ID=NULL, ...) {

            lst <- tag(obj,"itemlist")
            sapply(1:length(lst), function(i)
              unblockhandler(lst[[i]], ID[[i]])
            )
          })


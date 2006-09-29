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
            
            if(missing(items) || length(items) == 0)
              stop("Need items to be a vector of items")
            
            checked = rep(checked, length(items))

            group = ggroup(horizontal = horizontal, container=container)
            
            lst = list()
            n = length(items)
            for(i in 1:n) {
              newItem = gcheckbox(items[i], checked=checked[i],
                handler=handler, action=action)
              lst[[ as.character(items[i]) ]] = newItem
              add(group, newItem)
            }
  

            ## make combination widget with all the values
            obj = new("gCheckboxgroupRGtk",block=group, widget=group, toolkit=toolkit)
  
            tag(obj, "items") <- items
            tag(obj, "itemlist") <- lst
            
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
              return(vals)
            } else {
              return(tag(obj,"items")[vals])
            }
          })

## toggles state to be T or F
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitRGtk2",obj="gCheckboxgroupRGtk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   items = tag(obj,"items")
                   lst = tag(obj,"itemlist")
                   values = rep(value, length.out=length(items))

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
            items = tag(obj,"items")
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

            if(missing(i))
              i = 1:length(items)
  
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

## handlers

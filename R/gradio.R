setClass("gRadioRGtk",
         contains="gComponentRGtk",
         prototype=prototype(new("gComponentRGtk"))
         )

## constructor
setMethod(".gradio",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   items, selected=1, horizontal=FALSE,
                   handler=NULL, action=NULL,
                   container=NULL,       
                   ...
                   ) {


            if (length(items)<2)
              stop("Radio button group makes sense only with at least two items.")
            
            button <- gtkRadioButtonNewWithLabel(NULL, items[1])
            buttonList = list(button)
            for (i in 2:length(items))  {
              button <- gtkRadioButtonNewWithLabelFromWidget(button, items[i])
              buttonList[[i]] = button
            }
            buttons <- button$getGroup()
            
            ## ordering is funny
            selected = length(items) + 1 - selected
            buttons[[selected]]$setActive(TRUE)
            
            if (horizontal)
              box <- gtkHBoxNew()
            else box <- gtkVBoxNew()
            
            for (button in buttons)
              box$packEnd(button, TRUE, TRUE, 0)
            

            
            
            obj = new("gRadioRGtk",block=box, widget=box, toolkit=toolkit)

            tag(obj, "buttons") <- buttons
            tag(obj, "buttonList") <- buttonList
            tag(obj, "items") <- items

            ## do we add to the container?
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE, toolkit=obj@toolkit)
              add(container,  obj)
            }
  
            ## add handler
            if(!is.null(handler))
              addhandlerchanged(obj, handler, action)

            
            invisible(obj)
          })

## methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gRadioRGtk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {

            index = ifelse(is.null(index),FALSE,as.logical(index))

            items = tag(obj,"items")
            buttonList = tag(obj,"buttonList")

            for(i in 1:length(buttonList)) {
              if(buttonList[[i]]$GetActive()) {
                return(ifelse(index, i, items[i]))
              }
  }
  return(NA)

          })

## svalue<-
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitRGtk2",obj="gRadioRGtk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   
                   
                   items = tag(obj,"items")
                   buttonList = tag(obj,"buttonList")
                   
                   if(!is.null(index) && index==TRUE) {
                     buttonList[[as.numeric(value)]]$SetActive(TRUE)
                   } else {
                     if(value %in% items) {
                       whichIndex = min(which(value == items))
                       buttonList[[whichIndex]]$SetActive(TRUE)
                     } else {
                       warning(Paste("This value",value,"is not among the items"))
                     }
                   }


                   return(obj)
          })


setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitRGtk2",x="gRadioRGtk"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            items = tag(x, "items")
            if(missing(i))
              items
            else
              items[i]
          })
            
setMethod("[",
          signature(x="gRadioRGtk"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitRGtk2",x="gRadioRGtk"),
          function(x, toolkit, i, j, ..., value) {
            items = tag(x, "items")
            n = length(items)
            
            ## set items
            if(missing(i))
              items <- value
            else
              items[i] <- value
            
            rep(items, length.out=n)
            tag(x,"items")<-items
            
            items = rev(items)                    # buttons wierness
            ## update widget
            buttons = tag(x, "buttons")
            sapply(1:length(buttons), function(i)
                   buttons[[i]][[1]]$SetText(items[i])
                   )
            
            ## all done
            return(x)
          })

setReplaceMethod("[",
                 signature(x="gRadioRGtk"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

##################################################
## handlers

setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gRadioRGtk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            buttons = tag(obj, "buttons")
            IDs = sapply(buttons, function(x) 
              .addHandler(x,toolkit,    # use . function here -- x doesn't have @toolkit
              signal="toggled",
              handler=handler,
              action=action)
              )

            handler.ID = tag(obj, "handler.id")
            if(is.null(handler.ID))
              handler.ID =list()
            for(i in 1:length(IDs))
              handler.ID[[length(handler.ID)+1]] = IDs[[i]]
            tag(obj, "handler.id", replace=FALSE) <- handler.ID

            invisible(IDs)
          })



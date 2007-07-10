setClass("gPanedgroupRGtk",
         contains="gContainerRGtk",
         prototype=prototype(new("gContainerRGtk"))
         )

## TODO: method obj[1 or 2 ] <- replacewidget
setMethod(".gpanedgroup",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   widget1, widget2, horizontal=TRUE, container=NULL, ...) {
            ## add a paned group
            
            force(toolkit)
            
            if(horizontal) {
              panedWindow = gtkHPanedNew()
            } else {
              panedWindow = gtkVPanedNew()
            }
            
            ## left or right *or* top or bottom
            leftgroup = ggroup()
            rightgroup = ggroup()
            
            panedWindow$Pack1(leftgroup@widget@block)#, resize=FALSE, shrink=FALSE)
            panedWindow$Pack2(rightgroup@widget@block)#, resize=FALSE, shrink=FALSE)

            obj = new("gPanedgroupRGtk", block=panedWindow, widget=panedWindow,
              toolkit=toolkit)

            tag(obj,"leftgroup") <- leftgroup
            tag(obj,"rightgroup") <- rightgroup

           
            if(!missing(widget1)) add(obj, widget1)
            if(!missing(widget2)) add(obj, widget2)
            
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj, ...)
            }
            
            return(obj)
          })

  
## add -- use this rather than at construction time
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gPanedgroupRGtk", value="gWidgetRGtk"),
          function(obj, toolkit, value, ...) {
            ctr = tag(obj,"ctr")
            if(is.null(ctr))
              ctr = 0

            if(ctr == 0) {
              add(tag(obj,"leftgroup"), value, expand=TRUE)
              ctr = 1
            } else if(ctr ==1) {
              add(tag(obj,"rightgroup"), value, expand=TRUE)
              ctr = 2
            } else {
              cat("Can only add two widgets to a gpanedgroup\n")
            }
            tag(obj,"ctr") <- ctr
            
          })

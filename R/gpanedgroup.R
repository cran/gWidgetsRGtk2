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
            
            if(horizontal) {
              panedWindow = gtkHPanedNew()
            } else {
              panedWindow = gtkVPanedNew()
            }
            
            ## left or right *or* top or bottom
            leftgroup = ggroup(); add(leftgroup, widget1, expand=TRUE)
            rightgroup = ggroup(); add(rightgroup, widget2, expand=TRUE)
            
            panedWindow$Pack1(leftgroup@widget@block)#, resize=FALSE, shrink=FALSE)
            panedWindow$Pack2(rightgroup@widget@block)#, resize=FALSE, shrink=FALSE)

            obj = new("gPanedgroupRGtk", block=panedWindow, widget=panedWindow,
              toolkit=toolkit)

            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj)
            }
            
            return(obj)
          })

  

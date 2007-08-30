## expander group, like a group, only expands, contracts if requested
## inherits from ggroup, see ggroup's arguments: horizontal, spacing, container
setClass("gExpandgroupRGtk",
         contains="gGroupRGtk",
         prototype=prototype(new("gGroupRGtk"))
         )

setMethod(".gexpandgroup",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   text="", markup=FALSE, horizontal=TRUE,
                   handler=NULL, action=NULL,
                   container = NULL, ...){

            force(toolkit)
            
            expander = gtkExpanderNew()
            if(markup)
              expander$SetUseMarkup(TRUE)
            if(text != "")
              expander$SetLabel(text)

            theArgs = list(...)
            
            group = ggroup(horizontal=horizontal, ...)
            expander$Add(getBlock(group)) # down from guiWidget to gWidgetRGtk

            ## we put widget=group here to get gGroup methods, but
            ## must be careful below to use "block" when referring to expander
            obj = new("gExpandgroupRGtk",block=expander,widget=getWidget(group),
              toolkit=toolkit)

            if(!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)

              if(!is.null(theArgs$expand) && theArgs$expand)
                add(container,obj,expand=TRUE)
              else
                add(container,obj)
            }
            
            if(!is.null(handler)) {
              id = addhandlerchanged(obj, handler, action)
            }
            invisible(obj)
          })

## methods

## value refers to label
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gExpandgroupRGtk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            obj@block$GetLabel()        # not @widget@
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitRGtk2",obj="gExpandgroupRGtk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   obj@block$SetLabel(value)
                   return(obj)
                 })

## control expand/close with logical
setReplaceMethod(".visible",
                 signature(toolkit="guiWidgetsToolkitRGtk2",obj="gExpandgroupRGtk"),
                 function(obj, toolkit, ..., value) {
                   obj@block$SetExpanded(as.logical(value))
                   return(obj)
                 })


## handlers
## putonto expander in @block
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gExpandgroupRGtk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            addhandler(obj@block, "activate",handler, action)
          })

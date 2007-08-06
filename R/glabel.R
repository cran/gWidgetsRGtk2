setClass("gLabelRGtk",
         contains="gComponentRGtk",
         prototype=prototype(new("gComponentRGtk"))
         )

## constructor
setMethod(".glabel",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   text= "", markup = FALSE, editable = FALSE, handler = NULL, 
                   action = NULL, container = NULL, 
                   ...
                   ) {

            force(toolkit)

            
            label <- gtkLabelNew()

            ## pack into an event box so that we can get signals
            evb = gtkEventBoxNew()
            evb$Add(label)

            obj = new("gLabelRGtk",block=evb, widget=label,toolkit=toolkit)

            if(markup) 
              tag(obj,"markup")<-TRUE
            else
              tag(obj, "markup") <- FALSE

            if(nchar(text)>0)
              svalue(obj) <- text

            if(editable) {
              edit = gedit()
              tag(obj, "edit") <- edit
              handler = NULL                      # override handler
              addhandlerchanged(edit, handler = function(h,...) {
                ## copy edit value into label, put back
                svalue(obj) <- svalue(edit)
                evb$Remove(edit@widget@widget) # get GTK object from gedit()
                evb$Add(label)
              })
              ##This is for connecting to the third mosue
              
              id = addhandlerclicked(obj,
                handler=function(h,...) {
                  svalue(edit) <- svalue(obj)
                  evb$Remove(label)                 # swap out
                  evb$Add(getBlock(edit))
                  getWidget(edit)$GrabFocus()
              })
              tag(obj, "handler.id") <- id
            }
            
            if(!is.null(handler)) {
              id = addhandlerclicked(obj, handler=handler,action=action)
            }
            
            ## attach?
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow()
              add(container, obj,...)
            }
            
            invisible(obj)
          })

## methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gLabelRGtk"),
          function(obj, toolkit, index=NULL, drop=NULL, ..) {
            markup = tag(obj, "markup")
            if(is.null(markup)) markup = FALSE

            val = obj@widget$GetText()
            if(!is.empty(markup) && markup==TRUE)
              val = gsub("<[^>]*>","",val)    # strip off
            return(val)
          })

## svalue<-
setReplaceMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gLabelRGtk"),
          function(obj, toolkit, index=NULL, ..., value) {
            ## set the text
            markup = tag(obj, "markup")
            if(is.null(markup)) markup = FALSE
            
            if(as.logical(markup)==TRUE)
              obj@widget$SetMarkup(value)
            else
              obj@widget$SetText(value)

            return(obj)
          })


## special GTK method for rotation
setGeneric(".rotatelabel",function(obj, angle, ...) standardGeneric(".rotatelabel"))
setMethod(".rotatelabel",
          signature("gLabelRGtk"),
          function(obj, angle, ...) {  
            obj@widget$SetAngle(angle)
          }
        )

##################################################
## handlers
## need to put handler on evb -- not widget
setMethod(".addHandler",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gLabelRGtk"),
          function(obj, toolkit, signal, handler, action=NULL, ...) {
            ID = .addHandler(obj@block, toolkit, signal, handler, action, override=obj,...)
            return(ID)
          })

setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gLabelRGtk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addHandler(obj@block, toolkit, signal="button-press-event",
                        handler=handler, action=action, override=obj,...)
          })

setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gLabelRGtk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            edit = tag(obj, "edit")
            if(!is.null(edit)) {
              ## we use unrealize here, the addhandlerchanged on edit wasn't
              ## working for some strage reason
              return(addhandlerunrealize(edit, handler, action))
            } else {
              ## use addhandlerclicked
              return(.addhandlerclicked(obj, toolkit, handler, action, ...))
            }
          })

### need to fuss with evb vs. label
setMethod(".adddroptarget",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gLabelRGtk"),
          function(obj, toolkit, targetType="text", handler=NULL, action=NULL, ...) {
            ## problem -- we want to add drop target to obj@block evb,
            ## but have handler refer to obj@widgeg=label. 
            addDropTarget(obj@block, toolkit, targetType, handler, action, override=obj)
            
          })

setMethod(".adddropsource",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gLabelRGtk"),
          function(obj, toolkit, targetType="text", handler=NULL, action=NULL, ...) {
            ## problem -- we want to add drop target to obj@block evb,
            ## but have handler refer to obj@widgeg=label. 
            addDropSource(obj@block, toolkit, targetType, handler, action, override=obj)
            
          })


## Put onto block
setMethod(".addpopupmenu",signature(toolkit="guiWidgetsToolkitRGtk2", obj="gLabelRGtk"),
          function(obj, toolkit, menulist, action=NULL, ...) {
            addPopupMenuWithSignal(obj@block, toolkit , menulist, action, override=obj,...)
          })
setMethod(".add3rdmousepopupmenu",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gLabelRGtk"),
          function(obj, toolkit, menulist,action=NULL, ...) {
            add3rdMousePopupMenuWithSignal(obj@block, toolkit,
                                           menulist, action, override=obj,...)
          })
##################################################
## internal function -- used by gvariables in  gcommandline
setGeneric("gaddlabel", function(obj, text="", markup=FALSE, pos=1, container=NULL, ...) standardGeneric("gaddlabel"))

setMethod("gaddlabel",
          signature("guiWidget"),
          function(obj, text="", markup=FALSE, pos=1, container=NULL, ...)
          gaddlabel(obj@widget, text, markup, pos, container, ...)
        )

setMethod("gaddlabel",
          signature("gWidgetRGtk"),
          function(obj, text="", markup=FALSE, pos=1, container=NULL, ...) {
            ## wrap widget into a new package with label
            if(pos %in% c(2,4)) {
              group = ggroup(horizontal=TRUE,container=container,
                toolkit=obj@toolkit)
            } else {
              group = ggroup(horizontal=FALSE,container=container,
                toolkit=obj@toolkit)
            }
            
            
            if(pos %in% 2:3) {
              glabel(text, markup=markup, container=group, toolkit=obj@toolkit)
              add(group, obj,expand=TRUE)
            } else {
              add(group, obj,expand=TRUE)
              glabel(text, markup=markup, container=group, toolkit=obj@toolkit)
            }
            ## group is returned. No methods added here, just a new package
            return(group)
          })
          

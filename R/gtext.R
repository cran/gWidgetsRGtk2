setClass("gTextRGtk",
         representation(tags="list"),
         contains="gComponentRGtk",
         prototype=prototype(new("gComponentRGtk"))
         )

setMethod(".gtext",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   text=NULL,
                   width=NULL, height=300,
                   font.attr = NULL, wrap = TRUE,
                   handler = NULL, action=NULL,
                   container=NULL, ...) {

            force(toolkit)
            
            group = ggroup()
            sw = gtkScrolledWindowNew()
            add(group, sw, expand=TRUE)
            sw$SetPolicy("GTK_POLICY_AUTOMATIC","GTK_POLICY_AUTOMATIC")
            if(!is.null(width))
              sw$SetSizeRequest(width,height)
            
            textview = gtkTextViewNew()
            textview$SetLeftMargin(10)
            textview$SetRightMargin(10)
            if(wrap)
              textview$SetWrapMode(GtkWrapMode['word'])
            else
              textview$SetWrapMode(GtkWrapMode['none'])
            
            sw$Add(textview)
            textview$Show()
            
            ## add tags to buffer
            buffer = textview$GetBuffer()
            ## weights
            fontWeights = names(PangoWeight)
            fontWeights = fontWeights[fontWeights != "normal"] # also in Styles
            for(i in fontWeights)
              buffer$createTag(i, weight = PangoWeight[i])
            
            ## styles
            fontStyles = names(PangoStyle)
            for(i in fontStyles)
              buffer$createTag(i, style = PangoStyle[i])
            ## family
            buffer$createTag("monospace",family = "monospace")
            ## sizes ## old defs for .PangoScale are no longer valid as of 10.4
            fontSizes = c(
              "xx-large"= PANGO_SCALE_XX_LARGE,
              "x-large" = PANGO_SCALE_X_LARGE,
              "large"   = PANGO_SCALE_LARGE,
              "medium"  = PANGO_SCALE_MEDIUM,
              "small"   = PANGO_SCALE_SMALL,
              "x-small" = PANGO_SCALE_X_SMALL,
              "xx-small" = PANGO_SCALE_XX_SMALL
              )
            
            for(i in names(fontSizes)) 
              buffer$createTag(i, scale = fontSizes[i])
            
            ## colors -- 
            fontColors = c("black","blue","red","yellow","brown","green","pink")
            for(i in fontColors) {
              buffer$createTag(i,foreground = i)
              buffer$createTag(Paste(i,".background"),background = i)
            }
            
            
            
            tags = list(
              styles = fontStyles,
              family = "monospace",
              weights = fontWeights,
              sizes = names(fontSizes),
              foreground.colors = fontColors,
              background.colors = paste(fontColors,".background", sep="")
              )

            obj = new("gTextRGtk", block=group, widget=textview, tags=tags, toolkit=toolkit)

            ##   ## Handle attributes
            ##   if(!is.null(font.attr))
            ##     font(obj) <- font.attr
            
            if(!is.null(text)) {
              add(obj, text, font.attr=font.attr)
            }
            
  
            ## attach to container
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj,...)
            }
            
            if (!is.null(handler)) {
              id = addhandler(obj, "changed", handler, action)
            }
            return(obj)
          })

## as.gText converts a textview into gText class for use of its methods
## need some way to get tags, this is a bit of hack
#as.gText = function(textview) {
#  obj = list(ref=NULL, textview=textview, tags=gtext()$tags)
#  .class(obj) = .class(gtext())
#  return(obj)
#}

### methods

## drop=TRUE to get only mouse selected text
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gTextRGtk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            ## grab all text
            buffer = obj@widget$GetBuffer()
            if(is.null(drop) || drop == FALSE) {
              start = buffer$GetStartIter()$iter
              end = buffer$GetEndIter()$iter
            } else {
              ## return only **selected** text
              bounds = buffer$GetSelectionBounds()
              if(bounds$retval == FALSE) return("")
              start = bounds$start
              end = bounds$end
            }
            return(buffer$GetText(start,end))
            })
          
##  svalue<-() replaces text
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitRGtk2",obj="gTextRGtk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   textbuffer = obj@widget$GetBuffer()
                   if(length(value) > 1)
                     value = paste(value, collapse="\n")
                   textbuffer$SetText(value)
                   return(obj)
                 })


## clear all text in buffer
setMethod("dispose",signature(obj="gTextRGtk"),
          function(obj,...)  {
            .dispose(obj, obj@toolkit, ...)
          })
setMethod(".dispose",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gTextRGtk"),
          function(obj, toolkit,  ...) {
            buffer = obj@widget$GetBuffer()
            startiter =  buffer$GetStartIter()$iter
            enditer =  buffer$GetEndIter()$iter
            buffer$Delete(startiter, enditer)
          })


### Add method is a workhorse for this class. Value can be
## * a line of text
## * a vector of lines of text
## * an gWidget instance
## need to do where value of "point"
## add, as a method, needs to have a consistent signature. I'

## add text
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gTextRGtk",value="character"),
          function(obj, toolkit, value,  ...) {
            theArgs = list(...)                      # look for font.attr, do.newline, where

            do.newline = ifelse(is.null(theArgs$do.newline), TRUE, as.logical(theArgs$do.newline))
            markup = theArgs$font.attr
            if(!is.null(markup))
              markup = markup[markup %in% unlist(obj@tags)] # only some markup
            where = ifelse(is.null(theArgs$where), "end",theArgs$where)

            buffer = obj@widget$GetBuffer()
            iter = switch(where,
              "end"=buffer$GetEndIter()$iter,
              "beginning"=buffer$GetStartIter()$iter,
              {cat("Only end, beginning implemented")
               buffer$GetEndIter()$iter
             })

            for(i in 1:length(value) ) {
              if(is.null(markup)) {
                buffer$Insert(iter, value[i])
              } else {
                  lst = list(object=buffer, iter=iter, text=value[i])
                  for(tag in markup)
                    lst = c(lst,tag)
                  do.call("gtkTextBufferInsertWithTagsByName",lst)
                }
              if(do.newline) buffer$Insert(iter,"\n")
            }
          })

## add a widget
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gTextRGtk",value="guiWidget"),
          function(obj, toolkit, value,  ...) {
            .add(obj,toolkit, value@widget, ...)
          })

setMethod(".add",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gTextRGtk",value="gWidgetRGtk"),
          function(obj, toolkit, value,  ...) {
            theArgs = list(...)                      # look for font.attr, do.newline, where
            
            do.newline = ifelse(is.null(theArgs$do.newline), TRUE, as.logical(theArgs$do.newline))

            where = ifelse(is.null(theArgs$where), "end",theArgs$where)
            buffer = obj@widget$GetBuffer()
            iter = switch(where,
              "end"=buffer$GetEndIter()$iter,
              "beginning"=buffer$GetStartIter()$iter,
              {cat("Only end, beginning implemented")
               buffer$GetEndIter()$iter
             })
            

            anchor = buffer$CreateChildAnchor(iter)
            getWidget(obj)$AddChildAtAnchor(getWidget(value), anchor)
            if(do.newline) buffer$Insert(iter,"\n")
            
            })


## set the font for the selected area of the gtext object
setReplaceMethod(".font",
                 signature(toolkit="guiWidgetsToolkitRGtk2",obj="gTextRGtk"),
                 function(obj, toolkit, ..., value) {
                   ## get tags that are known
                   tags = value
                   tags = tags[tags %in% unlist(obj@tags)]
                   if(length(tags) == 0) return()
                   
                   ## get start, end iters
                   buffer = obj@widget$GetBuffer()
                   bounds = buffer$GetSelectionBounds()
                   if(bounds$retval == FALSE) return(obj)
                   
                   for(i in tags)
                     buffer$ApplyTagByName(i, bounds$start, bounds$end)
                   
                   return(obj)
                 })



##################################################
##################################################
## testing This is copied from pygtk tutorial
addWidgetAtPoint = function(obj, value) {
  evb = gtkEventBoxNew()
  evb$SetBorderWidth(15)
  evb$AddEvents(c(GdkEventMask["button-press-mask"],
                  GdkEventMask["button-release-mask"],
                  GdkEventMask["button-motion-mask"],
                  GdkEventMask["button-motion-hint-mask"]))
  
  widget = value@widget
  if(is(widget,"gContainer") || is(widget,"gComponent"))
    widget = widget@widget                 # for instance, ggroup
  
  evb$Add(widget)
  evb$ShowAll()

  ## connect move handler?
  try(connectSignal(evb,
                signal = "button-press-event",
                f = movableWidgetButtonPressHandler,
                data = list(obj=obj@widget),
                user.data.first = TRUE),
      silent=TRUE)
  try(connectSignal(evb,
                signal = "button-release-event",
                f = movableWidgetButtonReleaseHandler,
                data = list(obj=obj@widget),
                user.data.first = TRUE),
      silent=TRUE)
  try(connectSignal(evb,
                signal = "motion-notify-event",
                f = movableWidgetButtonMotionNotifyHandler,
                data = list(obj=obj@widget),
                user.data.first = TRUE),
      silent=TRUE)
  
  
  ## get xpos, ypos
  ptr = obj@widget$GetPointer()
  xpos = ptr$x; ypos = ptr$y
  xpos = 1; ypos = 1

  buffer = obj@widget$GetBuffer()
  iter = buffer$GetEndIter()$iter
  anchor = buffer$CreateChildAnchor(iter)
  obj@widget$AddChildAtAnchor(evb, anchor)

  return()
  obj@widget$AddChildInWindow(evb, GtkTextWindowType['widget'],
               xpos, ypos)                 

}

movableWidgetButtonPressHandler = function(h, widget, event, ...) {

  textview = h$obj
  info = widget$GetData("moveable-widget-data")

  if(is.null(info)) {
    info = list("start_x"= NA, "start_y"=NA, button = NA)
    widget$SetData("moveable-widget-data", info)
  }

  if(!is.list(info[['button']]) || is.na(info[['button']])) {
    info$button = event
    allocation = widget$GetAllocation()
    info[['start_x']] = allocation$x
    info[['start_y']] = allocation$y
    info[['click_x']] = allocation$x + event$GetX()
    info[['click_y']] = allocation$y + event$GetY()
    widget$SetData("moveable-widget-data", info)
  }
  return(FALSE)
}

movableWidgetButtonReleaseHandler = function(h, widget, event, ...) {
  info = widget$GetData("moveable-widget-data")
  if(!is.list(info[['button']]) || is.na(info[['button']])) {
    cat("relase handler failed\n")
    return(FALSE)
  }
  
  info = widget$GetData("moveable-widget-data")
  
  x = info[['start_x']] + event$GetX() + widget$GetAllocation()$x - info[['click_x']]
  y = info[['start_y']] + event$GetY() + widget$GetAllocation()$y - info[['click_y']]
  
  widget$SetData("moveable-widget-data", NULL)
  
  h$obj$MoveChild(widget, x,y) 
  
  return(FALSE)
}

movableWidgetButtonMotionNotifyHandler = function(h, widget, event, ...) {
  
  info = widget$GetData("moveable-widget-data")
  
  ptr = widget$GetPointer()
  allocation = widget$GetAllocation()
  x = ptr$x + allocation$x
  y = ptr$y + allocation$y
  
  x = info[['start_x']] + (x - info[['click_x']])
  y = info[['start_y']] + (y - info[['click_y']])
  
  h$obj$MoveChild(widget, x,y)   
  return(FALSE)
}

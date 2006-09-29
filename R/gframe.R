setClass("gFrameRGtk",
         contains="gGroupRGtk",
         prototype=prototype(new("gGroupRGtk"))
         )

## add a frame for packing. subclass of gGroup
setMethod(".gframe",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   text = "", markup=FALSE,
                   pos = 0, ## pos in [0,1] 0 for left, 1 for right
                   container=NULL,
                   ...) {
            
            frame = gtkFrameNew()
            if(markup) {
              label = gtkLabelNew()
              label$SetMarkup(text)
              frame$SetLabelWidget(label)
            } else {
              frame$SetLabel(text)
            }
            frame$SetLabelAlign(pos,0)
            
            group = ggroup(...) # for horizontal, spacing etc.
            frame$Add(getBlock(group))

            ## add label to group
            obj = new("gFrameRGtk",
              block=frame, widget=group@widget, toolkit=toolkit)

            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj)
            }
            return(obj)
          })

### methods -- inherited from ggroup


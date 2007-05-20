##################################################
##
## gtkTreeViewColumn stuff

setMethod("svalue",signature(obj="GtkTreeViewColumn"),
          function(obj, index=NULL, drop=NULL, ...) {
            theArgs =   list(...)
            index = ifelse(is.null(index),FALSE, as.logical(index))
            drop =  ifelse(is.null(drop), TRUE, as.logical(drop))

            ## is this a treeviewCOlumn that ggrid made?
            col.no = try(tag(obj,"column.number"), silent=TRUE)
            if(inherits(col.no,"try-error"))
              return(NA)

            ## return index if requested
            if(index) return(col.no)
            
            ## else return the values
            
            gridObj = tag(obj,"gridObj")
            vals = gridObj[,col.no, visible=TRUE, drop=drop] # only show visible
            return(vals) 
          })

setMethod("id",signature(obj="GtkTreeViewColumn"),
          function(obj,  ...) {
            curname = tag(obj,"name")
            if(is.null(curname) || length(curname) == 0) {
              cat("No name for this view column\n")
              return(NA)
            } else {
              return(curname)
            }
          })


setReplaceMethod("id",signature(obj="GtkTreeViewColumn"),
          function(obj, ..., value) {
            curname = tag(obj,"name")
            if(is.null(curname) || length(curname) == 0) {
              ## not there, set it
              label = glabel(value)
              ## set in view col
              obj$SetWidget(getBlock(label))
              tag(obj,"widget") <- label
            } else {
              ## store in widget
              svalue(tag(obj,"widget"))<-value
            }
            tag(obj,"name") <- value
            return(obj)
          })

setMethod("addhandlerchanged",signature(obj="GtkTreeViewColumn"),
          function(obj, handler=NULL, action=NULL, ...) {
            lst = list()                # store ids for handlers
            lst[["cellrenderer"]] = addhandler(obj$GetCellRenderers()[[1]],
                 signal = "edited",
                 handler = handler,
                 action = action
                 )
            ## If view column comes from gdf.R then subsetBy is stored in object
            ## so changes there will propogate adding change to underlying model
            ## proved too slow as it seems to get called repeatedly, and
            ## wouldn't stop by setting return value
            gridObj = tag(obj,"gridObj")
            if(!is.null(gridObj)) {
              doSubsetBy = tag(gridObj,"doSubsetBy")
              if(!is.null(doSubsetBy) && as.logical(doSubsetBy) == TRUE) {
                subsetBy = tag(gridObj,"subsetBy")
                lst[["subsetBy"]] = addhandlerchanged(subsetBy, handler, action)
              }
            }
            return(lst)
          })


setMethod("removehandler",signature(obj="GtkTreeViewColumn"),
          function(obj, ID=NULL,...) {
            removehandler(obj$GetCellRenderers()[[1]],ID,...)
          })


## fix up [ for RGtkDataFrame
## is this needed?
setMethod("[",
          signature(x="RGtkDataFrame"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, guiToolkit("RGtk2"), i, j, ..., drop=drop)
          })
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitRGtk2",x="RGtkDataFrame"),
          function(x, toolkit, i, j, ..., drop=TRUE) {

            frame <- as.data.frame(x)
                                        #if (!missing(i) && length(i) > 0 && inherits(i[[1]], "GtkTreePath"))
                                        #	i <- .RGtkCall("R_gtk_tree_paths_to_indices", i)+1
            if(missing(i) && missing(j))
              frame[, , drop=drop]
            else if(missing(i))
              frame[,j, drop=drop]
            else if(missing(j))
              frame[i,, drop=drop]
            else
              frame[i,j,drop=drop]
          })



## which versino of RGtk2
getRGtk2Version = function() {
  m = installed.packages()
  ver = m["RGtk2","Version"]
  ver = unlist(strsplit(ver,"\\."))
  names(ver) <- c("major","minor","version")
  return(ver)
}

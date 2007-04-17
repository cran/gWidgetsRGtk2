setClass("gLayoutRGtk",
         contains="gContainerRGtk",
         prototype=prototype(new("gContainerRGtk"))
         )

## an gWidget for tables
 
## take two -- this time build up tale, then use visible to show
## this way, we don't need to set size initially
## constructor
setMethod(".glayout",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   homogeneous = FALSE,
                   spacing = 10,        # amount (pixels) between row, cols, NULL=0
                   container = NULL, ...
                   ) {

            force(toolkit)
            
            ## how to add in per column adjusments?
            adjust = "center"                             # left or right or center
            
            
            ## container for table
            group = ggroup(container=container)

            obj = new("gLayoutRGtk",
              block=group, widget=group, toolkit=toolkit)
            
            tag(obj,"nrows") <- 0
            tag(obj,"ncols") <- 0
            tag(obj,"val.lst") <- list()
            tag(obj,"homogeneous") <- homogeneous
            tag(obj,"spacing") <- spacing
            tag(obj,"adjust") <- adjust
            
            invisible(obj)
          })
          
## how we populate the table
setReplaceMethod("[",
                 signature(x="gLayoutRGtk"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitRGtk2",x="gLayoutRGtk"),
          function(x, toolkit, i, j, ..., value) {
            ## check that all is good
            if(is.character(value)) {
              ## wrap characters into labels
              value = glabel(value,...)
            }

            
            if(is.guiWidget(value)  || is.gWidget(value)) {
              nrows = tag(x,"nrows")
              ncols = tag(x,"ncols")
              val.lst = tag(x,"val.lst")
              nrows = max(nrows,max(i))
              ncols = max(ncols,max(j))
              val.lst[[length(val.lst)+1]] =
                list(i=i,j=j,value=value)
              
              tag(x,"nrows") <- nrows
              tag(x,"ncols") <- ncols
              tag(x,"val.lst") <- val.lst
            }  else {
              print(class(value))
              warning("Value is not an gWidget\n")
            }
            
            return(x)

            ##   if(obj$adjust == "right") {
            ##     group = ggroup()
            ##     addSpring(group)
            ##     add(group,value)
            ##   } else if(obj$adjust = "left") {
            ##     group = ggroup()
            ##     add(group,value)
            ##     addSpring(group)   
            ##   } else {
            ##     group = value
            ##   }
            
          })

### as written, deleting the table widget, destroys the subwidgets. These need to be removed before deleting the tblGroup

## show the table, elements are in val.lst
## visible<-
setReplaceMethod(".visible",
                 signature(toolkit="guiWidgetsToolkitRGtk2",obj="gLayoutRGtk"),
                 function(obj, toolkit, ..., value) {
                   if(is.logical(value) == TRUE) {
                     nrows = tag(obj,"nrows")
                     ncols = tag(obj,"ncols")
                     homogeneous = tag(obj,"homogeneous")
                     spacing = tag(obj,"spacing")
                     val.lst = tag(obj,"val.lst")
                     
                     table = gtkTableNew(nrows, ncols, homogeneous=homogeneous)
                     
                     ## table properties
                     if(!is.null(spacing)) {
                       table$SetRowSpacings(spacing)
                       table$SetColSpacings(spacing)
                     }
                     ## now pack in values
                     ## for 1,1 go from 01 to 01 with table ttach
                     for(vals in val.lst) {
                       i = vals$i; j = vals$j; value = vals$value
                       
                       i = c(min(i)-1,max(i))
                       j = c(min(j)-1,max(j))
                       
                       
                       addThis = getBlock(value)
                       table$Attach(addThis,min(j),max(j),min(i),max(i),
                                    xoptions="GTK_FILL",yoptions="GTK_FILL")
                       ##                   xoptions="GTK_SHRINK",yoptions="GTK_SHRINK")
                     }
                     
                     ## tried to do the old delete/add trick, but had problems. When I delete the table, the widgets can't be reused. I then tried to remove them using sapply, but no luch there either.
                     
                     if(!is.null(tag(obj,"table"))) {
                       warning("layouts are only made visible once.")
                       return(obj)
                     }

                     ### obj@widget is a ggroup instance
                     add(obj@widget,table, expand=TRUE)
                     tag(obj,"table") <- table
                     invisible(obj)
                   } else {
                     ## value=FALSE, no show
                     invisible(obj)
                   }
                 })

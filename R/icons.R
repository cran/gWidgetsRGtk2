## add to stock icons

## function to look up stock icons
## ie. ok returns "gtk-ok"

loadGWidgetIcons = function() {

  ## these were lifted from scigraphica
  iconNames = c("2dlayer",
    "3dcontour",
    "arrows",
    "arrows1",
    "larrow",
    "rarrow",
    "darrow",
    "uarrow",
    "barplot",
    "boxplot",
    "boxplot1",
    "bubbles",
    "calendar",                         # from KDE
    "cloud",
    "contour",
    "curve",
    "dataframe",
    "density",
    "evaluate",
    "factor",
    "numeric",
    "logical",
    "function",
    "function1",
    "graph",
    "graph2",
    "hist",
    "lines",
    "matrix",
    "newplot",
    "pch2",
    "plot",
    "plot1",
    "points",
    "polar",
    "scatterplot3d",
    "select",
    "spike",
    "subset",
    "symbol_circle",
    "symbol_cross",
    "symbol_diamond",
    "symbol_dntriangle",
    "symbol_dot",
    "symbol_impulse",
    "symbol_ltriangle",
    "symbol_none",
    "symbol_plus",
    "symbol_rtriangle",
    "symbol_square",
    "symbol_star",
    "symbol_uptriangle",
    "target",
    "ts")
  
  
  
  ## add the icons
  ## we use xpm icons gimp can convert
  ## Loop over all to add here
  iconFullNames = paste(iconNames,".xpm", sep="")
  iconFiles = sapply(iconFullNames, function(name) {
    system.file("images",name, package="gWidgetsRGtk2")
  })

  addToGtkStockIcons(iconNames, iconFiles)
}

## add stock icons from files
setMethod(".addStockIcons",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit, iconNames, iconFiles, ...) {
            addToGtkStockIcons(iconNames, iconFiles)
          })

addToGtkStockIcons = function(iconNames, iconFiles) {
  iconfactory = gtkIconFactoryNew()
  for(i in 1:length(iconNames)) {
    iconsource = gtkIconSourceNew()
    iconsource$SetFilename(iconFiles[i])
    
    iconset = gtkIconSetNew()
    iconset$AddSource(iconsource)
    
    stockName = paste("gWidgetsRGtk2-",iconNames[i],sep="")
    
    iconfactory$Add(stockName, iconset)
    
    items = list(test=list(stockName, iconNames[i],"","",""))
    gtkStockAdd(items)
  }
  
  iconfactory$AddDefault()
}

## find the stock icons. This includes those added bia loadGWidgetIcons()
setMethod(".getStockIcons",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit) {
            .stockicons = list()
            for(i in unlist(gtkStockListIds())) {
              name = sub("[a-zA-Z0-9]*-","",i)
              .stockicons[[name]] = i
            }
            return(.stockicons)
          })

## name can be a vector
## return NA, if not there
getstockiconname = function(name=NULL) {
  .stockicons = getStockIcons(toolkit=guiToolkit("RGtk2"))         # cache?

  if(is.null(name))
    return(unlist(.stockicons))
  

  tmpfun = function(names) {
    sapply(names, function(name) {
      ## already a stock name?
      if(name %in% .stockicons)
        return(name)
      
      if(is.null(.stockicons[[name]])) {
        return(NA)
      } else {
        return(.stockicons[[name]])
      }
    })
  }
  
  return(tmpfun(name))
}


#################################################
## functions to deal with icons
## class to icon translation -- return stock name
## with prefix
stockIconFromClass = function(theClass=NULL) {
  default = "symbol_star"
  
  if(is.null(theClass) ||
     is.na(theClass) ||
     length(theClass) == 0
     )
    return(NA)
  
  if(theClass %in% .models)
    return(getstockiconname("lines"))
  if(theClass %in% .ts)
    return(getstockiconname("ts"))
  if(theClass %in% .functions)
    return(getstockiconname("function"))

  ret = switch(theClass,
    "numeric"= "numeric",
    "integer"= "numeric",
    "logical" = "logical",
    "character"="select-font",
    "matrix" = "matrix",
    "data.frame" = "dataframe",
    "list" = "dataframe",
    "complex"="numeric",
    "factor"="factor",
    "recordedplot" = "plot",
    NA)
  
  return(getstockiconname(ret))
}

stockIconFromObject = function(obj)
  stockIconFromClass(class(obj)[1])



##
## 
##loadGWidgetIcons()


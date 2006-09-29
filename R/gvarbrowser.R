## Use this to filter by type
## knownTypes in common
### Use this for filtering by (gvarbrowser, gvarbrowsertree)
.datasets = c(
  "numeric","logical","factor","character",
  "data.frame","matrix","list",
  "table","xtabs",
  "nfnGroupedData","nffGroupedData","nmGroupedData"
  )
.models = c("lm","glm","lqs","aov","anova",
    "lme","lmList","gls",
  "ar","arma","arima0","fGARCH","fAPARCH"
    )
.ts = c("ts", "mts", "timeSeries", "its", "zoo")
.functions=c("function")
.plots = c("recordedplot")

knownTypes = list(
  "data sets and models"=c(.datasets, .models, .ts),
  "data sets"= .datasets,
  "model objects" = .models,
  "time series objects" = .ts,
  "functions"=.functions,
  "saved plots" = .plots,
  "all" = NULL
  )

## list of some type
lsType = function(type, envir=.GlobalEnv) {
  x = with(.GlobalEnv, sapply(ls(), function(i) class(get(i))))
  objects = names(x)[sapply(x, function(i) any(i %in% type))]
  return(objects)
}
lsDatasets = function(envir=.GlobalEnv())  lsType(.datasets, envir)
lsModels = function(envir=.GlobalEnv())  lsType(.models, envir)
lsTs = function(envir=.GlobalEnv())  lsType(.ts, envir)
lsFunctions = function(envir=.GlobalEnv())  lsType(.functions, envir)



offspring = function(path=c(), data=NULL) {
  if(length(path) == 0) {
    x = ls(envir=.GlobalEnv)
    if(length(x) == 0) {
      return(data.frame(names="",hasSubTree=FALSE,type=""))
    }

    type = c();hasTree=c()
    for(i in 1:length(x)) {
      y = getObjectFromString(x[i])
      type[i] = str2(y)
      hasTree[i] = hasSubTree(y)
    }
  } else {
    string = paste(path,collapse="$")
    obj = getObjectFromString(string)

    x = with(obj, ls())

    if(length(x) == 0) {
      return(data.frame(names="",hasSubTree=FALSE,type=""))
    }

    type = c();hasTree=c()
    for(i in 1:length(x)) {
      y = getObjectFromString(paste(string,x[i],sep="$"))
      type[i] = str2(y)
      hasTree[i] = hasSubTree(y)
    }
  }

  allValues = data.frame(names=I(x), hasSubTree=hasTree, type=I(type))

  if(!is.null(data)) {
    return(allValues[allValues$type %in% data, ,drop=FALSE])
  } else {
    return(allValues)
  }
}

hasSubTree = function(x) {
  tmp  = try(is.list(x) &&
    !is.guiWidget(x) && !is.gWidget(x) && !is.RGtkObject(x) &&
    !is.null(names(x)), silent=TRUE)
  if(!inherits(tmp,"try-error") && tmp)
    return(TRUE)
  else
    return(FALSE)
}

## in common.R
## getObjectFromString = function(string, envir=.GlobalEnv) {
##   out = try(eval(parse(text=string), envir=envir), silent=TRUE)
##   if(inherits(out, "try-error"))
##     return(NA)
##   return(out)
## }


setClass("gVarbrowserRGtk",
         representation(filter="guiComponent"),
         contains="gComponentRGtk",
         prototype=prototype(new("gComponentRGtk"))
         )

## THe main object
setMethod(".gvarbrowser",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   handler = function(h,...) {
                     values = h$obj[]                   # was values(h$obj)
                     value = paste(values,collapse="$")
                     if(!is.null(action))
                       print(do.call(h$action,list(svalue(value))))
                   },
                   action = "summary",
                   container = NULL,
                   ...) {
            
            group = ggroup(horizontal=FALSE, container=container)
            filterGroup = ggroup(container=group)
            glabel("Filter by:",container=filterGroup)
            filterPopup = gdroplist(names(knownTypes), container=filterGroup)
            svalue(filterPopup) <- "data sets"
            
            ## main tree
            tree = gtree(offspring=offspring,
              offspring.data=knownTypes[[svalue(filterPopup)]],
              col.types=data.frame(Name="string",Type="string"),
              icon.FUN = function(d,...) {
                byReturnVector(d,function(x) stockIconFromClass(x[,'type']))
              }
              )
            
            add(group,tree, expand=TRUE) 
            ## update the tree this way
            addhandlerclicked(filterPopup,
                              handler = function(h,...) {
                                key = svalue(filterPopup)
                                offspring.data = knownTypes[[key]]
                                update(h$action, offspring.data)
                              },
                              action=tree)
            
            ## add an idle handler for updating tree every 5 seconds
            idleid = addhandleridle(tree, interval=5000, handler = function(h,...) {
              key = svalue(filterPopup)
              offspring.data = knownTypes[[key]]
              update(h$obj, offspring.data)
            })
            addhandlerunrealize(tree, handler = function(h,...) {
              gSourceRemove(h$action)},
                                action=idleid)
            
            
            
            ## drop handler
            adddropsource(tree,handler=function(h,...) {
              
              values = h$obj[]
              values = sapply(values, untaintName)
              return(paste(values,collapse="$"))
            })
            
            tag(tree,"view")$SetEnableSearch(TRUE)
            tag(tree,"view")$SetHeadersClickable(TRUE)

            obj = new("gVarbrowserRGtk",block=group, widget=tree, filter=filterPopup)

            if(!is.null(handler)) {
              id = addhandlerdoubleclick(tree,
                handler=handler, action=action)
            }
            
            ## all done
            return(obj)
          })

### methods
## push methods and handlers down to tree in this case

setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gVarbrowserRGtk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            svalue(obj@widget,...)
          })
setMethod("[",
          signature(x="gVarbrowserRGtk"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x,guiToolkit("RGtk2"), i, j, ..., drop=drop)
          })
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitRGtk2",x="gVarbrowserRGtk"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            if(missing(i))
              x@widget[...]
            else
              x@widget[i,...]
          })





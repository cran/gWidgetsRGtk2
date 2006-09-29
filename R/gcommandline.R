## command line widget
## toggles between gtext() instances containing text to edit, and output to display.

setClass("gCommandlineRGtk",
         representation=representation("gComponentRGtk",
           textGroup="guiWidget",
           editText="guiWidget",
           showText="guiWidget",
           textGroupState="character",
           editButton="guiWidget",
           clearButton="guiWidget",
           runButton="guiWidget",
           historyButton="guiWidget",
           width="numeric",
           height="numeric",
           prompt="character",
           useConsole="logical"),
         contains="gComponentRGtk",
         prototype=prototype(new("gComponentRGtk"))
         )


## constructor
setMethod(".gcommandline",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   command = "", assignto=NULL,
                   useConsole = FALSE,
                   prompt = getOption("prompt"),
                   width = 500, height = .6*width,
                   container = NULL,
                   ...) {
            
            ## adjust command if need be
            if(nchar(command) > 0 && !is.null(assignto))
              command = addAssignto(command, assignto)
            
            
            ## the main widgets
            group = ggroup(horizontal = FALSE, container = container)
            toolbarGroup = ggroup(container=group, spacing = 0)
            textGroup = ggroup()                  # holds editText or showText
            add(group, textGroup, expand=TRUE)
            editText = gtext()
            showText = gtext()
            
            
            ## set up widget,
            
            ## toolbar
            ## the handlers
            openFile = function(h,...) {
              icl = h$action
              gfile("Select a file to read into command line",
                    type="open",
                    handler = function(h,...) {
                      file = h$file
                      tmp = icl@editText
                      svalue(tmp) <- readLines(file)
                      editCode(h)
                    })
            }
            saveFile = function(h,...) {
              icl = h$action
              win = gwindow("Save buffer contents",v)
              group = ggroup(horizontal=FALSE, container=win)
              saveFileName = gfilebrowse("",type="save")
              add(group, saveFileName)
              saveType = gradio(c("commands","output"), index=FALSE)
              gaddlabel(saveType,"Save which values?", pos=2, container=group)
              gseparator(container=group)
              buttonGroup = ggroup(container=group)
              addSpring(buttonGroup)
              gbutton("save",handler=function(h,...) {
                filename = svalue(saveFileName)
                if(is.empty(filename)) {
                  cat("Need file to save to\n")
                  return()
                }
                if(svalue(saveType) == "commands")
                  values = svalue(icl@editText)
                else
                  values = svalue(icl@showText) 
                writeLines(values, filename)
                dispose(win)
              }, container=buttonGroup)
            }
            editCode = function(h,...) {
              icl = h$action
              ## switch widgets
              delete(icl@textGroup, icl@showText)
              add(icl@textGroup, icl@editText, expand=TRUE)
              tag(icl,"textGroupState") <- "edit"
##              icl@textGroupState <- "edit"

              enabled(runButton) <- TRUE
              enabled(historyButton) <- TRUE
              enabled(clearButton) <- TRUE
              enabled(editButton) <- FALSE
            }
            runCode = function(h,...) {
              icl = h$action
              chunk = svalue(icl@editText)
              svalue(icl) <- chunk
            }
            
            selectHistory = function(h,...) {
              previous = svalue(h$action, index=25)
              win = gwindow("Select a previous value", visible=TRUE)
              group = ggroup(horizontal = FALSE, container = win)
              add(group, glabel("double click selection"))
              theHistory = gtable(previous, action = h$action,
                handler = function(h,...) {
                  newcommand = svalue(h$obj)
                  icl = h$action
                  svalue(icl@editText, font.attr = "monospace") <- newcommand
                  dispose(win)
                })
              add(group, theHistory, expand=TRUE)
              buttonGroup = ggroup(container=group)
              addSpring(buttonGroup)
              add(buttonGroup, gbutton("cancel",handler = function(h,...) dispose(win)))
            }
            
  ## pack into widget
            add(textGroup, editText, expand=TRUE)
            ## toolbars
            sourceButton = gbutton("open", container=toolbarGroup)
            saveButton = gbutton("save", container = toolbarGroup)
            editButton = gbutton("edit",  container = toolbarGroup)
            clearButton = gbutton("clear",container = toolbarGroup)
            runButton = gbutton("evaluate",  container = toolbarGroup)
            historyButton = gbutton("history",  container= toolbarGroup)

            obj = new("gCommandlineRGtk",
              block=group,
              widget = group,
              toolkit=toolkit,
              textGroup = textGroup, editText = editText, showText = showText,
              textGroupState = "edit",
              editButton = editButton, clearButton=clearButton,
              runButton = runButton, historyButton = historyButton, 
              width=width, height=height,
              prompt =prompt, useConsole = useConsole)
            
            tag(obj,"showText")<-showText
            tag(obj,"editText")<-editText # delete doesn't work if it makes copis using @ slot
            tag(obj,"textGroup") <- textGroup
            tag(obj,"textGroupState") <- "edit"
            ## add handlers to buttons
            addhandlerclicked(sourceButton,  handler = openFile, action=obj)
            addhandlerclicked(saveButton,  handler = saveFile, action=obj)
            addhandlerclicked(editButton,  handler = editCode, action=obj)
            addhandlerclicked(clearButton, action=obj, function(h,...)
                              dispose(h$action@editText))
            addhandlerclicked(runButton,  handler = runCode, action=obj)
            addhandlerclicked(historyButton,  handler = selectHistory, action=obj)
            ## initialize history
            tag(obj,"history")  <- c()
            ## initialize state: used to check if swap is needed
            obj@textGroupState <- "edit" # edit or text
            tag(obj,"textGroupState") <- "edit"
            
            ## which text widget?
            if(command == "") {
              enabled(editButton) <- TRUE
            } else {
              svalue(editText) <- command
            }
            
            return(obj)
            
          })
          

### Methods
## return all previous, or just the index most recent
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gCommandlineRGtk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            theArgs = list(...);
            
            commandHistory = tag(obj,"history")
            if(length(commandHistory) == 0)
              return(c())
            if(is.null(index)) {
              return(commandHistory)
            } else {
              n = length(commandHistory)
              m = max(1, n - index + 1)
              return(rev(commandHistory[m:n]))
            }
          })

## evaluate command, store in history, swqp out widgets
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitRGtk2",obj="gCommandlineRGtk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   ## get commane
                   command = value;
                   assignto = names(value)
                   if(!is.null(assignto)) {
                     command = addAssignto(command, assignto)
                   }
                   svalue(obj@editText,font.attr = "monospace") <-  command # why font.attr here?

                   ## add to history
                   tag(obj, "history", replace=FALSE) <- command

                   evalChunk(command, obj@showText, obj@prompt, obj@useConsole)

                   ## switch widgets -- if not correct
##                   textGroupState = obj@textGroupState
                   textGroupState = tag(obj,"textGroupState")
                   if(!is.null(textGroupState) && textGroupState == "edit") {
                     delete(obj@textGroup, obj@editText)
                     add(obj@textGroup, obj@showText, expand=TRUE)
                   }
#                   obj@textGroupState <- "text"
                   tag(obj,"textGroupState") <- "text"
                   enabled(obj@showText) <- FALSE        # no editing of this display
                   enabled(obj@runButton) <- FALSE
                   enabled(obj@historyButton) <- FALSE
                   enabled(obj@editButton) <- TRUE
                   
                   enabled(obj@clearButton) <- FALSE
                   return(obj)
                 })

## history function
setMethod("[",
          signature(x="gCommandlineRGtk"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })

setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitRGtk2",x="gCommandlineRGtk"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            history = tag(obj, "history")

            if(missing(i))
              return(history)
            else
              history(i)
          })

### working functions


## parse command(s) and make assingment on last one.
addAssignto = function(command,assignto) {
  assignto = make.names(assignto)
  tmp = unlist(strsplit(command, ";"))
  if(length(tmp)>1) {
    command = paste(tmp[-length(tmp)], Paste(assignto,"<-",tmp[length(tmp)]), collapse=";", sep=";")
  } else {
    command =  Paste(assignto,"<-", command)
  }
  return(command)
}




## taken from Sweave
## takes a chunk, iterweaves command and output
evalChunk = function(chunk, widget, prompt = getOption("prompt"),
  useConsole=FALSE) {
  svalue(widget) <- ""                 # clear out
  chunkexps <- try(parse(text=chunk), silent=TRUE)
  if(inherits(chunkexps,"try-error")) {
    add(widget, chunkexps, font.attr = c("monospace"))
#    addTextWidget(widget, chunkexps)
    cat("Houston, we have a problem with:\n",chunk,"\n")
    return(c())
  }
  if(length(chunkexps) == 0)
    return(c())
#  output = c()

  for(nce in 1:length(chunkexps)) {
    ce <- chunkexps[[nce]]
    dce <- deparse(ce, width.cutoff=0.75*getOption("width"))
    command = Paste(prompt,
      paste(dce,collapse=paste("\n", getOption("continue"), sep=""))
      )
    add(widget, command, font.attr = c("monospace","red","italic"))

    if(useConsole)
      cat(command,"\n")
    ## is there output?
    tmpcon <- file()
    sink(file=tmpcon)
    err <- RweaveEvalWithOpt(ce, list(eval=TRUE,print=FALSE,term=TRUE,visible=FALSE))
    cat("\n") # make sure final line is complete
    sink()
    theOutput <- readLines(tmpcon)
    close(tmpcon)
    ## delete empty output
    if(length(theOutput)==1 & theOutput[1]=="") theOutput <- NULL
    
    if(inherits(err, "try-error")) {
      add(widget, err, font.attr=c("monospace","red","bold"))
      if(useConsole)
        cat(err,"\n")
    } else {
      if(!is.null(theOutput)) {
        add(widget, theOutput, font.attr = c("monospace"))
        if(useConsole) 
          cat(paste(theOutput,sep="",collapse="\n"),"\n")
      }
    }
  }
}



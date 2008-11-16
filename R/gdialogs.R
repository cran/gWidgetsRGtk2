## some dialogs for R
## dialogs don't get windows, they make them
## dialogs are modal
## dialogs return their value -- not an object. so source(gfile()) should work

setMethod(".gmessage",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   message,
                   title = "message",
                   icon = c("info","warning","error","question"),
                   parent=NULL,
                   handler = NULL,
                   action = NULL,
                   ...
                   ) {
            
            force(toolkit)

            icon = match.arg(icon)
            
            icon = Paste("GTK_MESSAGE_",toupper(match.arg(icon)))
            button = "GTK_BUTTONS_OK"

            ## parent
            if(!is.null(parent)) {
              parent <- getBlock(parent)
              if(!is(parent,"GtkWindow"))
                parent <- parent$GetWindow()
              if(!is(parent,"GtkWindow"))
                parent <- NULL          # give up
            }
              
            
            ## use message dialog for Gtk
            dlg = gtkMessageDialogNew(
              message.format = message,
              parent = parent,
              flags = 0,
              buttons = button,
              type=icon)
            dlg$SetTitle(title)
            dlg$GrabFocus()
            dlg$GetWindow()$Raise()

            
            ## run in modal mode
            response = dlg$Run()
            h = list(obj=dlg, ref=dlg, action=action)
            if(response == GtkResponseType["cancel"] ||
               response == GtkResponseType["close"] ||
               response == GtkResponseType["delete-event"]) {
              dlg$Destroy()
              invisible(FALSE)
            } else if(response == GtkResponseType["ok"]) {
              if(!is.null(handler)) handler(h)
              dlg$Destroy()
              invisible(TRUE)
            } else {
              gwCat("Don't know this response")
              print(response)
              dlg$Destroy()
              invisible(NA)
            }
          })

## if OK then run handler, else not
setMethod(".gconfirm",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   message,
                   title = "Confirm",
                   icon = c("info", "warning", "error", "question"),
                   parent=NULL,
                   handler = NULL,
                   action = NULL,
                   ...
                   ) {
            
            if(missing(icon)) icon="question"
            icon = match.arg(icon)
            icon = Paste("GTK_MESSAGE_",toupper(match.arg(icon)))
            
            icon = "GTK_MESSAGE_QUESTION"
            buttons = "GTK_BUTTONS_OK_CANCEL"
            
            ## parent
            if(!is.null(parent)) {
              parent <- getBlock(parent)
              if(!is(parent,"GtkWindow"))
                parent <- parent$GetWindow()
              if(!is(parent,"GtkWindow"))
                parent <- NULL          # give up
            }
              


            dlg = gtkMessageDialogNew(
              message.format = message,
              parent = parent,
              flags = 0,
              buttons = buttons,
              type=icon)
            dlg$SetTitle(title)            
            dlg$GrabFocus()
            dlg$GetWindow()$Raise()

            ## add callback to close
            close.handler = function(h,...) h$obj$Destroy()
            
            ## run in modal mode
            response = dlg$Run()
            h = list(obj=dlg, action=action)
            if (response == GtkResponseType["close"] ||
                response == GtkResponseType["delete-event"] ||
                response == GtkResponseType["cancel"]) {
              dlg$Destroy()
              invisible(FALSE)
            } else if(response == GtkResponseType["ok"]) {
              if(!is.null(handler)) handler(h)
              dlg$Destroy()
              invisible(TRUE)
            } else {
              gwCat("Don't know this response")
              print(response)
              dlg$Destroy()
              invisible(NA)
            }
            
          })


## Add input to the above
## h,... in handler has componets action, input (for value)
setMethod(".ginput",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   message,
                   text="",
                   title = "Input",
                   icon = c("info", "warning", "error", "question"),
                   parent=NULL,                   
                   handler = NULL,
                   action = NULL,
                   ...
                   ) {
            
            icon = Paste("GTK_MESSAGE_",toupper(match.arg(icon)))

            ## parent
            if(!is.null(parent)) {
              parent <- getBlock(parent)
              if(!is(parent,"GtkWindow"))
                parent <- parent$GetWindow()
              if(!is(parent,"GtkWindow"))
                parent <- NULL          # give up
            }
            

            ## use message dialog for Gtk
            dlg = gtkMessageDialogNew(
              message.format = NULL,
              parent = parent,
              flags = 0,
              buttons = "GTK_BUTTONS_OK_CANCEL",
              type=icon
              )
            dlg$SetTitle(title)
            dlg$GrabFocus()
            dlg$GetWindow()$Raise()

            group = ggroup(horizontal=FALSE)
            glabel(message, container=group)
            input = gedit(text,container=group)
            
            
            ## find the area to pack the entry widget
            dlg$GetVbox()[[1]]$PackStart(getBlock(group)) 
            ##  dlg$GetVbox()[[2]]$GetWidget()$PackStart(group$ref) 
            ##  dlg$GetVbox()$PackStart(group$ref)

            ## set as default
            widget <- getWidget(input)
            widget['can-default'] <- TRUE
            widget$grabFocus()
            widget$grabDefault()

            
            ## run in modal mode
            response = dlg$Run()
            h = list(obj=dlg, ref=dlg, action=action, input=svalue(input))
            if(response == GtkResponseType["cancel"] ||
               response == GtkResponseType["close"] ||
               response == GtkResponseType["delete-event"]) {
              dlg$Destroy()
              invisible("")
            } else if(response == GtkResponseType["ok"]) {
              if(!is.null(handler)) handler(h)
              val = svalue(input)
              dlg$Destroy()
              ## input is widget, return value of widget
              invisible(val)
            } else {
              gwCat("Don't know this response")
              print(response)
              dlg$Destroy()
              invisible(NA)
            }
            
          })

## add a widget to the dialog. This is modal
setMethod(".gbasicdialog",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   title = "Dialog",
                   widget,
                   parent=NULL,                   
                   handler = NULL,
                   action = NULL,
                   ...
                   ) {


            ## parent
            if(!is.null(parent)) {
              parent <- getBlock(parent)
              if(!is(parent,"GtkWindow"))
                parent <- parent$GetWindow()
              if(!is(parent,"GtkWindow"))
                parent <- NULL          # give up
            } else {
              parent <- gtkWindowNew(show=FALSE)
            }


            
            dlg = gtkDialog(title,
              parent=parent,
              c("modal"),
              "gtk-ok", GtkResponseType["ok"],
              "gtk-cancel", GtkResponseType["cancel"])
            dlg$SetTitle(title)
            dlg$GrabFocus()
            dlg$GetWindow()$Raise()


            tag(widget,"dlg") <- dlg

            ## group to pack widget in
            group = ggroup()
            add(group, widget, expand=TRUE)
            
            ## find the area to pack the entry widget
            dlg$GetVbox()$PackStart(getBlock(group))
            
            ## run in modal mode
            response = dlg$Run()
            h = list(obj=widget, action=action)
            if(response == GtkResponseType["cancel"] ||
               response == GtkResponseType["close"] ||
               response == GtkResponseType["delete-event"]) {
              ## cancel action
              dlg$Destroy()
              return(FALSE)
            } else if(response == GtkResponseType["ok"]) {
              if(!is.null(handler))
                handler(h)
              dlg$Destroy()
              return(TRUE)              # was widget, but TRUE now
            } else {
              ## default action
              gwCat("Don't know this response")
              print(response)
              dlg$Destroy()
              invisible(NA)
            }
            
          })
          

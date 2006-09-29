## some dialogs for R
## dialogs don't get windows, they make them
## dialogs are modal
## dialogs return their value -- not an object. so source(gfile()) should work

setMethod(".gmessage",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   message,
                   icon = c("info","warning","error","question"),
                   button = c("close","ok","cancel"),
                   handler = defaultHandler,
                   action = NULL,
                   ...
                   ) {
            
            icon = match.arg(icon)
            button = match.arg(button)
            
            icon = Paste("GTK_MESSAGE_",toupper(match.arg(icon)))
            button = Paste("GTK_BUTTONS_",toupper(match.arg(button)))
            
            ## use message dialog for Gtk
            dlg = gtkMessageDialogNew(
              message.format = message,
              flags = 0,
              buttons = button,
              type=icon)
            dlg$GrabFocus()
            dlg$GetWindow()$Raise()

            
            ## run in modal mode
            response = dlg$Run()
            h = list(obj=dlg, ref=dlg, action=action)
            if(response == GtkResponseType["cancel"]) {
              dlg$Destroy()
              return(FALSE)
            } else if (response == GtkResponseType["close"]) {
              dlg$Destroy()
              return(FALSE)
            } else if(response == GtkResponseType["ok"]) {
              if(!is.null(handler)) handler(h)
              dlg$Destroy()
              return(TRUE)
            } else {
              print(response)
            }
          })
  
## if OK then run handler, else not
setMethod(".gconfirm",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   message,
                   icon = c("info", "warning", "error", "question"), 
                   handler = NULL,
                   action = NULL,
                   ...
                   ) {
            
            if(missing(icon)) icon="question"
            icon = match.arg(icon)
            icon = Paste("GTK_MESSAGE_",toupper(match.arg(icon)))
            
            icon = "GTK_MESSAGE_QUESTION"
            buttons = "GTK_BUTTONS_OK_CANCEL"
            
            dlg = gtkMessageDialogNew(
              message.format = message,
              flags = 0,
              buttons = buttons,
              type=icon)
            dlg$GrabFocus()
            dlg$GetWindow()$Raise()

            ## add callback to close
            close.handler = function(h,...) h$obj$Destroy()
            
            ## run in modal mode
            response = dlg$Run()
            h = list(obj=dlg, action=action)
            if(response == GtkResponseType["cancel"]) {
              dlg$Destroy()
              return(FALSE)
            } else if (response == GtkResponseType["close"]) {
              dlg$Destroy()
              return(FALSE)
            } else if(response == GtkResponseType["ok"]) {
              if(!is.null(handler)) handler(h)
              dlg$Destroy()
              return(TRUE)
            } else {
              print(response)
            }
            
          })


## Add input to the above
## h,... in handler has componets action, input (for value)
setMethod(".ginput",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   message,
                   icon = c("info","warning","error","question"),
                   handler = NULL,
                   action = NULL,
                   ...
                   ) {
            
            icon = Paste("GTK_MESSAGE_",toupper(match.arg(icon)))
            
            ## use message dialog for Gtk
            dlg = gtkMessageDialogNew(
              message.format = NULL,
              flags = 0,
              buttons = "GTK_BUTTONS_OK_CANCEL",
              type=icon
              )
            dlg$GrabFocus()
            dlg$GetWindow()$Raise()

            group = ggroup(horizontal=FALSE)
            glabel(message, container=group)
            input = gedit(container=group)
            
            ## find the area to pack the entry widget
            dlg$GetVbox()[[1]]$PackStart(getBlock(group)) 
            ##  dlg$GetVbox()[[2]]$GetWidget()$PackStart(group$ref) 
            ##  dlg$GetVbox()$PackStart(group$ref)
            
            
            ## run in modal mode
            response = dlg$Run()
            h = list(obj=dlg, ref=dlg, action=action, input=svalue(input))
            if(response == GtkResponseType["cancel"]) {
              dlg$Destroy()
              return(FALSE)
            } else if (response == GtkResponseType["close"]) {
              dlg$Destroy()
              return(FALSE)
            } else if(response == GtkResponseType["ok"]) {
              if(!is.null(handler)) handler(h)
              dlg$Destroy()
              return(svalue(input))
            } else {
              print(response)
            }
            
          })

## add a widget to the dialog. This is modal
setMethod(".gbasicdialog",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   title = "Dialog",
                   widget,
                   icon = c("info","warning","error","question"),
                   handler = NULL,
                   action = NULL,
                   ...
                   ) {
  
            icon = Paste("GTK_MESSAGE_",toupper(match.arg(icon)))
            window = gtkWindowNew(show=FALSE)
            dlg = gtkDialog(title, window,
              c("modal"),
              "gtk-ok", GtkResponseType["ok"],
              "gtk-cancel", GtkResponseType["cancel"])
            dlg$GrabFocus()
            dlg$GetWindow()$Raise()

            ## group to pack widget in
            group = ggroup()
            add(group, widget, expand=TRUE)
            
            ## find the area to pack the entry widget
            dlg$GetVbox()$PackStart(group$ref)
            
            ## run in modal mode
            response = dlg$Run()
            h = list(obj=widget, action=action)
            if(response == GtkResponseType["cancel"]) {
              ## cancel action
              dlg$Destroy()
              return(FALSE)
            } else if (response == GtkResponseType["close"]) {
              ## close action
              dlg$Destroy()
              return(FALSE)
            } else if(response == GtkResponseType["ok"]) {
              if(!is.null(handler)) handler(h)
              dlg$Destroy()
              return(widget)
            } else {
              ## default action
              dlg$Destroy()
            }
            
          })
          

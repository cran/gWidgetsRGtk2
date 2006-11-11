## add calendar widget: shoule I have gcalendar, gcalendarbrowser?
## no handler function, can add to entry object with addhandler

setClass("gCalendarRGtk",
         contains="gEditRGtk",
         prototype=prototype(new("gEditRGtk"))
         )


setMethod(".gcalendar",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   text="",
                   format="%Y-%m-%d",
                   handler = NULL, action=NULL,
                   container=NULL,...) {

            force(toolkit)

            group = ggroup(horizontal=TRUE, container=container)
            entry = gedit(text=text, container=group,
              handler=handler,action=action,...)

            calendar.cb = function(h,...) {
              ## called when button is clicked
              ## pop up a calendar, when date selected, copy to entry
              win = gtkWindowNew(show=FALSE)
              cal = gtkCalendarNew()
              win$Add(cal)
              cal$Show();
              win$Show()

              cal$AddCallback("day-selected-double-click", function(w,...) {
                l = cal$GetDate()
                dateselected = paste(l$year,l$month+1,l$day,sep="-",collapse="-")
                ## format date
                dateselected = format(as.Date(dateselected,format=format))
                svalue(entry) <- dateselected

                ## call handler if present
                if(!is.null(handler)) {
                  h = list()
                  h$obj = entry
                  h$action = action
                  handler(h)
                }
                
                ## call change event on entry widget
                win$Destroy()
              })
            }

            gbutton("calendar",handler=calendar.cb, container=group)

            obj = new("gCalendarRGtk",
              block=group, widget = entry@widget@widget, toolkit=toolkit)

            invisible(obj)
          })

### methods inherit from gedit methods

require(gWidgets)
options("guiToolkit"="RGtk2")


## remove once new gWidgets is installed
gWidgetsDir <- system.file("tests",package="gWidgets")

if(gWidgetsDir != "") {
  files <- list.files(gWidgetsDir,
                      pattern = "\\.R$",
                      full.names = TRUE)

  
  for(unitTest in files) {
    source(unitTest)
  }
}

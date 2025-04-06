# Duke University Brain-Computer Interface Project
# Shiny app to retrieve BCI2000 file and convert to EDF format
# Jan 2022

# Launch app ui.r and server.r from specified appDir
# Note the specification of a tcp port that the process will listen on for http requests

library("shiny")

# Specify directory containing ui.r and server.r

# Local dir
ad <- "C:/Projects/Duke/BCI/BCI-EDF-App"

# Execute 
runApp(appDir=ad,
       launch.browser=T,
       host = getOption("shiny.host", "127.0.0.1"),
       #port=4295,
       display.mode = c("auto", "normal", "showcase"))
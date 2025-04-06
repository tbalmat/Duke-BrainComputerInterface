# Duke University Brain-Computer Interface Project
# Shiny app to retrieve BCI2000 file and convert to EDF format
# Jan 2022

# Shiny app server function

# Information on shiny available at:
# https://shiny.rstudio.com/
# https://github.com/rstudio/shiny
# https://cran.r-project.org/web/packages/shiny/shiny.pdf

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)
#options(device="windows")
options(shiny.maxRequestSize=10**9) 

library(shiny)
library(shinyjs)
library(DT)
library(readr)

setwd("C:/Projects/Duke/BCI/BCI-EDF-App")
source("../BCI2000/BCIFunctions.r")
sampleBCIFile <- "C:/Projects/Duke/BCI/BCI2000/CHANGEMES001R01.dat"
edfDir <- "C:/Projects/Duke/BCI/EDF/BCIConversion"

shinyServer(

  function(input, output, session) {

    #######################################################################################################
    # Function:  Error handler
    #######################################################################################################

    msgWindow <- function(level="ERROR", title="", msg, size="m", buttonText="OK")
      showModal(modalDialog(HTML(paste("<b>", level, "</b><br><br>", paste(msg, collapse="<br><br>", sep=""), sep="")),
                            title=paste("BCI-EDF ", title, sep=""),
                            size=size, easyClose=T, footer=modalButton(buttonText)))

    #######################################################################################################
    # Configure a download handler for the sample BCI2000 file
    #######################################################################################################

    output$sampleBCI2000FileDownload <-
      downloadHandler(filename="CHANGEMES001R01.dat",
                      contentType=".dat",
                      content=function(file) {
                                tryCatch({
                                  x <- readr::read_file_raw(sampleBCIFile)
                                  readr::write_file(edfdat, file)},
                                  warning=function(err) msgWindow("WARNING", "Download sample BCI2000 file", err),
                                  error=function(err) msgWindow("ERROR", "Download sample BCI2000 file", err)
                                )
                              }
                     )

    #######################################################################################################
    # Observe event function:  Browse file selected
    #######################################################################################################

    observeEvent(input$fileUploadBrowse, {

      tryCatch({
        # Read file
        brFile <<- input$fileUploadBrowse
        x <- readr::read_file(brFile[,"datapath"])
        #output$resultsBCI <- renderText(charToRaw(x))
        output$resultsBCI <- renderText(gsub("\r", "", gsub("\n", "<br>", x)))
        bcidat <<- bciRetrieve(input$fileUploadBrowse[,"datapath"])},
        warning=function(err) msgWindow("WARNING", "File upload", err),
        error=function(err) msgWindow("ERROR", "File upload", err)
      )

    }, ignoreInit=T)

    #######################################################################################################
    # Observe event function:  Convert selected BCI2000 file to EDF
    #######################################################################################################

    observeEvent(input$convertEDF, {

      tryCatch({
        edfdat <- bci2edf(bcidat)
        x <- edfdat
        #x[which(x=="00")] <- as.raw("20")
        #x[which(x=="20")] <- as.raw("2E")
        #output$resultsEDF <- renderText(rawToChar(x))
        output$resultsEDF <- renderText(x)
        updateTabsetPanel(session, "tabsetResults", selected="tabPanelResultsEDF")
        f <- paste(edfDir, "/", sub(".dat", ".edf", brFile[,"name"]), sep="")
        output$edfFileName <- renderText(f)
        readr::write_file(edfdat, f)},
        warning=function(err) msgWindow("WARNING", "Convert to EDF", err),
        error=function(err) msgWindow("ERROR", "Convert to EDF", err)
      )

    }, ignoreInit=T)

  }

)

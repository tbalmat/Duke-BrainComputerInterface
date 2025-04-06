# Duke University Brain-Computer Interface Project
# Shiny app to retrieve BCI2000 file and convert to EDF format
# Jan 2022

# Shiny app user interface function

# Information on shiny available at:
# https://shiny.rstudio.com/
# https://github.com/rstudio/shiny
# https://cran.r-project.org/web/packages/shiny/shiny.pdf

library(shiny)
library(shinyjs)
library(DT)

setwd("C:/Projects/Duke/BCI/BCI-EDF-App")

shinyUI(

  fluidPage(

    useShinyjs(),
    #includeCSS("../app/www/iCPAGdb-spacelab.css"),
    #shinythemes::themeSelector(),

    title="BCI2000-EDF",

    # Configure some styles
    # It is better to apply these to individual objects when possible, to guarantee application
    # For instance, the navbarPage, below, specifies a theme, which causes styles established here to be ignored
    # Defining styles as the final step of the navbarPage definition overrides styles defined in the theme
    # Use the "inspection" feature of your browser (right-button over object of interest) to reveal classes
    tags$head(
      # Reposition and alter appearance of notification window
      tags$style(HTML(".shiny-notification {font-size:14px; color:black; font-weight:bold; width:50%;
                      height=200px; position:fixed; top:calc(50%); left:calc(25%)}")),
      # Make text and background of fileInput progress bar transparent
      # Otherwise, "File uploaded" message appears once file upoaded, but prior to a read.table operation accomplished
      # Note that background:transparent causes the progress bar to disappear, which is useful for fileInput(), but also
      # causes the bar to disappear when using shiny::Progress$new(), the standard progress object
      # Progress$new() has a style parameter, but accepts only two values: notification or old
      # Notification uses the .progress-bar style below, old seems to behave  similarly
      # Unfortunately the unmodifiable text of the fileInput() progress bar and the fixed style of Progress bars
      # does not give much flexibility for tailoring progress bar appearance
      #tags$style(HTML(".progress-bar {color: transparent!important; background:transparent!important}")),
      # Change color of text that appears inside of the bar
      # This affects the bar for file input objects, but not for progress#new() objects, since their text appears
      # below the bar
      #tags$style(HTML(".progress-bar {color: transparent!important}")),
      # Change the color of the progress bar (file input and progress meter)
      #tags$style(HTML(".progress-bar {background-color: gray;}"))
      #HTML(".shiny-file-input-progress {color: transparent!important}")
      # Hide file input progress bar
      #tag$style(HTML(".shiny-file-input-progress {display: none}")),
      # Customize the modal window
      #tags$style(".modal-body {padding: 10px}
      #            .modal-content  {-webkit-border-radius: 6px !important;-moz-border-radius: 6px !important;border-radius: 6px !important;}
      #            .modal-dialog { width: 240px; display: inline-block; text-align: left; vertical-align: top;}
      #            .modal-header {background-color: #339FFF; border-top-left-radius: 6px; border-top-right-radius: 6px}
      #            .modal { text-align: right; padding-right:10px; padding-top: 24px;}
      #            .close { font-size: 16px}"))
      #tags$style(HTML(".modal-lg {position: relative; display: flex; flex-direction: column; margin-top: 50%}"))
      #tags$style(HTML(".modal-lg {width: 50%; margin-top: 10%}")),
      #tags$style(HTML(".modal {margin-top: 10%}")),
      #tags$style(HTML(".modal-header {color: #ffffff; background-color: #0066cc; border-top-left-radius: 6px; border-top-right-radius: 6px}"))
      # Adjust style of action buttons
      # Button appearance instructions are in iCPAGdb-spacelab.css (.btn-default class)
      # An attempt was made to include corresponding css tags here, but they are over-ridden when a css file is used
      # Note that the active Shiny process may have to be reloaded in order for changes to the css file to take effect
      #tags$style(HTML(".btn {color:white; background:linear-gradient(#54b4eb, #2fa4e7 60%, #0088dd)}")),
      # Adjust style of file input button
      #tags$style(HTML(".btn-file {color:white; background:linear-gradient(#54b4eb, #2fa4e7 60%, #0088dd)}")),
    ),

    div(

      div(
        HTML("<H3>Duke University BCI Project</H3><br>"),
        style="display:inline-block; vertical-align:top; margin-top:0px; width:97%"
      ),

      navbarPage(
        id="tabsetCPAG", title="", inverse=T,
        # Themes have fixed search locations:
        # theme=shinytheme("xyz") searches for xyz.min.css in the shinythemes/css directory
        # of the shinythemes package directory in the R installation directory
        # theme="xyz.css" searches for file xyz.css in the www directory within the directory that the
        # shiny app was executed from within, not the current working directory, as set by setwd() 
        # The following method does not report an error, but the theme file is not loaded
        # theme="app/www/iCPAGdb-spacelab.css",
        # The following method does not report an error, but the theme file is not loaded
        # theme="C:/Users/Kyung Soon/Documents/R/win-library/4.0/shinythemes/shinythemes/css/spacelab.min.css",
        # The following methods load specified themes
        # Use the shinytheme() function (that returns a character vector)
        # This will locate the css file in the shinythemes package directory
        # Note that font urls within the css reference relative directory locations, which are
        # interpreted with respect to the css file location
        theme=shinythemes::shinytheme("spacelab"),
        # Use a character vector indicating the desired theme (file name)
        # This method is identical to theme=shinythemes::shinytheme("spacelab")
        # theme="shinythemes/css/spacelab.min.css",
        # Grab custom theme css from www directory
        # Note that font file references must be altered and/or fonts must be copied to
        # appropriate directories as referenced in css @font-face {} instructions
        # This method does not function (for me, anyway) in a Shiny Server environment
        # theme="iCPAGdb-spacelab.css",
        # theme="spacelab.min.css",

        # Upload a BCI2000 file and convert to EDF
        tabPanel(title="Upload BCI2000 file and convert to EDF", value="uploadConvert",
          div(
            # Prompts
            div(
              sidebarPanel(width=12,
                # Upload
                div(
                  HTML("<b>1. Upload a BCI2000 file</b>"),
                  div(
                    HTML("Note:  Maximum file size is 1GB.  Expected upload time is appproxiamtaely 30 seconds per 100MB. File contents are displayed in the table below. To download a sample GWAS file for review, click here:&nbsp;"),
                    downloadLink(outputId="sampleBCI2000FileDownload", label="sample BCI2000 file."),
                    HTML("&nbsp;For information on BCI2000 file format, click here: &nbsp;<a href=https://www.bci2000.org/mediawiki/index.php/Technical_Reference:BCI2000_File_Format target=_blank>BCI 2000 file format</a>."), 
                    style="margin-top:10px; margin-bottom:10px; margin-right:0%"
                  ),
                  # File browser
                  div(
                    div(
                      fileInput("fileUploadBrowse", "Choose file", multiple=FALSE, accept=".dat", width="85%"),
                      style="display:inline-block; vertical-align:top; margin-top:0px; width:24%"
                    ),
                    style="margin-top:20px; margin-bottom:0px"
                  ),
                  style="margin-top:0px; margin-bottom:-30px"
                ),
                HTML("<hr style='height:1px;color:black;background-color:black'>"),
                # Convert to EDF
                div(
                  HTML("<b>2. Convert to EDF</b><br><br>"),
                  div(
                    actionButton("convertEDF", "Convert"),
                    style="display:inline-block; vertical-align:top; margin-top:-5px"
                    ),
                  div(
                    textOutput("edfFileName"),
                    style="display:inline-block; vertical-align:top; margin-top:5px; margin-left:20px"
                  ),
                  style="margin-top:20px; margin-bottom:0px"
                )
              ),
              style="margin-top:0px"
            ),
            style="margin-top:-40px; margin-left:-15px; margin-right:-30px"
          ),
          # Results
          div(
            div(
              HTML("<b>Results</b>"),
              style="margin-top:10px; margin-bottom:10px"
            ),
            tabsetPanel(id="tabsetResults", type="pills",
              # BCI
              tabPanel(title="BCI2000", value="tabPanelResultsBCI",
                HTML("<br>"),
                htmlOutput(outputId="resultsBCI")
              ),
              # EDF
              tabPanel(title="EDF", value="tabPanelResultsEDF",
                HTML("<br><center>"),
                textOutput("resultsEDF"),
                HTML("</center>")
              )
            ),
            style="width=100%; margin-top:20px; margin-left:10px; margin-right:20px"
          ),
          style="margin-left:-15px; margin-top:-25px"
        ),

        # Quick-start
        tabPanel(title="Quick-start", value="tabPanelQuickStart",
          #div(includeHTML("app/QuickStart.html"), style="margin-top:15px; margin-left:20%; margin-right:20%")
        ),

        # About
        tabPanel(title="About BCI-EDF", value="tabPanelAbout",
          #div(includeHTML("app/About.html"), style="margin-top:15px; margin-left:20%; margin-right:20%")
        ),

        # Bibliography
        tabPanel(title="Bibliography", value="tabPanelBibliography",
          #div(includeHTML("app/Bibliography.html"), style="margin-top:15px; margin-left:20%; margin-right:20%")
        ),

        # Define styles within (as final step of) navbarPage to overide specific theme styles
        # Hide panel, otherwise clickable, yet invisible elements appear in the bar
        # with contents equal to the contents of the style tag, below
        shinyjs::hidden(
          tabPanel(title="", value="navBarStyles",
            # Use the "inspection" feature of your browser (right-button over object of interest) to reveal classes
            # Grab colors with the browser color sampler (dropper)
            # Buttons, default and hover
            tags$style(HTML(".btn-default{color:#ffffff;background:linear-gradient(#6c93be, #446e9b 60%, #3e648d);border-color:#446e9b}")),
            tags$style(HTML(".btn-default:hover{color:#ffffff;background:linear-gradient(#6e96c2, #45709e 80%, #416994); border-color:#447e9b}")),
            # Hide title, round corners of bar
            tags$style(HTML(".navbar-brand {display: none;}")),
            tags$style(HTML(".navbar {border-top-left-radius:7px; border-top-right-radius:7px}")),
            # Modal box appearance
            tags$style(HTML(".modal {margin-top:10%}")),
            tags$style(HTML(".modal-header {background:linear-gradient(#6c93be, #446e9b 60%, #3e648d); border-radius:5px;}")),
            # Opacity of main window
            tags$style(HTML(".modal-backdrop.in{opacity:0.15;filter:alpha(opacity=50)}")),
            # Title
            tags$style(HTML(".modal-title{color:#ffffff; margin:0;line-height:1.42857143}")),
            # tabsetPanel buttons
            # In the rendered HTML, the buttons appear as anchor tags within in unordered lists
            # View (toggle) pseudo class of object using the browser inspector to review pseudo classes (hover,
            # active, focus, focus-within) and related css properties
            # Pill button height (all pseudo classes)
            tags$style(HTML(".nav-pills{line-height:0.75}"))
            #tags$style(HTML(".nav-pills>li>a:hover{color:green;}")),
            #tags$style(HTML(".nav-pills>li>a:active{color:red;}"))
          )
        )

      ),

      style="margin-left: 10px"
    )
  )
)

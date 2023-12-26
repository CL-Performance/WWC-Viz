#Load dependencies

library(shiny)
library(shinycssloaders)
library(shinyWidgets)
library(waiter)
library(shinyjs)


# Define UI for application that draws a histogram
fluidPage(

  useWaiter(),
  
  useShinyjs(),
 

  
  # Add custom CSS for styling
  tags$style(
    HTML('

    body {
      font-family: "Source Sans 3", sans-serif; /* Use your desired font-family */
      font-weight: bold; /* Set font weight to bold */
      background-color: #202020; /* Set background color to #202020 */
      color: #aaaabb; /* Set text color to black */
    }
    
    h2 {
      text-align: center; /* Set text alignment to center */
      font-weight: bold; /* Set font weight to bold */
      padding-bottom: 0px;
    }
    
    h3 {
      text-align: center; /* Set text alignment to center */
      font-weight: bold; /* Set font weight to bold */
      padding-bottom: 0px;
    }
    
    .well {
      background-color: #202020 !important; /* Set background color for the sidebar */
    }
    
    .selectize-input {
      background-color: #aaaabb !important; /* Set background color for selectInput */
      color: #202020 !important; /* Set text color for selectInput */
      font-weight:bold;
    }
    
    .selectize-dropdown-content .option {background-color: #202020 !important; /* Set background color for selectInput */
      color: #aaaabb !important; /* Set text color for selectInput */
      font-weight:bold;
      padding: 4px 0 !important; /* Adjust top and bottom padding */
      margin: 0 !important; /* Reset margins */
    }
    
    .nav-tabs >  {
      font-weight:bold;
    }
        
    .nav-tabs>li.active>a, .nav-tabs>li.active>a:focus, .nav-tabs>li.active>a:hover  {
      background-color: #aaaabb;
      color: #202020;
      font-weight:bold;
        }
    
    .nav-tabs > li > a {
      color: #02675e;
    }
    
    .noUi-connect { background: #02675e;
                    border: #000000;
    }
    .noUi-handle {  background: #aaaabb;
                    border: #000000;
    }
    
    .noUi-tooltip { background: #aaaabb;
                    color: #202020;
    }
    
    .noUi-target {
                    background: #aaaabb;
    }
    

    
    
    #preloader {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        background-color: #000;
        display: flex;
        justify-content: center;
        align-items: center;
        z-index: 9999;
      }

      #video {
        width: 100%;
        max-height: 100%;
        object-fit: cover;
      }

      #main-content {
        display: none;
      }
    

    ')
  ),

# Video Preloader
  div(id = "preloader", width = "100%",
      tags$video(id = "video", autoplay = "autoplay", muted = "muted",
                 tags$source(src = "loader.mp4", type = "video/mp4"),
                 "Your browser does not support the video tag."
      )
  ),


    # Application title
  titlePanel("",windowTitle = "CL Performance"),
   
        # Show a plot of the generated distribution
  mainPanel( id = "main-content", width = "100%", height = "100%", style = "display: none;", 
    tabsetPanel(
            
#XG Tab
      tabPanel("xG Timeline",
        sidebarLayout(
              
          sidebarPanel(
            width = 3,
            titlePanel("WWC 2023"),
                
            selectInput("team", "Team:", NULL),
            verbatimTextOutput("selected_team"),
                
            selectInput("match", "Match:", NULL),
            verbatimTextOutput("selected_match"),
                
            withSpinner(plotOutput("barPlot", height = "450px"), image = 'loaderdark.gif', image.width = 250, hide.ui = FALSE)
          ),

# Show a plot of the generated distribution
          mainPanel( width = 9,

            column( width = 11,
              htmlOutput("xGPlotTitle"),
              withSpinner(plotOutput("xGPlot", height = "600px"), image = 'loaderdark.gif', image.width = 500, hide.ui = TRUE)
            ),
            column( width = 1,
              style = "padding-top: 120px;",
                        
              noUiSliderInput(
                inputId = "xGaxis",
                min = .5, max = 5,
                step = 0.5,
                value = 5,
                orientation = "vertical",
                direction = "rtl",
                width = "100px", height = "550px"
              ),
          
              verbatimTextOutput(outputId = "xGaxi")
            )
          )
        )
      ),
#XG Tab          
          
#Zone Tabs          
#Shot Assist Zone Tab
      tabPanel("Shot Assist Zones", 
        sidebarPanel(
          width = 4,
          noUiSliderInput(

            inputId = "shotXBin",
            min = 1,
            max = 12,
            step = 1,
            value = 5,
            format = wNumbFormat(decimals = 0, suffix = " - Lanes" )
          ),
          br(),
          noUiSliderInput(
            inputId = "shotYBin",
            min = 1,
            max = 12,
            step = 1,
            value = 6,
            format = wNumbFormat(decimals = 0, suffix = " - Rows" )
          ),
          br(),
          withSpinner(plotOutput("totalShotAssistZonePlot", height = "480px"), image = 'loaderdark.gif', image.width = 250, hide.ui = FALSE)
        ),
        mainPanel(
          width = 8,
          withSpinner(plotOutput("shotAssistZonePlot", height = "700px"), image = 'loaderdark.gif', image.width = 250, hide.ui = FALSE)         
                              
        )
      ),
#Goal Assist Zone Tab
      tabPanel("Goal Assist Zones", 
        sidebarPanel(
          width = 4,
          noUiSliderInput(
            
            inputId = "goalXBin",
            min = 1,
            max = 12,
            step = 1,
            value = 5,
            format = wNumbFormat(decimals = 0, suffix = " - Lanes" )
          ),
          br(),
          noUiSliderInput(
            inputId = "goalYBin",
            min = 1,
            max = 12,
            step = 1,
            value = 6,
            format = wNumbFormat(decimals = 0, suffix = " - Rows" )
          ),
          br(),
          withSpinner(plotOutput("totalGoalAssistZonePlot", height = "480px"), image = 'loaderdark.gif', image.width = 250, hide.ui = FALSE)
        ),
        mainPanel(
          width = 8,
          withSpinner(plotOutput("goalAssistZonePlot", height = "700px"), image = 'loaderdark.gif', image.width = 250, hide.ui = FALSE)         
        )
      ), 


#Goal Assist Zone Tab
      tabPanel("Passing Network", 
        sidebarPanel(
          width = 5,
          titlePanel("WWC 2023"),
                 
          selectInput("team2", "Team:", NULL),
          verbatimTextOutput("selected_team2"),
                 
          selectInput("match2", "Match:", NULL),
          verbatimTextOutput("selected_match2"),
          withSpinner(plotOutput("passingBarPlot", height = "450px"), image = 'loaderdark.gif', image.width = 250, hide.ui = FALSE)
        ),
        
        mainPanel( width = 7,
          column( width = 11,
                  withSpinner(plotOutput("passingNetworkPlot", height = "700px"), image = 'loaderdark.gif', image.width = 500, hide.ui = TRUE)
          ),
          column( width = 1,
                  style = "padding-top: 60px;",
                  div(
                    style = "text-align: right;", "Names:",
                    checkboxInput("passingNetworkNameBol", "", TRUE),
                  
                  
                  noUiSliderInput(
                    inputId = "passingNetworkMinPasses",
                    min = 1, max = 10,
                    step = 1,
                    value = 1,
                    orientation = "vertical",
                    direction = "rtl",
                    width = "100px", height = "550px"
                  ),
                  
                  verbatimTextOutput(outputId = "passingNetworkMinPasses")
                  ),
          )
        )          
      ),


#Final third entries
      tabPanel("Chance Creation", 

       sidebarPanel(
         width = 2,
                 selectInput("team3", "Team:", NULL),
                 verbatimTextOutput("selected_team3")  
                 ),
       
          column(width = 5,
                 br(),
                 withSpinner(plotOutput("finalThirdEntryPlot", height = "700px"), image = 'loaderdark.gif', image.width = 250, hide.ui = FALSE)
           ),
         column(width = 5,
                br(),
                withSpinner(plotOutput("regainsPlot", height = "700px"), image = 'loaderdark.gif', image.width = 250, hide.ui = FALSE)

          )
         )
        




#End Tabset Panel  
    )
#End Main Panel
  )
#End Fluid Page
)



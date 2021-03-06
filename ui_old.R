library(shiny)
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("GMPE Evaluation"),
  
  # Sidebar with a slider input for distance range
  sidebarPanel(
    wellPanel(
      selectInput("eval", "Evaluation type:",
                   list("Residual analysis" = "resid_analysis",
                        "Scores" = "error_scores"))
    ),
    
    wellPanel(
      checkboxInput(inputId = "local", label = "Choose a csv file on your machine", value = FALSE),
      
      conditionalPanel(
        condition = "input.local == false",
        textInput(inputId="url", label="File URL:", value="./sample.csv")
      ),
      
      conditionalPanel(
        condition = "input.local == true",
        fileInput(inputId = "file", label="Choose a csv file on your machine:"),
        helpText(HTML("<div style=\"color: red; font-weight: bold\">Be patient
        after choosing your file - it might take a few seconds.</div>"))
      ),
      
      conditionalPanel(
        condition = "input.eval == 'resid_analysis'",
        uiOutput("Models")
      ),
      conditionalPanel(
        condition = "input.eval == 'error_scores'",
        helpText(HTML("<div style=\"color: red; font-weight: bold\">Choose
                      your models below carefully - I do not check for matching
                      prediction types.</div>")),
        uiOutput("Models2")
      ),
      
      uiOutput("Events")
    ),
    
    wellPanel(
      uiOutput("Rhyp"),
      uiOutput("Mag")
    ),
    
    wellPanel(
     uiOutput("VS30")
    ),
    
    wellPanel(
      p(strong("Earthquake type (you can select more than one):")),
      uiOutput("eqtype")
    ),
    
    wellPanel(
      p(strong("Site class (you can select more than one):")),
      uiOutput("siteClass")
    ),
    
    conditionalPanel(
      condition = "input.eval == 'resid_analysis'",
      wellPanel(
        p(strong("Linear model fit to the residuals")),
        p(strong("Which variables to include:")),
        checkboxInput(inputId = "cb_rhyp", label = "Rhyp", value = TRUE),
        checkboxInput(inputId = "cb_mag", label = "Mag", value = TRUE),
        checkboxInput(inputId = "cb_avs30", label = "AVS30", value = TRUE),
        checkboxInput(inputId = "cb_eqtype", label = "EQ Type", value = TRUE),
        checkboxInput(inputId = "cb_siteclass", label = "Site class", value = TRUE)
      ),
      
      wellPanel(
        p(strong("Plot spatial residuals?")),
        checkboxInput(inputId = "spatial", label = "Yes", value = FALSE),
        conditionalPanel(
          condition = "input.spatial == true",
          p(strong("View:")),
          radioButtons("view_type", "View type:",
                      list("Average" = "ave",
                            "Smoothed" = "smooth"))
        )
    ))
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    conditionalPanel(
      condition = "input.eval == 'resid_analysis'",
      tabsetPanel(
        tabPanel("Residual plots", 
                 div(plotOutput("plot_resid", height="1000px"))),
        tabPanel("Unexplained residual plots",
                 div(plotOutput("plot_unex_resid", height="1000px"))),
        tabPanel("Model summary", h4("Linear Model"), verbatimTextOutput("summary"), 
                 h4("Mixed Effects Model"),
                 verbatimTextOutput("summary.re")),
        tabPanel("Spatial residual plot", 
                 div(plotOutput("spatplot", height="1000px"))),
        tabPanel("Spatial unexplained residual plot", 
                 div(plotOutput("spatplot2", height="1000px")))
      )
    ),
    
    conditionalPanel(
      condition = "input.eval == 'error_scores'",
      tabsetPanel(
        tabPanel("Scores", tableOutput("scores")),
        tabPanel("Running score plot", 
                 div(plotOutput("runningPlot", height = "1000px")))
      )
    )
  )
))

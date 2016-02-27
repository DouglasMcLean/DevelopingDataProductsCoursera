# Compound Poisson Distribution R-shiny application: ui.R
# Douglas McLean
# 26/2/16

library(shiny)

# Define UI for Gamma-Event Compound Poisson distribution application 
shinyUI(fluidPage(
    
  # Application title
  titlePanel("Gamma-Event Compound Poisson Distributions"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
      radioButtons("dist", "Distribution type:"  ,
                   c("Poisson"          = "pois" ,
                     "Gamma"            = "gamma",
                     "CompoundPoisson"  = "cp"   ,
                     "ConditionalEvent" = "ce"    )),
      br(),
      
      numericInput("lambda", label = h5("Poisson Rate"),
                   value = 1, min = 0.01, max = 100 ),
      numericInput("k", label = h5("Gamma Shape"),
                   value = 5, min = 1, max = 100 ),
      numericInput("theta", label = h5("Gamma Scale"),
                   value = 1, min = 0.01, max = 100 ),
      numericInput("ntrunc", label = h5("Series Truncation"),
                   value = 25, min = 1, max = 100, step = 1),
      numericInput("n", label = h5("Grid Size/#Simulations"),
                   value = 100, min = 1, step = 1),
      
      br(),
      
      sliderInput("Range", label = h5("Range"), min = 0, 
                  max = 100, value = c(0, 30)),
      sliderInput("Event_Range", label = h5("Event Range"), min = 0, 
                  max = 35, value = c(0, 10), step = 1),
      sliderInput("y", label = h5("Sample y Value"), min = 0, 
                  max = 100, value = 1)
    ),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    # tableOutput("table")
    mainPanel(
      tabsetPanel(type = "tabs", 
        tabPanel("Theoretical Densities", plotOutput("plot")            ), 
        tabPanel("Simulations",           plotOutput("simulations")     ), 
        tabPanel("Numerical Summary",     verbatimTextOutput("summary") )
      )
    )
  )
))

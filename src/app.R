#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
# Define UI for application that draws a histogram
ui <- fluidPage(
   theme = shinytheme("slate"),
   # Application title
   titlePanel("Monte Carlo Integration Calculator"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        radioButtons("dist", "Distribution type:",
                     c("Normal" = "norm",
                       "Uniform" = "unif",
                       "Log-normal" = "lnorm",
                       "Exponential" = "exp",
                       "Geometric" = "geo",
                       "Binomial" = "binom",
                       "Poisson" = "poisson")
                     ),
      
        conditionalPanel("input.dist == 'geo'",
                          numericInput("geop","Enter the probability value:","")),
        conditionalPanel("input.dist == 'binom'",
                         numericInput("size", "Enter the size:","")),
        conditionalPanel("input.dist == 'binom'",
                         numericInput("bp","Enter the probability value:","")),
        conditionalPanel("input.dist == 'poisson'",
                          numericInput("lambda","Enter the lambda value:","")),
        
        
        # br() element to introduce extra vertical spacing ----
        br(),

        textInput("text", "Enter the function:"),
        numericInput("iterations", "Input the number of iterations:", "1"),
        numericInput("leftb", "Left Bound:", "0"),
        numericInput("rightb", "Right Bound:", "0")
        
      ),
      #tags$head(
        #  tags$style(HTML(
        #    ".shiny-output-error { visibility: hidden; },
         #   .shiny-output-error:before { visibility: hidden; }"
         # )
      #  )
      #),
      # Show a plot of the generated distribution
      mainPanel(
        verbatimTextOutput("text"),
        verbatimTextOutput("theta.hat"),
        verbatimTextOutput("actual"),
        plotOutput(outputId = "plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$text <- renderText({
     paste("Function:", input$text)
     
   })
   formula = function(){
     req(input$iterations)
     req(input$dist)

     
     m=input$iterations
     left_bound = input$leftb
     right_bound = input$rightb
     ndist=input$dist
     ngeop=input$geop
     nsize=input$size
     nbp=input$bp
     nlambda=input$lambda
     
     if(ndist == "unif")({
       x=runif(m, min = left_bound, max = right_bound)
     })
     else if(ndist == "norm")({
       x=rnorm(m, left_bound, right_bound)
     })
     else if(ndist == "geo")({
       x=rgeom(m, ngeop) #n,prob
     })
     else if(ndist == "binom")({
       x=rbinom(m,nsize,nbp) #n,size,prob
     })
     else if(ndist == "exp")({
       x=rexp(m)
     })
     else if(ndist == "poisson")({
       x=rpois(m,nlambda) #n,lambda
     })
     else if(ndist == "lnorm")({
       x=rlnorm(m)
     })
     
     req(input$text)
     req(input$rightb)
     req(input$leftb)
     
     ntext <- input$text
     nrightb <- parse(text=input$rightb)
     nleftb <- parse(text=input$leftb)
     nntext = parse(text = ntext)
     mean(eval(nntext))*(eval(nrightb)-eval(nleftb))
     
   }
  
   integral = function(){
     req(input$text)
     req(input$rightb)
     req(input$leftb)
     req(input$iterations)
     req(input$dist)
     
     m=input$iterations
     ndist=input$dist
     ngeop=input$geop
     nsize=input$size
     nbp=input$bp
     nlambda=input$lambda
     
     ntext <- input$text
     nrightb <- parse(text=input$rightb)
     nleftb <- parse(text=input$leftb)
     nntext = parse(text = ntext)
     integrand <- function(x){ eval(parse(text = input$text))}
     #Mu1b <- Vectorize(integrand, "x")
     val = integrate(integrand, lower = eval(nleftb), upper = eval(nrightb))
     val[1]
   }
   
   output$theta.hat = renderText({
     paste("Estimate:", formula())
   })
   
   output$actual = renderText({
     paste("Actual:", integral())
   })
   
   library(ggplot2)
   output$plot = renderPlot({
     actual_value = integral() # actual value of the integral
     simulation_data = vector(length = input$iterations)
     for(i in 1:length(simulation_data))
       simulation_data[i] = formula()
     
     estimation_data = data.frame(seq(1:input$iterations), simulation_data)
     names(estimation_data) = c("Simulation", "Value")
     # print(estimation_data)
     
     values_plot = ggplot(estimation_data) +
       geom_point(aes(Simulation, Value), color = "blue") + 
       geom_hline(yintercept = as.numeric(actual_value[1]), color = "red") + 
       labs(title = "Scatterplot of Parameter Estimations", x = "Simulation", y = "Value")
     
     values_plot
   })
}

# Run the application 
shinyApp(ui = ui, server = server)


#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinythemes)
# Define UI for application that draws a histogram
# library(dygraphs) # optional, used for dygraphs

# Header elements for the visualization
header <- dashboardHeader(title = "Tools", disable = FALSE)

# Sidebar elements for the search visualizations
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text = "Bootstrap and Jackknife",  tabName = "bootknife"),
    menuItem(text="Monte Carlo Integration", tabName = "mci")
  ) # /menuItem
  # this is where other menuItems & menuSubItems would go
) # /sidebarMenu
# /dashboardSidebar

#Body elements for the search visualizations.
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "mci",
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
              mainPanel(
                verbatimTextOutput("text"),
                verbatimTextOutput("theta.hat"),
                verbatimTextOutput("actual"),
                plotOutput(outputId = "plot")
              )
            )
    ),
    tabItem(tabName = "bootknife",
            # Sidebar with a slider input for number of bins 
            titlePanel("Bootstrap"),
            
            # Sidebar with a slider input for number of bins 
            sidebarLayout(
              sidebarPanel(
                sliderInput("repSize",
                            "Size of Boostrap Replicates",
                            min = 1000,
                            max = 10000,
                            value = 100),
                checkboxGroupInput(inputId = "ci", label="Types of Confidence Intervals:", choiceNames = c("Percentile", "Normal", "Bca", "Basic"),
                                   choiceValues = c("perc", "norm", "bca", "basic"), selected = c("perc")),
                radioButtons("estimator", "Choose Estimator:",
                             c("Mean" = "mean",
                               "Median" = "median"
                             ))
              ),
              
              mainPanel(
                plotOutput("distPlot"),
                tags$p("Useful Statistics"),
                verbatimTextOutput("stats"),
                verbatimTextOutput("CI")
              )
            )
    )
  ) # /tabItems
) # /dashboardBody

dashboardPage(header, sidebar, body, skin = "black")

ui <- fluidPage(
  
  theme = shinytheme("slate"),
  # Application title
  titlePanel("Monte Carlo Integration Calculator"),
  dashboardPage(header, sidebar, body, skin = "black")
  
)

getData = function (repSize, estimator) {
  library(bootstrap)
  library(boot)
  data(law, package = "bootstrap")
  r <- function(x, i) { #want correlation of columns 1 and 2 
    cor(x[i,1], x[i,2]) 
  }
  
  m = function(x,i){
    mean(x[i])
  }
  
  md = function(x,i){
    median(x[i])
  }
  
  var = function(x,i){
    var(x[i])
  }
  stat = m
  if(estimator == "mean"){
    stat = m
  } else if(estimator == "median")
    stat = md
  else
    stat = r
  print(law$GPA)
  results <- boot(data = law$GPA, statistic = stat, R = repSize)
  return (list("dt" = results))
  
}

formatCI = function(ci_obj) {
  bca_vals = c(1.5,2.34)
  studt_vals = c(3.14,4.18)
  df2 = data.frame(bca_vals, studt_vals)
  row.names(df2) = c("bca", "studt")
  colnames(df2) = c("lower", "upper")
  num_sim = 500 
  str1 = "BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS\nBased on "
  str2 = " bootstrap replicates\n\nIntervals:\n"
  str7 = "Level\t"
  types = "BCA\t\tStudent-t\n"
  str3 = "95%\t("
  str4 = ", "
  str5 = ")\t("
  str6 = ")"
  
  
  cat(paste(str1, num_sim, str2, str3, bca_vals[1], str4, bca_vals[2], str5, studt_vals[1], str4, studt_vals[2], str6))
}


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
    estimate = try(mean(eval(nntext))*(eval(nrightb)-eval(nleftb)))
    if(class(estimate) == "try-error") 
      print("fuck")
    else
      estimate
      
    
  }
  
  integral = function(){
    req(input$text)
    req(input$rightb)
    req(input$leftb)
    
    ntext <- input$text # text expression for function to integrate
    nrightb <- parse(text=input$rightb) # upper bound
    nleftb <- parse(text=input$leftb) # lower bound
    integrand <- function(x){
      eval(parse(text = input$text))
    }
    
    val = try(integrate(integrand, lower = eval(nleftb), upper = eval(nrightb)))
    if(class(val) == "try-error")
      print("Integral cannot be computed")
    else
      val[1]
  }
  
  output$theta.hat = renderText({
    paste("Monte Carlo Estimate:", formula())
  })
  
  output$actual = renderText({
    paste("Actual Value:", integral())
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
  
  
  dt = reactive ({
    getData(input$repSize, input$estimator)
  })
  
  output$distPlot <- renderPlot({
    data = dt()$dt
    ci = dt()$ci
    #hist(data$t, col='darkgray', border = 'white', main ="Histogram of the Estimator", xlab = "Estimator" )
    res = data.frame(data$t)
    ggplot(res) + geom_histogram() + xlab("Estimator")
    #abline(v=mean(data$t), col="red")
    #abline(v=median(data$t), col="black", lty=2)
    #legend("topleft", 
    #      c("mean", "median"),
    #     lty=c(1, 2), 
    #    col=c("red","black"), 
    #   bty = "n")
  })
  
  output$stats = renderPrint({
    data = dt()$dt
    summary(data$t)
    
  })
  
  output$CI = renderPrint({
    estimator = dt()$dt
    ci_types = input$ci
    boot.ci(estimator, type=ci_types)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
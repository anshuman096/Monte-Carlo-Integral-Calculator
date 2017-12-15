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
library(ggplot2)

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
                             c("Uniform" = "unif",
                               "Normal" = "norm",
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
                numericInput("leftb", "Left Bound:", "0"),
                numericInput("rightb", "Right Bound:", "0"),
                numericInput("iterations", "Input the number of iterations:", "1")
              ),
              mainPanel(
                verbatimTextOutput("text"),
                verbatimTextOutput("theta.hat"),
                verbatimTextOutput("actual"),
                plotOutput(outputId = "distribution_plot"),
                plotOutput(outputId = "plot")
              )
            )
    ),
    tabItem(tabName = "bootknife",
            titlePanel("Non Parametric Bootstrap and Jackknife"),
            
            # Sidebar layout with input and output definitions ----
            sidebarLayout(
              
              # Sidebar panel for inputs ----
              sidebarPanel(
                
                # Input: Select a file ----
                fileInput("file1", "Choose CSV File",
                          multiple = TRUE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                sliderInput("repSize",
                            "Size of Boostrap Replicates",
                            min = 1000,
                            max = 10000,
                            value = 1000),
                checkboxGroupInput(inputId = "ci", label="Types of Confidence Intervals (Only for Bootstrap):", choiceNames = c("Percentile", "Normal", "Bca", "Basic"),
                                   choiceValues = c("perc", "norm", "bca", "basic"), selected = c("perc")),
                radioButtons("estimator", "Choose Estimator:",
                             c("Mean" = "mean",
                               "Median" = "median",
                               "Variance" =  "var"
                             )),
                radioButtons("method", "Choose Method: ",
                             c("Bootstrap" = "bt",
                               "Jackknife" = "jk"))
              ),
              
              
              
              # Main panel for displaying outputs ----
              mainPanel(
                # Output: Data file ----
                tableOutput("contents"),
                uiOutput("checkbox"),
                uiOutput("estimator"),
                uiOutput("submit"),
                plotOutput("distPlot"),
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


getData = function (data, repSize, func) {
  library(boot)
  cat(data[0])
  results <- boot(data = data, statistic = median, R = repSize)
  return (list("dt" = results))
  
}

jackknife = function(x, estimator){ 
  
  
  m = function(x,i){
    mean(x[i])
  }
  
  md = function(x,i){
    median(x[i])
  }
  
  vari = function(x,i){
    var(x[i])
  }
  theta = m
  if(estimator == "mean"){
    theta = m
  } else if(estimator == "median")
    theta = md
  else
    theta = vari
  
  n = length(x)
  theta.hat = theta(x)
  theta.jack = numeric(n)
  for(i in 1:n) {
    theta.jack[i] = theta(x[-i])
  }
  bias = (n-1)*mean(theta.jack)-theta.hat
  se = sqrt((n-1) * mean((theta.jack - mean(theta.jack))^2))
  return (list("values" = theta.jack, "bias"=bias, "se"=se))
}


getData = function (data, repSize, estimator) {
  library(boot)
  
  m = function(x,i){
    mean(x[i])
  }
  
  md = function(x,i){
    median(x[i])
  }
  
  vari = function(x,i){
    var(x[i])
  }
  stat = m
  if(estimator == "mean"){
    stat = m
  } else if(estimator == "median")
    stat = md
  else
    stat = vari
  
  results <- boot(data = data, statistic = stat, R = repSize)
  return (list("dt" = results))
  
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
      print("Cannot be estimated")
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
      print("Integral cannot be computed or diverges")
    else
      val[1]
  }
  
  output$theta.hat = renderText({
    paste("Monte Carlo Estimate:", formula())
  })
  
  output$actual = renderText({
    paste("Actual Value:", integral())
  })
  
  output$distribution_plot = renderPlot({
    req(input$dist)
    req(input$leftb)
    req(input$rightb)
    req(input$text)
    
    sampling_dist = input$dist
    expr = input$text
    left_bound = input$leftb
    right_bound = input$rightb
    
    expression = function(x) { eval(parse(text = input$text)) }
    
    dist_plot = ggplot(data.frame(x = c(left_bound, right_bound)), aes(x)) +
      title("Sampling Area Curve - Sample values are drawn from shaded areas")
    
    vector_vals = seq(as.integer(left_bound), as.integer(right_bound))
    
    if(sampling_dist == "unif") {
      dist_plot + 
        geom_hline(yintercept = as.numeric(right_bound), color = "black") + 
        geom_rect(aes(xmin = left_bound, xmax = right_bound, ymin = 0, ymax = right_bound)) + 
        stat_function(fun = expression, color = "blue") + 
        scale_y_continuous(limits = c(0, right_bound))
    } else if(sampling_dist == "norm") {
      dist_plot + 
        stat_function(fun = dnorm, n = input$iterations, args = list(mean = 0, sd = 1), geom = "area") + 
        stat_function(fun = dnorm, xlim = c(left_bound, right_bound), geom = "area") + 
        stat_function(fun = expression, color = "blue")
    } else if(sampling_dist == "lnorm") {
      dist_plot + 
        stat_function(fun = dlnorm) + 
        stat_function(fun = dlnorm, xlim = c(left_bound, right_bound), geom = "area") + 
        stat_function(fun = expression, color = "blue")
    } else if(sampling_dist == "exp") {
      dist_plot + 
        stat_function(fun = dexp) + 
        stat_function(fun = dexp, xlim = c(left_bound, right_bound), geom = "area") + 
        stat_function(fun = expression, color = "blue")
    } else if(sampling_dist == "geo") {
      sample_vals = data.frame(vector_vals, dgeom(vector_vals, prob = input$geop))
      names(sample_vals) = c("x", "y")
      sample_vals$fill_var = vector(mode = "logical", length = length(sample_vals$x))
      sample_vals$fill_var = TRUE
      
      ggplot(sample_vals, aes(x = x, y = y, group = fill_var)) +
        geom_point(aes(fill = fill_var)) + 
        geom_line(aes(fill = fill_var)) + 
        geom_area() + 
        scale_fill_brewer() + 
        stat_function(fun = expression, color = "blue") + 
        labs(title = "Sampling Area Curve - Sample values are drawn from shaded areas", x = "x", y = "y")
      
    } else if(sampling_dist == "binom") {
      sample_vals = data.frame(vector_vals, dbinom(vector_vals, size = input$size, prob = input$bp))
      names(sample_vals) = c("x", "y")
      sample_vals$fill_var = vector(mode = "logical", length = length(sample_vals$x))
      sample_vals$fill_var = TRUE
      
      ggplot(sample_vals, aes(x = x, y = y, group = fill_var)) +
        geom_point(aes(fill = fill_var)) + 
        geom_line(aes(fill = fill_var)) + 
        geom_area() + 
        scale_fill_brewer() + 
        stat_function(fun = expression, color = "blue") + 
        labs(title = "Sampling Area Curve - Sample values are drawn from shaded areas", x = "x", y = "y")
      
    } else {
      sample_vals = data.frame(vector_vals, dpois(vector_vals, lambda = input$lambda))
      names(sample_vals) = c("x", "y")
      sample_vals$fill_var = vector(mode = "logical", length = length(sample_vals$x))
      sample_vals$fill_var = TRUE
      
      ggplot(sample_vals, aes(x = x, y = y, group = fill_var)) +
        geom_point(aes(fill = fill_var)) + 
        geom_line(aes(fill = fill_var)) + 
        geom_area() + 
        scale_fill_brewer() + 
        stat_function(fun = expression, color = "blue") + 
        labs(title = "Sampling Distribution Curve", x = "x", y = "y")
    }
    
    
  })
  
  
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
  
  
  df = data.frame()
  
  output$contents <- renderTable({
    req(input$file1)
    df <- read.csv(input$file1$datapath)
    head(df)
  })
  
  output$checkbox <- renderUI({
    req(input$file1)
    df <- read.csv(input$file1$datapath)
    chvals = names(df)
    chval_ans = c()
    for (i in 1:ncol(df))
    {
      if (is.numeric(df[1, i]))
        chval_ans = c(chval_ans, chvals[i])
    }
    radioButtons("data", "Choose Data Column",choiceNames = chval_ans, choiceValues = chval_ans, inline=T)
    #checkboxGroupInput("data", label="Choose Data Column", choiceNames = names(df),
    #choiceValues = names(df))
  })
  
  output$estimator = renderUI({
    req(input$df)
  })
  
  output$submit = renderUI({
    req(input$data)
    actionButton("submit", "Submit")
  })
  
  dt = eventReactive(input$submit, {
    data <- read.csv(input$file1$datapath)
    col = input$data
    print(data[,col])
    data[,col]
    
  })
  
  results = reactive({
    req(input$method)
    if(input$method == "bt")
      return (getData(dt(), input$repSize, input$estimator))
    else
      return (jackknife(dt(), input$estimator))
  })
  
  
  bootstrapData = function(){
    res = results()$dt
    x_label = paste("Estimator =", input$estimator)
    main_lab = paste("Histogram of the", input$estimator, "of the Bootstrap samples")
    hist(res$t, main =main_lab, xlab = x_label, breaks=40 , col=rgb(0.2,0.8,0.5,0.5) , border=F)
    abline(v=median(res$t), col="black", lty=2)
    abline(v=mean(res$t), col="red", lty=1)
    legend("topleft", 
           c("mean", "median"),
           lty=c(1, 2), 
           col=c("red","black"), 
           bty = "n")
  }
  
  
  jkData = function() {
    res = results()
    hist(res$values, main ="Histogram of the Estimator", xlab = "Estimator",  breaks=40 , col=rgb(0.2,0.8,0.5,0.5) , border=F)
    abline(v=median(res$values), col="black", lty=2)
    abline(v=mean(res$values), col="red", lty=1)
    legend("topleft", 
           c("mean", "median"),
           lty=c(1, 2), 
           col=c("red","black"), 
           bty = "n")
  }
  
  output$distPlot <- renderPlot({
    #data = getData(dt(), input$repSize)$dt
    if(input$method == "bt")
      return (bootstrapData())
    else
      return (jkData())
  })
  
  output$stats = renderPrint({
    if(input$method == 'bt'){
      data = results()$dt
      s_err = sqrt(var(data$t))
      bi = mean(data$t) - data$t0
      m = mean(data$t)
      md = median(data$t)
      dtf = data.frame(s_err, bi, m, md)
      colnames(dtf) = c("Std Error", "Bias", "Mean", "Median")
      return (c("Std Error" = s_err, "Bias" = bi, "Mean" = m, "Median" = md))
    }
  })
  output$CI = renderPrint({
    if(input$method == 'bt'){
      data = results()$dt
      #res = boot.ci(results()$dt, type=input$ci)
      #res[c(4)]
      return (boot.ci(results()$dt, type=input$ci))
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
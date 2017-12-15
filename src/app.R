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


# Header elements for the visualization
header <- dashboardHeader(title = "Tools", disable = FALSE)

# Sidebar elements for the search visualizations
sidebar <- dashboardSidebar(sidebarMenu(
  menuItem(text = "Monte Carlo Integration", tabName = "mci"),
  menuItem(text = "Inverse Transform", tabName = "inverseTransform"),
  menuItem(text = "Accept Reject", tabName = "acceptReject"),
  menuItem(text = "Bootstrap and Jackknife",  tabName = "bootknife"),
  menuItem(text = "Permutation Testing",  tabName = "permTest")
  
))

#Body elements for the search visualizations.
body <- dashboardBody(tabItems(
  #MCI Application
  tabItem(tabName = "mci",
          span(titlePanel("Monte Carlo Integration Calculator"), style="color:black"),
          sidebarLayout(
            sidebarPanel(
              radioButtons(
                "dist",
                "Distribution type:",
                c(
                  "Uniform" = "unif",
                  "Normal" = "norm",
                  "Log-normal" = "lnorm",
                  "Exponential" = "exp",
                  "Geometric" = "geo",
                  "Binomial" = "binom",
                  "Poisson" = "poisson"
                )
              ),
              
              conditionalPanel(
                "input.dist == 'geo'",
                numericInput("geop", "Enter the probability value:", "")
              ),
              conditionalPanel(
                "input.dist == 'binom'",
                numericInput("size", "Enter the size:", "")
              ),
              conditionalPanel(
                "input.dist == 'binom'",
                numericInput("bp", "Enter the probability value:", "")
              ),
              conditionalPanel(
                "input.dist == 'poisson'",
                numericInput("lambda", "Enter the lambda value:", "")
              ),
              
              
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
          )),
  #Inverse Transform 
  tabItem(tabName = "inverseTransform",
          span(titlePanel("Inverse Transformation Method"), style="color:black"),
          
          sidebarLayout(
            sidebarPanel(
              textInput("cdf", "Enter the CDF:"),
              textInput("pdf", "Enter the PDF if the CDF is not available:"),
              numericInput("leftb", "Left Bound:", "0"),
              numericInput("rightb", "Right Bound:", "0"),
              numericInput("iterations", "Input the number of iterations:", "1")
              
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
              plotOutput(outputId = "invHist")
            )
          )
  ),
  #Accept Reject 
  tabItem(tabName = "acceptReject",
          span(titlePanel("Acceptance Rejection Method"), style="color:black"),
          
          sidebarLayout(
            sidebarPanel(
              
              # br() element to introduce extra vertical spacing ----
              br(),
              
              textInput("text", "Enter the function:"),
              numericInput("leftb", "Left Bound:", "0"),
              numericInput("rightb", "Right Bound:", "0"),
              numericInput("iterations", "Input the number of iterations:", "1")
            ),
            mainPanel(
              plotOutput(outputId = "acceptrejectplot")
            )
          )
  ),
  
  #Bootknife Application
  tabItem(
    tabName = "bootknife",
    theme = shinytheme("superhero"),
    # App title ----
    span(titlePanel("Non Parametric Bootstrap and Jackknife"), style="color:black"),
    
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
                           choiceValues = c("Percentile", "Normal", "BCA", "Basic"), selected = c("BCA")),
        radioButtons("estimator", "Choose Estimator:",
                     c("Mean" = "mean",
                       "Median" = "median",
                       "Variance" =  "var"
                     )),
        radioButtons("method", "Choose Method: ",
                     c("Bootstrap" = "bt",
                       "Jackknife" = "jk")),
        width = 4
      ),
      
      
      
      # Main panel for displaying outputs ----
      mainPanel(
        # Output: Data file ----
        tableOutput("contents"),
        uiOutput("checkbox"),
        uiOutput("estimator"),
        uiOutput("submit"),
        plotOutput("distPlot"),
        tableOutput("stats"),
        tableOutput("CI"),
        width = 5
        
      )
    )
  ),
  #Permutation Testing
  tabItem(tabName = "permTest",
          theme = shinytheme("superhero"),
          # App title ----
          span(titlePanel("Permutation Tests"), style="color:black"),
          
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
                          "Size of the Replicates",
                          min = 999,
                          max = 2000,
                          value = 200),
              checkboxGroupInput(inputId = "ci", label="Types of Confidence Intervals (Only for Bootstrap):", choiceNames = c("Percentile", "Normal", "Bca", "Basic"),
                                 choiceValues = c("Percentile", "Normal", "BCA", "Basic"), selected = c("BCA")),
              radioButtons("test", "Choose the test you would like to perform:",
                           c("Two Sample t-Test" = "tt",
                             "Two Sample K-S Test" = "kst",
                             "Two-sample Cramer-von Mises" =  "cvmt"
                           )),
              width = 3
            ),
            
            
            
            # Main panel for displaying outputs ----
            mainPanel(
              # Output: Data file ----
              tableOutput("contents2"),
              uiOutput("checkbox2"),
              uiOutput("selection"),
              uiOutput("submit2"),
              plotOutput("histPlot"),
              
              plotOutput("ecdf1"),
              tableOutput("summary1"),
              plotOutput("ecdf2"),
              tableOutput("summary2"),
              width = 5
              
              
            )
          )
  )
  
) # /tabItems
)# /dashboardBody

ui <- fluidPage(
  theme = shinytheme("slate"),
  # Application title
  titlePanel("Visualization Tools for Statistical Computing"),
  dashboardPage(header, sidebar, body, skin = "black")
  
)


cvmts.test <- function(x,y){
  nx <- length(x)
  ny <- length(y)
  sx <- ecdf(x)(x) - ecdf(y)(x)
  sy <- ecdf(x)(y) - ecdf(y)(y)
  return(nx*ny*(sum(sx^2)+sum(ny^2))/(nx+ny)^2)
}

getData = function (data, repSize, func) {
  library(boot)
  
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
  
  simData = function(){
    nrightb <- parse(text=input$rightb) # upper bound
    nleftb <- parse(text=input$leftb) # lower bound
    m=input$iterations
    
    if(input$cdf != ""){
      og_f = function(x){
        eval(parse(text = input$cdf))
      }
    }
    else if(input$pdf !=""){
      og_f = input$pdf
      integrand <- function(x){
        eval(parse(text = input$pdf))
      }
      
      val = try(integrate(integrand, lower=0, upper=y))
      print(val)
      if(class(val) == "try-error")
        print("Integral cannot be computed")
      else
        og_f = function(x){ val[1] }
    }
    
    
    
    inv_f = inverse(og_f, lower = input$leftb,  upper =  input$rightb)
    simulation_data = vector(length = input$iterations)
    for(i in 1:length(simulation_data)){
      u = runif(1,0,1)
      simulation_data[i] = inv_f(u)
    }
    simulation_data
  }
  
  output$invHist = renderPlot({
    arr = simData()
    hist(arr, prob=TRUE)
  })
  
  acceptreject=function(n){
    req(input$text)
    req(input$rightb)
    req(input$leftb)
    #n <- 1000
    k <- 0 #counter for accepted
    j <- 0 #iterations
    y <- numeric(n)
    nrightb <- input$rightb
    nleftb <- input$leftb
    func=function(x) {
      eval(parse(text = input$text))
    }
    
    integrand <- function(x){
      eval(parse(text = input$text))
    }
    #func
    #c=optimize(integrand, interval=c(input$leftb, input$rightb), maximum=TRUE)
    c <- optim(input$leftb,integrand, method="SANN",control=list(fnscale=-1)) 
    #print(resopt)
    x <- runif(1) #random variate from g
    while (k < n) {
      u <- runif(1)
      j <- j + 1
      x <- runif(1) #random variate from g
      if ((func(x)/c[[1]]) > u) {
        #we accept x
        k <- k + 1
        y[k] <- x
      }
    }
    y
  }
  
  output$acceptrejectplot=renderPlot({
    hist(acceptreject(1000),freq=F)
  })
  
  output$text <- renderText({
    paste("Function:", input$text)
  })
  
  formula = function() {
    req(input$iterations)
    req(input$dist)
    
    
    m = input$iterations
    left_bound = input$leftb
    right_bound = input$rightb
    ndist = input$dist
    ngeop = input$geop
    nsize = input$size
    nbp = input$bp
    nlambda = input$lambda
    
    if (ndist == "unif")
      ({
        x = runif(m, min = left_bound, max = right_bound)
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
  
  integral = function() {
    req(input$text)
    req(input$rightb)
    req(input$leftb)
    
    ntext <- input$text # text expression for function to integrate
    nrightb <- parse(text = input$rightb) # upper bound
    nleftb <- parse(text = input$leftb) # lower bound
    integrand <- function(x) {
      eval(parse(text = input$text))
    }
    
    val = try(integrate(integrand,
                        lower = eval(nleftb),
                        upper = eval(nrightb)))
    if (class(val) == "try-error")
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
    
    expression = function(x) {
      eval(parse(text = input$text))
    }
    
    dist_plot = ggplot(data.frame(x = c(left_bound, right_bound)), aes(x)) +
      title("Sampling Area Curve - Sample values are drawn from shaded areas")
    
    vector_vals = seq(as.integer(left_bound), as.integer(right_bound))
    
    if (sampling_dist == "unif") {
      dist_plot +
        geom_hline(yintercept = as.numeric(right_bound), color = "black") +
        geom_rect(aes(
          xmin = left_bound,
          xmax = right_bound,
          ymin = 0,
          ymax = right_bound
        )) +
        stat_function(fun = expression, color = "blue") +
        scale_y_continuous(limits = c(0, right_bound))
    } else if (sampling_dist == "norm") {
      dist_plot +
        stat_function(
          fun = dnorm,
          n = input$iterations,
          args = list(mean = 0, sd = 1),
          geom = "area"
        ) +
        stat_function(
          fun = dnorm,
          xlim = c(left_bound, right_bound),
          geom = "area"
        ) +
        stat_function(fun = expression, color = "blue")
    } else if (sampling_dist == "lnorm") {
      dist_plot +
        stat_function(fun = dlnorm) +
        stat_function(
          fun = dlnorm,
          xlim = c(left_bound, right_bound),
          geom = "area"
        ) +
        stat_function(fun = expression, color = "blue")
    } else if (sampling_dist == "exp") {
      dist_plot +
        stat_function(fun = dexp) +
        stat_function(
          fun = dexp,
          xlim = c(left_bound, right_bound),
          geom = "area"
        ) +
        stat_function(fun = expression, color = "blue")
    } else if (sampling_dist == "geo") {
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
      
    } else if (sampling_dist == "binom") {
      sample_vals = data.frame(vector_vals,
                               dbinom(vector_vals, size = input$size, prob = input$bp))
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
    for (i in 1:length(simulation_data))
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
    res3 = boot.ci(results()$dt, type=c("bca"))
    r3_lower = res3[[c(4)]][4]
    r3_upper = res3[[c(4)]][5]
    abline(v=r3_lower, col='black', lty=4)
    abline(v=r3_upper, col='black', lty=4)
    legend("topleft", 
           c("mean", "median", "BCa CI"),
           lty=c(1, 2, 4), 
           col=c("red","black", "black"), 
           bty = "o")
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
  
  make_df2 = function(){
    data = results()$dt
    s_err = sqrt(var(data$t))
    bi = mean(data$t) - data$t0
    m = mean(data$t)
    md = median(data$t)
    dtf2 = data.frame(s_err, bi, m, md)
    colnames(dtf2) = c("Std Error", "Bias", "Mean", "Median")
    return (dtf2)
  }
  
  make_df_jk = function(){
    data = results()
    bi = data$bias
    se_val = data$se
    m = mean(data$values)
    md = median(data$values)
    dtf3 = data.frame(se_val, bi, m, md)
    colnames(dtf3) = c("Std Error", "Bias", "Mean", "Median")
    return (dtf3)
  }
  
  output$stats = renderTable({
    if(input$method == 'bt'){
      dtf2 = make_df2()
      #return (c("Std Error" = s_err, "Bias" = bi, "Mean" = m, "Median" = md))
    }
    else {
      dtf3 = make_df_jk()
    }
    #return (c("Std Error" = se_val, "Bias" = bi, "Mean" = m, "Median" = md))
  })
  
  make_df = function(){
    req(input$ci)
    data = results()$dt
    res1 = boot.ci(results()$dt, type=c("perc"))
    r1_lower = res1[[c(4)]][4]
    r1_upper = res1[[c(4)]][5]
    r1 = c(r1_lower, r1_upper)
    res2 = boot.ci(results()$dt, type=c("norm"))
    r2_lower = res2[[c(4)]][2]
    r2_upper = res2[[c(4)]][3]
    r2 = c(r2_lower, r2_upper)
    res3 = boot.ci(results()$dt, type=c("bca"))
    r3_lower = res3[[c(4)]][4]
    r3_upper = res3[[c(4)]][5]
    r3 = c(r3_lower, r3_upper)
    res4 = boot.ci(results()$dt, type=c("basic"))
    r4_lower = res4[[c(4)]][4]
    r4_upper = res4[[c(4)]][5]
    r4 = c(r4_lower, r4_upper)
    dtf = data.frame(r1, r2, r3, r4)
    row.names(dtf) = c("Lower Limit", "Upper Limit")
    colnames(dtf) = c("Percentile", "Normal", "BCA", "Basic")
    return (dtf[input$ci])
  }
  
  output$CI = renderTable({
    if(input$method == 'bt'){
      dtf = make_df()
      #return (c("res1" = r1, "res2" = r2, "res3" = r3, "res4" = r4))
      #return (boot.ci(results()$dt, type=input$ci))
    }
    
  }, rownames = TRUE)
  

  cramer = function(x,y) {
    #Performing the permutation test
    n1 = length(x)
    n2 = length(y)
    R <- input$repSize             #number of replicates
    z <- c(x,y)         #pooled sample
    K <- 1:(n1+n2)
    reps <- numeric(R)   #storage for replicates
    t0 <- cvmts.test(x, y)
    for (i in 1:R) {
      #generate indices k for the first sample
      k <- sample(K, size = max(n1,n2), replace = FALSE)
      x1 <- z[k]
      y1 <- z[-k]          #complement of x1
      reps[i] <- cvmts.test(x1, y1)
    }
    p <- mean(c(t0, reps) >= t0)
    return (list("reps" = reps, "p"=p, "t0"=t0))
  }
  
  tt = function(x,y) {
    #Performing the permutation test
    n1 = length(x)
    n2 = length(y)
    R <- input$repSize             #number of replicates
    z <- c(x,y)         #pooled sample
    K <- 1:(n1+n2)
    reps <- numeric(R)   #storage for replicates
    t0 <- t.test(x, y)$statistic
    for (i in 1:R) {
      #generate indices k for the first sample
      k <- sample(K, size = max(n1,n2), replace = FALSE)
      x1 <- z[k]
      y1 <- z[-k]          #complement of x1
      reps[i] <- t.test(x1, y1)$statistic
    }
    p <- mean(c(t0, reps) >= t0)
    return (list("reps" = reps, "p"=p, "t0"=t0))
  }
  
  kst = function(x,y) {
    options(warn = -1)
    #Performing the permutation test
    n1 = length(x)
    n2 = length(y)
    R <- input$repSize             #number of replicates
    z <- c(x,y)         #pooled sample
    K <- 1:(n1+n2)
    reps <- numeric(R)   #storage for replicates
    t0 <- ks.test(x, y,exact = F)$statistic
    for (i in 1:R) {
      #generate indices k for the first sample
      k <- sample(K, size = max(n1,n2), replace = FALSE)
      x1 <- z[k]
      y1 <- z[-k]          #complement of x1
      reps[i] <- ks.test(x1, y1, exact = F)$statistic
    }
    p <- mean(c(t0, reps) >= t0)
    return (list("reps" = reps, "p"=p, "t0"=t0))
  }
  
  output$contents2 <- renderTable({
    req(input$file1)
    df <- read.csv(input$file1$datapath)
    head(df)
  })
  
  output$checkbox2 <- renderUI({
    req(input$file1)
    df <- read.csv(input$file1$datapath)
    chvals = names(df)
    chval_ans = c()
    for (i in 1:ncol(df))
    {
      if (is.numeric(df[1, i]))
        chval_ans = c(chval_ans, chvals[i])
    }
    radioButtons("col1", "Choose Data Column 1",choiceNames = chval_ans, choiceValues = chval_ans, inline=T)
    
  })
  
  output$selection = renderUI({
    req(input$col1)
    df <- read.csv(input$file1$datapath)
    chvals = names(df)
    chval_ans = c()
    for (i in 1:ncol(df))
    {
      if (is.numeric(df[1, i]))
        chval_ans = c(chval_ans, chvals[i])
    }
    radioButtons("col2", "Choose Data Column 2",choiceNames = chval_ans, choiceValues = chval_ans, inline=T)
  })
  
  output$submit2 = renderUI({
    req(input$col1)
    req(input$col2)
    actionButton("submit", "Submit")
  })
  
  dt2 = eventReactive(input$submit, {
    data <- read.csv(input$file1$datapath)
    req(input$col1)
    req(input$col2)
    col = c(input$col1, input$col2)
    
    return (data[,col])
    
  })
  
  getResults = function(x,y) {
    if(input$test == 'tt')
      return(tt(x,y))
    else if(input$test=='kst')
      return (kst(x,y))
    else
      return (cramer(x,y))
  }
  
  
  output$histPlot = renderPlot({
    data = dt2()
    
    col1 = input$col1
    col2 = input$col2
    
    x=data[,col1]
    y=data[,col2]
    
    x = x[!is.na(x)]
    y = y[!is.na(y)]
    
    
    cr = getResults(x,y)
    
    xlab = ""
    obs = ""
    if(input$test == 'tt'){
      xlab = paste("T", "(p = ",cr$p,")")
      obs = "T"
    }
    else if(input$test=='kst'){
      xlab = paste("D", "(p = ",cr$p,")")
      obs = "D"
    }
    else{
      xlab = paste("W", "(p = ",cr$p,")")
      obs="W"
    }
    
    hist(cr$reps, main="", xlab=xlab, breaks="scott" , col=rgb(0.2,0.8,0.5,0.5) , border=F)
    points(cr$t0, 0, cex = 1, pch = 16) 
    legend("topleft", 
           c(paste("Observed", obs)),
           pch=c(16),
           col=c("black"),
           inset=0.05,
           bty = "o")
    
  })
  
  output$ecdf1 = renderPlot({
    
    data = dt2()
    col1 = input$col1
    col2 = input$col2
    
    x=data[,col1]
    y=data[,col2]
    
    x = x[!is.na(x)]
    y = y[!is.na(y)]
    
    plot(ecdf(x), main=paste("Ecdf of", input$col1),col=rgb(0.2,0.8,0.5,0.5), verticals = T)
  })
  
  output$summary1 = renderTable({
    data = dt2()
    col1 = input$col1
    x=data[,col1]
    x = x[!is.na(x)]
    return(as.array(summary(x)))
  })
  
  output$ecdf2 = renderPlot({
    
    data = dt2()
    
    col1 = input$col1
    col2 = input$col2
    
    x=data[,col1]
    y=data[,col2]
    
    x = x[!is.na(x)]
    y = y[!is.na(y)]
    
    plot(ecdf(y), main= paste("Ecdf of", input$col2),col=rgb(0.2,0.8,0.5,0.5), verticals = T)
  })
  
  output$summary2 = renderTable({
    data = dt2()
    col2 = input$col2
    y=data[,col2]
    y = y[!is.na(y)]
    return(as.array(summary(y)))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
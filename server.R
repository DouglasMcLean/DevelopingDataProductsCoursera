# Compound Poisson Distribution R-shiny application: server.R
# Douglas McLean
# 26/2/16

library(shiny)
source("CompoundPoissonFunctions.R")

# Define server logic for Compound Poisson distribution application
shinyServer(function(input, output) {
  
  # Reactive expression to generate the requested distribution.
  # Called whenever the inputs change. 
  # Output for plot uses the value computed from this expression
  datum <- reactive({
    y <- switch(input$dist,
                pois  = seq(from=input$Event_Range[1],to=input$Event_Range[2],by=1),
                gamma = seq(from=input$Range[1],to=input$Range[2],by=1/input$n),
                cp    = seq(from=input$Range[1],to=input$Range[2],by=1/input$n),
                ce    = seq(from=input$Event_Range[1],to=input$Event_Range[2],by=1),
                seq(from=input$Event_Range[1],to=input$Event_Range[2],by=1))

    Density <- switch(input$dist,
                      pois  = dpois(x=y, lambda=input$lambda),
                      gamma = dgamma(x=y, shape=input$k, scale=input$theta),
                      cp    = dcp(y=y, k=input$k, theta=input$theta, lambda=input$lambda, ntrunc=input$ntrunc),
                      ce    = dce(n=y, y=input$y, k=input$k, theta=input$theta, lambda=input$lambda, ntrunc=input$ntrunc),
                      dpois(x=y,lambda=input$lambda))
    
    Density
  })
  

  # Second reactive expression to simulate from the requested distribution.
  # Again, called whenever the inputs change. 
  # Output for plot uses the value computed from this expression
  replicates <- reactive({

    n <- input$n
    
    Simulations <- switch(input$dist,
                      pois  = rpois( n=n, lambda=input$lambda),
                      gamma = rgamma(n=n, shape=input$k, scale=input$theta),
                      cp    = rcp(   n=n, k=input$k, theta=input$theta, lambda=input$lambda, ntrunc=input$ntrunc),
                      ce    = rce(   n=n, y=input$y, k=input$k, theta=input$theta, lambda=input$lambda, ntrunc=input$ntrunc)
                      )
    
    Simulations
  })
  
  
  
  
  # Generate a plot of the density functions
  output$plot <- renderPlot({
    
    y <- switch(input$dist,
                pois  = seq(from=input$Event_Range[1],to=input$Event_Range[2],by=1),
                gamma = seq(from=input$Range[1],to=input$Range[2],by=1/input$n),
                cp    = seq(from=input$Range[1],to=input$Range[2],by=1/input$n),
                ce    = seq(from=input$Event_Range[1],to=input$Event_Range[2],by=1),
                seq(from=input$Event_Range[1],to=input$Event_Range[2],by=1))
    
    n1 <- input$Event_Range[1]
    n2 <- input$Event_Range[2]
    
    max.datum <- max(datum()[y>0])
    
    #switch(input$dist,
    #       pois  = barplot(datum(), names.arg=n1:n2, xlab="n",        main="Unconditional N", ylim=c(0,1)),
    #       gamma = plot(y, datum(), type="l", lwd=2, col="red",       xlab="y", main="Gamma Density"),
    #       cp    = plot(y, datum(), type="l", lwd=2, col="darkgreen", xlab="y", main="Compound Poisson Density", ylim=c(0,max.datum)),
    #       ce    = barplot(datum(), names.arg=n1:n2, xlab="n",        main=paste("Conditional on Y=",input$y,sep=""),ylim=c(0,1))
    #)
    switch(input$dist,
           pois  = barplot(datum(), names.arg=n1:n2, xlab="n",        main="Unconditional N"),
           gamma = plot(y, datum(), type="l", lwd=2, col="red",       xlab="y", main="Gamma Density"),
           cp    = plot(y, datum(), type="l", lwd=2, col="darkgreen", xlab="y", main="Compound Poisson Density", ylim=c(0,max.datum)),
           ce    = barplot(datum(), names.arg=n1:n2, xlab="n",        main=paste("Conditional on Y=",input$y,sep=""))
    )
    
  })
  
  # Generate a plot of the density functions
  output$simulations <- renderPlot({
    
    
    y <- switch(input$dist,
                pois  = seq(from=input$Event_Range[1],to=input$Event_Range[2],by=1),
                gamma = seq(from=input$Range[1],to=input$Range[2],by=1/input$n),
                cp    = seq(from=input$Range[1],to=input$Range[2],by=1/input$n),
                ce    = seq(from=input$Event_Range[1],to=input$Event_Range[2],by=1),
                seq(from=input$Event_Range[1],to=input$Event_Range[2],by=1))
    
    n1 <- input$Event_Range[1]
    n2 <- input$Event_Range[2]
    
    max.datum <- max(datum()[y>0])
    
    
    #switch(input$dist,
    #       pois  = hist(replicates(), main="Unconditional N", col="darkgrey", xlim=c(n1,n2), freq=TRUE),
    #       gamma = hist(replicates(), main="Gamma Density",   col="red"),
    #       cp    = hist(replicates(), main="Compound Poisson Density",  col="darkgreen"),
    #       ce    = hist(replicates(), main=paste("Conditional on Y=",input$y,sep=""), col="darkgrey", xlim=c(n1,n2))
    #)
    switch(input$dist,
           pois  = barplot(summary(as.factor(replicates())), 
                           names.arg=names(summary(as.factor(replicates()))), 
                           xlab="n", main="Unconditional N"),
           gamma = hist(replicates(), main="Gamma Density",   col="red"),
           cp    = hist(replicates(), main="Compound Poisson Density",  col="darkgreen"),
           ce    = barplot(summary(as.factor(replicates())), 
                           names.arg=names(summary(as.factor(replicates()))), 
                           xlab="n", main=paste("Conditional on Y=",input$y,sep=""))
    )
    
  })
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    switch(input$dist,
           pois  = summary(as.factor(replicates())),
           gamma = summary(replicates()),
           cp    = summary(replicates()),
           ce    = summary(as.factor(replicates()))
    )
    
    
  })
  
  # Generate an HTML table view of the data
  output$table <- renderTable({
    data.frame(x=datum())
  })
  
})

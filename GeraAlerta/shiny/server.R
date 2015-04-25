library("shiny")
library("ggplot2")

#ttype <- "Rt"

# Simulation and Shiny Application of Resource Competition
shinyServer(function(input, output) {
  
  # Hit counter
  
  mydata <- reactive({

    apt <- input$ap
    ttype <- input$ttype
    range <- input$range
    
    df <- dfRt[dfRt$APS==apt,]
    list(long=subset(df, (df$time > range[1]) & (df$time <=range[2])), wide=c())
  #list(long=dtest, wide=c())

    })
  
 #  output$datatable <- renderTable(mydata()[["long"]], digits=2)
    
#gf <- ggplot(dsim, aes(x=x, y=lambda, colour=COMPETING))
#gf <- gf + xlab("Number of competing individuals") + geom_line() + facet_grid(PERFORMANCE ~ .) + geom_vline(aes(xintercept = z), vline.data)

  output$graph1 <- renderPlot({

    tt <- mydata()[["long"]]$time
    SE <- mydata()[["long"]]$SE
    dfg <- mydata()[["long"]]
    
    if (input$ttype=="Rt") {
    g <- ggplot(dfg, aes(x=time, y=Rt))
    g <- g+ geom_ribbon(aes(ymin = Rtlr, ymax=Rtur)) + geom_line() 
    brks <- as.integer(ggplot_build(g)$panel$ranges[[1]]$x.labels)
    lbrks <- length(brks)
    if (brks[1]==0)
      brks <- brks[2:lbrks]
    g <- g + scale_x_continuous(breaks=brks, labels=SE[brks]) +
        facet_grid(method ~ .) + ggtitle("Dengue Rt")  
    #g <- g + geom_line() 
  }
  else if (input$ttype=="Incidence") {
    g <- ggplot(dfg, aes(x=time, y=casosm))
    g <- g + geom_line() 
    brks <- as.integer(ggplot_build(g)$panel$ranges[[1]]$x.labels)
    lbrks <- length(brks)
    if (brks[1]==0)
      brks <- brks[2:lbrks]
    g <- g + scale_x_continuous(breaks=brks, labels=SE[brks]) +
      ggtitle("Num. dengue cases")  
  }
  else if (input$ttype=="Level") {
    g <- ggplot(dfg, aes(x=time, y=liv*as.integer(method), group=method))
    g <- g + geom_line(aes(colour=method), size=4)
    brks <- as.integer(ggplot_build(g)$panel$ranges[[1]]$x.labels)
    lbrks <- length(brks)
    if (brks[1]==0)
      brks <- brks[2:lbrks]
    g <- g + scale_x_continuous(breaks=brks, labels=SE[brks]) +
      scale_y_continuous("Intervals", labels=NULL) +
      ggtitle("Rt > 1")
  }
    
    print(g)
  })
    
  
})
  

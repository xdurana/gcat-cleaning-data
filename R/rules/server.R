library(shiny)
library(arules)
library(arulesViz)

source('https://raw.githubusercontent.com/brooksandrew/Rsenal/master/R/rules2df.R')
dataset <- readRDS('data/medsnconditions.Rds')

function(input, output) {
  
  as(dataset, 'transactions')
  
  output$choose_columns <- renderUI({
    checkboxGroupInput("cols", "Choose variables:", 
                       choices  = colnames(dataset),
                       selected = colnames(dataset))
  })
  
  
  output$choose_lhs <- renderUI({
    checkboxGroupInput("colsLHS", "Choose LHS variables:", 
                       choices  = input$cols,
                       selected = input$cols[1])
  })
  
  output$choose_rhs <- renderUI({
    checkboxGroupInput("colsRHS", "Choose RHS variables:", 
                       choices  = input$cols,
                       selected = input$cols[1])
  })
  
  ## Extracting and Defining arules
  rules <- reactive({
    tr <- as(dataset[,input$cols], 'transactions')
    arAll <- apriori(tr, parameter=list(support=input$supp, confidence=input$conf, minlen=input$minL, maxlen=input$maxL))
    
    if(input$rhsv=='Subset' & input$lhsv!='Subset'){
      varsR <- character()
      for(i in 1:length(input$colsRHS)){
        tmp <- with(dataset, paste(input$colsRHS[i], '=', levels(as.factor(get(input$colsRHS[i]))), sep=''))
        varsR <- c(varsR, tmp)
      }
      ar <- subset(arAll, subset=rhs %in% varsR)
      
    } else if(input$lhsv=='Subset' & input$rhsv!='Subset') {
      varsL <- character()
      for(i in 1:length(input$colsLHS)){
        tmp <- with(dataset, paste(input$colsLHS[i], '=', levels(as.factor(get(input$colsLHS[i]))), sep=''))
        varsL <- c(varsL, tmp)
      }
      ar <- subset(arAll, subset=lhs %in% varsL)
      
    } else if(input$lhsv=='Subset' & input$rhsv=='Subset') {
      varsL <- character()
      for(i in 1:length(input$colsLHS)){
        tmp <- with(dataset, paste(input$colsLHS[i], '=', levels(as.factor(get(input$colsLHS[i]))), sep=''))
        varsL <- c(varsL, tmp)
      }
      varsR <- character()
      for(i in 1:length(input$colsRHS)){
        tmp <- with(dataset, paste(input$colsRHS[i], '=', levels(as.factor(get(input$colsRHS[i]))), sep=''))
        varsR <- c(varsR, tmp)
      }
      ar <- subset(arAll, subset=lhs %in% varsL & rhs %in% varsR)
      
    } else {
      ar <- arAll
    }
    quality(ar)$chiSquare <- interestMeasure(ar, method='chiSquare', transactions=tr)
    quality(ar)$conviction <- interestMeasure(ar, method='conviction', transactions=tr)
    #quality(ar)$hyperConfidence <- interestMeasure(ar, method='hyperConfidence', transactions=tr)
    #quality(ar)$cosine <- interestMeasure(ar, method='cosine', transactions=tr)
    #quality(ar)$coverage <- interestMeasure(ar, method='coverage', transactions=tr)
    #quality(ar)$doc <- interestMeasure(ar, method='doc', transactions=tr)
    #quality(ar)$gini <- interestMeasure(ar, method='gini', transactions=tr)
    #quality(ar)$hyperLift <- interestMeasure(ar, method='hyperLift', transactions=tr)
    ar
  })
  
  # Rule length
  nR <- reactive({
    nRule <- ifelse(input$samp == 'All Rules', length(rules()), input$nrule)
  })
  
  ## Grouped Plot #########################
  output$groupedPlot <- renderPlot({
    ar <- rules()
    plot(sort(ar, by=input$sort)[1:nR()], method='grouped', control=list(k=input$k))
  }, height=800, width=800)
  
  ## Graph Plot ##########################
  output$graphPlot <- renderPlot({
    ar <- rules()
    plot(sort(ar, by=input$sort)[1:nR()], method='graph', control=list(type=input$graphType))
  }, height=800, width=800)
  
  ## Scatter Plot ##########################
  output$scatterPlot <- renderPlot({
    ar <- rules()
    plot(sort(ar, by=input$sort)[1:nR()], method='scatterplot')
  }, height=800, width=800)
  
  ## Parallel Coordinates Plot ###################
  output$paracoordPlot <- renderPlot({
    ar <- rules()
    plot(sort(ar, by=input$sort)[1:nR()], method='paracoord')
  }, height=800, width=800)
  
  ## Matrix Plot ###################
  output$matrixPlot <- renderPlot({
    ar <- rules()
    plot(sort(ar, by=input$sort)[1:nR()], method='matrix', control=list(reorder=T))
  }, height=800, width=800)
  
  ## Item Frequency Plot ##########################
  output$itemFreqPlot <- renderPlot({
    trans <- as(dataset[,input$cols], 'transactions')
    itemFrequencyPlot(trans)
  }, height=800, width=800)
  
  ## Rules Data Table ##########################
  output$rulesDataTable <- renderDataTable({
    ar <- rules()
    rulesdt <- rules2df(ar)
    rulesdt
  })
  
  ## Rules Printed ########################
  output$rulesTable <- renderPrint({
    #hack to disply results... make sure this match line above!!
    #ar <- apriori(dataset[,input$cols], parameter=list(support=input$supp, confidence=input$conf, minlen=input$minL, maxlen=input$maxL))
    ar <- rules()
    inspect(sort(ar, by=input$sort))
  })
  
  ## Download data to csv ########################
  output$downloadData <- downloadHandler(
    filename = 'arules_data.csv',
    content = function(file) {
      write.csv(rules2df(rules()), file)
    }
  )
}
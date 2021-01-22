#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(DT)
library(arules)
library(arulesViz)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    dataframe<-reactive({
        req(input$gg)
        read.csv(input$gg$datapath, sep = ",")
        #if(is.null(input$gg))
        #return(NULL)
        #data<-read.csv(input$gg$datapath)
        
    })
    
    output$view <- DT::renderDataTable ({
        if(input$disp == "head")
        {head(dataframe())
        }
        
        else
            dataframe()
        
        #DT::datatable(data, options =  list(lengthMenu = c(5, 30, 50), pageLength = 5))
    })
    
    
    output$sum<-renderPrint({
        
        data1 <- read.transactions(
            input$gg$datapath,
            format = "basket",
            sep = ",",
            rm.duplicates = T)
        
        summary(data1)
    })
    
    output$apri <- renderPrint({
        
        data1<- read.transactions(
            input$gg$datapath,
            format = "basket",
            sep = ",",
            rm.duplicates = T)
        
        rules1 <- apriori(data1, parameter = list(support =as.numeric(input$supo), confidence = as.numeric(input$confi)))
        # interdata <- inspect(rules1)
        #find redundant rules
        rules.sorted = sort(rules1, by ="lift")
        subset.matrix = is.subset(rules.sorted, rules.sorted, sparse = FALSE)
        subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
        redundant = colSums(subset.matrix, na.rm=T) >= 1
        
        #remove redundant rules
        rules.pruned <- rules.sorted[!redundant]
        interdata <- inspect(rules.pruned)
        
        
        # 
    })
    
    output$ec <- DT::renderDataTable(server = FALSE, {
        
        data1<- read.transactions(
            input$gg$datapath,
            format = "basket",
            sep = ",",
        )
        
        freq.items = eclat(data1, parameter=list(support = as.numeric(input$supo)))
        # freq.items <- sort(freq.items, by="support")
        inspectDT(freq.items, options = list(lengthChange = FALSE))
        
        
    })
    
    output$rules<-renderPlot({
        data1<- read.transactions(
            input$gg$datapath,
            format = "basket",
            sep = ",",
            rm.duplicates = T)
        
        rules1 <- apriori(data1, parameter = list(support =as.numeric(input$supo), confidence = as.numeric(input$confi)))
        # interdata <- inspect(rules1)
        #find redundant rules
        rules.sorted = sort(rules1, by ="lift")
        subset.matrix = is.subset(rules.sorted, rules.sorted, sparse = FALSE)
        subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
        redundant = colSums(subset.matrix, na.rm=T) >= 1
        
        #remove redundant rules
        rules.pruned <- rules.sorted[!redundant]
        interdata <- inspect(rules.pruned)
        plot(rules.pruned[1:40], method = "graph")
        
    })
    
    output$group <- renderPlot({
        
        data1<- read.transactions(
            input$gg$datapath,
            format = "basket",
            sep = ",",
            rm.duplicates = T)
        
        rules1 <- apriori(data1, parameter = list(support =as.numeric(input$supo), confidence = as.numeric(input$confi)))
        # interdata <- inspect(rules1)
        #find redundant rules
        rules.sorted = sort(rules1, by ="lift")
        subset.matrix = is.subset(rules.sorted, rules.sorted, sparse = FALSE)
        subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
        redundant = colSums(subset.matrix, na.rm=T) >= 1
        
        #remove redundant rules
        rules.pruned <- rules.sorted[!redundant]
        interdata <- inspect(rules.pruned)
        plot(rules.pruned, method = "group")
        
    })
    
})

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(magrittr)
library(plyr)
library(dplyr)
library(stringr)
library(reshape2)#n R, we can transpose our data very easily
library(tidyr)
library(arules)
library(arulesViz)
library(datasets)
library(ggplot2)
library(plotly)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    tags$h2(""),
    setBackgroundImage(
        src = "https://images.unsplash.com/photo-1610479368542-c037939ead16?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1576&q=80"
    ),
    
    
    # Application title
    
    column(12,titlePanel("Implementasi Algoritma Apriori dan Eclat"),style = {'color:white; '}),
    
    
    
    
    # Sidebar with a slider input for number of bins
    
    sidebarLayout(
        sidebarPanel(
            h2( align = "center", style = "color:black"), width = 3,
            fileInput("gg", "Choose CSV File", 
                      multiple = FALSE ,
                      accept = c("csv", 
                                 "comma-separated-values", 
                                 ".csv")), 
            style = "background-color: #fffdd0; color:blue",
            
            radioButtons("disp", "Display",
                         choices = c(All = "all",
                                     Head = "head"),
                         selected = "all"),
            
            numericInput("supo",
                         "Select Support",
                         0.05,
                         min = 0,
                         max = 1,
            ),
            numericInput("confi",
                         "Select Confidence",
                         0.3,
                         min = 0,
                         max = 1,
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(width = 9, style = "color: black",
                  tabsetPanel(type = "tab", 
                              tabPanel("Tabel", DT::dataTableOutput("view")),
                              tabPanel("Summary", verbatimTextOutput("sum")),
                              tabPanel("Apriori", verbatimTextOutput("apri")),
                              tabPanel("Frequent Item using eclat", DT::dataTableOutput("ec")),
                              tabPanel("Absolute Frequency plot", plotOutput("plot.absolute")),
                              tabPanel("Relative Frequency plot", plotOutput("plot.relative")),
                              tabPanel("Top Ten Apriori Rules With Arrow plots",plotOutput("rules")),
                              tabPanel("Grouped Metrixe Plot", plotOutput("group"))
                              
                              
                              
                              
                  ))
    )
)
)

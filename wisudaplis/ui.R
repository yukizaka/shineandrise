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
        src = "https://cdn.pixabay.com/photo/2020/09/19/10/37/seascape-5584129_960_720.jpg"
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
                         0.035,
                         min = 0,
                         max = 1,
            ),
            numericInput("confi",
                         "Select Confidence",
                         0.6,
                         min = 0,
                         max = 1,
            ),
            tags$strong("Kategori Atribut IPK "),
            br(),
            tags$strong("- P  : IPK ≥ 3,51 "),
            br(),
            tags$strong("- SM : 3,01 ≤ IPK ≤ 3,50"),
            br(),
            tags$strong("- M  : 2,76 ≤ IPK ≤ 3,00"),
            br(),
            tags$strong("- TP : 2,00 ≤ IPK ≤ 2,75"),
            br(),
            br(),
            tags$strong("Kategori Atribut Lama Studi"),
            br(),
            tags$strong("- A  : Lama Studi ≤ 4 th"),
            br(),
            tags$strong("- B  : 4 th < Lama Studi ≤ 5 th"),
            br(),
            tags$strong("- C  : 5 th < Lama Studi ≤ 6 th"),
            br(),
            tags$strong("- D  : 6 th < Lama Studi ≤ 7 th "),
            br(),
            tags$strong("- E  : Lama Studi > 7 th "),
            br(),
            br(),
            tags$strong("Kategori Atribut Asal Sekolah"),
            br(),
            tags$strong("- A1  : Kota Bengkulu"),
            br(),
            tags$strong("- A2  : Kabupaten Bengkulu"),
            br(),
            tags$strong("- A3  : Luar Provinsi Bengkulu"),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(width = 9, style = "color: black",
                  tabsetPanel(type = "tab", 
                              tabPanel("Tabel", DT::dataTableOutput("view")),
                              tabPanel("Summary", verbatimTextOutput("sum")),
                              tabPanel("Apriori", verbatimTextOutput("apri")),
                              tabPanel("Frequent Item using eclat", DT::dataTableOutput("ec")),
                              tabPanel("Frequency plot", 
                                       box(status = 'primary', title = 'Filter for the frequency plot',
                                           #'Asal Sekolah',
                                           selectInput('cat', 'Categorical variables:', c('Jalur Masuk',  'Jenis Kelamin', 'Prodi', 'Lama Studi', 'IPK','Asal Sekolah')),
                                           footer = 'Frequency plot for categorical variables'),
                                       
                                       plotOutput("plot.freq")),
                              tabPanel("Top Ten Apriori Rules With Arrow plots",plotOutput("rules")),
                              tabPanel("Parallel Coordinates Plot", plotOutput("group"))
                                       
                              
                              
                              
                              
                              # tabPanel("Data Analysis",
                              #          # 'dash',
                              #         #Dashboard filters
                              #             #title = 'Filters', 
                              #             status = 'primary', width = 10,
                              #             splitLayout(tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                              #                         cellWidths = c('0%', '19%', '4%', '19%', '4%', '19%', '4%', '19%', '4%', '19%'),
                              #                         selectInput('vb1', 'Variabel 1 :', c('Jalur Masuk',  'Jenis Kelamin', 'Prodi', 'Lama Studi', 'IPK')),
                              #                         div(),
                              #                         selectInput('vb2', 'Variabel 2 :', c('None', 'Jalur Masuk',  'Jenis Kelamin', 'Prodi', 'Lama Studi', 'IPK')),
                              #                         div(),
                              #                         selectInput('vb3', 'Variabel 3 :', c('None', 'Jalur Masuk',  'Jenis Kelamin', 'Prodi', 'Lama Studi', 'IPK')),
                              #                         # div(),
                              #                         # selectInput('as', 'Asal Sekolah', c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')),
                              #                         # div(),
                              #                         # selectInput('jm', 'Jalur Masuk', c('SNMPTN', 'SBMPTN', 'SPMU')),
                              #                         # div(),
                              #                         # selectInput('prod', 'Program Studi', c('Informatika', 'Teknik Sipil', 'Teknik Elektro', 'Teknik Mesin'))),
                              #                         # 
                              #                         plotOutput("barPlot")
                              #                       
                              #                       ))
                                    
                                          # splitLayout(cellWidths = c('4%', '12%', '10%'),
                                          #             div(),
                                          
                                          # fluidRow(column(4,
                                          #                 selectInput(
                                          #                     'LamaStudi',
                                          #                     'LamaStudi:', 
                                          #                     label = h4(""),
                                          #                     multiple = TRUE,
                                          #                     selected = c("SSR", "SR", "S"),
                                          #                     selectize = TRUE,
                                          #                     width = '400px'
                                          #                 ))),
                                          # 
                                          # selectInput('LamaStudi', 'LamaStudi:', c('SSR', 'SR', 'S')),
                                          # selectInput('regis', 'Registrations:', c('Total', 'New', 'Casual')),
                                          # selectInput('weather', 'Weather choice:', c('All', 'Good', 'Fair', 'Bad', 'Very Bad'))),
                                      #Boxes to display the plots
                                    
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                  ))
    )
)
)
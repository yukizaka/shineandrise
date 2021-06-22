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
library(igraph)
library(dplyr)



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
        
        rules1 <- apriori(data1, parameter = list(support =as.numeric(input$supo), confidence = as.numeric(input$confi), minlen=2))
        # interdata <- inspect(rules1)
        #find redundant rules
        rules.sorted = sort(rules1, by ="lift")
        subset.matrix = is.subset(rules.sorted, rules.sorted, sparse = FALSE)
        subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
        redundant = colSums(subset.matrix, na.rm=T) >= 1
        
        #remove redundant rules
        rules.pruned <- rules.sorted[!redundant]
        options(max.print=1000000)
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
        
        rules1 <- apriori(data1, parameter = list(support =as.numeric(input$supo), confidence = as.numeric(input$confi), minlen=2))
        # interdata <- inspect(rules1)
        #find redundant rules
        rules.sorted = sort(rules1, by ="lift")
        subset.matrix = is.subset(rules.sorted, rules.sorted, sparse = FALSE)
        subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
        redundant = colSums(subset.matrix, na.rm=T) >= 1
        
        #remove redundant rules
        rules.pruned <- rules.sorted[!redundant]
        top10subRules <- head(rules.pruned, n = 10, by = "confidence")
        saveAsGraph(top10subRules, file="rules.graphml")
        plot(top10subRules, method = "graph")
        
    })
    
    output$group <- renderPlot({
        
        data1<- read.transactions(
            input$gg$datapath,
            format = "basket",
            sep = ",",
            rm.duplicates = T)
        
        rules1 <- apriori(data1, parameter = list(support =as.numeric(input$supo), confidence = as.numeric(input$confi), minlen=2))
        # interdata <- inspect(rules1)
        #find redundant rules
        rules.sorted = sort(rules1, by ="lift")
        subset.matrix = is.subset(rules.sorted, rules.sorted, sparse = FALSE)
        subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
        redundant = colSums(subset.matrix, na.rm=T) >= 1
        
        #remove redundant rules
        rules.pruned <- rules.sorted[!redundant]
        interdata <- inspect(rules.pruned)
        plot(rules.pruned, method = "paracoord")
        
    })
    
    
    output$plot.freq <- renderPlot({
        
        #Column name variable
        cat_val = ifelse(input$cat == 'Jalur Masuk', 'JalurMasuk',
                         # ifelse(input$cat == 'Asal Sekolah', 'AsalSekolah',
                                ifelse(input$cat == 'Jenis Kelamin', 'JenisKelamin',
                                       ifelse(input$cat == 'Prodi', 'Prodi',
                                              ifelse(input$cat == 'Lama Studi', 'LamaStudi',
                                                     ifelse(input$cat == 'Asal Sekolah', 'AsalSekolah',
                                                        ifelse(input$cat == 'IPK', 'Ipk',
                                                                    ))))))
        
        #Frecuency plot
        fp = read.csv(input$gg$datapath, sep = ",")
        ggplot(data = fp, aes(x = fp[[cat_val]]))+
            geom_bar(stat = 'count', fill = 'mediumseagreen', 
                     width = 0.5)+
            stat_count(geom = 'text', size = 4,
                       aes(label = ..count..),
                       position = position_stack(vjust = 1.03))+
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.text = element_text(size = 12),
                  axis.title = element_text(size = 14),
                  plot.title = element_text(size = 16, face="bold"))+
            labs(title = sprintf('Frecuency plot of the variable %s', cat_val),
                 x = sprintf('%s', input$cat), y = 'Count')
        
    })
    
    # output$barPlot <- renderPlot({
    #     
    #     data1<- read.transactions(
    #         input$gg$datapath,
    #         format = "basket",
    #         sep = ",",
    #         rm.duplicates = T)
    #     
    #     if(input$LamaStudi != 'SSR'){
    #         
    #         if(input$Ipk != 'B2'){
    #             
    #             #Creating a table filter by year and weathersit for the bar plot
    #             Ipk <- data1 %>% group_by(Prodi, Ipk) %>% filter(yr == input$LamaStudi) %>%  count(JenisKelamin)
    #             
    #             Ipk <- Ipk %>% filter(Ipk == input$Ipk)
    #             
    #         } else{
    #             
    #             #Creating a table filter by year for the bar plot
    #             weather <- bike %>% group_by(season, weathersit) %>% filter(yr == input$year) %>%  summarise(new = sum(new), casual = sum(casual), total = sum(total))
    #             
    #         }
    #         
    #     } else{
    #         
    #         if(input$weather != 'All'){
    #             
    #             #Creating a table filter by weathersit for the bar plot
    #             weather <- bike %>% group_by(season, weathersit) %>% filter(weathersit == input$weather) %>%  summarise(new = sum(new), casual = sum(casual), total = sum(total))
    #             
    #         } else{
    #             
    #             #Creating a table for the bar plot
    #             weather <- bike %>% group_by(season, weathersit) %>%  summarise(new = sum(new), casual = sum(casual), total = sum(total))
    #             
    #         }
    #     }
    #     
    #     #Column name variable
    #     regis_val = ifelse(input$regis == 'Total', 'total', 
    #                        ifelse(input$regis == 'New', 'new','casual'))
    #     
    #     #Bar plot
    #     ggplot(weather, aes(x = season, y = weather[[regis_val]], 
    #                         fill = weathersit))+
    #         geom_bar(stat = 'identity', position=position_dodge())+
    #         geom_text(aes(label = weather[[regis_val]]),
    #                   vjust = -0.3, position = position_dodge(0.9), 
    #                   size = 4)+
    #         theme(axis.text.y = element_blank(),
    #               axis.ticks.y = element_blank(),
    #               axis.text = element_text(size = 12),
    #               axis.title = element_text(size = 14),
    #               plot.title = element_text(size = 16, face = 'bold'),
    #               plot.subtitle = element_text(size = 14),
    #               legend.text = element_text(size = 12))+
    #         ylim(NA, 805000)+
    #         labs(title = sprintf('%s bike sharing registrations by season and weather', input$regis),
    #              subtitle = sprintf('Throughout the year %s', input$year),
    #              x = 'Season', 
    #              y = sprintf('%s count of registrations', input$regis))+
    #         scale_fill_manual(values = c('Bad' = 'salmon2', 'Fair' = 'steelblue3', 'Good' = 'mediumseagreen', 'Very Bad' = 'tomato4'),
    #                           name = 'Weather')
    #     
    # })
    
    # output$barPlot <- renderPlot({
    #     #    
    #     # data4<- read.transactions(
    #     #     input$gg$datapath,
    #     #     format = "basket",
    #     #     sep = ",",
    #     #     rm.duplicates = T)    
    #     
    #     data4 = read.csv(input$gg$datapath, sep = ",")
    #         
    #         vb1_val = ifelse(input$vb1 == 'Jalur Masuk', 'JalurMasuk',
    #                          # ifelse(input$cat == 'Asal Sekolah', 'AsalSekolah',
    #                          ifelse(input$vb1 == 'Jenis Kelamin', 'JenisKelamin',
    #                                 ifelse(input$vb1 == 'Prodi', 'Prodi',
    #                                        ifelse(input$vb1 == 'Lama Studi', 'LamaStudi',
    #                                               ifelse(input$vb1 == 'IPK', 'Ipk',
    #                                               )))))
    #         
    #         vb2_val = ifelse(input$vb2 == 'None', '',
    #                     ifelse(input$vb2 == 'Jalur Masuk', 'JalurMasuk',
    #                          # ifelse(input$cat == 'Asal Sekolah', 'AsalSekolah',
    #                          ifelse(input$vb2 == 'Jenis Kelamin', 'JenisKelamin',
    #                                 ifelse(input$vb2 == 'Prodi', 'Prodi',
    #                                        ifelse(input$vb2 == 'Lama Studi', 'LamaStudi',
    #                                               ifelse(input$vb2 == 'IPK', 'Ipk',
    #                                               ))))))
    #         
    #         vb3_val = ifelse(input$vb3 == 'None', '',
    #                          ifelse(input$vb3 == 'Jalur Masuk', 'JalurMasuk',
    #                                 # ifelse(input$cat == 'Asal Sekolah', 'AsalSekolah',
    #                                 ifelse(input$vb3 == 'Jenis Kelamin', 'JenisKelamin',
    #                                        ifelse(input$vb3 == 'Prodi', 'Prodi',
    #                                               ifelse(input$vb3 == 'Lama Studi', 'LamaStudi',
    #                                                      ifelse(input$vb3 == 'IPK', 'Ipk',
    #                                                      ))))))
    #         
    #         shine1 = data4 %>% select(input$vb1_val, input$vb2_val, input$vb3_val) %>% group_by(input$vb2_val) %>% count(input$vb3_val, input$vb1_val) %>% distinct()
    #         shine1
    #         
    #         ggplot(data=shine1, aes(fill=input$vb2_val, y=n, x=input$vb3_val, label = n, legend = TRUE)) + 
    #             geom_bar(position="dodge", stat="identity") +
    #             scale_fill_viridis(discrete = T, option = "E") +
    #             scale_fill_brewer(palette = 'Paired') +
    #             ggtitle("Lama Studi berdasarkan IPK dan Jenis Kelamin Mahasiswa") +
    #             geom_text(hjust = 0.5, size = 4, position = position_dodge(width = .75), vjust = 2) +
    #             facet_wrap(~input$vb1_val) +
    #             theme_ipsum() +
    #             theme(legend.position="bottom") +
    #             xlab("")
    #         
    #     
    #         
    # })
    
})



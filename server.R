library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(doBy)
shinyServer(
  
  function(input, output,session) {
    #LOAD the data
    mydata=read.csv("video_sale_ranked.csv")
    
    #Function for populating the info boxes on overview panel
    populateOverviewInfobox=function(input,output,session){
      
      temp=group_by(mydata,Publisher)%>%summarise(total_sales=sum(Global_Sales))%>%arrange(desc(total_sales))
      tempgenre=group_by(mydata,Genre)%>%summarise(total_sales=sum(Global_Sales))%>%arrange(desc(total_sales))
      output$bestSellingGame=renderInfoBox({
        infoBox("Best Seller", mydata[1,]$Name,paste(mydata[1,]$Global_Sales,"Million Copies"), icon = icon("thumbs-up", lib = "glyphicon"),
                color = "green")
      })
      
      output$WorstSellingGame=renderInfoBox({
        infoBox("Worst Seller", mydata[nrow(mydata),]$Name,paste(mydata[nrow(mydata),]$Global_Sales,"Million"), icon = icon("thumbs-down", lib = "glyphicon"),
                color = "red")
      })
      
      output$bestSellingPublisher=renderInfoBox({
        infoBox("Most Successful Publisher", temp[1,]$Publisher,paste(temp[1,]$total_sales,"Million Copies"), icon = icon("thumbs-up", lib = "glyphicon"),
                color = "green")
      })
      
      output$WorstSellingPublisher=renderInfoBox({
        infoBox("Least Successful Publisher", temp[nrow(temp),]$Publisher,paste(temp[nrow(temp),]$total_sales,"Million Copies"), icon = icon("thumbs-down", lib = "glyphicon"),
                color = "red")
      })
      output$bestSellingGenre=renderInfoBox({
        infoBox("Best Selling Genre", tempgenre[1,]$Genre,paste(tempgenre[1,]$total_sales,"Million Copies"), icon = icon("thumbs-up", lib = "glyphicon"),
                color = "green")
      })
      output$WorstSellingGenre=renderInfoBox({
        infoBox("Worst Selling Genre", tempgenre[nrow(tempgenre),]$Genre,paste(tempgenre[nrow(tempgenre),]$total_sales,"Million Copies"), icon = icon("thumbs-down", lib = "glyphicon"),
                color = "red")
      })
    }
    
    #Common function to populate the drow downs of all the panels
    populateDropdown=function(input,output,session){
      #dropdown for games
      updateSelectizeInput(session=session,inputId = "searchGame",choices = unique(mydata$Name),selected='')
      #dropdown for publisher
      updateSelectizeInput(session=session,inputId = "searchPublisher",choices = unique(mydata$Publisher),selected='')
      #dropdown for genre
      updateSelectizeInput(session=session,inputId = "searchGenre",choices = unique(mydata$Genre),selected='')
      #dropdown for year
      updateSelectizeInput(session=session,inputId = "searchYear",choices = sort(unique(mydata$Year)),selected='')
      
    }
    
    #common function to populate the tables
    newData <- reactive({
     
      if(input$maintab=='2'){
        #Game
        req(input$searchGame)
        data <- filter(mydata, Name==input$searchGame)
        
      }else if(input$maintab=='3'){
        #Publisher
        req(input$searchPublisher)
        data <- head(filter(mydata, Publisher==input$searchPublisher,desc('Global_Sales')),n=5)
        
      }else if(input$maintab=='4'){
        #Genre
        req(input$searchGenre)
        data <-head(filter(mydata, Genre==input$searchGenre),5)
      }else if(input$maintab=='5'){
        #Year
        req(input$searchYear)
        data <-head(filter(mydata, Year==input$searchYear),5)
      }
      
      
    })
    
    #to populate the year wise reactive plotly graph
    yearWisePlotly<-reactive({
      if(input$maintab=='5'){
        
        
        if(is.null(input$searchYear)|| input$searchYear==''){
          #---------------Year wise --------------
          
          
          #Year Plot
          gbl_sale <- select(mydata, c("Year", "Global_Sales", "Genre"))
          #gbl_sale_total <- data.frame(Year = as.integer(), Total_Sale = as.numeric())
          
          #Define Empty DF
          
          gbl_sale_genre <- data.frame(Year = as.integer(), Total_Sale = as.numeric(),Genre =as.character())
          
          for (i in 2000:2017){
            temp_df1 = filter(gbl_sale,gbl_sale$Year==i)
            uni = unique(temp_df1$Genre)
            for(gen in uni){
              temp_gen = filter(gbl_sale,gbl_sale$Year==i & gbl_sale$Genre==gen)
              temp_df = data.frame(Year=i,Total_Sale=sum(temp_gen$Global_Sales),Genre = gen)
              gbl_sale_genre = rbind(gbl_sale_genre,temp_df)
              
            }
          }
          
          animatedPlot=plot_ly(
            gbl_sale_genre,
            x =  ~ Year,
            y = ~ Total_Sale,
            color = ~ Genre,
            type = 'scatter',
            mode = 'lines+markers',
            hoverinfo = 'text',
            text = ~paste('</br> Year: ', Year,
                          '</br> Sale: ', Total_Sale,
                          '</br> Genre: ', Genre)
            
          )%>%layout(
            title = "Overall Game Sale form year 2000 to 2017",
            xaxis = list(title = 'Year'),
            yaxis = list(title = 'Copies Sold(In Millions)')
          )
          return(animatedPlot)
          
          
        }
        
      }
    })
    
    genreWisePlotly<-reactive({
      if(input$maintab=='4'){
        
        
        if(is.null(input$searchGenre)|| input$searchGenre==''){
          #Genre Plot
          
            
            sum_sales<-summaryBy(Global_Sales~Genre,data=mydata,FUN=sum)
            
            pie= sum_sales%>%plot_ly(labels = ~Genre, values = ~Global_Sales.sum,width = 900,height=550) %>%
              add_pie(hole = 0.6) %>%
              layout(title = "Sales Distribution among Genres",  showlegend = FALSE )
            
            return (pie)
            
          
        }
        
      }
    })
    
    #Common function to populate all the plots
    newPlot<-reactive({
      
       if(input$maintab=='2'){
        #Game wise Plot
         if(is.null(input$searchGame)|| input$searchGame==''){
           
           temp=mydata%>%group_by(Name)%>%summarise(total_sales=sum(Global_Sales))%>%arrange(desc(total_sales))
           temp=head(temp,n=15)
           top15=ggplot(temp,aes(x=reorder(Name,total_sales),y=total_sales))+
             geom_bar(stat="identity",fill="cornsilk4")+
             coord_flip()+
             geom_text(aes(label=total_sales),vjust=0.5,color="white",size=4.0,hjust=1.2)+
             ylab("Copies Sold(In Millions)")+xlab("")+ggtitle("Top Selling Games")+theme_bw()+
             theme(plot.title = element_text(size=rel(2) ),axis.text.x=element_text(size=rel(1.5)),axis.text.y=element_text(size=rel(1.5)),
             axis.title.x=element_text(size=rel(1.5)),axis.title.y=element_text(size=rel(1.5)),
             legend.text =element_text(size=rel(1.2)),legend.title = element_text(size=rel(1.5)) )+
             scale_fill_brewer(palette="Set1")
           
           return(top15)
         }
        req(input$searchGame)
        data <- filter(mydata, Name==input$searchGame)
        myplot=data%>%gather(key='Region',value='Sales',NA_Sales:Other_Sales)%>%
          ggplot(aes(x=Platform,y=Sales,width=0.5))+geom_col(position = position_stack(),aes(fill=Region))+scale_fill_brewer(palette="Set1")+
          labs(x='Platform',y='Copies Sold(In Millions)')+theme_bw()+
          theme(axis.text.x=element_text(size=rel(1.5)),axis.text.y=element_text(size=rel(1.5)),
                axis.title.x=element_text(size=rel(1.5)),axis.title.y=element_text(size=rel(1.5)),
                legend.text =element_text(size=rel(1.2)),legend.title = element_text(size=rel(1.5)) )
        myplot
        
      }else if(input$maintab=='3'){
        #Publisher Plot 
        if(is.null(input$searchPublisher)|| input$searchPublisher==''){
          temp=mydata %>%group_by(Publisher)%>%summarise(total_sales=sum(Global_Sales))%>%arrange(desc(total_sales))
          temp=head(temp,n=15)
          
          top15=ggplot(temp,aes(x=reorder(Publisher,total_sales),y=total_sales))+
            geom_bar(stat="identity",fill="cornsilk4")+
            coord_flip()+
            geom_text(aes(label=total_sales),vjust=0.5,color="white",size=4.0,hjust=1.2)+
            ylab("Copies Sold(In Millions)")+xlab("")+ggtitle("Top Selling Publishers")+theme_bw()+
            theme(plot.title = element_text(size=rel(2) ),axis.text.x=element_text(size=rel(1.5)),axis.text.y=element_text(size=rel(1.5)),
            axis.title.x=element_text(size=rel(1.5)),axis.title.y=element_text(size=rel(1.5)),
            legend.text =element_text(size=rel(1.2)),legend.title = element_text(size=rel(1.5)) )
          return(top15)
        }
        req(input$searchPublisher)
        data <- head(filter(mydata, Publisher==input$searchPublisher),n=5)
        myplot<-data%>%gather(key='Region',value='Sales',NA_Sales:Other_Sales)%>%
          ggplot(aes(x=Name,y=Sales,width=0.5))+
          geom_col(position = position_stack(),aes(fill=Region))+
          labs(x='Game Title',y='Copies Sold(In Millions)')+scale_fill_brewer(palette="Set1")+theme_bw()+
          theme(axis.title.x = element_text(size = rel(1.5)),
                axis.title.y = element_text(size = rel(1.5)),
                axis.text.x = element_text(size = rel(1.5)),
                axis.text.y = element_text(size = rel(1.5)),
                plot.title = element_text(color = "Black",size = 15,family = "Courier"),
                legend.text = element_text(size=rel(1.2))
               )
        
        myplot
        
      }else if(input$maintab=='4'){
        
        req(input$searchGenre)
        data <-head(filter(mydata, Genre==input$searchGenre),5)
        myplot=data%>%gather(key='Region',value='Sales',NA_Sales:Other_Sales)%>%
          ggplot(aes(x=Name,y=Sales,width=0.5))+geom_col(position = position_stack(),aes(fill=Region))+scale_fill_brewer(palette="Set1")+
          labs(x='Game Title',y='Copies Sold(In Millions)')+theme_bw()+
          theme(axis.text.x=element_text(size=rel(1.5)),axis.text.y=element_text(size=rel(1.5)),
                axis.title.x=element_text(size=rel(1.5)),axis.title.y=element_text(size=rel(1.5)),
                legend.text =element_text(size=rel(1.2)),legend.title = element_text(size=rel(1.5)) )
        myplot
        
      }else if(input$maintab=='5'){
        #Year Plot
        
        req(input$searchYear)
        data <-head(filter(mydata, Year==input$searchYear),5)
        myplot= data%>%gather(key='Region',value='Sales', NA_Sales:Other_Sales)%>%
          ggplot(aes(x=Name,y=Sales,width=0.5))+geom_col(aes(fill=Region))+scale_fill_brewer(palette="Set1")+
          labs(x='Game Title',y='Copies Sold(In Millions)')+theme_bw()+theme(plot.title = element_text(size=rel(2) ),axis.text.x=element_text(size=rel(1.5)),axis.text.y=element_text(size=rel(1.5)),
                                                             axis.title.x=element_text(size=rel(1.5)),axis.title.y=element_text(size=rel(1.5)),
                                                             legend.text =element_text(size=rel(1.2)),legend.title = element_text(size=rel(1.5)) )+
          scale_fill_brewer(palette="Set1")
        myplot
      }
    })
    
    ########Code for populating the dropdowns############
    populateDropdown(input,output,session)
   
    ############Overview tab code###########
    populateOverviewInfobox(input,output,session)
    
    #populate the tables. Generic function is used which populates the content of table
    #based on the tab selected
    output$mytable2<-output$mytable3<-output$mytable4<-output$mytable5<-renderTable({newData()},striped = TRUE,hover = TRUE)
    #populate the plots. Generic function is used which populates the plots based on 
    #tab selected
    output$plot2<-output$plot3<-output$plot4<-output$plot5<-renderPlot({newPlot()})
    output$plot6<-renderPlotly({yearWisePlotly()})
    output$plot7<-renderPlotly({genreWisePlotly()})
    
   
   
    
  })


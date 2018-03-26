library(reshape2)
library(lubridate)
library(ggplot2)
library(shiny)

rm(list=ls())

setwd("C:/Users/atulm/work/RCode/splitwise")


#import data
origDF = read.csv("province_export.csv",as.is = T)

#remove na
origDF<-origDF[!(is.na(origDF$Date)),]
origDF<-origDF[!(origDF$Description=="Total balance"),]

#structure
origDF$Date<-as.POSIXct(origDF$Date)
origDF$Category<-as.factor(origDF$Category)


#View rent / internet / netflix
View(origDF[origDF$Category=="Rent",])
View(origDF[grep("Netflix*",origDF$Description,ignore.case = TRUE),])
View(origDF[grep("*charter*",origDF$Description,ignore.case = TRUE),])



#quick
levels(origDF$Category)
str(origDF)
View(origDF)

#shiny
shinyServer<-function(input,output){
  
  output$dateRangeText2 <- renderText({
    paste(input$dateRange[1],input$dateRange[2])
    
  })
 

  output$distPlot1 <- renderPlot({
    
    startDate<-as.POSIXct(input$dateRange[1])
    endDate<-as.POSIXct(input$dateRange[2])
    subsetOrigDF<-origDF[origDF$Date>=input$dateRange[1] & origDF$Date<=input$dateRange[2],]
    subsetOrigDF$Date<-as.character(subsetOrigDF$Date)
    output$table <- renderTable(subsetOrigDF)
    
    #myshare
    myShareDF<-melt(data=subsetOrigDF,id=c("Date","Description","Category","Cost","Currency"))
    
    #drop currency
    colnames(myShareDF)<-c("Date","Description","Category","Cost","Currency","User","Share")
    myShareDF<-myShareDF[,!(names(myShareDF) %in% c("Currency"))]
    
    #add month and year
    myShareDF$year<-year(myShareDF$Date)
    myShareDF$month<-month(myShareDF$Date)
    myShareDF$day<-day(myShareDF$Date)
    
    #remove total balance and cost as double
    myShareDF<-myShareDF[myShareDF$User!="Aniket",]
    myShareDF$Cost<-as.double(myShareDF$Cost)
    myShareDF$Share<-as.double(myShareDF$Share)
    myShareDF$Share[myShareDF$Share<0]<-myShareDF$Share[myShareDF$Share<0]*-1
    
    catShareDF<-aggregate(myShareDF$Share,by=list(myShareDF$Category),FUN=sum)
    colnames(catShareDF)<-c("category","cost")
    
    
    ggplot(data=catShareDF[catShareDF$category %in% input$variable,], aes(x="", y=cost, fill=factor(category))) +
      geom_bar(stat="identity",width=1)+
      coord_polar(theta = "y",start = 0)
  })
  
  output$distPlot2 <- renderPlot({
    startDate<-as.POSIXct(input$dateRange[1])
    endDate<-as.POSIXct(input$dateRange[2])
    subsetOrigDF<-origDF[origDF$Date>=input$dateRange[1] & origDF$Date<=input$dateRange[2],]
    subsetOrigDF$Date<-as.character(subsetOrigDF$Date)
    output$table <- renderTable(subsetOrigDF)
    
    #myshare
    myShareDF<-melt(data=subsetOrigDF,id=c("Date","Description","Category","Cost","Currency"))
    
    #drop currency
    colnames(myShareDF)<-c("Date","Description","Category","Cost","Currency","User","Share")
    myShareDF<-myShareDF[,!(names(myShareDF) %in% c("Currency"))]
    
    #add month and year
    myShareDF$year<-year(myShareDF$Date)
    myShareDF$month<-month(myShareDF$Date)
    myShareDF$day<-day(myShareDF$Date)
    
    #remove total balance and cost as double
    myShareDF<-myShareDF[myShareDF$User!="Aniket",]
    myShareDF$Cost<-as.double(myShareDF$Cost)
    myShareDF$Share<-as.double(myShareDF$Share)
    myShareDF$Share[myShareDF$Share<0]<-myShareDF$Share[myShareDF$Share<0]*-1
    
    
    
    monthlyShareDF<-aggregate(myShareDF$Share,by=list(myShareDF$Category,myShareDF$month),FUN = sum)
    colnames(monthlyShareDF)<-c("Category","Month","Share")
    
    ggplot(data=monthlyShareDF[monthlyShareDF$Category %in% input$variable,], aes(x=Month, y=Share, fill=Category)) +
      geom_bar(colour="black", stat="identity")+
      scale_x_continuous(breaks = seq(1,12,by=1))
    
  })
}

shinyUI <- fluidPage(
  pageWithSidebar(
  
  # Application title
    headerPanel("Splitwise Analytics!"),
  
    # Sidebar with a slider input for number of observations
    sidebarPanel(
      dateRangeInput('dateRange',
                   label = 'Select start and end date',
                   start = Sys.Date() - 2, end = Sys.Date()
      ),
      checkboxGroupInput("variable", "Variables to show:",
                       levels(monthlyShareDF$Category),choices =levels(monthlyShareDF$Category) )
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot1"),
    plotOutput("distPlot2")
    
  )
  ),
  fluidPage(
    tableOutput('table')
  )
  
)

shinyApp(ui = shinyUI, server = shinyServer) # this launches your app

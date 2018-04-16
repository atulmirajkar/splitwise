library(reshape2)
library(lubridate)
library(ggplot2)
library(shiny)

rm(list=ls())

setwd("C:/Users/atulm/RCode/Splitwise")



#shiny
shinyServer<-function(input,output,session){
  #read data
  mydata<-reactive({
    
    origDF = read.csv("expenses.csv",as.is = T)
    
    #remove na
    origDF<-origDF[!(is.na(origDF$Date)),]
    origDF<-origDF[!(origDF$Description=="Total balance"),]
    
    #structure
    origDF$Date<-as.POSIXct(origDF$Date)
    origDF$Category<-as.factor(origDF$Category)
    origDF$Group<-as.factor(origDF$Group)
    
    #allGroupDF
    #drop currency
    allGroupDF<-origDF[,!(names(origDF) %in% c("X"))]
    
    #add month and year
    allGroupDF$year<-year(allGroupDF$Date)
    allGroupDF$month<-month(allGroupDF$Date)
    allGroupDF$day<-day(allGroupDF$Date)
    
    #remove total balance and cost as double
    allGroupDF$Cost<-as.double(allGroupDF$Cost)
    allGroupDF$Share<-as.double(allGroupDF$Share)
    return(allGroupDF)
  })
  
  output$dateRangeText2<-renderText({
    paste(input$dateRange[1],input$dateRange[2])
    
  })
  
  inputCategory<-reactive({
    data=mydata()
    levels(data$Category)
    
  })
  
  inputGroup<-reactive({
    data=mydata()
    levels(data$Group)
    
  })
  
  observeEvent(mydata(),{
    updateCheckboxGroupInput(session, "category", choices = inputCategory())
    updateCheckboxGroupInput(session, "group", choices = inputGroup())
  })
  
  
  
  
  output$distPlot1<-renderPlot({
    data=mydata()
    
    startDate<-as.POSIXct(input$dateRange[1])
    endDate<-as.POSIXct(input$dateRange[2])
    if(is.null(input$category) || is.null(input$group)) return(NULL)
    
    data<-data[data$Date>=input$dateRange[1] & data$Date<=input$dateRange[2],]
    
    
    
    catShareDF<-data[data$Category %in% input$category,]
    catGroupShareDF<-catShareDF[catShareDF$Group %in% input$group,]
    
    if(nrow(catGroupShareDF)==0) return(NULL)
    
    ggplot(data=catGroupShareDF, aes(x="", y=Cost, fill=factor(Category))) +
      geom_bar(stat="identity",width=1)+
      coord_polar(theta = "y",start = 0)
  })
  
  output$distPlot2<-renderPlot({
    data=mydata()
    
    startDate<-as.POSIXct(input$dateRange[1])
    endDate<-as.POSIXct(input$dateRange[2])
    data<-data[data$Date>=input$dateRange[1] & data$Date<=input$dateRange[2],]
    
    if(is.null(input$category) || is.null(input$group)) return(NULL)
    
    catShareDF<-data[data$Category %in% input$category,]
    catGroupShareDF<-catShareDF[catShareDF$Group %in% input$group,]
    if(nrow(catGroupShareDF)==0) return(NULL)
    
    monthlyShareDF<-aggregate(catGroupShareDF$Share,by=list(catGroupShareDF$Category,catGroupShareDF$month),FUN = sum)
    colnames(monthlyShareDF)<-c("Category","Month","Share")
    
    if(nrow(monthlyShareDF)==0) return(NULL)
    
    
    ggplot(data=monthlyShareDF, aes(x=Month, y=Share, fill=Category)) +
      geom_bar(colour="black", stat="identity")+
      scale_x_continuous(breaks = seq(1,12,by=1))
    
  })
}

shinyUI<-fluidPage(
  pageWithSidebar(
    
    # Application title
    headerPanel("Splitwise Analytics!"),
    
    # Sidebar with a slider input for number of observations
    sidebarPanel(
      dateRangeInput('dateRange',
                     label = 'Select start and end date',
                     start = Sys.Date() - 2, end = Sys.Date()
      ),
      checkboxGroupInput("category", "categorys to show:"),
      
      checkboxGroupInput("group", "groups to show:")
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

shinyApp(ui = shinyUI, server = shinyServer)

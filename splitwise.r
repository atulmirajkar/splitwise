library(reshape2)
library(lubridate)
library(ggplot2)
library(shiny)
library(jsonlite)
library(curl)

rm(list=ls())


#shiny
shinyServer<-function(input,output,session){
  #read data
  rv<-reactiveValues(data=NULL,dateAdjustedData=NULL,categoryAdjustedData=NULL,groupAdjustedData=NULL)
  
  #
  observeEvent(input$load,{
    origDF <- fromJSON("http://localhost:9093/getStoredJson")
    origDF <- origDF[-1,]
    
    #remove na
    origDF<-origDF[!(is.na(origDF$Date)),]
    origDF<-origDF[!(origDF$Description=="Total balance"),]
    
    #structure
    origDF$Date<-as.POSIXct(origDF$Date)
    #origDF$Category<-as.factor(origDF$Category)
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
    rv$data<-allGroupDF
  })
  
  
  
  
  
  observeEvent(rv$data,{
    updateCheckboxGroupInput(session, "group", choices = inputGroup())
  })
  
  inputGroup<-reactive({
    data=rv$data
    levels(data$Group)
    
  }) 
  
  
  
  dateAdjustedLoad<-reactive({
    data=rv$data
    if(is.null(input$group)) return(NULL)
    groupAdjustedData<-data[data$Group %in% input$group,]
    #output$groupAdjustedTable <- renderTable(groupAdjustedData)
    
    rv$groupAdjustedDF<-groupAdjustedData
    
    
    data<- rv$groupAdjustedDF
    if(is.null(input$dateRange[1]) || is.null(input$dateRange[2])) return(NULL)
    data<-data[data$Date>=input$dateRange[1] & data$Date<=input$dateRange[2],]
    rv$dateAdjustedData<-data
    updateCheckboxGroupInput(session, "category", choices = inputCategory())
    return(data)
  })
  
  inputCategory<-reactive({
    data=rv$dateAdjustedData
    levels(as.factor(data$Category))
    
  })
  
  finalDataLoad<-reactive({
    data=dateAdjustedLoad()
    if(is.null(input$category)) return(NULL)
    categoryAdjustedData<-data[data$Category %in% input$category,]
    output$categoryAdjustedTable <- renderTable(categoryAdjustedData)
    rv$categoryAdjustedDF<-categoryAdjustedData
    
    return(categoryAdjustedData)
    
    
    
    
  })
  
  
  
  output$distPlot1<-renderPlot({
    subsetedData<-finalDataLoad()
    if(is.null(subsetedData)) return(NULL)
    subsetedData<-rv$dateAdjustedData
    subsetedData$Category<-as.factor(subsetedData$Category)
    subsetedData<-subsetedData[subsetedData$Category %in% input$category,]
    if(nrow(subsetedData)==0) return(NULL)
    
    subsetedData<-aggregate(Cost~Category,data=subsetedData,sum)
    subsetedData$RevCost<-rev(subsetedData$Cost)
    subsetedData$RevCum<-cumsum(subsetedData$RevCost)
    
    
    subsetedData$Midpoint=(subsetedData$RevCum-subsetedData$RevCost)+(subsetedData$RevCost/2)
    subsetedData$Labels=paste0(round((subsetedData$RevCost/sum(subsetedData$Cost))*100,1),"%")
    
    
    ggplot(data=subsetedData, aes(x="",y=Cost, fill=factor(Category))) +
      geom_bar(width=1,stat="identity")+
      geom_text(aes(x=1.2,y=Midpoint,label=Labels),color="black",fontface="bold")+
      coord_polar(theta = "y",start=0)
    
  })
  
  
  
  output$distPlot2<-renderPlot({
    subsetedData<-rv$dateAdjustedData
    if(is.null(subsetedData)) return(NULL)
    subsetedData$Category<-as.factor(subsetedData$Category)
    subsetedData<-subsetedData[subsetedData$Category %in% input$category,]
    if(nrow(subsetedData)==0) return(NULL)
    
    monthlyShareDF<-aggregate(subsetedData$Share,by=list(subsetedData$Category,subsetedData$month),FUN = sum)
    colnames(monthlyShareDF)<-c("Category","Month","Share")
    
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
      actionButton("load", "Load/Refresh"),
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
      plotOutput("distPlot2"),
      tableOutput("groupAdjustedTable"),
      tableOutput("categoryAdjustedTable"),
      tableOutput("dateAdjustedTable")
      
    )
  ),
  fluidPage(
    tableOutput('table')
  )
  
)

shinyApp(ui = shinyUI, server = shinyServer)

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
  rv<-reactiveValues(data=NULL,dateAdjustedData=NULL,categoryAdjustedData=NULL,groupAdjustedData=NULL,urlFile="")
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['file']])) {
      rv$urlFile<-(query[['file']])
    }
  })
  #on clicking load
  observeEvent(input$load,{
      origDF <- fromJSON(paste0("http://backend:9093/getStoredJsonFile?file=",rv$urlFile))
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
      
      #update group checkboxes
      updateCheckboxGroupInput(session, "group", choices = inputGroup())
    })
  
  
  inputGroup<-reactive({
    data=rv$data
    levels(data$Group)
    
  }) 
  

  
  observeEvent(input$group,{
    data=rv$data
    if(is.null(input$group)) return(NULL)
    groupAdjustedData<-data[data$Group %in% input$group,]
    
    
    rv$groupAdjustedDF<-groupAdjustedData
  
    
    dateAdjustedLoad()
    
    #update category checkboxes
    updateCheckboxGroupInput(session, "category", choices = inputCategory())
  })
  
  observeEvent(input$dateRange,{
    dateAdjustedLoad()
  })
  dateAdjustedLoad<-reactive({
    if(is.null(input$dateRange[1]) || is.null(input$dateRange[2])) return(NULL)
    
    data<- rv$groupAdjustedDF
    data<-data[data$Date>=input$dateRange[1] & data$Date<=input$dateRange[2],]
    rv$dateAdjustedData<-data
    
    #update category checkboxes
    updateCheckboxGroupInput(session, "category", choices = inputCategory())
    
    return(data)
  })
  
  inputCategory<-reactive({
    data=rv$dateAdjustedData
    levels(as.factor(data$Category))
    
  })
  
   observeEvent(input$category,{
    data=rv$dateAdjustedData
    if(is.null(input$category)) return(NULL)
    
    categoryAdjustedData<-data[data$Category %in% input$category,]
    drillData<-categoryAdjustedData[,names(categoryAdjustedData) %in% c("month","day","Description","Category","Share")]
    
    output$categoryAdjustedTable <- renderTable(drillData)
    rv$categoryAdjustedDF<-categoryAdjustedData
    
    
  })
    

  
  output$distPlot1<-renderPlot({
    subsetedData<-rv$dateAdjustedData
    if(is.null(subsetedData)) return(NULL)
    
    subsetedData$Category<-as.factor(subsetedData$Category)
    subsetedData<-subsetedData[subsetedData$Category %in% input$category,]
    if(nrow(subsetedData)==0) return(NULL)
    
    subsetedData<-aggregate(Share~Category,data=subsetedData,sum)
    subsetedData$Category<-factor(subsetedData$Category,levels = rev(levels(subsetedData$Category)))
    subsetedData$LabelPos<-cumsum(subsetedData$Share)

    
    subsetedData$Midpoint=(subsetedData$LabelPos-subsetedData$Share)+(subsetedData$Share/2)
    subsetedData$Labels=paste0(round((subsetedData$Share/sum(subsetedData$Share))*100,1),"%")
    
    
    ggplot(data=subsetedData, aes(x="Percent",y=Share, fill=factor(Category))) +
      geom_bar(width=1,stat="identity",color='black')+
      geom_text(aes(x=1.2,y=Midpoint,label=Labels),color="black",fontface="bold")+
      coord_polar(theta = "y",start=0)+
      theme(axis.ticks=element_blank(),  # the axis ticks
            axis.title=element_blank(),  # the axis labels
            axis.text.y=element_blank())+ # the 0.75, 1.00, 1.25 labels.
      scale_y_continuous(
        breaks=subsetedData$Midpoint,   # where to place the labels
        labels=paste0(subsetedData$Category,subsetedData$Share,sep = " ") # the labels
      )
      
  })
  
  
  
  output$distPlot2<-renderPlot({
    subsetedData<-rv$dateAdjustedData
    if(is.null(subsetedData)) return(NULL)
    subsetedData$Category<-as.factor(subsetedData$Category)
    subsetedData<-subsetedData[subsetedData$Category %in% input$category,]
    if(nrow(subsetedData)==0) return(NULL)
    
    monthlyShareDF<-aggregate(subsetedData$Share,by=list(subsetedData$Category,subsetedData$month),FUN = sum)
    colnames(monthlyShareDF)<-c("Category","Month","Share")
    
    monthlyShareDF$Month<-as.factor(monthlyShareDF$Month)
    monthlyShareDF$Category<-as.factor(monthlyShareDF$Category)
    
    monthlyShareDF$labelPos<-ave(monthlyShareDF$Share,monthlyShareDF$Month,FUN=cumsum)
    monthlyShareDF$Category<-factor(monthlyShareDF$Category,levels = rev(levels(monthlyShareDF$Category)))
    
    
    ggplot(data=monthlyShareDF, aes(x=Month, y=Share, fill=Category,width=.5)) +
      geom_bar(colour="black", stat="identity")+
      geom_text(aes(y=labelPos, label=Share), vjust=1.6, color="white", size=3.5)
    
  })
}

shinyUI<-fluidPage(
 
    
    # Application title
    headerPanel("Splitwise Analytics!"),
    fluidRow(
      column(4,
             # Sidebar with a slider input for number of observations
             
               actionButton("load", "Load/Refresh"),
               dateRangeInput('dateRange',
                              label = 'Select start and end date',
                              start = Sys.Date() - 2, end = Sys.Date()
               ),
               checkboxGroupInput("category", "categorys to show:"),
               
               checkboxGroupInput("group", "groups to show:")
               
             ),
      column(4,
             plotOutput("distPlot1")),
      column(4,
             plotOutput("distPlot2")),
      fluidRow(offset=4,
               tableOutput("categoryAdjustedTable"))
    )

  
)

shinyApp(ui = shinyUI, server = shinyServer)

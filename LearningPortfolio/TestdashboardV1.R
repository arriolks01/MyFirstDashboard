## app.R ##
library(shiny)
library(shinydashboard)

#install.packages("moments")



ui <- dashboardPage(
  
  dashboardHeader(
    title = "My First Dashboard:)"
  ),# dashboardHeader
  
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("Uniform Distribution",tabName = "Uniform", icon=icon("square")),#item1
      
      menuItem("Normal Distribution",tabName = "Normal", icon=icon("bell-o")) #item2 
      
    ) #sidebar menu
    
    
    
    
  ),#dashboardSidebar
  
  
  dashboardBody(
    tabItems(
      
      tabItem(
        
        tabName = "Uniform",
        
        
        fluidRow(
        
                  box( title= "Please select a number",
                       solidHeader= TRUE,
                       background="yellow",
                       status="warning",
                       height=312,
                       sliderInput(inputId="number",
                                   label="",
                                   value=500, min=25,max=1000)),
                  
                  
                  box(
                    title= "Histogram",
                    solidHeader= TRUE,
                    background="light-blue",
                    status = "primary",
                    plotOutput("hist",height=250)),
                  
                  
                  valueBoxOutput("meanBox"),
                  
                  valueBoxOutput("medianBox"),
                  
                  valueBoxOutput("sdBox"),
                  
                  
                  )# fluidRow
          
          
        ),#tabItem
       
      
      tabItem(tabName="Normal",
              
              fluidRow(
                
                box(title= "Pick a number, any number!",
                    
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    status = "warning",
                    sliderInput(inputId = "normnumber", #FIXED LINE!!!:D Validates rnorm on line 129 :D
                                label="",
                                value=500, min= 25,
                                max=1000 ) ),
                
                box( title = "density plot",
                     
                     solidHeader = TRUE,
                     background = "light-blue",
                     status="primary",
                     plotOutput("density",height=250)),
                
                
                infoBoxOutput("meanInfoBox"),
                
                infoBoxOutput("medianInfoBox"),
                
                infoBoxOutput("sdInfoBox"),
                
          
              )#fluidRow2
              
              
              ) #tabItem-normal
      
      
      
        
      ) #tabItems
      
    )#dashboardBody 
    
    
    
    
  )#dashboardPage
  





#--------------------------------SERVER-----------------------------------------------------------

server <- function(input, output) {
  
  histdata <-reactive({runif(input$number,min=0,max=1)})
  densitydata <- reactive({rnorm(input$normnumber)})
  
  output$hist<- renderPlot({
    
    hist(histdata(),xlab="Value",
         
         main=paste(input$number,"random values between 0 and 1"))
  })
  
  output$density<- renderPlot({
    
    hist(densitydata(),xlab="Value",
         
         main=paste("Standard normal distribution\n",input$normnumber,"random values"), probability = TRUE)
    lines(density(densitydata()))
    
  })
    
    #add render Valueboxes here ??
    
    output$meanBox <-renderValueBox({
      
    valueBox(
      
      round (mean(histdata()),3),"Mean",
      color="aqua"
      
      )
    })#output$meanBox <-renderValueBox
    
    output$medianBox <-renderValueBox({
      
      valueBox(
        
        round (median(histdata()),3),"Median",
        color="fuchsia"
        
      )
    })#output$medianBox <-renderValueBox
    
    
    output$sdBox <-renderValueBox({
      
      valueBox(
        
        round (sd(histdata()),3),"Standard deviation",
        color="purple"
        
      )
    })#output$sdBox <-renderValueBox
    
    
    output$meanInfoBox <-renderInfoBox({
      
      infoBox("Mean",
             round(mean(densitydata()),3),
             icon=icon("align-center"),
             color = "aqua")
      })#output$meanInfoBox <-renderInfoBox
    
    
    output$medianInfoBox <-renderInfoBox({
      
      infoBox("Median",
              round(median(densitydata()),3),
              icon=icon("area-chart"),
              color = "fuchsia")
    })#output$medianInfoBox <-renderInfoBox
    
    output$sdInfoBox <-renderInfoBox({
      
      infoBox("Standard Deviation",
              round(sd(densitydata()),3),
              icon=icon("scribd"),fill=FALSE,
              color = "purple")
    })#output$medianInfoBox <-renderInfoBox
    

  
}#server

shinyApp(ui, server)


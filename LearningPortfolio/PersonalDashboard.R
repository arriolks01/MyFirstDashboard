## app.R ##
library(shiny)
library(shinydashboard)

#install.packages("moments")

ui <- dashboardPage(
  dashboardHeader(
    title="My Practice dashboard"
  ),
  dashboardSidebar(
    
  sidebarMenu(
    
    menuItem("Uniform Distribution",tabName = "Uniform", icon=icon("square")),#item1
    
    menuItem("Normal Distribution",tabName = "Normal", icon=icon("bell-o")) #item2 
    
  ) #sidebar menu
    
    
    
  ), # dashboard Sidebar
  
  dashboardBody(
  
    
    fluidRow(
      
      column (width=6,
              
              
      box( title= "Please select a number",
           solidHeader= TRUE,
           background="yellow",
           status="warning",
           width=NULL,
           height=312,
           sliderInput(inputId="number",
                       label="",
                       value=500, min=25,max=1000)),
      
      
      box(
        title= "Histogram",
        solidHeader= TRUE,
        background="olive",
        status = "primary",
        width=NULL,
        plotOutput("hist",height=250))
      
    ),
    
    column(width=6,
    
           tabBox(#For Mean and Median, Upper Box
             title="Central Tendancy",
             id="tabs1",height = 120, width =  NULL,
             tabPanel("Mean",
                h2 (textOutput("meantext")),width=NULL),      
                      tabPanel("Median",
                               h2 (textOutput("mediantext")),width=NULL)
             ),                
    
           tabBox( #For variablity and standard deviation, Lower Box
             title="Variability",
             id="tabs2",height = 120, width =  NULL,
             side = "right",
             tabPanel("Variance",
                      h2 (textOutput("vartext")),width=NULL),      
             tabPanel("Standard Deviation",
                      h2 (textOutput("sdtext")),width=NULL)
           ),  
           
           tabBox( #For Skewness and Kurtosis
             title="Appearance",
             id="tabs3",height = 120, width =  NULL,
             side = "left",
             tabPanel("skewness",
                      h2 (textOutput("skewness")),width=NULL),      
             tabPanel("Kurtosis",
                      h2 (textOutput("kurtosis")),width=NULL)
           )       
      
    )
  
    )# fluidRow
    
    
    
    
    
  )
)



server <- function(input, output) {
  
  histdata <-reactive({runif(input$number,min=0,max=1)})
  
  output$hist<- renderPlot({
    
    hist(histdata(),xlab="Value",
         
         main=paste(input$number,"random values between 0 and 1"))
  })
 
   
  output$meantext <-renderText({
    paste("Mean",round(mean(histdata()),3))})
  
  
  output$mediantext <-renderText({
    paste("Median",round(median(histdata()),3))})
  
  
  output$vartext <-renderText({
    paste("Variance",round(var(histdata()),3))})
  
  output$sdtext <-renderText({
    paste("Standard deviation",round(sd(histdata()),3))})
  
 
  output$Skewness <-renderText({
    paste("Skewness",round(Skewness(histdata()),3))})
  
  output$kurtosis <-renderText({
    paste("Kurtosis",round(kurtosis(histdata()),3))})
  
  
  
  }

shinyApp(ui, server)


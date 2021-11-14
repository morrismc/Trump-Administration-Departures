# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(reshape2)
  library(DataCombine)
  EmploymentData <- read.csv('OPM_Data.csv')
  yearFrac = EmploymentData$Year + (EmploymentData$Month/12)
  EmploymentData <- cbind(EmploymentData, yearFrac)
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$cabPlot <- renderPlot({
    
    dtat    <-  filter(EmploymentData, Employment == input$cabinent)
     

    p <- ggplot(dtat, aes(x = dtat$fracYear, y = dtat$Location...All) )+
      geom_line()+
      # geom_line(data = stateDept, aes(x = stateDept$fracYear, y = stateDept$Foreign.Countries))+
      labs(x = 'Year',y = 'Number of Employees')+
      # labs(title = input$cabinent)+
      theme_classic()+
    
      
   
    
      theme(axis.text.x = element_text(angle = 45,hjust = 1))+
      scale_x_continuous( 
      breaks =seq(2006, 2018, 2),
      limits = c(2006, 2018))

      if(input$show_presidents == TRUE){
        p = p +  geom_vline(xintercept = 2009) +
                 geom_vline(xintercept = 2013) +
                 geom_vline(xintercept = 2017)
      }
    

    p
  })
  
}
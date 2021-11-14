# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Cabinent Agency Evolution"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      selectInput(inputId = "cabinent",
                  label = "Cabinent Agency",
                  choices = c('AF-DEPARTMENT OF THE AIR FORCE',
                              'AG-DEPARTMENT OF AGRICULTURE',
                              'AR-DEPARTMENT OF THE ARMY',
                              'CM-DEPARTMENT OF COMMERCE', 
                              'DD-DEPARTMENT OF DEFENSE',
                              'DJ-DEPARTMENT OF JUSTICE',
                              'DL-DEPARTMENT OF LABOR',
                              'DN-DEPARTMENT OF ENERGY',
                              'ED-DEPARTMENT OF EDUCATION',
                              'HE-DEPARTMENT OF HEALTH AND HUMAN SERVICES',
                              'HS-DEPARTMENT OF HOMELAND SECURITY',
                              'HU-DEPARTMENT OF HOUSING AND URBAN DEVELOPMENT',
                              'IN-DEPARTMENT OF THE INTERIOR',
                              'NV-DEPARTMENT OF THE NAVY',
                              'ST-DEPARTMENT OF STATE',
                              'TD-DEPARTMENT OF TRANSPORTATION',
                              'TR-DEPARTMENT OF THE TREASURY',
                              'VA-DEPARTMENT OF VETERANS AFFAIRS'
                              )),
                  
      # checkboxInput(inputID = "show_presidents",
      #               strong('Show Presidents'),
      #               FALSE)
      checkboxInput("show_presidents", "Show Presidents", FALSE),
                    verbatimTextOutput("value")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Line Plot ----
      plotOutput(outputId = "cabPlot")
      
    )
  )
)
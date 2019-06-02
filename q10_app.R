library(shiny)
library(ggplot2)
library(plotly)
#library(extrafont)
# Define UI ----

file <- "dog.bites.age.csv"
Objective <- "Age"
title1 <- "Do older or younger dogs bite more often?"
title2 <- paste("Dog ",Objective," within",sep="")
linecolor <- rgb(8,48,107,maxColorValue = 255)
fillcolor <- rgb(158,202,225,maxColorValue = 255)
textcolor <- rgb(8,48,107,maxColorValue = 255)

ui <- fluidPage(
  titlePanel(title1),
  
  sidebarLayout(
    
    sidebarPanel(
      radioButtons("Borough", h3("Boroughs"),
                   choices = list("New York City", "Bronx" , "Brooklyn" ,
                                  "Manhattan" ,"Queens" , "Staten Island" ),
                   selected = "Bronx"),
      selectInput("Year", h3("Year"), 
                  choices = as.list(c("All years",as.character(seq(2015,2017)))), selected = "2016")
      
    ),
    
    mainPanel(
      plotlyOutput("barplot")
    )
    
  )
)

# Define server logic ----
server <- function(input, output) {
  
  
  output$barplot <- renderPlotly({
    
    data <- read.csv(file)
    if (input$Borough == "New York City"){
      data <- data
    }
    else{
      data <- data[data$Borough== input$Borough,]
    }
    
    if (input$Year == "All years"){
      data <- data
    }
    else{
      data <- data[data$Year== input$Year,]
    }
    
    p <- ggplot(data=data)+
      geom_histogram(mapping = aes_string(x=Objective),
               color= linecolor,
               fill = fillcolor,
               binwidth = 0.5
               )+
      labs(title=paste(title2,input$Borough),x=paste("Dog",Objective))+
      theme_minimal()+
      theme(plot.title = element_text(color=textcolor, 
                                      size=14, 
                                      face="bold")
            # ,
            # axis.text.x = element_blank()
            )
    #theme_xkcd
    ggplotly(p)
    
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
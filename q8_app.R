library(shiny)
library(ggplot2)
library(plotly)
#library(extrafont)
# Define UI ----

file <- "dog.bites.gender.csv"
Objective <- "Gender"
title1 <- "Do male or female dogs bite more often?"
title2 <- paste("Dog ",Objective,"s within",sep="")
linecolor <- 'rgb(8,48,107)'
fillcolor <- c('rgb(211,94,96)', 'rgb(128,133,133)')
textcolor <- 'rgb(8,48,107)'
palettecolor <- "Blues"

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
    
    data <- data.frame(table(data[,Objective]))
    colnames(data)[1] <- Objective
      
    plot_ly(data, labels = ~Gender, values = ~Freq, type = 'pie',
            marker = list(colors = fillcolor,
                          line = list(color = linecolor, width = 1)),
            showlegend = T) %>%
      layout(title = paste(title2, input$Borough),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
            # ,
            # axis.text.x = element_blank()
    #theme_xkcd
    #ggplotly(p)
    
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
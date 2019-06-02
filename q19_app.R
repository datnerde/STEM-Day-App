library(shiny)
library(ggplot2)
library(plotly)
library(zipcode)
library(maps)
library(sf)
#library(nycmaps)
# library(wordcloud)
# library(tm)
#library(extrafont)
# Define UI ----

file <- "dog.bites.map.csv"
Objective <- "ZipCode"
title1 <- "Dog Bites Map"
title2 <- paste("Dog Bites within",sep="")
linecolor <- "black"
fillcolor <- "magma"
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
                  choices = as.list(c("All years",as.character(seq(2015,2017)))), selected = "2017")
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
    data <- data[order(data[,2],decreasing = T),]
    colnames(data)[1] <- Objective
    
    map <- st_read("ZIP_CODE_040114/ZIP_CODE_040114.shp")
    index <- data.frame("index"=as.numeric(rownames(map)),"ZIPCODE"=map$ZIPCODE)
    map1 <- merge(index,data,by.x="ZIPCODE",by.y=Objective,all.x=T)
    map1$Freq[is.na(map1$Freq)] <- 0
    map1 <- map1[order(map1$index,decreasing = F),]
    rownames(map1) <- map1$index
    map$Freq <- map1$Freq
    
    
    p <- ggplot(data=map) + 
      geom_sf(aes(fill=Freq,
                  text = paste("The area of <b>", ZIPCODE, "</b> had \n", Freq, "dog bites in ",input$Year)),
              size = 0.5, color = linecolor) +
      #scale_colour_brewer(palette = fillcolor)+
      scale_fill_viridis_c(option=fillcolor,direction = 1, trans = "sqrt")+
      labs(title=paste(title2,input$Borough,"in",input$Year))+
      theme_minimal()+
      theme(plot.title = element_text(color=textcolor, 
                                      size=14, 
                                      face="bold")
            ,
            axis.text.x = element_blank(),
            axis.text.y = element_blank()
      )
    p %>%
      ggplotly(tooltip = "text")
    
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
file <- "dog.bites.breed.csv"
Objective <- "Breed"
linecolor <- rgb(8,48,107,maxColorValue = 255)
fillcolor <- rgb(158,202,225,maxColorValue = 255)
textcolor <- rgb(8,48,107,maxColorValue = 255)
title2 <- paste("Dog ",Objective,"s within ",sep="")

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
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
    data <- data[1:10,]
    colnames(data)[1] <- Objective
    
    p <- ggplot(data=data)+
      geom_col(mapping = aes_string(x=Objective,y="Freq"),
               color= linecolor,
               fill = fillcolor,
               width = 0.5)+
      labs(title=paste(title2,input$Borough,"in",input$Year),x=paste("Dog",Objective))+
      theme_minimal()+
      theme(plot.title = element_text(color=textcolor, 
                                      size=14, 
                                      face="bold"),
            axis.text.x = element_blank())
    #theme_xkcd
    ggplotly(p)
    
    
  })
}
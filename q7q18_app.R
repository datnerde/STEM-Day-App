library(shiny)
library(ggplot2)
library(plotly)
#library(extrafont)
# Define UI ----

file1 <- "dog.bites.date.csv"
file2 <- "national.bites.csv"
Objective <- "Group"
title1 <- "Which weekday or month are there more dog bites?"
title2 <- paste("Dog bites",sep="")
linecolor <- rgb(8,48,107,maxColorValue = 255)
fillcolor <- "Blues"
textcolor <- rgb(8,48,107,maxColorValue = 255)

ui <- fluidPage(
  titlePanel(title1),
  
  sidebarLayout(
    
    sidebarPanel(
      
      "Number of dog bites in the nation is divided by 500.",
      br(),
      
      checkboxGroupInput("Borough", h3("Boroughs"),
                   choices = list("New York City", "Bronx" , "Brooklyn" ,
                                  "Manhattan" ,"Queens" , "Staten Island", "the United States"),
                   selected = "Bronx"),
    
    
      selectInput("Year", h3("Year"), 
                choices = as.list(c(as.character(seq(2015,2017)))), selected = "2017"),
      
      radioButtons("Group", h3("By"),
                         choices = list("Month","Weekday"),
                         selected = "Month")
    
      
    ),
    
    mainPanel(
      plotlyOutput("barplot")
    )
    
  )
)

# Define server logic ----
server <- function(input, output) {
  
  
  output$barplot <- renderPlotly({
    
    data1 <- read.csv(file1)
    data1 <- data1[,c("Borough", "Year", input$Group)]
    data1 <- data1[data1$Year==input$Year,]
    data2 <- data.frame()
    data3 <- data.frame()
    data <- data.frame()
    
    if ("the United States" %in% input$Borough){
      data2 <- read.csv(file2)
      data2 <- data2[data2$Group==input$Group,]
      data2 <- data2[,c("Borough","Freq","Lab")]
      colnames(data2) <- c("Borough","Freq",input$Group)
      data2$Freq <- data2$Freq/500
    }
    
    
    if ("New York City" %in% input$Borough){
      data3 <- data.frame(table(data1[,input$Group]))
      colnames(data3) <- c(input$Group,"Freq")
      data3$Borough <- "New York City"
    }

    data4 <- data1[data1$Borough %in% input$Borough,]
    
    if (nrow(data4) != 0){
      for (i in unique(data4$Borough)){
        data5 <- data4[data4$Borough == i,]
        data5 <- data.frame(table(data5[,input$Group]))
        colnames(data5) <- c(input$Group,"Freq")
        data5$Borough <- i
        data <- rbind(data,data5)
      }
    }
    
    data <- rbind(data,data2,data3)
    
    if(input$Group == "Month"){
      data[,input$Group] <- factor(data[,input$Group],levels=month.abb)
    }
    else{
      weekday.abb <- c("Sun","Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
      data[,input$Group] <- factor(data[,input$Group],levels=weekday.abb)
      }
    
    p <- ggplot(data=data)+
      geom_bar(mapping = aes_string(x=input$Group,y="Freq",fill="Borough"),
               stat="identity", 
               position=position_dodge(),
               color= linecolor,
               width = 0.5)+
      labs(title=paste(title2,"in",input$Year),x=input$Group,y="Number of Dog Bites")+
      theme_minimal()+
      scale_fill_brewer(palette=fillcolor)+
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
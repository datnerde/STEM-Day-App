library(sf)
server <- function(input, output) {
  
  ##q4q6
  output$q4q6_barplot <- renderPlotly({
    file <- "dog.bites.breed.csv"
    Objective <- "Breed"
    linecolor <- rgb(8,48,107,maxColorValue = 255)
    fillcolor <- rgb(158,202,225,maxColorValue = 255)
    textcolor <- rgb(8,48,107,maxColorValue = 255)
    title2 <- paste("Dog ",Objective,"s within ",sep="")
    
    
    data <- read.csv(file)
    if (input$q4q6Borough == "New York City"){
      data <- data
    }
    else{
      data <- data[data$Borough== input$q4q6Borough,]
    }
    
    if (input$q4q6Year == "All years"){
      data <- data
    }
    else{
      data <- data[data$Year== input$q4q6Year,]
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
      labs(title=paste(title2,input$q4q6Borough,"in",input$q4q6Year),x=paste("Dog",Objective))+
      theme_minimal()+
      theme(plot.title = element_text(color=textcolor, 
                                      size=14, 
                                      face="bold"),
            axis.text.x = element_blank())
    #theme_xkcd
    ggplotly(p)
  })
  
  ##q7q18
  output$q7q18_barplot <- renderPlotly({
    
    file1 <- "dog.bites.date.csv"
    file2 <- "national.bites.csv"
    Objective <- "Group"
    title1 <- "Which weekday or month are there more dog bites?"
    title2 <- paste("Dog bites",sep="")
    linecolor <- rgb(8,48,107,maxColorValue = 255)
    fillcolor <- "Blues"
    textcolor <- rgb(8,48,107,maxColorValue = 255)
    
    data1 <- read.csv(file1)
    data1 <- data1[,c("Borough", "Year", input$q7q18Group)]
    data1 <- data1[data1$Year==input$q7q18Year,]
    data2 <- data.frame()
    data3 <- data.frame()
    data <- data.frame()
    
    if ("the United States" %in% input$q7q18Borough){
      data2 <- read.csv(file2)
      data2 <- data2[data2$Group==input$q7q18Group,]
      data2 <- data2[,c("Borough","Freq","Lab")]
      colnames(data2) <- c("Borough","Freq",input$q7q18Group)
      data2$Freq <- data2$Freq/500
    }
    
    
    if ("New York City" %in% input$q7q18Borough){
      data3 <- data.frame(table(data1[,input$q7q18Group]))
      colnames(data3) <- c(input$q7q18Group,"Freq")
      data3$Borough <- "New York City"
    }
    
    data4 <- data1[data1$Borough %in% input$q7q18Borough,]
    
    if (nrow(data4) != 0){
      for (i in unique(data4$Borough)){
        data5 <- data4[data4$Borough == i,]
        data5 <- data.frame(table(data5[,input$q7q18Group]))
        colnames(data5) <- c(input$q7q18Group,"Freq")
        data5$Borough <- i
        data <- rbind(data,data5)
      }
    }
    
    data <- rbind(data,data2,data3)
    
    if(input$q7q18Group == "Month"){
      data[,input$q7q18Group] <- factor(data[,input$q7q18Group],levels=month.abb)
    }
    else{
      weekday.abb <- c("Sun","Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
      data[,input$q7q18Group] <- factor(data[,input$q7q18Group],levels=weekday.abb)
    }
    
    p <- ggplot(data=data)+
      geom_bar(mapping = aes_string(x=input$q7q18Group,y="Freq",fill="Borough"),
               stat="identity", 
               position=position_dodge(),
               color= linecolor,
               width = 0.5)+
      labs(title=paste(title2,"in",input$q7q18Year),x=input$q7q18Group,y="Number of Dog Bites")+
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
  
  ##q8
  output$q8_barplot <- renderPlotly({
    
    file <- "dog.bites.gender.csv"
    Objective <- "Gender"
    title1 <- "Do male or female dogs bite more often?"
    title2 <- paste("Dog ",Objective,"s within",sep="")
    linecolor <- 'rgb(8,48,107)'
    fillcolor <- c('rgb(211,94,96)', 'rgb(128,133,133)')
    textcolor <- 'rgb(8,48,107)'
    palettecolor <- "Blues"
    
    data <- read.csv(file)
    if (input$q8Borough == "New York City"){
      data <- data
    }
    else{
      data <- data[data$Borough== input$q8Borough,]
    }
    
    if (input$q8Year == "All years"){
      data <- data
    }
    else{
      data <- data[data$Year== input$q8Year,]
    }
    
    data <- data.frame(table(data[,Objective]))
    colnames(data)[1] <- Objective
    
    plot_ly(data, labels = ~Gender, values = ~Freq, type = 'pie',
            marker = list(colors = fillcolor,
                          line = list(color = linecolor, width = 1)),
            showlegend = T) %>%
      layout(title = paste(title2, input$q8Borough),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    # ,
    # axis.text.x = element_blank()
    #theme_xkcd
    #ggplotly(p)
    
    
  })
  
  
  ##q19
  output$q19_barplot <- renderPlotly({
    
    file <- "dog.bites.map.csv"
    Objective <- "ZipCode"
    title1 <- "Dog Bites Map"
    title2 <- paste("Dog Bites within",sep="")
    linecolor <- "black"
    fillcolor <- "magma"
    textcolor <- rgb(8,48,107,maxColorValue = 255)
    
    data <- read.csv(file)
    if (input$q19Borough == "New York City"){
      data <- data
    }
    else{
      data <- data[data$Borough== input$q19Borough,]
    }
    
    if (input$q19Year == "All years"){
      data <- data
    }
    else{
      data <- data[data$Year== input$q19Year,]
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
                  text = paste("The area of <b>", ZIPCODE, "</b> had \n", Freq, "dog bites in ",input$q19Year)),
              size = 0.5, color = linecolor) +
      #scale_colour_brewer(palette = fillcolor)+
      scale_fill_viridis_c(option=fillcolor,direction = 1, trans = "sqrt")+
      labs(title=paste(title2,input$q19Borough,"in",input$q19Year))+
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
  
  ##q9
  output$q9_barplot <- renderPlotly({
    
    file <- "dog.bites.spay.csv"
    Objective <- "SpayNeuter"
    title1 <- "Do spayed/neutered vs non-spayed/neutered dogs bite more often?"
    title2 <- paste("Dogs spayed/neutered within ",sep="")
    linecolor <- 'rgb(8,48,107)'
    fillcolor <- c('rgb(211,94,96)', 'rgb(128,133,133)')
    textcolor <- 'rgb(8,48,107)'
    palettecolor <- "Blues"
    
    data <- read.csv(file)
    if (input$q9Borough == "New York City"){
      data <- data
    }
    else{
      data <- data[data$Borough== input$q9Borough,]
    }
    
    if (input$q9Year == "All years"){
      data <- data
    }
    else{
      data <- data[data$Year== input$q9Year,]
    }
    
    data <- data.frame(table(data[,Objective]))
    colnames(data)[1] <- Objective
    
    plot_ly(data, labels = ~SpayNeuter, values = ~Freq, type = 'pie',
            marker = list(colors = fillcolor,
                          line = list(color = linecolor, width = 1)),
            showlegend = T) %>%
      layout(title = paste(title2, input$q9Borough),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    # ,
    # axis.text.x = element_blank()
    #theme_xkcd
    #ggplotly(p)
    
    
  })
  
  
  ##q10
  output$q10_barplot <- renderPlotly({
    
    file <- "dog.bites.age.csv"
    Objective <- "Age"
    title1 <- "Do older or younger dogs bite more often?"
    title2 <- paste("Dog ",Objective," within",sep="")
    linecolor <- rgb(8,48,107,maxColorValue = 255)
    fillcolor <- rgb(158,202,225,maxColorValue = 255)
    textcolor <- rgb(8,48,107,maxColorValue = 255)
    
    data <- read.csv(file)
    if (input$q10Borough == "New York City"){
      data <- data
    }
    else{
      data <- data[data$Borough== input$q10Borough,]
    }
    
    if (input$q10Year == "All years"){
      data <- data
    }
    else{
      data <- data[data$Year== input$q10Year,]
    }
    
    p <- ggplot(data=data)+
      geom_histogram(mapping = aes_string(x=Objective),
                     color= linecolor,
                     fill = fillcolor,
                     binwidth = 0.5
      )+
      labs(title=paste(title2,input$q10Borough),x=paste("Dog",Objective))+
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
  
  
  ##q1
  output$q1_barplot <- renderPlotly({
    
    file <- "dog.licenses.breed.csv"
    Objective <- "Breed"
    title1 <- "What is the most popular breed of dogs within each borough?"
    title2 <- paste("Dog ",Objective,"s within",sep="")
    linecolor <- rgb(8,48,107,maxColorValue = 255)
    fillcolor <- rgb(158,202,225,maxColorValue = 255)
    textcolor <- rgb(8,48,107,maxColorValue = 255)
    
    
    data <- read.csv(file)
    if (input$q1Borough == "New York City"){
      data <- data
    }
    else{
      data <- data[data$Borough== input$q1Borough,]
    }
    
    if (input$q1Year == "All years"){
      data <- data
    }
    else{
      data <- data[data$BirthYear== input$q1Year,]
    }
    
    data <- data.frame(table(data[,Objective]))
    data <- data[order(data[,2],decreasing = T),]
    data <- data[1:10,]
    colnames(data)[1] <- Objective
    
    p <- ggplot(data=data)+
      geom_bar(mapping = aes_string(x=Objective,y="Freq"),
               stat="identity", 
               position=position_dodge(),
               color= linecolor,
               fill= fillcolor,
               width = 0.5)+
      labs(title=paste(title2,input$q1Borough,"in",input$q1Year),x=paste("Dog",Objective),y="Number of Dog Breeds")+
      theme_minimal()+
      #scale_fill_brewer(palette=fillcolor)+
      theme(plot.title = element_text(color=textcolor, 
                                      size=14, 
                                      face="bold")
            ,
            axis.text.x = element_blank()
      )
    
    
    #theme_xkcd
    ggplotly(p)
    
    
  })
  
  
  
  
  ##q5
  output$q5_barplot <- renderPlotly({
    
    
    file <- "dog.licenses.name.csv"
    Objective <- "Name"
    title1 <- "What is the most popular dog name?"
    title2 <- paste("Dog ",Objective,"s within",sep="")
    linecolor <- rgb(8,48,107,maxColorValue = 255)
    fillcolor <- rgb(158,202,225,maxColorValue = 255)
    textcolor <- rgb(8,48,107,maxColorValue = 255)
    
    data <- read.csv(file)
    if (input$q5Borough == "New York City"){
      data <- data
    }
    else{
      data <- data[data$Borough== input$q5Borough,]
    }
    
    if (input$q5Year == "All years"){
      data <- data
    }
    else{
      data <- data[data$BirthYear== input$q5Year,]
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
      labs(title=paste(title2,input$q5Borough),x=paste("Dog",Objective))+
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
  
  ##q11
  output$q11_barplot <- renderPlotly({
    
    
    file <- "dog.licenses.date.csv"
    Objective <- "LicenseIssuedYear"
    Objective2 <- "LicenseIssuedMonth"
    title1 <- "Which month and year had the most number of dog licenses issued?"
    title2 <- paste("Dog licenses",sep="")
    linecolor <- rgb(8,48,107,maxColorValue = 255)
    fillcolor <- "Blues"
    textcolor <- rgb(8,48,107,maxColorValue = 255)
    
    data1 <- read.csv(file)
    data1 <- data1[,c("Borough", Objective, Objective2)]
    data1 <- data1[data1[,Objective]==input$q11Year,]
    data2 <- data.frame()
    data <- data.frame()
    
    
    if ("New York City" %in% input$q11Borough){
      data2 <- data.frame(table(data1[,Objective2]))
      colnames(data2) <- c(Objective2,"Freq")
      data2$Borough <- "New York City"
    }
    
    data4 <- data1[data1$Borough %in% input$q11Borough,]
    
    if (nrow(data4) != 0){
      for (i in unique(data4$Borough)){
        data5 <- data4[data4$Borough == i,]
        data5 <- data.frame(table(data5[,Objective2]))
        colnames(data5) <- c(Objective2,"Freq")
        data5$Borough <- i
        data <- rbind(data,data5)
      }
    }
    
    data <- rbind(data,data2)
    
    # if(input$q11Group == "Month"){
    #   data[,input$q11Group] <- factor(data[,input$q11Group],levels=month.abb)
    # }
    # else{
    #   weekday.abb <- c("Sun","Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
    #   data[,input$q11Group] <- factor(data[,input$q11Group],levels=weekday.abb)
    # }
    
    p <- ggplot(data=data)+
      geom_bar(mapping = aes_string(x=Objective2,y="Freq",fill="Borough"),
               stat="identity", 
               position=position_dodge(),
               color= linecolor,
               width = 0.5)+
      labs(title=paste(title2,"in",input$q11Year),x=Objective2,y="Number of Dog Licences Issued")+
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
  
  
  ##q12
  output$q12_barplot <- renderPlotly({
    
    file <- "dog.licenses.date.csv"
    Objective <- "LicenseExpiredYear"
    Objective2 <- "LicenseExpiredMonth"
    title1 <- "Which month and year had the most number of dog licenses expired?"
    title2 <- paste("Dog licenses",sep="")
    linecolor <- rgb(8,48,107,maxColorValue = 255)
    fillcolor <- "Blues"
    textcolor <- rgb(8,48,107,maxColorValue = 255)
    
    data1 <- read.csv(file)
    data1 <- data1[,c("Borough", Objective, Objective2)]
    data1 <- data1[data1[,Objective]==input$q12Year,]
    data2 <- data.frame()
    data <- data.frame()
    
    
    if ("New York City" %in% input$q12Borough){
      data2 <- data.frame(table(data1[,Objective2]))
      colnames(data2) <- c(Objective2,"Freq")
      data2$Borough <- "New York City"
    }
    
    data4 <- data1[data1$Borough %in% input$q12Borough,]
    
    if (nrow(data4) != 0){
      for (i in unique(data4$Borough)){
        data5 <- data4[data4$Borough == i,]
        data5 <- data.frame(table(data5[,Objective2]))
        colnames(data5) <- c(Objective2,"Freq")
        data5$Borough <- i
        data <- rbind(data,data5)
      }
    }
    
    data <- rbind(data,data2)
    
    # if(input$q12Group == "Month"){
    #   data[,input$q12Group] <- factor(data[,input$q12Group],levels=month.abb)
    # }
    # else{
    #   weekday.abb <- c("Sun","Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
    #   data[,input$q12Group] <- factor(data[,input$q12Group],levels=weekday.abb)
    # }
    
    p <- ggplot(data=data)+
      geom_bar(mapping = aes_string(x=Objective2,y="Freq",fill="Borough"),
               stat="identity", 
               position=position_dodge(),
               color= linecolor,
               width = 0.5)+
      labs(title=paste(title2,"in",input$q12Year),x=Objective2,y="Number of Dog Licences Expired")+
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
    

  
  ##q13q14
  output$q13q14_barplot <- renderPlotly({
  
    file <- "dog.licenses.map.csv"
    Objective <- "ZipCode"
    title1 <- "Dog Licenses Map"
    title2 <- paste("Dog licenses issued within",sep="")
    linecolor <- "black"
    fillcolor <- "magma"
    textcolor <- rgb(8,48,107,maxColorValue = 255)
    
    
    data <- read.csv(file)
    if (input$q13q14Borough == "New York City"){
      data <- data
    }
    else{
      data <- data[data$Borough== input$q13q14Borough,]
    }
    
    if (input$q13q14Year == "All years"){
      data <- data
    }
    else{
      data <- data[data$LicenseIssuedYear== input$q13q14Year,]
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
                  text = paste("The area of <b>", ZIPCODE, "</b> had \n", Freq, "dog licenses issued in ",input$q13q14Year)),
              size = 0.5, color = linecolor) +
      #scale_colour_brewer(palette = fillcolor)+
      scale_fill_viridis_c(option=fillcolor,direction = 1, trans = "sqrt")+
      labs(title=paste(title2,input$q13q14Borough,"in",input$q13q14Year))+
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
  
  ##q17q21
  output$q17q21_barplot <- renderPlotly({
    
    file <- "dog.name.year.csv"
    Objective <- "Name"
    title1 <- "Dog Name Word Cloud"
    title2 <- paste("Dog ",Objective,"s within",sep="")
    linecolor <- rgb(8,48,107,maxColorValue = 255)
    fillcolor <- rgb(158,202,225,maxColorValue = 255)
    textcolor <- rgb(8,48,107,maxColorValue = 255)
    
    
    
    data <- read.csv(file)
    if (input$q17q21Borough == "New York City"){
      data <- data
    }
    else{
      data <- data[data$Borough== input$q17q21Borough,]
    }
    
    if (input$q17q21Year == "All years"){
      data <- data
    }
    else{
      data <- data[data$BirthYear== input$q17q21Year,]
    }
    
    data <- data.frame(table(data[,Objective]))
    data <- data[order(data[,2],decreasing = T),]
    data <- data[1:30,]
    colnames(data)[1] <- Objective
    
    
    wordcloud_rep(data[,Objective],data[,2]/100, scale=c(2,0.2),
                  min.freq = 50,max.words=20,
                  colors=brewer.pal(8, "RdBu"))
    
   
  })
  
  ##q17q21v2
  wordcloud_rep <- repeatable(wordcloud)
  
  output$q17q21v2_barplot <- renderPlotly({
    
    file <- "dog.name.year.csv"
    Objective <- "Name"
    title1 <- "Dog Name Word Cloud"
    title2 <- paste("Dog ",Objective,"s within",sep="")
    linecolor <- rgb(8,48,107,maxColorValue = 255)
    fillcolor <- rgb(158,202,225,maxColorValue = 255)
    textcolor <- rgb(8,48,107,maxColorValue = 255)
    
    
    data <- read.csv(file)
    if (input$q17q21v2Borough == "New York City"){
      data <- data
    }
    else{
      data <- data[data$Borough== input$q17q21v2Borough,]
    }
    
    if (input$q17q21v2Year == "All years"){
      data <- data
    }
    else{
      data <- data[data$BirthYear== input$q17q21v2Year,]
    }
    
    data <- data.frame(table(data[,Objective]))
    data <- data[order(data[,2],decreasing = T),]
    data <- data[1:30,]
    colnames(data)[1] <- Objective
    
    
    wordcloud(data[,Objective],data[,2]/100, scale=c(2,0.2),
              min.freq = 0,max.words=10,
              colors=brewer.pal(8, "RdBu"))
    
  })
  
  ##q20
  output$q20_barplot <- renderPlotly({
    
    
    file <- "dog.name.year.csv"
    Objective <- "Name"
    title1 <- "Which names no longer seem to be popular?"
    title2 <- paste("Dog ",Objective,"s within",sep="")
    linecolor <- rgb(8,48,107,maxColorValue = 255)
    fillcolor <- rgb(158,202,225,maxColorValue = 255)
    textcolor <- rgb(8,48,107,maxColorValue = 255)
    
    
    data <- read.csv(file)
    if (input$q20Borough == "New York City"){
      data <- data
    }
    else{
      data <- data[data$Borough== input$q20Borough,]
    }
    
    if (input$q20Year == "All years"){
      data <- data
    }
    else{
      data <- data[data$BirthYear== input$q20Year,]
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
      labs(title=paste(title2,input$q20Borough,"in",input$q20Year),x=paste("Dog",Objective))+
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
    
  ##b3
  output$b3_barplot <- renderPlotly({
    
    
    file <- "dog.licenses.gender.csv"
    Objective <- "Gender"
    title1 <- "Are there more male dogs born in certain months than female dogs?"
    title2 <- paste("Dog born within",sep="")
    linecolor <- rgb(8,48,107,maxColorValue = 255)
    fillcolor <- "Blues"
    textcolor <- rgb(8,48,107,maxColorValue = 255)
    
    
    data <- read.csv(file)
    data <- data[,c("Borough","BirthYear","BirthMonth","Gender")]
    if (input$b3Borough == "New York City"){
      data <- data
    }
    else{
      data <- data[data$Borough== input$b3Borough,]
    }
    
    if (input$b3Year == "All years"){
      data <- data
    }
    else{
      data <- data[data$BirthYear== input$b3Year,]
    }
    
    data1 <- data[data[,Objective]=="Female",]
    data1 <- data.frame(table(data1[,"BirthMonth"]))
    colnames(data1) <- c("Month","Freq")
    data1[,Objective] <- "Female"
    data2 <- data[data[,Objective]=="Male",]
    data2 <- data.frame(table(data2[,"BirthMonth"]))
    colnames(data2) <- c("Month","Freq")
    data2[,Objective] <- "Male"
    
    data <- rbind(data1,data2)
    
    p <- ggplot(data=data)+
      geom_bar(mapping = aes_string(x="Month",y="Freq",fill=Objective),
               stat="identity", 
               #position=position_dodge(),
               color= linecolor,
               width = 0.5)+
      labs(title=paste(title2,input$b3Borough,"in",input$b3Year),x="Month",y="Number of Dog born")+
      theme_minimal()+
      scale_fill_brewer(palette=fillcolor)+
      theme(plot.title = element_text(color=textcolor, 
                                      size=14, 
                                      face="bold")
            # ,
            # axis.text.x = element_blank()
      )
    
    ggplotly(p)
    
    
  })
  
  #b5b6
  output$b5b6_barplot <- renderPlotly({
    
    file <- "dog.licenses.date2.csv"
    Objective <- "AnimalBirthMonth"
    Objective2 <- "LicenseIssuedDate"
    title1 <- "Does the license issue date correspond with the dogs birth month?"
    title2 <- paste("Dog born/license issued date within",sep="")
    linecolor <- "Dark2"
    fillcolor <- "Blues"
    textcolor <- rgb(8,48,107,maxColorValue = 255)
    
    
    
    data <- read.csv(file)
    data <- data[,c("Borough","BirthYear",Objective,Objective2)]
    #data <- as.character(data)
    
    if (input$b5b6Borough == "New York City"){
      data <- data
    }
    else{
      data <- data[data$Borough== input$b5b6Borough,]
    }
    
    if (input$b5b6Year == "All years"){
      data <- data
    }
    else{
      data <- data[data$BirthYear== input$b5b6Year,]
    }
    
    data[,Objective] <- as.Date(data[,Objective],"%Y-%m-%d")
    data[,Objective2] <- as.Date(data[,Objective2],"%Y-%m-%d")
    data <- data[order(data[,Objective],decreasing = F),]
    data$index <- 1:nrow(data)
    
    data1 <- data[,c("index",Objective)]
    colnames(data1) <- c("index","Date")
    data1$Group <- Objective
    
    data2 <- data[,c("index",Objective2)]
    colnames(data2) <- c("index","Date")
    data2$Group <- Objective2
    
    data <- rbind(data1,data2)
    
    p <- ggplot(data=data)+
      
      geom_line(mapping = aes_string(x="index",y="Date",color="Group"),
                #stat="identity", 
                #position=position_dodge(),
                #color= linecolor[1],
                lwd = 0.5)+
      
      labs(title=paste(title2,input$b5b6Borough,"in",input$b5b6Year),x="Dog",y="Date")+
      theme_minimal()+
      scale_color_brewer(palette=linecolor)+
      theme(plot.title = element_text(color=textcolor, 
                                      size=10, 
                                      face="bold")
            # ,
            # axis.text.x = element_blank()
      )
    
    ggplotly(p)
    
    
  })
  
  
  
  
  
  ##q12
  output$qa12_barplot <- renderPlotly({
    
    
    
  })
  
  
  ##q12
  output$qa12_barplot <- renderPlotly({
    
    
    
  })
  
  ##q12
  output$qa12_barplot <- renderPlotly({
    
    
    
  })
 
}
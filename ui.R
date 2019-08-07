## app.R ##
library(shinydashboard)
library(plotly)
library(dashboardthemes)
#setting for dog bites
Objective <- "Breed"
q4q6title1 <- "Which dog breed bites the most?"
q4q6title2 <- paste("Dog ",Objective,"s within ",sep="")
q7q18title1 <- "Which weekday or month are there more dog bites?"
q7q18title2 <- paste("Dog bites",sep="")
q8title1 <- "Do male or female dogs bite more often?"
q8title2 <- paste("Dog ",Objective,"s within",sep="")
q19title1 <- "Dog Bites Map"
q19title2 <- paste("Dog Bites within",sep="")
q9title1 <- "Do spayed/neutered vs non-spayed/neutered dogs bite more often?"
q10title1 <- "Do older or younger dogs bite more often?"
q1title1 <- "What is the most popular breed of dogs within each borough?"
q5title1 <- "What is the most popular dog name?"
q11title1 <- "Which month and year had the most number of dog licenses issued?"
q12title1 <- "Which month and year had the most number of dog licenses expired?"
q13q14title1 <- "Dog Licenses Map"
q17q21title1 <- "Dog Name Word Cloud"
q17q21v2title1 <- "Dog Name Word Cloud"
q20title1 <- "Which names no longer seem to be popular?"
b3title1 <- "Are there more male dogs born in certain months than female dogs?"
b5b6title1 <- "Does the license issue date correspond with the dogs birth month?"

#create new logo
logo_blue_gradient <- shinyDashboardLogoDIY(
  
  boldText = "Puppy Scientist"
  ,mainText = ""
  ,textSize = 16
  ,badgeText = "DSI"
  ,badgeTextColor = "white"
  ,badgeTextSize = 2
  ,badgeBackColor = "#40E0D0"
  ,badgeBorderRadius = 3
  
)

#UI Sidebar companent
ui <- dashboardPage(
  dashboardHeader(title = logo_blue_gradient
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("", tabName = "map", icon = icon("archway")),
      menuItem("DogBites", tabName = "dashboard1", icon = icon("feather"),
               menuSubItem("Bites/Breed", tabName = "q4q6"),
               menuSubItem("Bites/Weekday", tabName = "q7q18"),
               menuSubItem("Bites/Gender", tabName = "q8"),
               menuSubItem("Bites/Neuter", tabName = "q9"),
               menuSubItem("Bites/Age", tabName = "q10"),
               menuSubItem("Bushwick Bites", tabName = "q19")
      ),
      menuItem("DogLicense", tabName = "dashboard2", icon = icon("bone", lib = "font-awesome"),
               menuSubItem("Popular Breeds", tabName = "q1"),
               menuSubItem("Popular Names", tabName = "q5"),
               menuSubItem("Issued Licenses", tabName = "q11"),
               menuSubItem("License Expirations", tabName = "q12"),
               menuSubItem("Most/Fewest Licences", tabName = "q13q14"),
           #    menuSubItem("Name/Year", tabName = "q17q21"),
            #   menuSubItem("Name/Year", tabName = "q17q21v2"),
               menuSubItem("Name/Year", tabName = "q20"),
               menuSubItem("Birth/Gender", tabName = "b3"),
               menuSubItem("License/Birth", tabName = "b5b6")
      ),
      menuItem("DogRun", tabName = "dashboard3", icon = icon("paw"),
               menuSubItem("E1", tabName = "e1"),
               menuSubItem("B8", tabName = "b8")
               #menuSubItem("Popular Breeds", tabName = "q1"),
               #menuSubItem("Popular Breeds", tabName = "q1")
               )
    )
  ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "purple_gradient"
    ),
    tabItems(
      # First tab content
      tabItem(tabName = "map",
              mainPanel(
                img(src='dashboardpic.jpg', align = "center")
              )
              
              
              ),
      
      tabItem(tabName = "e1",
              mainPanel(
                img(src='E1.jpeg', align = "center")
              )
              
              
      ),
      
      tabItem(tabName = "b8",
              mainPanel(
                img(src='B8.png', align = "center")
              )
              
              
      ),
      
      
      
      
      ## q10_barplot
      tabItem(tabName = "q10",
              titlePanel(q10title1),
              
              sidebarLayout(
                
                sidebarPanel(
                  radioButtons("q10Borough", h3("Boroughs"),
                               choices = list("New York City", "Bronx" , "Brooklyn" ,
                                              "Manhattan" ,"Queens" , "Staten Island" ),
                               selected = "Bronx"),
                  selectInput("q10Year", h3("Year"), 
                              choices = as.list(c("All years",as.character(seq(2015,2017)))), selected = "2016")
                  
                ),
                mainPanel(
                  plotlyOutput("q10_barplot")
                )
                
              )
      ),
      
      ## q9_barplot
      tabItem(tabName = "q9",
              titlePanel(q9title1),
              
              sidebarLayout(
                
                sidebarPanel(
                  radioButtons("q9Borough", h3("Boroughs"),
                               choices = list("New York City", "Bronx" , "Brooklyn" ,
                                              "Manhattan" ,"Queens" , "Staten Island" ),
                               selected = "Bronx"),
                  selectInput("q9Year", h3("Year"), 
                              choices = as.list(c("All years",as.character(seq(2015,2017)))), selected = "2016")
                ),
                
                mainPanel(
                  plotlyOutput("q9_barplot")
                )
                
              )
      ),
      
      
      
      # DogBites - q4q6
      tabItem(tabName = "q4q6",
              titlePanel(q4q6title1),
              sidebarLayout(
                
                sidebarPanel(
                  radioButtons("q4q6Borough", h3("Boroughs"),
                               choices = list("New York City", "Bronx" , "Brooklyn" ,
                                              "Manhattan" ,"Queens" , "Staten Island" ),
                               selected = "Bronx"),
                  selectInput("q4q6Year", h3("Year"), 
                              choices = as.list(c("All years",as.character(seq(2015,2017)))), selected = "2016")
                ),
                
                mainPanel(
                  plotlyOutput("q4q6_barplot")
                )
                
              )
      ),
      
      ## q7q18_barplot
      tabItem(tabName = "q7q18",
              titlePanel(q7q18title1),
              
              sidebarLayout(
                
                sidebarPanel(
                  
                  "Number of dog bites in the nation is divided by 500.",
                  br(),
                  
                  checkboxGroupInput("q7q18Borough", h3("Boroughs"),
                                     choices = list("New York City", "Bronx" , "Brooklyn" ,
                                                    "Manhattan" ,"Queens" , "Staten Island", "the United States"),
                                     selected = "Bronx"),
                  
                  
                  selectInput("q7q18Year", h3("Year"), 
                              choices = as.list(c(as.character(seq(2015,2017)))), selected = "2017"),
                  
                  radioButtons("q7q18Group", h3("By"),
                               choices = list("Month","Weekday"),
                               selected = "Month")
                  
                  
                ),
                
                mainPanel(
                  plotlyOutput("q7q18_barplot")
                )
                
              )
      ),
      
      ## q8_barplot
      tabItem(tabName = "q8",
              titlePanel(q8title1),
              
              sidebarLayout(
                
                sidebarPanel(
                  radioButtons("q8Borough", h3("Boroughs"),
                               choices = list("New York City", "Bronx" , "Brooklyn" ,
                                              "Manhattan" ,"Queens" , "Staten Island" ),
                               selected = "Bronx"),
                  selectInput("q8Year", h3("Year"), 
                              choices = as.list(c("All years",as.character(seq(2015,2017)))), selected = "2017")
                ),
                
                mainPanel(
                  plotlyOutput("q8_barplot")
                )
                
              )
      ),
      
      ## q19_barplot
      tabItem(tabName = "q19",
              titlePanel(q19title1),
              
              sidebarLayout(
                
                sidebarPanel(
                  radioButtons("q19Borough", h3("Boroughs"),
                               choices = list("New York City", "Bronx" , "Brooklyn" ,
                                              "Manhattan" ,"Queens" , "Staten Island" ),
                               selected = "Bronx"),
                  selectInput("q19Year", h3("Year"), 
                              choices = as.list(c("All years",as.character(seq(2015,2017)))), selected = "2017")
                ),
                
                mainPanel(
                  plotlyOutput("q19_barplot")
                )
                
              )
      ),
      
      
      ## q1_barplot
      tabItem(tabName = "q1",
              titlePanel(q1title1),
              
              sidebarLayout(
                
                sidebarPanel(
                  radioButtons("q1Borough", h3("Boroughs"),
                               choices = list("New York City", "Bronx" , "Brooklyn" ,
                                              "Manhattan" ,"Queens" , "Staten Island" ),
                               selected = "Bronx"),
                  selectInput("q1Year", h3("Year"), 
                              choices = as.list(c("All years",as.character(seq(2000,2016)))), selected = "2016")
                ),
                
                mainPanel(
                  plotlyOutput("q1_barplot")
                )
                
              )
      ),
      
      
      ## q5_barplot
      tabItem(tabName = "q5",
              titlePanel(q5title1),
              
              
              sidebarLayout(
                
                sidebarPanel(
                  radioButtons("q5Borough", h3("Boroughs"),
                               choices = list("New York City", "Bronx" , "Brooklyn" ,
                                              "Manhattan" ,"Queens" , "Staten Island" ),
                               selected = "Bronx"),
                  selectInput("q5Year", h3("Year"), 
                              choices = as.list(c("All years",as.character(seq(2000,2016)))), selected = "2016")
                  
                ),
                
                
                mainPanel(
                  plotlyOutput("q5_barplot")
                )
                
              )
      ),
      
      ## q11_barplot
      tabItem(tabName = "q11",
              titlePanel(q11title1),
              
              
              sidebarLayout(
                
                sidebarPanel(
                  
                  checkboxGroupInput("q11Borough", h3("Boroughs"),
                                     choices = list("New York City", "Bronx" , "Brooklyn" ,
                                                    "Manhattan" ,"Queens" , "Staten Island"),
                                     selected = "Bronx"),
                  
                  
                  selectInput("q11Year", h3("Year"), 
                              choices = as.list(c(as.character(seq(2015,2016)))), selected = "2016")
                  
                  # radioButtons("Group", h3("By"),
                  #              choices = list("Month","Weekday"),
                  #              selected = "Month")
                  
                  
                ),
                
                
                
                mainPanel(
                  plotlyOutput("q11_barplot")
                )
                
              )
      ),
      
      ## q12_barplot
      tabItem(tabName = "q12",
              titlePanel(q12title1),
              sidebarLayout(
                
                sidebarPanel(
                  
                  checkboxGroupInput("q12Borough", h3("Boroughs"),
                                     choices = list("New York City", "Bronx" , "Brooklyn" ,
                                                    "Manhattan" ,"Queens" , "Staten Island"),
                                     selected = "Bronx"),
                  
                  
                  selectInput("q12Year", h3("Year"), 
                              choices = as.list(c(as.character(seq(2016,2021)))), selected = "2019")
                  
                  # radioButtons("Group", h3("By"),
                  #              choices = list("Month","Weekday"),
                  #              selected = "Month")
                  
                  
                ),  
                
                mainPanel(
                  plotlyOutput("q12_barplot")
                )
                
              )
      ),
      
      
      
      ## q13q14_barplot
      tabItem(tabName = "q13q14",
              titlePanel(q13q14title1),
              
              
              sidebarLayout(
                
                sidebarPanel(
                  radioButtons("q13q14Borough", h3("Boroughs"),
                               choices = list("New York City", "Bronx" , "Brooklyn" ,
                                              "Manhattan" ,"Queens" , "Staten Island" ),
                               selected = "Bronx"),
                  selectInput("q13q14Year", h3("Year"), 
                              choices = as.list(c("All years",as.character(seq(2014,2016)))), selected = "2016")
                ),
                
                
                mainPanel(
                  plotlyOutput("q13q14_barplot")
                )
                
              )
      ),
      
      
      
      
      ## q17q21_barplot
      tabItem(tabName = "q17q21",
              titlePanel(q17q21title1),
              
              
              sidebarLayout(
                
                sidebarPanel(
                  radioButtons("q17q21Borough", h3("Boroughs"),
                               choices = list("New York City", "Bronx" , "Brooklyn" ,
                                              "Manhattan" ,"Queens" , "Staten Island" ),
                               selected = "Bronx"),
                  selectInput("q17q21Year", h3("Year"), 
                              choices = as.list(c("All years",as.character(seq(2000,2016)))), selected = "2016")
                ),
                
                
                mainPanel(
                  plotlyOutput("q17q21_barplot")
                )
                
              )
      ),
      
      ## q17q21v2_barplot
      tabItem(tabName = "q17q21v2",
              titlePanel(q17q21v2title1),
              
              
              sidebarLayout(
                
                sidebarPanel(
                  radioButtons("q17q21v2Borough", h3("Boroughs"),
                               choices = list("New York City", "Bronx" , "Brooklyn" ,
                                              "Manhattan" ,"Queens" , "Staten Island" ),
                               selected = "Bronx"),
                  selectInput("q17q21v2Year", h3("Year"), 
                              choices = as.list(c("All years",as.character(seq(2000,2016)))), selected = "2016")
                ),
                
                mainPanel(
                  plotlyOutput("q17q21v2_barplot")
                )
                
              )
      ),
      
      
      ## q20_barplot
      tabItem(tabName = "q20",
              titlePanel(q20title1),
              
              
              
              sidebarLayout(
                
                sidebarPanel(
                  radioButtons("q20Borough", h3("Boroughs"),
                               choices = list("New York City", "Bronx" , "Brooklyn" ,
                                              "Manhattan" ,"Queens" , "Staten Island" ),
                               selected = "Bronx"),
                  selectInput("q20Year", h3("Year"), 
                              choices = as.list(c("All years",as.character(seq(2000,2016)))), selected = "2016")
                ),
                
                
                mainPanel(
                  plotlyOutput("q20_barplot")
                )
                
              )
      ),
      
      
      
      ## b3_barplot
      tabItem(tabName = "b3",
              titlePanel(b3title1),
              
              
              sidebarLayout(
                
                sidebarPanel(
                  radioButtons("b3Borough", h3("Boroughs"),
                               choices = list("New York City", "Bronx" , "Brooklyn" ,
                                              "Manhattan" ,"Queens" , "Staten Island" ),
                               selected = "Bronx"),
                  selectInput("b3Year", h3("Year"), 
                              choices = as.list(c("All years",as.character(seq(2000,2016)))), selected = "2016")
                ),
                
                
                mainPanel(
                  plotlyOutput("b3_barplot")
                )
                
              )
      ),
      
      
      
      ## b5b6_barplot
      tabItem(tabName = "b5b6",
              titlePanel(b5b6title1),
              
              
              sidebarLayout(
                
                sidebarPanel(
                  radioButtons("b5b6Borough", h3("Boroughs"),
                               choices = list("New York City", "Bronx" , "Brooklyn" ,
                                              "Manhattan" ,"Queens" , "Staten Island" ),
                               selected = "Bronx"),
                  selectInput("b5b6Year", h3("Year"), 
                              choices = as.list(c("All years",as.character(seq(2014,2016)))), selected = "2016")
                ),
                
                
                mainPanel(
                  plotlyOutput("b5b6_barplot")
                )
                
              )
      ),
      
      # Third tab content
      tabItem(tabName = "dashboard2",
              h2("Summary Stat")
      ),
      # Fourth tab content
      tabItem(tabName = "dashboard3",
              h2("Summary Stat")
      )
    )
  )
)



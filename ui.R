## app.R ##
library(shinydashboard)
library(plotly)
#setting for dog bites
Objective <- "Breed"
title1 <- "Which dog breed bites the most?"
title2 <- paste("Dog ",Objective,"s within ",sep="")

#UI
ui <- dashboardPage(
  dashboardHeader(title = "Puppy Scientist"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("DogBites", tabName = "dashboard1", icon = icon("feather")),
      menuItem("DogLicense", tabName = "dashboard2", icon = icon("bone", lib = "font-awesome")),
      menuItem("DogRun", tabName = "dashboard3", icon = icon("paw"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "map",h2("Interactive Map here")),
      # Second tab content
      tabItem(tabName = "dashboard1",
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



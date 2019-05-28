## app.R ##
library(shinydashboard)

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
              h2("Demo Plot here"),
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
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



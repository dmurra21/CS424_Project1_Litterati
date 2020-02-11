library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(sp)
library(lubridate)
library(tidyverse)

ui <-  dashboardPage( skin = "black",
  dashboardHeader(title = "Litterati Challenge 65"),
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Top 10", tabName = "top10", icon = icon("trophy")),
      menuItem("About", tabName = "about", icon = icon("info"))
    )
  ),
  
  body <- dashboardBody(
    tabItems(
      tabItem(
        tabName = "home",
        fluidRow(
          box(headerPanel(title =  "Go Green Forest Park Clean-Up 2019" ), width = 12, height = 150, textOutput("aboutText"))
        ),
        fluidRow(
          # A static valueBox
          valueBox(1, "City", icon = icon("city"), color="olive"),
          valueBox(textOutput("npeople"), "Participants", icon = icon("users"), color = "olive"),
          valueBox(textOutput("nlitter"), "Pieces of Litter Picked Up", icon = icon("trash"), color="olive" )
        ),
        fluidRow(
          box(title="Forest Park, IL, USA",leafletOutput("map1",), width = 12, height = 500)
        )
      ),
      tabItem(
        tabName = "top10",
        h2("Top 10 Stats"),
        fluidRow(
          box(title="Top Ten Litter Tags", plotOutput("plot1",), width = 12),
        ),
        fluidRow(
          box(title="Top Ten Litter Pickers", plotOutput("plot2",), width = 8),
          box(title="", tableOutput("tab1"), width = 4)
        )
      ),
      tabItem(
        tabName = "about",
        h2("About"),
        h3("Dashboard created by Desiree Murray for CS 424-Data Visualization at UIC"),
        h3("Data from Litterati - https://www.litterati.org"),
        h3("Challenge 65 - https://www.evl.uic.edu/aej/424/litterati challenge-65.csv"),
        h3("Created using R, RStudio, Shiny")
      )
    )
  ) # end dashboardBody
) # end ui

server <- function(input, output) { 
  # read in dataframe into "litterati" variable
  litterati <- read.csv("litterati.csv", header = TRUE, stringsAsFactors = FALSE)
  
  # remove incomplete rows
  litterati <- litterati[complete.cases(litterati),]
  
  # make sure cols are of appropriate types
  litterati$challengeId <- as.numeric(litterati$challengeId)
  litterati$litterjoinId <- as.numeric(litterati$litterjoinId)
  litterati$litterId <- as.numeric(litterati$litterId)
  litterati$user_id <- as.numeric(litterati$user_id)
  litterati$lat <- as.numeric(litterati$lat)
  litterati$lon <- as.numeric(litterati$lon)
  litterati$username <- as.character(litterati$username)
  
  # get rid of out of range values
  litterati <- litterati[(litterati$lat < 41.88 & litterati$lat > 41.83),]
  litterati <- litterati[(litterati$lon > -87.9 & litterati$lon < -87.7),]
  litterati <- subset(litterati, username != "mwoe")
  
  # add "untagged" tag to tagless rows
  litterati$tags[litterati$tags == ""] <- "untagged"
  
  # replace litterati-generated usernames with userxxxx usernames
  litterati <- mutate(litterati, "username" = ifelse(grepl("litterati", litterati$username), str_replace(litterati$username, "litterati-", "user"), litterati$username))
  
  # convert litterTimestamp to date/time format and switch to Chicago time
  litterati$litterTimestamp <- ymd_hms(litterati$litterTimestamp)
  lubridate::is.POSIXct(litterati$litterTimestamp)
  litterati$litterTimestamp <- force_tz(litterati$litterTimestamp, tzone = "America/Chicago")
  
  # split tags up and create dataframe with tag frequencies in descending order
  tags <- litterati$tags
  tags <- strsplit(tags, "\\,")
  tags <- sort(table(unlist(tags)))
  tags <- as.data.frame(tags)
  tags <- tags[order(tags$Freq, decreasing = T),]
  
  
  #top 10 pickers list/plot
  top10pickers <- table(litterati$username)
  top10pickers <- sort(top10pickers, decreasing = T)
  top10pickers <- head(top10pickers, n=10L)
  top10pickers <- as.data.frame(top10pickers)
  
  t10 <- table(litterati$username)
  t10 <- sort(t10, decreasing = T)
  t10 <- head(t10, n=10L)
  
  #create SpatialPointsDataFrame using col nums where lat and lon can be found (5, 6)
  litterati.SP <- SpatialPointsDataFrame(litterati[,c(5,6)], litterati[,-c(5,6)])
  
  output$aboutText <- renderText(
    print("Forest Park's Go Green group hosted a neighborhood clean-up on Earth Day 2019 to raise
          awareness of environmental issues. With the help of the app Litterati, the group was able to 
          document where trash exists in their town by taking pictures of it. Over 12,000 pieces of litter 
          were collected during the clean-up.")
  )
  
  output$plot1 <- renderPlot({
    p1 <- ggplot(head(tags), aes(x=Var1, y=Freq, fill=Var1)) + geom_bar(stat = "identity", width = 0.5) + theme_light() 
    p1 + labs(x="Litter Tag", y="Tag Frequency") + theme(legend.position = "none")
  })
  
  output$plot2 <- renderPlot({
    p2 <- ggplot(top10pickers, aes(Var1, y=Freq, fill=Var1)) + geom_bar(stat = "identity", width = 0.5) + theme_light() 
    p2 + labs(x="Litterati Username", y="Pieces of Litter Collected") + theme(legend.position = "none")
  })
  
  output$tab1 <- renderTable(t10)
  
    
  output$map1 <- renderLeaflet({
    m <- leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = litterati, lng = ~lon, lat = ~lat, popup = ~tags, clusterOptions = markerClusterOptions())
  })
  
  output$nlitter <- renderText(nrow(litterati))
  output$npeople <- renderText(length(unique(litterati$username)))
}

shinyApp(ui, server)
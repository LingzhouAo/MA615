library(stringr)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

time_seq <- seq(1988,2017)[-26]
for (i in 1:length(time_seq)){
  filename<-paste("weather data/",time_seq[i],".txt",sep = "")
  tex<-read.table(filename,header = TRUE, fill = TRUE)
  if(length(tex[1,])==16|length(tex[1,])==17){
    sub_tex<-tex[,c(1,2,3,4,13,14)]
  }
  if(length(tex[1,])==18){
    sub_tex<-tex[,c(1,2,3,4,14,15)]
  }
  names(sub_tex)<-c('YY','MM','DD','hh','ATMP','WTMP')
  if (i == 1){
    data <- sub_tex
  }
  else{
    data<-rbind(data,sub_tex)
  }
  
}
temperature <- data %>%
  filter(hh==12)

temperature[temperature$YY < 99,]$YY <-temperature[temperature$YY < 99,]$YY + 1900
temperature <- temperature %>%
  filter(ATMP<99) %>%
  filter(WTMP<99)
temperature$ATMP <- as.numeric(temperature$ATMP)
temperature$WTMP <- as.numeric(temperature$WTMP)

temperature$time<-paste(temperature$YY,"-",temperature$MM,"-",temperature$DD,sep = "")
temperature$time<-as.Date(temperature$time)

year_1988<-temperature[temperature$YY==1988,]
year_2017<-temperature[temperature$YY==2017,]
A_1988<-sample(year_1988$ATMP,size=200)
A_2017<-sample(year_2017$ATMP,size=200)
W_1988<-sample(year_1988$WTMP,size=200)
W_2017<-sample(year_2017$WTMP,size=200)
test_A<-t.test(A_1988,A_2017)
test_W<-t.test(W_1988,W_2017)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Assignment5-Part1"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Time Series", tabName = "time_series"),
      menuItem("Air and Sea Temperature Correlation", tabName = "correlation"),
      menuItem("T test", tabName = "t_test")
    )
  ),
  
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "time_series",
              fluidRow(
                box(selectInput("time_series_mode",
                                "Mode:",
                                choices = list("Air Temperature", "Sea Temperature",
                                               "Air and Sea Temperatures")), 
                    plotOutput("plot1"), width = 12)
              )
              ),
      
      tabItem(tabName = "correlation",
              fluidRow(
                box(selectInput("cor_mode",
                                "Mode:",
                                choices = list("Scatter Plot", "Smooth Line")), 
                    plotOutput("plot2"), width = 12)
              )
              
              ),
      tabItem(tabName = "t_test",
              fluidRow(
                box(selectInput("t_test_mode",
                                "Mode:",
                                choices = list("A_ttest","W_ttest")),
                    textOutput("ttestOutput"), width = 12)
              ))
      
              )
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    
    if (input$time_series_mode == "Air Temperature") {
      
      graph <- temperature %>% ggplot(aes(time, ATMP)) +
        geom_line(na.rm = TRUE, col = "blue") +
        labs(title = "Time Series of Air Temperature",
             subtitle = "Data obtained from the National Data Buoy Center",
             y = "Temperature",
             x = "Year")
      print(graph)
    }
    
    if (input$time_series_mode == "Sea Temperature") {
      
      graph <- temperature %>% ggplot(aes(time, WTMP)) +
        geom_line(na.rm = TRUE, col = "blue") +
        labs(title = "Time Series of Sea Temperature",
             subtitle = "Data obtained from the National Data Buoy Center",
             y = "Temperature",
             x = "Year")
      print(graph)
    }     
    
    if (input$time_series_mode == "Air and Sea Temperatures") {
      
      graph <- ggplot(temperature, aes(time)) + 
        geom_line(aes(y = ATMP, col = "ATMP")) + 
        geom_line(aes(y = WTMP, col = "WTMP")) +
        scale_colour_manual(values=c("red", "blue")) +
        labs(x = "Year", y = "Temperature",
             title = "Time Series of Air & Sea Temperature",
             subtitle = "Data obtained from the National Data Buoy Center")
      print(graph)
    }     
    
  })
  
  
  output$plot2 <- renderPlot({
    if (input$cor_mode == "Scatter Plot") {
      
      graph <- ggplot(temperature) + 
        geom_point(mapping = aes(x = ATMP, y = WTMP)) +
        labs(x = "Air Temperature", 
             y = "Sea Temperature",
             title = "Scatter Plot of Correlation between ATMP and WTMP")
      print(graph)
    }
    
    if (input$cor_mode == "Smooth Line") {
      
      graph <- ggplot(temperature) + 
        geom_smooth(mapping = aes(x = ATMP, y = WTMP)) +
        labs(x = "Air Temperature", 
             y = "Sea Temperature",
             title = "Smooth Line of Correlation between ATMP and WTMP")
      print(graph)
    }
    
  })
  output$ttestOutput <- renderPrint({
    if (input$t_test_mode == "A_ttest"){
      return(test_A)
      
    }
    if (input$t_test_mode == "W_ttest"){
      return(test_W)
    }
    
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
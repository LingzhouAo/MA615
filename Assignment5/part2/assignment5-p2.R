library(stringr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)

veg.1 <- read_xlsx("veg1.xlsx")
a <- apply(veg.1, 2, n_distinct)
c <- names(a[a>1])

veg2 <- select(veg.1, c)
apply(veg2, 2, n_distinct)
veg.3 <- dplyr::rename(veg2, 
                       Geo = `Geo Level`, 
                       State = `State ANSI`,
                       Data = `Data Item`,
                       Category = `Domain Category`)

cnames.3 <- colnames(veg.3)
cnames.3

unique(veg.3[,"Commodity"])

unique(veg.3[,"Data"]) %>% print(n=60)

unique(veg.3[,"Domain"])

unique(veg.3[,"Category"])

unique(veg.3[,"Value"])


veg.3 <- veg.3 %>%
  separate(Category, into = c("Label", "Chemical"), sep=",") %>%
  separate(Data, into=c("Vegetable","Class Desc"),sep=" - ") %>%
  separate(`Class Desc`, into=c("Class","Production Practice","Unit"),sep=",") %>%
  separate(`Production Practice`,into=c("Production Practice","Utilization Practice","Statistic Category"),sep=" / ") %>%
  separate(Domain,into=c("Domain","Type"),sep=", ") 

veg.3 <- veg.3 %>%
  separate(Chemical, into=c("DuplicateType","Active Ingredient or Action Taken"),sep=": ") %>%
  separate(`Active Ingredient or Action Taken`, into=c("Temp1","Active ingredient or Action Taken","Temp2"),sep=c(1,-2)) %>%
  separate(`Active ingredient or Action Taken`, into=c("Active ingredient or Action Taken","EPA Pesticide Chemical Code"),sep="=") %>%
  separate(Geo,into=c("Area","Temp3"),sep=" : ") %>%
  select(-Vegetable,-Label,-DuplicateType,-Temp1,-Temp2,-Temp3)


##Restricted use chemical

veg4 <- veg.3 %>%
  filter(Domain=="RESTRICTED USE CHEMICAL") %>%
  select(Commodity, Domain:`EPA Pesticide Chemical Code`) %>%
  unique()


##toxicity measurement
toxicity <- tibble(
  `Toxicity Measurements(mg/kg)` =
    c(20, 5620, 20, 11,
      869, 54, 5000, 82,
      869, 3129, 458, 450,
      14, 12, 50, 50,
      430, 1563, 86,380,
      54, 5000, 3129, 458,
      450, 144, 12, 50,
      50, 430, 1563, 86,
      300, 60, 1.9, 72.1,
      82, 869, 150, 300,
      4640, 56, 16, 73,
      1.9, 56, 73, 121
    )
)
veg4 <- veg4 %>%
  bind_cols(toxicity)

broccoli<-filter(veg4,Commodity=="BROCCOLI")
cauliflower<-filter(veg4,Commodity=="CAULIFLOWER")
library(shiny)
library(shinydashboard)
#Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Assignmet5-Part2"),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Chemical", icon = icon("th"), tabName = "chemical",
             menuSubItem('Brocolli',tabName = 'brocolli',icon = icon('bar-chart')),
             menuSubItem('Cauliflower',tabName = 'cauliflower', icon = icon('bar-chart'))
             )
  )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "brocolli",
              fluidRow(
                h2('Broccoli treatment toxicity'),
                box(sliderInput("range3","Broccoli range of displaying treaments",
                                min=1,
                                max=28,
                                value = c(1,28))),
                box(plotOutput("brocolliplot")
                )
              )),
      tabItem(tabName = 'cauliflower',
              fluidRow(
                h2('cauliflower treatment toxicity'),
                box(sliderInput("range4","cauliflower range of displaying treaments",
                                min=1,
                                max=20,
                                value = c(1,20))),
                box(plotOutput("cauplot")
                )
              ))
    )
))




# Define server logic required to draw a histogram
server <- function(input, output) {
  output$brocolliplot <- renderPlot({
    
    new_broccoli <- broccoli[c(input$range3[1]:input$range3[2]),]
    ggplot(data=new_broccoli,mapping=aes(x=`Active ingredient or Action Taken`,
                                 y=`Toxicity Measurements(mg/kg)`))+labs(title = "Broccoli")+
      geom_bar(stat = "identity")+coord_flip()
  })
  
  output$cauplot <- renderPlot({
    new_cauliflower <- cauliflower[c(input$range4[1]:input$range4[2]),]
    ggplot(data=new_cauliflower,mapping=aes(x=`Active ingredient or Action Taken`,
                                 y=`Toxicity Measurements(mg/kg)`))+labs(title = "Cauliflower")+
      geom_bar(stat = "identity")+coord_flip()
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

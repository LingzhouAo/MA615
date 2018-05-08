library(ggmap)
suppressMessages(library("tidyverse"))
library(magick)
library(ggplot2)

final <- read_csv("houses.csv")

df_final <- data_frame(price = final$price,
                      lat = final$lat,
                      lng = final$lng
)
yelp <- read_csv("YelpData.csv")
df_yelp <- data_frame(price = yelp$price,
                     lat = yelp$latitude,
                     lng = yelp$longitude
)

final.scale <- scale(df_final)

final.scale[,2] <- final.scale[,2] *5
final.scale[,3] <- final.scale[,3] *5

#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- final.scale
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# Choose 10 as the number of cluters

k.m <- kmeans(data, 10, nstart = 50, iter.max = 15)

df_cluster <- data_frame(cluster = k.m$cluster)
new_final <- cbind(df_final, df_cluster)


price_label <- c(1:10)
lat_label <- c(1:10)
lng_label <- c(1:10)
for (i in c(1:10)){
  new_final$label[new_final$cluster == i] <- mean(new_final$price[new_final$cluster == i])
  price_label[i] <- mean(new_final$price[new_final$cluster == i])
  lat_label[i] <- mean(new_final$lat[new_final$cluster == i])
  lng_label[i] <- mean(new_final$lng[new_final$cluster == i])
}
price_label
lat_label
lng_label

new_final$new_label <- factor(new_final$label)

register_google(key = 'AIzaSyAmOQseQL27vmpzc4Lpddmocrzv1jDkXwg')
map <- get_googlemap(center =c(-71.0589,42.3601), zoom = 11, maptype = "roadmap")
ggmap(map)

ggplot(data = new_final, aes(x = lng, y = lat , color = new_label), size = 5, shape = 21)+geom_point()

ggmap(map)+
  geom_point(data = new_final,aes(x = lng, y = lat, color = new_label))

#deal restaurant
row.has.na <- apply(df_yelp, 1, function(x){any(is.na(x))})
sum(row.has.na)

df_yelp <- df_yelp[!row.has.na,]

label_lat <- k.m$centers[,2]
label_lng <- k.m$centers[,3]

yelp_scale <- scale(df_yelp)
yelp_lat <- yelp_scale[,2]*5
yelp_lng <- yelp_scale[,3]*5

r_label <- c(1:943)

for (i in c(1:943)){
  r_l_x <- yelp_lat[i]
  r_l_y <- yelp_lng[i]
  l <- 1
  d <- (r_l_x - label_lat[1])**2 + (r_l_y - label_lng[1])**2
  for (j in c(2:10)){
    new_d <- (r_l_x - label_lat[j])**2 + (r_l_y - label_lng[j])**2
    if (d > new_d){
      l <- j
      d <- new_d
    }
  }
  r_label[i] <- price_label[l]
}

new_df_yelp <- cbind(df_yelp,r_label)
names(new_df_yelp)[4] <- paste("house_price")


ggplot(data = new_df_yelp, aes(x = house_price, y = price, color = price))+geom_point()

library(plyr)
fre_1 <- count(new_df_yelp,"house_price")

ggplot(data = fre_1,aes(x = house_price, y = freq))+geom_line()

library(shiny)
library(shinydashboard)
# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Relationship between houses and restaurants"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("K-Means", tabName = "kmeans"),
      menuItem("Relationship", tabName = "relationship"),
      menuItem("Analysis", tabName = "analysis")
    )
  ),
  
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "kmeans",
              fluidRow(
                box(selectInput("kmeans_results",
                                "Mode:",
                                choices = list("cluster", "On Map")), 
                    plotOutput("plot1"), width = 12)
              )
      ),
      
      tabItem(tabName = "relationship",
              fluidRow(
                box(selectInput("restaurants_graphs",
                                "Mode:",
                                choices = list("point", "Line")), 
                    plotOutput("plot2"), width = 12)
              )
              
      ),
      tabItem(tabName = "analysis",
              fluidRow(
                box(
                    textOutput("summary"), width = 12)
              ))
      
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    
    if (input$kmeans_results == "cluster") {
      
      graph <- ggplot(data = new_final, aes(x = lng, y = lat , color = new_label), size = 5, shape = 21)+geom_point()

      print(graph)
    }
    
    if (input$kmeans_results == "On Map") {
      
      graph <- ggmap(map)+
        geom_point(data = new_final,aes(x = lng, y = lat, color = new_label))
      print(graph)
    }     
    
  })
  
  
  output$plot2 <- renderPlot({
    if (input$restaurants_graphs == "point") {
      
      graph <- ggplot(data = new_df_yelp, aes(x = house_price, y = price, color = price))+geom_point()
      print(graph)
    }
    
    if (input$restaurants_graphs == "Line") {
      
      graph <- ggplot(data = fre_1,aes(x = house_price, y = freq))+geom_line()
      print(graph)
    }
    
  })
  output$summary <- renderPrint({
    a <- "First, by observing the clustering result on google map, I think the result is quite
    realiable based on the information we know about the house price of Boston and I put each
    restaurant into the nearest cluster. The point figure shows for each cluster which price level's
    restaurant it contains and the line figure shows for each cluster how many restaurants it contains.
    And based on the results, I figure out for those higest price level restaurants, they will choose
    the high house price area but not the highest beacause they want to attract as many target customers
    as they can but also they don't want to pay too much renting fee. For those low price level restaruants,
    they are everywhere, so I guess some of them are fast food restaurants. Everyone needs fast food.
    Therefore, based on the map that shows the distribution of house price, you can choose the best place
    for dinning to live."
    return(a)
    
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
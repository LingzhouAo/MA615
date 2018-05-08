#library(ggmap)
suppressMessages(library("tidyverse"))
library(magick)
library(RCurl)
library(geoChina)

zipcode <-c('02118','02119','02121','02124','02125','02126','02127','02128',
            '02129','02130','02131','02132','02134','02135','02136','02210','02215','02445')
houses <- read_csv('02115.csv')
for (zip in zipcode){
  temp <- read_csv(paste(zip,'.csv',sep=''))
  houses <- rbind(houses,temp)
}

df_houses <- data_frame(address=houses$address, 
                        price = substr(houses$price,2,nchar(houses$price)-3),
                        rooms = substr(houses$others,1,1)
)
df_houses$rooms[df_houses$rooms %in% "S"] <- "1"

df_houses$temp <- as.numeric(df_houses$price)

df_houses <- df_houses %>%
  separate(price, c("price_1", "price_2"), sep=",")

df_houses$price_1[df_houses$temp != "NA"] <- '0'

for (i in c(847, 948, 1222, 1605, 2156, 2478, 2708, 2926, 3006, 3079, 3092, 3109, 3259, 3291)){
  df_houses$price_2[i] <- df_houses$temp[i]
}

df_houses$price <- paste(df_houses$price_1,df_houses$price_2,sep="")

df_houses <- df_houses %>%
  select(-temp,-price_1,-price_2)

df_houses$price <- as.numeric(df_houses$price)

df_houses$rooms <- as.numeric(df_houses$rooms)

row.has.na <- apply(df_houses, 1, function(x){any(is.na(x))})
sum(row.has.na)

new_df_houses <- df_houses[!row.has.na,]

new_df_houses$average <- new_df_houses$price / new_df_houses$rooms


#getGeoData <- function(location, api_key){
#  location <- gsub(' ','+',location)
#  geo_data <- getURL(paste("https://maps.googleapis.com/maps/api/geocode/json?address=",location,sprintf("&key=%s",api_key), sep=""))
#  geo_data <- fromJSON(geo_data)
#  return(geo_data$results[[1]]$geometry$location)
#}

location <- apply(new_df_houses[,1], 2, function(x){geocode(x,api = 'google',key = 'AIzaSyAmOQseQL27vmpzc4Lpddmocrzv1jDkXwg')})
lat_1 <- location[['address']]$lat
lng_1 <- location[['address']]$lng
new_df_houses <- cbind(new_df_houses,lat_1)
new_df_houses <- cbind(new_df_houses,lng_1)

names(new_df_houses)[5]<-paste("lat")
names(new_df_houses)[6]<-paste("lng")

row.has.na_1 <- apply(new_df_houses, 1, function(x){any(is.na(x))})
sum(row.has.na_1)

final_house <- new_df_houses[!row.has.na_1,]
write.csv(final_house, file = "houses.csv")

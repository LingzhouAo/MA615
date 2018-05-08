
#Get Yelp Data
library(tidyverse)
library(httr)

client_id <- "MUTrozyn5FCHH3K33e4cMQ"
client_api_key <- "ivLPY-1iBZaoPlPOMrb5oS6_dAIk3B7kflPavzHVdWyo-S9M30HSlvICt-pXnP2lHrot04BlWY947tiXwZQ5FoVP6CJq-4wbIH7J8X_c9mghjGbg8kS4OwSpIkDvWnYx"

yelp_business_search <- function(term = NULL, location = NULL, 
                                 categories = NULL, radius = NULL, 
                                 limit = 50, client_id = NULL, 
                                 client_api_key = NULL) {
  
  yelp <- "https://api.yelp.com"
  url <- modify_url(yelp, path = c("v3", "businesses", "search"),
                    query = list(term = term, location = location, limit = limit, 
                                 radius = radius, categories = categories))
  res <- GET(url, add_headers('Authorization' = paste("bearer", client_api_key)))
  results <- content(res)
  
  yelp_httr_parse <- function(x) {
    
    parse_list <- list(name = x$name, 
                       price = nchar(x$price), 
                       latitude = x$coordinates$latitude, 
                       longitude = x$coordinates$longitude
                       )
    
    parse_list <- lapply(parse_list, FUN = function(x) ifelse(is.null(x), "", x))
    
    df <- data_frame(name=parse_list$name, 
                     price = parse_list$price, 
                     latitude=parse_list$latitude, 
                     longitude = parse_list$longitude
                     )
    df
  }
  results_list <- lapply(results$businesses, FUN = yelp_httr_parse)
  payload <- do.call("rbind", results_list)
  payload <- payload %>%
    filter(grepl(term, name))
  
  payload
}

results <- yelp_business_search(term = "", 
                                location = "02128",
                                radius = 16000, 
                                client_id = client_id, 
                                client_api_key = client_api_key)

for (district in c('02129','02210','02118','02127','02125','02119','02121','02124','02126',
                   '02130','02131','02132','02136','02134','02135','02115','02215','02445')){
  temp <-yelp_business_search(term = "", 
                              location = district,
                              radius = 16000, 
                              client_id = client_id, 
                              client_api_key = client_api_key)
  results <- rbind(results,temp)
}

write.csv(results, file = "YelpData.csv")


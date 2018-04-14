
library(tidyverse)

bb <- read_csv("billboard.csv")

bb.1 <- bb %>% gather(key="week", value = "rank", -year, -artist.inverted, -track, -time, -genre, -date.entered, -date.peaked)

bb.2 <- bb.1 %>% select(year, artist=artist.inverted, time, track, date = date.entered, week, rank )


bb.3 <- bb.2 %>% arrange(track)

bb.4 <- bb.3 %>% filter(!is.na(rank))

bb.5 <- bb.4 %>% separate(week, into=c("A", "B", "C"), sep=c(1, -8), convert=TRUE)

bb.6 <- bb.5 %>% select(-A, -C)

bb.7 <- bb.6 %>%   dplyr::rename(week = B)


bb.8 <- bb.7 %>% arrange(artist, track)

bb.9 <- bb.8 %>% mutate(date = date + (week-1)*7 )

bb.10 <- bb.9 %>% mutate(rank = as.integer(rank))



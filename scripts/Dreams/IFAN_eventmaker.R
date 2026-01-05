rm(list=ls())
setwd("/Users/lucanaudszus/sciebo/Imaginary Friends at Night/Traumstudie_2023")
### load packages and sources
library(readxl)
library(tidyverse)
### set working directory
setwd("data/2_preprocessed-data/social-content-analysis/4_Templates")
### initialize variables
eventdata = data.frame(matrix(ncol = 3, nrow = 0))
epidata = data.frame(matrix(ncol = 3, nrow = 0))
sitdata = data.frame(matrix(ncol = 3, nrow = 0))

for (filename in list.files()){
  print(sprintf("Processing %s", filename))
  data = read_xlsx(filename)
  data$social <- as.numeric(data$social)
  data <- data %>%
    fill(date)
  if (any(is.na(data$date))) {
    break
  }
  data <- data[!(is.na(data$situation)),]
  data$code <- data$code[1]
  id = substring(filename, 1, 6)
  if (id %in% eventdata$id) {
    break
  }
  if (is.na(data$code[1])) {
    data$code <- id
  }
  if (id != data$code[1]) {
    break
  }
  data$eventcode <- 0
  data$epicode <- 0
  data$situationCode <- 0
  data$date <- as.Date(data$date)
  for (irow in 1:length(data$code)){
    data$eventcode = paste(data$code, data$date, data$episode, data$event, sep = "_")
    data$epicode = paste(data$code, data$date, data$episode, sep = "_")
    data$situationCode = paste(data$code, data$date, data$episode, data$event, data$situation, sep = "_")
  }
  for (inevent in unique(data$eventcode)){
    eventdata = rbind(eventdata, cbind(id, inevent, paste(data$content[data$eventcode == inevent], collapse = " ")))
  }
  for (inepi in unique(data$epicode)){
    epidata = rbind(epidata, cbind(id, inepi, paste(data$content[data$epicode == inepi], collapse = " ")))
  }
  for (insit in unique(data$situationCode)) {
    sitdata = rbind(sitdata, cbind(id, insit, paste(data$content[data$situationCode == insit], collapse = " ")))
  }
}
colnames(eventdata) <- c("id", "event", "content")
colnames(epidata) <- c("id", "episode", "content")
colnames(sitdata) <- c("id", "situation", "content")

df.neve <- eventdata %>%
  group_by(id) %>%
  summarise(n_eve = n())

#setwd("C:/Users/lanau/sciebo/Imaginary Friends at Night/1 Vorstudie")
write.csv(eventdata, "/Users/lucanaudszus/sciebo/Imaginary Friends at Night/Traumstudie_2023/data/2_preprocessed-data/events_2024-11-18.csv", row.names = F)
write.csv(df.neve, "/Users/lucanaudszus/sciebo/Imaginary Friends at Night/Traumstudie_2023/data/2_preprocessed-data/n-events_2024-11-18.csv", row.names = F)
write.csv(epidata, "/Users/lucanaudszus/sciebo/Imaginary Friends at Night/Traumstudie_2023/data/2_preprocessed-data/episodes_2024-11-18.csv", row.names = F)
write.csv(sitdata, "/Users/lucanaudszus/sciebo/Imaginary Friends at Night/Traumstudie_2023/data/2_preprocessed-data/situations_2024-11-18.csv", row.names = F)


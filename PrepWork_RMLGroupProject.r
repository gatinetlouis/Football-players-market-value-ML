getwd()
setwd("./Desktop/EDHEC DAAI/Introduction to Machine Learning")

library(Amelia)
library(readr)
library(dplyr)
library(tidyverse)

data <- read_csv("fifadata.csv")

### Removing the Photo, Flag, Club Logo, Loaned From columns // We removed scores of each players for each position on the field

data <- data %>% select(-Photo, -Flag, -`Club Logo`, -`Loaned From`, -(LS:RB), -Joined, -X1, -Special, -`Real Face`, -`Body Type`, -`Jersey Number`)

### Converting arrival date in R format

#data[which(is.na(data[,"Joined"])),"Joined"] <- "Loaned" # C'est tout casse
#data$Joined <- strptime(data$Joined, format="%b %d, %Y")

### Replacing the NA's in the Joined column by the string "Loaned"

### Converting financial values as integers (Wage, Release Clause and Value)

data$Value <- as.numeric(substring(data$Value,2,nchar(data$Value)-1))
data$Wage <- as.numeric(substring(data$Wage,2,nchar(data$Wage)-1))
data$`Release Clause` <- as.numeric(substring(data$`Release Clause`,2,nchar(data$`Release Clause`)-1))

data$Weight <- as.numeric(substring(data$Weight, 1, 3)) * 0.45359237

data <- data %>% separate(Height, c('feet', 'inches'), "'", convert = TRUE) %>%
  mutate(Height = (12*feet + inches)*2.54) %>% select(-inches, -feet)

data <- data %>% separate(`Work Rate`, c('Offensive Work Rate', 'Defensive Work Rate'), "/ ")

#years <- as.character(2018:2026)
#for (k in 1:length(years)) {
#  X <- data %>% filter(nchar(`Contract Valid Until`) != 4) %>% select(`Contract Valid Until`, contains(years[k]))
#  for (i in 1:length(X)) X[i] <- years[k]
#}

text <- NULL
first <- NULL
last <- NULL

for (i in 1:nrow(data)) {
  text[i]=data$`Contract Valid Until`[i] 
  first[i]=nchar(text[i])-4
  last[i]=nchar(text[i])
  data$`Contract Valid Until`[i]=substring(text[i],first[i],last[i])
}

data$`Remaining Contract Duration` <- as.numeric(data$`Contract Valid Until`) - 2018

data <- data %>% select(-`Contract Valid Until`)

for (i in 1:nrow(data)) {
  if (data$Position[i] %in% c("LW", "LF", "RW", "RF", 'CF', 'LS', 'RS', 'ST')) data$Field_Position[i] <- "Attack"
  if (data$Position[i] %in% c('CDM', 'LDM', 'RDM', 'CM', 'LCM', 'RCM', 'CAM', 'LAM', 'RAM', 'LM', 'RM')) data$Field_Position[i] <- "Mitfielder"
  if (data$Position[i] %in% c('CB', 'LCB', 'RCB', 'LB', 'RB', 'LWB', 'RWB')) data$Field_Position[i] <- "Defenser"
  if (data$Position[i] %in% c("GK")) data$Field_Position[i] <- "Goalkeeper"
}

data <- data %>% select(-Position)


df <- data[which(apply(is.na(data),1,any)),]

### Il faut enlever les joueurs en pret car ils n'ont pas de clause liberatoire et fausseraient la regression

data <- na.omit(data)

str(data)

datareg <- data %>% select(-ID, -Name, -Nationality, -Club)

my.model <- lm(Wage~., data=datareg)
summary(my.model)


# Outliers?




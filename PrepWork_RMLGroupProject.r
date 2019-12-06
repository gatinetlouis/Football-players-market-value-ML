getwd()
setwd("/Users/antoinepiguet/Desktop/EDHEC DAAI/Introduction to Machine Learning")

library(Amelia)
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)

data <- read_csv("fifadata.csv")

data_test <- read_csv("fifadata.csv")



### Removing the Photo, Flag, Club Logo, Loaned From columns // We removed scores of each players for each position on the field

data <- data %>% select(-Photo, -Flag, -`Club Logo`, -(LS:RB), -Joined, -`Loaned From`, -X1, -Special, -`Real Face`, -`Body Type`, -`Jersey Number`)

### Body Type removed because of high correlation with Height and Weight




#### Converting arrival date in R format

#data[which(is.na(data[,"Joined"])),"Joined"] <- "Loaned" # C'est tout casse
#data$Joined <- strptime(data$Joined, format="%b %d, %Y")

#### Replacing the NA's in the Joined column by the string "Loaned"





### Converting financial values as integers (Wage, Release Clause and Value)

data$Value <- ifelse(substring(data$Value, nchar(data$Value)) == "M",
                     as.numeric(substring(data$Value,2,nchar(data$Value)-1)),
                     as.numeric(substring(data$Value,2,nchar(data$Value)-1)) / 1000)
data$Wage <- as.numeric(substring(data$Wage,2,nchar(data$Wage)-1))
data$`Release Clause` <- as.numeric(substring(data$`Release Clause`,2,nchar(data$`Release Clause`)-1))

### Converting measures of height and weight into international metric system

data$Weight <- as.numeric(substring(data$Weight, 1, 3)) * 0.45359237
data <- data %>% separate(Height, c('feet', 'inches'), "'", convert = TRUE) %>%
  mutate(Height = (12*feet + inches)*2.54) %>% select(-inches, -feet)

### Renaming all modified variables
data <- data %>% rename(`Value (in M€)` = Value,
                        `Wage (in K€)` = Wage,
                        `Release Clause (in M€)` = `Release Clause`,
                        `Weight (in kg)` = Weight,
                        `Height (in cm)` = Height)

### Splitting the Work Rate column into its 2 components (Offensive / Defensive)
data <- data %>% separate(`Work Rate`, c('Offensive Work Rate', 'Defensive Work Rate'), "/ ")


### Creating a column that captures the remaining duration of the player's contract

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



df <- data[which(apply(is.na(data),1,any)),]  # All remaining NA's are players who have been loaned to another team (1264 players) 
                                              # nad 300 other players for whom FIFA did not gather the information or was denied the access
data <- na.omit(data)



ggplot(data) + aes(x=`Wage (in K€)`) + geom_histogram(bins=30) + scale_x_log10()


datareg <- data %>% select(-ID, -Name, -Nationality, -Club) # Removing irrelevant categorical variables for the regression
my.model <- lm(`Value (in M€)`~., data=datareg)
summary(my.model)

cor(data$`Value (in M€)`, data$`Wage (in K€)`)
# Removing outliers regarding the transfer value from the dataset allows to decrease the variance

### Plot of the Value vs Remaining Contract Years

ggplot(data3) +
  aes(x=`Wage (in K€)`, y=`Value (in M€)`, colour=Field_Position) +
  geom_point() +
  geom_smooth(method='lm') +
  scale_x_continuous(limits=c(0,300)) +
  theme_bw()

# As expected, the palyers' market value is positively correlated to the players' wages. 
# Looking at the different field positions we see that, for a given market value, Defensers get a better wage than Attackers. 
# This reflects the law of offer and supply: there are more Attackers valued at 30 M€ than Defensers so the latter get better paid.








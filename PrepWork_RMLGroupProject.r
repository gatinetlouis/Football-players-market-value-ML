getwd()
setwd("/Users/antoinepiguet/Desktop/EDHEC DAAI/Introduction to Machine Learning")

library(Amelia)
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(randomForest)
library(shiny)

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
data$`Offensive Work Rate` <- factor(data$`Offensive Work Rate`, levels=c("Low", "Medium", 'High'))
data$`Defensive Work Rate` <- factor(data$`Defensive Work Rate`, levels=c("Low", "Medium", 'High'))
data$Field_Position <- factor(data$Field_Position, levels=c("Goalkeeper", "Defenser", 'Mitfielder', "Attack"))
data$`Preferred Foot` <- factor(data$`Preferred Foot`, levels=c("Left", "Right"))
data$`International Reputation` <- factor(data$`International Reputation`, levels=1:5)
data$`Weak Foot` <- factor(data$`International Reputation`, levels=1:5)
data$`Skill Moves` <- factor(data$`International Reputation`, levels=1:5)
data$Club <- as.factor(data$Club)

summary(data$`International Reputation`)


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

ggplot(data) +
  aes(x=`Wage (in K€)`, y=`Value (in M€)`, colour=Field_Position) +
  geom_point() +
  geom_smooth(method='lm') +
  scale_x_continuous(limits=c(0,300)) +
  theme_bw()
  

# As expected, the palyers' market value is positively correlated to the players' wages. 
# Looking at the different field positions we see that, for a given market value, Defensers get a better wage than Attackers. 
# This reflects the law of offer and supply: there are more Attackers valued at 30 M€ than Defensers so the latter get better paid.

ggplot(data) +
  aes(x=`Offensive Work Rate`, y=`Value (in M€)`, fill=`Offensive Work Rate`) +
  geom_boxplot() +
  scale_y_log10()

ggplot(data) +
  aes(group=Overall, y=`Value (in M€)`, fill=Overall) +
  geom_boxplot(colour="Black", size=0.3) +
  scale_fill_gradient(low="White", high="Blue") +
  scale_x_continuous(limits=c(-0.2,0.4)) +
  facet_grid(Field_Position~.)

#Adding teams and leagues to database:

topclub<-data %>% group_by(Club) %>% summarize(count=n(), Val=sum(`Value (in M€)`),) %>% arrange(desc(Val)) #TopClub by nb of player and Value in M€

teams<-read.csv("teams.csv")
league<-read.csv("leagues.csv")
league$LeagueCode<-League$Code
teams2<-merge(teams, league, by="LeagueCode", all= F)
teams2$Club<-teams2$Name.x
test<-merge(data, teams2, by="Club", all= T)

######

#Regression models : predict Value with inputs

#1 Linear regression (full)

datareg <- data %>% select(-ID, -Name, -Nationality, -Club)

#We do a 10-folds cross validation
set.seed(12345)

n=nrow(data)
MSE=rep(NA,20)
MAPE=rep(NA,20)
for (i in 1:20){
  train_index=sample(1:n,round(0.7*n))
  train=datareg[train_index,]
  test=datareg[-train_index,]
  linear_model <- lm(Value (in M€)~., data=train)
  pred_lm=predict(linear_model,newdata=test)
  MSE[i]=mean((test$`Value (in M€)`-pred_lm)^2)
  MAPE[i]=mean(abs((test$`Value (in M€)`-pred_lm)/(test$`Value (in M€)`)))
  
}

MSE_lm=mean(MSE)
MAPE_lm=mean(MAPE)

print(MSE_lm);print(MAPE_lm)

#2. Trees and Random forests

n<-nrow(data)
set.seed(12345)
train_index=sample(1:n,round(0.7*n))
train=datareg[train_index,]
test=datareg[-train_index,]

tree<-rpart("Value (in M€)"~., data=train, cp=0.000000001)
printcp(tree) 
plotcp(tree)
alpha.opt<-tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
besttree<-prune(tree, cp=alpha.opt)
pred<-predict(besttree, newdata=test)
mean((data$"Value (in M€)"-pred)^2) #quadratic err

visTree(besttree)


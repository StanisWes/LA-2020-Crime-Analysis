###Crime in Los Angeles 2020
###Wesley Stanis



#Packages
library(caret)
library(dplyr)
library(maps)
library(ggplot2)
library(ggmap)
library(ggthemes)
library(tm)
library(SnowballC)
library(wordcloud2)
library(RColorBrewer)
library(quanteda)
library(wordcloud)
library(randomForest)
library(BayesFactor)
library(arules)

setwd("C:\\Users\\Wes\\Desktop\\UCLA_Ext\\Intro")
df <- read.csv("la_crime_data.csv")



#Checking how many NAs there are, as well as checking how many 
#unique values there are for each column
sapply(df, function(x) sum(is.na(x)))
apply(df, 2, function(x) length(unique(x)))




#creating a time category column based on the times of a crime
df$TimeCat <- cut(df$TIME.OCC, breaks = c(0,400,800,1200,1600,2000,2359),
                 labels=c("Midnight to 4AM","Early Morning","Morning",
                          "Afternoon","Late Afternoon/Evening","Evening/Latenight"))
#Making area a factor
df$AREA <- as.factor(df$AREA)

#-----------------------------------------------
#Creating the map for our plots
la_map <- get_stamenmap(
  bbox = c(left = -119.11, bottom = 33.61, right = -117.48, top = 34.61),
  maptype = "terrain",
  zoom = 10
)

#plottig data on the map
ggmap(la_map) +
  geom_point(data = dfx,
             aes(x=LON, y=LAT, col = Vict.Age),
             size = 1.2, alpha = .5) +
  scale_color_viridis_c("Victim's Age", option = "magma") +
  theme_map() +
  theme(legend.background = element_blank()) +
  ggtitle("Crimes in Los Angeles with Gender Unknown")

#Filtering data to have only male and female
df1 <- filter(df, Vict.Sex == "F" | Vict.Sex == "M")


ggmap(la_map) +
  geom_point(data = df1,
             aes(x = LON, y = LAT, col = Vict.Sex),
             size = .1, alpha = .5) +
  labs(colour = "Victim's Gender") +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  theme_map() +
  ggtitle("Crimes in Los Angeles")


#filtering the data to have aggravated assault and deadly weapon crimes  
df_agg_a <- filter(df, Crm.Cd.Desc == "ASSAULT WITH DEADLY WEAPON, AGGRAVATED ASSAULT" |
                     Crm.Cd.Desc == "ASSAULT WITH DEADLY WEAPON ON POLICE OFFICER")

#Creating another filter to have only male and female in this subset
df_agg_a_MF <- filter(df_agg_a, Vict.Sex == "F" | Vict.Sex == "M")

#Creating a subset in data where sex is unknown
dfx <- filter(df, Vict.Sex == "X")


#creating subests of the data for these specific crimes
dfvst <- filter(df, Crm.Cd.Desc == "VEHICLE - STOLEN")
dfburg <- filter(df, Crm.Cd.Desc == "BURGLARY")
dfhom <- filter(df, Crm.Cd.Desc == "CRIMINAL HOMICIDE")
dftheft <- filter(df, Crm.Cd.Desc == "THEFT, PERSON")



ggmap(la_map) +
  geom_point(data = df_agg_a,
             aes(x=LON, y=LAT, col = Vict.Age),
             size = .1, alpha = .5) +
  scale_color_viridis_c("Victim's Age", option = "magma") +
  theme_map() +
  theme(legend.background = element_blank()) +
  ggtitle("Assault with Deadly Weapon")


ggmap(la_map) +
  geom_point(data = dfburg,
             aes(x=LON, y=LAT, col = Vict.Age),
             size = 2, alpha = .6) +
  scale_color_viridis_c("Victim's Age", option = "magma") +
  theme_map() +
  theme(legend.background = element_blank()) +
  ggtitle("Burglaries in Los Angeles")


#Creating another mao focusing on downtown LA
la_map_DT <- get_stamenmap(
  bbox = c(left = -118.42, bottom = 33.97, right = -118.1, top = 34.159),
  maptype = "terrain",
  zoom = 12
)

#Plotting data downtown
ggmap(la_map_DT) +
  geom_point(data = df_agg_a,
             aes(x=LON, y=LAT, col = Vict.Age),
             size = .1, alpha = .5) +
  scale_color_viridis_c("Victim's Age", option = "magma") +
  theme_map() +
  theme(legend.background = element_blank()) +
  ggtitle("Assault with Deadly Weapon")

#Creating more subsets of data regarding the time categories
Nightcrimes = subset(df, TimeCat %in% c('Evening/Latenight', 'Midnight to 4AM'))
Morningcrimes = subset(df, TimeCat %in% c('Early Morning', 'Morning'))
Afternooncrimes = subset(df, TimeCat %in% c('Afternoon', 'Late Afternoon/Evening'))


ggmap(la_map) +
  geom_point(data = Afternooncrimes,
             aes(x = LON, y = LAT, col = TimeCat),
             size = .2, alpha = .5) +
  labs(colour = "Crimes in the Afternoon") +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  theme_map() +
  ggtitle("Crimes in Los Angeles")

ggmap(la_map_DT) +
  geom_point(data = Morningcrimes,
             aes(x = LON, y = LAT, col = TimeCat),
             size = 2, alpha = .5) +
  labs(colour = "Crimes in the Morning") +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  theme_map() +
  ggtitle("Crimes in Los Angeles")

#-----------------------------------------------
##There were 129 different types of crime codes


#function to clean up textual data and generating a word cloud

mywc <- function(text){

  
  corpus<-Corpus(VectorSource(text))
  
  ## cleaning corpus
  corpus<- tm_map(corpus, tolower)
  corpus<- tm_map(corpus, removeNumbers)
  corpus<- tm_map(corpus, removeWords, stopwords("english"))
  corpus<- tm_map(corpus, removePunctuation)
  corpus<- tm_map(corpus, stripWhitespace)
  
  ## create word matrix
  termdocmat <- TermDocumentMatrix(corpus)
  wordmat <- as.matrix(termdocmat)
  
  ## Tally up the lines
  wordmat_tally <- sort(rowSums(wordmat),decreasing=TRUE)
  worddat <- data.frame(word = names(wordmat_tally),freq=wordmat_tally)
  
  wordcloud(words = worddat$word, freq = worddat$freq, min.freq = 1,
            max.words=130, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
}
#Applying the function to the crime descriptions 
mywc(df$Crm.Cd.Desc)
#-----------------------------------------------


#Checking how many instances there is a victim age of 0
sum(df$Vict.Age==0)
#55543

#Factorizing the crime description
df$Crm.Cd.Desc <- as.factor(df$Crm.Cd.Desc)

#Filtering the data to only where the victim's age was 0
dfz <- filter(df, Vict.Age == 0)
#Selecting only the age and crime description variables
dfz <- dfz[c(10,12)]


#Calculating the total times there is a victim labeled at the age of
#0, for each crime description
Crime0 <- dfz %>% group_by(Crm.Cd.Desc, Vict.Age) %>%
   summarise(count = sum(Vict.Age==0))

#Plotting
ggplot(Crime0, aes(reorder(Crm.Cd.Desc,count),count)) +
  geom_bar(stat = "identity", col="Black",fill="Blue")  + 
  labs(title = "Total Crimes Where Victim's\n Age is 0", 
       y = "Count", x = "Crime") +
  theme(axis.text.x=element_text(angle =90))

#Focusing in on the top 5 types of crime
Crime0 %>% 
  arrange(desc(count))%>%
  head(5)%>%
  ggplot(aes(x=reorder(Crm.Cd.Desc,count),y=count)) +
  geom_bar(stat = "identity", col="Black",fill="Blue")  +
  labs(title = "Total Crimes Where Victim's\n Age is 0", 
       y = "Count", x = "Crime") +
  coord_flip()

#Checking out how many crimes there are by time category

#function to create a barplot by count of specific time cats
bptime <- function(dataframe, text) {
  barplot(table(dataframe$TimeCat), main = text)
  
}


bptime(df,"Crimes by Time Frame")
bptime(df_agg_a, "Aggravated Assaults")
bptime(dfhom, "Homicides")
bptime(dftheft, "Theft")
bptime(dfburg, "Burglary")
bptime(dfvst, "Stolen Vehicle")




#Filtering the data o only contain vehicle stolen crimes
dfvs <- filter(df, Crm.Cd.Desc == "VEHICLE - STOLEN")

#There are 24284 instances

sum(dfvs$Vict.Age==0)

#24220 of them have 0 as the age
24284 - 24220


#Checking the map of stoeln b=vehicle crimes

ggmap(la_map) +
  geom_point(data = dfvs,
             aes(x=LON, y=LAT, col = Vict.Age),
             size = .1, alpha = .5) +
  scale_color_viridis_c("Victim's Age", option = "magma") +
  theme_map() +
  theme(legend.background = element_blank()) +
  ggtitle("Vehicle Stolen")


#Sampling data for ease on computing power

set.seed(12)
df2 <- df[sample(nrow(df), 20000, replace = FALSE, prob = NULL),]

#Creating a binary of whether a crime is a stolen vehicle
df2$Vehicle_Stolen <- ifelse(df2$Crm.Cd.Desc=="VEHICLE - STOLEN", 1, 0)

#Creating a binary of whether a weapon was reported
df2$Weapon <- ifelse(df2$Weapon.Desc=="", 0, 1)


#Making the vehicle stolen variable a factor
df2$Vehicle_Stolen <- as.factor(df2$Vehicle_Stolen)

#Subsetting to have only the variables we want for our model
df3 <- df2[c(4,5,12,13,29,30,31)]

#Making weapon a factor
df3$Weapon <- as.factor(df3$Weapon)




#Getting ready to split the data for a random forest model
set.seed(12)
n <- nrow(df3)  
xtrain <- round(n*0.6)  


xindex <- sample(n, xtrain)   # Create an index

train_set <- df3[xindex,]   # Create training set
test_set <- df3[-xindex,]   # Create test set

RFM <- randomForest(Vehicle_Stolen~., data=train_set, ntree=800, 
                   mtry = 3, importance=TRUE)

p1 <- predict(RFM, newdata=test_set, type="class")

# ---------------------------------------------------------------
# Print "confusion matrix" with the actual test set response values
# along with the predicted values using the fitted model "prediction"
table(p1, test_set$Vehicle_Stolen)
varImpPlot(RFM)


print(RFM)

error_rate <- sum(test_set$Vehicle_Stolen != p1) / 
  nrow(test_set)*100
error_rate
#2.6625 %

cm <- confusionMatrix(p1, test_set$Vehicle_Stolen)
cm$table
fourfoldplot(cm$table)

# ---------------------------------------------------------------


#making a logistic regression model


#Function to calculate the ROC curve
calc_ROC <- function(probabilities, known_truth, model.name=NULL)
{
  outcome <- as.numeric(factor(known_truth))-1
  pos <- sum(outcome)
  neg <- sum(1-outcome) 
  pos_probs <- outcome*probabilities 
  neg_probs <- (1-outcome)*probabilities 
  true_pos <- sapply(probabilities,
                     function(x) sum(pos_probs>=x)/pos) 
  false_pos <- sapply(probabilities,
                      function(x) sum(neg_probs>=x)/neg)
  if (is.null(model.name))
    result <- data.frame(true_pos, false_pos)
  else
    result <- data.frame(true_pos, false_pos, model.name)
  result %>% arrange(false_pos, true_pos)
}

#Building our models on at a time, then calculating the ROC curve
glm1 <- glm(Vehicle_Stolen ~ TIME.OCC,
            data = df3,
            family = binomial)

ROC1 <- calc_ROC(probabilities=glm1$fitted.values,
                 known_truth=df3$Vehicle_Stolen,
                 model.name="Time")

glm2 <- glm(Vehicle_Stolen ~ AREA,
            data = df3,
            family = binomial)

ROC2 <- calc_ROC(probabilities=glm2$fitted.values,
                 known_truth=df3$Vehicle_Stolen,
                 model.name="Area")

glm3 <- glm(Vehicle_Stolen ~ Vict.Age,
            data = df3,
            family = binomial)

ROC3 <- calc_ROC(probabilities=glm3$fitted.values,
                 known_truth=df3$Vehicle_Stolen,
                 model.name="Age")

glm4 <- glm(Vehicle_Stolen ~ Vict.Sex,
            data = df3,
            family = binomial)

ROC4 <- calc_ROC(probabilities=glm4$fitted.values,
                 known_truth=df3$Vehicle_Stolen,
                 model.name="Gender")

glm5 <- glm(Vehicle_Stolen ~ TIME.OCC + AREA + Vict.Age + Vict.Sex,
            data = df3,
            family = binomial)

ROC5 <- calc_ROC(probabilities=glm5$fitted.values,
                 known_truth=df3$Vehicle_Stolen,
                 model.name="Combined")

#Plotting the ROC curves all together
ggplot(data=NULL, aes(x=false_pos, y=true_pos)) +
  geom_line(data=ROC1, aes(color=model.name)) +
  geom_line(data=ROC2, aes(color=model.name)) +
  geom_line(data=ROC3, aes(color=model.name)) +
  geom_line(data=ROC4, aes(color=model.name)) +
  geom_line(data=ROC5, aes(color=model.name))

#Gender and combined are the same


#Checking mean for time categories
ChkMean <- function(Time_cat){
  mean(df$Vict.Age[df$TimeCat==Time_cat]) 
  
}
ChkMean("Midnight to 4AM")
ChkMean("Early Morning")
ChkMean("Morning")
ChkMean("Afternoon")
ChkMean("Late Afternoon/Evening")
ChkMean("Evening/Latenight")

#Setting these columns to null as they have many missing data in order to build
#our MCMC model
df$Crm.Cd.2 <- NULL
df$Crm.Cd.3 <- NULL
df$Crm.Cd.4 <- NULL

#Bayesian Anova
y <- anovaBF(Vict.Age ~ TimeCat, data=df)


summary(y)

#50000 simulations for MCMC
mcmcout <- posterior(y, iterations = 50000)
par(mar=c(17.5,4,.25,1))
boxplot(as.matrix(mcmcout[,2:7]),las = 2)

#Summary
summary(mcmcout)





# Importing dataset (129880 Obs)
airline_data = read.csv('D:/Studies/Data Mining II/DM Project/satisfaction_2015.csv')
summary(airline_data)
View(airline_data)


##### DATA CLEANING   #######
# Changing the labels
colnames(airline_data)[1] <- "ID"
colnames(airline_data)[2] <- "Satisfaction"
colnames(airline_data)[10] <- "Departure.or.Arrival.Time.conveniant"

# 393 Missing Values
missing.values <- sum(is.na(airline_data))
missing.values
# Finding which columns have the NA values
names(which(colSums(is.na(airline_data))>0))
# Replacing the NA values with the mean of the column "Arrival Delay in Minutes" 
airline_data$Arrival.Delay.in.Minutes[is.na(airline_data$Arrival.Delay.in.Minutes)] <- round(mean(airline_data$Arrival.Delay.in.Minutes, na.rm = TRUE))


# Replacing the labels of Satisfaction column
airline_data$Satisfaction <- as.character(airline_data$Satisfaction)
airline_data$Satisfaction[airline_data$Satisfaction == 'neutral or dissatisfied']  <- "dissatisfied"
airline_data$Satisfaction <- as.factor(airline_data$Satisfaction)

# Dropping Arrival.Delay.in.Minutes which is highly correlated with Departure.Delay.in.Minutes
# Dropping ID column which has no relation with the survey results
airline_data = subset(airline_data, select = -c(ID,Arrival.Delay.in.Minutes) )



######    RANDOM FOREST    #######

#### DATA PARTITIONING  ####
# 60 % for training and 40% for testing
tnum <- floor(0.6 * nrow(airline_data))
num <- sample(1:nrow(airline_data), tnum)
airline.train <- airline_data[num, ]
airline.test <- airline_data[-num, ]

# Data Modelling
set.seed(99)
library(randomForest)
modrf<-randomForest(airline.train$Satisfaction ~ .,data = airline.train,na.action = na.roughfix)
imp_RF <- importance(modrf)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]
ggplot(imp_DF, aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + coord_flip() + theme(legend.position="none")

library(caret)
# Modelling with 10 most important variables identified above
caret_matrix <- train(x=airline.train[,c(3,4,5,6,8,10,13,14,15,17)], y=airline.train[,1], data=airline.train, method='rf')
caret_matrix
solution_rf <- predict(caret_matrix, airline.test)
confusionMatrix(table(airline.test$Satisfaction,solution_rf), positive = 'satisfied') 


######### BOOSTING ##########
library (gbm)
set.seed (100)
airline.boost =gbm(airline.train$Satisfaction~. ,data=airline.train, distribution="bernoulli", n.trees =500 , interaction.depth =5)
summary(airline.boost)

yhat.boost=predict (airline.boost ,newdata =airline.test,n.trees =500)
mean(( yhat.boost -airline.test$Satisfaction)^2)

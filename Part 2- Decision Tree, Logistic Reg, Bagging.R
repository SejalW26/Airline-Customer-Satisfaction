library(ggplot2)
library(corrplot)
library(dplyr)
library(reshape2)
library(VIM)

# Importing dataset (129880 Obs)
setwd("Project- Passenger Satisfaction")
airline_data <- read.csv("Passenger_Satisfaction.csv", header= TRUE)
summary(airline_data)
View(airline_data)

# Changing the labels 
colnames(airline_data)[1] <- "ID"
colnames(airline_data)[2] <- "Satisfaction"
colnames(airline_data)[10] <- "Departure.or.Arrival.Time.conveniant"
colnames(airline_data)[17] <- "On.board.service"

# 393 Missing Values
missing.values <- sum(is.na(airline_data))
missing.values
aggr(airline_data)

# Finding which columns have the NA values
names(which(colSums(is.na(airline_data))>0))
# Replacing the NA values with the mean of the column "Arrival Delay in Minutes" 
airline_data$Arrival.Delay.in.Minutes[is.na(airline_data$Arrival.Delay.in.Minutes)] <- round(mean(airline_data$Arrival.Delay.in.Minutes, na.rm = TRUE))

# Replacing the labels of Satisfaction column
airline_data$Satisfaction <- as.character(airline_data$Satisfaction)
airline_data$Satisfaction[airline_data$Satisfaction == 'neutral or dissatisfied']  <- "dissatisfied"
airline_data$Satisfaction <- as.factor(airline_data$Satisfaction)
levels(airline_data$Satisfaction) <- c("satisfied" , "dissatisfied")
summary(airline_data)


#Changing Factor variables to Numeric
airline_data$Gender<-as.numeric(airline_data$Gender)
airline_data$Satisfaction<-as.numeric(airline_data$Satisfaction)
airline_data$Customer.Type<-as.numeric(airline_data$Customer.Type)
airline_data$Type.of.Travel<-as.numeric(airline_data$Type.of.Travel)
airline_data$Class<-as.numeric(airline_data$Class)

# Changing type to Factor
airline_data$Gender<-as.factor(airline_data$Gender)
airline_data$Satisfaction<-as.factor(airline_data$Satisfaction)
airline_data$Customer.Type<-as.factor(airline_data$Customer.Type)
airline_data$Type.of.Travel<-as.factor(airline_data$Type.of.Travel)
airline_data$Class<-as.factor(airline_data$Class)

#Finding the Correlation
airline_data_quants<-airline_data[c(3:24)]
airline_data_corr<-cor(airline_data_quants,use="pairwise.complete.obs",method="pearson")
round(airline_data_corr,3)
# Corelation Matrix
airline_data_corr_matrix <- cor(subset(airline_data, select = c(Gender, Customer.Type, Age, Type.of.Travel, Class, Flight.Distance, Seat.comfort, Departure.or.Arrival.Time.conveniant, Food.and.drink, Gate.location, Inflight.wifi.service, Inflight.entertainment, Online.support,Ease.of.Online.booking, On.board.service, Leg.room.service, Baggage.handling, Checkin.service, Cleanliness, Online.boarding, Departure.Delay.in.Minutes, Arrival.Delay.in.Minutes)))
corrplot(airline_data_corr_matrix, method="number")                                       

#HeatMap
cor_matrix<-cor(select_if(airline_data, is.numeric))   # select_if() is a dplyr function
melted_cor_matrix = melt(cor_matrix) # melt is a function of package reshape
options(repr.plot.width=15, repr.plot.height=15)
ggplot(data = melted_cor_matrix, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_gradient2(low = "yellow", high = "red") +
  labs(title = "HeatMAp", x = "Numeric Variables", y = "Numeric Variables", fill = " Correlation Coefficient") +
  theme(axis.text.x = element_text(angle = 90))
options(repr.plot.width=9, repr.plot.height=7)


##########    DATA PARTITIONING    #########

set.seed(1)
train_partition<-sample(c(1:dim(airline_data)[1]),dim(airline_data)[1]*0.7)
training_data<-airline_data[train_partition,]
test_data<-airline_data[-train_partition,]
table(training_data$Satisfaction)
table(test_data$Satisfaction)

##########    DATA MODELLING    #########

### LOGISTIC REGRESSION  ###

logreg <-glm(Satisfaction~., data= airline_data, family ='binomial' )
summary(logreg)
# Predict with training data
predict1<-predict(logreg, training_data, type = 'response')
head(predict1)
#check for accuracy and error percentage
pr1<-ifelse(predict1>0.5,1,0)
table_1<-table(predicted=pr1, Actual=training_data$Satisfaction)
table_1
error1 <- 1-sum(diag(table_1))/sum(table_1)
acc1 <- sum(diag(table_1))/sum(table_1)
plot(training_data$Satisfaction, predict1)

# Predict with test data
predict2<-predict(logreg, test_data, type = 'response')
head(predict2)
#check for accuracy and error percentage
pr2<-ifelse(predict2>0.5,1,0)
table_test<-table(predicted=pr2, Actual=test_data$Satisfaction)
table_test
error2 <- 1-sum(diag(table_2))/sum(table_2)
acc_test <- sum(diag(table_2))/sum(table_2)
acc_test <- acc_test* 100
plot(test_data$Satisfaction, predict2)

library(gains)
gain <- gains(airline_data$Satisfaction, logreg$fitted.values, groups=100)
plot(c(0,gain$cume.pct.of.total*sum(airline_data$Satisfaction))~ c(0,gain$cume.obs), xlab="# cases", ylab="Cumulative", main="", type="l") 
lines(c(0,sum(airline_data$Satisfaction))~c(0, dim(airline_data)[1]), lty=2)

confusionMatrix(logreg, airline_data$Satisfaction)
str(logreg)
str(airline_data)
plotROC(test_data$Satisfaction, predict2)


### DECISION TREES  ###

install.packages("rpart")
library(rpart)
install.packages("tree")
library(tree)
library(caret)
library(rpart.plot)
set.seed(1)
dectree <- train(Satisfaction ~ .,
                 data = training_data,
                 method = "rpart")
dectree
rpart.plot(dectree$finalModel)

ndectree<-tree(Satisfaction ~ ., data = training_data, method = "rpart")
summary(ndectree)
ndectree

plot(ndectree)
text(ndectree,pretty = 5)
par(mfrow=c(1,1))
ndectree_rpart_tree<-rpart(Satisfaction ~ .,data= training_data, method="class")
rpart.plot(ndectree_rpart_tree,box.palette = "RdBu",shadow.col="gray",nn=TRUE)
summary(ndectree_rpart_tree)

treePreds <- predict(ndectree_rpart_tree, type='class')
summary(treePreds)
table(dnn = c('observed', 'predicted'), airline_data$Satisfied, treePreds)


# Validation data decision tree
ndectree_prediction<-predict(ndectree,newdata=test_data, type= "class")
confusionMatrix(ndectree,test_data$Satisfaction)

ndectree <- as.factor(ndectree)
str(ndectree)
str(test_data$Satisfaction)

confusionMatrix(
  factor(ndectree_prediction, levels = 1:2),
  factor(test_data$Satisfaction, levels = 1:2)
)


### BAGGING   ###

library(ipred)
library(rpart)
library(MASS)
install.packages("Ecdat")
library(Ecdat)
library(vcd)
library(irr)
gbag <- bagging(Satisfaction ~ ., data = airline_data, coob=TRUE)
print(gbag)
bagPred<-predict(gbag, airline_data)
keep<-table(bagPred, airline_data$Satisfaction)
keep
Kappa(keep)

ctrl<-trainControl(method="cv", number=10)
trainModel<-train(Satisfaction~.,data=airline_data, method="treebag",trControl=ctrl)



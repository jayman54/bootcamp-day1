#Install and load the dplyr package.  It is much easier to work with than standard R data frames
install.packages("dplyr")
#Install the RCurl library used to for downloading https data
install.packages("RCurl")

library(dplyr)
library(RCurl)


#Load the data from a URL from UCI
urlDataLink <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data'
urlDataUrl <- getURL(urlDataLink, ssl.verifypeer= FALSE)

#load the data from the URL into a dataframe
adult <-read.csv(textConnection(urlDataUrl), header = FALSE)

#release the URL freeing up memory
rm(urlDataUrl)

#dataframe does not have any headers.  Add headers
names(adult) <- c('Age', 'Workclass','FinalWeight','Education','EducationNumber','MaritalStatus','Occupation','Relationship','Race'
                  ,'Gender','CapitalGain','CapitalLoss','HoursPWeek','NativeCountry','Income')

#Load csv into dplyr table
adult <- tbl_df(adult)

#Certain attributes contain "?" to represent NA. change back to NA
adult <- mutate(adult, Workclass = ifelse(Workclass==' ?', NA, Workclass))
adult <- mutate(adult, Occupation = ifelse(Occupation==' ?', NA, Occupation))
adult <- mutate(adult, NativeCountry = ifelse(NativeCountry==' ?', NA, NativeCountry))

#Summary Stats
summary(adult)
head(adult)

#random number
r <- runif(dim(adult)[1])

#split
train <- adult[r<= .7,]
test <- adult[r> .7,]


#train model
model <- glm(Income ~.,family=binomial(link='logit'),data=train)

summary(model)


#Preidict
install.packages("ROCR")
library(ROCR)

#Scoreing the data
p <- predict(model, newdata=test, type="response")

#Evaluating
pr <- prediction(p, test$Income)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc



#RF
install.packages("randomForest")
library(randomForest)
model.rf <- randomForest(Income ~ ., data=train)
summary(model.rf)

#Scoreing the data
p.rf <- predict(model.rf, newdata=test, type="response")

#Evaluating
pr.rf <- prediction(p.rf, test$Income)
prf.rm <- performance(pr.rf, measure = "tpr", x.measure = "fpr")
plot(prf.rf)

auc.rf <- performance(pr.rf, measure = "auc")
auc.rf <- auc.rf@y.values[[1]]
auc.rf


#PMML
library("pmml")

pmml(model)
pmml(model.rf)


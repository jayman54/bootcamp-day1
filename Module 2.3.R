#Install and load the dplyr package.  It is much easier to work with than standard R data frames
#install.packages("dplyr")
#Install the RCurl library used to for downloading https data
#install.packages("RCurl")

library(dplyr)
library(RCurl)
library(ggplot2)

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
adult <- adult %>%
  mutate(Workclass = ifelse(Workclass==' ?', NA, Workclass),
        Occupation = ifelse(Occupation==' ?', NA, Occupation),
        NativeCountry = ifelse(NativeCountry==' ?', NA, NativeCountry))

#R is treating these fields like numers and not factors.  We should convert them
adult <- adult %>%
  mutate(Workclass = as.factor(Workclass),
         Occupation = as.factor(Occupation),
         NativeCountry = as.factor(NativeCountry))


#Summary Stats
summary(adult)
head(adult)


#************************************************************
#Sampling
#************************************************************

#for reproduceability set seed
set.seed(123)

#create vecotr equal length to the data set

r <- runif(dim(adult)[1]) %>%
  as.data.frame()
names(r) <- c("rand")

#bind cols back togeather with a vector of random values
adult <- bind_cols(adult, r)

#Create training and test and optimization sets

#Training Set with 60% of all data
adult.train <- adult %>%
  filter(rand < .6)

#Optimization set of 10%
adult.optimize <- adult %>%
  filter(rand >= .6, rand < .7)

#Test set of 30%
adult.test <- adult %>%
  filter(rand >= .7)



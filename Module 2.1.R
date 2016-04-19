#Install and load the dplyr package.  It is much easier to work with than standard R data frames
#install.packages("dplyr")
#Install the RCurl library used to for downloading https data
#install.packages("RCurl")

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

#***************************************************
# Missing Values
#***************************************************

#Check NAs
adult %>%
  select(Workclass, Occupation, NativeCountry) %>%
  summary()

#Convert NAs to 0.  Only if 0 does not already exist
adult <- adult %>% 
  mutate( Workclass.clean.0 = as.factor(ifelse(is.na(Workclass),0, as.character(Workclass)))
         )

adult %>%
  select(Workclass, Workclass.clean.0) %>%
  summary()

#Convert NAs to most occuring level

#determine the most occuring level
x <- adult %>% 
  group_by(Workclass) %>%
  summarise(n = n()) %>%
  top_n(1) %>%
  mutate(n = as.character(Workclass)) %>%
  select(n) %>%
  as.character()

#Assign NA levels the most occuring value
adult <- adult %>% 
  mutate( Workclass.clean.MO = as.factor(ifelse(is.na(Workclass),x, as.character(Workclass)))
  )

adult %>%
  select(Workclass, Workclass.clean.0, Workclass.clean.MO) %>%
  summary()

#***************************************************
# Outliers
#***************************************************

#Check for outliers
adult %>%
  select(HoursPWeek) %>%
  summary()

#Anyone who has more than 60 hours will be converted to 61 hours
adult <- adult %>%
  mutate(HoursPWeek.clean.61 = ifelse(HoursPWeek > 60, 61, HoursPWeek))

adult %>%
  select(HoursPWeek, HoursPWeek.clean.61) %>%
  summary()

#Group work hours into 4 groups
adult <- adult %>%
  mutate(HoursPWeek.clean.groups = 
           as.factor(
             ifelse(HoursPWeek < 5, "No Work",
                 ifelse(HoursPWeek < 35, "PartTime",
                        ifelse(HoursPWeek < 50, "FullTime", "OverTime")
                 ) #ifelse
             )
           )# as.factor
         ) #mutate

adult %>%
  select(HoursPWeek, HoursPWeek.clean.groups) %>%
  summary()


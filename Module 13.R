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

#R is treating these fields like numers and not factors.  We should convert them
adult <- adult %>%
  mutate(Workclass = as.factor(Workclass),
         Occupation = as.factor(Occupation),
         EducationNumber = as.factor(EducationNumber),
         NativeCountry = as.factor(NativeCountry))

#Check summary stats again
summary(adult)

#Three fields are showing NAs.  Lets take a closeer look
adult %>%
  select(Workclass, Occupation, NativeCountry) %>%
  summary()

#Workclass and Occupaiton appear to have missing values.  How many?  is it random or systemic?
adult %>%
  select(Workclass, Occupation) %>%
  summary()

#Run a count to see how many Occupation Workclass NAs appear in tandem.  Clearly these two are linked.  When one is NA so is the other 
# except for 7 records. What does this mean?
adult %>%
  filter(is.na(Workclass)) %>%
  group_by(Occupation) %>%
  summarise(Cnt=n()) 

adult %>%
  filter(is.na(Occupation)) %>%
  group_by(Workclass) %>%
  summarise(Cnt=n())


# Notice that some of the adults have an age = 17.  Approximately  395.  Are they really 17?  were they given the age of 17 for any age under 18?  Why does it stop at 17?
# Does education, maritalstatus, captital gain mean the same thing for someone who is youg?  Should we treat this as two different datasets based on age?

#Run Summary so we can get a comparisson
adult %>%
  summary()
# Run it just for under 18
adult %>%
  filter(Age < 18) %>%
  summary()


#Eduction and EducationNumber are linked
adult %>%
  select(Education,EducationNumber) %>%
  distinct()  %>%
  arrange(EducationNumber)

adult %>%
  group_by(Education,EducationNumber) %>%
  summarise(
    cnt = n()) %>%
  ungroup() %>%
  arrange(EducationNumber)

#Visualize the distribution of education                                
adult %>% 
  mutate(Ed = as.numeric(EducationNumber)) %>%
  select(Ed) %>%
  as.matrix() %>%
  hist()

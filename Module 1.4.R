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


#********************************** Univariate distributions ********************************** 

#************************************************************
#Histograms of Continious variables
#************************************************************

#Visualize the distribution of education from previous exercise                         
adult %>% 
  #mutate(Ed = as.numeric(EducationNumber)) %>%
  select(EducationNumber) %>%
  as.matrix() %>%
  hist()

#ggplot2 histogram Education
ggplot(adult) +
  geom_histogram(aes(x=EducationNumber),
                 binwidth=1, fill="gray")

#ggplot2 histogram Age
ggplot(adult) +
  geom_histogram(aes(x=Age),
                 binwidth=2, fill="gray")

#ggplot2 histogram HoursPerweek
ggplot(adult) +
  geom_histogram(aes(x=HoursPWeek),
                 binwidth=5, fill="gray")

#************************************************************
#Density plots of Continious variables
#************************************************************

#ggplot2 DP Education
ggplot(adult) +
  geom_density(aes(x=EducationNumber))

#ggplot2 DP Age
ggplot(adult) +
  geom_density(aes(x=Age))

#ggplot2 DP Hours Per week
ggplot(adult) +
  geom_density(aes(x=HoursPWeek))

#************************************************************
#Bar plot of categorical data
#************************************************************

ggplot(adult) + 
  geom_bar(aes(x=Race), fill="gray")

ggplot(adult) + 
  geom_bar(aes(x=MaritalStatus), fill="gray")

ggplot(adult) + 
  geom_bar(aes(x=Relationship), fill="gray")

ggplot(adult) + 
  geom_bar(aes(x=Occupation), fill="gray")

#************************************************************
#Multivariate
#************************************************************

#************************************************************
#Scatter
#************************************************************

#create a numeric version of income
adult <- adult %>%
  mutate(Inc = ifelse(Income == " >50K",1,0))
  
#Non linear sooth
ggplot(adult, aes(x=Age, y=Inc)) + geom_point() 
ggplot(adult, aes(x=Age, y=Inc)) + geom_point() + stat_smooth(method="lm")
ggplot(adult, aes(x=Age, y=Inc)) + geom_point() + stat_smooth(method="lm") +stat_smooth()

#Non linear smooth
ggplot(adult, aes(x=EducationNumber, y=Inc)) + geom_point()
ggplot(adult, aes(x=EducationNumber, y=Inc)) + geom_point() +stat_smooth(method="lm")
ggplot(adult, aes(x=EducationNumber, y=Inc)) + geom_point() +stat_smooth(method="lm") + stat_smooth()
#************************************************************
#Box plots
#************************************************************

ggplot(adult, aes(y=Age, x=Income)) + geom_boxplot()

ggplot(adult, aes(y=Age, x=MaritalStatus)) + geom_boxplot()










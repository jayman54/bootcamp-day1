#Install and load the dplyr package.  It is much easier to work with than standard R data frames
#install.packages("dplyr")
#Install the RCurl library used to for downloading https data
#install.packages("RCurl")
#install.packages("caret")

library(dplyr)
library(RCurl)
library(ggplot2)
library(caret)

#Load the data from a URL from UCI
urlDataLink <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data'
urlDataUrl <- getURL(urlDataLink, ssl.verifypeer= FALSE)

#load the data from the URL into a dataframe
house <-read.table(textConnection(urlDataUrl), header = FALSE, sep = "")

#release the URL freeing up memory
rm(urlDataUrl)

#dataframe does not have any headers.  Add headers
names(house) <- c('CRIM', 'ZN','INDUS','CHAS','NOX','RM','AGE','DIS','RAD'
                  ,'TAX','PTRATIO','B','LSTAT','MEDV')

#Load csv into dplyr table
house <- tbl_df(house)


#R is treating these fields like numers and not factors.  We should convert them
house <- house %>%
  mutate(CHAS = as.factor(CHAS),
         RAD = as.factor(RAD)
         )


#Summary Stats
summary(house)
head(house)


#************************************************************
#Bucket continious
#************************************************************

#Density of Mean Value
ggplot(house) +
  geom_density(aes(x=MEDV))

#Density of Median Value by access to the charles river
house %>%
  ggplot(aes(x=MEDV, fill=CHAS)) +
  geom_density(, alpha=.5)

#Density of Median Value by access to radial highway
house %>%
  ggplot(aes(x=MEDV, fill=RAD)) +
  geom_density(, alpha=.25)

#find the most expensive RAD
house %>%
  group_by(RAD) %>%
  summarise(n=mean(MEDV)) %>%
  arrange(desc(n))

#Density of Median Value by access by age
#house %>%
#  ggplot(aes(x=MEDV, fill=AGE)) +
#  geom_density(, alpha=.25)

#summary of age
house %>%
  select(AGE) %>%
  summary()

#cut ages into buckets
house <- house %>%
  mutate(AGE.bucket = cut(AGE, breaks=quantile(AGE)))

#Density of Median Value by access by age bucket
house %>%
  ggplot(aes(x=MEDV)) +
  geom_density(, alpha=.25) 

house %>%
  ggplot(aes(x=MEDV, fill=AGE.bucket)) +
  geom_density(, alpha=.25) 

#Box plots of the same thing
house %>%
  ggplot(aes(x=AGE.bucket, y=MEDV)) +
  geom_boxplot()

#************************************************************
#Binary Transform
#************************************************************

#Create formula to isolate columns we want to dummify
f<- as.formula("~ RAD + CHAS")

#Create the temporary data set of dummified values
x<- predict(dummyVars(f,data=house, sep=".", fullRank=TRUE), newdata=house) %>%
  as.data.frame() %>%
  tbl_df()

#inspect data
summary(x)

#Merge the two sets back togeather
house <- bind_cols(house,x)

#drop the temporary data set
rm(x)

#View the data
house %>%
  select(RAD, RAD.2, RAD.3, RAD.4, RAD.5, RAD.6, RAD.7, RAD.8, RAD.24 ) %>%
  head()

#************************************************************
#Scale 
#************************************************************

house <- house %>%
    mutate(MEDV.CenterScale = scale(MEDV),
           CRIM.CenterScale = scale(CRIM)
           )


#************************************************************
#Log Transform
#************************************************************

#reload the house data before logging.  The sale functions throw it off

#Non Log
house %>% 
  ggplot(aes(x=MEDV)) + geom_density()

#Log Transform
house %>% 
  mutate(MEDV.ln = log(MEDV)) %>%
  ggplot(aes(x=MEDV.ln)) + geom_density()


#Non Log
house %>% 
  ggplot(aes(x=MEDV, y=LSTAT)) + geom_point() +stat_smooth()

#Log Transform
house %>% 
  mutate(MEDV.ln = log(MEDV)) %>%
  ggplot(aes(x=MEDV.ln, y=LSTAT)) + geom_point() +stat_smooth()

#Non Log
ggplot(house, aes(x=MEDV, y=RM)) + geom_point() +stat_smooth()

#Log Transform
house %>% 
  mutate(MEDV.ln = log(MEDV)) %>%
  ggplot(aes(x=MEDV.ln, y=RM)) + geom_point() +stat_smooth()























#Install and load the dplyr package.  It is much easier to work with than standard R data frames
#features include the following functions

#filter() (and slice())
#arrange()
#select() (and rename())
#distinct()
#mutate() (and transmute())
#summarise()
#sample_n() and sample_frac()

install.packages("dplyr")
library(dplyr)

#Load csv into dplyr table
Auto <- tbl_df(read.csv("AutomobilePriceData.csv"))




#return the first few rows
head(Auto)




#Complete summary statistics for data set
summary(Auto)


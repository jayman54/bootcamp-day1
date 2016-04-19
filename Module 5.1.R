install.packages("pmml")
library(pmml)

head(iris)

irisLR <- lm(Sepal.Length~., iris)

saveXML(pmml(irisLR),"IrisLR.xml")

#http://www.pmml2sql.com/
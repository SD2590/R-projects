adult <- read.csv('/resources/data/adult_sal.csv')

head(adult)

library(dplyr)
adult <- select(adult,-X)

str(adult)
summary(adult)

#Combine the employer into groups
adult$type_employer <- as.character(adult$type_employer)
adult$type_employer[adult$type_employer=='Never-worked'|adult$type_employer=='Without-pay'] <- 'Unemployed'
adult$type_employer[adult$type_employer=='Local-gov'|adult$type_employer=='State-gov'] <- 'SL-gov'
adult$type_employer[adult$type_employer=='Self-emp-inc'|adult$type_employer=='Self-emp-not-inc'] <- 'Self-emp'
adult$type_employer <- as.factor(adult$type_employer)

#Combile the marital into groups
adult$marital <- as.character(adult$marital)
adult$marital[adult$marital=='Divorced'|adult$marital=='Separated'|adult$marital=='Widowed'] <- 'Not-Married'
adult$marital[adult$marital=='Married-AF-spouse'|adult$marital=='Married-civ-spouse'|adult$marital=='Married-spouse-absent'] <- 'Married'
adult$marital <- as.factor(adult$marital)

#Group countries into continent
install.packages('countrycode')
library(countrycode)
adult$continent <- factor(countrycode(sourcevar = adult[, "country"],origin = "country.name",destination = "continent"))
                                     
#got warning that Some values were not matched unambiguously: ?, Columbia, England, Hong, Scotland, South, Yugoslavia
summary(adult$continent)
adult$continent[adult$country=='Columbia'] <- 'Americas'
adult$continent[adult$country=='Scotland'] <- 'Europe'
adult$continent[adult$country=='England'] <- 'Europe'
adult$continent[adult$country=='Hong'] <- 'Asia'
adult$continent[adult$country=='Yugoslavia'] <- 'Europe'

#Split Americas intp NA and SA
adult$continent <- as.character(adult$continent)
adult %>% distinct(country,continent) %>%  select(country,continent) %>% filter(continent == 'Americas') 
adult$continent[adult$country=='United-States'] <- 'North America'
adult$continent[adult$country=='Canada'] <- 'North America'
adult$continent[adult$country=='Puerto-Rico'] <- 'North America'

adult$continent[adult$continent=='Americas'] <- 'Latin and South America'
adult$continent <- as.factor(adult$continent)
summary(adult$continent)

#Change ? values to Na
adult[adult == '?'] <- NA

#Use na.omit() to omit NA data from the adult data frame
adult <- na.omit(adult)
#Check if any NA is there
any(is.na(adult))

library(ggplot2)
library(dplyr)

#Histogram of ages colored by income
ggplot(adult,aes(age)) + geom_histogram(aes(fill=income),color='black',binwidth=1) + theme_bw()

#Barplot of type_employer colored by income
ggplot(adult,aes(type_employer)) + geom_bar(aes(fill=income),color='black') + theme_bw()

#Plot continent vs education colored by income
ggplot(adult,aes(continent,education)) + geom_count(aes(color=income)) + theme_bw()

#Plot a histogram of hours worked per week
ggplot(adult,aes(hr_per_week)) + geom_histogram(fill='blue', bins=50) + theme_bw()

#Create a barplot of region with the fill color defined by income class
ggplot(adult,aes(continent)) + geom_bar(aes(fill=income)) + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Remove extra column before building model
adult <- select(adult,-country)

# Import Library
library(caTools)


# Set a random see so your "random" results are the same as this notebook
set.seed(101) 

# Split up the sample, basically randomly assigns a booleans to a new column "sample"
sample <- sample.split(adult$income, SplitRatio = 0.70) # SplitRatio = percent of sample==TRUE

# Training Data
train = subset(adult, sample == TRUE)

# Testing Data
test = subset(adult, sample == FALSE)


#Build logistic regression model
model = glm(income ~ ., family = binomial(logit), data = train)
summary(model)
new.step.model <- step(model)
summary(new.step.model)

#Create a confusion matrix using the predict function with type='response' as an argument inside of that function.
test$predicted.income = predict(model, newdata=test, type="response")
table(test$income, test$predicted.income > 0.5)

#What was the accuracy of our model?
(6300+1368)/(6300+479+880+1368)

#recall
6300/(6300+479)

#Precision
6300/(6300+880)

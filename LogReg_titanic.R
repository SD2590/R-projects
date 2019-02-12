df.train <- read.csv("/resources/data/titanic_train.csv")
#Amelia is a package to map missing values
library(Amelia)

library(ggplot2)

#survived vs not survived?
ggplot(df.train,aes(Survived)) + geom_bar(fill="blue") + geom_text(stat='count', aes(label=..count..), vjust=-1)

#Pclass distribution
ggplot(df.train,aes(Pclass)) + geom_bar(aes(fill=factor(Pclass)))

#Males and Females?
ggplot(df.train,aes(Sex)) + geom_bar(aes(fill=factor(Sex)))

#Histogram of Ages onboard
ggplot(df.train,aes(Age)) + geom_histogram(fill="blue",alpha=0.3,bins=20)

#Jitter plot of Pclass vs Age based on survival
ggplot(df.train,aes(Pclass, Age)) + geom_jitter(aes(colour = factor(Survived))) +  facet_grid(Sex ~ .) 
                                                

#siblings/spouses
ggplot(df.train,aes(SibSp)) + geom_bar(aes(fill=factor(SibSp)))

#Fare distribution?
ggplot(df.train,aes(Fare)) + geom_histogram(fill="green",color="black",alpha=0.5)

#Find avg age by PClass to fill in the missing age values
pl <- ggplot(df.train,aes(Pclass,Age)) + geom_boxplot(aes(group=Pclass,fill=factor(Pclass),alpha=0.4)) 
pl + scale_y_continuous(breaks = seq(min(0), max(80), by = 2)) + theme_bw()

#Imputation of Age based on Pclass
impute_age <- function(age,class){
  out <- age
  for (i in 1:length(age)){
    
    if (is.na(age[i])){
      
      if (class[i] == 1){
        out[i] <- 37
        
      }else if (class[i] == 2){
        out[i] <- 29
        
      }else{
        out[i] <- 24
      }
    }else{
      out[i]<-age[i]
    }
  }
  return(out)
}

fixed.ages <- impute_age(df.train$Age,df.train$Pclass)
  
df.train$Age <- fixed.ages


library(dplyr)
#Select only the columns needed
df.train <- select(df.train,-PassengerId,-Name,-Ticket,-Cabin)
head(df.train)
str(df.train)

#Change some columns to factors
df.train$Survived <- factor(df.train$Survived)
df.train$Pclass <- factor(df.train$Pclass)
df.train$Parch <- factor(df.train$Parch)
df.train$SibSp <- factor(df.train$SibSp)

#Build logister Regression model
log.model <- glm(Survived ~ .,family = binomial('logit'),data=df.train)
summary(log.model)

#Test the model
df.test <- read.csv("/resources/data/titanic_test.csv")

#Handle the NAs in test dataset
pl1 <- ggplot(df.test,aes(Pclass,Age)) + geom_boxplot(aes(group=Pclass,fill=factor(Pclass),alpha=0.4)) 
pl1 + scale_y_continuous(breaks = seq(min(0), max(80), by = 2)) + theme_bw()

impute_age_test <- function(age,class){
  out <- age
  for (i in 1:length(age)){
    
    if (is.na(age[i])){
      
      if (class[i] == 1){
        out[i] <- 42
        
      }else if (class[i] == 2){
        out[i] <- 26
        
      }else{
        out[i] <- 24
      }
    }else{
      out[i]<-age[i]
    }
  }
  return(out)
}

fixed.ages.test <- impute_age_test(df.test$Age,df.test$Pclass)

df.test$Age <- fixed.ages.test




#Handle the row with Fare as NA
pl2 <- ggplot(df.test,aes(Pclass,Fare)) + geom_boxplot(aes(group=Pclass,fill=factor(Pclass),alpha=0.4)) 
pl2 + scale_y_continuous(breaks = seq(min(0), max(515), by = 5)) + theme_bw()

test.nona <- subset(test,is.na(test$Fare)=F)
tapply(test.nona$Fare,test.nona$Pclass,mean)
#Mean fare for Pclass 3 is 12.459
df.test$Fare[is.na(df.test$Fare)] <- 12.459

#Remove unwanted cols
df.test <- select(df.test,-PassengerId,-Name,-Ticket,-Cabin)

#Change int to factor class
df.test$Pclass <- factor(df.test$Pclass)
df.test$Parch <- factor(df.test$Parch)
df.test$SibSp <- factor(df.test$SibSp)

library(plyr)
df.test$Parch <-  revalue(df.test$Parch,c('9' = '6'))
fitted.prob <- predict(log.model,df.test,type='response')
fitted.results <- ifelse(fitted.prob>0.5,1,0)

test <- read.csv("/resources/data/titanic_test.csv")

#Build data frame with PaasengerID and Survived columns
result <- data.frame(test$PassengerId,fitted.results)
colnames(result) <- c('PassengerId','Survived')

#write to csv
write.csv(result, "Titanic.csv", row.names=FALSE)


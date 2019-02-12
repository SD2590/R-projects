bike <- read.csv("/resources/data/bikeshare.csv")
library(ggplot2)
#Create a scatter plot of count vs temp
ggplot(bike,aes(x=temp,y=count)) + geom_point(aes(color=temp),alpha=0.2) + theme_bw()

#Plot count versus datetime as a scatterplot with a color gradient based on temperature
#convert the datetime column into POSIXct before plotting.
bike$datetime <- as.POSIXct(bike$datetime)
ggplot(bike,aes(x=datetime,y=count)) + geom_point(aes(color=temp),alpha=0.2) + scale_color_gradient(low='green',high='orange')
ggplot(bike,aes(datetime,count)) + geom_point(aes(color=temp),alpha=0.5)  + scale_color_continuous(low='#55D8CE',high='#FF6E2E') +theme_bw()


#Create a boxplot, with the y axis indicating count and the x axis begin a box for each season.
ggplot(bike,aes(x=factor(season),y=count)) + geom_boxplot(aes(color=factor(season)))

#Add column for hour
bike$hour <- format(bike$datetime,"%H")

#create a scatterplot of count versus hour, with color scale based on temp. Only use bike data where workingday==1.
bike_workingday <- subset(bike,workingday==1)
ggplot(bike_workingday,aes(x=hour,y=count)) + geom_point(aes(color=temp),alpha=0.3,position=position_jitter(w=1, h=0)) + scale_color_gradientn(colors=c('dark blue','blue','light blue','light green','yellow','orange','red')) + theme_bw()

#create the same plot for non working days:
bike_nonworkingday <- subset(bike,workingday==0)
ggplot(bike_nonworkingday,aes(x=hour,y=count)) + geom_point(aes(color=temp),alpha=0.3,position=position_jitter(w=1, h=0)) + scale_color_gradientn(colors=c('dark blue','blue','light blue','light green','yellow','orange','red')) + theme_bw()


#Build model based on temp
temp.model <- lm(count~temp,bike)
summary(temp.model)

#How many bike rentals would we predict if the temperature was 25 degrees Celsius?
newdata <- data.frame(temp=25)
predict(temp.model,newdata)

#Use sapply() and as.numeric to change the hour column to a column of numeric values.
bike$hour <- sapply(bike$hour,as.numeric)

#Build a model that attempts to predict count based off of the following features
#season
#holiday
#workingday
#weather
#temp
#humidity
#windspeed
#hour (factor)

model <- lm(count ~ . -casual - registered -datetime -atemp,bike )

summary(model)

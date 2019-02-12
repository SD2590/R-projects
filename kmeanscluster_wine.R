df1 <- read.csv("/resources/data/winequality-red.csv",sep = ";")
df2 <- read.csv("/resources/data/winequality-white.csv",sep = ";")

#Add a new column for label
df1$label <- 'Red'
df2$label <- 'White'

#Combine the two df
wine <- rbind.data.frame(df1,df2)

#EDA
library(ggplot2)
#Histogram of residual sugar from the wine data. Color by red and white wines
ggplot(wine,aes(residual.sugar)) + geom_histogram(aes(fill=label),color="black",bins = 50) + scale_fill_manual(values = c('#ae4554','#faf7ea')) + theme_bw()

#Create a Histogram of citric.acid from the wine data. Color by red and white wines
ggplot(wine,aes(citric.acid)) + geom_histogram(aes(fill=label),color="black",bins=50) + scale_fill_manual(values = c('#ae4554','#faf7ea')) + theme_dark()

#Create a Histogram of alcohol from the wine data. Color by red and white wines.
ggplot(wine,aes(alcohol)) + geom_histogram(aes(fill=label),color="black",bins=50) + scale_fill_manual(values = c('#ae4554','#faf7ea')) + theme_dark()

#Create a scatterplot of residual.sugar versus citric.acid, color by red and white wine.
ggplot(wine,aes(citric.acid,residual.sugar)) + geom_point(aes(color=label), alpha=0.4) + scale_color_manual(values = c('#ae4554','#faf7ea')) + theme_dark()

#Create a scatterplot of volatile.acidity versus residual.sugar, color by red and white wine.
ggplot(wine,aes(volatile.acidity,residual.sugar)) + geom_point(aes(color=label), alpha=0.4) + scale_color_manual(values = c('#ae4554','#faf7ea')) + theme_dark()


#Build the k means cluster model
clus.data <- wine[,1:12]

wine.cluster <- kmeans(clus.data,centers = 2,nstart=30)
table(wine.cluster$cluster,wine$label)

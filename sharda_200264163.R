#R Mini Project: Data loading, cleaning and transformation 

#(1)Loading the Titanic Dataset into data.df dataframe
data.df=read.csv("train.csv")

#(2)Data Summarization - number of tuples and attributes in the dataset
data.df.n_rows=nrow(data.df)
data.df.n_cols=ncol(data.df)
data.df.n_rows
data.df.n_cols

#(3)Data Subsetting
data.df.subset<-data.df[,c(1,6,10,12)]
data.df.subset

#(4)Data Cleaning
#(a)Replacing NA/missing values in 'Age' with its median
Age_median <- median(na.omit(data.df.subset$Age))
data.df.subset$Age[is.na(data.df.subset$Age)] <- Age_median
data.df.subset

#(b)Replacing NA/missing values in 'Embarked' with its mode
y<-max(table(data.df.subset$Embarked))
Embarked_mode<-names(table(data.df.subset$Embarked))[table(data.df.subset$Embarked)==y]
data.df.subset$Embarked[data.df.subset$Embarked == ""] <- Embarked_mode
data.df.subset$Embarked

#(c)Replacing NA/missing values in 'Fare' with its mean: 
#Assumption - The fare cannot be 0 and any entry that is recorded as 0.0000 is a missing value or incorectly captured as 0.0000
mean_Fare=sum(data.df.subset$Fare["Fare"!=0])/nrow(subset(data.df.subset,subset=(Fare!=0)))
data.df.subset$Fare[data.df.subset$Fare==0] <- mean_Fare

#Final dataset after cleaning
data.df.subset

#(5)Data Visualizatiom
#(a)Histogram for age of passengerrs on board the Titanic
hist(data.df.subset$Age,main="Histogram for Age of Passengers on RMS Titanic",xlab="Age",ylab="Number of Passengers",xlim=c(0,90),ylim=c(0,250),las=1,breaks=30,col="black",border="white")

#(b)Scatter plot for Fare vs Age
attach(data.df.subset)
plot(Age,Fare,main="Scatter Plot for Fare vs Age",xlab="Age",ylab="Fare",pch=20)

#(6)Anomaly Detection - Detecting anomalies in Age and storing the corresponding PassengerIDs
Age_sd=sd(data.df.subset$Age)
Age_sd
Age_mean=mean(data.df.subset$Age)
lb=Age_mean - 2*Age_sd
ub=Age_mean + 2*Age_sd

anomalous_indices=subset(data.df.subset$PassengerId,subset=(data.df.subset$Age<lb | data.df.subset$Age>ub))
anomalous_indices

#(7)Data subsetting part 2
#(a)
subset1 <- subset(data.df.subset,subset=(Age>=25 & Age<=80))
subset1
#(b)
data.df.subset.v2 <- subset1[,c("Age","Fare","Embarked")]
data.df.subset.v2


#(8)Rescaling Fare to (0,100) range and 
require(scales)
data.df.subset.v2$Fare
Fare_Rescaled <- rescale(data.df.subset.v2$Fare,to=c(0,100))
data.df.subset.v2$Fare_Rescaled <- Fare_Rescaled
data.df.subset.v2





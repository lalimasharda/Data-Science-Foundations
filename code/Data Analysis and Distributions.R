require(data.table)

#Q1a
# return: string(“cylinders”, “displacement”, “horsepower”, “weight”, “acceleration”, “year”, “origin”), 
# represents the covariate providing the best prediction
SLR <- function(path='../data/hw23R-linear.txt'){
  my_data <- read.csv(path)  
  my_data
  
  #attributes_list=c("cylinders", "displacement", "horsepower", "weight", "acceleration", "year", "origin") 
  attributes_list=c(colnames(my_data))
  #mpg vs cylinders
  my_data.lm1=lm(mpg~cylinders,data=my_data)
  plot(my_data$cylinders, my_data$mpg, main="mpg vs cylinders", 
       xlab="cylinders", ylab="mpg", pch=4)
  abline(my_data.lm1)
  summary(my_data.lm1)
  
  
  #mpg vs displacement
  my_data.lm2=lm(mpg~displacement,data=my_data)
  plot(my_data$displacement, my_data$mpg, main="mpg vs displacement", 
       xlab="displacement", ylab="mpg", pch=4)
  abline(my_data.lm2)
  summary(my_data.lm2)
  
  
  #mpg vs horsepower
  my_data.lm3=lm(mpg~horsepower,data=my_data)
  plot(my_data$horsepower, my_data$mpg, main="mpg vs horsepower", 
       xlab="horsepower", ylab="mpg", pch=4)
  abline(my_data.lm3)
  summary(my_data.lm3)
  
  
  #mpg vs weight
  my_data.lm4=lm(mpg~weight,data=my_data)
  plot(my_data$weight, my_data$mpg, main="mpg vs weight", 
       xlab="weight", ylab="mpg", pch=4)
  abline(my_data.lm4)
  summary(my_data.lm4)
  
  #mpg vs acceleration
  my_data.lm5=lm(mpg~acceleration,data=my_data)
  plot(my_data$acceleration, my_data$mpg, main="mpg vs acceleration", 
       xlab="acceleration", ylab="mpg", pch=4)
  abline(my_data.lm5)
  summary(my_data.lm5)
  
  #mpg vs year
  my_data.lm6=lm(mpg~year,data=my_data)
  plot(my_data$year, my_data$mpg, main="mpg vs year", 
       xlab="year", ylab="mpg", pch=4)
  abline(my_data.lm6)
  summary(my_data.lm6)
  
  #mpg vs origin
  my_data.lm7=lm(mpg~origin,data=my_data)
  plot(my_data$origin, my_data$mpg, main="mpg vs year", 
       xlab="origin", ylab="mpg", pch=4)
  abline(my_data.lm7)
  summary(my_data.lm7)
  
lst=c(summary(my_data.lm1)$r.squared,summary(my_data.lm2)$r.squared,summary(my_data.lm3)$r.squared,summary(my_data.lm4)$r.squared,summary(my_data.lm5)$r.squared,summary(my_data.lm6)$r.squared,summary(my_data.lm7)$r.squared)
lst
index=which(lst==max(lst))

return(attributes_list[index+1])
}

#Q1b 
# return: list of following variables, Intercept, CylindersCoeff, DispCoeff, HPCoeff, WeightCoeff, AccCoeff, YearCoeff, OriginCoeff
MLR <- function(path='../data/hw23R-linear.txt'){
  my_data2 <- read.csv(path)  
  my_data2
  
  fit <- lm(mpg ~ , data=my_data2)
  summary(fit)
  cf <- c(coef(fit))
  
  #calculating and displying the equation of MLR model
  eq <- paste0("mpg = ", cf[1],
               ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " cyl ",
               ifelse(sign(cf[3])==1, " + ", " - "), abs(cf[3]), " disp",
               ifelse(sign(cf[4])==1, " + ", " - "), abs(cf[4]), " hp",
               ifelse(sign(cf[5])==1, " + ", " - "), abs(cf[5]), " wgt",
               ifelse(sign(cf[6])==1, " + ", " - "), abs(cf[6]), " acln",
               ifelse(sign(cf[7])==1, " + ", " - "), abs(cf[7]), " year",
               ifelse(sign(cf[8])==1, " + ", " - "), abs(cf[8]), " origin")
  
  eq
  
  # fill in the list with the coeffes you compute
  result <- list("Intercept"=cf[1], "CylindersCoeff"=cf[2], "DispCoeff"=cf[3], "HPCoeff"=cf[4], "WeightCoeff"=cf[5], "AccCoeff"=cf[6], "YearCoeff"=cf[7], "OriginCoeff"=cf[8])
  return(result)
}

#Q2
# return: list of following variables, Intercept， Lag1oeff，Lag2Coeff，Lag3Coeff, Lag4Coeff, Lag5Coeff, VolumeCoeff
LogisticRegression <- function(path='../data/hw23R-logistic.txt'){
  my_data3=read.csv(path)
  my_data3
  
  #str(my_data3)
  
  set.seed(123)
  train <- my_data3[1:1000,]
  test <- my_data3[1001:1250,]
  
  model <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume + Today,family=binomial(link='logit'),data=train)
  summary(model)
  
  cf=coef(model)
  cf
  fitted.results <- predict(model,test,type='response')
  fitted.results <- ifelse(fitted.results > 0.5,"Up","Down")
  
  misClasificError <- mean(fitted.results != test$Direction)
  print(paste('Accuracy = ',1-misClasificError))
  
  # fill in the list with the coeffes you compute
  result <- list("Intercept" = cf[1],"Lag1Coeff" =cf[2], "Lag2Coeff" =cf[3], "Lag3Coeff" =cf[4],"Lag4Coeff" =cf[5], "Lag5Coeff" =cf[6],"VolumeCoeff"=cf[7],"todayCoeff"=cf[8])
  return(result)
}

#Q3
# return: float of training accuracy 
LogisticRegressionImproved <- function(path='../data/hw23R-logistic.txt'){
  my_data3=read.csv(path)
  my_data3
  
  #str(my_data3)
  
  set.seed(123)
  train <- my_data3[1:1000,]
  test <- my_data3[1001:1250,]
  
  
  model2 <- glm(Direction ~ Lag5 + Volume + Today, family=binomial(link='logit'),data=train)
  summary(model2)
  
  cf=coef(model2)
  cf    
  
  fitted.results <- predict(model2,test,type='response')
  fitted.results <- ifelse(fitted.results > 0.5,"Up","Down")
  
  misClasificError <- mean(fitted.results != test$Direction)
  print(paste('Accuracy = ',1-misClasificError))
  
}

#Q4
# return: list of two variables, Intercept， xCoeff
BigSLR <- function(path='../data/slr-90m-data.csv'){

  require(biglm)
  my_data3 <- read.csv.ffdf(file = path, header = TRUE, VERBOSE = TRUE, 
                            first.rows =50000,next.rows = 50000, colClasses=NA)
  model3=bigglm(y~x, data = my_data3, fammily=gaussian())
  
  xrange <- c(min(my_data3$x), max(my_data3$x))
  yrange <- c(min(my_data3$y), max(my_data3$y))
  plot(x=NA,xlab="x",ylab="y",type="n",xlim = xrange,ylim = yrange)
  abline(model3)
  summary(model3)
  cf=coef(model3)
  
  set.seed(123)
  rows_data<-nrow(my_data3)
  
  data_1<-my_data3[sample(1:rows_data,size=0.01*rows_data,replace=FALSE),]
  
  data_2<-my_data3[sample(1:rows_data,size=0.02*rows_data,replace=FALSE),]
  
  data_3<-my_data3[sample(1:rows_data,size=0.03*rows_data,replace=FALSE),]
  
  data_4<-my_data3[sample(1:rows_data,size=0.04*rows_data,replace=FALSE),]
  
  data_5<-my_data3[sample(1:rows_data,size=0.05*rows_data,replace=FALSE),]
  
  model_1=bigglm(y~x, data = data_1, fammily=gaussian())
  abline(model_1,col="blue")
  
  model_2=bigglm(y~x, data = data_2, fammily=gaussian())
  abline(model_2,col="red")
  
  model_3=bigglm(y~x, data = data_3, fammily=gaussian())
  abline(model_3,col="green")
  
  model_4=bigglm(y~x, data = data_4, fammily=gaussian())
  abline(model_4,col="orange")
  
  model_5=bigglm(y~x, data = data_5, fammily=gaussian())
  abline(model_5,col="purple")
  
  legend("bottomright",legend = c("Complete Model","1% model","2% model","3% model", "4% model","5% model"),
         col = c("black","blue","red","green","orange","purple"),lty=1:1,cex = 0.8)
  
  # fill in the list with the coeffes you compute
  # from the next line, you should infer that model should be the variable name of your model
  result <- list("Intercept"=cf[1], "xCoeff"=cf[2])
  return(result)
}

#Q5
# return: string ("reject" to reject null hypothesis, "not-reject" to not reject null hypothesis)
ZTest <- function(x, test_type, alpha, pop_mean, pop_sd){
  if(test_type=='left-tailed')
  {
    ztable=qnorm(alpha)
  }
  else if(test_type=='right-tailed')
  {
    ztable=qnorm(1-alpha)
  }
  else if(test_type=='two-tailed')
  {
    ztable1=qnorm(alpha/2)
    ztable2=qnorm(1-(alpha/2))
  }
  zcal=(mean(x)-pop_mean)*sqrt(length(x))/pop_sd
  if(test_type=='left-tailed')
  {
    if(zcal>ztable)
    {
      result="not-reject"
      return(result)
    }
    else
    {
      result="reject"
      return(result)
    }
  }
  else if(test_type=='right-tailed')
  {
    if(zcal>ztable)
    {
      result="reject"
      return(result)
    }
    else
    {	
      result="not-reject"
      return(result)
    }
  }
  else if(test_type=='two-tailed')
  {
    if(zcal>ztable2 || zcal<ztable1)
    {
      result="reject"
      return(result)
    }
    else
    {	
      result="not-reject"
      return(result)
    }	
  }
}

#Q6
# populationDistribution: string('uniform','normal')
# sampleSize: integer (~30)
# numberOfSamples: integer (>100)
# return: list of two variables, mean and se
CLT <- function(populationDistribution, sampleSize, numberOfSamples){
  lims <- runif(3,0,1)
  means <- c()
  for (i in 1:numberOfSamples)
  {
    if (populationDistribution == 'uniform')
    {
      a <- lims[1]/lims[3]
      b <- lims[2]/lims[3]
      if(a > b)
      {
        t = b
        b = a
        a = t
      }
      x <- runif(sampleSize, a, b)
      
    }
    if (populationDistribution == 'normal')
    {
      avg <- lims[1]/lims[3]
      sd <- lims[2]/lims[3]
      x <- rnorm(sampleSize, avg, sd)
      
    }
    means <- c(means, mean(x))
  }
  
  hist(means, main = "Distribution", xlab = "Sample Means", prob = T, col = "darkred")
  
  
  meanClt <- mean(means)
  seClt <- sd(means)
  result <- list("mean"=meanClt, "se"=seClt)
  result
  return(result)
  
}


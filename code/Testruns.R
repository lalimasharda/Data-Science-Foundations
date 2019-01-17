#q 1a

path=('../data/hw23R-linear.txt')
  my_data <- read.csv(path)  
  my_data
  
  my_data.lm1=lm(mpg~cylinders,data=my_data)
  plot(my_data$cylinders, my_data$mpg, main="mpg vs cylinders", 
       xlab="cylinders", ylab="mpg", pch=4)
  #ggplot2.scatterplot(data=d, xName='X',yName='Y')
  #plot(d)
  abline(my_data.lm)
  summary(my_data.lm)
  
  
  #mpg vs displacement
  my_data.lm2=lm(mpg~displacement,data=my_data)
  plot(my_data$displacement, my_data$mpg, main="mpg vs displacement", 
       xlab="displacement", ylab="mpg", pch=4)
  abline(my_data.lm2)
  summary(my_data.lm2)
  lm(formula = mpg ~ displacement, data = my_data)
  
  #mpg vs horsepower
  my_data.lm3=lm(mpg~horsepower,data=my_data)
  plot(my_data$horsepower, my_data$mpg, main="mpg vs horsepower", 
       xlab="horsepower", ylab="mpg", pch=4)
  abline(my_data.lm3)
  summary(my_data.lm3)
  
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
  attributes_list
  lst
  index=which(lst==max(lst))
  attributes_list=c(colnames(my_data))
  
  attributes_list[index+1]
  
  
########################################################################################  
#q 1b
  setwd('C:/Users/Lalima Sharda/Desktop/MCS Semester I/FDS/HW23R_Updated/HW23R/data')
  
  path=('../data/hw23R-linear.txt')
    my_data2 <- read.csv(path)  
    my_data2
    # fill in the list with the coeffes you compute
    fit <- lm(mpg~., data=my_data2)
    summary(fit)
    
    summary(fit)$r.squared
    cf <- c(coef(fit))
    cf[1]
    ## sign check to avoid having plus followed by minus for negative coefficients
    eq <- paste0("mpg = ", cf[1],
                 ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " cyl ",
                 ifelse(sign(cf[3])==1, " + ", " - "), abs(cf[3]), " disp",
                 ifelse(sign(cf[4])==1, " + ", " - "), abs(cf[4]), " hp",
                 ifelse(sign(cf[5])==1, " + ", " - "), abs(cf[5]), " wgt",
                 ifelse(sign(cf[6])==1, " + ", " - "), abs(cf[6]), " acln",
                 ifelse(sign(cf[7])==1, " + ", " - "), abs(cf[7]), " year",
                 ifelse(sign(cf[8])==1, " + ", " - "), abs(cf[8]), " origin")
    
    eq
######################################################################################    
#q 2    
    setwd('C:/Users/Lalima Sharda/Desktop/MCS Semester I/FDS/HW23R_Updated/HW23R/data')
    
    (path='../data/hw23R-logistic.txt')
      my_data3=read.csv(path)
      my_data3
      #str(my_data3)
     
      set.seed(123)
      train <- my_data3[1:1000,]
      test <- my_data3[1001:1250,]
      
      model <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5+ Volume+Today,family=binomial(link='logit'),data=train)
      summary(model)
      
      fitted.results <- predict(model,test,type='response')
      fitted.results <- ifelse(fitted.results > 0.5,"Up","Down")
      
      misClasificError <- mean(fitted.results != test$Direction)
      print(paste('Accuracy = ',1-misClasificError))
      
      
#####################################################################################      
#q 3
            path='../data/hw23R-logistic.txt'
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
        
        
#######################################################################################################        
#q 4a,b
        
        # Check to see if packages are installed, if not install.
        inst_pkgs = load_pkgs =  c("ff","ffbase","biglm")
        inst_pkgs = inst_pkgs[!(inst_pkgs %in% installed.packages()[,"Package"])]
        if(length(inst_pkgs)) install.packages(inst_pkgs)
        
        # Dynamically load packages
        pkgs_loaded = lapply(load_pkgs, require, character.only=T)
        
        
        path='../data/slr-90m-data.csv'
        require(biglm)
        my_data3 <- read.csv.ffdf(file = path, header = TRUE, VERBOSE = TRUE, 
                               first.rows =50000,next.rows = 50000, colClasses=NA)
        model3=bigglm(y~x, data = my_data3, fammily=gaussian())
        
        plot(my_data3$x, my_data3$y, main="y vs x", 
             xlab="x", ylab="y", pch=4)
        abline(model3)
        summary(model3)
        cf=coef(model3)
        
        set.seed(123)
        rows_data<-nrow(my_data3)
        data_1<-sample(1:rows_data,size=0.01*rows_data,replace=FALSE)
        fortify(data_1)
        
        data_2<-sample(1:rows_data,size=0.02*rows_data,replace=FALSE)
        fortify(data_2)
        
        data_3<-sample(1:rows_data,size=0.03*rows_data,replace=FALSE)
        fortify(data_3)
        
        data_4<-sample(1:rows_data,size=0.04*rows_data,replace=FALSE)
        fortify(data_4)
        
        data_5<-sample(1:rows_data,size=0.05*rows_data,replace=FALSE)
        fortify(data_5)
        
        path='../data/slr-90m-data.csv'
        library(ggplot2)
        ggplot(data_5) + 
          geom_jitter(aes(data_1$x,data_1$y), colour="blue") + geom_smooth(aes(data_1$x,data_1$y), method=lm, se=FALSE) +
          geom_jitter(aes(data_2$x,data_2$y), colour="green") + geom_smooth(aes(data_2$x,data_2$y), method=lm, se=FALSE) +
          geom_jitter(aes(data_3$x,data_3$y), colour="red") + geom_smooth(aes(data_3$x,data_3$y), method=lm, se=FALSE) +
          labs(x = "Percentage cover (%)", y = "Number of individuals (N)")
        
        
        getwd()=='C:/Users/Lalima Sharda/Desktop/MCS Semester I/FDS/HW23R_Updated/HW23R/data'
        path='../data/slr-90m-data.csv'
        require(biglm)
        library(data.table)
        mydata_31<-fread(path)
        model3=bigglm(y~x, data = mydata_31, fammily=gaussian())
        
        plot(mydata_31$x, mydata_31$y, main="y vs x", 
             xlab="x", ylab="y", pch=4)
        abline(model3)
        summary(model3)
        cf=coef(model3)
        set.seed(123)
        rows_data<-nrow(mydata_31)
        data_1<-mydata_31[sample(1:rows_data,size=0.01*rows_data,replace=FALSE)]
        
        data_2<-mydata_31[sample(1:rows_data,size=0.02*rows_data,replace=FALSE)]
        
        data_3<-mydata_31[sample(1:rows_data,size=0.03*rows_data,replace=FALSE)]
        
        data_4<-mydata_31[sample(1:rows_data,size=0.04*rows_data,replace=FALSE)]
        
        data_5<-mydata_31[sample(1:rows_data,size=0.05*rows_data,replace=FALSE)]
        
        data_1
        data_2
        data_3
        data_4
        data_5
        
        data_combo <- rbind(data_1,data_2,data_3,data_4,data_5)
        data_combo
        
        
        model_1=bigglm(y~x, data = data_1, fammily=gaussian())
        abline(model_1)
        
        model_2=bigglm(y~x, data = data_2, fammily=gaussian())
        abline(model_2)
        
        model_3=bigglm(y~x, data = data_3, fammily=gaussian())
        abline(model_3)
        
        model_4=bigglm(y~x, data = data_4, fammily=gaussian())
        abline(model_4)
        
        model_5=bigglm(y~x, data = data_5, fammily=gaussian())
        abline(model_5)
        
###########################################################################
        # Check to see if packages are installed, if not install.
        inst_pkgs = load_pkgs =  c("ff","ffbase","biglm")
        inst_pkgs = inst_pkgs[!(inst_pkgs %in% installed.packages()[,"Package"])]
        if(length(inst_pkgs)) install.packages(inst_pkgs)
        
        # Dynamically load packages
        pkgs_loaded = lapply(load_pkgs, require, character.only=T)
        
        path='../data/slr-90m-data.csv'
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
##########################################################################        

                # populationDistribution: string('uniform','normal')
        # sampleSize: integer (~30)
        # numberOfSamples: integer (>100)
        # return: list of two variables, mean and se
        populationDistribution="uniform"
        sampleSize=50
        numberOfSamples=200
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
        


        if(populationDistribution=='poisson')
        {
          set.seed(47)
          lambda<-5
          n<-sampleSize
          rows<-numberofSamples
          
          sim<-rpois(n*rows,lambda)
          m<-matrix(sim,rows)
          m
          sample.means<-rowMeans(m)
          sm.average<-mean(sample.means)
          sm.sd<-sd(sample.means)
          sm.sd.clt<-sqrt(lambda/n)#expected standard deviation
          
          runif(10, min=1, max=3) 
          plot( dpois(m, lambda),type="h")
          require(ggplot2)
          x<-seq(m)
          x
          plot(dpois(x,lambda),type="h")
        }
        
      
        
        
    
        
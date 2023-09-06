library(tidyverse)

#load data
cars=read.csv("Cars.csv", header = TRUE)  # read dataset
# Create a combined mpg variable
MPG_Combo <- 0.6*cars$MPG_City+0.4*cars$MPG_Highway
#Turn into database
cars=data.frame(cars,MPG_Combo)

boxplot(cars$MPG_Combo,
        main = 'Combined MPG',
        ylab = 'MPG')

boxplot(MPG_Combo ~ Type, 
        data = cars, 
        main = 'Combined Miles per Galon by Car Type',
        xlab = 'type',
        ylab = 'Combined MPG')

summary(cars$Horsepower)

boxplot(cars$Horsepower)

qqnorm(cars$Horsepower, main = 'HP qqplot', ylab='Horsepower'); qqline(cars$Horsepower,col =2)

shapiro.test(cars$Horsepower)


par(mfrow = c(1,3))
Sports = filter(cars, Type == 'Sports')
SUV = filter(cars, Type == 'SUV')
Truck = filter(cars, Type == 'Truck')

## Visual
qqnorm(Sports$Horsepower, main = 'Sports HP', ylab='Horsepower'); 
qqline(Sports$Horsepower,col =2)

qqnorm(SUV$Horsepower, main = 'SUV HP', ylab=''); 
qqline(SUV$Horsepower, col =2)

qqnorm(Truck$Horsepower, main = 'Truck HP', ylab=''); 
qqline(Truck$Horsepower, col =2)

shapiro.test(Sports$Horsepower)
shapiro.test(SUV$Horsepower)
shapiro.test(Truck$Horsepower)


shapiro.test(SUV$Horsepower)

shapiro.test(Truck$Horsepower)



wilcox.test(SUV$Horsepower,
            Truck$Horsepower,
            alternative = 'two.sided')



July = filter(airquality, Month == '7')
August = filter(airquality, Month == '8')



par(mfrow = c(1,2))

## Visual
qqnorm(July$Wind, main = 'July', ylab='Wind'); 
qqline(July$Wind,col =2)

qqnorm(August$Wind, main = 'August', ylab=''); 
qqline(August$Wind, col =2)



# Remember:
#     H0:   Normally distributed
#     Halt: NOT Normally Distributed
shapiro.test(July$Wind)
shapiro.test(August$Wind)


var.test(July$Wind, August$Wind, alternative = "two.sided")


t.test(July$Wind, August$Wind, var.equal=T, alternative = 'two.sided')












install.packages("caret")
install.packages("datatable")
install.packages("mltools")
install.packages("Hmisc")
install.packages("ggplot2")
library(caret)
library(data.table)
library(mltools)
library(Hmisc)
library(ggplot2)
ams572_2 <- read.csv("./Desktop/AMS 572 FINAL CODE 2.csv", header =TRUE)
data <- ams572_2
data$Wage1<-(log(data$Wage))


#### Check for normal distribution for wage
qqnorm(data$Wage1)
par(mfrow=c(2,2))
###plot for normal QQ looks fairly straight so assume normality is met
plot(lm(data$Wage1~data$Club))


#### Mean wage of Man. Utd vs Man. City using T-test
man_utd <- data$Wage1[data$Club == "Manchester United"]
man_city <- data$Wage1[data$Club == "Manchester City"]

#Equal Variances bc P-value is greater than alpha level
var.test(man_utd,man_city)


##fail to reject null hypothesis 
t.test(x=man_utd, y=man_city,alternative = "greater", conf.level = 0.1)

#### Mean wage of Man. Utd vs all using T-test
man_utd <- data$Wage1[data$Club == "Manchester United"]
all_clubs <- data$Wage1[data$Club != "Manchester United"]


#Equal Variances bc P-value is greater than alpha level
var.test(man_utd,all_clubs)


##fail to reject null hypothesis 
t.test(x=man_utd, y=all_clubs,alternative = "greater", conf.level = 0.1)

##Extra information about the data
mean(data$Wage[data$Club == "Manchester United"])
sd(data$Wage[data$Club == "Manchester United"])
min(data$Wage[data$Club == "Manchester United"])
max(data$Wage[data$Club == "Manchester United"])

mean(data$Wage[data$Club == "Manchester City"])
sd(data$Wage[data$Club == "Manchester City"])
min(data$Wage[data$Club == "Manchester City"])
max(data$Wage[data$Club == "Manchester City"])

mean(data$Wage[data$Club != "Manchester United"])
sd(data$Wage[data$Club != "Manchester United"])
min(data$Wage[data$Club != "Manchester United"])
max(data$Wage[data$Club != "Manchester United"])

###################################################################################
#### chi sq test for zones vs the position
Position<- data$New_Position
Zone<- data$Zone

Zone1<- Zone[Zone== 1]
Zone2<- Zone[Zone== 2]
  
Z1<- Position[Zone== 1]
(length(Z1[Z1== "Midfield"])/length(Z1)) #percentage of Midfield from Z1
(length(Z1[Z1== "Defense"])/length(Z1)) #percentage of Defense from Z1
(length(Z1[Z1== "Forward"])/length(Z1)) #percentage of Forward from Z1
(length(Z1[Z1== "Keeper"])/length(Z1)) #percentage of Keeper from Z1


Z2<- Position[Zone== 2]
(length(Z2[Z2== "Midfield"])/length(Z2)) #percentage of Midfield from Z2
(length(Z2[Z2== "Defense"])/length(Z2)) #percentage of Defense from Z2
(length(Z2[Z2== "Forward"])/length(Z2)) #percentage of Forward from Z2
(length(Z2[Z2== "Keeper"])/length(Z2)) #percentage of Keeper from Z2


chisq.test(Zone,Position)

###################################################################################

#ANOVA test for mean weight for 4 clubs
WeightA<- data$Weight[data$Club== c("Arsenal")]#weights for Arsenal players
WeightC<- data$Weight[data$Club== c("Chelsea")]  #weights for Chelsea players
WeightB<- data$Weight[data$Club== c("FC Barcelona")] #weights for FC Barcelona players
WeightJ<- data$Weight[data$Club== c("Juventus")] #weights for Juventus players

##checking normality
plot(lm(data$Weight~data$Club== c("Arsenal")))
plot(lm(data$Weight~data$Club== c("Chelsea")))
plot(lm(data$Weight~data$Club== c("FC Barcelona")))
plot(lm(data$Weight~data$Club== c("Juventus")))


mean_amt <- c(mean(WeightA),mean(WeightB),mean(WeightC),mean(WeightJ))
Club_name <- c('Arsenal','Chelsea', 'FC Barcelona',	'Juventus')
plot(mean_amt, xlab = "Club", ylab = "Mean amount",  xaxt='n', ann=FALSE)
axis(1, at= 1:4, labels=Club_name)

A<- length(WeightA) #how many players for Arsenal
C<- length(WeightC) #how many players for Chelsea
B<- length(WeightB) #how many players for FC Barcelona
J<- length(WeightJ) #how many players for Juventus

Weights<- c(WeightA, WeightC, WeightB, WeightJ)
Club_names <- rep(c('Arsenal','Chelsea', 'FC Barcelona','Juventus'),c(A,C,B,J))

fitAOV<- aov(Weights~Club_names)
summary(fitAOV)

###################################################################################
attach(data)
Zone[which(Zone==2)]= "Europe"
Zone[which(Zone==1)] = "Americas"
Zone.new = Zone[which(Zone!=3)]
preferred.foot.new = Preferred.Foot[which(Zone!=3)]
wage.new = Wage[which(Zone!=3)]

fit1<- glm(wage.new~Zone.new + preferred.foot.new)
summary(fit1)

#ZONE IS SIG ON WAGE BUT PREF FOOT ISNT


#### effect of Missing value mean imputation
Wage.Missing = data$Wage.Missing
Wage.Missing = impute(Wage.Missing, mean)
Wage.Missing = Wage.Missing[which(Zone!=3)]

fit2 <- glm(Wage.Missing~Zone.new + preferred.foot.new)
summary(fit2)


#### effect of Missing value median imputation
Wage.Missing = data$Wage.Missing
Wage.Missing = impute(Wage.Missing,median)
Wage.Missing = Wage.Missing[which(Zone!=3)]

fit3 <- glm(Wage.Missing~Zone.new+preferred.foot.new)
summary(fit3)





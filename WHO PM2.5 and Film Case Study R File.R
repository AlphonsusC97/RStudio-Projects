#read table
pm25  = read.csv ("pm25.csv", header =TRUE)
movie  = read.csv ("movie.csv", header =TRUE)

head (pm25)
par(mfrow = c(2,2))
#scatterplot with smoother
pairs(pm25, panel = panel.smooth)
pm25
#correlation matrix
cor(pm25)

pm25.lm = lm(pm25~temperature+humidity+wind+precipitation, data = pm25)
summary(pm25.lm)

summary(pm25.lm)$coefficients
#Quantile Calculation
qt((1-0.05/2), 51, lower.tail = FALSE)

qt(0.05/2, 51, lower.tail = FALSE)
#Confidence Interval
-1.27742262 + 2.007584 * 0.11854373
-1.27742262 - 2.007584 * 0.11854373

coefficients(pm25.lm)
#Mathematical Multiple Regression Model
#pm25 = 102.72258771 + -1.27742262 + -0.58015926 +  -0.01090918
anova(pm25.lm)

#Full Regression SS = 9014.4 + 12739.7 + 622.6 + 21.8 = 22398.5
#Regression MS = Regression SS / k = 22398.5 /4 = 5599.625
#F Stat = Reg MS / Res MS = 5599.625 / 101.2  = 55.33226

#P Value = 6.079192e-18
pf(55.33226 ,df1= 4,df2 = 51, lower.tail =FALSE) 

#validate Full Model
par(mfrow = c(1,2))
plot(pm25.lm , which = 1:2)

par(mfrow = c(2,2))
plot(pm25$humidity, pm25.lm$residuals, main = "Residuals vs Humidity",
     xlab = "Humidity", ylab = "Residuals")
plot(pm25$temperature, pm25.lm$residuals, main = "Residuals vs Humidity",
     xlab = "Humidity", ylab = "Residuals")
plot(pm25$wind, pm25.lm$residuals, main = "Residuals vs Humidity",
     xlab = "Humidity", ylab = "Residuals")
plot(pm25$precipitation, pm25.lm$residuals, main = "Residuals vs Humidity",
     xlab = "Humidity", ylab = "Residuals")

#Find R Squared = (22398.5+5160.6)-5160.6/(22398.5+5160.6) 
#R Squared = 22398.5/27559.1= 0.8127448
summary(pm25.lm)$r.squared

#Part F
pm25.lm2 = lm (pm25~temperature+humidity+wind, data = pm25)
summary(pm25.lm2)

pm25.lm3 = lm (pm25~temperature+humidity, data = pm25)
summary(pm25.lm3)
#Final fitted regression model
coefficients(pm25.lm2)
#pm25hat = 97.09977

#Q2
head(movie)

table(movie[,1:2])
 
#Part B
par(mfrow =c(1,2))
with(movie, interaction.plot(Genre, Gender, Score, col = 1:2))
with(movie, interaction.plot(Gender, Genre, Score, col = 1:3))
boxplot(Score~Genre+Gender , data =movie)

#Part C

#Part D
movie.lm = lm (Score~Gender * Genre ,  data= movie)
anova(movie.lm)

movie.lm2 = lm (Score ~Gender + Genre,  data= movie)
anova(movie.lm2)

plot(movie.lm, which =1:2)
hist(resid(movie.lm))

plot(movie.lm2, which =1:2)
hist(resid(movie.lm2))


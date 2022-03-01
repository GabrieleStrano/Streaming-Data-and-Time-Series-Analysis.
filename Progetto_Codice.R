setwd("C:/Users/Gabriele/Desktop/UniMiB/MaterieNuove/StreamingData&TimeSeries/Pelagatti/Progetto")
library(readr)
library(xts)
library(zoo)
library(forecast)
library(ggplot2)
library(olsrr)
library(dplyr)

dataset <- read_csv("Project_data_2021_2022 (TRAINSET).csv", col_types = cols(Date = col_character()))
dataset$nuovo2 <- paste(dataset$Date, dataset$Hour)
dataset$nuovo2 <- as.POSIXct(dataset$nuovo2, tz = "GMT", format = "%Y-%m-%d %H")

##
#importo ts
firstHour <- 24*(as.Date("2004-03-10 18:00:00") - as.Date("2004-01-01 00:00:00"))
firstHour-6

lastHour <- 24*(as.Date("2005-02-28 23:00:00") - as.Date("2005-01-01 00:00:00"))
lastHour


ts3 <- ts(dataset$CO, as.Date(dataset$nuovo2, "%Y-%m-%d %H:%M:%S", tz= "CET"), frequency= 8760, start = c(2004,firstHour-6), end = c(2005, lastHour+23))
plot(ts3)


y <- msts(ts3, start = c(2004,firstHour-6), end = c(2005, lastHour+23), seasonal.periods=c(24,168,8760))
autoplot(y_f)



# creo sinusoidi da usare nell'arimax
omega <- outer(1:length(ts3), 1:4) * 2 * pi / 168
cc <- cos(omega)
ss <- sin(omega)



##
null_value<- subset.data.frame(dataset, is.na(dataset$CO))
#1° step null values

train <- window(y, end = c(2004, 2173)) #524
train1 <- msts(train, end = c(2004, 2173), seasonal.periods=c(24,168), ts.frequency = 8760)

test <- window(y, start = c(2004, 2177), end = c(2004, 2200)) #528-551
test1 <- msts(test, start = c(2004, 2177), end = c(2004, 2200), seasonal.periods=c(24,168), ts.frequency = 8760)

BoxCox(train1, lambda = "auto") #non è necessaria trasformazione


mod1 <- Arima(train1, c(2, 1, 1), 
              list(order = c(0, 1, 1), period = 24),
              xreg = cbind(cc, ss)[1:524,])
summary(mod1)

res1 <- mod1$residuals

par(mfrow=c(1,2))
Acf(res1, lag.max = 240)
Pacf(res1, lag.max = 240)

Acf(res1, lag.max = 900)
Pacf(res1, lag.max = 900)

par(mfrow=c(1,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
hist(res1)


# FORECAST
frc1 <- forecast(mod1, 27, xreg = cbind(cc, ss)[525:551,])
frc_new1 <- frc1$mean
frc_new1

err1 <- test1[1:24] - frc_new1[4:27]
mape1 <- mean(abs(err1)/test1[1:24]*100)
mape1 #7.636472

dataset2 <- dataset
dataset2[525:527,3] <- frc_new1[1:3]

## 2°

days <- read_csv("C:/Users/Gabriele/Downloads/days.csv")
days$...1 <- NULL
days$nuovo2 <- paste(days$Date, days$Hour)
days$nuovo2 <- as.POSIXct(days$nuovo2, tz = "GMT", format = "%Y-%m-%d %H")
View(days)
#######################################################################
library(tidyverse)
library(lubridate)

days$Month <- lubridate::month(ymd(days$Date))

######################################################################
#creiamo dummies mensili
dummies12 <- with(days, cbind(Month == 1,
                              Month == 2,
                              Month == 3,
                              Month == 4,
                              Month == 5,
                              Month == 6,
                              Month == 7,
                              Month == 8,
                              Month == 9,
                              Month == 10,
                              Month == 11))
ddummies12 <- matrix(as.integer(dummies12), ncol = 11)
colnames(ddummies12) <- paste0("month", c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))


#creiamo dummies giornaliere
dummies <- with(days, cbind(DayOfWeek == 1,
                            DayOfWeek == 2,
                            DayOfWeek == 3,
                            DayOfWeek == 4,
                            DayOfWeek == 5,
                            DayOfWeek == 6))
ddummies <- matrix(as.integer(dummies), ncol = 6)
colnames(ddummies) <- paste0("weekday", c(1, 2, 3, 4, 5, 6))


#creiamo dummies orarie
dummies24 <- with(days, cbind(Hour == 0,
                              Hour == 1,
                              Hour == 2,
                              Hour == 3,
                              Hour == 4,
                              Hour == 5,
                              Hour == 6,
                              Hour == 7,
                              Hour == 8,
                              Hour == 9,
                              Hour == 10,
                              Hour == 11,
                              Hour == 12,
                              Hour == 13,
                              Hour == 14,
                              Hour == 15,
                              Hour == 16,
                              Hour == 17,
                              Hour == 18,
                              Hour == 19,
                              Hour == 20,
                              Hour == 21,
                              Hour == 22))
ddummies24 <- matrix(as.integer(dummies24), ncol = 23)
colnames(ddummies24) <- paste0("Hour", c(0,1, 2, 3, 4, 5, 6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22))

autoplot(ddummies)

train <- window(y, end = c(2004, 2350)) #701
train2 <- msts(train, end = c(2004, 2350), seasonal.periods=c(24,168), ts.frequency = 8760)

test <- window(y, start = c(2004, 2375), end = c(2004, 2398)) #726-749
test2 <- msts(test, start = c(2004, 2375), end = c(2004, 2398), seasonal.periods=c(24,168), ts.frequency = 8760)

BoxCox(train2, lambda = "auto") #non sembra necessaria trasformazione

auto.arima(train2, xreg = cbind(ddummies24, ddummies)[1:701,])

mod2 <- Arima(train2, c(2, 1, 1), 
              list(order = c(1, 0, 0), period = 24),
              xreg = ddummies[1:701,])
summary(mod2)


res1 <- mod2$residuals

par(mfrow=c(1,2))
Acf(res1, lag.max = 240)
Pacf(res1, lag.max = 240)

Acf(res1, lag.max = 900)
Pacf(res1, lag.max = 900)

par(mfrow=c(1,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
hist(res1)


# FORECAST
frc2 <- forecast(mod2, 48, xreg = ddummies[702:749,])
frc_new2 <- frc2$mean
frc_new2

err2 <- test2[1:24] - frc_new2[25:48]
mape2 <- mean(abs(err2)/test2[1:24]*100)
mape2 #11.97453


dataset2[702:725,3] <- frc_new2[1:24]

##
#3°

train <- window(y, end = c(2004, 3474)) #1825
train3 <- msts(train, end = c(2004, 3474), seasonal.periods=c(24,168), ts.frequency = 8760)


test <- window(y, start = c(2004, 3489), end = c(2004, 3512)) #1840-1863
test3 <- msts(test, start = c(2004, 3489), end = c(2004, 3512), seasonal.periods=c(24,168), ts.frequency = 8760)

BoxCox(train3, lambda = "auto") 


mod3 <- Arima(train3, c(2, 1, 1), 
              list(order = c(0, 1, 1), period = 24),
              xreg = cbind(cc, ss)[1:1825,])
summary(mod3)

res1 <- mod3$residuals

par(mfrow=c(1,2))
Acf(res1, lag.max = 240)
Pacf(res1, lag.max = 240)

Acf(res1, lag.max = 900)
Pacf(res1, lag.max = 900)

par(mfrow=c(1,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
hist(res1)


# FORECAST
frc3 <- forecast(mod3, 27, xreg = cbind(cc, ss)[1826:1863,]) 
frc_new3 <- frc3$mean
frc_new3

err3 <- test3[1:24] - frc_new3[15:38]
mape3 <- mean(abs(err3)/test3[1:24]*100)
mape3 #5.627172


dataset2[1826:1839,3] <- frc_new3[1:14]

###
#4°

train <- window(y, end = c(2004, 4069)) #2420
train4 <- msts(train, end = c(2004, 4069), seasonal.periods=c(24,168), ts.frequency = 8760)


test <- window(y, start = c(2004, 4108), end = c(2004, 4131)) #2459-2482
test4 <- msts(test, start = c(2004, 4108), end = c(2004, 4131), seasonal.periods=c(24,168), ts.frequency = 8760)

BoxCox(train4, lambda = "auto") 


mod4 <- Arima(train4, c(2, 1, 1), 
              list(order = c(0, 1, 1), period = 24),
              xreg = cbind(cc, ss)[1:2420,])
summary(mod4)

res1 <- mod4$residuals

par(mfrow=c(1,2))
Acf(res1, lag.max = 240)
Pacf(res1, lag.max = 240)

Acf(res1, lag.max = 900)
Pacf(res1, lag.max = 900)

par(mfrow=c(1,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
hist(res1)


# FORECAST
frc4 <- forecast(mod4, 62, xreg = cbind(cc, ss)[2421:2482,])
frc_new4 <- frc4$mean
frc_new4

err4 <- test4[1:24] - frc_new4[39:62]
mape4 <- mean(abs(err4)/test4[1:24]*100)
mape4 #6.968226


dataset2[2421:2458,3] <- frc_new4[1:38]

##
#5°
train <- window(y, end = c(2004, 5063)) #3414
train5 <- msts(train, end = c(2004, 5063), seasonal.periods=c(24,168), ts.frequency = 8760)

test <- window(y, start = c(2004, 5065), end = c(2004, 5088)) #3416-3439
test5 <- msts(test, start = c(2004, 5065), end = c(2004, 5088), seasonal.periods=c(24,168), ts.frequency = 8760)

mod5 <- Arima(train5, c(2, 1, 1), 
              list(order = c(0, 1, 1), period = 24),
              xreg = cbind(cc, ss)[1:3414,])
summary(mod5)

res1 <- mod5$residuals

par(mfrow=c(1,2))
Acf(res1, lag.max = 240)
Pacf(res1, lag.max = 240)

Acf(res1, lag.max = 900)
Pacf(res1, lag.max = 900)

par(mfrow=c(1,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
hist(res1)


# FORECAST
frc5 <- forecast(mod5, 25, xreg = cbind(cc, ss)[3415:3439,])
frc_new5 <- frc5$mean
frc_new5

err5 <- test5[1:24] - frc_new5[2:25]
mape5 <- mean(abs(err5)/test5[1:24]*100)
mape5 #8.111334

dataset2[3415,3] <- frc_new5[1]

##
# aggiorniamo null_value
null_value<- subset.data.frame(dataset2, is.na(dataset2$CO))


#6°
train <- window(y, end = c(2004, 5295)) #3646
train6 <- msts(train, end = c(2004, 5295), seasonal.periods=c(24,168), ts.frequency = 8760)


test <- window(y, start = c(2004, 5300), end = c(2004, 5323)) #3651-3674
test6 <- msts(test, start = c(2004, 5300), end = c(2004, 5323), seasonal.periods=c(24,168), ts.frequency = 8760)

BoxCox(train6, lambda = "auto") 



mod6 <- Arima(train6, c(2, 1, 1), 
              list(order = c(0, 1, 1), period = 24),
              xreg = cbind(cc,ss)[1:3646,])
summary(mod6)

res1 <- mod6$residuals

par(mfrow=c(1,2))
Acf(res1, lag.max = 240)
Pacf(res1, lag.max = 240)

Acf(res1, lag.max = 900)
Pacf(res1, lag.max = 900)

par(mfrow=c(1,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
hist(res1)


# FORECAST
frc6 <- forecast(mod6, 28, xreg = cbind(cc,ss)[3647:3674,])
frc_new6 <- frc6$mean
frc_new6

err6 <- test6[1:24] - frc_new6[5:28]
mape6 <- mean(abs(err6)/test6[1:24]*100)
mape6 #3.723082 


dataset2[3647:3650,3] <- frc_new6[1:4]

##
#7°

train <- window(y, end = c(2004, 5693)) #4044
train7 <- msts(train, end = c(2004, 5693), seasonal.periods=c(24,168), ts.frequency = 8760)


test <- window(y, start = c(2004, 5739), end = c(2004, 5762)) #4090-4113
test7 <- msts(test, start = c(2004, 5739), end = c(2004, 5762), seasonal.periods=c(24,168), ts.frequency = 8760)


mod7 <- Arima(train7, c(2, 1, 1), 
              list(order = c(0, 1, 1), period = 24),
              xreg = ddummies[1:4044,])
summary(mod7)

res1 <- mod7$residuals

par(mfrow=c(1,2))
Acf(res1, lag.max = 240)
Pacf(res1, lag.max = 240)

Acf(res1, lag.max = 900)
Pacf(res1, lag.max = 900)

par(mfrow=c(1,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
hist(res1)


# FORECAST
frc7 <- forecast(mod7, 69, xreg = ddummies[4045:4113,])
frc_new7 <- frc7$mean
frc_new7

err7 <- test7[1:24] - frc_new7[46:69]
mape7 <- mean(abs(err7)/test7[1:24]*100)
mape7 #8.076678


dataset2[4045:4089,3] <- frc_new7[1:45]

##
#8°

train <- window(y, end = c(2004, 5998)) #4349
train8 <- msts(train, end = c(2004, 5998), seasonal.periods=c(24,168), ts.frequency = 8760)


test <- window(y, start = c(2004, 6018), end = c(2004, 6041)) #4369-4392
test8 <- msts(test, start = c(2004, 6018), end = c(2004, 6041), seasonal.periods=c(24,168), ts.frequency = 8760)


mod8 <- Arima(train8, c(2, 1, 1), 
              list(order = c(0, 1, 1), period = 24),
              xreg = cbind(cc,ss)[1:4349,])

summary(mod8)

res1 <- mod8$residuals

par(mfrow=c(1,2))
Acf(res1, lag.max = 240)
Pacf(res1, lag.max = 240)

Acf(res1, lag.max = 900)
Pacf(res1, lag.max = 900)

par(mfrow=c(1,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
hist(res1)


# FORECAST
frc8 <- forecast(mod8, 43, xreg = cbind(cc,ss)[4350:4392,])
frc_new8 <- frc8$mean
frc_new8


# grafico delle previsioni (zoom)
autoplot(frc_new8, ts.colour = 'black', size = 0.5) +
  scale_color_manual(labels = c("Actual", "Forecasted", "Train"),
                     values=c("green", "red", "black")) +
  autolayer(frc_new8, series = 'Forecasted', size = 0.5) +
  autolayer(test8, series='Actual', size = 0.8)


err8 <- test8[1:24] - frc_new8[20:43]
mape8 <- mean(abs(err8)/test8[1:24]*100)
mape8 #9.890763


dataset2[4350:4359,3] <- frc_new8[1:10]
dataset2[4361:4368,3] <- frc_new8[12:19]

##
#9°

train <- window(y, end = c(2004, 6565)) #4916
train9 <- msts(train, end = c(2004, 6565), seasonal.periods=c(24,168), ts.frequency = 8760)


test <- window(y, start = c(2004, 6567), end = c(2004, 6590)) #4918-4941
test9 <- msts(test, start = c(2004, 6567), end = c(2004, 6590), seasonal.periods=c(24,168), ts.frequency = 8760)


mod9 <- Arima(train9, c(2, 1, 1), 
              list(order = c(0, 1, 1), period = 24),
              xreg = cbind(cc,ss)[1:4916,])

summary(mod9)

res1 <- mod9$residuals

par(mfrow=c(1,2))
Acf(res1, lag.max = 240)
Pacf(res1, lag.max = 240)

Acf(res1, lag.max = 900)
Pacf(res1, lag.max = 900)

par(mfrow=c(1,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
hist(res1)


# FORECAST
frc9 <- forecast(mod9, 25, xreg = cbind(cc,ss)[4917:4941,])
frc_new9 <- frc9$mean
frc_new9

err9 <- test9[1:24] - frc_new9[2:25]
mape9 <- mean(abs(err9)/test9[1:24]*100)
mape9 #5.269234


dataset2[4917,3] <- frc_new9[1]

##
#10°

train <- window(y, end = c(2004, 8344)) #6695
train10 <- msts(train, end = c(2004, 8344), seasonal.periods=c(24,168), ts.frequency = 8760)


test <- window(y, start = c(2004, 8420), end = c(2004, 8491)) #6771-6794-6842
test10 <- msts(test, start = c(2004, 8420), end = c(2004, 8491), seasonal.periods=c(24,168), ts.frequency = 8760)


train2_10 <- BoxCox(train10, lambda = -0.7667518)


mod10 <- Arima(train2_10, c(1, 1, 2), 
               list(order = c(1, 0, 0), period = 24),
               xreg = cbind(cc,ss)[1:6695,])

summary(mod10)


res1 <- mod10$residuals

par(mfrow=c(1,2))
Acf(res1, lag.max = 240)
Pacf(res1, lag.max = 240)

Acf(res1, lag.max = 900)
Pacf(res1, lag.max = 900)

par(mfrow=c(1,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
hist(res1)


frc10 <- forecast(mod10, 147, xreg = cbind(cc,ss)[6696:6842,])
frc_new10 <- InvBoxCox(frc10$mean ,lambda = -0.7667518)
frc_new10


autoplot(frc_new10, ts.colour = 'black', size = 0.5) +
  scale_color_manual(labels = c("Actual", "Forecasted", "Train"),
                     values=c("green", "red", "black")) +
  autolayer(frc_new10, series = 'Forecasted', size = 0.5) +
  autolayer(test10, series='Actual', size = 0.8)



err10 <- test10[1:72] - frc_new10[76:147]
mape10 <- mean(abs(err10)/test10[1:72]*100)
mape10 #17.78257 sinusoidi

dataset2[6696:6770,3] <- frc_new10[1:75]

##
#11°

train <- window(y, end = c(2004, 8586)) #6937
train11 <- msts(train, end = c(2004, 8586), seasonal.periods=c(24,168), ts.frequency = 8760)


test <- window(y, start = c(2004, 8592), end = c(2004, 8615)) #6943-6966
test11 <- msts(test, start = c(2004, 8592), end = c(2004, 8615), seasonal.periods=c(24,168), ts.frequency = 8760)


mod11 <- Arima(train11, c(2, 1, 1), 
               list(order = c(0, 1, 1), period = 24),
               xreg = ddummies[1:6937,])

summary(mod11)

res1 <- mod11$residuals

par(mfrow=c(1,2))
Acf(res1, lag.max = 240)
Pacf(res1, lag.max = 240)

Acf(res1, lag.max = 900)
Pacf(res1, lag.max = 900)

par(mfrow=c(1,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
hist(res1)


# FORECAST
frc11 <- forecast(mod11, 29, xreg = ddummies[6938:6966,])
frc_new11 <- frc11$mean
frc_new11

err11 <- test11[1:24] - frc_new11[6:29]
mape11 <- mean(abs(err11)/test11[1:24]*100)
mape11 #7.948962


dataset2[6938:6942,3] <- frc_new11[1:5]

##
#12°

train <- window(y, end = c(2005, 44)) #7155
train12 <- msts(train, end = c(2005, 44), seasonal.periods=c(24,168), ts.frequency = 8760)


test <- window(y, start = c(2005, 97), end = c(2005, 144)) #7208-7255
test12 <- msts(test, start = c(2005, 97), end = c(2005, 144), seasonal.periods=c(24,168), ts.frequency = 8760)


mod12 <- Arima(train12, c(2, 1, 1), 
               list(order = c(0, 1, 1), period = 24),
               xreg = ddummies[1:7155,])

summary(mod12)

res1 <- mod12$residuals

par(mfrow=c(1,2))
Acf(res1, lag.max = 240)
Pacf(res1, lag.max = 240)

Acf(res1, lag.max = 900)
Pacf(res1, lag.max = 900)

par(mfrow=c(1,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
hist(res1)


# FORECAST
frc12 <- forecast(mod12, 100, xreg = ddummies[7156:7255,])
frc_new12 <- frc12$mean
frc_new12

err12 <- test12[1:48] - frc_new12[53:100]
mape12 <- mean(abs(err12)/test12[1:48]*100)
mape12 #9.645901 


dataset2[7156:7207,3] <- frc_new12[1:52]

##
#13°
null_value<- subset.data.frame(dataset2, is.na(dataset2$CO))


train <- window(y, end = c(2005, 664)) #7775
train13 <- msts(train, end = c(2005, 664), seasonal.periods=c(24,168), ts.frequency = 8760)


test <- window(y, start = c(2005, 674), end = c(2005, 697)) #7785-7808
test13 <- msts(test, start = c(2005, 674), end = c(2005, 697), seasonal.periods=c(24,168), ts.frequency = 8760)


mod13 <- Arima(train13, c(2, 1, 1), 
               list(order = c(0, 1, 1), period = 24),
               xreg = ddummies[1:7775,])

summary(mod13)

res1 <- mod13$residuals

par(mfrow=c(1,2))
Acf(res1, lag.max = 240)
Pacf(res1, lag.max = 240)

Acf(res1, lag.max = 900)
Pacf(res1, lag.max = 900)

par(mfrow=c(1,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
hist(res1)


# FORECAST
frc13 <- forecast(mod13, 33, xreg = ddummies[7776:7808,])
frc_new13 <- frc13$mean
frc_new13

err13 <- test13[1:24] - frc_new13[10:33]
mape13 <- mean(abs(err13)/test13[1:24]*100)
mape13 #6.353076


dataset2[7776:7784,3] <- frc_new13[1:9]

##
#14°

train <- window(y, end = c(2005, 928)) #8039
train14 <- msts(train, end = c(2005, 928), seasonal.periods=c(24,168), ts.frequency = 8760)

test <- window(y, start = c(2005, 1005), end = c(2005, 1052)) #8116-8163 
test14 <- msts(test, start = c(2005, 1005), end = c(2005, 1052), seasonal.periods=c(24,168), ts.frequency = 8760)

#
train15 <- msts(train, start = c(2005, 674), end = c(2005, 928), seasonal.periods=c(24,168), ts.frequency = 8760)
plot(train15)


mod14 <- Arima(train15, c(2, 1, 2), 
               list(order = c(1, 0, 0), period = 24),
               xreg = cbind(cc, ss)[7785:8039,])

summary(mod14)


res1 <- mod14$residuals

par(mfrow=c(1,2))
Acf(res1, lag.max = 240)
Pacf(res1, lag.max = 240)

Acf(res1, lag.max = 900)
Pacf(res1, lag.max = 900)

par(mfrow=c(1,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
hist(res1)



# FORECAST
frc14 <- forecast(mod14, 124, xreg = cbind(cc, ss)[8040:8163,])
frc_new14 <- frc14$mean
frc_new14


autoplot(frc_new14, ts.colour = 'black', size = 0.5) +
  scale_color_manual(labels = c("Actual", "Forecasted", "Train"),
                     values=c("green", "red", "black")) +
  autolayer(frc_new14, series = 'Forecasted', size = 0.5) +
  autolayer(test14, series='Actual', size = 0.8)


err14 <- test14[1:48] - frc_new14[77:124]
mape14 <- mean(abs(err14)/test14[1:48]*100)
mape14 #13.93768 

dataset2[8040:8115,3] <- frc_new14[1:76]

##
null_value<- subset.data.frame(dataset2, is.na(dataset2$CO)) #non abbiamo più null value.

#####################################################################################

# adesso possiamo provare la previsione con arima
ts_f <- ts(dataset2$CO, as.Date(dataset2$nuovo2, "%Y-%m-%d %H:%M:%S", tz= "CET"), frequency= 8760, start = c(2004,firstHour-6), end = c(2005, lastHour+23))
plot(ts_f[4000:6500], type='l')

y_f <- msts(ts_f, start = c(2004,firstHour-6), end = c(2005, lastHour+23), seasonal.periods=c(24,168,8760))
plot(y_f)


#assegnamo i primi train e test.
train_f <- window(y_f, end = c(2004, 7439)) #5790
trainf1 <- msts(train_f, end = c(2004, 7439), seasonal.periods=c(24,168), ts.frequency = 8760)

test_f <- window(y_f, start = c(2004, 7440), end = c(2005, 1415)) #5791-8526
testf1 <- msts(test_f, start = c(2004, 7440), end = c(2005, 1415), seasonal.periods=c(24,168), ts.frequency = 8760)

###########################################################################################
##########################################################################################
#iniziamo con la prima previsione
BoxCox(trainf1, lambda= 'auto')
auto.arima(trainf1, xreg = cbind(ddummies24, ddummies)[1:5790,])

par(mfrow=c(1,1))
#cerchiamo di trovare movimenti strani nei dati per capire se alcune previsioni fatte sembrano
#storpiare i dati
plot(trainf1[1:2895], type = 'l')
plot(trainf1[2896:5790], type = 'l')
plot(log(trainf1))
plot(testf1)
plot(diff(trainf1))



omega2 <- outer(1:length(ts_f), 1:8) * 2 * pi / 168 #168 sett, 730 mens.
cc2 <- cos(omega2)
ss2 <- sin(omega2)




#train_f2 <- window(y_f, start = c(2004, 4518), end = c(2004, 7439)) #2869-5790
#trainf12 <- msts(train_f2, start = c(2004, 4518), end = c(2004, 7439), seasonal.periods=c(24,168), ts.frequency = 8760)


modf1 <- Arima(trainf1, c(1, 1, 2), 
               list(order = c(1, 0, 1), period = 24),
               xreg = cbind(cc2, ss2)[1:5790,],
               #method = 'CSS',
               include.drift = TRUE)

summary(modf1) 


res1 <- modf1$residuals

par(mfrow=c(1,2))
Acf(res1, lag.max = 240)
Pacf(res1, lag.max = 240)

Acf(res1, lag.max = 900)
Pacf(res1, lag.max = 900)

par(mfrow=c(1,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
hist(res1)



# FORECAST
frcf1 <- forecast(modf1, 2736, xreg= cbind(cc2, ss2)[5791:8526,]) #xreg = cbind(cc2, ss2)
frc_newf1 <- frcf1$mean
frc_newf1


# grafico delle previsioni (zoom)
autoplot(frc_newf1, ts.colour = 'black', size = 0.5) +
  scale_color_manual(labels = c("Actual", "Forecasted", "Train"),
                     values=c("green", "red", "black")) +
  autolayer(frc_newf1, series = 'Forecasted', size = 0.5) +
  autolayer(testf1, series='Actual', size = 0.8)

autoplot(frcf1, ts.colour = 'black', size = 0.5) +
  scale_color_manual(labels = c("Actual", "Forecasted", "Train"),
                     values=c("green", "red", "black")) +
  autolayer(frcf1, series = 'Forecasted', size = 0.5) +
  autolayer(testf1, series='Actual', size = 0.8)


# grafico della serie storica + le previsioni (totale)
autoplot(frc_newf1, ts.colour = 'black', size = 0.5) +
  scale_color_manual(labels = c("Actual", "Forecasted", "Train"),
                     values=c("green", "red", "black")) +
  autolayer(frc_newf1, series = 'Forecasted', size = 0.5) +
  autolayer(testf1, series='Actual', size = 0.5) +
  autolayer(trainf1, series='Train', size = 0.5)


# test su circa 4 mesi.
errf1 <- testf1[1:2736] - frc_newf1[1:2736]
mapef1 <- mean(abs(errf1)/testf1[1:2736]*100)
mapef1


# MAPE=15.9103, (2,0,2)(1,0,0) cc+ss 2*16 AiCc=33504.11   
# MAPE=15.3632, (1,1,2)(1,0,1) cc+ss 2*16 AICc=33078.78 + drift 
# MAPE=17.62285,(1,1,2)(1,0,1) cc+ss 2*16 AiCc=33076.73 probabile arima finale




##########################################################################################
#assegnamo i nuovi train e test.(1 mese per volta) #prevediamo dicembre.
train_d <- window(y_f, end = c(2004, 8015)) #6366
traind <- msts(train_d, end = c(2004, 8015), seasonal.periods=c(24,168), ts.frequency = 8760)

test_d <- window(y_f, start = c(2004, 8016), end = c(2004, 8759))  #6367-7110
testd <- msts(test_d, start = c(2004, 8016), end = c(2004, 8759), seasonal.periods=c(24,168), ts.frequency = 8760)


install.packages("beepr") #avvisa a fine calcolo
library(beepr)
beep(2)


auto.arima(traind, xreg = reg[1:6366,]) 

modd <- Arima(traind, c(1, 1, 2), 
              list(order = c(1, 0, 0), period = 24),
              xreg = reg[1:6366,])
              #xreg = cbind(cc2, ss2)[1:6366,])

summary(modd)
#(1,1,2)(1,0,1) #73639.44 e 14.12
#(3,1,1)(1,0,1) #73643.15 e 14.73
#(3,1,1)(1,0,0) #74032.15 e 16.86
#(1,1,2)(1,0,0) #74047.13 e 15.79


res1 <- modd$residuals

par(mfrow=c(1,2))
Acf(res1, lag.max = 240)
Pacf(res1, lag.max = 240)

Acf(res1, lag.max = 900)
Pacf(res1, lag.max = 900)

par(mfrow=c(1,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
hist(res1)


# FORECAST
frcf1 <- forecast(modd, 744, xreg= reg[6367:7110,]) #xreg = cbind(cc2, ss2)
frc_newf1 <- frcf1$mean
frc_newf1



# grafico delle previsioni (zoom)
autoplot(frc_newf1, ts.colour = 'black', size = 0.5) +
  scale_color_manual(labels = c("Actual", "Forecasted", "Train"),
                     values=c("green", "red", "black")) +
  autolayer(frc_newf1, series = 'Forecasted', size = 0.5) +
  autolayer(testd, series='Actual', size = 0.8)




# grafico della serie storica + le previsioni (totale)
autoplot(frc_newf1, ts.colour = 'black', size = 0.5) +
  scale_color_manual(labels = c("Actual", "Forecasted", "Train"),
                     values=c("green", "red", "black")) +
  autolayer(frc_newf1, series = 'Forecasted', size = 0.5) +
  autolayer(testd, series='Actual', size = 0.5) +
  autolayer(traind, series='Train', size = 0.5)


# test sul mese di dicembre.
errd <- testd[1:744] - frc_newf1[1:744]
maped <- mean(abs(errd)/testd[1:744]*100)
maped #17.87928 AiCc = 73294.93


##########################################################################################
##########################################################################################
#assegnamo i nuovi train e test.(1 mese per volta) #prevediamo gennaio.
train_g <- window(y_f, end = c(2004, 8759)) #7110
traing <- msts(train_g, end = c(2004, 8759), seasonal.periods=c(24,168), ts.frequency = 8760)

test_g <- window(y_f, start = c(2004, 8760), end = c(2005, 743))  #7111-7854
testg <- msts(test_g, start = c(2004, 8760), end = c(2005, 743), seasonal.periods=c(24,168), ts.frequency = 8760)


auto.arima(traing, xreg = cbind(cc2, ss2)[1:7110,]) 

modg <- Arima(traing, c(3, 1, 1), #1-1-2 male
              list(order = c(1, 0, 0), period = 24),
              xreg = cbind(cc2, ss2)[1:7110,])
summary(modg)
beep(2)
Sys.time()


# omega3 <- outer(1:length(ts_f), 1:2) * 2 * pi / 24 #168 sett, 730 mens.
# cc3 <- cos(omega3)
# ss3 <- sin(omega3)


auto.arima(traing, xreg= reg[1:7110,])

#utilizziamo fuourier per creare i regressori
reg <- fourier(y_f, K=c(4,2,0))
reg2 <- fourier(y_f, K=c(4,2,0), h=744)
reg3 <- fourier(train_ma, K=c(4,2,0), h=792)


modg <- Arima(traing, c(1, 1, 2), 
              list(order = c(1, 0, 0), period = 24),
              xreg= reg[1:7110,])
summary(modg)
beep(2)


res1 <- modg$residuals

par(mfrow=c(1,2))
Acf(res1, lag.max = 240)
Pacf(res1, lag.max = 240)

Acf(res1, lag.max = 900)
Pacf(res1, lag.max = 900)

par(mfrow=c(1,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
hist(res1)


#(1,1,2)(1,0,0) AICc=82672.31 - 21.55
#(3,1,1)(1,0,0) AICc=82812.38 - 21.55

###################################################

# FORECAST
frcf1 <- forecast(modg, 744, xreg= reg[7111:7854,]) #xreg= cbind(cc2, ss2)[7111:7854,]
frc_newf1 <- frcf1$mean
frc_newf1



# grafico delle previsioni (zoom)
autoplot(frc_newf1, ts.colour = 'black', size = 0.5) +
  scale_color_manual(labels = c("Actual", "Forecasted", "Train"),
                     values=c("green", "red", "black")) +
  autolayer(frc_newf1, series = 'Forecasted', size = 0.5) +
  autolayer(testg, series='Actual', size = 0.8)




# grafico della serie storica + le previsioni (totale)
autoplot(frc_newf1, ts.colour = 'black', size = 0.5) +
  scale_color_manual(labels = c("Actual", "Forecasted", "Train"),
                     values=c("green", "red", "black")) +
  autolayer(frc_newf1, series = 'Forecasted', size = 0.5) +
  autolayer(testg, series='Actual', size = 0.5) +
  autolayer(traing, series='Train', size = 0.5)


# test sul mese di dicembre.
errg <- testg[1:744] - frc_newf1[1:744]
mapeg <- mean(abs(errg)/testg[1:744]*100)
mapeg 

##########################################################################
#ultimo mese febbraio

train_fe <- window(y_f, end = c(2005, 743)) #7854
trainfe <- msts(train_fe, end = c(2005, 743), seasonal.periods=c(24,168), ts.frequency = 8760)

test_fe <- window(y_f, start = c(2005, 744), end = c(2005, 1415))  #7855-8526
testfe <- msts(test_fe, start = c(2005, 744), end = c(2005, 1415), seasonal.periods=c(24,168), ts.frequency = 8760)


modfe <- Arima(trainfe, c(1, 1, 2), 
              list(order = c(1, 0, 1), period = 24),
              xreg= reg[1:7854,])
summary(modfe)
beep(2)


res1 <- modfe$residuals

par(mfrow=c(1,2))
Acf(res1, lag.max = 240)
Pacf(res1, lag.max = 240)

Acf(res1, lag.max = 900)
Pacf(res1, lag.max = 900)

par(mfrow=c(1,2),cex.axis=1, cex.lab=1, cex.main=1, fg="black",
    col.axis="black", col.lab="black", col.main="black", bg="white")
hist(res1)


#(3,1,1)(1,0,1) AICc=90714.34 - 13.59
#(1,1,2)(1,0,1) AICc=90715.23 - 14.05
#(1,1,2)(1,0,0) AICc=91290.26 - 12.54
#(3,1,1)(1,0,0) AICc=91269.45 - 12.28

###################################################

# FORECAST
frcf1 <- forecast(modfe, 672, xreg= reg[7855:8526,]) #xreg= cbind(cc2, ss2)[7111:7854,]
frc_newf1 <- frcf1$mean
frc_newf1



# grafico delle previsioni (zoom)
autoplot(frc_newf1, ts.colour = 'black', size = 0.5) +
  scale_color_manual(labels = c("Actual", "Forecasted", "Train"),
                     values=c("green", "red", "black")) +
  autolayer(frc_newf1, series = 'Forecasted', size = 0.5) +
  autolayer(testfe, series='Actual', size = 0.8)




# grafico della serie storica + le previsioni (totale)
autoplot(frc_newf1, ts.colour = 'black', size = 0.5) +
  scale_color_manual(labels = c("Actual", "Forecasted", "Train"),
                     values=c("green", "red", "black")) +
  autolayer(frc_newf1, series = 'Forecasted', size = 0.5) +
  autolayer(testfe, series='Actual', size = 0.5) +
  autolayer(trainfe, series='Train', size = 0.5)


# test sul mese di dicembre.
errfe <- testfe[1:672] - frc_newf1[1:672]
mapefe <- mean(abs(errfe)/testfe[1:672]*100)
mapefe 
#########################################################################################

plot(testd)
plot(testg)
plot(testfe)

#togli 2 giorni a Febbraio
train_ma <- window(y_f, end = c(2005, lastHour-25)) #8478
trainma <- msts(train_ma, end = c(2005, lastHour-25), seasonal.periods=c(24,168), ts.frequency = 8760)

##################
y_f3 <- BoxCox(trainma, lambda = 0)

modma <- Arima(y_f3, c(3, 1, 1), 
               list(order = c(1, 0, 0), period = 24),
               xreg= reg[1:8478,])
summary(modma)



res1 <- modma$residuals

par(mfrow=c(1,2))
Acf(res1, lag.max = 240)
Pacf(res1, lag.max = 240)

Acf(res1, lag.max = 900)
Pacf(res1, lag.max = 900)


# FORECAST
frcf1 <- forecast(modma, 792, xreg= reg3[1:792,], lambda= 0, biasadj = TRUE)
frc_newf1 <- frcf1$mean
frc_newf1


# grafico delle previsioni (zoom)
autoplot(frc_newf1, ts.colour = 'black', size = 0.5) +
  scale_color_manual(labels = c("Actual", "Forecasted", "Train"),
                     values=c("green", "red", "black")) +
  autolayer(frc_newf1, series = 'Forecasted', size = 0.5) +
  autolayer(testfe, series='Actual', size = 0.8)



# grafico della serie storica + le previsioni con intervalli di confidenza(totale)
autoplot(frcf1, ts.colour = 'black', size = 0.5) +
  scale_color_manual(labels = c("Actual", "Forecasted", "Train"),
                     values=c("black", "red", "black")) +
  autolayer(frcf1, series = 'Forecasted', size = 0.5) +
  autolayer(y_f, series='Actual', size = 0.5) 
  #autolayer(y_f, series='Train', size = 0.5)



##################################################################################
#inserire frc_newf1:
frc_newf1[49:792] #in dataset finale
##################################################################################

##################################################################################
#modellli della famiglia UCM
library(KFAS)

#RW con doppia stagionalità 24h dummies e settimanale trigonometrica 8 armoniche: 20.21348


ucm_mod1 <- SSModel(train3 ~ SSMtrend(1, NA) +
                      SSMseasonal(24, NA, "dummy") +
                      SSMseasonal(168, NA, "trig",
                                  harmonics = 1:4),
                    H = NA)



vary <- var(train3, na.rm = TRUE)
ucm_mod1$P1inf <- ucm_mod1$P1inf * 0
ucm_mod1$a1[1] <- mean(train3, na.rm = TRUE)
diag(ucm_mod1$P1) <- vary

ucm_mod1$Q

#valori iniziali delle varianze
init <- numeric(5)
init[1] <- log(vary/100) 
init[2] <- log(vary/1000)
init[3] <- log(vary/1000)
init[4] <- log(vary/20) 

#funzione per fitSSM
update_fun <- function(pars, model){
  model$Q[1, 1, 1] <- exp(pars[1])
  model$Q[2, 2, 1] <- exp(pars[2])
  #model$Q[3, 3, 1] <- exp(pars[3])
  diag(model$Q[3:10, 3:10, 1]) <- exp(pars[3])
  model$H[1, 1, 1] <- exp(pars[4])
  model
}

fit2 <- fitSSM(ucm_mod1, init, update_fun)
print(fit2$optim.out$convergence)

beep(2)

summary(fit2)

###
data <- c(rep(NA, 24))
temp_mod <- SSModel(data ~  SSMtrend(1, fit2$model$Q[1,1,1]) +
                      SSMseasonal(24, fit2$model$Q[2,2,1], "dummy") +
                      SSMseasonal(168, fit2$model$Q[3, 3, 1], "trig",
                                  harmonics = 1:4),
                    H = fit2$model$H)
ucm_pred <- predict(fit2$model, newdata=temp_mod)[1:24]

plot(ucm_pred, type='l')

errUCM <- test3[1:24] - ucm_pred[1:24]
mapeUCM <- mean(abs(errUCM)/test3[1:24]*100)
mapeUCM 

############################################################################################
############################################################################################

#LLT con doppia stagionalità 24h dummies e settimanale trigonometrica 8 armoniche: 20.20538


ucm_mod1 <- SSModel(train3 ~ SSMtrend(2, list(NA,NA)) +
                      SSMseasonal(24, NA, "dummy") +
                      SSMseasonal(168, NA, "trig",
                                  harmonics = 1:8),
                    H = NA)

ucm_mod1$Q

vary <- var(train3, na.rm = TRUE)
ucm_mod1$P1inf <- ucm_mod1$P1inf * 0
ucm_mod1$a1[1] <- mean(train3, na.rm = TRUE)
diag(ucm_mod1$P1) <- vary



#valori iniziali delle varianze
init <- numeric(5)
init[1] <- log(vary/100) 
init[2] <- log(vary/100) 
init[3] <- log(vary/1000)
init[4] <- log(vary/1000)
init[5] <- log(vary/10)

#funzione per fitSSM
update_fun <- function(pars, model){
  model$Q[1, 1, 1] <- exp(pars[1])
  model$Q[2, 2, 1] <- exp(pars[2])
  model$Q[3, 3, 1] <- exp(pars[3])
  diag(model$Q[4:19, 4:19, 1]) <- exp(pars[4])
  model$H[1, 1, 1] <- exp(pars[5])
  model
}

fit2 <- fitSSM(ucm_mod1, init, update_fun)
print(fit2$optim.out$convergence)

beep(2)

summary(fit2)

###
data <- c(rep(NA, 24))
temp_mod <- SSModel(data ~  SSMtrend(2, list(fit2$model$Q[1,1,1],fit2$model$Q[2,2,1])) +
                      SSMseasonal(24, fit2$model$Q[3,3,1], "dummy") +
                      SSMseasonal(168, fit2$model$Q[4, 4, 1], "trig",
                                  harmonics = 1:8),
                    H = fit2$model$H)
ucm_pred <- predict(fit2$model, newdata=temp_mod)[1:24]
ucm_pred

plot(ucm_pred, type='l')
plot(test3)

errUCM <- test3[1:24] - ucm_pred[1:24]
mapeUCM <- mean(abs(errUCM)/test3[1:24]*100)
mapeUCM 

###########################################################################################

#IRW con doppia stagionalità 24h dummies e settimanale trigonometrica 8 armoniche: 20.20538

ucm_mod1 <- SSModel(train3 ~ SSMtrend(2, list(0,NA)) +
                      SSMseasonal(24, NA, "dummy") +
                      SSMseasonal(168, NA, "trig",
                                  harmonics = 1:8),
                    H = NA)

ucm_mod1$Q

vary <- var(train3, na.rm = TRUE)
ucm_mod1$P1inf <- ucm_mod1$P1inf * 0
ucm_mod1$a1[1] <- mean(train3, na.rm = TRUE)
diag(ucm_mod1$P1) <- vary



#valori iniziali delle varianze
init <- numeric(5)
init[1] <- 0 
init[2] <- log(vary/100) 
init[3] <- log(vary/1000)
init[4] <- log(vary/1000)
init[5] <- log(vary/20)

#funzione per fitSSM
update_fun <- function(pars, model){
  model$Q[1, 1, 1] <- exp(pars[1])
  model$Q[2, 2, 1] <- exp(pars[2])
  model$Q[3, 3, 1] <- exp(pars[3])
  diag(model$Q[4:19, 4:19, 1]) <- exp(pars[4])
  model$H[1, 1, 1] <- exp(pars[5])
  model
}

fit2 <- fitSSM(ucm_mod1, init, update_fun)
print(fit2$optim.out$convergence)

beep(2)


###
data <- c(rep(NA, 24))
temp_mod <- SSModel(data ~  SSMtrend(2, list(0,fit2$model$Q[2,2,1])) +
                      SSMseasonal(24, fit2$model$Q[3,3,1], "dummy") +
                      SSMseasonal(168, fit2$model$Q[4, 4, 1], "trig",
                                  harmonics = 1:8),
                    H = fit2$model$H)
ucm_pred <- predict(fit2$model, newdata=temp_mod)[1:24]

plot(ucm_pred, type='l')
plot(test3)

errUCM <- test3[1:24] - ucm_pred[1:24]
mapeUCM <- mean(abs(errUCM)/test3[1:24]*100)
mapeUCM 


###########################################################################################

# RW con doppia stagionalità 24h-4arm trig e settimanale trig-4arm : 18.15649
#IRW con doppia stagionalità 24h-4arm trig e settimanale trig-4arm : 18.12544
#IRW con doppia stagionalità 24h-8arm trig e settimanale trig-2arm : 17.54224
# RW con doppia stagionalità 24h-8arm trig e settimanale trig-2arm : 17.42077

ucm_mod1 <- SSModel(train3 ~ SSMtrend(2, list(0,NA)) +
                      SSMseasonal(24, NA, "trig",
                                  harmonics = 1:8) +
                      SSMseasonal(168, NA, "trig",
                                  harmonics = 1:4),
                    H = NA)



vary <- var(train3, na.rm = TRUE)
ucm_mod1$P1inf <- ucm_mod1$P1inf * 0
ucm_mod1$a1[1] <- mean(train3, na.rm = TRUE)
diag(ucm_mod1$P1) <- vary



#valori iniziali delle varianze
init <- numeric(5)
init[1] <- 0 
init[2] <- log(vary/100) 
init[3] <- log(vary/1000)
init[4] <- log(vary/1000)
init[5] <- log(vary/20)

#funzione per fitSSM
update_fun <- function(pars, model){
  model$Q[1, 1, 1] <- exp(pars[1])
  model$Q[2, 2, 1] <- exp(pars[2])
  diag(model$Q[3:18, 3:18, 1]) <- exp(pars[3])
  #model$Q[3, 3, 1] <- exp(pars[3])
  diag(model$Q[19:26, 19:26, 1]) <- exp(pars[4])
  model$H[1, 1, 1] <- exp(pars[5])
  model
}

fit2 <- fitSSM(ucm_mod1, init, update_fun)
print(fit2$optim.out$convergence)

beep(2)

summary(fit2)

###
data <- c(rep(NA, 24))
temp_mod <- SSModel(data ~  SSMtrend(2, list(0,fit2$model$Q[2,2,1])) +
                      SSMseasonal(24, fit2$model$Q[2,2,1], "trig",
                                  harmonics = 1:8) +
                      SSMseasonal(168, fit2$model$Q[3, 3, 1], "trig",
                                  harmonics = 1:4),
                    H = fit2$model$H)
ucm_pred <- predict(fit2$model, newdata=temp_mod)[1:24]


plot(ucm_pred, type='l')
plot(test3)

errUCM <- test3[1:24] - ucm_pred[1:24]
mapeUCM <- mean(abs(errUCM)/test3[1:24]*100)
mapeUCM 

#####
#plot

ggplot() +
  autolayer(test3, series="REAL VALUE",size=1) +
  autolayer(ts(ucm_pred, start=start(test3),
               frequency=frequency(test3)),
            series="UCM PRED", size=1, alpha=0.7)

############################################################################################
train <- window(y_f, end = c(2004, 3474)) #1825
train3mod <- msts(train, end = c(2004, 3474), seasonal.periods=c(24,168), ts.frequency = 8760)


test <- window(y_f, start = c(2004, 3489), end = c(2004, 3512)) #1840-1863
test3mod <- msts(test, start = c(2004, 3489), end = c(2004, 3512), seasonal.periods=c(24,168), ts.frequency = 8760)



#USARE VALORI LOG
plot(train3mod)
y3 <- log(train3mod)
plot(y3)

omega4 <- outer(1:length(y3), 1:2) * 2 * pi / 8760 #168 sett, 730 mens.
cc4 <- cos(omega4)
ss4 <- sin(omega4)

modU1 <- SSModel(y3~cc4+ss4+SSMtrend(1, NA)+
                   SSMseasonal(24, NA, "trig",
                               harmonics = 1:8)+
                   SSMseasonal(168, NA, "trig",
                               harmonics = 1:2), 
                 H = NA)

reg <- arima(y3, c(7,0,0), xreg = cbind(cc4, ss4))
cfs <- coefficients(reg)[9:12]

modU1$a1[1:4] <- cfs
modU1$a1[5] <- y3[1]
modU1$P1inf <- matrix(0, 25, 25)
vary <- var(y3)

diag(modU1$P1[5:25, 5:25]) <- vary
diag(modU1$P1[1:4, 1:4]) <- diag(reg$var.coef[9:12, 9:12])
modU1$P1
######################################
init <- numeric(4)
init[1] <- log(vary/100) 
init[2] <- log(vary/1000)
init[3] <- log(vary/1000)
init[4] <- log(vary/20) 

#funzione per fitSSM
update_fun <- function(pars, model){
  model$Q[1, 1, 1] <- exp(pars[1])
  #model$Q[2, 2, 1] <- exp(pars[2])
  diag(model$Q[2:17, 2:17, 1]) <- exp(pars[2])
  #model$Q[3, 3, 1] <- exp(pars[3])
  diag(model$Q[18:21, 18:21, 1]) <- exp(pars[3])
  model$H[1, 1, 1] <- exp(pars[4])
  model
}


fit1 <- fitSSM(modU1, init, update_fun)
cat("Codice di convergenza =", fit1$optim.out$convergence)
beep(2)

#Completiamo come abbiamo fatto finora

v3 <- log(test3mod)

omega5 <- outer(1:length(v3), 1:2) * 2 * pi / 8760 #168 sett, 730 mens.
cc5 <- cos(omega5)
ss5 <- sin(omega5)

###
data <- c(rep(NA, 24))
temp_mod <- SSModel(data ~ cc5+ss5+ SSMtrend(1, fit1$model$Q[1,1,1]) +
                      SSMseasonal(24, fit1$model$Q[2,2,1], "trig",
                                  harmonics = 1:8) +
                      SSMseasonal(168, fit1$model$Q[3, 3, 1], "trig",
                                  harmonics = 1:2),
                    H = fit1$model$H)

ucm_pred <- predict(fit1$model, newdata=temp_mod)[1:24]


plot(ucm_pred, type='l')
plot(v3)

errUCM <- v3[1:24] - ucm_pred[1:24]
mapeUCM <- mean(abs(errUCM)/v3[1:24]*100)
mapeUCM 

#BUON RISULTATO:
#RW doppia stag. 24-8arm, 168-2arm, ARIMA7,0,0, FREQ sinusoidi annuale: 12.04085

#########################################################################################################
# TEST SU UN INTERO MESE

plot(trainfe)
y3 <- log(trainfe)
plot(y3)

omega4 <- outer(1:length(y3), 1:2) * 2 * pi / 8760 #168 sett, 730 mens.
cc4 <- cos(omega4)
ss4 <- sin(omega4)

modU1 <- SSModel(y3~cc4+ss4+SSMtrend(1, NA)+
                   SSMseasonal(24, NA, "trig",
                               harmonics = 1:8)+
                   SSMseasonal(168, NA, "trig",
                               harmonics = 1:2), 
                 H = NA)

reg <- arima(y3, c(7,0,0), xreg = cbind(cc4, ss4))
cfs <- coefficients(reg)[9:12]

modU1$a1[1:4] <- cfs
modU1$a1[5] <- y3[1]
modU1$P1inf <- matrix(0, 25, 25)
vary <- var(y3)

diag(modU1$P1[5:25, 5:25]) <- vary
diag(modU1$P1[1:4, 1:4]) <- diag(reg$var.coef[9:12, 9:12])
modU1$P1
######################################
init <- numeric(4)
init[1] <- log(vary/100) 
init[2] <- log(vary/1000)
init[3] <- log(vary/1000)
init[4] <- log(vary/20) 

#funzione per fitSSM
update_fun <- function(pars, model){
  model$Q[1, 1, 1] <- exp(pars[1])
  #model$Q[2, 2, 1] <- exp(pars[2])
  diag(model$Q[2:17, 2:17, 1]) <- exp(pars[2])
  #model$Q[3, 3, 1] <- exp(pars[3])
  diag(model$Q[18:21, 18:21, 1]) <- exp(pars[3])
  model$H[1, 1, 1] <- exp(pars[4])
  model
}


fit1 <- fitSSM(modU1, init, update_fun)
cat("Codice di convergenza =", fit1$optim.out$convergence)
beep(2)

#Completiamo come abbiamo fatto finora

v3 <- log(testfe)

omega5 <- outer(1:length(v3), 1:2) * 2 * pi / 8760 #168 sett, 730 mens.
cc5 <- cos(omega5)
ss5 <- sin(omega5)

###
data <- c(rep(NA, 672))
temp_mod <- SSModel(data ~ cc5+ss5+ SSMtrend(1, fit1$model$Q[1,1,1]) +
                      SSMseasonal(24, fit1$model$Q[2,2,1], "trig",
                                  harmonics = 1:8) +
                      SSMseasonal(168, fit1$model$Q[3, 3, 1], "trig",
                                  harmonics = 1:2),
                    H = fit1$model$H)

ucm_pred <- predict(fit1$model, newdata=temp_mod)[1:672]
ucm_pred_t <- InvBoxCox(ucm_pred ,lambda = 0)


plot(ucm_pred, type='l')
plot(v3)

errUCM <- testfe[1:672] - ucm_pred_t[1:672]
mapeUCM <- mean(abs(errUCM)/testfe[1:672]*100)
mapeUCM #12.8341

#OTTIMO RISULTATO:
#RW doppia stag. 24-8arm, 168-2arm, ARIMA7,0,0, FREQ sinusoidi annuale: 12.8341

ggplot() +
  autolayer(trainfe, series = "TRAIN", size = 1) +
  autolayer(testfe, series="REAL VALUE",size=1) +
  autolayer(ts(ucm_pred_t, start=start(testfe),
               frequency=frequency(testfe)),
            series="UCM PRED", size=1, alpha=0.7)


#plot #Questo plot è migliore.

ggplot() +
  autolayer(v3, series="REAL VALUE",size=1) +
  autolayer(ts(ucm_pred, start=start(v3),
               frequency=frequency(v3)),
            series="UCM PRED", size=1, alpha=0.7)
##############################################################################################
#Previsione su Marzo


omega6 <- outer(1:length(y_f3), 1:2) * 2 * pi / 8760 #168 sett, 730 mens.
cc6 <- cos(omega6)
ss6 <- sin(omega6)

modU1 <- SSModel(y_f3~cc6+ss6+SSMtrend(1, NA)+
                   SSMseasonal(24, NA, "trig",
                               harmonics = 1:8)+
                   SSMseasonal(168, NA, "trig",
                               harmonics = 1:2), 
                 H = NA)

reg <- arima(y_f3, c(7,0,0), xreg = cbind(cc6, ss6))
cfs <- coefficients(reg)[9:12]

modU1$a1[1:4] <- cfs
modU1$a1[5] <- y_f3[1]
modU1$P1inf <- matrix(0, 25, 25)
vary <- var(y_f3)

diag(modU1$P1[5:25, 5:25]) <- vary
diag(modU1$P1[1:4, 1:4]) <- diag(reg$var.coef[9:12, 9:12])
modU1$P1
######################################
init <- numeric(4)
init[1] <- log(vary/100) 
init[2] <- log(vary/1000)
init[3] <- log(vary/1000)
init[4] <- log(vary/20) 

#funzione per fitSSM
update_fun <- function(pars, model){
  model$Q[1, 1, 1] <- exp(pars[1])
  #model$Q[2, 2, 1] <- exp(pars[2])
  diag(model$Q[2:17, 2:17, 1]) <- exp(pars[2])
  #model$Q[3, 3, 1] <- exp(pars[3])
  diag(model$Q[18:21, 18:21, 1]) <- exp(pars[3])
  model$H[1, 1, 1] <- exp(pars[4])
  model
}


fit1 <- fitSSM(modU1, init, update_fun)
cat("Codice di convergenza =", fit1$optim.out$convergence)
beep(2)



v3 <- log(testfe)

omega7 <- outer(1:792, 1:2) * 2 * pi / 8760 #168 sett, 730 mens.
cc7 <- cos(omega7)
ss7 <- sin(omega7)

###
data <- c(rep(NA, 792))
temp_mod <- SSModel(data ~ cc7+ss7+ SSMtrend(1, fit1$model$Q[1,1,1]) +
                      SSMseasonal(24, fit1$model$Q[2,2,1], "trig",
                                  harmonics = 1:8) +
                      SSMseasonal(168, fit1$model$Q[3, 3, 1], "trig",
                                  harmonics = 1:2),
                    H = fit1$model$H)

ucm_pred <- predict(fit1$model, newdata=temp_mod)[1:792]
ucm_pred_t <- InvBoxCox(ucm_pred ,lambda = 0)



ggplot() +
  autolayer(trainma, series = "TRAIN", size = 1) +
  autolayer(ts(ucm_pred_t, start=c(2005, lastHour-24),
               frequency=frequency(trainma)),
            series="UCM PRED", size=1, alpha=0.7)+
  scale_color_manual(labels = c("Train", "UCM PRED"),
                     values=c("black", "green"))


ucm_pred_t[49:792] #PREVISIONI FINALI MARZO CON MODELLO UCM
###############################################################################################

###############################################################################################
#############################################################################################


#################################################################################################
library(tsfknn)
library(Metrics)

trainb <- window(y_f, end = c(2004, 3474)) #1825
train3b <- msts(trainb, end = c(2004, 3474), seasonal.periods=c(24,168), ts.frequency = 8760)


testb <- window(y_f, start = c(2004, 3475), end = c(2004, 3714)) #1840-1863   ##3512
test3b <- msts(testb, start = c(2004, 3475), end = c(2004, 3714), seasonal.periods=c(24,168), ts.frequency = 8760)


#KNN
p <- 1:168 
k <- c(3,5,7,9) 
h <- 240

for (i in c(1:4)){
  res <- knn_forecasting( timeS=train3b, h=h, lags=p, k=i, msas="recursive", cf="mean" )
  print(mae(res$prediction , test3b))
}

res <- knn_forecasting(timeS=train3b, h=h, lags=p, k=7, msas="recursive", cf="mean" )
res$prediction


ggplot() +
  autolayer(train3b, series = "TRAIN", size = 1) +
  autolayer(res$prediction, series = 'PREDICTION', size = 1)+
  autolayer(test3b, series = "TEST", size = 1, alpha=0.7)


errKNN <- test3b - res$prediction
mapeKNN <- mean(abs(errKNN)/test3b*100)
mapeKNN #19.31

#fare questo knn su un intero mese.
trainfe
testfe #672
h <- 672

res <- knn_forecasting(timeS=trainfe, h=h, lags=p, k=7, msas="recursive", cf="mean" )
res$prediction


ggplot() +
  autolayer(trainfe, series = "TRAIN", size = 1) +
  autolayer(testfe, series = "TEST", size = 1, alpha=1)+
  autolayer(res$prediction, series = 'PREDICTION', alpha= 0.7, size = 1)+
  scale_color_manual(labels = c("PREDICTION", "TEST", "TRAIN"),
                   values=c("red", "green", "black"))


errKNN <- testfe - res$prediction
mapeKNN <- mean(abs(errKNN)/testfe*100)
mapeKNN #11.51
#############################################################################################
#esporto questo per svolgere LSTM in python
write.csv(dataset2, "C:\\Users\\Gabriele\\Downloads\\dataset2.csv", row.names = FALSE)

#Importo il dataset risultante dalla LSTM.
forecast_DF <- read_csv("C:/Users/Gabriele/Downloads/forecast_DF.csv")
View(forecast_DF)

#############################################################################################

# Creare dataset finale con le previsioni dei modelli.

#previsioni arima: frc_newf1[49:792]

forecast_DF[,5] <- frc_newf1[49:792]
colnames(forecast_DF)[5] <- "ARIMA"
forecast_DF[,5] <- round(forecast_DF[,5])


#previsioni UCM: ucm_pred_t[49:792]

forecast_DF[,6] <- ucm_pred_t[49:792]
colnames(forecast_DF)[6] <- "UCM"
forecast_DF[,6] <- round(forecast_DF[,6])


#sposto la colonna ML in ultima posizione

forecast_DF[,7] <- forecast_DF[,4]
forecast_DF[,4] <- NULL

# Rimuovo la prima colonna con il timestamp divenuta ormai inutile.
forecast_DF[,1] <- NULL

# Esporto dataframe con le previsioni finali.

write.csv(forecast_DF, "C:\\Users\\Gabriele\\Downloads\\dataset_previsioni.csv", row.names = FALSE)

library(mgcv)
library(dlnm)
library(tidyverse)

city <- read.csv("National average _3", header=TRUE, fileEncoding="cp936")
colnames(city) <- c("Date","Temperature","Humidity","WindSpeed","Pressure",
                    "R0orig","R0alp","R0dlt","R0omi",
                    "dosePCA","UV","totalarrival","nationalarrival",
                    "popdensity","latitude","longitude")

city_model <- city

#####run the model
### exposure function
varfun = "ns"
vardf=2
argvar <- list(fun=varfun, df=vardf)

### lag function
lag <- 13
lagfun="ns"
lagdf=2

###omi
virus <-city_model
  # %>%
  # filter(R0omi>0.1 & R0omi<12) 


# crossbasis
cb.Temperature <- crossbasis(virus$Temperature, lag=lag, argvar=argvar,
                             arglag=list(fun=lagfun, df=lagdf), 
                             cen=virus[which.min(virus$R0omi), 2])
cb.Humidity <- crossbasis(virus$Humidity, lag=lag, argvar=argvar,
                          arglag=list(fun=lagfun, df=lagdf),
                          cen=virus[which.min(virus$R0omi), 3])
cb.WindSpeed <- crossbasis(virus$WindSpeed, lag=lag,argvar=argvar,
                           arglag=list(fun=lagfun, df=lagdf),
                           cen=virus[which.min(virus$R0omi), 4])
cb.UV <- crossbasis(virus$UV, lag=lag,argvar=argvar,
                    arglag=list(fun=lagfun, df=lagdf),
                    cen=virus[which.min(virus$R0omi), 11])

lag2 <- 7
cb.tarrival <- crossbasis(virus$totalarrival, lag=lag2, argvar=argvar,
                          arglag=list(fun=lagfun, df=lagdf),
                          cen=virus[which.min(virus$R0omi), 12])
cb.narrival <- crossbasis(virus$nationalarrival, lag=lag2, argvar=argvar,
                          arglag=list(fun=lagfun, df=lagdf),
                          cen=virus[which.min(virus$R0omi), 13])


virus_model <- gam(R0omi ~ te(as.numeric(as.Date(Date)), latitude, longitude, bs=c("cr","tp"), d=c(1,2), k=c(5, 5))
                   + cb.Temperature + cb.Humidity + cb.WindSpeed + cb.UV 
                   + cb.tarrival + cb.narrival + s(Pressure)
                   + popdensity + dosePCA, 
                   data=virus,
                   family=gaussian(link="log"))
summary(virus_model)

pred.temp <- crosspred(cb.Temperature, virus_model, cen=virus[which.min(virus$R0omi), 2], bylag=0.2)
pred.humi <- crosspred(cb.Humidity, virus_model, cen=virus[which.min(virus$R0omi), 3], bylag=0.2)
pred.wind <- crosspred(cb.WindSpeed, virus_model, cen=virus[which.min(virus$R0omi), 4], bylag=0.2)
pred.UV <- crosspred(cb.UV, virus_model, cen=virus[which.min(virus$R0omi), 11], bylag=0.2)
pred.tarrival <- crosspred(cb.tarrival, virus_model, cen=virus[which.min(virus$R0omi), 12], bylag=0.2)

png(file = "omi_tempreature.png", width = 3000, height = 2000, res = 300)
par(mar=c(4,5,3,5),oma=c(1,1,1,1))
plot(pred.temp, "contour", key.title=title("RR"),cex.axis=2,
     plot.axes={axis(1,cex.axis=2)
       axis(2,cex.axis=2)},
     key.axes = axis(4,cex.axis=2),
     plot.title=title(xlab="",ylab="Lag (days)",cex.main=2,cex.lab=2))
dev.off()

png(file = "omi_humidity.png", width = 3000, height = 2000, res = 300)
par(mar=c(4,5,3,5),oma=c(2,2,2,2))
plot(pred.humi, "contour", key.title=title("RR"),cex.axis=2,
     plot.axes={axis(1,cex.axis=2)
       axis(2,cex.axis=2)},
     key.axes = axis(4,cex.axis=2),
     plot.title=title(xlab="", ylab="Lag (days)",cex.main=2,cex.lab=2))
dev.off()

png(file = "omi_windspeed.png", width = 3000, height = 2000, res = 300)
par(mar=c(4,5,3,5),oma=c(2,2,2,2))
plot(pred.wind, "contour", key.title=title("RR"),cex.axis=2,
     plot.axes={axis(1,cex.axis=2)
       axis(2,cex.axis=2)},
     key.axes = axis(4,cex.axis=2),
     plot.title=title(xlab="",ylab="Lag (days)",cex.main=2,cex.lab=2))
dev.off()

png(file = "omi_UV.png", width = 3000, height = 2000, res = 300)
par(mar=c(4,5,3,5),oma=c(2,2,2,2))
plot(pred.UV, "contour", key.title=title("RR"),cex.axis=2,
     plot.axes={axis(1,cex.axis=2)
       axis(2,cex.axis=2)},
     key.axes = axis(4,cex.axis=2),
     plot.title=title(xlab="",ylab="Lag (days)",cex.main=2,cex.lab=2))
dev.off()

png(file = "omi_arrival.png", width = 3000, height = 2000, res = 300)
par(mar=c(4,5,3,5),oma=c(2,2,2,2))
plot(pred.tarrival, "contour", key.title=title("RR"),cex.axis=2,
     plot.axes={axis(1,cex.axis=2)
       axis(2,cex.axis=2)},
     key.axes = axis(4,cex.axis=2),
     plot.title=title(xlab="",ylab="Lag (days)",cex.main=2,cex.lab=2))
dev.off()





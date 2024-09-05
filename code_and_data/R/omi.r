#####attributable function
source("attribute.R")


#####data
city <- read.csv("Adur.csv", header=TRUE, fileEncoding="cp936")
colnames(city) <- c("Date","Temperature","Humidity","WindSpeed","Pressure",
                    "R0orig","R0alp","R0dlt","R0omi",
                    "dosePCA","totalarrival","nationalarrival","UV",
                    "male","female","doctors","nonwhite","popdensity")

library(tidyverse)
city_model <- 
  city %>%
  select(c(-14, -15)) %>%
  mutate(time=seq(from=1, to=nrow(city), by=1))



#####run the model
###dlt
virus <-
  city_model %>%
  slice(664:840) %>%
  filter(R0omi>0 & R0omi<10)

library(dlnm)
#exposure function
varfun = "ns"
vardf=3
argvar <- list(fun=varfun, df=vardf)

#lag function
lag <- 3
lagfun="ns"
lagdf=3

# crossbasis
cb.Temperature <- crossbasis(virus$Temperature, lag=lag, argvar=argvar,
                             arglag=list(fun=lagfun, df=lagdf),
                             cen=min(virus$Temperature))
cb.Humidity <- crossbasis(virus$Humidity, lag=lag, argvar=argvar,
                          arglag=list(fun=lagfun, df=lagdf),
                          cen=min(virus$Humidity))
cb.WindSpeed <- crossbasis(virus$WindSpeed, lag=lag,argvar=argvar,
                           arglag=list(fun=lagfun, df=lagdf),
                           cen=min(virus$WindSpeed))
cb.Pressure <- crossbasis(virus$Pressure, lag=lag,argvar=argvar,
                          arglag=list(fun=lagfun, df=lagdf),
                          cen=min(virus$Pressure))


library(splines)
library(mgcv)
virus_model <- gam(R0omi ~ cb.Temperature+cb.Humidity+cb.WindSpeed+cb.Pressure
                   + dosePCA + totalarrival + nationalarrival + UV +
                   + ns(time, 13*1)
                   + doctors + nonwhite + popdensity, 
                   data=virus, family=quasipoisson())
summary(virus_model)

#Ԥ����������Ӱ�죬����СR0Ϊ���գ���R0�ٷֱȱ䶯����ӳӰ��̶�
#ȡԤ������������СֵΪ���գ������������ӹ��׶�
#minR0 <- min(orig_model$fitted.values)
pred_temp <- crosspred(cb.Temperature, virus_model, bylag=0.2)
temp_minR0 <- attr(which.min(pred_temp$allfit), "names")
pred_humi <- crosspred(cb.Humidity, virus_model, bylag=0.2)
humi_minR0 <- attr(which.min(pred_humi$allfit), "names")
pred_wind <- crosspred(cb.WindSpeed, virus_model, bylag=0.2)
wind_minR0 <- attr(which.min(pred_wind$allfit), "names")
pred_pres <- crosspred(cb.Pressure, virus_model, bylag=0.2)
pres_minR0 <- attr(which.min(pred_pres$allfit), "names")


##���㹱�׶�
attrback_temp <- attrdl(virus$Temperature, cb.Temperature, virus$R0omi,
                        virus_model, tot=FALSE, type="af")
attrback_humi <- attrdl(virus$Temperature, cb.Humidity, virus$R0omi,
                        virus_model, tot=FALSE, type="af")
attrback_wind <- attrdl(virus$WindSpeed, cb.WindSpeed, virus$R0omi,
                        virus_model, tot=FALSE, type="af")
attrback_pres <- attrdl(virus$Pressure, cb.Pressure, virus$R0omi,
                        virus_model, tot=FALSE, type="af")

#attrforw <- attrdl(orig$Temperature,cb.Temperature,orig$R0orig,
#                   orig_model, tot=FALSE,type="af",dir="forw",
#                   cen=29)
result <- cbind(virus, 
                    attrback_temp, attrback_humi, attrback_wind, attrback_pres)

##���ӻ����׶�
png(file = "omi�������ӹ��׶�.png", width = 3500, height = 2000, res = 300)
par(mar=c(4,5,3,5),oma=c(2,2,2,2))
plot(as.Date(result$Date), result$attrback_temp,
     ylab="Attributable fraction",xlab="Date",
     ylim=c(-1, 1),
     cex.lab=1.5,
     axes=TRUE, frame.plot=FALSE, cex=0.8, pch=22, bg="#4DBBD5E5",
     main="Omicron")
points(as.Date(result$Date), result$attrback_humi,
       ylab="Attributable fraction",xlab="Date",
       cex.lab=1.5,
       cex=0.8, pch=22, bg="#F7BA0B")
points(as.Date(result$Date), result$attrback_wind,
       ylab="Attributable fraction",xlab="Date",
       cex.lab=1.5,
       cex=0.8, pch=22, bg="#A580A4")
points(as.Date(result$Date), result$attrback_pres,
       ylab="Attributable fraction",xlab="Date",
       cex.lab=1.5,
       cex=0.8, pch=22, bg="#C9E5C8")
legend("topleft",
       fill=c("#4DBBD5E5", "#F7BA0B", "#A580A4", "#C9E5C8"),
       legend=c("Temperature", "Humidity", "Wind Speed", "Pressure"),
       horiz=FALSE, border=FALSE, bty="n", cex=1)
dev.off()


png(file = "Temperature���׶�.png", width = 3500, height = 2000, res = 300)
par(mar=c(4,5,3,5),oma=c(2,2,2,2))
plot(as.Date(alpresult$Date), alpresult$attrback_temp,
     ylab="Attributable fraction",xlab="Date",
     ylim=c(-1, 1),
     cex.lab=1.5,
     axes=TRUE, frame.plot=FALSE, cex=0.8, pch=22, bg="#4DBBD5E5",
     main="SARS-CoV-2") +
  abline(h=0, lwd=1.5, lty=2, col="#696969") +
  legend("topleft",
         fill=c("#4DBBD5E5"),
         legend=c("Temperature"),
         horiz=FALSE, border=FALSE, bty="n", cex=1)
dev.off()
png(file = "Humidity���׶�.png", width = 3500, height = 2000, res = 300)
par(mar=c(4,5,3,5),oma=c(2,2,2,2))
plot(as.Date(origresult$Date), origresult$attrback_humi,
       ylab="Attributable fraction",xlab="Date",
       cex.lab=1.5,
       axes=TRUE, frame.plot=FALSE, cex=0.8, pch=22, bg="#F7BA0B",
       main="SARS-CoV-2") +
  abline(h=0, lwd=1.5, lty=2, col="#696969") +
  legend("topleft",
         fill=c("#F7BA0B"),
         legend=c("Humidity"),
         horiz=FALSE, border=FALSE, bty="n", cex=1)
dev.off()
png(file = "WindSpeed���׶�.png", width = 3500, height = 2000, res = 300)
par(mar=c(4,5,3,5),oma=c(2,2,2,2))
plot(as.Date(origresult$Date), origresult$attrback_wind,
       ylab="Attributable fraction",xlab="Date",
       cex.lab=1.5,
       axes=TRUE, frame.plot=FALSE, cex=0.8, pch=22, bg="#A580A4",
       main="SARS-CoV-2") +
  abline(h=0, lwd=1.5, lty=2, col="#696969") +
  legend("topleft",
         fill=c("#A580A4"),
         legend=c("Wind Speed"),
         horiz=FALSE, border=FALSE, bty="n", cex=1)
dev.off()
png(file = "Pressure���׶�.png", width = 3500, height = 2000, res = 300)
par(mar=c(4,5,3,5),oma=c(2,2,2,2))
plot(as.Date(origresult$Date), origresult$attrback_pres,
       ylab="Attributable fraction",xlab="Date",
       cex.lab=1.5,
       axes=TRUE, frame.plot=FALSE, cex=0.8, pch=22, bg="#C9E5C8",
       main="SARS-CoV-2") +
  abline(h=0, lwd=1.5, lty=2, col="#696969") +
  legend("topleft",
         fill=c("#C9E5C8"),
         legend=c("Pressure"),
         horiz=FALSE, border=FALSE, bty="n", cex=1)
dev.off()





## ������
result <- merge.data.frame(city_model, origresult)
write.table(cityresult, "Adur_AF.csv", row.names=FALSE, col.names=TRUE, sep=",")

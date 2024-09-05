#####data
city <- read.csv("Adur.csv", header=TRUE, fileEncoding="cp936")
colnames(city) <- c("Date","Temperature","Humidity","WindSpeed","Pressure",
                    "R0orig","R0alp","R0dlt","R0omi",
                    "dosePCA","totalarrival","nationalarrival","UV",
                    "male","female","doctors","nonwhite","popdensity","latitude","longitude")

library(tidyverse)
city_model <- 
  city
  # select(c(-14, -15)) %>%
  # mutate(longitude, latitude)
city_model <- city_model[-which(is.na(city_model$totalarrival | city_model$UV)),]


#####run the model
###alp
virus <-
  city_model %>%
  slice(275:446) %>%
  filter(R0alp>0 & R0alp<10)



library(dlnm)
#exposure function
varfun = "ns"
vardf= 5
argvar <- list(fun=varfun, df=vardf)

#lag function
lag <- 10
lagfun="ns"
lagdf=3

# crossbasis
cb.Temperature <- crossbasis(virus$Temperature, lag=lag, argvar=argvar,
                             arglag=list(fun=lagfun, df=lagdf), 
                             cen=virus[which.min(virus$R0alp), 2])
cb.Humidity <- crossbasis(virus$Humidity, lag=lag, argvar=argvar,
                          arglag=list(fun=lagfun, df=lagdf),
                          cen=virus[which.min(virus$R0alp), 3])
cb.WindSpeed <- crossbasis(virus$WindSpeed, lag=lag,argvar=argvar,
                           arglag=list(fun=lagfun, df=lagdf),
                           cen=virus[which.min(virus$R0alp), 4])
#cb.Pressure <- crossbasis(virus$Pressure, lag=lag,argvar=argvar,
#                          arglag=list(fun=lagfun, df=lagdf),
#                          cen=virus[which.min(virus$R0alp), 5])
cb.UV <- crossbasis(virus$UV, lag=lag,argvar=list(fun="ns", df=2),
                          arglag=list(fun=lagfun, df=lagdf),
                          cen=virus[which.min(virus$R0alp), 13])

lag2 <- 13
cb.tarrival <- crossbasis(virus$totalarrival, lag=lag2, argvar=list(fun="ns", df=2),
                          arglag=list(fun=lagfun, df=lagdf),
                          cen=virus[which.min(virus$R0alp), 11])
#cb.narrival <- crossbasis(virus$nationalarrival, lag=lag2, argvar=list(fun="ns", df=2),
                          #arglag=list(fun=lagfun, df=lagdf),
                          #cen=virus[which.min(virus$R0alp), 12])


library(splines)
library(mgcv)
virus_model <- gam(R0alp ~ te(as.numeric(as.Date(Date)), latitude, longitude, bs=c("cr","tp"), d=c(1,2), k=c(5, 10))
                    + cb.Temperature + cb.Humidity + cb.WindSpeed + cb.UV 
                    + cb.tarrival 
                    + s(Pressure)
                    + doctors + nonwhite + popdensity, 
                   data=virus,
                   family=gaussian(link="log"))
summary(virus_model)

##???㹱?׶?
library(FluMoDL)
attrback_temp <- attrdl(virus$Temperature, cb.Temperature, virus$R0alp,
                        virus_model,tot=FALSE, type="af",
                        cen=virus[which.min(virus$R0alp), 2])
attrback_humi <- attrdl(virus$Temperature, cb.Humidity, virus$R0alp,
                        virus_model, tot=FALSE, type="af", 
                        cen=virus[which.min(virus$R0alp), 3])
attrback_wind <- attrdl(virus$WindSpeed, cb.WindSpeed, virus$R0alp,
                        virus_model, tot=FALSE, type="af",
                        cen=virus[which.min(virus$R0alp), 4])
#attrback_pres <- attrdl(virus$Pressure, cb.Pressure, virus$R0alp,
#                        virus_model, tot=FALSE, type="af",
#                        cen=virus[which.min(virus$R0alp), 5])
attrback_UV <- attrdl(virus$UV, cb.UV, virus$R0alp,
                      virus_model, tot=FALSE, type="af",
                      cen=virus[which.min(virus$R0alp), 13])
attrback_tarrival <- attrdl(virus$totalarrival, cb.tarrival, virus$R0alp,
                            virus_model, tot=FALSE, type="af",
                            cen=virus[which.min(virus$R0alp), 11])
#attrback_narrival <- attrdl(virus$nationalarrival, cb.narrival, virus$R0alp,
                            #virus_model, tot=FALSE, type="af",
                            #cen=virus[which.min(virus$R0alp), 12])

result <- cbind(virus, 
                attrback_temp, attrback_humi, attrback_wind, attrback_UV,
                attrback_tarrival)
apply(result[ ,c(21:25)], 2, mean, na.rm=TRUE)


## ???ӻ?
afplot <- function(x){
  p <- plot(as.Date(result$Date), x,
            ylab="Attributable fraction",xlab="Date",
            ylim=c(-1, 1),
            cex.lab=1.5,
            axes=TRUE, frame.plot=FALSE, cex=0.8, pch=22, bg="#4DBBD5E5",
            main="alp")
  abline(h=0, col="grey")
  return(p)
}
afplot(result$attrback_temp)
afplot(result$attrback_humi)
afplot(result$attrback_wind)
afplot(result$attrback_UV)
afplot(result$attrback_tarrival)
afplot(result$attrback_narrival)

legend("topleft",
       fill=c("#4DBBD5E5", "#F7BA0B", "#A580A4", "#C9E5C8"),
       legend=c("Temperature", "Humidity", "Wind Speed", "Pressure"),
       horiz=FALSE, border=FALSE, bty="n", cex=1)


#Ԥ??????????Ӱ?죬????СR0Ϊ???գ???R0?ٷֱȱ䶯��??ӳӰ???̶?
#ȡԤ????????????СֵΪ???գ????????????ӹ??׶?
#minR0 <- min(alp_model$fitted.values)
pred_temp <- crosspred(cb.Temperature, virus_model, 
                       cen=virus[which.min(virus$R0alp), 2], bylag=0.2)
png(file = "alp_Temperature?ܱ?¶??Ӧ????.png", width = 3000, height = 1500, res = 300)
par(mar=c(3,3.5,2,1), mgp=c(1.7,0.5,0), omi=c(0,0,0,0),
    cex.axis=1.20,cex.main=1.5,cex.lab=1.3) #?趨ҳ?߾?
plot(pred_temp, "overall", col="#DC0000E5",
     xlab="Temperature (??F)",
     main="alp",
     lwd=2)
#abline(v=36, col = "black", lty=3, lwd=2)
dev.off()

#temp_minR0 <- attr(which.min(pred_temp$allfit), "names")
pred_humi <- crosspred(cb.Humidity, virus_model, bylag=0.2)
#humi_minR0 <- attr(which.min(pred_humi$allfit), "names")
pred_wind <- crosspred(cb.WindSpeed, virus_model, bylag=0.2)
#wind_minR0 <- attr(which.min(pred_wind$allfit), "names")
pred_pres <- crosspred(cb.Pressure, virus_model, bylag=0.2)
#pres_minR0 <- attr(which.min(pred_pres$allfit), "names")




## ????????
result <- merge.data.frame(city_model, alpresult)
write.table(cityresult, "Adur_AF.csv", row.names=FALSE, col.names=TRUE, sep=",")

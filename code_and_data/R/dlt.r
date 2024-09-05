city <- read.csv("Adur.csv", header=TRUE, fileEncoding="cp936")
colnames(city) <- c("Date","Temperature","Humidity","WindSpeed","Pressure",
                    "R0orig","R0alp","R0dlt","R0omi",
                    "dosePCA","totalarrival","nationalarrival","UV",
                    "male","female","doctors","nonwhite","popdensity")
longitude <- rnorm(842, -0.285896429, sd=0.003^2)
latitude <- rnorm(842, 50.83854429, sd=0.003^2)

library(tidyverse)
city_model <- 
  city %>%
  select(c(-14, -15)) %>%
  mutate(longitude, latitude)
city_model <- city_model[-which(is.na(city_model$totalarrival | city_model$nationalarrival | city_model$UV)),]


#####run the model
###dlt
virus <-
  city_model %>%
  slice(446:727) %>%
  filter(R0dlt>0 & R0dlt<10)

#####run the model
###dlt

library(dlnm)
#exposure function
varfun = "ns"
vardf=3
argvar <- list(fun=varfun, df=vardf)

#lag function
lag <- 15
lagfun="ns"
lagdf=3

# crossbasis
cb.Temperature <- crossbasis(virus$Temperature, lag=lag, argvar=argvar,
                             arglag=list(fun=lagfun, df=lagdf), 
                             cen=virus[which.min(virus$R0dlt), 2])
cb.Humidity <- crossbasis(virus$Humidity, lag=lag, argvar=argvar,
                          arglag=list(fun=lagfun, df=lagdf),
                          cen=virus[which.min(virus$R0dlt), 3])
cb.WindSpeed <- crossbasis(virus$WindSpeed, lag=lag,argvar=argvar,
                           arglag=list(fun=lagfun, df=lagdf),
                           cen=virus[which.min(virus$R0dlt), 4])
#cb.Pressure <- crossbasis(virus$Pressure, lag=lag,argvar=argvar,
#                          arglag=list(fun=lagfun, df=lagdf),
#                          cen=virus[which.min(virus$R0dlt), 5])
cb.UV <- crossbasis(virus$UV, lag=lag,argvar=argvar,
                          arglag=list(fun=lagfun, df=lagdf),
                          cen=virus[which.min(virus$R0dlt), 13])

lag2 <- 11
cb.tarrival <- crossbasis(virus$totalarrival, lag=lag2, argvar=argvar,
                          arglag=list(fun=lagfun, df=lagdf),
                          cen=virus[which.min(virus$R0dlt), 11])
cb.narrival <- crossbasis(virus$nationalarrival, lag=lag2, argvar=argvar,
                          arglag=list(fun=lagfun, df=lagdf),
                          cen=virus[which.min(virus$R0dlt), 12])
cb.dose <- crossbasis(virus$dosePCA, lag=lag2, argvar=argvar,
                          arglag=list(fun=lagfun, df=lagdf),
                          cen=virus[which.min(virus$R0dlt), 10])


library(splines)
library(mgcv)
virus_model <- gam(R0dlt ~ te(as.numeric(as.Date(Date)), latitude, longitude, bs=c("cr","tp"), d=c(1,2), k=c(5, 5))
                    + cb.Temperature + cb.Humidity + cb.WindSpeed + cb.UV 
                    + cb.tarrival + cb.narrival + cb.dose
                    + s(Pressure)
                    + doctors + nonwhite + popdensity, 
                   data=virus,
                   family=gaussian(link="log"))
summary(virus_model)

##计算贡献度
library(FluMoDL)
attrback_temp <- attrdl(virus$Temperature, cb.Temperature, virus$R0dlt,
                        virus_model,tot=FALSE, type="af",
                        cen=virus[which.min(virus$R0dlt), 2])
attrback_humi <- attrdl(virus$Temperature, cb.Humidity, virus$R0dlt,
                        virus_model, tot=FALSE, type="af", 
                        cen=virus[which.min(virus$R0dlt), 3])
attrback_wind <- attrdl(virus$WindSpeed, cb.WindSpeed, virus$R0dlt,
                        virus_model, tot=FALSE, type="af",
                        cen=virus[which.min(virus$R0dlt), 4])
#attrback_pres <- attrdl(virus$Pressure, cb.Pressure, virus$R0dlt,
#                        virus_model, tot=FALSE, type="af",
#                        cen=virus[which.min(virus$R0dlt), 5])
attrback_UV <- attrdl(virus$UV, cb.UV, virus$R0dlt,
                      virus_model, tot=FALSE, type="af",
                      cen=virus[which.min(virus$R0dlt), 13])
attrback_tarrival <- attrdl(virus$totalarrival, cb.tarrival, virus$R0dlt,
                            virus_model, tot=FALSE, type="af",
                            cen=virus[which.min(virus$R0dlt), 11])
attrback_narrival <- attrdl(virus$nationalarrival, cb.narrival, virus$R0dlt,
                            virus_model, tot=FALSE, type="af",
                            cen=virus[which.min(virus$R0dlt), 12])
attrback_dose <- attrdl(virus$dosePCA, cb.narrival, virus$R0dlt,
                            virus_model, tot=FALSE, type="af",
                            cen=virus[which.min(virus$R0dlt), 12])

result <- cbind(virus, 
                attrback_temp, attrback_humi, attrback_wind, attrback_UV,
                attrback_tarrival, attrback_narrival, attrback_dose)
apply(result[ ,c(19:25)], 2, mean, na.rm=TRUE)

#预测气候因子影响，以最小R0为参照，用R0百分比变动来反映影响程度
#取预测气候因子最小值为参照，计算气候因子贡献度
#minR0 <- min(dlt_model$fitted.values)
pred_temp <- crosspred(cb.Temperature, virus_model, 
                       cen=virus[which.min(virus$R0dlt), 2], bylag=0.2)
png(file = "dlt_Temperature总暴露反应曲线.png", width = 3000, height = 1500, res = 300)
par(mar=c(3,3.5,2,1), mgp=c(1.7,0.5,0), omi=c(0,0,0,0),
    cex.axis=1.20,cex.main=1.5,cex.lab=1.3) #设定页边距
plot(pred_temp, "overall", col="#DC0000E5",
     xlab="Temperature (°F)",
     main="dlt",
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




## 可视化
afplot <- function(x){
  p <- plot(as.Date(result$Date), x,
            ylab="Attributable fraction",xlab="Date",
            ylim=c(-1, 1),
            cex.lab=1.5,
            axes=TRUE, frame.plot=FALSE, cex=0.8, pch=22, bg="#4DBBD5E5",
            main="dlt")
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


## 结果输出
result <- merge.data.frame(city_model, dltresult)
write.table(cityresult, "Adur_AF.csv", row.names=FALSE, col.names=TRUE, sep=",")

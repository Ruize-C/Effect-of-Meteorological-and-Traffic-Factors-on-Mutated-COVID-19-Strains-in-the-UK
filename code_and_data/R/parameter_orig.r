# 初始化
library(mgcv)
library(dlnm)
library(tidyverse)
library(FluMoDL)

city <- read.csv(choose.files())
colnames(city) <- c("Date","Temperature","Humidity","WindSpeed","Pressure",
                    "R0orig","R0alp","R0dlt","R0omi",
                    "dosePCA","totalarrival","nationalarrival","UV",
                    "male","female","doctors","nonwhite","popdensity","latitude","longitude")

city_model <- city

city_model <- city_model[-which(is.na(city_model$totalarrival |city_model$nationalarrival | city_model$UV)),]

#####run the model
###orig

virus <-
  city_model %>%
  slice(45:275) %>%
  filter(R0orig>0 & R0orig<3) 

rownames <- c("Tempreture","Humidity","Windspeed")
colnames <- c("vardf","lag","lagdf")

M <- matrix(c(3,14,3,3,14,3,3,14,3), nrow = 3, byrow = TRUE,dimnames = list(rownames,colnames))
aic <- c(rep(0, 30))
varfun <- "ns"
lagfun <- "ns"

cb.UV <- crossbasis(virus$UV, 
                    lag=16,
                    argvar=list(fun=lagfun, df=3),
                    arglag=list(fun=lagfun, df=3),
                    cen=virus[which.min(virus$R0orig), 13])

cb.tarrival <- crossbasis(virus$totalarrival, 
                          lag=7, 
                          argvar=list(fun=lagfun, df=3),
                          arglag=list(fun=lagfun, df=3),
                          cen=virus[which.min(virus$R0orig), 11])

cb.narrival <- crossbasis(virus$nationalarrival, 
                          lag=7, 
                          argvar=list(fun=lagfun, df=3),
                          arglag=list(fun=lagfun, df=3),
                          cen=virus[which.min(virus$R0orig), 12])

# Cyclic tuning of three parameters of air temperature, humidity and wind speed.
for (i in rownames){
  
  # change vardf
  aic_vardf <- c(rep(0,9))
  for (j in 2:10)
    {
    
    M[i,1] <- j
    
    cb.Temperature <- crossbasis(virus$Temperature, 
                                 lag=M[1,2], 
                                 argvar=list(fun = varfun, df = M[1,1]),
                                 arglag=list(fun=lagfun, df=M[1,3]), 
                                 cen=virus[which.min(virus$R0orig), 2])
    
    cb.Humidity <- crossbasis(virus$Humidity, 
                              lag=M[2,2], 
                              argvar=list(fun = varfun, df = M[2,1]),
                              arglag=list(fun=lagfun, df=M[2,3]),
                              cen=virus[which.min(virus$R0orig), 3])
                              
    cb.WindSpeed <- crossbasis(virus$WindSpeed, 
                               lag=M[3,2],
                               argvar=list(fun = varfun, df = M[3,1]),
                                arglag=list(fun = lagfun, df=M[3,3]),
                                cen=virus[which.min(virus$R0orig), 4])
                                                         
    virus_model <- gam(R0orig ~ te(as.numeric(as.Date(Date)), latitude, longitude, bs=c("cr","tp"), d=c(1,2), k=c(5, 5))
                      + cb.Temperature + cb.Humidity + cb.WindSpeed + cb.UV 
                      + cb.tarrival + cb.narrival + s(Pressure)
                      + doctors + nonwhite + popdensity, 
                      data=virus,
                      family=gaussian(link="log"))
    aic_vardf[j-1] <- AIC(virus_model)
  }
  M[i,1] <- which.min(aic_vardf)+1
  
  # change lag
  aic_lag <- c(rep(0,30))
  for (j in 1:30)
  {
    
    M[i,2] <- j
    
    cb.Temperature <- crossbasis(virus$Temperature, 
                                 lag=M[1,2], 
                                 argvar=list(fun = varfun, df = M[1,1]),
                                 arglag=list(fun=lagfun, df=M[1,3]), 
                                 cen=virus[which.min(virus$R0orig), 2])
    
    cb.Humidity <- crossbasis(virus$Humidity, 
                              lag=M[2,2], 
                              argvar=list(fun = varfun, df = M[2,1]),
                              arglag=list(fun=lagfun, df=M[2,3]),
                              cen=virus[which.min(virus$R0orig), 3])
    
    cb.WindSpeed <- crossbasis(virus$WindSpeed, 
                               lag=M[3,2],
                               argvar=list(fun = varfun, df = M[3,1]),
                               arglag=list(fun = lagfun, df=M[3,3]),
                               cen=virus[which.min(virus$R0orig), 4])
    
    virus_model <- gam(R0orig ~ te(as.numeric(as.Date(Date)), latitude, longitude, bs=c("cr","tp"), d=c(1,2), k=c(5, 5))
                       + cb.Temperature + cb.Humidity + cb.WindSpeed + cb.UV 
                       + cb.tarrival + cb.narrival + s(Pressure)
                       + doctors + nonwhite + popdensity, 
                       data=virus,
                       family=gaussian(link="log"))
    aic_lag[j] <- AIC(virus_model)
  }
  M[i,2] <- which.min(aic_lag)

  # change lagdf
  aic_lagdf <- c(rep(0,4))
  for (j in 2:5)
  {
    
    M[i,3] <- j
    
    cb.Temperature <- crossbasis(virus$Temperature, 
                                 lag=M[1,2], 
                                 argvar=list(fun = varfun, df = M[1,1]),
                                 arglag=list(fun=lagfun, df=M[1,3]), 
                                 cen=virus[which.min(virus$R0orig), 2])
    
    cb.Humidity <- crossbasis(virus$Humidity, 
                              lag=M[2,2], 
                              argvar=list(fun = varfun, df = M[2,1]),
                              arglag=list(fun=lagfun, df=M[2,3]),
                              cen=virus[which.min(virus$R0orig), 3])
    
    cb.WindSpeed <- crossbasis(virus$WindSpeed, 
                               lag=M[3,2],
                               argvar=list(fun = varfun, df = M[3,1]),
                               arglag=list(fun = lagfun, df=M[3,3]),
                               cen=virus[which.min(virus$R0orig), 4])
    
    virus_model <- gam(R0orig ~ te(as.numeric(as.Date(Date)), latitude, longitude, bs=c("cr","tp"), d=c(1,2), k=c(5, 5))
                       + cb.Temperature + cb.Humidity + cb.WindSpeed + cb.UV 
                       + cb.tarrival + cb.narrival + s(Pressure)
                       + doctors + nonwhite + popdensity, 
                       data=virus,
                       family=gaussian(link="log"))
    aic_lagdf[j-1] <- AIC(virus_model)
  }
  M[i,3] <- which.min(aic_lagdf)+1
}
print(M)
cb.Temperature <- crossbasis(virus$Temperature, 
                             lag=M[1,2], 
                             argvar=list(fun = varfun, df = M[1,1]),
                             arglag=list(fun=lagfun, df=M[1,3]), 
                             cen=virus[which.min(virus$R0orig), 2])

cb.Humidity <- crossbasis(virus$Humidity, 
                          lag=M[2,2], 
                          argvar=list(fun = varfun, df = M[2,1]),
                          arglag=list(fun=lagfun, df=M[2,3]),
                          cen=virus[which.min(virus$R0orig), 3])

cb.WindSpeed <- crossbasis(virus$WindSpeed, 
                           lag=M[3,2],
                           argvar=list(fun = varfun, df = M[3,1]),
                           arglag=list(fun = lagfun, df=M[3,3]),
                           cen=virus[which.min(virus$R0orig), 4])

virus_model <- gam(R0orig ~ te(as.numeric(as.Date(Date)), latitude, longitude, bs=c("cr","tp"), d=c(1,2), k=c(5, 5))
                   + cb.Temperature + cb.Humidity + cb.WindSpeed + cb.UV 
                   + cb.tarrival + cb.narrival + s(Pressure)
                   + doctors + nonwhite + popdensity, 
                   data=virus,
                   family=gaussian(link="log"))
summary(virus_model)

key<-FALSE


attrback_temp <- attrdl(virus$Temperature, cb.Temperature, virus$R0orig,
                        virus_model,tot=key, type="af",
                        cen=virus[which.min(virus$R0orig), 2])
attrback_humi <- attrdl(virus$Temperature, cb.Humidity, virus$R0orig,
                        virus_model, tot=key, type="af", 
                        cen=virus[which.min(virus$R0orig), 3])
attrback_wind <- attrdl(virus$WindSpeed, cb.WindSpeed, virus$R0orig,
                        virus_model, tot=key, type="af",
                        cen=virus[which.min(virus$R0orig), 4])
attrback_UV <- attrdl(virus$UV, cb.UV, virus$R0orig,
                      virus_model, tot=key, type="af",
                      cen=virus[which.min(virus$R0orig), 13])
attrback_tarrival <- attrdl(virus$totalarrival, cb.tarrival, virus$R0orig,
                            virus_model, tot=key, type="af",
                            cen=virus[which.min(virus$R0orig), 11])
attrback_narrival <- attrdl(virus$nationalarrival, cb.narrival, virus$R0orig,
                            virus_model, tot=key, type="af",
                            cen=virus[which.min(virus$R0orig), 12])

result <- cbind(virus, 
                attrback_temp, attrback_humi, attrback_wind, attrback_UV,
                attrback_tarrival, attrback_narrival)
apply(result[ ,c(21:26)], 2,mean, na.rm=TRUE)


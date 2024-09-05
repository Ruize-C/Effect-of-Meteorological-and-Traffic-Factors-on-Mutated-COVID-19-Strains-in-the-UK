library(tidyverse)
library(dlnm)
library(splines)
library(mgcv)
library(FluMoDL)


af_allcity <- as.data.frame(matrix(nrow=0, ncol=5))
colnames(af_allcity) <- c("Temperature", "Humidity", "WindSpeed", "UV", "TotalArrival")

temp <- list.files(path="../data/all_save_data", pattern="*.csv")

for(i in 1:length(temp)){
  file <- read.csv(temp[i], header=TRUE, fileEncoding="CP936")
  
  filename <- paste0(getwd(),"/", temp[i])
  nfilename <- nchar(basename(filename))
  cityname <- substr(basename(filename), start=1, stop=nfilename-4)
  
  colnames(file) <- c("Date","Temperature","Humidity","WindSpeed","Pressure",
                      "R0orig","R0alp","R0dlt","R0omi",
                      "UV","totalarrival","nationalarrival",
                      "popdensity","latitude","longitude")
  city_model <- file
  city_model <- city_model[-which(is.na(city_model$totalarrival |city_model$nationalarrival | city_model$UV)),]
  
  
  ##### main model
  # exposure function
  varfun = "ns"
  vardf=3
  argvar <- list(fun=varfun, df=vardf)
  
  # lag function
  lag <- 13
  lagfun="ns"
  lagdf=3
  
  # SARS-Cov-2
  virus <-
    city_model %>%
    slice(1:355) %>%
    filter(R0orig>0 & R0orig<5) 
  # alp
  virus <-
    city_model %>%
    slice(275:519) %>%
    filter(R0alp>0 & R0alp<7) 
  # dlt
  virus <-
    city_model %>%
    slice(446:727) %>%
    filter(R0dlt>0 & R0dlt<9) 
  # omi
  virus <-
    city_model %>%
    slice(634:840) %>%
    filter(R0omi>0 & R0omi<12) 
  
  # crossbasis
  cb.Temperature <- crossbasis(virus$Temperature, lag=lag, argvar=argvar,
                               arglag=list(fun=lagfun, df=lagdf), 
                               cen=virus[which.min(virus$R0orig), 2])
  cb.Humidity <- crossbasis(virus$Humidity, lag=lag, argvar=argvar,
                            arglag=list(fun=lagfun, df=lagdf),
                            cen=virus[which.min(virus$R0orig), 3])
  cb.WindSpeed <- crossbasis(virus$WindSpeed, lag=lag,argvar=argvar,
                             arglag=list(fun=lagfun, df=lagdf),
                             cen=virus[which.min(virus$R0orig), 4])
  cb.UV <- crossbasis(virus$UV, lag=lag,argvar=argvar,
                      arglag=list(fun=lagfun, df=lagdf),
                      cen=virus[which.min(virus$R0orig), 11])
  
  lag2 <- 7
  cb.tarrival <- crossbasis(virus$totalarrival, lag=lag2, argvar=argvar,
                            arglag=list(fun=lagfun, df=lagdf),
                            cen=virus[which.min(virus$R0orig), 12])
  cb.narrival <- crossbasis(virus$nationalarrival, lag=lag2, argvar=argvar,
                            arglag=list(fun=lagfun, df=lagdf),
                            cen=virus[which.min(virus$R0orig), 13])
  
  virus_model <- gam(R0orig ~ te(as.numeric(as.Date(Date)), latitude, longitude, bs=c("cr","tp"), d=c(1,2), k=c(5, 5))
                     + cb.Temperature + cb.Humidity + cb.WindSpeed + cb.UV 
                     + cb.tarrival + cb.narrival + s(Pressure)
                     + popdensity, 
                     data=virus,
                     family=gaussian(link="log"))
  summary(virus_model)
  
  
  ##### exposure-response
  pred.temp <- crosspred(cb.Temperature, virus_model, cen=virus[which.min(virus$R0orig), 2], bylag=0.2)
  pred.humi <- crosspred(cb.Humidity, virus_model, cen=virus[which.min(virus$R0orig), 3], bylag=0.2)
  pred.wind <- crosspred(cb.WindSpeed, virus_model, cen=virus[which.min(virus$R0orig), 4], bylag=0.2)
  pred.UV <- crosspred(cb.UV, virus_model, cen=virus[which.min(virus$R0orig), 11], bylag=0.2)
  pred.tarrival <- crosspred(cb.tarrival, virus_model, cen=virus[which.min(virus$R0orig), 12], bylag=0.2)
  
  
  ##### attributable fractions
  attrback_temp <- attrdl(virus$Temperature, cb.Temperature, virus$R0orig,
                          virus_model,tot=FALSE, type="af",
                          cen=virus[which.min(virus$R0orig), 2])
  attrback_humi <- attrdl(virus$Temperature, cb.Humidity, virus$R0orig,
                          virus_model, tot=FALSE, type="af", 
                          cen=virus[which.min(virus$R0orig), 3])
  attrback_wind <- attrdl(virus$WindSpeed, cb.WindSpeed, virus$R0orig,
                          virus_model, tot=FALSE, type="af",
                          cen=virus[which.min(virus$R0orig), 4])
  attrback_UV <- attrdl(virus$UV, cb.UV, virus$R0orig,
                        virus_model, tot=FALSE, type="af",
                        cen=virus[which.min(virus$R0orig), 11])
  attrback_tarrival <- attrdl(virus$totalarrival, cb.tarrival, virus$R0orig,
                              virus_model, tot=FALSE, type="af",
                              cen=virus[which.min(virus$R0orig), 12])
  attrback_narrival <- attrdl(virus$nationalarrival, cb.narrival, virus$R0orig,
                              virus_model, tot=FALSE, type="af",
                              cen=virus[which.min(virus$R0orig), 13])
  
  result <- cbind(virus, 
                  attrback_temp, attrback_humi, attrback_wind, attrback_UV,
                  attrback_tarrival, attrback_narrival)
  af <- t(as.data.frame(apply(result[ ,c(19:24)], 2, mean, na.rm=TRUE)))
  row.names(af) <- cityname
  
  af_allcity <- rbind(af_allcity, af)
}

write.table(af_allcity, "finalresult/af_allcity.csv", row.names=TRUE, col.names=FALSE, sep=",")
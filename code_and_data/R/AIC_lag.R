### aic for environment
aic <- c(rep(0, 30))
for(i in 1:30){
  varfun = "ns"
  vardf=3
  argvar <- list(fun=varfun, df=vardf)
  
  lag <- i
  lagfun="ns"
  lagdf=3

  cb.Temperature <- crossbasis(virus$Temperature, lag=lag, argvar=argvar,
                               arglag=list(fun=lagfun, df=lagdf), 
                               cen=virus[which.min(virus$R0alp), 2])
  cb.Humidity <- crossbasis(virus$Humidity, lag=lag, argvar=argvar,
                            arglag=list(fun=lagfun, df=lagdf),
                            cen=virus[which.min(virus$R0alp), 3])
  cb.WindSpeed <- crossbasis(virus$WindSpeed, lag=lag,argvar=argvar,
                             arglag=list(fun=lagfun, df=lagdf),
                             cen=virus[which.min(virus$R0alp), 4])
  cb.UV <- crossbasis(virus$UV, lag=lag,argvar=argvar,
                      arglag=list(fun=lagfun, df=lagdf),
                      cen=virus[which.min(virus$R0alp), 13])
  
  virus_model <- gam(R0alp ~ te(as.numeric(as.Date(Date)), latitude, longitude, bs=c("cr","tp"), d=c(1,2), k=c(5, 5))
                     + cb.Temperature + cb.Humidity + cb.WindSpeed + cb.UV 
                     + cb.tarrival + cb.narrival + s(Pressure)
                     + doctors + nonwhite + popdensity, 
                     data=virus,
                     family=gaussian(link="log"))
  aic[i] <- AIC(virus_model)
}

aic2 <- c(rep(0, 30))
for(j in 1:30){
  lag2 <- j
  
  cb.tarrival <- crossbasis(virus$totalarrival, lag=lag2, argvar=argvar,
                            arglag=list(fun=lagfun, df=lagdf),
                            cen=virus[which.min(virus$R0alp), 11])
  cb.narrival <- crossbasis(virus$nationalarrival, lag=lag2, argvar=argvar,
                            arglag=list(fun=lagfun, df=lagdf),
                            cen=virus[which.min(virus$R0alp), 12])
  
  virus_model <- gam(R0alp ~ te(as.numeric(as.Date(Date)), latitude, longitude, bs=c("cr","tp"), d=c(1,2), k=c(5, 5))
                     + cb.Temperature + cb.Humidity + cb.WindSpeed + cb.UV 
                     + cb.tarrival + cb.narrival + s(Pressure)
                     + doctors + nonwhite + popdensity, 
                     data=virus,
                     family=gaussian(link="log"))
  aic2[j] <- AIC(virus_model)
}

library(dlnm)
library(splines)
fit <- lm(R0alp ~ ns(Temperature, df=4), data=virus)
xlim <- range(virus$Temperature)
x_grid <- seq(xlim[1],xlim[2])
preds <- predict(fit, newdata=list(Temperature=x_grid), se.fit=T)
plot(virus$Temperature, virus$R0alp, col="gray",
     cex.axis=0.8,cex.lab=0.8)
lines(x_grid, preds$fit, col='blue')

nsdf <- function(x, df){
  fit <- lm(virus$R0alp ~ ns(x, df=df))
  xlim <- range(x)
  x_grid <- seq(xlim[1],xlim[2])
  preds <- predict(fit, newdata=list(x=x_grid), se.fit=T)
  p <- plot(x, virus$R0alp, col="gray",
       cex.axis=0.8,cex.lab=0.8)
  lines(x_grid, preds$fit, col='blue')
  return(p)
}

nsdf(virus$Temperature, df=6)
nsdf(virus$Humidity, df=6)
nsdf(virus$WindSpeed, df=6)
nsdf(virus$Pressure, df=3)  #È¡³É Æ½»¬s
nsdf(virus$UV, df=2)
nsdf(virus$totalarrival, df=2)
nsdf(virus$nationalarrival, df=2)

longitude <- rnorm(226, -0.2858964, sd=0.003^2)

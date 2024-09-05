library(tidyverse)
library(ggplot2)
library(ggpubr)


city <- read.csv("Adur.csv", header=TRUE, fileEncoding="cp936")
colnames(city) <- c("Date","Temperature","Humidity","WindSpeed","Pressure",
                    "R0orig","R0alp","R0dlt","R0omi",
                    "dosePCA","UV","totalarrival","nationalarrival",
                    "popdensity","latitude","longitude")

city_model <- city

city_model <- city_model[-which(is.na(city_model$totalarrival |city_model$nationalarrival | city_model$UV)),]

###orig
orig <-
  city_model %>%
  slice(1:355) %>%
  filter(R0orig>0 & R0orig<5) 

p11 <- 
  ggplot(data=orig,
       mapping=aes(x=Temperature, y=R0orig)) +
  labs(x=expression(paste("Temperature(", degree, " F)")), y="R0orig") +
  geom_point() +
  geom_smooth(method="loess")
p12 <-
  ggplot(data=orig,
       mapping=aes(x=Humidity, y=R0orig)) +
  labs(x="Humidity(%)", y="") +
  geom_point() +
  geom_smooth(method="loess")
p13 <-
  ggplot(data=orig,
       mapping=aes(x=WindSpeed, y=R0orig)) +
  labs(x="Wind Speed(mph)", y="") +
  geom_point() +
  geom_smooth(method="loess")
p14 <-
  ggplot(data=orig,
       mapping=aes(x=Pressure, y=R0orig)) +
  labs(x="Pressure(inHg)", y="") +
  geom_point() +
  geom_smooth(method="loess")
p15 <-
  ggplot(data=orig,
         mapping=aes(x=totalarrival/10000, y=R0orig)) +
  labs(x="Total Arrival", y="") +
  geom_point() +
  geom_smooth(method="loess")
p16 <-
  ggplot(data=orig,
         mapping=aes(x=UV, y=R0orig)) +
  labs(x="UV", y="") +
  geom_point() +
  geom_smooth(method="loess")


Adur_orig <- ggarrange(p11,p12,p13,p14,p15,p16, 
                            nrow=2, ncol=3)
png(file="Adur_orig.png", width=1200, height=500, res=120)
Adur_orig
dev.off()


###alp
alp <-
  city_model %>%
  slice(275:519) %>%
  filter(R0alp>0 & R0alp<7) 

p21 <- 
  ggplot(data=alp,
         mapping=aes(x=Temperature, y=R0alp)) +
  labs(x=expression(paste("Temperature(", degree, " F)")), y="R0alp") +
  geom_point() +
  geom_smooth(method="loess")
p22 <-
  ggplot(data=alp,
         mapping=aes(x=Humidity, y=R0alp)) +
  labs(x="Humidity(%)", y="") +
  geom_point() +
  geom_smooth(method="loess")
p23 <-
  ggplot(data=alp,
         mapping=aes(x=WindSpeed, y=R0alp)) +
  labs(x="Wind Speed(mph)", y="") +
  geom_point() +
  geom_smooth(method="loess")
p24 <-
  ggplot(data=alp,
         mapping=aes(x=Pressure, y=R0alp)) +
  labs(x="Pressure(inHg)", y="") +
  geom_point() +
  geom_smooth(method="loess")
p25 <-
  ggplot(data=alp,
         mapping=aes(x=totalarrival/10000, y=R0alp)) +
  labs(x="Total Arrival", y="") +
  geom_point() +
  geom_smooth(method="loess")
p26 <-
  ggplot(data=alp,
         mapping=aes(x=UV, y=R0alp)) +
  labs(x="UV", y="") +
  geom_point() +
  geom_smooth(method="loess")


Adur_alp <- ggarrange(p21,p22,p23,p24,p25,p26, 
                            nrow=2, ncol=3)
png(file="Adur_alp.png", width=1200, height=500, res=120)
Adur_alp
dev.off()


###dlt
dlt <-
  city_model %>%
  slice(446:727) %>%
  filter(R0dlt>0 & R0dlt<9) 

p31 <- 
  ggplot(data=dlt,
         mapping=aes(x=Temperature, y=R0dlt)) +
  labs(x=expression(paste("Temperature(", degree, " F)")), y="R0dlt") +
  geom_point() +
  geom_smooth(method="loess")
p32 <-
  ggplot(data=dlt,
         mapping=aes(x=Humidity, y=R0dlt)) +
  labs(x="Humidity(%)", y="") +
  geom_point() +
  geom_smooth(method="loess")
p33 <-
  ggplot(data=dlt,
         mapping=aes(x=WindSpeed, y=R0dlt)) +
  labs(x="Wind Speed(mph)", y="") +
  geom_point() +
  geom_smooth(method="loess")
p34 <-
  ggplot(data=dlt,
         mapping=aes(x=Pressure, y=R0dlt)) +
  labs(x="Pressure(inHg)", y="") +
  geom_point() +
  geom_smooth(method="loess")
p35 <-
  ggplot(data=dlt,
         mapping=aes(x=totalarrival/10000, y=R0dlt)) +
  labs(x="Total Arrival", y="") +
  geom_point() +
  geom_smooth(method="loess")
p36 <-
  ggplot(data=dlt,
         mapping=aes(x=UV, y=R0dlt)) +
  labs(x="UV", y="") +
  geom_point() +
  geom_smooth(method="loess")
p37 <-
  ggplot(data=dlt,
         mapping=aes(x=dosePCA/1000, y=R0dlt)) +
  labs(x="dose", y="") +
  geom_point() +
  geom_smooth(method="loess")


Adur_dlt <- ggarrange(p31,p32,p33,p34,p35,p36,p37,
                           nrow=2, ncol=4)
png(file="Adur_dlt.png", width=1200, height=500, res=120)
Adur_dlt
dev.off()


###omi
omi <-
  city_model %>%
  slice(664:840) %>%
  filter(R0omi>0 & R0omi<12) 

p41 <- 
  ggplot(data=omi,
         mapping=aes(x=Temperature, y=R0omi)) +
  labs(x=expression(paste("Temperature(", degree, " F)")), y="R0omi") +
  geom_point() +
  geom_smooth(method="loess")
p42 <-
  ggplot(data=omi,
         mapping=aes(x=Humidity, y=R0omi)) +
  labs(x="Humidity(%)", y="") +
  geom_point() +
  geom_smooth(method="loess")
p43 <-
  ggplot(data=omi,
         mapping=aes(x=WindSpeed, y=R0omi)) +
  labs(x="Wind Speed(mph)", y="") +
  geom_point() +
  geom_smooth(method="loess")
p44 <-
  ggplot(data=omi,
         mapping=aes(x=Pressure, y=R0omi)) +
  labs(x="Pressure(inHg)", y="") +
  geom_point() +
  geom_smooth(method="loess")
p45 <-
  ggplot(data=omi,
         mapping=aes(x=totalarrival/10000, y=R0omi)) +
  labs(x="Total Arrival", y="") +
  geom_point() +
  geom_smooth(method="loess")
p46 <-
  ggplot(data=omi,
         mapping=aes(x=UV, y=R0omi)) +
  labs(x="UV", y="") +
  geom_point() +
  geom_smooth(method="loess")
p47 <-
  ggplot(data=omi,
         mapping=aes(x=dosePCA/1000, y=R0omi)) +
  labs(x="dose", y="") +
  geom_point() +
  geom_smooth(method="loess")

Adur_omi <- ggarrange(p41,p42,p43,p44,p45,p46,p47,
                      nrow=2, ncol=4)
png(file="Adur_omi.png", width=1200, height=500, res=120)
Adur_omi
dev.off()


library(grid)
grid.newpage()  ##新建页面
pushViewport(viewport(layout = grid.layout(4,7))) #将页面分成4*7矩阵
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}
print(p11, vp = vplayout(1,1))
print(p12, vp = vplayout(1,2))
print(p13, vp = vplayout(1,3))
print(p14, vp = vplayout(1,4))
print(p15, vp = vplayout(1,5))
print(p16, vp = vplayout(1,6))

print(p21, vp = vplayout(2,1))
print(p22, vp = vplayout(2,2))
print(p23, vp = vplayout(2,3))
print(p24, vp = vplayout(2,4))
print(p25, vp = vplayout(2,5))
print(p26, vp = vplayout(2,6))

print(p31, vp = vplayout(3,1))
print(p32, vp = vplayout(3,2))
print(p33, vp = vplayout(3,3))
print(p34, vp = vplayout(3,4))
print(p35, vp = vplayout(3,5))
print(p36, vp = vplayout(3,6))
print(p37, vp = vplayout(3,7))

print(p41, vp = vplayout(4,1))
print(p42, vp = vplayout(4,2))
print(p43, vp = vplayout(4,3))
print(p44, vp = vplayout(4,4))
print(p45, vp = vplayout(4,5))
print(p46, vp = vplayout(4,6))
print(p47, vp = vplayout(4,7))



p11 <- 
  ggplot(data=orig,
         mapping=aes(x=Temperature, y=R0orig)) +
  labs(x=expression(paste("Temperature(", degree, " F)")), y="R0orig") +
  scale_x_continuous(limits=c(30, 80)) +
  geom_smooth(method="loess", fill="#BEE7E9") +
  theme_bw()
p12 <-
  ggplot(data=orig,
         mapping=aes(x=Humidity, y=R0orig)) +
  labs(x="Humidity(%)", y="") +
  scale_x_continuous(limits=c(50, 100)) +
  geom_smooth(method="loess", fill="#BEE7E9") +
  theme_bw()
p13 <-
  ggplot(data=orig,
         mapping=aes(x=WindSpeed, y=R0orig)) +
  labs(x="Wind Speed(mph)", y="") +
  scale_x_continuous(limits=c(0, 20)) +
  geom_smooth(method="loess", fill="#BEE7E9") +
  theme_bw()
p14 <-
  ggplot(data=orig,
         mapping=aes(x=Pressure, y=R0orig)) +
  labs(x="Pressure(inHg)", y="") +
  scale_x_continuous(limits=c(28.5, 30.5)) +
  geom_smooth(method="loess", fill="#BEE7E9") +
  theme_bw()
p15 <-
  ggplot(data=orig,
         mapping=aes(x=totalarrival/10000, y=R0orig)) +
  labs(x="Total Arrival", y="") +
  scale_x_continuous(limits=c(0, 30)) +
  geom_smooth(method="loess", fill="#BEE7E9") +
  theme_bw()
p16 <-
  ggplot(data=orig,
         mapping=aes(x=UV, y=R0orig)) +
  labs(x="UV", y="") +
  scale_x_continuous(limits=c(0, 3)) +
  geom_smooth(method="loess", fill="#BEE7E9") +
  theme_bw()

p21 <- 
  ggplot(data=alp,
         mapping=aes(x=Temperature, y=R0alp)) +
  labs(x=expression(paste("Temperature(", degree, " F)")), y="R0alp") +
  scale_x_continuous(limits=c(30, 80)) +
  geom_smooth(method="loess", fill="#BEE7E9") +
  theme_bw()
p22 <-
  ggplot(data=alp,
         mapping=aes(x=Humidity, y=R0alp)) +
  labs(x="Humidity(%)", y="") +
  scale_x_continuous(limits=c(50, 100)) +
  geom_smooth(method="loess", fill="#BEE7E9") +
  theme_bw()
p23 <-
  ggplot(data=alp,
         mapping=aes(x=WindSpeed, y=R0alp)) +
  labs(x="Wind Speed(mph)", y="") +
  scale_x_continuous(limits=c(0, 20)) +
  geom_smooth(method="loess", fill="#BEE7E9") +
  theme_bw()
p24 <-
  ggplot(data=alp,
         mapping=aes(x=Pressure, y=R0alp)) +
  labs(x="Pressure(inHg)", y="") +
  scale_x_continuous(limits=c(28.5, 30.5)) +
  geom_smooth(method="loess", fill="#BEE7E9") +
  theme_bw()
p25 <-
  ggplot(data=alp,
         mapping=aes(x=totalarrival/10000, y=R0alp)) +
  labs(x="Total Arrival", y="") +
  scale_x_continuous(limits=c(0, 30)) +
  geom_smooth(method="loess", fill="#BEE7E9") +
  theme_bw()
p26 <-
  ggplot(data=alp,
         mapping=aes(x=UV, y=R0alp)) +
  labs(x="UV", y="") +
  scale_x_continuous(limits=c(0, 3)) +
  geom_smooth(method="loess", fill="#BEE7E9") +
  theme_bw()

p31 <- 
  ggplot(data=dlt,
         mapping=aes(x=Temperature, y=R0dlt)) +
  labs(x=expression(paste("Temperature(", degree, " F)")), y="R0dlt") +
  scale_x_continuous(limits=c(30, 80)) +
  geom_smooth(method="loess", fill="#BEE7E9") +
  theme_bw()
p32 <-
  ggplot(data=dlt,
         mapping=aes(x=Humidity, y=R0dlt)) +
  labs(x="Humidity(%)", y="") +
  scale_x_continuous(limits=c(50, 100)) +
  geom_smooth(method="loess", fill="#BEE7E9") +
  theme_bw()
p33 <-
  ggplot(data=dlt,
         mapping=aes(x=WindSpeed, y=R0dlt)) +
  labs(x="Wind Speed(mph)", y="") +
  scale_x_continuous(limits=c(0, 20)) +
  geom_smooth(method="loess", fill="#BEE7E9") +
  theme_bw()
p34 <-
  ggplot(data=dlt,
         mapping=aes(x=Pressure, y=R0dlt)) +
  labs(x="Pressure(inHg)", y="") +
  scale_x_continuous(limits=c(28.5, 30.5)) +
  geom_smooth(method="loess", fill="#BEE7E9") +
  theme_bw()
p35 <-
  ggplot(data=dlt,
         mapping=aes(x=totalarrival/10000, y=R0dlt)) +
  labs(x="Total Arrival", y="") +
  scale_x_continuous(limits=c(0, 30)) +
  geom_smooth(method="loess", fill="#BEE7E9") +
  theme_bw()
p36 <-
  ggplot(data=dlt,
         mapping=aes(x=UV, y=R0dlt)) +
  labs(x="UV", y="") +
  scale_x_continuous(limits=c(0, 3)) +
  geom_smooth(method="loess", fill="#BEE7E9") +
  theme_bw()
p37 <-
  ggplot(data=dlt,
         mapping=aes(x=dosePCA/1000, y=R0dlt)) +
  labs(x="dose", y="") +
  geom_smooth(method="loess", fill="#BEE7E9") +
  theme_bw()

p41 <- 
  ggplot(data=omi,
         mapping=aes(x=Temperature, y=R0omi)) +
  labs(x=expression(paste("Temperature(", degree, " F)")), y="R0omi") +
  scale_x_continuous(limits=c(30, 80)) +
  geom_smooth(method="loess", fill="#BEE7E9") +
  theme_bw()
p42 <-
  ggplot(data=omi,
         mapping=aes(x=Humidity, y=R0omi)) +
  labs(x="Humidity(%)", y="") +
  scale_x_continuous(limits=c(50, 100)) +
  geom_smooth(method="loess", fill="#BEE7E9") +
  theme_bw()
p43 <-
  ggplot(data=omi,
         mapping=aes(x=WindSpeed, y=R0omi)) +
  labs(x="Wind Speed(mph)", y="") +
  scale_x_continuous(limits=c(0, 20)) +
  geom_smooth(method="loess", fill="#BEE7E9") +
  theme_bw()
p44 <-
  ggplot(data=omi,
         mapping=aes(x=Pressure, y=R0omi)) +
  labs(x="Pressure(inHg)", y="") +
  scale_x_continuous(limits=c(28.5, 30.5)) +
  geom_smooth(method="loess", fill="#BEE7E9") +
  theme_bw()
p45 <-
  ggplot(data=omi,
         mapping=aes(x=totalarrival/10000, y=R0omi)) +
  labs(x="Total Arrival", y="") +
  scale_x_continuous(limits=c(0, 30)) +
  geom_smooth(method="loess", fill="#BEE7E9") +
  theme_bw()
p46 <-
  ggplot(data=omi,
         mapping=aes(x=UV, y=R0omi)) +
  labs(x="UV", y="") +
  scale_x_continuous(limits=c(0, 3)) +
  geom_smooth(method="loess", fill="#BEE7E9") +
  theme_bw()
p47 <-
  ggplot(data=omi,
         mapping=aes(x=dosePCA/1000, y=R0omi)) +
  labs(x="dose", y="") +
  geom_smooth(method="loess", fill="#BEE7E9") +
  theme_bw()
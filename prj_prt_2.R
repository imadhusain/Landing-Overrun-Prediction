# step 0 create cleaned data set
#import data
library(readxl)
library(tidyverse)
library(magrittr)
FAA1 <- read_excel("FAA1.xls")
FAA2 <- read_excel("FAA2.xls")
#remove duplicates if any within data set
FAA1 <- unique(FAA1)
FAA2 <- unique(FAA2)
#finally create unique dataset
merged <- rbind(FAA1[,-2],FAA2)
merged <- unique(merged)
FAA <- merge(merged,FAA1,by=names(merged), all.x=TRUE)
#removing abnormal values
faa_clean <- FAA %>% select(names(FAA)) %>% 
  filter(replace(duration,is.na(duration),60) > 40 &
           (speed_ground > 30 &
              speed_ground < 140) &
           (replace(speed_air,is.na(speed_air),60) > 30 &
              replace(speed_air,is.na(speed_air),60) < 140) &
           height >= 6 &
           distance < 6000   )

#step 1 create binary variables 

faa <- faa_clean
faa$long.landing <- 0
faa$risky.landing <- 0

faa[which(faa$distance > 2500),"long.landing"] <- 1 
faa[which(faa$distance > 3000),"risky.landing"] <- 1 

nrow(faa[which(faa$distance > 2500),])
sum(faa$long.landing)
nrow(faa[which(faa$distance > 3000),])
sum(faa$risky.landing)

t(names(faa))
faa <- faa[,-7]

str(faa)

#step 2 hist

library(ggplot2)
ggplot(data = faa, aes(x = as.factor(long.landing))) +
geom_histogram(stat = "count")
     
table(faa$long.landing)

#step 3
t(names(faa))
faa$aircraft <- as.factor(faa$aircraft)
var_name <- rep('',7)
coeff <- rep(0,7)
odds_ratio <- rep(0,7)
direction <- rep('+',7)
p_val <- rep(0,7)
j <- 1

for(i in c(1,2,3,4,5,6,7)) {
  fit <- glm(long.landing ~ faa[,i],family=binomial(link='logit'),data=faa)
  var_name[j] <- names(faa)[i]
  coeff[j] <- abs(summary(fit)$coefficients[2,1])
  odds_ratio[j] <- exp(fit$coefficients[2])
    if(summary(fit)$coefficients[2,1] < 0) {direction[j] <- '-'}  
  p_val[j] <- summary(fit)$coefficients[2,4]
  j <- j+1
}
tt <- cbind(1:7,var_name,coeff,odds_ratio,direction,p_val)

#step 4
library(car)
names(faa)
scatterplotMatrix(~long.landing + no_pasg + speed_ground +
                    speed_air + height + pitch + duration, data <- faa,
                  regLine = F, ellipse = F, diagonal = F,smooth = F  )

par(mfrow = c(1,3))
plot(long.landing~speed_ground,data <- faa)
plot(long.landing~speed_air,data <- faa)
plot(long.landing~pitch,data <- faa)

par(mfrow = c(2,2))
plot( jitter(long.landing,0.1)~jitter(speed_ground),data <- faa,
      xlab = 'Speed Ground',ylab = 'Long Landing')
plot( jitter(long.landing,0.1)~jitter(speed_air),data <- faa,
      xlab = 'Speed Air',ylab = 'Long Landing')
plot( jitter(long.landing,0.1)~jitter(pitch),data <- faa,
      xlab = 'Pitch',ylab = 'Long Landing')
plot( jitter(long.landing,0.1)~jitter(as.numeric(aircraft)),data <- faa,
      xlab = 'Aircraft',ylab = 'Long Landing')

#install.packages("ggpubr")
library(ggpubr)
g_ground <- ggplot(data <- faa,aes(x=speed_ground,fill=factor(long.landing)))+
  geom_density(position="dodge",binwidth=5,aes(y=..density..,
       colour=factor(long.landing)),alpha = 0.5)

g_air <- ggplot(data <- faa,aes(x=speed_air,fill=factor(long.landing)))+
  geom_density(position="dodge",binwidth=5,aes(y=..density..,
   colour=factor(long.landing)),alpha = 0.5)

g_pitch <- ggplot(data <- faa,aes(x=pitch,fill=factor(long.landing)))+
  geom_density(position="dodge",binwidth=5,aes(y=..density..,
   colour=factor(long.landing)),alpha = 0.5)

g_aircraft <- ggplot(data <- faa,aes(x=aircraft,fill=factor(long.landing)))+
  geom_density(position="dodge",binwidth=5,aes(y=..density..,
  colour=factor(long.landing)),alpha = 0.5)

ggarrange(g_ground,g_air,g_pitch,g_aircraft, ncol = 2, nrow = 2)

#step 5
fit <- glm(long.landing ~ aircraft + speed_ground + pitch,
           family=binomial(link='logit'),data=faa)

summary(fit)

#step 6
null.model <- glm(long.landing ~ 1 , family=binomial(link='logit'),data=faa)
full.model <- glm(long.landing ~ aircraft + no_pasg + speed_ground + height +
                    pitch+ duration ,family=binomial(link='logit'),data=faa)
AIC.model <- step(null.model, scope=list(lower=null.model, upper=full.model),
                     direction='forward',k=2)
summary(AIC.model)

BIC(AIC.model)

#step 7
BIC.model <- step(null.model, scope=list(lower=null.model, upper=full.model),
                  direction='forward',k=log(nrow(faa)))
summary(BIC.model)
BIC(BIC.model)
#step 8

g_craft <- ggplot(data <- faa,aes(x=factor(aircraft),y=factor(long.landing)))+
  geom_jitter(position="jitter",aes(colour=factor(long.landing)),alpha = 0.5);

g_height <- ggplot(data <- faa,aes(x=height,y=factor(long.landing)))+
  geom_jitter(position="jitter",binwidth=5,aes(
                                               colour=factor(long.landing)),alpha = 0.5);

g_ground <- ggplot(data <- faa,aes(x=speed_ground,fill=factor(long.landing)))+
  geom_density(position="dodge",binwidth=5,aes(y=..density..,
                                               colour=factor(long.landing)),alpha = 0.5)

ggarrange(g_height,g_ground, g_craft,ncol = 2, nrow = 2)

exp(BIC.model$coefficients)[-1]


######## Risky landing

#step 2 hist

library(ggplot2)
ggplot(data = faa, aes(x = as.factor(risky.landing))) +
  geom_histogram(stat =    "count")

table(faa$risky.landing)

#step 3
t(names(faa))
faa$aircraft <- as.factor(faa$aircraft)
var_name <- rep('',7)
coeff <- rep(0,7)
odds_ratio <- rep(0,7)
direction <- rep('+',7)
p_val <- rep(0,7)
j <- 1

for(i in c(1,2,3,4,5,6,7)) {
  fit <- glm(risky.landing ~ faa[,i],family=binomial(link='logit'),data=faa)
  var_name[j] <- names(faa)[i]
  coeff[j] <- abs(summary(fit)$coefficients[2,1])
  odds_ratio[j] <- exp(fit$coefficients[2])
  if(summary(fit)$coefficients[2,1] < 0) {direction[j] <- '-'}  
  p_val[j] <- summary(fit)$coefficients[2,4]
  j <- j+1
}
tt <- cbind(1:7,var_name,coeff,odds_ratio,direction,p_val)

#step 4
library(car)
names(faa)
scatterplotMatrix(~risky.landing + no_pasg + speed_ground +
                    speed_air + height + pitch + duration, data <- faa,
                  regLine = F, ellipse = F, diagonal = F,smooth = F  )

library(ggpubr)
g_ground <- ggplot(data <- faa,aes(x=speed_ground,fill=factor(risky.landing)))+
  geom_density(position="dodge",binwidth=5,aes(y=..density..,
                                               colour=factor(risky.landing)),alpha = 0.5)

g_air <- ggplot(data <- faa,aes(x=speed_air,fill=factor(risky.landing)))+
  geom_density(position="dodge",binwidth=5,aes(y=..density..,
                                               colour=factor(risky.landing)),alpha = 0.5)

g_craft <- ggplot(data <- faa,aes(x=factor(aircraft),y=factor(risky.landing)))+
  geom_jitter(position="jitter",aes(colour=factor(risky.landing)),alpha = 0.5);

ggarrange(g_air,g_ground, g_craft,ncol = 3, nrow = 1)

#step 5
fit <- glm(risky.landing ~ aircraft + speed_ground ,
           family=binomial(link='logit'),data=faa)

summary(fit)

#step 6
null.model <- glm(risky.landing ~ 1 , family=binomial(link='logit'),data=faa)
full.model <- glm(risky.landing ~ aircraft + no_pasg + speed_ground + height +
                    pitch+ duration ,family=binomial(link='logit'),data=faa)
AIC.model.r <- step(null.model, scope=list(lower=null.model, upper=full.model),
                  direction='forward',k=2)
summary(AIC.model.r)

BIC(AIC.model.r)

#step 7
BIC.model.r <- step(null.model, scope=list(lower=null.model, upper=full.model),
                  direction='forward',k=log(nrow(faa)))
summary(BIC.model.r)
BIC(BIC.model.r)
#step 8

g_craft <- ggplot(data <- faa,aes(x=factor(aircraft),y=factor(risky.landing)))+
  geom_jitter(position="jitter",aes(colour=factor(risky.landing)),alpha = 0.5);

g_height <- ggplot(data <- faa,aes(x=height,y=factor(risky.landing)))+
  geom_jitter(position="jitter",binwidth=5,aes(
    colour=factor(risky.landing)),alpha = 0.5);

g_ground <- ggplot(data <- faa,aes(x=speed_ground,fill=factor(risky.landing)))+
  geom_density(position="dodge",binwidth=5,aes(y=..density..,
                                               colour=factor(risky.landing)),alpha = 0.5)

ggarrange(g_height,g_ground, g_craft,ncol = 1, nrow = 3)

exp(BIC.model.r$coefficients)[-1]

g_craft <- ggplot(data <- faa,aes(x=factor(aircraft),y=factor(risky.landing)))+
  geom_jitter(position="jitter",aes(colour=factor(risky.landing)),alpha = 0.5);

g_ground <- ggplot(data <- faa,aes(x=speed_ground,fill=factor(risky.landing)))+
  geom_density(position="dodge",binwidth=5,aes(y=..density..,
                                               colour=factor(risky.landing)),alpha = 0.5)

ggarrange(g_ground, g_craft,ncol = 1, nrow = 2)

#step 12
pred.l <- ifelse(predict(BIC.model,type = 'response') < 0.5,0,1)
pred.r <- ifelse(predict(BIC.model.r,type = 'response') < 0.5,0,1)

thresh <- seq(0.01,0.5,0.01)
sensitivity <- specificity <- rep(NA,length(thresh))
for( j in seq(along=thresh)) {
  pp<- ifelse(predict(BIC.model,type = 'response') < thresh[j],0,1)
  xx<-xtabs(~faa$long.landing+pp)
  specificity[j]<-xx[1,1]/(xx[1,1]+xx[1,2])
  sensitivity[j]<-xx[2,2]/(xx[2,1]+xx[2,2])
}
par(mfrow=c(1,2))
matplot(thresh,cbind(sensitivity,specificity),type="l",xlab="Threshold",
        ylab="Proportion",lty=1:2)
plot(1-specificity,sensitivity,type="l");abline(0,1,lty=2)


pred.r <- ifelse(predict(BIC.model.r,type = 'response') < 0.5,0,1)

thresh <- seq(0.01,0.5,0.01)
sensitivity.r <- specificity.r <- rep(NA,length(thresh))
for( j in seq(along=thresh)) {
  pp<- ifelse(predict(BIC.model.r,type = 'response') < thresh[j],0,1)
  xx<-xtabs(~faa$risky.landing+pp)
  specificity.r[j]<-xx[1,1]/(xx[1,1]+xx[1,2])
  sensitivity.r[j]<-xx[2,2]/(xx[2,1]+xx[2,2])
}
par(mfrow=c(1,2))
matplot(thresh,cbind(sensitivity.r,specificity.r),type="l",xlab="Threshold",
        ylab="Proportion",lty=1:2)
plot(1-specificity.r,sensitivity.r,type="l");abline(0,1,lty=2)

plot(1-specificity,sensitivity, type="l", col="blue")
points(1-specificity.r,sensitivity.r,type="l",col="red")
lines(1-specificity.r,sensitivity.r, col="red",lty=2)

#step13
new.ind <- data.frame(aircraft="boeing",duration=200,no_pasg=80,
                      speed_ground=115,speed_air=120,height=40,pitch=4)
p.l <- predict(BIC.model,newdata=new.ind,type = 'response',se.fit = T)
p.r <- predict(BIC.model.r,newdata=new.ind,type='response' ,se.fit = T)

c(p.l$fit,p.l$fit-2*p.l$se.fit[1],p.l$fit+2*p.l$se.fit[1])

c(p.r$fit,p.r$fit-2*p.r$se.fit[1],p.r$fit+2*p.r$se.fit[1])


#step 14
r.logit <- glm(risky.landing ~ speed_ground + aircraft,
                    data = data, family=binomial(link='logit'))
summary(r.logit) 

r.probit <- glm(risky.landing ~ speed_ground + aircraft,
                    data = data, family=binomial(link='probit'))
summary(r.probit)

r.haz <- glm(risky.landing ~ speed_ground + aircraft,
                    data = data, family=binomial(link='cloglog'))
summary(r.haz) 

BIC(r.haz)


#step 15
thresh <- seq(0.01,0.5,0.01)
sensitivity.l <- specificity.l <- rep(NA,length(thresh)) # for logit
sensitivity.p <- specificity.p <- rep(NA,length(thresh)) # for probit
sensitivity.c <- specificity.c <- rep(NA,length(thresh)) # for cloglog

for( j in seq(along=thresh)) {
  #for logit
  pp<- ifelse(predict(r.logit,type = 'response') < thresh[j],0,1)
  xx<-xtabs(~faa$risky.landing+pp)
  specificity.l[j]<-xx[1,1]/(xx[1,1]+xx[1,2])
  sensitivity.l[j]<-xx[2,2]/(xx[2,1]+xx[2,2])
  #for probit
  pp<- ifelse(predict(r.probit,type = 'response') < thresh[j],0,1)
  xx<-xtabs(~faa$risky.landing+pp)
  specificity.p[j]<-xx[1,1]/(xx[1,1]+xx[1,2])
  sensitivity.p[j]<-xx[2,2]/(xx[2,1]+xx[2,2])
  #for cloglog
  pp<- ifelse(predict(r.haz,type = 'response') < thresh[j],0,1)
  xx<-xtabs(~faa$risky.landing+pp)
  specificity.c[j]<-xx[1,1]/(xx[1,1]+xx[1,2])
  sensitivity.c[j]<-xx[2,2]/(xx[2,1]+xx[2,2])
  }

#step 15
plot(1-specificity.l,sensitivity.l, type="l", col="blue")

#points(1-specificity.p,sensitivity.p,type="o",col="red",pch=21)
lines(1-specificity.p,sensitivity.p, type = "b",col="red",lty=4)

#points(1-specificity.c,sensitivity.c,type="x",col="green",pch=25)
lines(1-specificity.c,sensitivity.c, type = "o",col="green",lty=3)

par(mfrow=c(1,3))
plot(1-specificity.l,sensitivity.l, type="l", col="blue",main = 'Logit')
plot(1-specificity.p,sensitivity.p, type="l", col="red",main = 'Probit')
plot(1-specificity.c,sensitivity.c, type="l", col="green",main = 'Hazard')


#step 16

pred.logit <- predict(r.logit,type = 'response') 
pred.probit <- predict(r.probit,type = 'response') 
pred.hazard <- predict(r.haz,type = 'response') 

pred.logit[which(pred.logit==max(pred.logit))]


faa[as.numeric(names(tail(sort(pred.logit),5))),] # Logit Model
faa[as.numeric(names(tail(sort(pred.probit),5))),] # Probit Model 
faa[as.numeric(names(tail(sort(pred.hazard),5))),] # Hazard Model

top.logit <- sort(as.numeric(names(tail(sort(pred.logit),5))))
top.probit <- sort(as.numeric(names(tail(sort(pred.probit),5))))
top.hazard <- sort(as.numeric(names(tail(sort(pred.hazard),5))))

print(topn <- cbind(top.logit,top.probit,top.hazard))

# step 17
r.logit <- glm(risky.landing ~ speed_ground + aircraft,
               data = faa, family=binomial(link='logit'))
r.probit <- glm(risky.landing ~ speed_ground + aircraft,
                data = faa, family=binomial(link='probit'))
r.haz <- glm(risky.landing ~ speed_ground + aircraft,
             data = faa, family=binomial(link='cloglog'))
l.logit <- glm(long.landing ~ speed_ground + aircraft + height,
               data = faa, family=binomial(link='logit'))
l.probit <- glm(long.landing ~ speed_ground + aircraft + height,
                data = faa, family=binomial(link='probit'))
l.haz <- glm(long.landing ~ speed_ground + aircraft + height,
             data = faa, family=binomial(link='cloglog'))


new.ind <- data.frame(aircraft="boeing",duration=200,no_pasg=80,
                      speed_ground=115,speed_air=120,height=40,pitch=4)
r.l.predict <- predict(r.logit,newdata=new.ind,type = 'response',se.fit = T)
r.p.predict <- predict(r.probit,newdata=new.ind,type='response' ,se.fit = T)
r.h.predict <- predict(r.haz,newdata=new.ind,type='response' ,se.fit = T)
l.l.predict <- predict(l.logit,newdata=new.ind,type = 'response',se.fit = T)
l.p.predict <- predict(l.probit,newdata=new.ind,type='response' ,se.fit = T)
l.h.predict <- predict(l.haz,newdata=new.ind,type='response' ,se.fit = T)

p_vector <- c(r.l.predict$fit,r.p.predict$fit,r.h.predict$fit,l.l.predict$fit,
              l.p.predict$fit,l.h.predict$fit)
se_vector <- c(r.l.predict$se.fit,r.p.predict$se.fit,r.h.predict$se.fit,
               l.l.predict$se.fit,l.p.predict$se.fit,l.h.predict$se.fit)
n_vector <- c("Risky logit", "Risky probit","Risky hazard", "Long logit",
              "Long probit","Long hazard")

tt <- cbind(p_vector,se_vector,n_vector)

sum(r.h.predict$fit)


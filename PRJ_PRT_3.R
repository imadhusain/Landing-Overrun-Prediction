# step 0 create cleaned data set
#import data
library(MASS)
library(nnet)
library(car)
library(readxl)
library(tidyverse)
library(dplyr)
library(magrittr)
library(ggplot2)
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
faa_clean <- FAA %>% dplyr::select(names(FAA)) %>% 
  filter(replace(duration,is.na(duration),60) > 40 &
           (speed_ground > 30 &
              speed_ground < 140) &
           (replace(speed_air,is.na(speed_air),60) > 30 &
              replace(speed_air,is.na(speed_air),60) < 140) &
           height >= 6 &
           distance < 6000   )
#faa is the final dataset to work on
faa <- faa_clean
#Discretization of Landing Distance 
faa$Y <- 3
faa[which(faa$distance < 1000),"Y"] <- 1 
faa[which(faa$distance < 2500 & faa$distance >= 1000),"Y"] <- 2
#Custom Encoded of Variables
#faa$Y <- ordered(factor(faa$Y, levels = c(1,2,3)))
faa$aircraft <- as.factor(faa$aircraft)
#Droping actual Landing Distance variable
table(faa$Y)
faa <- faa[,-7]
#Distribution of Y
library(ggplot2)
ggplot(data = faa, aes(x = Y,fill=factor(Y))) +
      geom_bar(stat ="count")
names(faa)

#step 2

names(faa)

#building individual models to 
aircraft.model<- multinom(Y ~ aircraft,data = faa)
no_pasg.model<- multinom(Y ~ no_pasg,data = faa)
speed_ground.model<- multinom(Y ~ speed_ground,data = faa)
speed_air.model<- multinom(Y ~ speed_air,data = faa)
height.model<- multinom(Y ~ height      ,data = faa)
pitch.model<- multinom(Y ~ pitch,data = faa)
duration.model<- multinom(Y ~ duration,data = faa)

z.aircraft<- summary(aircraft.model)$coefficients/summary(aircraft.model)$standard.errors
z.no_pasg<- summary(no_pasg.model)$coefficients/summary(no_pasg.model)$standard.errors
z.speed_ground<- summary(speed_ground.model)$coefficients/summary(speed_ground.model)$standard.errors
z.speed_air<- summary(speed_air.model)$coefficients/summary(speed_air.model)$standard.errors
z.height<- summary(height.model)$coefficients/summary(height.model)$standard.errors
z.pitch<- summary(pitch.model)$coefficients/summary(pitch.model)$standard.errors
z.duration<- summary(duration.model)$coefficients/summary(duration.model)$standard.errors
z <- summary(aircraft.model)$coefficients/summary(aircraft.model)$standard.errors

# 2-tailed Wald z tests to test significance of coefficients
p.aircraft <- (1 - pnorm(abs(z.aircraft), 0, 1)) * 2
p.no_pasg <- (1 - pnorm(abs(z.no_pasg), 0, 1)) * 2
p.speed_ground <- (1 - pnorm(abs(z.speed_ground), 0, 1)) * 2
p.speed_air <- (1 - pnorm(abs(z.speed_air), 0, 1)) * 2
p.height <- (1 - pnorm(abs(z.height), 0, 1)) * 2
p.pitch <- (1 - pnorm(abs(z.pitch), 0, 1)) * 2
p.duration <- (1 - pnorm(abs(z.duration), 0, 1)) * 2

sum(p.aircraft[,2]>0.05) # significant
sum(p.no_pasg[,2]>0.05) # not significant
sum(p.speed_ground[,2]>0.05) #significant
sum(p.speed_air[2]>0.05) # significant
sum(p.height[,2]>0.05) #significant
sum(p.pitch[,2]>0.05) #not significant overall
sum(p.duration[,2]>0.05) #not significant overall

#visualize correlation with important factors
library(ggpubr)

g_ground <- ggplot(data <- faa,aes(x=speed_ground,fill=factor(Y)))+
  geom_density(position="dodge",aes(y=..density..,
               colour=factor(Y)),alpha = 0.5)

g_height <- ggplot(data <- faa,aes(x=height,fill=factor(Y)))+
  geom_density(position="dodge",binwidth=5,aes(y=..density..,
              colour=factor(Y)),alpha = 0.5)

beoing.index <- which(faa$aircraft=='boeing')

g_aircraft.b <- ggplot(data <- faa[beoing.index,],
                     aes(x=Y,fill=factor(Y)))+
  geom_density(position="dodge",binwidth=5,aes(y=..density..,
               colour=factor(Y)),alpha = 0.5)

g_aircraft.a <- ggplot(data <- faa[-beoing.index,],
                       aes(x=Y,fill=factor(Y)))+
  geom_density(position="dodge",binwidth=5,aes(y=..density..,
                  colour=factor(Y)),alpha = 0.5)

ggarrange(g_ground,g_height,g_aircraft.b,g_aircraft.a, ncol = 2, nrow = 2)


#Model using significant variables
manual.model <- multinom(Y ~ speed_ground + height + aircraft,data = faa)
summary(manual.model)
z.manual <- summary(manual.model)$coefficients/summary(manual.model)$standard.errors
# 2-tailed Wald z tests to test significance of coefficients
p.manual <- (1 - pnorm(abs(z.manual), 0, 1)) * 2


#Step wise modelling
if (!requireNamespace("nnet", quietly = TRUE)) install.packages("nnet");
library(nnet)
library(MASS)

g_duration <- ggplot(data <- faa,aes(x=duration,fill=factor(Y)))+
  geom_density(position="dodge",binwidth=5,aes(y=..density..,
                                               colour=factor(Y)),alpha = 0.5)

g_air <- ggplot(data <- faa,aes(x=speed_air,fill=factor(Y)))+
  geom_density(position="dodge",binwidth=5,aes(y=..density..,
                                               colour=factor(Y)),alpha = 0.5)

ggarrange(g_air,g_duration, ncol = 1, nrow = 2)

#dropping speed air, duration from dataset
library(nnet)
faa.subset <- faa[,-c(4,7)]
null.model <- multinom(Y ~ 1,data = faa.subset)
full.model <- multinom(Y ~ .,data = faa.subset)
library(MASS)
step.model <- stepAIC(object = null.model,
      scope = list(lower=null.model,upper=full.model),
      direction = 'forward',
      k = 2
      )
summary(step.model)
z.step <- summary(step.model)$coefficients/summary(step.model)$standard.errors
# 2-tailed Wald z tests to test significance of coefficients
p.step <- (1 - pnorm(abs(z.step), 0, 1)) * 2



#comparing models
#check if aic is less
AIC(step.model)
AIC(manual.model)
AIC(step.model) < AIC(manual.model)

#checking deviance
deviance(manual.model)
deviance(step.model)

delta.dev <- deviance(step.model)-deviance(manual.model)
delta.degf <- step.model$edf-manual.model$edf
pchisq(delta.dev,abs(delta.degf),lower=F)

#in sample predictions
y.pred <- predict(step.model,faa.subset)
y.prob <- predict(step.model,faa.subset,type='probs')

faa.subset$y.pred <- y.pred 
faa.subset$y.prob <- y.prob 


#visualize correlation with important factors
library(ggpubr)
library(ggplot2)

p_ground <- ggplot(data <- faa.subset,aes(x=speed_ground,fill=factor(y.pred)))+
  geom_density(aes(y=..density..,colour=factor(y.pred)),alpha = 0.5)

p_height <- ggplot(data <- faa.subset,aes(x=height,fill=factor(y.pred)))+
  geom_density(position="dodge",binwidth=5,aes(y=..density..,
                                       colour=factor(y.pred)),alpha = 0.5)

p_pitch <- ggplot(data <- faa.subset,aes(x=pitch,fill=factor(y.pred)))+
  geom_density(position="dodge",binwidth=5,aes(y=..density..,
                                       colour=factor(y.pred)),alpha = 0.5)

beoing.index <- which(faa$aircraft=='boeing')

p_aircraft.b <- ggplot(data <- faa.subset[-beoing.index,],
                       aes(x=factor(y.pred),fill=factor(y.pred)))+
                geom_density(position="dodge",
                aes(y=..density..,colour=factor(y.pred),alpha = 0.1))

p_aircraft.a <- ggplot(data <- faa.subset[-beoing.index,],
                aes(x=y.pred[-beoing.index],fill=factor(y.pred[-beoing.index])))+
  geom_density(position="dodge",binwidth=5,aes(y=..density..,
               colour=factor(y.pred[-beoing.index])),alpha = 0.5)

ggarrange(p_ground,p_height,p_pitch,p_aircraft.b,
          p_aircraft.a, ncol = 2, nrow = 3)

#Confusion Matrix
xtabs(~faa.subset$y.pred+faa.subset$Y)
names(faa.subset)
plot(faa.subset$speed_ground,faa.subset$y.prob[,3])

ggplot(faa.subset, aes(x=Y,fill=factor(Y))) + 
  geom_bar(colour=Y,alpha = 0.5)

ggplot(faa.subset, aes(x=Y,fill=factor(Y))) + 
  geom_bar()

ggplot(faa.subset, aes(x=y.pred,fill=factor(y.pred))) + 
  geom_bar()


summary(step.model)

######

plot(density(faa$no_pasg),main = 'Number of Passengers')
hist(faa$no_pasg,main = 'Number of Passengers')


scatterplotMatrix(~ no_pasg + Y + no_pasg + speed_ground +
                    speed_air + height + pitch + duration, data <- faa,
                  regLine = F, ellipse = F, diagonal = F,smooth = F  )


g_no_pasgn_dist <- ggplot(data <- faa,aes(x=no_pasg,fill=factor(aircraft)))+
  geom_density(position="dodge",aes(y=..density..,
                                    colour=factor(aircraft)),alpha = 0.5)
faa.np <- faa[,-4] %>% na.omit
null.model <- glm(no_pasg ~ 1,family='poisson',data = faa.np)
full.model <- glm(no_pasg ~ .,family='poisson',data = faa.np)

step.model <- step(object = null.model,
                      scope = list(lower=null.model,upper=full.model),
                      direction = 'forward',
                   
                      k = 2)
summary(full.model)
summary(step.model)

  lines(faa.np$no_pasg,step.model$fitted.values)

install.packages("dplyr")
install.packages("tidyverse")
install.packages("car")
library(dplyr)
library(tidyverse)
library(car)
#step 1
library(readxl)
FAA1 <- read_excel("FAA1.xls")
FAA2 <- read_excel("FAA2.xls")

#step 2
str(FAA1)
str(FAA2)
variable.names(FAA1)
variable.names(FAA2)
setdiff(variable.names(FAA1),variable.names(FAA2))

#step 3

#remove duplicates if any within data set
FAA1 <- unique(FAA1)
FAA2 <- unique(FAA2)

# check for duplicates between the data sets
merged <- rbind(FAA1[,-2],FAA2)
sum(duplicated(merged))

#finally create unique dataset
merged <- unique(merged)
FAA <- merge(merged,FAA1,by=names(merged), all.x=TRUE)

#step 4

str(FAA)
summary(FAA)
mat <- var(FAA[,-1],na.rm = T)
for(i in 1:7)   {
  print(paste(names(mat[,i])[i],mat[i,i]))
   }
table(FAA$aircraft)

# step 6

faa_clean <- FAA %>% select(names(FAA)) %>% 
  filter(replace(duration,is.na(duration),60) > 40 &
          (speed_ground > 30 &
          speed_ground < 140) &
          (replace(speed_air,is.na(speed_air),60) > 30 &
          replace(speed_air,is.na(speed_air),60) < 140) &
          height >= 6 &
          distance < 6000
         )

# step 7

str(faa_clean)
summ <- summary(faa_clean)
mat <- sqrt(var(faa_clean[,-1],na.rm = T))
for(i in 1:7)   {
  print(paste(names(mat[,i])[i],mat[i,i]))
}
table(faa_clean$aircraft)

par(mfrow = c(3,3))
for(i in 2:8) {
hist(faa_clean[,i],main = names(faa_clean)[i],xlab = '',ylab='') }
names(faa_clean)

cor(faa_clean[,-1],use = "complete.obs")[,6]


scatterplotMatrix(faa_clean[,-1],groups = faa_clean[,1],
                  regLine = F, ellipse = F, by.group=T,
                  legend = F,
                  smooth = F
)

plot(faa_clean$no_pasg,faa_clean$distance)
par(mfrow = c(2,3))
for(i in c(2,3,4,5,6,8)) {
plot(faa_clean[,i],faa_clean$distance ,main = names(faa_clean)[i],
     xlab = '',ylab='') }

table(faa_clean$aircraft)

faa_clean$airbus <- 1
faa_clean$airbus[faa_clean$aircraft == "boeing"] <- 0
faa_clean$airbus <- as.factor(faa_clean$airbus)

coeff <- rep (0,7)
p_val <- rep(0,7)
var_name <- rep('',7)
j <- 1
i <- 2
for(i in c(2,3,4,5,6,8,9)) {
fit <- lm(faa_std[,7] ~ faa_std[,i])
var_name[j] <- names(faa_clean)[i]
p_val[j] <- summary(fit)$coefficients[2,4]
coeff[j] <- summary(fit)$coefficients[2,1]
j <- j+1
}
tt1 <- cbind(var_name,coeff,p_val)

str(faa_std)

  
names(faa_clean)
# X’= {X-mean(X)}/sd(X)

faa_std <- cbind(faa_clean[,1],
round((faa_clean[,2]-mean(faa_clean[,2],na.omit = T))/sd(faa_clean[,2],na.rm=T),3),
round((faa_clean[,3]-mean(faa_clean[,3],na.omit = T))/sd(faa_clean[,3],na.rm=T),3),
round((faa_clean[,4]-mean(faa_clean[,4],na.rm = T))/sd(faa_clean[,4],na.rm=T),3),
round((faa_clean[,5]-mean(faa_clean[,5],na.omit = T))/sd(faa_clean[,5],na.rm=T),3),
round((faa_clean[,6]-mean(faa_clean[,6],na.omit = T))/sd(faa_clean[,6],na.rm=T),3),
faa_clean[,7],
round((faa_clean[,8]-mean(faa_clean[,8],na.rm = T))/sd(faa_clean[,8],na.rm=T),3),
faa_clean[,9])

faa_std <- bind_cols(list(faa_clean[,1],
                 as.numeric(round((faa_clean[,2]-mean(faa_clean[,2],na.omit = T))/sd(faa_clean[,2],na.rm=T),3)),
                 as.numeric(round((faa_clean[,3]-mean(faa_clean[,3],na.omit = T))/sd(faa_clean[,3],na.rm=T),3)),
                 as.numeric(round((faa_clean[,4]-mean(faa_clean[,4],na.rm = T))/sd(faa_clean[,4],na.rm=T),3)),
                 as.numeric(round((faa_clean[,5]-mean(faa_clean[,5],na.omit = T))/sd(faa_clean[,5],na.rm=T),3)),
                 as.numeric(round((faa_clean[,6]-mean(faa_clean[,6],na.omit = T))/sd(faa_clean[,6],na.rm=T),3)),
                 as.numeric(faa_clean[,7]),
                 as.numeric(round((faa_clean[,8]-mean(faa_clean[,8],na.rm = T))/sd(faa_clean[,8],na.rm=T),3)),
                 faa_clean[,9]))

names(faa_std) <- names(faa_clean)

table(faa_std[,9])
table(faa_clean[,9])
str(faa_std)

coeff <- rep (0,7)
p_val <- rep(0,7)
var_name <- rep('',7)
j <- 1
for(i in c(2,3,4,5,6,8,9)) {
  fit <- lm(faa_std[,7] ~ faa_std[,i])
  var_name[j] <- names(faa_std)[i]
  p_val[j] <- summary(fit)$coefficients[2,4]
  coeff[j] <- summary(fit)$coefficients[2,1]
  j <- j+1
}
tt1 <- cbind(var_name,coeff,p_val)

str(faa_std)
faa_std <- as.data.frame(faa_std)

cor(faa_std$distance,as.numeric(faa_std$airbus))

names(faa_clean)


LD <- faa_clean$distance
Speed_ground <- faa_clean$speed_ground
Speed_air <- faa_clean$speed_air
Model1 <- lm(LD ~ Speed_ground)
Model2 <- lm(LD ~ Speed_air)
Model3 <- lm(LD ~ Speed_ground + Speed_air)

summary(Model1)$coefficients
summary(Model2)$coefficients
summary(Model3)$coefficients

scatterplot(faa_std$speed_ground,faa_std$speed_air)
cor(faa_std$speed_ground,faa_std$speed_air,use = "complete.obs")


Y <- faa_std$distance
X1 <- faa_std$speed_ground
X2 <- faa_std$airbus
X3 <- faa_std$height
X4 <- faa_std$pitch
X5 <- faa_std$duration
X6 <- faa_std$no_pasg

Model1 <- lm(Y ~ X1)
Model2 <- lm(Y ~ X1+X2)
Model3 <- lm(Y ~ X1+X2+X3)
Model4 <- lm(Y ~ X1+X2+X3+X4)
Model5 <- lm(Y ~ X1+X2+X3+X4+X5)
Model6 <- lm(Y ~ X1+X2+X3+X4+X5+X6)

r_sqaure <- c(
  summary(Model1)$r.squared,
  summary(Model2)$r.squared,
  summary(Model3)$r.squared,
  summary(Model4)$r.squared,
  summary(Model5)$r.squared,
  summary(Model6)$r.squared)

plot(r_sqaure,xlab='Number of Parameters',ylab='R Squared'
     ,main="Variation of R Squared with Number of Parameters")+
  lines.default(x = 1:6 , y = r_sqaure)

adj_r_sqr <- c(
  summary(Model1)$adj.r.squared,
  summary(Model2)$adj.r.squared,
  summary(Model3)$adj.r.squared,
  summary(Model4)$adj.r.squared,
  summary(Model5)$adj.r.squared,
  summary(Model6)$adj.r.squared)

plot(adj_r_sqr,xlab='Number of Parameters',ylab='Adjusted R Squared'
     ,main="Variation of Adjusted R Squared with Number of Parameters")+
  lines.default(x = 1:6 , y = adj_r_sqr)

cbind()
R_sqr <- cbind(r_sqaure,"R Square",1:6)
Adj_R_sqr <- cbind(adj_r_sqr,"Adjusted R Square",1:6)

R_Cmp <- cbind(R_sqr,Adj_R_sqr)
R_Cmp <- as.data.frame(R_Cmp)

names(R_Cmp) <- c("Value","Type","Index")

install.packages("ggplot2")
library(ggplot2)


ggplot(data=R_Cmp,
       aes(x=Index, y=Value, colour=Type, group = 2)) +
  geom_line()

AIC(Model1)


aic_cmp <- c(
  AIC(Model1),
  AIC(Model2),
  AIC(Model3),
  AIC(Model4),
  AIC(Model5),
  AIC(Model6))

plot(aic_cmp,xlab='Number of Parameters',ylab='AIC'
     ,main="Variation of AIC with Number of Parameters")+
  lines.default(x = 1:6 , y = aic_cmp)

t(aic_cmp)

??StepAIC

install.packages("MASS")
library(MASS)

?stepAIC
model <- lm(distance ~.,data = faa_std[,-1])

stepAIC(model)

str(faa_std)

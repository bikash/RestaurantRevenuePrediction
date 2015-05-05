# Leitura 

setwd("/Users/bikash/repos/kaggle/RestaurantRevenuePrediction/")
#setwd("/home/ekstern/haisen/bikash/kaggle/RestaurantRevenuePrediction/")

library(MASS)
library(cvTools)
library(DAAG)

train <- read.table("data/train.csv", head = T, sep = ",", encoding="UTF-8")
head(train)
levels(train$City)
attach(train)

test <- read.table("data/test.csv", head = T, sep = ",", encoding="UTF-8")

tempo <- as.numeric(as.Date("03/23/2015", format = "%m/%d/%Y")
                    - as.Date(Open.Date, format = "%m/%d/%Y"))

####### An�lise explorat�ria #####

hist(revenue)
lrevenue <- log(revenue)
hist(lrevenue)

ltempo <- log(tempo)

plot(City, revenue)
plot(City.Group, revenue)
plot(Type, revenue)

plot(tempo,revenue)
plot(tempo,lrevenue)
plot(ltempo,lrevenue)

par(mfrow = c(2,2))
plot(lrevenue ~ P1, pch = 20) ; grid()
lines(c(by(lrevenue, P1, mean)) ~ sort(unique(P1)), lty = 2) ; grid()
plot(lrevenue ~ P2, pch = 20) ; grid()
lines(c(by(lrevenue, P2, mean)) ~ sort(unique(P2)), lty = 2) ; grid()
plot(lrevenue ~ P3, pch = 20) ; grid()
lines(c(by(lrevenue, P3, mean)) ~ sort(unique(P3)), lty = 2) ; grid()
plot(lrevenue ~ P4, pch = 20) ; grid()
lines(c(by(lrevenue, P4, mean)) ~ sort(unique(P4)), lty = 2) ; grid()

par(mfrow = c(2,2))
plot(lrevenue ~ P5, pch = 20) ; grid()
lines(c(by(lrevenue, P5, mean)) ~ sort(unique(P5)), lty = 2) ; grid()
plot(lrevenue ~ P6, pch = 20) ; grid()
lines(c(by(lrevenue, P6, mean)) ~ sort(unique(P6)), lty = 2) ; grid()
plot(lrevenue ~ P7, pch = 20) ; grid()
lines(c(by(lrevenue, P7, mean)) ~ sort(unique(P7)), lty = 2) ; grid()
plot(lrevenue ~ P8, pch = 20) ; grid()
lines(c(by(lrevenue, P8, mean)) ~ sort(unique(P8)), lty = 2) ; grid()

par(mfrow = c(2,2))
plot(lrevenue ~ P9, pch = 20) ; grid()
lines(c(by(lrevenue, P9, mean)) ~ sort(unique(P9)), lty = 2) ; grid()
plot(lrevenue ~ P10, pch = 20) ; grid()
lines(c(by(lrevenue, P10, mean)) ~ sort(unique(P10)), lty = 2) ; grid()
plot(lrevenue ~ P11, pch = 20) ; grid()
lines(c(by(lrevenue, P11, mean)) ~ sort(unique(P11)), lty = 2) ; grid()
plot(lrevenue ~ P12, pch = 20) ; grid()
lines(c(by(lrevenue, P12, mean)) ~ sort(unique(P12)), lty = 2) ; grid()

par(mfrow = c(2,2))
plot(lrevenue ~ P13, pch = 20) ; grid()
lines(c(by(lrevenue, P13, mean)) ~ sort(unique(P13)), lty = 2) ; grid()
plot(lrevenue ~ P14, pch = 20) ; grid()
lines(c(by(lrevenue, P14, mean)) ~ sort(unique(P14)), lty = 2) ; grid()
plot(lrevenue ~ P15, pch = 20) ; grid()
lines(c(by(lrevenue, P15, mean)) ~ sort(unique(P15)), lty = 2) ; grid()
plot(lrevenue ~ P16, pch = 20) ; grid()
lines(c(by(lrevenue, P16, mean)) ~ sort(unique(P16)), lty = 2) ; grid()

par(mfrow = c(2,2))
plot(lrevenue ~ P17, pch = 20) ; grid()
lines(c(by(lrevenue, P17, mean)) ~ sort(unique(P17)), lty = 2) ; grid()
plot(lrevenue ~ P18, pch = 20) ; grid()
lines(c(by(lrevenue, P18, mean)) ~ sort(unique(P18)), lty = 2) ; grid()
plot(lrevenue ~ P19, pch = 20) ; grid()
lines(c(by(lrevenue, P19, mean)) ~ sort(unique(P19)), lty = 2) ; grid()
plot(lrevenue ~ P20, pch = 20) ; grid()
lines(c(by(lrevenue, P20, mean)) ~ sort(unique(P20)), lty = 2) ; grid()

par(mfrow = c(2,2))
plot(lrevenue ~ P21, pch = 20) ; grid()
lines(c(by(lrevenue, P21, mean)) ~ sort(unique(P21)), lty = 2) ; grid()
plot(lrevenue ~ P22, pch = 20) ; grid()
lines(c(by(lrevenue, P22, mean)) ~ sort(unique(P22)), lty = 2) ; grid()
plot(lrevenue ~ P23, pch = 20) ; grid()
lines(c(by(lrevenue, P23, mean)) ~ sort(unique(P23)), lty = 2) ; grid()
plot(lrevenue ~ P24, pch = 20) ; grid()
lines(c(by(lrevenue, P24, mean)) ~ sort(unique(P24)), lty = 2) ; grid()

par(mfrow = c(2,2))
plot(lrevenue ~ P25, pch = 20) ; grid()
lines(c(by(lrevenue, P25, mean)) ~ sort(unique(P25)), lty = 2) ; grid()
plot(lrevenue ~ P26, pch = 20) ; grid()
lines(c(by(lrevenue, P26, mean)) ~ sort(unique(P26)), lty = 2) ; grid()
plot(lrevenue ~ P27, pch = 20) ; grid()
lines(c(by(lrevenue, P27, mean)) ~ sort(unique(P27)), lty = 2) ; grid()
plot(lrevenue ~ P28, pch = 20) ; grid()
lines(c(by(lrevenue, P28, mean)) ~ sort(unique(P28)), lty = 2) ; grid()

par(mfrow = c(2,2))
plot(lrevenue ~ P29, pch = 20) ; grid()
lines(c(by(lrevenue, P29, mean)) ~ sort(unique(P29)), lty = 2) ; grid()
plot(lrevenue ~ P30, pch = 20) ; grid()
lines(c(by(lrevenue, P30, mean)) ~ sort(unique(P30)), lty = 2) ; grid()
plot(lrevenue ~ P31, pch = 20) ; grid()
lines(c(by(lrevenue, P31, mean)) ~ sort(unique(P31)), lty = 2) ; grid()
plot(lrevenue ~ P32, pch = 20) ; grid()
lines(c(by(lrevenue, P32, mean)) ~ sort(unique(P32)), lty = 2) ; grid()

par(mfrow = c(2,2))
plot(lrevenue ~ P33, pch = 20) ; grid()
lines(c(by(lrevenue, P33, mean)) ~ sort(unique(P33)), lty = 2) ; grid()
plot(lrevenue ~ P34, pch = 20) ; grid()
lines(c(by(lrevenue, P34, mean)) ~ sort(unique(P34)), lty = 2) ; grid()
plot(lrevenue ~ P35, pch = 20) ; grid()
lines(c(by(lrevenue, P35, mean)) ~ sort(unique(P35)), lty = 2) ; grid()
plot(lrevenue ~ P36, pch = 20) ; grid()
lines(c(by(lrevenue, P36, mean)) ~ sort(unique(P36)), lty = 2) ; grid()

corr <- c(cor(c(by(lrevenue, P1, mean)), sort(unique(P1))),
          cor(c(by(lrevenue, P2, mean)), sort(unique(P2))),
          cor(c(by(lrevenue, P3, mean)), sort(unique(P3))),
          cor(c(by(lrevenue, P4, mean)), sort(unique(P4))),
          cor(c(by(lrevenue, P5, mean)), sort(unique(P5))),
          cor(c(by(lrevenue, P6, mean)), sort(unique(P6))),
          cor(c(by(lrevenue, P7, mean)), sort(unique(P7))),
          cor(c(by(lrevenue, P8, mean)), sort(unique(P8))),
          cor(c(by(lrevenue, P9, mean)), sort(unique(P9))),
          cor(c(by(lrevenue, P10, mean)), sort(unique(P10))),
          cor(c(by(lrevenue, P11, mean)), sort(unique(P11))),
          cor(c(by(lrevenue, P12, mean)), sort(unique(P12))),
          cor(c(by(lrevenue, P13, mean)), sort(unique(P13))),
          cor(c(by(lrevenue, P14, mean)), sort(unique(P14))),
          cor(c(by(lrevenue, P15, mean)), sort(unique(P15))),
          cor(c(by(lrevenue, P16, mean)), sort(unique(P16))),
          cor(c(by(lrevenue, P17, mean)), sort(unique(P17))),
          cor(c(by(lrevenue, P18, mean)), sort(unique(P18))),
          cor(c(by(lrevenue, P19, mean)), sort(unique(P19))),
          cor(c(by(lrevenue, P20, mean)), sort(unique(P20))),
          cor(c(by(lrevenue, P21, mean)), sort(unique(P21))),
          cor(c(by(lrevenue, P22, mean)), sort(unique(P22))),
          cor(c(by(lrevenue, P23, mean)), sort(unique(P23))),
          cor(c(by(lrevenue, P24, mean)), sort(unique(P24))),
          cor(c(by(lrevenue, P25, mean)), sort(unique(P25))),
          cor(c(by(lrevenue, P26, mean)), sort(unique(P26))),
          cor(c(by(lrevenue, P27, mean)), sort(unique(P27))),
          cor(c(by(lrevenue, P28, mean)), sort(unique(P28))),
          cor(c(by(lrevenue, P29, mean)), sort(unique(P29))),
          cor(c(by(lrevenue, P30, mean)), sort(unique(P30))),
          cor(c(by(lrevenue, P31, mean)), sort(unique(P31))),
          cor(c(by(lrevenue, P32, mean)), sort(unique(P32))),
          cor(c(by(lrevenue, P33, mean)), sort(unique(P33))),
          cor(c(by(lrevenue, P34, mean)), sort(unique(P34))),
          cor(c(by(lrevenue, P35, mean)), sort(unique(P35))),
          cor(c(by(lrevenue, P36, mean)), sort(unique(P36)))
)

which(abs(corr) > 0.5)

# Modelo inicial, todas as vari�veis ##############

mod <- lm(revenue ~ tempo + City + City.Group + Type + P1 + P2 + P3 + P4 + P5 + P6 + 
            P7 + P8 + P9 + P10 + P11 + P12 + P13 + P14 + P15 + P16 + P17 + P18 + P19 + 
            P20 + P21 + P22 + P23 + P24 + P25 + P26 + P27 + P28 + P29 + P30 + P31 + 
            P32 + P33 + P34 + P35 + P36)
summary(mod)


mod <- lm(revenue ~ tempo + City.Group + Type + P1 + P2 + P3 + P4 + P5 + P6 + 
            P7 + P8 + P9 + P10 + P11 + P12 + P13 + P14 + P15 + P16 + P17 + P18 + P19 + 
            P20 + P21 + P22 + P23 + P24 + P25 + P26 + P27 + P28 + P29 + P30 + P31 + 
            P32 + P33 + P34 + P35 + P36)
summary(mod)

step(mod)

mod <- lm(revenue ~ tempo + P6 + P8 + P9 + P20 + P24 + P26 + P28)
summary(mod)

# MOD 1: WLS #####

mod1 <- lm(revenue ~ tempo + City + City.Group + Type + P1 + P2 + P3 + P4 + P5 + P6 + 
             P7 + P8 + P9 + P10 + P11 + P12 + P13 + P14 + P15 + P16 + P17 + P18 + P19 + 
             P20 + P21 + P22 + P23 + P24 + P25 + P26 + P27 + P28 + P29 + P30 + P31 + 
             P32 + P33 + P34 + P35 + P36)
summary(mod1)

mod.var1 <- lm(abs(residuals(mod1)) ~ mod1$fitted)
mod.wls1 <- lm(revenue ~ tempo + City.Group + City + Type + P1 + P2 + P3 + P4 + P5 + P6 + 
                 P7 + P8 + P9 + P10 + P11 + P12 + P13 + P14 + P15 + P16 + P17 + P18 + P19 + 
                 P20 + P21 + P22 + P23 + P24 + P25 + P26 + P27 + P28 + P29 + P30 + P31 + 
                 P32 + P33 + P34 + P35 + P36, weights=1 / mod.var1$fitted^2)
summary(mod.wls1)

step(mod.wls1)

mod.wls1 <- lm(formula = revenue ~ tempo + P1 + P16 + P17 + P18 + P19 + P20 + 
                 P23 + P24 + P26 + P28, weights = 1/mod.var1$fitted^2)
summary(mod.wls1)


plot(rstudent(mod.wls1) ~ mod.wls1$fitted)  ;  grid()

tmp <- cbind(mod.wls1$fitted, sqrt(abs(rstudent(mod.wls1))))
plot(tmp, ylab="Raiz do m�dulo do escore", xlab="Valor ajustado", pch=20) ;  grid()
lines(lowess(tmp), col="red", lty=2, lwd=2)  

qqnorm(rstudent(mod.wls1), pch=20)  ;  qqline(rstudent(mod.wls1), col="red")  ;  grid()
hist(rstudent(mod.wls1))
shapiro.test(rstudent(mod.wls1))

p <- length(coef(mod.wls1))  ;  n <- nrow(train)
tmp <- qt(1 - .05 / (2 * n), n - p - 1) * c(-1, 1)
plot(rstudent(mod.wls1) ~ mod.wls1$fitted, pch=20, ylim=1.5 * tmp)  ;  grid()
abline(h=tmp, col="red", lty=2)

plot(hatvalues(mod.wls1), pch=20, xaxt="n", xlab="", ylab="valores-chap�u")  ;  grid()
abline(h=c(.5, 2 * p / n), lty=2)  

plot(cooks.distance(mod.wls1), pch=20, xaxt="n", xlab="", ylab="Dist�ncia de Cook�s")  ;  grid()

mod.wls1$coef

# MOD 2: WLS com lrevenue ##################################

mod2 <- lm(lrevenue ~ tempo + City + City.Group + Type + P1 + P2 + P3 + P4 + P5 + P6 + 
             P7 + P8 + P9 + P10 + P11 + P12 + P13 + P14 + P15 + P16 + P17 + P18 + P19 + 
             P20 + P21 + P22 + P23 + P24 + P25 + P26 + P27 + P28 + P29 + P30 + P31 + 
             P32 + P33 + P34 + P35 + P36)
summary(mod2)

mod.var2 <- lm(abs(residuals(mod2)) ~ mod2$fitted)
mod.wls2 <- lm(lrevenue ~ tempo + City.Group + City + Type + P1 + P2 + P3 + P4 + P5 + P6 + 
                 P7 + P8 + P9 + P10 + P11 + P12 + P13 + P14 + P15 + P16 + P17 + P18 + P19 + 
                 P20 + P21 + P22 + P23 + P24 + P25 + P26 + P27 + P28 + P29 + P30 + P31 + 
                 P32 + P33 + P34 + P35 + P36, weights=1 / mod.var2$fitted^2)
summary(mod.wls2)

step(mod.wls2)

mod.wls2 <- lm(formula = lrevenue ~ tempo + P2 + P8 + P9 + P20 + P24 + 
                 P25 + P26 + P28, weights = 1/mod.var2$fitted^2)
summary(mod.wls2)

plot(rstudent(mod.wls2) ~ mod.wls2$fitted)  ;  grid()

tmp <- cbind(mod.wls2$fitted, sqrt(abs(rstudent(mod.wls2))))
plot(tmp, ylab="Raiz do m�dulo do escore", xlab="Valor ajustado", pch=20) ;  grid()
lines(lowess(tmp), col="red", lty=2, lwd=2)  

qqnorm(rstudent(mod.wls2), pch=20)  ;  qqline(rstudent(mod.wls2), col="red")  ;  grid()
hist(rstudent(mod.wls2))
shapiro.test(rstudent(mod.wls2))

p <- length(coef(mod.wls2))  ;  n <- nrow(train)
tmp <- qt(1 - .05 / (2 * n), n - p - 1) * c(-1, 1)
plot(rstudent(mod.wls2) ~ mod.wls2$fitted, pch=20, ylim=1.5 * tmp)  ;  grid()
abline(h=tmp, col="red", lty=2)

plot(hatvalues(mod.wls2), pch=20, xaxt="n", xlab="", ylab="valores-chap�u")  ;  grid()
abline(h=c(.5, 2 * p / n), lty=2)  

plot(cooks.distance(mod.wls2), pch=20, xaxt="n", xlab="", ylab="Dist�ncia de Cook�s")  ;  grid()

mod.wls2$coef

# MOD 3: WLS com lrevenue e ltempo####

mod3 <- lm(lrevenue ~ ltempo + City + City.Group + Type + P1 + P2 + P3 + P4 + P5 + P6 + 
             P7 + P8 + P9 + P10 + P11 + P12 + P13 + P14 + P15 + P16 + P17 + P18 + P19 + 
             P20 + P21 + P22 + P23 + P24 + P25 + P26 + P27 + P28 + P29 + P30 + P31 + 
             P32 + P33 + P34 + P35 + P36)
summary(mod3)

mod.var3 <- lm(abs(residuals(mod3)) ~ mod3$fitted)
mod.wls3 <- lm(lrevenue ~ ltempo + City.Group + City + Type + P1 + P2 + P3 + P4 + P5 + P6 + 
                 P7 + P8 + P9 + P10 + P11 + P12 + P13 + P14 + P15 + P16 + P17 + P18 + P19 + 
                 P20 + P21 + P22 + P23 + P24 + P25 + P26 + P27 + P28 + P29 + P30 + P31 + 
                 P32 + P33 + P34 + P35 + P36, weights=1 / mod.var3$fitted^2)
summary(mod.wls3)

step(mod.wls3)

mod.wls3 <- lm(formula = lrevenue ~ ltempo + P2 + P8 + P9 + P20 + P24 + 
                 P25 + P26 + P28, weights = 1/mod.var3$fitted^2)
summary(mod.wls3)

plot(rstudent(mod.wls3) ~ mod.wls3$fitted)  ;  grid()

tmp <- cbind(mod.wls3$fitted, sqrt(abs(rstudent(mod.wls3))))
plot(tmp, ylab="Raiz do m�dulo do escore", xlab="Valor ajustado", pch=20) ;  grid()
lines(lowess(tmp), col="red", lty=2, lwd=2)  

qqnorm(rstudent(mod.wls3), pch=20)  ;  qqline(rstudent(mod.wls3), col="red")  ;  grid()
hist(rstudent(mod.wls3))
shapiro.test(rstudent(mod.wls3))

p <- length(coef(mod.wls3))  ;  n <- nrow(train)
tmp <- qt(1 - .05 / (2 * n), n - p - 1) * c(-1, 1)
plot(rstudent(mod.wls3) ~ mod.wls3$fitted, pch=20, ylim=1.5 * tmp)  ;  grid()
abline(h=tmp, col="red", lty=2)

plot(hatvalues(mod.wls3), pch=20, xaxt="n", xlab="", ylab="valores-chap�u")  ;  grid()
abline(h=c(.5, 2 * p / n), lty=2)  

plot(cooks.distance(mod.wls3), pch=20, xaxt="n", xlab="", ylab="Dist�ncia de Cook�s")  ;  grid()

# MOD 4: taking highest correlations #####

mod4 <- lm(formula = lrevenue ~ ltempo + P2 + P11 + P22 + P29 + P30)

mod.var4 <- lm(abs(residuals(mod4)) ~ mod4$fitted)

mod.wls4 <- lm(formula = lrevenue ~ ltempo + P2 + P11 + P22 + P29 + P30,
               weights = 1/mod.var4$fitted^2)
summary(mod.wls4)

# WLS and interactions with Type################

mod <- lm(revenue ~ tempo + City.Group + P1:Type + P2:Type + P3:Type + P4:Type + P5:Type
          + P6:Type + P7:Type + P8:Type + P9:Type + P10:Type + P11:Type + P12:Type + P13:Type
          + P14:Type + P15:Type + P16:Type + P17:Type + P18:Type + P19:Type + P20:Type + P21:Type 
          + P22:Type + P23:Type + P24:Type + P25:Type + P26:Type + P27:Type + P28:Type + P29:Type
          + P30:Type + P31:Type + P32:Type + P33:Type + P34:Type + P35:Type + P36:Type)
summary(mod)

mod.var <- lm(abs(residuals(mod)) ~ mod$fitted)
mod.wls <- lm(revenue ~ tempo + City.Group + P1:Type + P2:Type + P3:Type + P4:Type + P5:Type
              + P6:Type + P7:Type + P8:Type + P9:Type + P10:Type + P11:Type + P12:Type + P13:Type
              + P14:Type + P15:Type + P16:Type + P17:Type + P18:Type + P19:Type + P20:Type + P21:Type 
              + P22:Type + P23:Type + P24:Type + P25:Type + P26:Type + P27:Type + P28:Type + P29:Type
              + P30:Type + P31:Type + P32:Type + P33:Type + P34:Type + P35:Type + P36:Type, 
              weights=1 / mod.var$fitted^2)
summary(mod.wls)

step(mod.wls)

mod.wls <- lm(formula = revenue ~ tempo + Type:P4 + Type:P5 + Type:P8 + 
                Type:P22 + Type:P23 + Type:P25 + Type:P26 + Type:P28 + Type:P29 + 
                Type:P30, weights = 1/mod.var$fitted^2)
summary(mod.wls)


#Retirando NA's
mod.wls <- lm(formula = revenue ~ tempo + Type:P4, weights = 1/mod.var$fitted^2)
summary(mod.wls)

# WLS and interactions with City.Group################

mod3 <- lm(revenue ~ tempo + Type + P1:City.Group + P2:City.Group + P3:City.Group 
           + P4:City.Group + P5:City.Group + P6:City.Group + P7:City.Group + P8:City.Group 
           + P9:City.Group + P10:City.Group + P11:City.Group + P12:City.Group + P13:City.Group
           + P14:City.Group + P15:City.Group + P16:City.Group + P17:City.Group + P18:City.Group 
           + P19:City.Group + P20:City.Group + P21:City.Group + P22:City.Group + P23:City.Group
           + P24:City.Group + P25:City.Group + P26:City.Group + P27:City.Group + P28:City.Group
           + P29:City.Group + P30:City.Group + P31:City.Group + P32:City.Group + P33:City.Group
           + P34:City.Group + P35:City.Group + P36:City.Group)
summary(mod3)

mod.var3 <- lm(abs(residuals(mod3)) ~ mod3$fitted)
mod.wls3 <- lm(revenue ~ tempo + Type + P1:City.Group + P2:City.Group + P3:City.Group 
               + P4:City.Group + P5:City.Group + P6:City.Group + P7:City.Group + P8:City.Group 
               + P9:City.Group + P10:City.Group + P11:City.Group + P12:City.Group + P13:City.Group
               + P14:City.Group + P15:City.Group + P16:City.Group + P17:City.Group + P18:City.Group 
               + P19:City.Group + P20:City.Group + P21:City.Group + P22:City.Group + P23:City.Group
               + P24:City.Group + P25:City.Group + P26:City.Group + P27:City.Group + P28:City.Group
               + P29:City.Group + P30:City.Group + P31:City.Group + P32:City.Group + P33:City.Group
               + P34:City.Group + P35:City.Group + P36:City.Group, 
               weights=1 / mod.var3$fitted^2)
summary(mod.wls3)

step(mod.wls3)

mod.wls3 <- lm(formula = revenue ~ tempo + City.Group:P2 + City.Group:P8 + 
                 City.Group:P10 + City.Group:P11 + City.Group:P12 + City.Group:P15 + 
                 City.Group:P17 + City.Group:P18 + City.Group:P21 + City.Group:P22 + 
                 City.Group:P25 + City.Group:P26 + City.Group:P28 + City.Group:P31 + 
                 City.Group:P36, weights = 1/mod.var3$fitted^2)
summary(mod.wls3)


plot(rstudent(mod.wls3) ~ mod.wls3$fitted)  ;  grid()

tmp <- cbind(mod.wls3$fitted, sqrt(abs(rstudent(mod3.wls))))
plot(tmp, ylab="Raiz do m�dulo do escore", xlab="Valor ajustado", pch=20) ;  grid()
lines(lowess(tmp), col="red", lty=2, lwd=2)  

qqnorm(rstudent(mod.wls3), pch=20)  ;  qqline(rstudent(mod.wls3), col="red")  ;  grid()
hist(rstudent(mod.wls3))
shapiro.test(rstudent(mod.wls3))

p <- length(coef(mod.wls3))  ;  n <- nrow(train)
tmp <- qt(1 - .05 / (2 * n), n - p - 1) * c(-1, 1)
plot(rstudent(mod.wls3) ~ mod.wls3$fitted, pch=20, ylim=1.5 * tmp)  ;  grid()
abline(h=tmp, col="red", lty=2)

# WLS and interactions with City.Group and Type################

mod <- lm(revenue ~ tempo + City + P1:Type + P2:Type + P3:Type + P4:Type + P5:Type
          + P6:Type + P7:Type + P8:Type + P9:Type + P10:Type + P11:Type + P12:Type + P13:Type
          + P14:Type + P15:Type + P16:Type + P17:Type + P18:Type + P19:Type + P20:Type + P21:Type 
          + P22:Type + P23:Type + P24:Type + P25:Type + P26:Type + P27:Type + P28:Type + P29:Type
          + P30:Type + P31:Type + P32:Type + P33:Type + P34:Type + P35:Type + P36:Type 
          + P1:City.Group + P2:City.Group + P3:City.Group 
          + P4:City.Group + P5:City.Group + P6:City.Group + P7:City.Group + P8:City.Group 
          + P9:City.Group + P10:City.Group + P11:City.Group + P12:City.Group + P13:City.Group
          + P14:City.Group + P15:City.Group + P16:City.Group + P17:City.Group + P18:City.Group 
          + P19:City.Group + P20:City.Group + P21:City.Group + P22:City.Group + P23:City.Group
          + P24:City.Group + P25:City.Group + P26:City.Group + P27:City.Group + P28:City.Group
          + P29:City.Group + P30:City.Group + P31:City.Group + P32:City.Group + P33:City.Group
          + P34:City.Group + P35:City.Group + P36:City.Group)
summary(mod)

mod.var <- lm(abs(residuals(mod)) ~ mod$fitted)
mod.wls <- lm(revenue ~ tempo + City + P1:Type + P2:Type + P3:Type + P4:Type + P5:Type
              + P6:Type + P7:Type + P8:Type + P9:Type + P10:Type + P11:Type + P12:Type + P13:Type
              + P14:Type + P15:Type + P16:Type + P17:Type + P18:Type + P19:Type + P20:Type + P21:Type 
              + P22:Type + P23:Type + P24:Type + P25:Type + P26:Type + P27:Type + P28:Type + P29:Type
              + P30:Type + P31:Type + P32:Type + P33:Type + P34:Type + P35:Type + P36:Type 
              + P1:City.Group + P2:City.Group + P3:City.Group 
              + P4:City.Group + P5:City.Group + P6:City.Group + P7:City.Group + P8:City.Group 
              + P9:City.Group + P10:City.Group + P11:City.Group + P12:City.Group + P13:City.Group
              + P14:City.Group + P15:City.Group + P16:City.Group + P17:City.Group + P18:City.Group 
              + P19:City.Group + P20:City.Group + P21:City.Group + P22:City.Group + P23:City.Group
              + P24:City.Group + P25:City.Group + P26:City.Group + P27:City.Group + P28:City.Group
              + P29:City.Group + P30:City.Group + P31:City.Group + P32:City.Group + P33:City.Group
              + P34:City.Group + P35:City.Group + P36:City.Group, 
              weights=1 / mod.var$fitted^2)
summary(mod.wls)

step(mod.wls)

mod.wls <- lm(formula = revenue ~ tempo + City + P1:Type + Type:P2 + Type:P3 + 
                Type:P4 + Type:P5 + Type:P6 + Type:P7 + Type:P8 + Type:P11 + 
                Type:P12 + Type:P13 + Type:P14 + Type:P15 + Type:P16 + Type:P18 + 
                Type:P19 + Type:P20 + Type:P21 + Type:P22 + Type:P23 + Type:P24 + 
                Type:P25 + Type:P26 + Type:P27 + Type:P28 + Type:P29 + Type:P30 + 
                Type:P31 + Type:P32 + Type:P33 + Type:P34 + P1:City.Group + 
                P2:City.Group + P3:City.Group + P4:City.Group + P5:City.Group + 
                P6:City.Group + P7:City.Group + P8:City.Group + City.Group:P10 + 
                P13:City.Group + P14:City.Group + P15:City.Group + P16:City.Group + 
                City.Group:P17 + P18:City.Group + P20:City.Group + P21:City.Group + 
                P22:City.Group + P23:City.Group + P27:City.Group + P29:City.Group, 
              weights = 1/mod.var$fitted^2)
summary(mod.wls)


#modelos que consideram City n�o s�o adequados, pois muitas cidades tem apenas uma unidade na
#amostra de treino

# WLS and interactions with City.Group and Type, without City################

mod <- lm(revenue ~ tempo + P1:Type + P2:Type + P3:Type + P4:Type + P5:Type
          + P6:Type + P7:Type + P8:Type + P9:Type + P10:Type + P11:Type + P12:Type + P13:Type
          + P14:Type + P15:Type + P16:Type + P17:Type + P18:Type + P19:Type + P20:Type + P21:Type 
          + P22:Type + P23:Type + P24:Type + P25:Type + P26:Type + P27:Type + P28:Type + P29:Type
          + P30:Type + P31:Type + P32:Type + P33:Type + P34:Type + P35:Type + P36:Type 
          + P1:City.Group + P2:City.Group + P3:City.Group 
          + P4:City.Group + P5:City.Group + P6:City.Group + P7:City.Group + P8:City.Group 
          + P9:City.Group + P10:City.Group + P11:City.Group + P12:City.Group + P13:City.Group
          + P14:City.Group + P15:City.Group + P16:City.Group + P17:City.Group + P18:City.Group 
          + P19:City.Group + P20:City.Group + P21:City.Group + P22:City.Group + P23:City.Group
          + P24:City.Group + P25:City.Group + P26:City.Group + P27:City.Group + P28:City.Group
          + P29:City.Group + P30:City.Group + P31:City.Group + P32:City.Group + P33:City.Group
          + P34:City.Group + P35:City.Group + P36:City.Group)
summary(mod)

mod.var <- lm(abs(residuals(mod)) ~ mod$fitted)
mod.wls <- lm(revenue ~ tempo + P1:Type + P2:Type + P3:Type + P4:Type + P5:Type
              + P6:Type + P7:Type + P8:Type + P9:Type + P10:Type + P11:Type + P12:Type + P13:Type
              + P14:Type + P15:Type + P16:Type + P17:Type + P18:Type + P19:Type + P20:Type + P21:Type 
              + P22:Type + P23:Type + P24:Type + P25:Type + P26:Type + P27:Type + P28:Type + P29:Type
              + P30:Type + P31:Type + P32:Type + P33:Type + P34:Type + P35:Type + P36:Type 
              + P1:City.Group + P2:City.Group + P3:City.Group 
              + P4:City.Group + P5:City.Group + P6:City.Group + P7:City.Group + P8:City.Group 
              + P9:City.Group + P10:City.Group + P11:City.Group + P12:City.Group + P13:City.Group
              + P14:City.Group + P15:City.Group + P16:City.Group + P17:City.Group + P18:City.Group 
              + P19:City.Group + P20:City.Group + P21:City.Group + P22:City.Group + P23:City.Group
              + P24:City.Group + P25:City.Group + P26:City.Group + P27:City.Group + P28:City.Group
              + P29:City.Group + P30:City.Group + P31:City.Group + P32:City.Group + P33:City.Group
              + P34:City.Group + P35:City.Group + P36:City.Group, 
              weights=1 / mod.var$fitted^2)
summary(mod.wls)

step(mod.wls)

mod.wls <- lm(formula = revenue ~ tempo + P1:Type + Type:P2 + Type:P4 + 
                Type:P5 + Type:P6 + Type:P8 + Type:P9 + Type:P10 + Type:P11 + 
                Type:P13 + Type:P14 + Type:P15 + Type:P17 + Type:P18 + Type:P20 + 
                Type:P22 + Type:P23 + Type:P24 + Type:P25 + Type:P26 + Type:P27 + 
                Type:P28 + Type:P29 + Type:P30 + Type:P31 + Type:P34 + Type:P35 + 
                Type:P36 + P1:City.Group + P2:City.Group + P3:City.Group + 
                P4:City.Group + P5:City.Group + P6:City.Group + City.Group:P7 + 
                P8:City.Group + P9:City.Group + P11:City.Group + City.Group:P12 + 
                P13:City.Group + P14:City.Group + P15:City.Group + City.Group:P16 + 
                P17:City.Group + P18:City.Group + P20:City.Group + P22:City.Group + 
                P24:City.Group + P26:City.Group + P27:City.Group + P29:City.Group + 
                P31:City.Group + City.Group:P33, weights = 1/mod.var$fitted^2)
summary(mod.wls)

# Cross Validate##########

treino1 <- data.frame(revenue, tempo, P1, P16, P17, P18, P19, P20, P23, P24, P26, P28)
treino2 <- data.frame(lrevenue, tempo, P2, P8, P9, P20, P24, P25, P26, P28)
treino3 <- data.frame(lrevenue, ltempo, P2, P8, P9, P20, P24, P25, P26, P28)
treino4 <- data.frame(lrevenue, ltempo, P2, P11, P22, P29, P30)

cv1 <- cv.lm(df = treino1, form.lm = mod.wls1, m=10)
cv2 <- cv.lm(df = treino2, form.lm = mod.wls2, m=10)
cv3 <- cv.lm(df = treino3, form.lm = mod.wls3, m=10)
cv4 <- cv.lm(df = treino4, form.lm = mod.wls4, m=10)

coef(mod.wls3)

# Exportando resultados para submiss�o#####

nt <- nrow(test)
t <- as.numeric(as.Date("03/23/2015", format = "%m/%d/%Y")
                - as.Date(test$Open.Date, format = "%m/%d/%Y"))
lt <- log(t)

test <- cbind(test, lt)

var <- names(mod.wls3$coef[-1])
var[1] <- "lt" 

v <- c()

for(i in 1:length(var))
  v[i] <- which(names(test) == var[i])
int <- rep(1,nt)
test <- cbind(int,test[,v])
head(test)

coef3 <- mod.wls3$coef

Prediction <- c()
for(i in 1:nt){
  Prediction[i] <- exp(sum(test[i,] * coef3))
  cat("iteration = ", i, "\n")
}

#alguns s�o negativos!
head(Prediction,1000)

Id <- 0:99999
submission <- data.frame(Id, Prediction)

write.table(submission, "output/lm_Submision.csv", row.names = F, sep = ",")



# cross validate nos modelos! 
# ver https://www.kaggle.com/c/restaurant-revenue-prediction/forums/t/13099/how-to-correct-cross-validate
# ver http://www.statmethods.net/stats/regression.html
# ver http://statistics.ats.ucla.edu/stat/r/dae/rreg.htm para exemplos de regress�o robusta
# ver http://stats.stackexchange.com/questions/103459/how-do-i-know-which-method-of-cross-validation-is-best
# remediar valores negativos
# non linear regression?
# bayesian approach?

library(plm)

data = read.csv("E:\\Kuliah\\Semester 7\\Statistika Offisial\\TUGAS IPM\\IPM.csv", sep=";")
summary(data)

#Uji Chow
common=plm(IPM~PDRB+JPM, data=data, model="pooling")
fixed=plm(IPM~PDRB+JPM, data=data, model="within")
pooltest(common,fixed)

#Uji Haussman
fixed=plm(IPM~PDRB+JPM, data=data, model="within")
random=plm(IPM~PDRB+JPM, data=data, model="random")
phtest(fixed,random)

#Uji Breusch Pagan
bp = plm(IPM~PDRB+JPM, data=data, model="within")
#Efek Dua Arah
plmtest(bp, effect="twoways", type="bp")
#Efek Individu/Cross Section
plmtest(bp, effect="individual", type="bp")
#Efek Waktu/Time
plmtest(bp, effect="time", type="bp")

#Uji Autokorelasi 
glc=plm(IPM~PDRB+JPM, data=data, model="within", effect="individual")
pbgtest(glc, order=3)

#Heteroscedaticity Robust Covariance Estimator
library(lmtest)
g1=plm(IPM~PDRB+JPM, data=data, model="random", effect ="individual")
coeftest(g1, vcovHC)        
summary(g1)
ranef(g1)

X1=data$PDRB
X2=data$JPM
#Uji Glejser
error=abs(g1$residuals)
model = lm(error~X1+X2)
summary(model)

library(nortest)
ad.test(g1$residuals)
vif(g1)
library(car)
vif(g1)

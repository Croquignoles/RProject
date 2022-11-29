library(PCAmixdata)

ozone = read.table("H:/personnel/2A/Stat/ozone.txt", header = TRUE, sep = " ", dec = ".")
attach(ozone)
head(ozone)
summary(ozone)#stat unidim

#bidimentionnel
plot(ozone[,1:11])
plot(maxO3~as.factor(pluie))
plot(maxO3~as.factor(vent))
table(vent,pluie)

#multidimentionnel = acp
resAcp = PCAmix(ozone[,1:11],graph = FALSE)
round(resAcp$eig,digits = 2)
#round(apply(ozone[,1:11],2,sd),digits=1)
plot(resAcp,choice="cor")
#interprétation :1 faiseua de variable de vent , un de nébulosité meme si Ne15 court et un de var de température
#les vent appartent meme info pareil pour Ne et Température
#Ne et Vx opposées presque à 180°
#Temp et VX -> angle droit = peu de laiaison linéaire etre ces 2 vars
#Ne et Temp diffcile a interpreté : correction négative 
#MaxO3v relier aux temp, enticorrl au Ne, peu de correl avec vent


#modélisation
res <- lm(maxO3~T9+T12+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15+maxO3v,data = ozone)
step(res,trace = 0)

res <- lm(maxO3~1,data = ozone)
step(res,~T9+T12+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15+maxO3v,trace = 0)

res <- lm(formula = maxO3 ~ T12 + maxO3v + Ne9 + Vx9, data = ozone)
summary(res)

shapiro.test(res$residuals)
plot(res$fitted,res$residuals)
abline(h=0,col=2)

plot(res$fitted,ozone$maxO3)
abline(0,1,col=2)

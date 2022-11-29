station = read.table("H:/personnel/2A/Stat/station.txt", header = TRUE, sep = "", dec = ".")
attach(station)
head(station)
summary(station)
plot(station)

res <- lm(ventes~nbpompes+nbconc+trafic,data = station)
step(res)

res <- lm(ventes~1,data = station)
step(res,~nbpompes+nbconc+trafic)

res <- lm(formula = ventes ~ nbpompes + trafic +nbconc, data = station)
summary(res)

shapiro.test(res$residuals)
plot(res$fitted,res$residuals)
abline(h=0,col=2)

plot(res$fitted,station$ventes)
abline(0,1,col=2)

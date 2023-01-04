load("donneesProjet2A.RData")
attach(donneesProjet)

#step ascendant
res1 <- lm(Pct.BF ~ 1)
step(res, ~ Age + Weight + Height + Neck + Chest + Abdomen + Hip + Thigh + Knee + Ankle + Bicep + Forearm + Wrist)
res1 <- lm(formula = Pct.BF ~ Abdomen + Weight + Wrist + Bicep + Age + Thigh)

#step descendant
res2 <- lm(Pct.BF ~ Age + Weight + Height + Neck + Chest + Abdomen + Hip + Thigh + Knee + Ankle + Bicep + Forearm + Wrist)
step(res2)
res2 <- lm(formula = Pct.BF ~ Age + Height + Neck + Abdomen + Hip + Thigh + Forearm + Wrist)

summary(res1)
summary(res2)

res <- res2
summary(res)
shapiro.test(res$residuals)

hist(res$residuals)

plot(res$fitted,res$residuals,pch=" ")
text(res$fitted,res$residuals,dimnames(donneesProjet)[[1]])
abline(h=0,col=2)
abline(h=10,col=2)
abline(h=-10,col=2)


plot(res$fitted,res$residuals)
abline(h=0,col=2)
strangeOnes <- which(res$residuals > 10 | res$residuals < -10)
strangeOnes
newData <-donneesProjet[-strangeOnes,]

res3 <- lm(Pct.BF ~ Age + Weight + Height + Neck + Chest + Abdomen + Hip + Thigh + Knee + Ankle + Bicep + Forearm + Wrist, data = newData)
step(res3)
res3 <- lm(formula = Pct.BF ~ Age + Height + Neck + Abdomen + Hip + Thigh + Forearm + Wrist, data = newData)
summary(res3)
res <- res3

plot(res$fitted,res$residuals)
abline(h=0,col=2)

shapiro.test(res$residuals)

hist(res$residuals)

plot(res$fitted,res$residuals,pch=" ")
text(res$fitted,res$residuals,dimnames(donneesProjet)[[1]])
abline(h=0,col=2)
abline(h=10,col=2)
abline(h=-10,col=2)

residus.stud<-rstudent(res) 
plot(residus.stud,ylab="res. student.",ylim=c(-3.5,3.5)) 
abline(h=c(-2,0,2),lty=c(2,1,2)) 

load("donneesProjet2A.RData")
attach(donneesProjet)

#step ascendant
res <- lm(Pct.BF ~ 1)
step(res, ~ Age + Weight + Height + Neck + Chest + Abdomen + Hip + Thigh + Knee + Ankle + Bicep + Forearm + Wrist)

summary(res)
lm(formula = Pct.BF ~ Abdomen + Weight + Wrist + Bicep + Age + Thigh)

#step descendant
res <- lm(Pct.BF ~ Age + Weight + Height + Neck + Chest + Abdomen + Hip + Thigh + Knee + Ankle + Bicep + Forearm + Wrist)
step(res)

summary(res)

shapiro.test(res$residuals)

plot(res$fitted,res$residuals)
abline(h=0,col=2)
strangeOnes <- which(res$residuals > 15 | res$residuals < -15)

res <- lm(Pct.BF ~ Age + Weight + Height + Neck +
 Chest + Abdomen + Hip + Thigh + Knee + Ankle + Bicep + Forearm + Wrist
 , data = donneesProjet[-strangeOnes])
step(res)
plot(res$fitted,res$residuals)
abline(h=0,col=2)
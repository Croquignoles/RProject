load("donneesProjet2A.RData")
attach(donneesProjet)

#step ascendant
res <- lm(Pct.BF ~ 1)
step(res, ~ Age + Weight + Height + Neck + Chest + Abdomen + Hip + Thigh + Knee + Ankle + Bicep + Forearm + Wrist)

lm(formula = Pct.BF ~ Abdomen + Weight + Wrist + Bicep + Age + Thigh)

#step descendant
res <- lm(Pct.BF ~ Age + Weight + Height + Neck + Chest + Abdomen + Hip + Thigh + Knee + Ankle + Bicep + Forearm + Wrist)
step(res)

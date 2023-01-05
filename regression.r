load("donneesProjet2A.RData")
attach(donneesProjet)

library(performance)


# Fortes correlations ( <0.85 )
# Weigh  : 4     with /chest /abdomen /hip /thigh
# Chest : 2      with /weigh /abdomen
# Abdomen : 3    with /weigh /chest /hip
# Hip : 3        with /weigh /abdomen /thigh
# Thigh : 2      with /weigh /hip

#Plusieurs corrélations ainsi simplification u modèle à l'aide d'une régression multiple intéressante

#Utilisation de la méthode step

#Step Ascendant
res1 <- lm(Pct.BF ~ 1)
step(res1, ~ Age + Weight + Height + Neck + Chest + Abdomen + Hip + Thigh + Knee + Ankle + Bicep + Forearm + Wrist)
res1 <- lm(formula = Pct.BF ~ Abdomen + Weight + Wrist + Bicep + Age + Thigh)
summary(res1)
check_model(res1)
check_normality(res1)
check_heteroscedasticity(res1)

#Step Descendant
res2 <- lm(Pct.BF ~ Age + Weight + Height + Neck + Chest + Abdomen + Hip + Thigh + Knee + Ankle + Bicep + Forearm + Wrist)
step(res2)
res2 <- lm(formula = Pct.BF ~ Age + Height + Neck + Abdomen + Hip + Thigh + Forearm + Wrist)
summary(res2)
check_model(res2)
check_normality(res2)
check_heteroscedasticity(res2)

plot(res1$fitted, res1$residuals) # graphique des valeurs predites versus les residus
abline(h = 0, col = 2)
hist(res1$residuals)

outLiners <- which(res1$residuals > 10 | res1$residuals < -10)
outLiners
newData <- donneesProjet[-outLiners,]

res3 <- lm(Pct.BF ~ Age + Height + Neck + Abdomen + Hip + Thigh + Forearm + Wrist, data = newData)
step(res3, ~ Age + Weight + Height + Neck + Chest + Abdomen + Hip + Thigh + Knee + Ankle + Bicep + Forearm + Wrist, data = newData)
res3 <- lm(formula = Pct.BF ~ Age + Height + Neck + Abdomen + Hip + Thigh + Forearm + Wrist, data = newData)

summary(res3)
check_model(res3)
check_normality(res3)
check_heteroscedasticity(res3)

hist(res3$residuals)


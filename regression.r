load("donneesProjet2A.RData")
attach(donneesProjet)

library(GGally)
library(performance)

ggpairs(donneesProjet)


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

#On choisis de continuer de travailler avec le modèle 2
res <- res2
summary(res)
check_model(res)


hist(res$residuals) #residues semblent normaux
#On test la normalité des résidues
check_normality(res)
shapiro.test(res$residuals)
#   Shapiro-Wilk normality test
# data:  res$residuals
# W = 0.99105, p-value = 0.13

#La p-value est supérieur, on a donc un non rejet de H0 : on a bien normalité des résidues

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


plot(res3, 3)
shapiro.test(res3$residuals)

hist(res3$residuals)

plot(res3$fitted, res3$residuals, pch = " ")
text(res3$fitted, res3$residuals, dimnames(donneesProjet)[[1]])
abline(h = 0, col = 2)


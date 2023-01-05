load("archive/plaques.Rda")
attach(plaques)

head(plaques)
levels(duree)
table(duree)

plot(rugosite~duree)

res <- lm(rugosite~duree)
anova(res)
# p-value : 30% , non rejet de H0 où H0 : la nullité des effet diff du facteur durée 
# la facteur durée n'a pas d'impact en moyenne sur la rugosité

## Penser à valider la nomalité des residuals
shapiro.test(res$residuals) #test de normalité
#p-val : 75% -> non rejet de H0 donc normalité des residus
bartlett.test(rugosite~duree)#test d'homo
#p-val : 74% -> non rejet de H0 : variance de Y dans PD = variance de Y dans D2 = ... = sigma²

#Tous les commentaires précédents sur anova sont valides 
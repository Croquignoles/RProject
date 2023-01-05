load("archive/lait.Rda")
attach(lait)
levels(aliment)
table(aliment)
levels(dose)
table(dose)
table(aliment,dose)#données équilibrés
boxplot(rdt.laitier~aliment*dose)
#les botplot ne sont pas tous au même niveau : les mu sont différents
#rien de chacant pour la dispertion des boxplots
boxplot(rdt.laitier~aliment)
boxplot(rdt.laitier~dose)
#on reconfirme ce qu'on vient de dire 

interaction.plot(dose,aliment,rdt.laitier)
interaction.plot(aliment,dose,rdt.laitier)
#les courbes d'intéractions se croisent , perte de parallélisme -> interaction significative


tapply(rdt.laitier,list(aliment,dose),mean)
tapply(rdt.laitier,list(aliment,dose),min)
tapply(rdt.laitier,list(aliment,dose),var)
#.....

res.aov <- aov(rdt.laitier~aliment*dose) # modele avec interaction
summary(res.aov)
#Pr(<F) de aliment:dose <5% rejet de H0  : il faut garder les interractions 

step(res.aov)
res <- lm(rdt.laitier~aliment*dose)
summary(res)
#p-val <5% , rejet de H0 le modèle est utile
#R² le modèle à 2 facteurs explique 80% de la variabilité du modèle
#Res standart error -> l'estimation sans biais du sigma des termes d'erreurs S = 1.6 L/H

# Intercept : rendement moyen avec aliment alim.ens. et dose faible  mu = 14,8
#     /   Dose  |    faible   |   forte
# Aliment ------|-------------|------------
# alim.ens.     |     14.8    |    18
# foin          |     12      |    12.8
# herbe         |     12.2    |    13.4
# paille        |     9.4     |    14.2

shapiro.test(res$residuals) # test de normalite des residus
#pval > 5% no rejet léger de H0 -> ok  

fligner.test(rdt.laitier,aliment,dose) # test d’homoscedasticite
#pval > 5% , non rejet de H0 -> ok

#tout est ok donc interprétation sprécéddentes ok

load("archive/plaques2.Rda")
attach(plaques2)
levels(dopant)
table(dopant)
levels(duree)
table(duree)
table(dopant,duree)#modèle équilibré

interaction.plot(dopant,duree,rugosite)
interaction.plot(duree,dopant,rugosite)
# perte de parallélisme -> interaction significative mais attention si on reguarde les boxplot perte de significativité à cause du bruit 

res.aov <-aov(rugosite~dopant*duree)
summary(res.aov)
# Pr(>F) > 5% donc interraction inutile dans les modèle à 2 facteurs

step(res.aov)
res <- lm(rugosite~dopant)
summary(res)
#pval < 5% modèle utile 
#mais R² = 20% , seulement 20% de la variabilité expliqué par ce modèle
#écart type : 0.75

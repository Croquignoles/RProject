load('archive/lampes.Rda')
head(lampes)
class(lampes$duree)
class(lampes$procede)
table(lampes$procede)
#Les données sont équilibrées, pour tous lesniveaux  i du facteur, on dipose du meme  nombre ni = 30 d'observation
#Modèle d'ANOVA à 1 facteur pour modéliser les données disponibles
#Y_i,k : i correspond au procédé et k à l'individu
#Y = mu + alpha_i^procédé + eps_i,k
#mu : durée de vie moyenne d'une lampe
# alpha : effat diferentiel , alpha_P1 = 0 , mu_i : durée de vie moyenne d'une lampe en fonction du procédé
#eps : terme d'erreur aléatoire ~ N(0,sigma^2)
#                                N à valider sur les données
#                                sigma^2 , ne dépend as de i , à valider sur les data

plot(lampes$duree~lampes$procede)

## Commentaires 2 ordres : 

#  - botplot au meme niveau ? -> savoir si le procedé a un impact sur la durée de vie 
# H0 : mu_p1 = mu_P2 = ... = mu
# same as alpha_P1 = alpha_P2 = ... = 0
# le facteurde procédé n'a pas d'impact en moyenne sur la durée de vie des lampes
# le modèle est inutile

#Ici rejet visuel de H0 , le procédé a un impact significative sur la durée de vie des lampes

#  - hopothèses d'homogénéité des variables , dispertion des boxplots

#Ici validé visuellement, rien de choquant

##Estimation du modèle statistique

res <- lm(lampes$duree~lampes$procede)
anova(res)
#pas vraiment utile en ANOVA à 1 facteur
#H0 : alpha_P1 = alpha_P2 = ... = 0 , ici p-val  = 2.2e-16 : fort rejet de H0 -> le modèle est utile
#R^2 : qualité de représentation du modèle : ici 98% du modèle expliqué
#Residual standart error : estimation sans biais de sigma : ici ~88h
summary(res)
#Estimate Std. : 
#Intercept : estimation de mu_P1
#procedePi : estimation de alpha_Pi ; en passant de P_i-1 à P_1
#p-val : risque de spécification <5% 
#comparaison :si  >5% les 2 procédés P1 et P5 sont équivalement entre eux en moyenne

# Manque à valider la normalité et homogénéités des variances
shapiro.test(res$residuals) #test de normalité
#p-val : 67% -> non rejet de H0 donc normalité des residus
bartlett.test(lampes$duree~lampes$procede)#test d'homo
#p-val : 17%>5% -> non rejet de H0 : variance de Y dans P1 = variance de Y dans P2 = ... = sigma²

#Tous les commentaires précédents sur anova sont valides 

##Compraisons multiples : 

#comparaison de toutes les moyennes 2 à 2 avec un rique global de 5% (pas de risque spécifique)
pairwise.t.test(lampes$duree,lampes$procede,p.adjust="bonf")
#si p-val < 5% ->rejet de H0 
#Ici rejet de tous sauf P1/P5 et P2/P4
# -> pas d'écart significatif entre les procédés P1 et P5 ni entre les procédés P2 et P4
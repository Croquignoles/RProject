donnees <- data.frame(engraisA=c(48,49,50,49),engraisB=c(47,49,48,48),engraisC=c(49,51,50,50))
donnees
stack(donnees) # cette fonction R permet d’empiler les colonnes d’un tableau de donnees
rendement <- stack(donnees)$values
engrais <- stack(donnees)$ind
is.factor(engrais) # on peut verifier que la variable "engrais" est bien de type "facteur"
# ou
class(engrais) # permet de verifier de quelle type est la variable "engrais"
class(rendement) # permet de verifier de quelle type est la variable "rendement"
plot(rendement~engrais) # permet d’obtenir les boxplots paralleles du rendement
# par type d’engrais
apply(donnees,2,summary)
# ou de maniere equivalente
tapply(rendement,engrais,summary)
res.aov <- aov(rendement~engrais)
summary(res.aov)
res <- lm(rendement~engrais)
anova(res) # permet de retrouver le tableau de l’analyse de variance precedent
summary(res) # permet d’obtenir en plus l’estimation des parametres
# (calage sur le premier groupe PAR DEFAUT)
# pour se caler sur le troisieme groupe
#------------------------------
res <- lm(rendement~C(engrais,base=3))
summary(res)
# pour se caler sur la moyenne
#------------------------------
res <- lm(rendement~C(engrais,sum))
summary(res)
shapiro.test(res$residuals) # test de normalite des residus
bartlett.test(rendement,engrais) # test d’homoscedasticite
plot(res$fitted,res$residuals) # graphique des valeurs predites versus les residus
pairwise.t.test(rendement,engrais,p.adjust="bonf")
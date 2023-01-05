load("archive/rendement3.Rda")
attach(rendement3)
head(rendement3)

levels(annee)
table(annee)

levels(variete)
table(variete)

levels(lieu)
table(lieu)

table(annee,variete,lieu)#données équilibrés
boxplot(rdt~annee*variete*lieu)

res.aov <- aov(rdt~annee*variete*lieu)
summary(res.aov)

step(res.aov)
res <- lm(rdt ~ annee + variete + lieu + annee:variete + annee:lieu)
summary(res)

#   Annee            2017          |         2018
# Variete      V1  |  V2  |  V3    |    V1  |  V2  |  V3
# Lieu : L1   121  |  101 |  119   |    83  |  121 |  81 
#        L2   131  |  111 |  129   |    84  |  122 |  82
#        L3   123  |  103 |  121   |    88  |  126 |  86
#        L4   125  |  105 |  123   |    78  |  116 |  76


shapiro.test(res$residuals) # test de normalite des residus
#pval > 5% no rejet léger de H0 -> ok  

fligner.test(rdt,annee,variete,lieu) # test d’homoscedasticite
#pval > 5% , non rejet de H0 -> ok
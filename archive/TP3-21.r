rendement<-c(15,14,17,18,17,18,12,13,12,14,15,15,
16,19,18,23,24,25,15,14,14,12,11,10,
18,17,17,20,21,21,17,19,17,12,13,12)
plant<-as.factor(rep(rep(c("Plant I","Plant II","Plant III","Plant IV"),each=3),3))
engrais<-as.factor(rep(c("Engrais A","Engrais B","Engrais C"),each=12))
donnees<-data.frame(rendement,plant,engrais)
levels(engrais) # donne les differents niveaux du facteur "engrais"
levels(plant) # donne les differents niveaux du facteur "plant"
table(engrais) # indique les nombres d’observations par niveau du facteur "engrais"
table(plant) # indique les nombres d’observations par niveau du facteur "plant"
boxplot(rendement~engrais*plant)
boxplot(rendement~plant)
boxplot(rendement~engrais)
interaction.plot(plant,engrais,rendement)
# ou
interaction.plot(engrais,plant,rendement)
tapply(rendement,list(engrais,plant),mean)
tapply(rendement,list(engrais,plant),min)
tapply(rendement,list(engrais,plant),var)
res.aov <- aov(rendement~engrais*plant) # modele avec interaction
summary(res)

# ou
res.aov <- aov(rendement~engrais+plant+engrais:plant) # modele avec interaction
summary(res.aov)
res.aov <- aov(rendement~engrais*plant) # modele avec interaction
summary(res.aov)
# ou
res.aov <- aov(rendement~engrais+plant+engrais:plant) # modele avec interaction
summary(res.aov)
res<-lm(rendement~engrais*plant) # calage sur la case "Type I" et "Engrais A"
summary(res)
# H0 : le modèle est inutile , ici pval<5% donc rejet H0 , le modèle est utile 
# H0 <=> alpha_i^engrais == aplha_j^plant==B_i,j== 0
# La variabilité du rendement est expliqué à R² = 96% par le modèle à 2 facteurs (engrais*plant)
# Res standart error : estaimation sans biaais de l'écart type sigma des termes d'errurs du modèle est s= 0,9 t/h

#Intercept : rendement moyen avec engrais A et Plant 1 , mu_A1 = 15.333
#Si on passe à engrais B : alpha_B = 2.33
#Si on passe à plant 2 : alpha_2 = 2.333

#interraction B_i,j où B_A,j = 0 pour tout j et B_i,1 = 0 pour tout i 
#B_B,2 = 4 , bonne interaction
#B_C,3 = 3.33 , intéraction significative
#B_B,4 = -6 , interraction significative -> grande perte 
#B_C,4 = -4.3 , interraction significative -> grande perte
# si on doit utiliser le plant 4 il faut utiliser l'engrais A
#possible de faire un tab : https://docs.google.com/spreadsheets/d/1saBV7VWYq6XxT4uOwRnuTxaLxxgsG2IyvfylccGdcCQ/edit#gid=0


shapiro.test(res$residuals) # test de normalite des residus
#pval>5% Non rejet de H0 -> ok 
bartlett.test(rendement,engrais,plant) # test d’homoscedasticite
#pval =2%<5% , léger rejet de H0 -> bof! mais ca passe 
# explication: données équilibrés (même nombre d'observation dans chaqu cas (i,j):3 (c'st ptit))
#Robusteste de L'anova à de léger écart 
# pour se caler sur la moyenne
fligner.test(rendement,engrais,plant) # autre test d’homoscedasticite
#pval = 4.3%<5% , très léger rejet de H0 -> permet de rester overt à l'interprétation et de rester critique face aux observations 
res <- lm(rendement~C(engrais,sum)+C(plant,sum)+C(engrais,sum):C(plant,sum))
summary(res)


plot(res$fitted,res$residuals) # graphique des valeurs predites versus les residus0
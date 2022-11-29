# I- Découverte des fonc R permettant de faire de la régression linéaire
# 1 Simulation des données

beta0=4;
beta1=-2;
beta2=+3;
beta3=-5;
beta4=0.8;
beta5=1.4;
#standard deviation (sigma)

#choisir taille échantillon
n<-100

#génerer de xi, i=1,..,n
x3<-runif(n,min=-5,max=5) #choix arbitraire : les xk ont la même distribition
x4<-runif(n,min=-5,max=5) #impact : ici uniquement, les Bk sont "comparables" :: Bk = 2 et Bj = 4, xj 2 fois plus impactante que xk dans le modèle  
x5<-runif(n,min=-5,max=5) #peut permettre d'ordonner l'importance des variables
x1<-runif(n,min=-5,max=5) #si on regarde un seul Bk -> impact marginal
x2<-runif(n,min=-5,max=5)
#générer des réalisation de epsilon i, i=0,..,n
eps<-rnorm(n,mean=0,sd=2)
#fabrication des yi, i=1,..,n
y<-beta0+beta1*x1+beta2*x2+beta3*x3+beta4*x4+beta5*x5+eps

#affichage des variables 2 à 2
matYX<-data.frame(y,x1,x2,x3,x4,x5)
pairs(matXY)# same result plot(matXY)
#meilleur correl entre x3 et y -> liaison marginal forte
#graph entre les xk -> pas de lien entre les xk :: aucune correlation visible 

#régression linéaire multiple
res<-lm(y~x1+x2+x3+x4+x5,data = matXY)
summary(res)
#p value (test de ficher) de significativité du modèle 
# -H0 : le modèle est inutile -> inutilité simultané de tous les coefs
# -H1 : non H0 -> "il exite au moins un Bk diff de 0"
#ici p-val << 0.5 -> fort rejet de H0 = "modèle utile"
#R² ~= 0.98 % de variabilité de y est expliqué par le modèle 
#R² ajusted : version du R² pénalisé par le nombre de variable explicative : l sert à comparer 2 modèles ayant un nombre diff de var explicatives
#on le veut le plus haut possible (par un % de variabilité expliquée)
#Residual standart error -> estimation sans biais
#derniere col coef test de nullité individuel de chaque variable -> *** rejet de toutes les hypothèses nulles :: toutes les variables sont utiles dans le modèle 

shapiro.test(res$residuals) #p val -> 0.7 non rejet de H0 :: normalité des residus
plot(res$fitted,res$residuals)
abline(h=0,col=2)
#residus bien aléatoire -> pas de structure + variance observable constante

plot(res$fitted,y)
abline(0,1,col=2)

#3 S´election de variables en r´egression lin´eaire multiple

res <- lm(y~x1+x2+x3+x4+x5,data=matYX)
summary(res)

#3.2 Elimination d’une variable du mod`ele
drop1(res)

#3.3 Ajout d’une variable explicative dans le mod`ele
res <- lm(y~x5+x4,data=matYX)
add1(res,~x1+x2+x3+x4+x5)

#Remarque sur le choix de la premi`ere variable explicative du mod`ele
cor(matYX) # calcul de la matrice des correlations lineaires des donnees
cor(matYX[,1],matYX[,-1]) # calcul uniquement des corr. entre les xk et y

res <- lm(y~1,data=matYX) # on part d’un modele sans variable explicative
add1(res,~x1+x2+x3+x4+x5) # on choisit la variable qui permet d’obtenir le "Cp" le plus faible.

#3.4 Strat´egies pour choisir le “meilleur” mod`ele de r´egression lin´eaire multiple avec R
#les 2 méthodes peuvent ne pas aboutir au même meilleur modèle

#régression pas à pas ascendante
#on part d'un modèle reduit : modèle avec peu de variables explicatives (on peut commencer sans aucune variable explicative)
#A chaque étape step reguarde s'il est possible de faire entrer une nouvelle variable utile au modèle au sens du critère AIC
#arrêt : aucune nouvelle variable utile ne peut entrer dans le modèle
res <- lm(y~1,data=matYX) # modele initial sans variable explicative, les var. etant dans "matYX"
step(res,~x1+x2+x3+x4+x5,trace=0) # avec x1,...,xp, p colonnes de "matYX"

#régression pas à pas descendante
#on part d'un modèle complet avec toutes les variables expliquatives disponnibles
#à chaque étape step sort la variable la moins utile au sens du critère AIC
#arret: il n'y a plus de variable à faire sortir du mosèle
#inconvenient : etre capable d'estimer le modèle complet
res <- lm(y~x1+x2+x3+x4+x5,data=matYX) # modele initial complet
step(res,trace = 1)

#remarque : paramètre Trace(F ou T) -> pour afficher ou non toutes les étapes réalisés par step

#3.5Exercice
a0=5
a1=65
a2=86
a3=10
a4=0.5
a5=0
a6=91.33
a7=0
a8=5.2
a9=0

x6= runif(n,min=-5,max=5)
x7= runif(n,min=-5,max=5)
x8= runif(n,min=-5,max=5)
x9= runif(n,min=-5,max=5)

y<-a0+a1*x1+a2*x2+a3*x3+a4*x4+a5*x5+a6*x6+a7*x7+a8*x8+a9*x9+eps

matYXs<-data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x9)

res <- lm(y~1,data=matYXs)
step(res,~x1+x2+x3+x4+x5+x6+x7+x8+x9,trace=0)

res <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=matYXs)
step(res,trace=0)

#4 Pr´ediction dans un mod`ele de r´egression lin´eaire multiple
matYX <- data.frame(y,x1,...,xp)
res <- lm(y~x1+...+xp,data=matYX)
predict(res,data.frame(x1=nouveaux1,...,xp=nouveauxp))

#lors d'une étude faire de la stat desciptive avant de la modelisation
#stat unidimensionnel
#bi-dimentionnel
#multi-dimentionnel



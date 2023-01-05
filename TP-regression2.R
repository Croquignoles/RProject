#===================================================
# ENSC 2A - TP de Modelisation Statistique -
#--------------------------------------------
# Regression lineaire multiple
#====================================================

#========================================
# Mise en oeuvre sur des donnees simulees
#========================================

# Simulation des donnees
#------------------------
n<-100
x1<-runif(n,min=-5,max=5)
x2<-runif(n,min=-5,max=5)
x3<-runif(n,min=-5,max=5)
x4<-runif(n,min=-5,max=5)
x5<-runif(n,min=-5,max=5)
sigma<-2
e<-rnorm(n,mean=0,sd=sigma)
y<-4-2*x1+3*x2-5*x3+0.8*x4+1.4*x5+e

matYX<-data.frame(y,x1,x2,x3,x4,x5)

# une premiere "visualisation" des donnees
#------------------------------------------

plot(matYX)    # identique à :  pairs(matYX)


# Estimation du modele de regression lineaire multiple
#-----------------------------------------------------

res<-lm(y~x1+x2+x3+x4+x5,data=matYX)
summary(res)

par(mfrow=c(1,2))
plot(res$fitted,res$residuals)
abline(h=0,col=2)
plot(res$fitted,y)
abline(0,1,col=2)

par(mfrow=c(1,1))
plot(rstudent(res),ylab="res. student.",ylim=c(-3.5,3.5)) 
abline(h=c(-2,0,2),lty=c(2,1,2)) 

shapiro.test(res$residuals)

#===============================================
# Selection de variables
#===============================================

# Utilisation de la fonction "drop1"
#------------------------------------
n<-100
x1<-runif(n,min=-5,max=5)
x2<-runif(n,min=-5,max=5)
x3<-runif(n,min=-5,max=5)
x4<-runif(n,min=-5,max=5)
x5<-runif(n,min=-5,max=5)
sigma<-2
e<-rnorm(n,mean=0,sd=sigma)
y<-4-2*x1+3*x2-5*x3+1.4*x5+e    # modele sans "x4"

matYX<-data.frame(y,x1,x2,x3,x4,x5)

res<-lm(y~x1+x2+x3+x4+x5,data=matYX)
summary(res)
drop1(res)

res<-lm(y~x1+x2+x3+x5,data=matYX)
drop1(res)

# Utilisation de la fonction "add1"
#------------------------------------
n<-100
x1<-runif(n,min=-5,max=5)
x2<-runif(n,min=-5,max=5)
x3<-runif(n,min=-5,max=5)
x4<-runif(n,min=-5,max=5)
x5<-runif(n,min=-5,max=5)
sigma<-2
e<-rnorm(n,mean=0,sd=sigma)
y<-4-2*x1+3*x2-5*x3+1.4*x5+e    # modele sans "x4"

matYX<-data.frame(y,x1,x2,x3,x4,x5)

res<-lm(y~x1+x2,data=matYX)
summary(res)
add1(res,~x1+x2+x3+x4+x5)

res<-lm(y~x1+x2+x3,data=matYX)
add1(res,~x1+x2+x3+x4+x5)

res<-lm(y~x1+x2+x3+x5,data=matYX)
add1(res,~x1+x2+x3+x4+x5)

# Choix d'un "modele de depart"
#-------------------------------
cor(matYX)  # calcul de la matrice des correlations lineaires des donnees
cor(matYX[,1],matYX[,-1])  # calcul uniquement des corr. entre les xk et y

res <- lm(y~1,data=matYX)   # on part d'un modele sans variable explicative
add1(res,~x1+x2+x3+x4+x5)  # on choisit la variable qui permet d'obtenir le Cp le plus faible.

#=================================================
# Selection automatique de variables avec "step"
#=================================================

# Exemple de regression pas a pas ascendante 
#--------------------------------------------
res <- lm(y~1,data=matYX) # modele initial sans variable explicative, les var. etant dans "matYX"
step(res,~x1+x2+x3+x4+x5)   # avec x1,...,xp,  p colonnes de "matYX"
step(res,~x1+x2+x3+x4+x5,trace=F)

# Exemple de regression pas a pas descendante  
res <- lm(y~x1+x2+x3+x4+x5,data=matYX) # modele initial complet,
step(res)  


#=================================================
#=================================================
#=================================================
# Etude des donnees du fichier "ozone.txt"
#=================================================
#=================================================
#=================================================

ozone<-read.table("archive/ozone.txt",header=T)

head(ozone)
dim(ozone)

summary(ozone)


colnames(ozone)

attach(ozone)

#---------------------------------------------------------
# une premiere visualisation des variables quantitatives
#---------------------------------------------------------
plot(ozone[,1:11])

#----------------------------------------
# Chargement du package PCAmixdata
#----------------------------------------
require(PCAmixdata) # permet de charger le package "PCAmixdata" 
# afin de pouvoir l'utiliser par la suite
help(PCAmix)
#--------------------------------------------------------------
# Mise en oeuvre de l'ACP - variables quantitatives seulement
#--------------------------------------------------------------
res<-PCAmix(ozone[,1:11],graph=FALSE) # tous les calculs de l'ACP sont stockes dans l'objet "res"
# sans les graphiques avec "graph=FALSE"
# NB : par defaut les graphiques des plans factoriels 1-2 
# sont affiches a l'ecran 

res # permet de voir l'ensemble des sorties numeriques disponibles

#-----------------------------------
# Choix du nombre d'axes à retenir
#-----------------------------------
round(res$eig,digit=2) # permet d'afficher les valeurs propres et les pourcentages 
# de variances expliquees par chaque axe    
# Graphique de l'ebouli des valeurs propres
barplot(res$eig[,1],main="Eigenvalues",names.arg=1:nrow(res$eig)) 
abline(h=1,col=2,lwd=2)

#--------------------------------------------------------------------
# Graphiques des individus et des variables sur le plan factoriel 1-2
#--------------------------------------------------------------------
?plot.PCAmix # permet d'afficher la fenetre d'aide de la commande "plot.PCA"
plot(res,axes=c(1,2),choice="ind") # on retrouve ici le graphique des individus (plan 1-2)
plot(res,axes=c(1,2),choice="cor") # on retrouve ici le cercle des correlations
# des variables quantitatives (plan 1-2)
plot(res,axes=c(1,2),choice="sqload") # on retrouve ici le graphique des "square loadings" (plan 1-2)

#--------------------------------------------------------------------
# Sorties numeriques pour les individus et es variables
#--------------------------------------------------------------------
res$ind # permet d'afficher l'ensemble des sorties numeriques associees aux individus : 
# coordonnees, contributions, cosinus carres
round(res$ind$cos2,digit=3) # uniquement les cosinus carres

res$quanti # permet d'afficher l'ensemble des sorties numeriques associees aux variables : 
# coordonnees, contributions, cosinus carres
round(res$quanti$cos2,digit=3) # uniquement les cosinus carres

#----------------------------------------------------------------------
# Utilisation "graphique" des variables qualitatives "pluie" et "vent"
#----------------------------------------------------------------------

plot(maxO3~pluie)
plot(maxO3~vent)

#-------------------------------------------------------------------------------
# Mise en oeuvre de l'ACP mixtes -avec variables quantitatives et qualitatives
#-------------------------------------------------------------------------------
res<-PCAmix(X.quanti=ozone[,1:11],X.quali=ozone[,12:13],graph=FALSE) # tous les calculs de l'ACP sont stockes dans l'objet "res"
# sans les graphiques avec "graph=FALSE"
# NB : par defaut les graphiques des plans factoriels 1-2 
# sont affiches a l'ecran 

res # permet de voir l'ensemble des sorties numeriques disponibles

#-----------------------------------
# Choix du nombre d'axes à retenir
#-----------------------------------
round(res$eig,digit=2) # permet d'afficher les valeurs propres et les pourcentages 
# de variances expliquees par chaque axe    
# Graphique de l'ebouli des valeurs propres
barplot(res$eig[,1],main="Eigenvalues",names.arg=1:nrow(res$eig)) 
abline(h=1,col=2,lwd=2)

#--------------------------------------------------------------------
# Graphiques des individus et des variables sur le plan factoriel 1-2
#--------------------------------------------------------------------
?plot.PCAmix # permet d'afficher la fenetre d'aide de la commande "plot.PCA"
plot(res,axes=c(1,2),choice="ind") # on retrouve ici le graphique des individus (plan 1-2)
plot(res,axes=c(1,2),choice="cor") # on retrouve ici le cercle des correlations
# des variables quantitatives (plan 1-2)
plot(res,axes=c(1,2),choice="levels") # on retrouve ici les graphiques des modalités (plan 1-2)
# des variables qualitatives
plot(res,axes=c(1,2),choice="sqload") # on retrouve ici le graphique des "square loadings" (plan 1-2)


#--------------------------------------------------------------------
# Sorties numeriques pour les variables quanti et quali
#--------------------------------------------------------------------
res$quanti # permet d'afficher l'ensemble des sorties numeriques associees aux variables quanti : 
# coordonnees, contributions, cosinus carres
res$levels # permet d'afficher l'ensemble des sorties numeriques associees aux modalités des variables quali : 
# coordonnees, contributions, cosinus carres
res$quali  # permet d'afficher l'ensemble des sorties numeriques associees aux variables quanti : 
# contributions uniquement

round(res$quanti$contrib.pct,digits=2)

apply(round(res$quanti$contrib.pct,digits=2),2,sum)


#------------------------------
# Modelisation statistique
#------------------------------

res<-lm(maxO3~T9+T12+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15+maxO3v,data=ozone)
# ou
res <- lm(maxO3~.,,data=ozone[,1:11])
summary(res)

# Modele pas satisfaisant !
# A completer par l'etude des residus !

# Faire de la selection de variables 

step(res)

res <- lm(maxO3~1,data=ozone) 
step(res,~T9+T12+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15+maxO3v,trace=TRUE)

# Modèle final selectionné 
#--------------------------
res2 <- lm(formula = maxO3 ~ T12 + Ne9 + Vx9 + maxO3v, data = ozone)
summary(res2)

plot(res2$fitted,res2$residuals)
abline(h=0,col=2)

shapiro.test(res2$residuals)

hist(res2$residuals)

plot(res2$fitted,res2$residuals,pch=" ")
text(res2$fitted,res2$residuals,dimnames(ozone)[[1]])
abline(h=0,col=2)

# On observe dans les residus 4 observations atypiques (outliers)
# Identification des outliers :

outliers <- which(abs(res2$residuals)>30)
outliers  # nom des outliers
res2$residuals[outliers]  # valeurs prises par les outliers

# Estimation du modele final sur le jeu de donnees sans les outliers

ozone2 <- ozone[-outliers,]
res3 <- lm(formula = maxO3 ~ T12 + Ne9 + Vx9 + maxO3v, data = ozone2)
summary(res3)

plot(res3$fitted,res3$residuals)
abline(h=0,col=2)

shapiro.test(res3$residuals)

# conclusion : le modele est meilleur et tout va bien pour les residus !



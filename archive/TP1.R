# I- Découverte des fonc R permettant de faire de la régression linéaire
# 1 Simulation des données

beta0=3;
beta1=2;
sd2=1;
#standard deviation (sigma)

#choisir taille échantillon
n=100

#génerer de xi, i=1,..,n
x<-runif(n,min=0,max=5)
#générer des réalisation de epsiloni, i=0,..,n
l<-rnorm(n,mean=0,sd=1)
#fabrication des yi, i=1,..,n
y<-beta0+beta1*x+l

# 2 Visualisation des données
plot(x,y);

# 3 Mise en oeuvre de la régression linéaire
res<-lm(y~x);
#la fonction lm signifie linear model et permet d'obtenir des estimation des parametre beta0, beta 1 et sd
summary(res);
#residual=bof
#coeff et residual standart error sont essentiels à travailler
#test de significativité ou "utilité du modele" c'est un test de fisher
#p-value est plus petite que risque de premier espèce (qui est de 5%) donc fort rejetde Ho => modèle linéaire utile
abline(res);
#trace un nuage de points

plot(res$fitted,res$residuals);
abline(h=0,col=2);
#pas de structure particulière donc tout va bien 
#+ variance constante 
#impossible de tirer de conclusion des résidus
#on peut reguarder les résidus studentisé : = residus - moyenne résidu(=0) / écart type résidus
#proche d'une normale centrée réduite

plot(res$fitted,y)
abline(0,1,col=2)

shapiro.test(res$residuals)

residus.stud<-rstudent(res)
plot(residus.stud,ylab="res. student.",ylim=c(-3.5,3.5))
abline(h=c(-2,0,2),lty=c(2,1,2))

# pour faire une prevision de y lorsque x=2 : estimation ponctuelle et intervalle de prediction
# de y0 au niveau de confiance 95%
res<-lm(y~x)
x0<-data.frame(x=2)
predict(res,new=x0,interval="pred",level=0.95)
# pour faire une prevision de l’esperance conditionnelle de y lorsque x=2 : intervalle de
# confiance de E[y|x=x0] au niveau de confiance 95% avec x0=2
predict(res,new=x0,interval="conf",level=0.95)

res<-lm(y~x)
# Calcul des intervalles de prevision ICpred et des intervalles de confiance ICmoy sur une grille
#------------------------------------------------------------------------------------------------
grille.x<-data.frame(x=seq(from=min(x),to=max(x),length.out=50))
ICpred<-predict(res,new=grille.x,interval="pred",level=0.95)
ICmoy<-predict(res,new=grille.x,interval="conf",level=0.95)
# Trace de ces intervalles de prevision et de confiance sur le nuage de points
#----------------------------------------------------------------------------
plot(x,y)
abline(res)
matlines(grille.x,cbind(ICpred[,],ICmoy[,-1]),lty=c(1,2,2,3,3),col=c(1,2,2,3,3))
legend("topleft",lty=c(2,3),col=c(2,3),c("IC prevision","IC moy"))

#-------------------------------------------------------------------
load("appartements.Rda")
head(appartements)
summary(appartements)
dim(appartements)
plot(prix~surface,data=appartements)

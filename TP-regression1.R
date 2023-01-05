#===============================================
# TP de regression lineaire simple
# ----------------------------------
#  ENSC 2A - modelisation statistique
#===============================================

#===================================
# Section 1 : simulations numeriques
#===================================

# Simulation d'un modele de regression lineaire simple
#------------------------------------------------------
n <- 200
beta0 <- 3
beta1 <- 2
sigma <- 1
t <- 5
vect.x <- runif(n,min=0,max=t)
vect.erreur <- rnorm(n,mean=0,sd=sigma)
vect.y <- beta0+beta1*vect.x+vect.erreur

# Visualisation des donnees (nuage de points)
#---------------------------------------------
plot(vect.x,vect.y)
abline(3,2,col=2,lwd=2)  # vraie droite de regression (possible en simulation seulement)

# Estimation du modele de regression lineaire simple
#----------------------------------------------------
res <- lm(vect.y~vect.x)

attributes(res)
summary(res) # affichage de sorties numériques du modele
coefficients(res)

abline(res,col=3,lwd=2) # ajout sur le nuage de point de la droite de regression estimee

# Etude des residus 
#-------------------
head(res$residuals)

shapiro.test(res$residuals) # test de normalite des residus

# deux graphiques utiles pour juger de la qualite du modele
par(mfrow=c(1,2))  
plot(res$fitted,res$residuals)
abline(h=0,col=2)
plot(res$fitted,vect.y)
abline(0,1,col=2)

# Les residus "studentises"
par(mfrow=c(1,1))
residus.stud<-rstudent(res) 
plot(residus.stud,ylab="res. student.",ylim=c(-3.5,3.5)) 
abline(h=c(-2,0,2),lty=c(2,1,2)) 

# pour faire de la prediction avec le modele
#--------------------------------------------
res<-lm(vect.y~vect.x)  # estimation du modele
x0<-data.frame(vect.x=2) # construction de la table de donnees pour laquelle on veut des estimations
predict(res,new=x0,interval="pred",level=0.95) # estimation de y0
predict(res,new=x0,interval="conf",level=0.95) # estimation de E[y sachant que x=x0]

grille.x<-data.frame(vect.x=seq(from=min(vect.x),to=max(vect.x),length.out=50))
ICpred<-predict(res,new=grille.x,interval="pred",level=0.95)
ICmoy<-predict(res,new=grille.x,interval="conf",level=0.95)
plot(vect.x,vect.y)
abline(res,col=3,lwd=2)
matlines(grille.x,cbind(ICpred[,-1],ICmoy[,-1]),lty=c(2,2,3,3),col=c(2,2,4,4),lwd=c(2,2,2,2))
legend("topleft",lty=c(2,3),col=c(2,4),c("IC de prediction de y","IC de prediction de E[y]"))


#-----------------------------------------------------
# Etude par simulation numerique du biais 
# des estimateurs de beta0, de beta1 et de sigma
#-----------------------------------------------------

N<-200
CoefReg<-matrix(0,ncol=2,nrow=N)
sigma.estime<-rep(0,N)

n<-1000
sigma<-1
beta0<-3
beta1<-2

x<-runif(n,min=0,max=5)
for (j in 1:N){
	e<-rnorm(n,mean=0,sd=sigma)
	y<-beta0+beta1*x+e
	res<-lm(y~x)
	CoefReg[j,]<-coefficients(res)
	sigma.estime[j]<-sqrt(sum(res$residuals^2)/res$df.residual)
	}

apply(CoefReg,2,mean)
mean(sigma.estime)

par(mfrow=c(1,1))
boxplot(list(CoefReg[,1], CoefReg[,2],sigma.estime),names=c("beta0 estime","beta1 estime","sigma estime"))
abline(h=beta0,col=2)
abline(h=beta1,col=3)
abline(h=sigma,col=4)

#-----------------------------------------------------
# Etude par simulation numerique de la precision  
# de l'estimateur de beta0
# en fonction de la taille n de l'echantillon (pour sigma fixe)
#-----------------------------------------------------

N<-200

vect.n<-seq(from=100,to=1000,by=100)
sigma<-1
beta0<-3
beta1<-2
CoefReg<-matrix(0,ncol=length(vect.n),nrow=N)


for (k in 1:length(vect.n)){
	x<-runif(vect.n[k],min=0,max=5)
	for (j in 1:N){
		e<-rnorm(vect.n[k],mean=0,sd=sigma)
		y<-beta0+beta1*x+e
		res<-lm(y~x)
		CoefReg[j,k]<-coefficients(res)[1]
	}
}

liste <- list()
for (k in 1:length(vect.n)){
	liste[[k]]<-CoefReg[,k]
}
par(mfrow=c(1,1))
boxplot(liste,names=paste("n=",vect.n))
title("Estimation de Beta0")
abline(h=beta0,col=2)

#-----------------------------------------------------
# Etude par simulation numerique de la precision  
# de l'estimateur de beta0
# en fonction de l'ecart-type sigma de l'erreur (pour n fixe)
#-----------------------------------------------------

N<-200

n<-200
vect.sigma<-seq(from=0.1,to=10,length=10)

beta0<-3
beta1<-2
CoefReg<-matrix(0,ncol=length(vect.sigma),nrow=N)


for (k in 1:length(vect.sigma)){
	x<-runif(n,min=0,max=5)
	for (j in 1:N){
		e<-rnorm(n,mean=0,sd=vect.sigma[k])
		y<-beta0+beta1*x+e
		res<-lm(y~x)
		CoefReg[j,k]<-coefficients(res)[1]
	}
}

liste <- list()
for (k in 1:length(vect.sigma)){
	liste[[k]]<-CoefReg[,k]
}

par(mfrow=c(1,1))
boxplot(liste,names=paste("sig=",round(vect.sigma,digits=2)))
title("Estimation de Beta0")
abline(h=beta0,col=2)


#==================================================
# Prix des appartements en fonction de la surface
#==================================================

# Saisie des donnees
#--------------------
prix<-c(130,280,268,500,320,250,378,250,350,300,155,245,200,325,85,78,375,200,270,85)
surface<-c(28,50,55,110,60,48,90,35,86,65,32,52,40,70,28,30,105,52,80,20)

#####
# ou
#####
# "Chargement" des donnees
#------------------------
load("appartements.Rda")
ls()
appartements
prix
appartements$prix
appartements$surface
attach(appartements)
prix

plot(surface,prix,xlab="surface",ylab="prix")

res<-lm(prix~ surface)
summary(res)
abline(res,col=2,lwd=2)

plot(res$fitted,res$residuals)
abline(h=0,col=2)
plot(res$fitted,prix)
abline(0,1,col=2)

shapiro.test(res$residuals)

residus.stud<-rstudent(res) 
plot(residus.stud,ylab="res. student.",ylim=c(-3.5,3.5)) 
abline(h=c(-2,0,2),lty=c(2,1,2)) 

res0<-lm(prix~ -1+surface)
summary(res0)
abline(res0,col=2)

# Estimation du prix moyen (esperance) de vente pour differentes surfaces
#-------------------------------------------------------------------------
surf<-seq(from=20,to=110,by=10)
surf
res.pred<-predict(res,data.frame(surface=surf),interval="conf",level=0.95)
round(cbind(surf,res.pred),digits=1)

res.pred<-predict(res,data.frame(surface=surf),interval="pred",level=0.95)
round(cbind(surf,res.pred),digits=1)

#==================================================
# Etude des 4 jeux de donnees "pedagogiques"
#==================================================

load("donneesPedago.Rda")
ls()
donneesPedago
attach(donneesPedago)

#--------------------------------
# Premiere regression : y1 sur x
#--------------------------------

res1 <- lm(y1~x)
# ou si pas attach(donneesPedago)
res1 <- lm(y1~x,data=donneesPedago)   

summary(res1)
shapiro.test(res1$residuals)

par(mfrow=c(3,1))
plot(x,y1) 
# ou si pas attach(donneesPedago)
plot(donneesPedago$x,donneesPedago$y1) 

abline(res1)
plot(res1$fitted,res1$residuals)
abline(h=0,col=2)
residus.stud<-rstudent(res1) 
plot(residus.stud,ylab="res. student.",ylim=c(-3.5,3.5)) 
abline(h=c(-2,0,2),lty=c(2,1,2)) 

#--------------------------------
# Deuxieme regression : y2 sur x
#--------------------------------

res2<-lm(y2~x)
summary(res2)
shapiro.test(res2$residuals)

par(mfrow=c(3,1))
plot(x,y2)
abline(res2)
plot(res2$fitted,res2$residuals)
abline(h=0,col=2)
residus.stud<-rstudent(res2) 
plot(residus.stud,ylab="res. student.",ylim=c(-3.5,3.5)) 
abline(h=c(-2,0,2),lty=c(2,1,2)) 


#--------------------------------
# Troisieme regression : y3 sur x
#--------------------------------

res3<-lm(y3~x)
summary(res3)
shapiro.test(res3$residuals)

par(mfrow=c(3,1))
plot(x,y3)
abline(res3)
plot(res3$fitted,res3$residuals)
abline(h=0,col=2)
residus.stud<-rstudent(res3) 
plot(residus.stud,ylab="res. student.",ylim=c(-3.5,3.5)) 
abline(h=c(-2,0,2),lty=c(2,1,2)) 

#--------------------------------
# Quatrieme regression : y4 sur z
#--------------------------------

res4<-lm(y4~z)
summary(res4)
shapiro.test(res4$residuals)

par(mfrow=c(3,1))
plot(z,y4)
abline(res4)
plot(res4$fitted,res4$residuals)
abline(h=0,col=2)
residus.stud<-rstudent(res4) 
plot(residus.stud,ylab="res. student.",ylim=c(-3.5,3.5)) 
abline(h=c(-2,0,2),lty=c(2,1,2)) 


#===========================================================
# Nombre de bacteries ayant survecu
#===========================================================

# Saisie des donnees
#--------------------
nt<-c(355,211,197,166,142,106,104,60,56,38,36,32,21,19,15,12)
t<-c(1:16)
#####
# ou
#####
# "Chargement" des donnees
#--------------------------
load("bacteries.Rda")
bacteries
attach(bacteries)

# estimation d'un modele de regression lineaire simple
#------------------------------------------------------
res<-lm(nt~t)
summary(res)
shapiro.test(res$residuals)

par(mfrow=c(3,1))
plot(t,nt)
abline(res,col=2,lwd=2)
plot(res$fitted,res$residuals)
abline(h=0)
hist(res$residuals)

# linearisation du modele
#-------------------------
LOGnt<-log(nt)
resLOG<-lm(LOGnt~t)
summary(resLOG)
shapiro.test(resLOG$residuals)

par(mfrow=c(2,1))
plot(t,LOGnt)
abline(resLOG,col=3,lwd=2)
plot(resLOG$fitted,resLOG$residuals)
abline(h=0)

# retour a l'échelle d'origine
#------------------------------
par(mfrow=c(1,1))
plot(t,nt)
valeursEstim<-exp(coefficients(resLOG)[1])*exp(coefficients(resLOG)[2]*t)
lines(t,valeursEstim,col=3,lwd=2)
abline(res,col=2,lwd=2)


######################################################
# Annexes
######################################################
donnees <- c(10, 8.04,  9.14 ,7.46  ,6.58 ,8,
8  ,6.95  ,8.14 ,6.77  ,5.76 ,8,      
13, 7.58,  8.74, 12.74 ,7.71, 8,       
9 , 8.81,  8.77 , 7.11, 8.84, 8,        
11, 8.33 , 9.26 , 7.81, 8.47, 8,         
14 ,9.96 , 8.10,  8.84 ,7.04 ,8,          
6  ,7.24 , 6.13 , 6.08, 7.25, 8,           
4 , 4.26,  3.10,  5.39 ,12.5, 19,            
12, 10.84 ,9.13 , 8.15, 5.56 ,8,             
7,  4.82 , 7.26,  6.42 ,7.91 ,8,              
5 , 5.68,  4.74 , 5.73, 6.89 ,8)
donnees <- matrix(donnees,ncol=6,byrow=T)

x<-donnees[,1]
y1<-donnees[,2]
y2<-donnees[,3]
y3<-donnees[,4]
y4<-donnees[,5]
z<-donnees[,6]

donneesPedago <- data.frame(x,y1,y2,y3,y4,z)
save(donneesPedago,file="donneesPedago.Rda")


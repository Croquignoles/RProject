ranked <- read.csv("high_diamond_ranked_10min.csv")
head(ranked)
summary(ranked)
#mean(ranked[1,])
if(!is.null(dev.list())) dev.off()
ranked[1,]
GoldDiff <- max(ranked$blueGoldDiff)
GoldDiff

blueWinning <- subset(ranked, blueWins == 1)
print(blueWinning)
goldDiffWins <- mean(blueWinning[,18])
goldDiffWins
redWinning <- subset(ranked, blueWins == 0)
goldDiffWins <- mean(redWinning[,18])
-goldDiffWins

#Proba Blue wins
probaBlueWinning <- nrow(blueWinning)/nrow(ranked)

#proba de gagner et de faire first blood
firstBloodAndBlueWins <- subset(ranked, blueFirstBlood == 1 & blueWins == 1)
probaFirstBloodAndBlueWins <- nrow(firstBloodAndBlueWins)/nrow(ranked)

#proba de gagner apr?s avoir fait un first blood
FirstBlood <- subset(ranked, blueFirstBlood == 1)
probaFirstBlood = nrow(FirstBlood)/nrow(ranked)

probaWinKnowingfirstBlood = probaFirstBloodAndBlueWins/probaFirstBlood
probaWinKnowingfirstBlood

#drake
drakeBlueAndWins <- subset(ranked, blueDragons == 1 & blueWins == 1)
probaDrakeBlueAndWins <- nrow(drakeBlueAndWins)/nrow(ranked)
DrakeBlue <- subset(ranked, blueDragons == 1)
probaDrake = nrow(DrakeBlue)/nrow(ranked)
probaWinKnowingDrake <-probaDrakeBlueAndWins/probaDrake
probaWinKnowingDrake

#heralds

heraldBlueAndWins <- subset(ranked, blueHeralds == 1 & blueWins == 1)
probaHeraldBlueAndWins <- nrow(heraldBlueAndWins)/nrow(ranked)
HeraldBlue <- subset(ranked, blueHeralds == 1)
probaHerald = nrow(HeraldBlue)/nrow(ranked)
probaWinKnowingHerald <-probaHeraldBlueAndWins/probaHerald
probaWinKnowingHerald

#death
VDeathsBlueAndWins <- subset(ranked, blueDeaths > 2 & blueWins == 1)
proba5DeathBlueAndWins <- nrow(VDeathsBlueAndWins)/nrow(ranked)
VDeathsBlue <- subset(ranked, blueDeaths > 2)
probaVDeaths = nrow(VDeathsBlue)/nrow(ranked)
probaWinKnowing5Deaths <-proba5DeathBlueAndWins/probaVDeaths
probaWinKnowing5Deaths

#test de shapiro
shapiro.test(ranked$blueWins[1:5000])
wilcox.test(ranked$blueWins[1:5000])
#pas de loi normale p-value significative
shapiro.test(ranked$blueTotalExperience[2501:7500])
#max(ranked$blueDeaths & ranked$blueWins==1)

mean(ranked$blueKills)
mean(ranked$redKills)
#écart non significatif entre les 2 côtés en terme de kill à 10 min

boxplot(list(ranked$blueKills,ranked$blueAssists))
#Comparaison entre les kills et les assists

plot(ranked$blueTotalGold,ranked$blueKills)
cor(ranked$blueTotalGold,ranked$blueKills)
#presque 0.90 corrélation intéressante mais logique kills rapportent des golds
if(!is.null(dev.list())) dev.off()
lm(ranked$blueTotalGold~ranked$blueKills+0)$coefficients

# Régression linéaire kill vs golds:
abscisses <- ranked$blueTotalMinionsKilled ; ordonnées <- ranked$blueDeaths
plot(abscisses,ordonnées ) 
abline(lm(ordonnées ~abscisses)$coefficients, col="red",lwd=2)
cor(ranked$blueTotalMinionsKilled,ranked$blueDeaths)
values <-list(nrow(subset(ranked, blueKills+redKills == 0 )),nrow(subset(ranked, blueKills+redKills < 2)))
barplot(values)


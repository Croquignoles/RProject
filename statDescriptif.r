install.packages("FactoMineR")
install.packages("factoextra")
install.packages("corrplot")

library("FactoMineR")
library("factoextra")
library("corrplot")

load("donneesProjet2A.RData")
attach(donneesProjet)

#Statistique Unidimensionnel
summary(donneesProjet)

#Statistic Bidimensionnel
plot(donneesProjet[, 1:14])

# plot(Pct.BF ~ Age)
# plot(Pct.BF ~ Weight)
# plot(Pct.BF ~ Height)
# plot(Pct.BF ~ Neck)
# plot(Pct.BF ~ Chest)
# plot(Pct.BF ~ Abdomen)
# plot(Pct.BF ~ Hip)
# plot(Pct.BF ~ Thigh)
# plot(Pct.BF ~ Knee)
# plot(Pct.BF ~ Ankle)
# plot(Pct.BF ~ Bicep)
# plot(Pct.BF ~ Forearm)
# plot(Pct.BF ~ Wrist)



res.pca <- PCA(donneesProjet, graph = FALSE)

eig.val <- get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

fviz_pca_var(res.pca, col.var = "black")
# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)


var <- get_pca_var(res.pca)
var
# Coordinates
var$coord[,1:2]
# Cos2: quality on the factore map
var$cos2[,1:2]
corrplot(var$cos2[,1:3], is.corr=FALSE)
# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2)

# Contributions to the principal components
var$contrib[,1:2]
corrplot(var$contrib[,1:3], is.corr=FALSE)


ind <- get_pca_ind(res.pca)
ind
# Coordinates of individuals
head(ind$coord,20)
# Quality of individuals
head(ind$cos2,20)
# Contributions of individuals
head(ind$contrib,20)
fviz_pca_ind(res.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
)

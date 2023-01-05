install.packages("FactoMineR", dependencies = TRUE)
install.packages("factoextra", dependencies = TRUE)
install.packages("corrplot")

library("FactoMineR")
library("factoextra")
library("corrplot")

load("donneesProjet2A.RData")
attach(donneesProjet)
length(donneesProjet[, 1])

# Statistique Unidimensionnel
summary(donneesProjet)

# Statistic Bidimensionnel
library(GGally)
ggpairs(donneesProjet)

#ACP
res_pca <- PCA(donneesProjet, graph = FALSE)

eig_val <- get_eigenvalue(res_pca)
eig_val
fviz_eig(res_pca, addlabels = TRUE, ylim = c(0, 50))

fviz_pca_var(res_pca, col.var = "black")
# Color by cos2 values: quality on the factor map
fviz_pca_var(res_pca,
    col.var = "cos2",
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    repel = TRUE # Avoid text overlapping
)
fviz_pca_var(res_pca,
    col.var = "contrib",
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)


var <- get_pca_var(res_pca)
var
# Coordinates
var$coord[, 1:3]
# Cos2: quality on the factore map
var$cos2[, 1:3]
corrplot(var$cos2[, 1:3], is.corr = FALSE)
# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(res_pca, choice = "var", axes = 1:2)
# Total cos2 of variables on Dim.2 and Dim.3
fviz_cos2(res_pca, choice = "var", axes = 2:3)

# Contributions to the principal components
var$contrib[, 1:3]
corrplot(var$contrib[, 1:3], is.corr = FALSE)


ind <- get_pca_ind(res_pca)
ind
# Coordinates of individuals
head(ind$coord, 20)
# Quality of individuals
head(ind$cos2, 20)
# Contributions of individuals
head(ind$contrib, 20)
fviz_pca_ind(res_pca,
    col.ind = "cos2",
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    repel = TRUE,
)

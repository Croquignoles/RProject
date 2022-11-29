lol <- read.csv("high_diamond_ranked_10min.csv")
library("FactoMineR")
library("factoextra")
res.pca <- PCA(lol, graph = FALSE)
print(res.pca)
eig.val <- get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
var <- get_pca_var(res.pca)
var
# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)

head(var$coord, 4)
fviz_pca_var(res.pca, col.var = "black")
head(var$cos2, 4)
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)
# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2)
# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 20)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 20)
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Create a grouping variable using kmeans
# Create 3 groups of variables (centers = 3)
set.seed(123)
res.km <- kmeans(var$coord, centers = 4, nstart = 25)
grp <- as.factor(res.km$cluster)
# Color variables by groups
fviz_pca_var(res.pca, col.var = grp, 
             palette = c("#0073C2FF", "#dc143c", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")
res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1
# Description of dimension 2
res.desc$Dim.2
ind <- get_pca_ind(res.pca)
ind
# Coordinates of individuals
head(ind$coord)
# Quality of individuals
head(ind$cos2)
# Contributions of individuals
head(ind$contrib)
fviz_pca_ind(res.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB","#dc143c", "#E7B800", "#FC4E07"),
             repel = TRUE,
             max.overlaps = 9879
)
fviz_cos2(res.pca, choice = "ind")
# Total contribution on PC1 and PC2
fviz_contrib(res.pca, choice = "ind", axes = 1:2)

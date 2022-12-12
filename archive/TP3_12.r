load("archive/plaques.Rda")
attach(plaques)

head(plaques)
levels(duree)
table(duree)

plot(rugosite~duree)

res <- lm(rugosite~duree)
anova(res)

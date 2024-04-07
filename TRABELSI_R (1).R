######################################## LIBRAIRIES ########################################

install.packages("dplyr")
install.packages("effects")
install.packages("MASS")
install.packages("mfx")
install.packages("lmtest")
install.packages("Hmsic")
install.packages("AER")
install.packages("labelled")
install.packages("stargazer")
install.packages("ordinal")
install.packages("ggeffects")
install.packages("DescTools")

library(dplyr)
library(ordinal)
library(MASS)
library(lmtest)
library(effects)
library(Hmisc)
library(AER)
library(labelled)
library(stargazer)
library(mfx)
library(ggeffects)
library(DescTools)

######################################## CHARGEMENT BASE DE DONNÉES ########################################

data("HealthInsurance")
attach(HealthInsurance)

tail(HealthInsurance)
head(HealthInsurance)

######################################## STATISTIQUES DESCRIPTIVES   #######################################

#Nettoyage

str(HealthInsurance)

data <- distinct(HealthInsurance)
dim(HealthInsurance)
dim(data)

describe(data)
which(is.na(data),arr.ind=TRUE)

#Presentation

summary(data)
var(age)
var(family)
sd(age)
sd(family)
describe(data1)
sapply(data,range)

# Health

table <- table(data$health)

prop.table(table)

barplot(table, main="Répartition des modalités de la santé", 
        xlab="Modalités",
        col="orange")

# Insurance

table_2 <- table(data$insurance)

prop.table(table_2)

barplot(table_2, main="Répartition des modalités de l'assurance", 
        xlab="Modalités",
        col="orange")

# Education

table_3 <- table(data$education)

prop.table(table_3)

barplot(table_3, main="Répartition des modalités au niveau du diplôme", 
        xlab="Modalités",
        col="orange")

######################################## REGRESSION LOGIT BINAIRE ########################################

# Régression logistique binaire

reg <- glm(health ~ age + insurance + family + married, family = binomial(logit), data = HealthInsurance)
reg

summary(reg)

stargazer(reg, title="Regression logistique binaire", align=TRUE, type = 'latex', header=FALSE, out="reg1.html") # output HTML

# Odds ratios

logitor(health ~ insurance + family + married, data=HealthInsurance)

cbind(Estimate=round(coef(reg),4),
      OR=round(exp(coef(reg)),4))

logit.or = exp(coef(reg))
logit.or

stargazer(reg, type="html", coef=list(logit.or), p.auto=FALSE, out="logitor.htm")


# Effets marginaux

EFBinaire <- logitmfx(health ~ insurance + family + married, data=HealthInsurance)
EFBinaire 

# Prédictions logistique binaire 

prob = predict(reg, type="response")
reg_predict <- cbind(HealthInsurance, prob)

ggplot(reg_predict, aes(x = health, y = prob, color = as.factor(insurance))) +
  geom_line() +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  scale_color_brewer(palette = "Dark2")


# Représentation graphique de l'effet de l'âge et de la famille

plot(ggeffect(reg, "age"))
plot(ggeffect(reg, "family"))


# R^2 pour logit binaire

pseudologit <- PseudoR2(reg, which = "all")

stargazer(pseudologit, title="Tests Pseudo R^2 / Binaire", align=TRUE, type = 'latex', header=FALSE, out="tests1.html")

# ANOVA ( on a pas pu le mettre dans la prÃ©sentation )

reg_anova <- glm(health ~ 1, data = HealthInsurance, family = binomial(logit))
anova1 <- anova(reg, reg_anova, test="LR")

stargazer(anova1, title="ANOVA / Regression logistique binaire", align=TRUE, type = 'latex', header=FALSE, out="anova_reg1.html")

# Test de vraisemblance logit

rapport_vraisemblance_binaire = 2*(logLik(reg)-logLik(reg_anova))

######################################## REGRESSION PROBIT BINAIRE ########################################

# Régression logistique binaire

reg2 <- glm(health ~ age + insurance + family + married, family = binomial(probit), data = HealthInsurance)
reg2

summary(reg2)

stargazer(reg2, title="Regression probit binaire", align=TRUE, type = 'latex', header=FALSE, out="reg2.html") # output HTML


# Effets marginaux

EFBinaire2 <- probitmfx(health ~ insurance + family + married, data=HealthInsurance)
EFBinaire2

# Prédictions probit binaire 

prob2 = predict(reg2, type="response")
reg_predict2 <- cbind(HealthInsurance, prob2)

ggplot(reg_predict2, aes(x = health, y = prob2, color = as.factor(insurance))) +
  geom_line() +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  scale_color_brewer(palette = "Dark2")


# Représentation graphique de l'effet du salaire

plot(ggeffect(reg2, "age"))
plot(ggeffect(reg2, "family"))


# R^2 pour logit binaire

pseudoprobit <- PseudoR2(reg2, which = "all")

stargazer(pseudoprobit, title="Tests Probit Pseudo R^2 / Binaire", align=TRUE, type = 'latex', header=FALSE, out="tests2.html")

# ANOVA ( on a pas pu le mettre dans la prÃ©sentation )

reg_anova <- glm(health ~ 1, data = HealthInsurance, family = binomial(logit))
anova2 <- anova(reg2, reg_anova, test="LR")

stargazer(anova2, title="ANOVA / Regression logistique binaire", align=TRUE, type = 'latex', header=FALSE, out="anova_reg1.html")

# Test de vraisemblance logit

rapport_vraisemblance_binaire = 2*(logLik(reg2)-logLik(reg_anova))

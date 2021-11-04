data <- read.table(file.choose(),header=TRUE,sep="!")
head(data)
# les varaibles sont quantitatives 
# on prédire Y par rapport à les autres varaibles donc on va effectuer une RLM 
#2 : matrice des corr???lations pour associer une valeur aux impressions visuels
correlation=cor(data)

#1 : r???alisation de tous les croisements de variables pour d???finir la forme du lien ??? appliquer
for (i in 1:7)
{
  plot(data[,i],data[,8],xlab=colnames(data)[i],ylab="aze")
  text(data[,i],data[,8],rownames(data))
  s=readline('taper sur entr???e pour le graphique suivant')
}
library(car)
#3 :Estimation des beta et de sigma???
reg1=lm(Y~.,data=data)
#Etude de la multicolinearit??? : VIF>4?
vif(reg1)
reg_multi1=lm(X3~X1+X2+X4+X5+X6+X7,data=data)
summary(reg_multi1)
# on supprime X3 
reg2=lm(Y~X1+X2+X4+X5+X6+X7,data=data)
vif(reg2)
reg_multi2=lm(X6~X1+X2+X4+X5+X7,data=data)
summary(reg_multi2)
# ON SUPPRIME x5 
reg3=lm(Y~X1+X2+X4+X6+X7,data=data)
vif(reg3)

summary(reg3)

reg4=lm(Y~X1+X4+X6+X7,data=data)
summary(reg4)

reg5=lm(Y~X7+X6+X4+X1,data=data)
summary(reg5)
reg6=lm(Y~X1+X4+X6,data=data)
summary(reg6)
reg7=lm(Y~X4+X6,data=data)
summary(reg7)

#mod???le final obtenu : 
#Y=-5.203e-04*X4+X6*-3.568e-04
#Poste analyse
#v???rification des hypoth???ses :
#etude des r???sidus:
#mediane = 0.0319 proche de 0
#|Q1| proche de Q3 et |min| proche du max => r???sidus centr???s  
plot(reg7$fitted.values,reg7$residuals)
abline(h=0)

plot(1:dim(data)[1],reg7$residuals,type="b")
abline(h=0)
#droite de Henry
qqPlot(reg7)
#aucun souci a prioro, juste le 60 est ??? l'ext???rieur de l'intervalle de confiance
#ensemble de graphiques de post analyse obtenus directement :
plot(reg7)
#recherche des points aberrants ou influants
influencePlot(reg7)
plot(1:dim(data)[1],rstudent(reg7))
# Recherche des points aberrants ou influants 
#StudRes        Hat       CookD
#10 -0.1755987 0.10214469 0.001185268
#43 -2.5014769 0.05437470 0.111774310
#57  0.6478246 0.09438628 0.014698538
#60 -3.5877694 0.04162913 0.159995067
#r???sidus studentiz???s
plot(1:dim(data)[1],rstudent(reg7))
abline(h=2)
abline(h=-2)
print(rownames(data)[rstudent(reg7)>2])
print(rownames(data)[rstudent(reg7)<=-2])
# problème avec 60 qui a une valeur de -3.90


#force de levier
plot(1:dim(data)[1],hatvalues(reg7))
abline(h=0.08) #p=2, n=75 2(p+1)/n = 0.08
print(rownames(data)[hatvalues(reg7)>0.08])
print(hatvalues(reg7)[hatvalues(reg7)>0.08])
#6,10,38,40,57,74 se trouve au dessous de la 
# surtout avec 6,38,7

#points influents : distance de cook
plot(1:dim(data)[1],cooks.distance(reg7))
abline(h=1)
abline(h=0.055)#4/(n-p-1)=0.055
print(rownames(data)[cooks.distance(reg7)>0.055])
print(cooks.distance(reg7)[cooks.distance(reg7)>0.055])
#

#distance de Welsh 2*sqrt((p+1)/n), p=2, n=75
plot(1:dim(data)[1],dffits(reg7))
abline(h=0.4)#2*sqrt((2+1)/75)=
abline(h=-0.4)
print(rownames(data)[dffits(reg7)>0.4])
print(rownames(data)[dffits(reg7)<=-0.4])
print(dffits(reg7)[dffits(reg7)>0.4])
print(dffits(reg7)[dffits(reg7)<=-0.4])
#idem distance de cook
#Au vu de cette post analyse,il faudrait supprimer cet individu 60 

valpredreg=predict(reg7)
print(valpredreg)

summary(reg7)
#R???gression sur composante principale
#n???cessite l'utilisation du package pls
library(pls)
#besoin de d???terminer le nombre de composantes ??? retenir
#On va r???aliser une ACPN => centrage et r???duction des donn???es
#Les donn???es sont centr???es par d???faut mais pas r???duites d'o??? l'option scale=T
reg_rcp=pcr(Y~.,data=data,scale=TRUE)
#calcul du RMSE
reg_mse=RMSEP(reg_rcp)
#repr???sentation graphique
plot(1:7,reg_mse$val[1,,2:8],type="b")

#Baisse du RMSE jusqu'??? la valeur de 8. Ensuite stabilisation des r???sultats
#pourcentage d'information expliqu??? par chaque axe
plot(explvar(reg_rcp),type="b")
print(explvar(reg_rcp))
print(cumsum(explvar(reg_rcp)))
#choix en validation crois???e - k=10 blocs par d???faut sous R
#CV par d???faut
reg_rcp_cv=pcr(data~.,data=data,scale=TRUE,validation='CV')
#calcul du RMSE
reg_rmse_cv=RMSEP(reg_rcp_cv,estimate=c("train","CV"))
#r???alisation du graphique
plot(1:7,reg_rmse_cv$val[8,,1:7],type='b',col='red',ylim=c(min(reg_rmse_cv$val[1,,]),max(reg_rmse_cv$val[2,,])))
points(1:12,reg_rmse_cv$val[2,,2:13],type='b',col='blue')
#mod???le final avec 8 composantes
reg_rcp_final=pcr(inves~.,data=investis,ncomp=8,scale=T)
#valeurs pr???dites
valpredrcp=predict(reg_rcp_final,newdata=investis)[,1,8]
#RMSE : 0.07105
RMSEP(reg_rcp_final)
#Equation de la r???gression
#attention, on r???cup???re les coefficients pour les donn???es centr???es r???duites
reg_rcp$coefficients[,,8]

#regression ridge 
#n???cessite le package glmnet
library(glmnet)
#d???finition des valeurs de lambda ??? tester
ens_lambda=seq(1,0,-0.01)
reg_ridge=glmnet(data.matrix(investis[,2:13]),investis[,1],alpha=0,lambda=ens_lambda)

#Estimations des constantes pour toutes les valeurs de lambda
coef_ridge=coef(reg_ridge)
plot(ens_lambda,coef_ridge[1,],type='l',ylim=c(min(coef_ridge),max(coef_ridge)))
for (i in 2:13)
{
  points(ens_lambda,coef_ridge[i,],type='l')
}
#stabilisation ??? partir de 0.4

#recherche de la valeur de lambda qui minimise le RMSE en validation crois???e
essai=cv.glmnet(data.matrix(investis[,2:13]),investis[,1],alpha=0,lambda=ens_lambda)
essai$lambda.min
#valeur de lambda=0.02

#calcul des coefficients (pour une valeur de lambda=0.4, le 0.02 ne me semble pas judicieux au vu des graphiques)
reg_ridge_final=glmnet(data.matrix(investis[,2:13]),investis[,1],alpha=0,lambda=0.02)
#estimation des coefficients
coef(reg_ridge_final)

#valeurs pr???dites :
valpredridge=predict(reg_ridge_final,newx=as.matrix(investis[,2:13]))
#RMSE : 0.01140
sqrt(sum((investis[,1]-valpredridge)^2/dim(investis)[1]))
#Les coefficients donn???s correspondent ??? ceux sur les donn???es d'origine

#R???gression PLS
#n???cessite comme tout ??? l'heure la librairie pls
#On doit diviser les donn???es en deux matrices : X et y
X=as.matrix(investis[,2:13])
y=as.matrix(investis[,1])
#Comme tout ??? l'heure le centrage est fait pas d???faut mais pas la r???duction d'o??? la pr???sence de l'option scale=T
reg_pls <- mvr(y ~ X, ncomp = 12, method = "oscorespls", scale = TRUE)
summary(reg_pls)
#repr???sentation graphique associ???e aux % d'info cumul???es sur X et sur y
plot(cumsum(explvar(reg_pls)),type="b")

#calcul du RMSE
reg_pls_mse=RMSEP(reg_pls)
plot(1:12,reg_pls_mse$val[1,,2:13],type="b")
plot(reg_pls,'val')
#5 axes sur RMSE

#en validation crois???e
set.seed(1)
reg_pls_cv <- mvr(y ~ X, ncomp = 12, method = "oscorespls", scale = TRUE,validation='CV')
summary(reg_pls_cv)
validationplot(reg_pls_cv,'MSEP')
#peu d'???volution de l'erreur entre 2 et 5 axes
validationplot(reg_pls_cv,'R2')

#avec 2 axes
reg_pls_final <- mvr(y ~ X, ncomp = 2, method = "oscorespls", scale = TRUE)
summary(reg_pls_final)
plot(reg_pls_final,line=T)

#equation 
#coefficients sur donn???es centr???es r???duites
coef(reg_pls_final)

#valeurs pr???dites
valpredpls=predict(reg_pls_final,newdata=X)
#RMSE : 0.0803
RMSEP(reg_pls_final)

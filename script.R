#si l'un des packages n'est pas installé :
#install.packages("ggplot2")
#install.packages("moments")
#install.packages("dplyr")
#install.packages("Hmisc")
#install.packages("corrplot")
#install.packages(c("FactoMineR", "factoextra"))
#install.packages("psy",dep=T)
#install.packages("ltm",dep=T)
#install.packages("Rcmdr",dep=T)

library(moments)
library("ggplot2")
library(dplyr)
library(psy)
library(ltm)
library("FactoMineR")
library("factoextra")
library(Hmisc)
library(corrplot)
library(car) # vif
library(tidyverse) # plot et regression




#Etape 2: collecte des données 
# importation des données et la converstion des chaînes de caractére en factor
data = read.csv('d.csv',
                stringsAsFactors = T,  # pour convertir en Factors 
                fileEncoding = "UTF-8", # spécifier l'encodage du fichier
                na.strings=c(".","NA")) # afin qu'il considère bien les "." comme des NA




#Etape 3:pretraitement

#supprimer qlqs lignes aléatoirement
data = data[-c(3,29,50),]

#supprimer les lignes des individus qui ne sont pas  Ci1 génie info
data = data[data[,"Filière"]=="Génie Informatique" & data[,"Niveau.d.étude"]=="CI1",]
View(data)


# supprimer la premiére collonne(Horodateur) et la deuxiéme collonne(Nom.d.utilisateur)
data = data[,-c(1,2,6,7)]

#3.1 conversion :codification des variables
saveNames = names(data)[4:ncol(data)]
names(data)[4:ncol(data)]=paste("Item",1:(ncol(data)-3),sep="")


rownames(data)=1:nrow(data)
#3.2 conversion :codification du contenu

if(is.factor(data$Age)){
  data$Age = as.integer(data$Age)
}



#3.2 nettoyage
#3.2.1 traiter les valeurs aberrantes:

boxplot(data$Age)
boxplot.stats(data$Age)$out
for (out in boxplot.stats(data$Age)$out) {
  for (i in 1:length(data$Age)) {
    if(data$Age[i]==out && !is.na(data$Age[i])){
      data$Age[i]=NA
    }
  }
}

View(data)

#3.2.2 valeurs manquantes:
#frequence NA
fNa = sum(is.na(data$Age))/length(data$Age)
fNa

# 3 cas: realimenter ,estimer,supprimer

#ce code estime/supprime 

if(fNa<0.05 && (length(data$age)-sum(is.na(data$age))>=30)){
  print("supprimer")
  ligneNa = which(is.na(data),arr.ind = T)
  data = data[-c(ligneNa[,1]),]
}else{
  print("estimer")
  for (i in 1:length(data$Age)) {
    if(is.na(data$Age[i])){
      data$Age[i]=median(data$Age,na.rm =TRUE)
    }
  }
}

boxplot(data$Age)

#3.3:test de normalite 
#3.3.1: test de normalite selon le sens de shapiro

#Présentez age sous forme de tableau
tab = as.data.frame(table(data$Age))
colnames(tab) = c("Age","Nbr")
tab["f"] = tab["Nbr"] / nrow(data)
tab

#calcule des fréquences cummulées

tab = tab[order(tab$Age),]
tab["F"] = cumsum(tab["f"])
tab

#histogramme d'age

hist(data$Age, 
     main= "Age", 
     xlab="Effectif", 
     border="white", 
     col="blue",
     xlim=c(17,22),
     las=1, 
     breaks=5)
hist(data$Age , xlim = c(17,22),breaks =4)
dens=density(data$Age)
plot(dens)
summary(data$Age)

#les mesures de tendance centrale

#cette fonction nous calcule le mode 
mode = function(v) {
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#les mesures de dispersion
#variance empirique

var(data$Age)

#Pour corriger ce biais
#variance empirique corrigée/variance empirique sans biais*

n = length(data$Age)
var(data$Age)*(n-1)/n

#l'écart-type

sd(data$Age)
#l'écart-type empirique divisé par la moyenne

n = length(data$Age)
sd(data$Age)*(n-1)/n

if(T){
  print("--------------------")
  print("mod");print(mode(data$Age))
  print("moy");print(mean(data$Age))
  print("med");print(median(data$Age))
  print("var");print(var(data$Age))
  print("std");print(sd(data$Age))
  print("skw");print(skewness(data$Age))
  print("kur");print(kurtosis(data$Age))
}


print(ggplot(data, aes(Age)) + geom_histogram(bins=20) ) 
print(ggplot(data, aes(x="", y=Age)) + geom_boxplot() + coord_flip() )


#test de normalité de la variable age

shapiro.test(data$Age)
if(shapiro.test(data$Age)[2]>0.05){
  print("Age suit la loi normale")
  print("On va utiliser les tests paramétrique")
}else{
  print("Age ne suit pas la loi normale")
  print("Vérifiant s'il est quasi normale")
  #3.3.2:test de quasi-normalite de l'age
  if(skewness(data$Age)<3 & -3<skewness(data$Age)& -3<kurtosis(data$Age) & kurtosis(data$Age)<3){
    print("Age est quasi normale")
    print("On va utiliser les tests paramétrique et non paramétrique")
  }else{
    print("Age n'est ni normale ni qusi normale")
    print("On va utiliser que les tests non paramétrique")
  }
}


#plot genre
#exploration graphique
#Représentez la distribution empirique d'une variable

# VARIABLE QUALITATIVE
value_counts = as.data.frame(table(data$Genre))
colnames(value_counts) = c("Genre","effectifs")

genre=c("F","H")
effectifs=c(21,21)
# Diagramme en secteurs
ggplot(value_counts, aes("",effectifs,fill=genre)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y")



#test de representativité


prop.test(table(data$Genre) , p = 0.36)#p-value = 0.08372

chisq.test(table(data$Genre) , p = c(0.36,0.64))#p-value 0.05873

if(chisq.test(table(data$Genre) , p = c(0.36,0.64))[3]>0.05){
  print("Il n'y a pas de diff?rence significatif entre les proportionnalités dans l'échantillon et la population")
  print("Il y a la représentativité")
}else{
  print("Il y a de diférence significatif entre les proportionnalités dans l'échantillon et la population")
  print("Il n'y a pas la représentativité")
}

chisq.test(table(data$Genre) , p = c(0.5,0.5))
if(chisq.test(table(data$Genre) , p = c(0.5,0.5))[3]>0.05){
  print("Il n'y a pas de différence significatif entre les proportionnalités dans l'échantillon et la population")
  print("Il y a l'équilibre")
}else{
  print("Il y a de différence significatif entre les proportionnalités dans l'échantillon et la population")
  print("Il n'y a pas l'équilibre")
}

#créer un nouveau dataframe contenant que les items

dataItems = data[,-c(1,2,3,28,33,42,43)] 


#cette boucle pour codifier les modalités 

for (i in 1:ncol(dataItems)) {
  dataItems[,i]=as.character(dataItems[,i])
  dataItems[dataItems[,i]=="Très d'accord",i]<-5
  dataItems[dataItems[,i]=="D'accord",i]<-4
  dataItems[dataItems[,i]=="Neutre",i]<-3
  dataItems[dataItems[,i]=="Pas d'accord",i]<-2
  dataItems[dataItems[,i]=="Pas du tout d'accord",i]<-1
}

#conversion en factor
for (i in 1:ncol(dataItems)){
  dataItems[,i]=as.factor(dataItems[,i])
}
summary(dataItems)

#summary des items
for (i in 1:ncol(dataItems)) {
  print("--------------------------")
  print(names(dataItems)[i])
  print(summary(dataItems[,i]))
}

#reste de la codification
#Item30/Item39 <=> non/oui , situation familiale célib/mar <=> 1/2 ,genre F/H <=> 1/2
levels(data$Item30)=c(0,1)
levels(data$Item39)=c(0,1)
levels(data$Genre)=c(1,2)
levels(data$Situation.Familiale)=c(1,2)




# remplacer les colonnes des items dans le premier dataframe par les nouveaux qui sont codifiés
data[,-c(1,2,3,28,33,42,43)] = dataItems

summary(dataItems)
View(dataItems)
View(data)

summary(data)

#conversion des variavles en numeric
for (i in 1:ncol(data)) {
  if(is.factor(data[,i])){
    data[,i] = as.numeric(data[,i])
  }
}

for (i in 1:ncol(dataItems)) {
  if(is.factor(dataItems[,i])){
    dataItems[,i] = as.numeric(dataItems[,i])
  }
}

#separer les items selon les themes

theme1 <- dataItems[,1:12]
theme2 <- dataItems[,13:24]
theme3 <- dataItems[,25:40]

#pre test du questionnaire:

#Test d'association

#theme1
if(T){
  print("-------------------------------------------------")
  print("Test d'association entre les items")
  print(bartlett.test(theme1))
  if(bartlett.test(theme1)[3]>0.05){
    print("H0 est vérifié")
    print("il n'ya pas d'association/significative entre les items")
  }else{
    print("H1 est vérifié")
    print("il ya une association/correlation significative entre les items")
  }
}

#theme2
if(T){
  print("-------------------------------------------------")
  print("Test d'association entre les items")
  print(bartlett.test(theme2))
  if(bartlett.test(theme2)[3]>0.05){
    print("H0 est vérifié")
    print("il n'ya pas d'association/significative entre les items")
  }else{
    print("H1 est vérifié")
    print("il ya une association/correlation significative entre les items")
  }
}

#theme3
if(T){
  print("-------------------------------------------------")
  print("Test d'association entre les items")
  print(bartlett.test(theme3))
  if(bartlett.test(theme3)[3]>0.05){
    print("H0 est vérifié")
    print("il n'ya pas d'association/significative entre les items")
  }else{
    print("H1 est vérifié")
    print("il ya une association/correlation significative entre les items")
  }
}

#tout les items
if(T){
  print("-------------------------------------------------")
  print("Test d'association entre les items")
  print(bartlett.test(dataItems))
  if(bartlett.test(dataItems)[3]>0.05){
    print("H0 est vérifié")
    print("il n'ya pas d'association/significative entre les items")
  }else{
    print("H1 est vérifié")
    print("il ya une association/correlation significative entre les items")
  }
}

#la force d'association
#=> le test de fiabilite du questionnaire (il ya correlation multiple des items)

# dans le cas de variables multiples, on utilise l'alpha de crobach, il explique la force de la correlation multiple 
# on determine le seuil de coherance a partir de 0.6
#on peut dire que le questionnaire(Chaque théme) est fiable si alpha > 0.6


#le graphique d'effondrement de la variance expliquee par les composants
#le seuil est de 60% = 0.6

#theme1

cronbach(theme1)
cronbach.alpha(theme1)
if(cronbach.alpha(theme1)[1]>0.6){
  print("Le theme 1 est fiable")
}else{
  print("Le theme 1 n'est pas fiable")
}

#theme2
cronbach(theme2)
cronbach.alpha(theme2)
if(cronbach.alpha(theme2)[1]>0.6){
  print("Le theme 2 est fiable")
}else{
  print("Le theme 2 n'est pas fiable")
}

#theme3
cronbach(theme3)
cronbach.alpha(theme3)
if(cronbach.alpha(theme3)[1]>0.6){
  print("Le theme 3 est fiable")
}else{
  print("Le theme 3 n'est pas fiable")
}

#tout les items
cronbach(dataItems)
cronbach.alpha(dataItems)
if(cronbach.alpha(dataItems)[1]>0.6){
  print("Le questionnaire est fiable")
}else{
  print("Le questionnaire n'est pas fiable")
}


#Traitement des themes fiables

#Analuse multivarié ACP

library(Rcmdr)
#Theme 2
summary(PCA(theme2, scale.unit = TRUE, ncp = 5, graph = TRUE)) #ncp nmbr de dimensions conservés vers la fin


#Premiére itération
rTh2 <- reliability(cov(theme2[,c("Item13","Item14","Item15","Item16","Item17",
                                  "Item18","Item19","Item20","Item21","Item22","Item23","Item24")], 
                        use="complete.obs"))
rTh2$alpha
rTh2$rel.matrix[rTh2$rel.matrix[,'Alpha']>rTh2$alpha,]
theme2_0 <- theme2[,-c(1,2,7,10)]
#Deuxiéme itération
rTh2 <- reliability(cov(theme2_0[,c("Item15","Item16","Item17",
                                    "Item18","Item20","Item21","Item23","Item24")], 
                        use="complete.obs"))


rTh2$alpha
rTh2$rel.matrix[rTh2$rel.matrix[,'Alpha']>rTh2$alpha,]
theme2_0 <- theme2_0[,-c(2,7)]
#Troisiéme itération
rTh2 <- reliability(cov(theme2_0[,c("Item15","Item17",
                                    "Item18","Item20","Item21","Item24")], 
                        use="complete.obs"))

rTh2$alpha
rTh2$rel.matrix
rTh2$rel.matrix[rTh2$rel.matrix[,'Alpha']>rTh2$alpha,]
theme2_0 <- theme2_0[,-1]
#Quatriéme itération
rTh2 <- reliability(cov(theme2_0[,c("Item17",
                                    "Item18","Item20","Item21","Item24")], 
                        use="complete.obs"))


rTh2$alpha
rTh2$rel.matrix
rTh2$rel.matrix[rTh2$rel.matrix[,'Alpha']>rTh2$alpha,]
theme2_0 <- theme2_0[,-5]

#Cinquiéme itération
rTh2 <- reliability(cov(theme2_0[,c("Item17",
                                    "Item18","Item20","Item21")], 
                        use="complete.obs"))


rTh2$alpha
rTh2$rel.matrix
rTh2$rel.matrix[rTh2$rel.matrix[,'Alpha']>rTh2$alpha,]
theme2_0 <- theme2_0[,-4]
#Sixiéme itération
rTh2 <- reliability(cov(theme2_0[,c("Item17",
                                    "Item18","Item20")], 
                        use="complete.obs"))


rTh2$alpha
rTh2$rel.matrix
rTh2$rel.matrix[rTh2$rel.matrix[,'Alpha']>rTh2$alpha,]
theme2_0 <- theme2_0[,-4]

PC <- princomp(~Item17+Item18+Item20,
               cor=TRUE, data=theme2_0)

summary(PC)
plot(PC)

summary(PCA(theme2_0, scale.unit = TRUE, ncp = 5, graph = TRUE))
#C'est comme une loop ? chaque  item doit être supprimer
#donc theme 2 va être traiter item par item

#Theme 3
summary(PCA(theme3, scale.unit = TRUE, ncp = 5, graph = TRUE))
#Premiére itération
rTh3 <- reliability(cov(theme3[,c("Item26","Item27","Item28","Item29","Item31",
                                  "Item32","Item33","Item34","Item35","Item36","Item37","Item38","Item41",
                                  "Item42","Item43","Item44")], use="complete.obs"))


rTh3$alpha
rTh3$rel.matrix[rTh3$rel.matrix[,'Alpha']>rTh3$alpha,]
theme3_0 <- theme3[,-c(1,8,13)] #ecarter les items aberrants
#Deuxiéme itération
rTh3 <- reliability(cov(theme3_0[,c("Item27","Item28","Item29","Item31",
                                    "Item32","Item33","Item35","Item36","Item37","Item38",
                                    "Item42","Item43","Item44")], use="complete.obs"))


rTh3$alpha
rTh3$rel.matrix[rTh3$rel.matrix[,'Alpha']>rTh3$alpha,]
#plus aucun item à écarter 

#vérification graphique
summary(PCA(theme3_0, scale.unit = TRUE, ncp = 5, graph = TRUE))
#Determination des facteurs
PC <- princomp(~Item27+Item28+Item29+Item31+Item32+Item33+Item36+Item37+Item38+Item42+Item43+Item44,
               cor=TRUE, data=theme3_0)
#permet de calculer la contribution 
#on a 3 factors
summary(PC)
plot(PC)

FA <- factanal(~Item27+Item28+Item29+Item31+Item32+Item33+Item35+Item36+Item37+Item38+Item42+Item43+Item44,
               factors=3, rotation="varimax", scores="regression", data=theme3_0)
print(FA)
#on regarde quel item est lie a F1 et quel item est en relation avec F2, de même pour F3
#item 27 lie au facteur1
#item 28 lie au facteur1
#item 29 lie au facteur1
#item 31 lie au facteru3
#item 32 lie au facteur3
#item 33 lie au facteur3
#item 35 lie au facteur3
#item 36 lie au facteur1
#item 37 lie au facteur1
#item 38 lie au facteru1
#item 42 lie au facteur2
#item 43 lie au facteru2
#item 44 lie au facteur2
saveNames[27:29] #F1
saveNames[36:38] #F1
saveNames[42:44] #F2

#le facteur 1 est principal chez les etudiants de genie info 0.232
#Item27 en relation avec facteur 1 avec un degre de 78%

#F1: les softs skills et l'autoformation sont importants pour intégrer le marché de travail
#F2: Les obstacles prévoyés, La compétitivité des alumnis,Le manque des compétences techniques,
#La punerie en termes de postes vacants

# Ajouter les scores au data.frame theme3_0
theme3_0 <<- within(theme3_0, {
  F3 <- FA$scores[,3]
  F2 <- FA$scores[,2]
  F1 <- FA$scores[,1]
})

View(theme3_0)


#etape 4 traitement des données:

#4.1: exploration numerique

for (i in 1:ncol(dataItems)) {
  dataItems[,i]=as.factor(dataItems[,i])
}
summary(dataItems)

#4.2: exploration graphique

for (i in 1:ncol(dataItems)) {
  dataItems[,i]=as.numeric(dataItems[,i])
}

for (i in names(dataItems)) {
  hist(dataItems[,i], 
       main= i, 
       xlab="Effectif", 
       border="white", 
       col="blue",
       xlim=c(0,5),
       las=1, 
       breaks=4)
}

#test d'hypothése item par item pour les themes 1 & 2 :


#theme1
theme1_0 = theme1
j=1
for (i in names(theme1)) {
  print("------------------------------------------")
  print(i)
  print(chisq.test(table(theme1[,i])))
  if(chisq.test(table(theme1[,i]))[3]>0.05){
    print("on ne peut pas valider")
    theme1_0=theme1_0[,-j]
  }else{
    print("on peut valider")
  }
  j <- j+1
}

for (i in names(theme1_0)) {  #Hist des items validés
  hist(theme1_0[,i], 
       main= i, 
       xlab="Effectif", 
       border="white", 
       col="blue",
       xlim=c(0,5),
       las=1, 
       breaks=4)
}


#les items mal posés (les neutres dominants) où les réponses aléatoires.

#theme 2
j=1
theme2_0 = theme2
for (i in names(theme2)) {
  print("------------------------------------------")
  print(i)
  print(chisq.test(table(theme2[,i])))
  if(chisq.test(table(theme2[,i]))[3]>0.05){
    print("on ne peut pas valider")
    theme2_0=theme2_0[,-j]
  }else{
    print("on peut valider")
  }
  j <- j+1
}

for (i in names(theme2_0)) {  #Hist des items validés
  hist(theme2_0[,i], 
       main= i, 
       xlab="Effectif", 
       border="white", 
       col="blue",
       xlim=c(0,5),
       las=1, 
       breaks=4)
}

#Item17, Item18; Item20 
saveNames[17] #Le.confinement.a.positivement.impacté.votre.cursus..
saveNames[18] #Le.COVID.était.une.occasion.pour.se.concentrer.sur.les.études.et.les.softskills..
saveNames[20] #Le.confinement.a.donné.l.opportunité.de.chercher.des.formations.en.ligne.et.à.participer.à.des.projets..

#Même remarque celle des réponses contradictoires

#Pour theme 3 :
#on teste la normalite des facteurs

#test de normalite de F1 , F2 et F3
#test items validés avec item39 

shapiro.test(theme3_0$F1)
if(shapiro.test(theme3_0$F1)[2]>0.05){
  print("F1 suit la loi normale")
  print(t.test(theme3_0$F1~data$Item39)) #p-value = 0.1568 > Il n'y a pas de diff => ils ne s'expliquent pas
}else{
  print("F1 ne suit pas la loi normale")
  print(wilcox.test(theme3_0$F1~data$Item39))
  print(t.test(theme3_0$F1~data$Item39))
}

shapiro.test(theme3_0$F2)
if(shapiro.test(theme3_0$F2)[2]>0.05){
  print("F2 suit la loi normale")
  print(t.test(theme3_0$F2~data$Item39))
}else{
  print("F2 ne suit pas la loi normale")
  print(wilcox.test(theme3_0$F2~data$Item39)) #p-value = 0.001143
  print(t.test(theme3_0$F2~data$Item39))      #p-value = 0.0006824
  #difference des p-value => valeur test non parametric
  #il y a diff
  #F2 explique l'Item39
}

saveNames[42]
saveNames[43]
saveNames[44]

shapiro.test(theme3_0$F3)
if(shapiro.test(theme3_0$F3)[2]>0.05){
  print("F3 suit la loi normale")
  print(t.test(theme3_0$F3~data$Item39))
}else{
  print("F3 ne suit pas la loi normale")
  print(wilcox.test(theme3_0$F3~data$Item39)) #p-value = 0.582
  print(t.test(theme3_0$F3~data$Item39)) #p-value = 0.8215
  # p-value >5% Il n'y a pas de diff => ils ne s'expliquent pas
}

saveNames[31]

#histogramme item39
hist(data$Item39)
saveNames[39]

#creation d'un nouveau dataframe contenant les items valides et les scores
newData <- theme1_0
newData <-cbind.data.frame(newData,theme2_0)
newData <-cbind(newData,theme3_0)
View(newData)


#création de deux dataframe un contient les individus qui disent oui et l'autre le contraire
dataNon <- data[data$Item39==1,]
dataOui <- data[data$Item39==2,]
View(dataNon)
View(dataOui)


#les variables quanti:
# F1
# F2
# F3
#age

#test de quasi normalite

#F2
skewness(theme3_0$F2) #-1.440498
kurtosis(theme3_0$F2) #5.51032

#F3
skewness(theme3_0$F3) #-1.648377
kurtosis(theme3_0$F3) #6.768986

#on a la quasi-normalité de l'age
# F1 normale
# F2 ni normale ni quasi 
# F3 ni normale ni quasi


#Analysez une variable quantitative et une qualitative par ANOVA

#Anova car F1 suit la loi normale 
#F1 non lié à la perception mais reste le factor principal 
#Quantification des qualis

#newData contenant les items validés
for (i in 1:20) {
  print("-----------------------------------------")
  print(names(newData)[i])
  fit <- aov(newData$F1~newData[,i]) 
  print(summary(fit))
}

#les items significatifs sont items14 items17 items19 
saveNames[14]
saveNames[17]
saveNames[19]

#conclusion: la formation académique n'est pas suffisante pour intégrer le marché de travail 
#l'étudiant doit s'autoformer(le confinement était une bonne période) et améliorer ses soft-skills,
#sans oublier que les profs jouent un rôle important concernant la formation 

#Certes il n'explique pas la perception mais la solution


#F2 ne suit pas la loi normale

for (i in 1:20) {
  if(kruskal.test(newData$F2~newData[,i])[3]<0.05)
  {
    print("-----------------------------------------")
    print(names(newData)[i])
    print(kruskal.test(newData$F2~newData[,i])[3])
  }
}

#les items qui s'est montré qu'ils ont une difference par rapport au F2

saveNames[4]
saveNames[10]
saveNames[14]
saveNames[24]

#conclusion : Le fait que F2 est significative /L'Item39, est que l'ensemble des items 4,10,14,24
#sign/ au F2, ses Items alors sont sign / Item39
#le diplome est non orienté et manque les frameworks, et que le marché de travail 
#témoigne un manque de compétences et d'évolution.

#la formation de l'ensa seule et la difficulté d'integrer 

mean(data$Item25) #3.214286 deg satisf, on peut rien conclure

summary.factor(dataOui$Item25)
barplot(table(dataOui$Item25))

summary.factor(dataNon$Item25)
barplot(table(dataNon$Item25))

chisq.test(data$Item25,data$Item39, simulate.p.value = TRUE) 

#Formation moyennement satisfaisante et ne justifie pas la perception il n y a pas d'assoc p-value > 0.05

#voir s'il y a une différence entre les gens qui pensent trouver des difficultés pour intégrer le marché de 
#travail et les autres

summary.factor(dataOui$Item14)
barplot(table(dataOui$Item14))

summary.factor(dataOui$Item17)
barplot(table(dataOui$Item17))

summary.factor(dataOui$Item19)
barplot(table(dataOui$Item19))

summary.factor(dataOui$Item10)
barplot(table(dataOui$Item10))

summary.factor(dataOui$Item4)
barplot(table(dataOui$Item4))

summary.factor(dataOui$Item24)
barplot(table(dataOui$Item24))

summary.factor(dataNon$Item14)
barplot(table(dataNon$Item14))

summary.factor(dataNon$Item17)
barplot(table(dataNon$Item17))

summary.factor(dataNon$Item19)
barplot(table(dataNon$Item19))

summary.factor(dataNon$Item10)
barplot(table(dataNon$Item10))

summary.factor(dataNon$Item4)
barplot(table(dataNon$Item4))

summary.factor(dataNon$Item24)
barplot(table(dataNon$Item24))

#item14, Item10, Item4 n'est pas validé car les neutres dominent
#Items17, Items19, Item24 sont valides

saveNames[17]
saveNames[19]
saveNames[24]

#Or, item17 et item19, appartient au test de F1 est tout les items
#et sachant que le F1 ne diffère pas de l'item de la perception
#seule l'item24 qui reste et ce ci confirmé par les test chisq ci dessous

chisq.test(data$Item17,data$Item39, simulate.p.value = F) #p-value > 5% pas rejet H0
chisq.test(data$Item19,data$Item39, simulate.p.value = T) #p-value > 5% pas rejet H0
chisq.test(data$Item24,data$Item39, simulate.p.value = T) #p-value < 5% rejet H0

#conclusion ; le diplome non orienté, fait penser que c'est difficile d'integrer le marché de travail

saveNames[40]

summary.factor(data$Item40)
barplot(table(data$Item40))
mean(data$Item40)

#l'integration dans le marché de travail est moyennement difficile

#les mesures de concentration

d = data$Age
lorenz = cumsum(sort(d)) / sum(d)
lorenz = c(0,lorenz) # La courbe de Lorenz commence à 0

n = length(lorenz)
lorenz_df = data.frame(x=((0:(n-1))/n), y=lorenz)

ggplot(lorenz_df, aes(x,y)) + geom_point(shape=1)

n = length(lorenz)
aire_ss_courbe = sum(lorenz[1:(n-1)])/n # aire sous la courbe de Lorenz. La dernière valeur ne participe pas à l'aire, d'où "[:-1]"
S = 0.5 - aire_ss_courbe # aire entre la 1e bissectrice et la courbe de Lorenz
gini = 2*S
gini

#on a gini est faible ce qui veut dire qu'on a une distribution d'age presque égalitaire
# cad si on prend 10% de la population cette dernière a 10% d'age de léchantillon


#regression

#The main difference between Regression and Classification algorithms that Regression algorithms are used to predict the continuous values such as price, salary, age, etc. and 
#Classification algorithms are used to predict/Classify the discrete values such as Male or Female, True or False, Spam or Not Spam, etc.

#A.1: la regression simple

plot(newData$F1,data$Age)

#A.1.1: test de correlation:

cor.test(newData$F1,data$Age, method = "pearson")
#p_value = 0.8518>5% => il n' y a pas une correlation significative entre F1 et l'age
#pas de correlation 
#pas de regression lineaire :(


#A.2: regressin logistique (Classification) LogoddsY = αi+ β1X1 +β2X2 +….. +βnXn
#le fait qu'on a pas, des var quanti pour faire une regression linéaire, on cherche un modèle 
#statistique à travers la classification et l'une de ses techniques, comme la regression logistique.

myData = data

for (i in 1:42) {
  if (myData[i,42]=="1")
  {
    myData[i,42]<-0
  }
  else{
    if (myData[i,42]=="2")
      {
       myData[i,42]<-1
    }
  }
}

View(myData)
myData <<- within(myData, {
  F3 <- FA$scores[,3]
  F2 <- FA$scores[,2]
  F1 <- FA$scores[,1]
})

myData = myData[,-c(1,2,6,7)]

summary(myData)

nuage1 = ggplot(myData, aes(F2, Item39))+ geom_point()
nuage1 

saveNames[24]#diplome 
nuage2 = ggplot(myData, aes(Item24, Item39))+ geom_point()
nuage2

saveNames[25] #echelle formation
nuage3 = ggplot(myData, aes(Item25, Item39))+ geom_point()
nuage3

# estimer et évaluer le modèle

model0 = glm(Item39~1, data = myData, family = binomial())
summary(model0)

model1 = glm(Item39 ~ Item24+Item25+F2,
             family = binomial(), data = myData)
summary(model1)

# tester l'hypothèse de linéarité 

prob = predict(model1, type = "response")
predictors = colnames(myData[])
mydata2 = myData %>% mutate(logit = log(prob/(1- prob))) %>% 
  tidyr::gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata2, aes(logit, predictor.value)) +
  geom_point(size = 0.5, alpha = 0.5)+ geom_smooth(method = "loess")+
  facet_wrap(~ predictors, scales = "free_y")

# la multicollinéarité

vif(model1)

# est-ce que le modèle est significatif?

modelChi = model1$null.deviance - model1$deviance
modeldl = model1$df.null - model1$df.residual
modelp = 1 - pchisq(modelChi, modeldl)
modelChi ; modeldl ; modelp

# inspecter l'influence des variables indépendantes

exp(model1$coefficients)
exp(Confint(model1))

# évaluer la qualité du modèle (source : Discovering statistics using R, auteur : Andy field)

logisticPseudoR2s <- function(LogModel){
  dev <- LogModel$deviance
  nullDev <- LogModel$null.deviance
  modelN <- length(LogModel$fitted.values)
  R.l <- 1 - dev/ nullDev
  R.cs <- 1 - exp(-(nullDev - dev) /modelN)
  R.n <- R.cs / (1 - (exp(-(nullDev/modelN))))
  cat("R^2 pour la régression logistique\n")
  cat("Hosmer et Lemeshow R^2 ", round(R.l, 3), "\n")
  cat("Cox et Snell R^2       ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2          ", round(R.n, 3), "\n")
}
logisticPseudoR2s(model1)

# diagnostic des résidus et des valeurs aberrantes

myData$residus = rstandard(model1) 
myData$large_resid = myData$residus > 2 | myData$residus < -2 # résidus larges

myData$large_resid #tous les résidus sans FALSE donc pas de problème à prevoir

myData$leverage = hatvalues(model1) # seuil = 3*(k+1)/n = 0.4 #Si jamais il y a un false, on vérifie si jamais il y a un laverage large, sinon c'est pas très grave

hist(myData$residus) # normalité #Ce ne suit pas de loi normale.




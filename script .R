library(readxl)
library(dplyr)
library(anocva)
library(Rcmdr)
library(moments)
#CONCEPTTION : 
#POPULATION : Laureat des ENSA Agadir, Kenitra, Oujda, Tanger, ENSIAS RABAT
#Etudiant Informatique Universite IBN Tofail
#Echantillon : Réduction ENSIAS Rabat et ENSAK Kenitra pour la representativite promotion 2018-2020
#Problematique : Les laureats rencontrent des difficultes au cours de leur integration dans le marche de travail
#Variable a expliquer: Integration marche du travail/Situation du Laureat
#Variables explicatives: Le profil du laureat, le monde de l'emploi.
#Notre entite des difficultes servira de variable mediatrice avec ces parametres
#Hypotheses:
#La crise de COVID-19 a impacte le secteur Informatique au Maroc
#Les laureats ont du mal a s'adapter au ryhtme de travail du Monde professionnel
#Les competences techniques sont fortement demandees par les recruteurs
#L'adaption aux nouvelles technologies et l'autoformation facilitent l'integration dans le Marche du travail
#Il y'a une forte demande des profils specialistes.
#Connaitre le secteur d'activite est obligatoire pour le recrutement
#Le stage du PFE du laureat est une grande opportunite d'obtention d'un emploi
#Il y'a plus d'Hommes que de Femmes dans le secteur Informatique au Maroc
#Les profils d'ingenieurs sont de plus en plus jeunes
#La formation recue est suffisante et adequate aux realites du marche du travail
#La fourchette salariale est satisfaisante au Maroc....

#IMPORTATION DES DONNEES
data = read_excel(file.choose())
View(data)

#PRETRAITEMENT
summary(data)#Reduction ENSIAS Rabat et ENSAK Kenitra pour la representativite promotion 2018-2020
#CONVERSION ET NETTOYAGE
data$Nationalite = as.factor(data$Nationalite)
data$Genre = as.factor(data$Genre)
data$Ecole = as.factor(data$Ecole)
data$Filiere = as.factor(data$Filiere)
data$certification = as.factor(data$certification)
data$stageParascolaire = as.factor(data$stageParascolaire)
data$difficulteRecherchePFE = as.ordered(data$difficulteRecherchePFE)
data$valeurAjouteeEntrPFE = as.ordered(data$valeurAjouteeEntrPFE)
data$encadrementPFE = as.ordered(data$encadrementPFE)
data$satisfactionChoixPFE = as.factor(data$satisfactionChoixPFE)
data$offreEmploiPFE = as.factor(data$offreEmploiPFE)
data$ situation = as.factor(data$ situation)
data$accesOpportuniteEmploi = as.ordered(data$accesOpportuniteEmploi)
data$secteurTravail = as.factor(data$secteurTravail)
data$modeTravail = as.factor(data$modeTravail)
data$avisFreelance = as.ordered(data$avisFreelance)
data$impactCOVID = as.ordered(data$impactCOVID)
data$apportFormationInsertion = as.ordered(data$apportFormationInsertion)
data$memedomaineFormEmp = as.factor(data$memedomaineFormEmp)
data$adaptationDifficile = as.ordered(data$adaptationDifficile)
data$culturedelentreObligatoire = as.ordered(data$culturedelentreObligatoire)
data$adoptionDigital = as.ordered(data$adoptionDigital)
data$moreTechnical_thanHumanDemand = as.ordered(data$moreTechnical_thanHumanDemand)
data$moreSpecial_thanNoviceDemand = as.ordered(data$moreSpecial_thanNoviceDemand)
data$demandeNouvellesTechno = as.ordered(data$demandeNouvellesTechno)
data$satisfactionSalaire = as.ordered(data$satisfactionSalaire)

#Analyse Univariee et Exploration des données
summary(data)
show = function(Data){
	par(mfrow = c(2,4))
	plot(Data$Nationalite, main="Nationalite", col="red")
	plot(Data$Genre, main="Genre", col="blue")
	hist(Data$Age, main="Age", col="green")
	plot(Data$Ecole, main="Ecole", col="orange")
	plot(Data$Filiere, main="Filiere", col="orange")
	hist(Data$anneeDiplome, main="annee Diplome", col="green")
	plot(Data$certification, main="Certification", col="blue")
	plot(Data$stageParascolaire, main="Stage Parascolaire", col="red")
	plot(Data$difficulteRecherchePFE, main="difficulteRecherchePFE")
	plot(Data$valeurAjouteeEntrPFE, main="valeurAjouteeEntrPFE")
	plot(Data$encadrementPFE, main="encadrementPFE")
	plot(Data$satisfactionChoixPFE, main="satisfactionChoixPFE")
	plot(Data$offreEmploiPFE, main="offreEmploiPFE")
	plot(Data$situation, main="situation")
	plot(Data$accesOpportuniteEmploi, main="accesOpportuniteEmploi")
	plot(Data$secteurTravail, main="secteurTravail")
	plot(Data$modeTravail, main="modeTravail")
	plot(Data$avisFreelance, main="avisFreelance")
	plot(Data$impactCOVID, main="impactCOVID")
	plot(Data$apportFormationInsertion, main="apportFormationInsertion")
	plot(Data$memedomaineFormEmp, main="memedomaineFormEmp")
	plot(Data$adaptationDifficile, main="adaptationDifficile")
	plot(Data$culturedelentreObligatoire, main="culturedelentreObligatoire")
	plot(Data$adoptionDigital, main="adoptionDigital")
	plot(Data$moreTechnical_thanHumanDemand, main="moreTechnical_thanHumanDemand")
	plot(Data$moreSpecial_thanNoviceDemand, main="moreSpecial_thanNoviceDemand")
	plot(Data$demandeNouvellesTechno, main="demandeNouvellesTechno")
	plot(Data$satisfactionSalaire, main="satisfactionSalaire")
	hist(data$grilleSalaire, main="grilleSalaire")
}

show(data)
levels(data$Ecole)

#Regrouper les autres ecoles : STRADE ECOLES
levels(data$Ecole) = c('Autres_Ecoles', 'ENSA Kenitra', 'Autres_Ecoles', 'Autres_Ecoles', 'ENSIAS Rabat', 'Autres_Ecoles')
summary(data$Ecole)

#Effectif de 25 <30 Donc pas représentatif
#Exclusion des autres Ecoles
newData = data %>%
filter(Ecole != "Autres_Ecoles")
summary(newData$Ecole)
	#Pour avoir 2 categories a effectif representatif
	levels(newData$Ecole) = c("ENSA Kenitra","ENSA Kenitra", "ENSIAS Rabat")

#Exploration des nouvelles donnee
summary(newData)

show(newData)
View(newData)
sqrt(var(data$Age))
sqrt(var(newData$grilleSalaire))
sqrt(var(data$grilleSalaire))
sqrt(var(newData$grilleSalaire))
#Impact de l'exclusion
par(mfrow = c(1,2))
hist(data$Age); hist(newData$Age)

#NETTOYAGE DES DONNEES
boxplot(data$Age, main="data Age Outliers", col="blue")$out; boxplot(newData$Age, main="newData Age Outliers", col="orange")$out
boxplot(data$anneeDiplome, main="data anneeDiplome Outliers", col="blue")$out; boxplot(newData$anneeDiplome, main="newData anneeDiplome Outliers", col="orange")$out
boxplot(data$grilleSalaire, main="data grilleSalaire Outliers", col="blue")$out; boxplot(newData$grilleSalaire, main="newData grilleSalaire Outliers", col="orange")$out


#ANALYSE DES DONNEES
#TEST DE NORMALITE DES VARIABLES QUANTITATIVES
par(mfrow = c(1,1))
hist(newData$Age)
shapiro.test(newData$Age);
#age ne suit pas la loi normal
#testons la quasi-normalite
kurtosis(newData$Age)
skewness(newData$Age)
#l'age ne suit pas la quasi-normlite

#test de normalite anneeDiplome
hist(newData$anneeDiplome)
shapiro.test(newData$anneeDiplome);
#anneeDiplome ne suit pas la loi normale
#testons la quasi-normalite
kurtosis(newData$anneeDiplome)
skewness(newData$anneeDiplome)
#anneeDiplome suit  la quasi normalite

#test de normalite grilleSalaire
hist(newData$grilleSalaire)
shapiro.test(newData$grilleSalaire)
# la grilleSalaire ne suit pas la loi normale
kurtosis(newData$grilleSalaire)
skewness(newData$grilleSalaire)
#la quasi-normalite-Oui

#================================================================================================
#ASSOCIATION ENTRE LES ATTRIBUTS DE L'ETUDIANT (VALIDATION DU MODELE)
#Testons les associations entre le statut de laureat et ses differents attributs
table(newData$situation,newData$Nationalite)
plot(newData$Nationalite, newData$situation, main="Nationalite Situation")
chisq.test(newData$situation,newData$Nationalite)
#p-value<0.05 il  y'a une association significative entre la siuation et la nationalite

table(newData$situation, newData$certification)
plot(newData$certification, newData$situation, main="situation certification")
chisq.test(newData$situation, newData$certification)
#p-value>0.05 il  n'y a pas une association sighificative entre la situation et certification

table(newData$Age,newData$situation)
plot(newData$situation, newData$Age, main="situation Age")
wilcox.test(newData$Age, newData$situation)
#p-value>0.05 il  n'y a pas une association significative entre la situation et l'age

table(newData$situation, newData$Ecole)
plot(newData$Ecole, newData$situation,  main="Situation Ecole")
chisq.test(newData$situation, newData$Ecole)
#p-value<0.05 il  y'a pas une association significative entre la situation et ecole

table(newData$anneeDiplome, newData$Ecole)
plot(newData$Ecole, newData$anneeDiplome, main="Ecole anneeDiplome")
t.test(newData$newData$anneeDiplome, newData$Ecole)
#p-value<0.05 il  y'a  une association significative entre l'age et et l'ecole

table(newData$situation, newData$Filiere)
plot(newData$situation, newData$Filiere, main="situation Filiere")
chisq.test(newData$situation, newData$Filiere)
#p-value<0.05 il  y'a une association significatif entre la situation et la filliere

table(newData$situation, newData$anneeDiplome)
plot(newData$situation, newData$anneeDiplome, main="situation anneeDiplome")
chisq.test(newData$anneeDiplome, newData$situation)
#p-value<0.05 il  y'a  une relation significative entre la situation et anneeDiplome

table(newData$Genre, newData$situation)
plot(newData$Genre, newData$situation, main="situation genre")
chisq.test(newData$Genre, newData$situation)
#p-value>0.05 il  n'a pas une relation sighificative

#Testons les associations entre la nationalite de laureat et ses differentes #attribut
table(newData$Nationalite,newData$situation)
plot(newData$Nationalite,newData$situation, main="Nationalite Situation")
chisq.test(newData$Nationalite,newData$situation)
#p-value<0.05 il  y'a  une relation significative entre la siuation et la nationalité

table(newData$Nationalite,newData$Genre)
plot(newData$Nationalite,newData$Genre, main="Nationalite et Genre")
chisq.test(newData$Nationalite,newData$Genre)
#p-value>0.05 il  n' y a pas une relation significative entre Nationalite et Genre

table(newData$Nationalite,newData$Age)
plot(newData$Nationalite,newData$Age, main="Nationalite Age")
t.test(newData$Age~newData$Nationalite)
#p-value>0.5%il n'y a pas une association entre l'age et la nationalité

table(newData$Nationalite, newData$Ecole)
plot(newData$Nationalite, newData$Ecole, main="Nationalite et Ecole")
chisq.test(newData$Nationalite, newData$Ecole)
#p-value>0.05 il n' y pas   une relation significative entre Nationalite et ecole
table(newData$Nationalite, newData$Filiere)
plot(newData$Nationalite, newData$Filiere, main="Nationalite Filliere")
chisq.test(newData$Nationalite, newData$Filiere)
#p-value>0.05 il n' y pas une relation significative entre la Nationalite et la filliere
table(newData$Nationalite, newData$anneeDiplome)
plot(newData$Nationalite, newData$anneeDiplome, main="Nationalite anneeDiplome")
t.test(newData$anneeDiplome~newData$Nationalite)
#p-value>0.05 il n' y a pas une relation significative entre la Nationalite et anneeDiplome
table(newData$Nationalite, newData$situation)
plot(newData$Nationalite, newData$situation)
chisq.test(newData$Nationalite, newData$situation)
#p-value<0.05 il y' a  une une association significative entre la Nationalite et situation
table(newData$Nationalite, newData$stageParascolaire)
plot(newData$Nationalite, newData$stageParascolaire, main="Nationalite stageParascolaire")
chisq.test(newData$Nationalite, newData$stageParascolaire)
#p-value>0.05 il n' y pas une association significative entre la Nationalite et stageParascolaire

#================================================================================================
#ASSOCIATION ENTRE LES ATTRIBUTS DU MARCHE DU TRAVAIL(VALIDATION DU MODELE)
chisq.test(newData$accesOpportuniteEmploi,newData$secteurTravail)
chisq.test(newData$accesOpportuniteEmploi,newData$modeTravail)
chisq.test(newData$accesOpportuniteEmploi,newData$avisFreelance)
chisq.test(newData$accesOpportuniteEmploi,newData$impactCOVID)
chisq.test(newData$accesOpportuniteEmploi,newData$apportFormationInsertion)
chisq.test(newData$accesOpportuniteEmploi,newData$memedomaineFormEmp)
chisq.test(newData$accesOpportuniteEmploi,newData$adaptationDifficile)
chisq.test(newData$accesOpportuniteEmploi,newData$culturedelentreObligatoire)
chisq.test(newData$accesOpportuniteEmploi,newData$adoptionDigital)
chisq.test(newData$accesOpportuniteEmploi,newData$moreTechnical_thanHumanDemand)
chisq.test(newData$accesOpportuniteEmploi,newData$moreSpecial_thanNoviceDemand)
chisq.test(newData$accesOpportuniteEmploi,newData$demandeNouvellesTechno)
chisq.test(newData$accesOpportuniteEmploi,newData$satisfactionSalaire)
#opportunite competencestechniquesHumaines - adapatation difficile - satisfaction Salaire

chisq.test(newData$secteurTravail,newData$modeTravail)
chisq.test(newData$secteurTravail,newData$avisFreelance)
chisq.test(newData$secteurTravail,newData$impactCOVID)
chisq.test(newData$secteurTravail,newData$apportFormationInsertion)
chisq.test(newData$secteurTravail,newData$memedomaineFormEmp)
chisq.test(newData$secteurTravail,newData$adaptationDifficile)
chisq.test(newData$secteurTravail,newData$culturedelentreObligatoire)
chisq.test(newData$secteurTravail,newData$adoptionDigital)
chisq.test(newData$secteurTravail,newData$moreTechnical_thanHumanDemand)
chisq.test(newData$secteurTravail,newData$moreSpecial_thanNoviceDemand)
chisq.test(newData$secteurTravail,newData$demandeNouvellesTechno)
chisq.test(newData$secteurTravail,newData$satisfactionSalaire)
#secteur et satisfaction
#secteur demandeTech_Humaine/Specialist_Novice/AdoptionDigital
#Le secteur Prive l'emporte

chisq.test(newData$modeTravail,newData$avisFreelance)
chisq.test(newData$modeTravail,newData$impactCOVID)
chisq.test(newData$modeTravail,newData$apportFormationInsertion)
chisq.test(newData$modeTravail,newData$memedomaineFormEmp)
chisq.test(newData$modeTravail,newData$adaptationDifficile)
chisq.test(newData$modeTravail,newData$culturedelentreObligatoire)
chisq.test(newData$modeTravail,newData$adoptionDigital)
chisq.test(newData$modeTravail,newData$moreTechnical_thanHumanDemand)
chisq.test(newData$modeTravail,newData$moreSpecial_thanNoviceDemand)
chisq.test(newData$modeTravail,newData$demandeNouvellesTechno)
chisq.test(newData$modeTravail,newData$satisfactionSalaire)
#Mode de travail avis_Freelance
#Mode de travail et memedomaine_de_formation
#Mode de travail et avisFormation

chisq.test(newData$avisFreelance,newData$impactCOVID)
chisq.test(newData$avisFreelance,newData$apportFormationInsertion)
chisq.test(newData$avisFreelance,newData$memedomaineFormEmp)
chisq.test(newData$avisFreelance,newData$adaptationDifficile)
chisq.test(newData$avisFreelance,newData$culturedelentreObligatoire)
chisq.test(newData$avisFreelance,newData$adoptionDigital)
chisq.test(newData$avisFreelance,newData$moreTechnical_thanHumanDemand)
chisq.test(newData$avisFreelance,newData$moreSpecial_thanNoviceDemand)
chisq.test(newData$avisFreelance,newData$demandeNouvellesTechno)
chisq.test(newData$avisFreelance,newData$satisfactionSalaire)
#Avis Freelance et adaptation difficile
#Avis Freelance et competence techniques

chisq.test(newData$impactCOVID,newData$apportFormationInsertion)
chisq.test(newData$impactCOVID,newData$memedomaineFormEmp)
chisq.test(newData$impactCOVID,newData$adaptationDifficile)
chisq.test(newData$impactCOVID,newData$culturedelentreObligatoire)
chisq.test(newData$impactCOVID,newData$adoptionDigital)
chisq.test(newData$impactCOVID,newData$moreTechnical_thanHumanDemand)
chisq.test(newData$impactCOVID,newData$moreSpecial_thanNoviceDemand)
chisq.test(newData$impactCOVID,newData$demandeNouvellesTechno)
chisq.test(newData$impactCOVID,newData$satisfactionSalaire)
#Rien de notable

chisq.test(newData$apportFormationInsertion,newData$memedomaineFormEmp)
chisq.test(newData$apportFormationInsertion,newData$adaptationDifficile)
chisq.test(newData$apportFormationInsertion,newData$culturedelentreObligatoire)
chisq.test(newData$apportFormationInsertion,newData$adoptionDigital)
chisq.test(newData$apportFormationInsertion,newData$moreTechnical_thanHumanDemand)
chisq.test(newData$apportFormationInsertion,newData$moreSpecial_thanNoviceDemand)
chisq.test(newData$apportFormationInsertion,newData$demandeNouvellesTechno)
chisq.test(newData$apportFormationInsertion,newData$satisfactionSalaire)
#Apport formation - satisfaction salaire
#Apport formation et competence technique

chisq.test(newData$memedomaineFormEmp,newData$adaptationDifficile)
chisq.test(newData$memedomaineFormEmp,newData$culturedelentreObligatoire)
chisq.test(newData$memedomaineFormEmp,newData$adoptionDigital)
chisq.test(newData$memedomaineFormEmp,newData$moreTechnical_thanHumanDemand)
chisq.test(newData$memedomaineFormEmp,newData$moreSpecial_thanNoviceDemand)
chisq.test(newData$memedomaineFormEmp,newData$demandeNouvellesTechno)
chisq.test(newData$memedomaineFormEmp,newData$satisfactionSalaire)
#Association entre la convergence formation emploie et les competences techniques

chisq.test(newData$adaptationDifficile,newData$culturedelentreObligatoire)
chisq.test(newData$adaptationDifficile,newData$adoptionDigital)
chisq.test(newData$adaptationDifficile,newData$moreTechnical_thanHumanDemand)
chisq.test(newData$adaptationDifficile,newData$moreSpecial_thanNoviceDemand)
chisq.test(newData$adaptationDifficile,newData$demandeNouvellesTechno)
chisq.test(newData$adaptationDifficile,newData$satisfactionSalaire)
#Association demande technique et difficulte d'adapation

chisq.test(newData$culturedelentreObligatoire,newData$adoptionDigital)
chisq.test(newData$culturedelentreObligatoire,newData$moreTechnical_thanHumanDemand)
chisq.test(newData$culturedelentreObligatoire,newData$moreSpecial_thanNoviceDemand)
chisq.test(newData$culturedelentreObligatoire,newData$demandeNouvellesTechno)
chisq.test(newData$culturedelentreObligatoire,newData$satisfactionSalaire)
#Association entre connaissance de la culture de l'entreprise et satisfactionSalaire

chisq.test(newData$moreTechnical_thanHumanDemand,newData$moreSpecial_thanNoviceDemand)
chisq.test(newData$moreTechnical_thanHumanDemand,newData$demandeNouvellesTechno)
chisq.test(newData$moreTechnical_thanHumanDemand,newData$satisfactionSalaire)
#Pas d'association significative

chisq.test(newData$moreSpecial_thanNoviceDemand,newData$demandeNouvellesTechno)
chisq.test(newData$moreSpecial_thanNoviceDemand,newData$satisfactionSalaire)
#Pas d'association notable

chisq.test(newData$demandeNouvellesTechno,newData$satisfactionSalaire)
#Il y'a une assoc.. entre la statisfaction du salaire et la maitrise des nouvelles technologies 

#VISUALISATION ET EXPLORATION ET EMISSION D'HYPOTHESES
plot(newData$Genre, newData$adaptationDifficile, main="Genre et Adaptation")
table(newData$Nationalite, newData$Genre)
plot(newData$Nationalite, newData$Genre, main="Nationalite et Genre")
table(newData$certification, newData$adaptationDifficile)
plot(newData$certification, newData$adaptationDifficile, main="Certification et Adaptation")
par(mfrow = c(1,2))
table(newData$certification, newData$moreSpecial_thanNoviceDemand)
plot(newData$certification, newData$moreSpecial_thanNoviceDemand, main="Certification et demande specialiste")
table(newData$certification, newData$moreTechnical_thanHumanDemand)
plot(newData$certification, newData$moreTechnical_thanHumanDemand, main="Certification et demande humaine")
table(newData$certification, newData$apportFormationInsertion)
plot(newData$certification, newData$apportFormationInsertion, main="Certification et qualite Formation")
table(newData$certification, newData$situation)
plot(newData$certification, newData$situation, main="Certification et Situation")

#Test graphique de la tendance
table(newData$Genre, newData$situation)
plot(newData$situation, newData$Genre, main="Genre et Situation")
table(newData$Genre, newData$grilleSalaire)
plot(newData$Genre, newData$grilleSalaire, main="Genre et Salaire")
table(newData$Genre, newData$satisfactionSalaire)
plot(newData$satisfactionSalaire, newData$Genre, main="Satisfaction Salaire et genre")
table(newData$Nationalite, newData$satisfactionSalaire)
plot(newData$Nationalite, newData$satisfactionSalaire, main="Nationalite et satisfactionSalaire")
table(newData$offreEmploiPFE, newData$valeurAjouteeEntrPFE)
plot(newData$offreEmploiPFE, newData$valeurAjouteeEntrPFE, main="offreEmploiPFE et VA")
table(newData$stageParascolaire, newData$adaptationDifficile)
plot(newData$stageParascolaire, newData$adaptationDifficile, main="Experience et Adaptation")


#Testons les associations entre le genre de laureat et ses differents attributs

table(newData$Genre, newData$Nationalite)
plot(newData$Genre, newData$Nationalite, main="Genre et Nationalite")
chisq.test(newData$Genre, newData$Nationalite)
#p-value>0.05 il  n'y a pas une relation sighificative

table(newData$Genre, newData$Age)
plot(newData$Genre, newData$Age, main="Genre et Age")
chisq.test(newData$Genre, newData$Age)
#p-value>0.05 il  n'y a pas une relation significative 

table(newData$Genre, newData$Ecole)
plot(newData$Genre, newData$Ecole, main="Genre et Ecole")
chisq.test(newData$Genre, newData$Ecole)
#p-value<0.05 il  n'y a  une relation significative 

table(newData$Genre, newData$Filiere)
plot(newData$Genre, newData$Filiere, main="Genre et Filiere")
chisq.test(newData$Genre, newData$Filiere)
#p-value>0.05 il  n'y a une relatin significatif 

table(newData$Genre, newData$certification)
plot(newData$Genre, newData$certification, main="Genre et certification")
chisq.test(newData$Genre, newData$certification)
#p-value>0.05 il  n'y a une relatin significatif

table(newData$Genre, newData$situation)
plot(newData$Genre, newData$situation, main="situation genre")
chisq.test(newData$Genre, newData$situation)
#p-value>0.05 il  n'y a pas une relation significative
table(newData$Genre, newData$stageParascolaire)
plot(newData$Genre, newData$stageParascolaire)
chisq.test(newData$Genre, newData$stageParascolaire)
#p-value>0.05 il  n'y a pas une relation significative
table(newData$Genre, newData$anneeDiplome)
plot(newData$Genre, newData$anneeDiplome)
t.test(newData$anneeDiplome~newData$Genre)
#p-value>0.05 il  n'y a pas une relation significative

par(mfrow = c(1,1))

#Les tests d'association entre le profil et le monde de l'emploi
summary(aov(newData$grilleSalaire~newData$Genre))
wilcox.test(newData$grilleSalaire~newData$Genre)
chisq.test(newData$grilleSalaire, newData$Genre)
kruskal.test(newData$grilleSalaire, newData$Genre)
t.test(newData$grilleSalaire~newData$Genre)
#Pas de discrimination au niveau des Genres 

summary(aov(newData$grilleSalaire~newData$certification))
t.test(newData$grilleSalaire~newData$certification)
#Les formation supplementaires n'influence presque surement pas le salaire
summary(aov(newData$grilleSalaire~newData$Age))
t.test(newData$grilleSalaire~newData$Age)
#Pas d'association salaire-Age
summary(aov(newData$grilleSalaire~newData$apportFormationInsertion))
plot(newData$apportFormationInsertion, newData$grilleSalaire, main='grilleSalaire et apportFormationInsertion')
#Il y'a une association entre la perception sur l'apport de la formation et le salaire
summary(aov(newData$grilleSalaire~newData$adoptionDigital))
plot(newData$adoptionDigital, newData$grilleSalaire, main='grilleSalaire et adoptionDigital')
#Il y'a une association entre l'adoption du digital et le salaire
#La preuve le salaire et les avis
summary(aov(newData$grilleSalaire~newData$stageParascolaire))
#Pas d'apport notable de l'experience sur le salaire
chisq.test(newData$accesOpportuniteEmploi, newData$avisFreelance)
chisq.test(newData$accesOpportuniteEmploi, newData$situation)
plot(newData$accesOpportuniteEmploi, newData$situation, main='accesOpportuniteEmploi et Situation')
#L'avis sur l'acces des opportunites d'emploi influence a un impact sur la situation
chisq.test(newData$Genre, newData$grilleSalaire)
plot(newData$Genre, newData$satisfactionSalaire, main="Satisfaction Salaire Global")
chisq.test(newData$offreEmploiPFE, newData$valeurAjouteeEntrPFE)
#Association entre la valeur ajoutee a l'entreprise lors du PFE et une eventuelle offre d'emploi
plot(newData$valeurAjouteeEntrPFE, newData$offreEmploiPFE, main="Impact sur l'apport en PFE et offre d'emploi")
summary(aov(newData$Age~newData$situation))
#Pas d'association entre l'age et la situation
plot(newData$situation, newData$Age, main="Tranche d'age et Situation")
#En moyenne, chacune des tranches d'age est touchee
t.test(newData$grilleSalaire~newData$Ecole)
wilcox.test(newData$grilleSalaire~newData$Ecole)
chisq.test(newData$grilleSalaire, newData$Ecole)
#Il n'y a pas une difference significative des salaires entre ces 2 ecoles
plot(newData$Ecole, newData$grilleSalaire, main="Salaire et Ecoles")
kruskal.test(newData$moreTechnical_thanHumanDemand, newData$Ecole)
kruskal.test(newData$satisfactionSalaire, newData$Ecole)

#================================================================================


#IMPACT COVID-19
par(mfrow = c(1,2))
kruskal.test(newData$adoptionDigital, newData$impactCOVID)
chisq.test(newData$adoptionDigital, newData$impactCOVID)
plot(newData$impactCOVID, newData$adoptionDigital, main="COVID-19 et Digital")
kruskal.test(newData$offreEmploiPFE, newData$impactCOVID)
plot(newData$impactCOVID, newData$offreEmploiPFE, main="COVID-19 offreEmploiPFE")
kruskal.test(newData$accesOpportuniteEmploi, newData$impactCOVID)
chisq.test(newData$accesOpportuniteEmploi, newData$impactCOVID)
plot(newData$impactCOVID, newData$accesOpportuniteEmploi, main="COVID-19 OpportuniteEmploi")
kruskal.test(newData$modeTravail, newData$impactCOVID)
chisq.test(newData$modeTravail, newData$impactCOVID)
plot(newData$modeTravail, newData$impactCOVID, main="COVID-19 OpportuniteEmploi")
#Un impact oui mais cela n'a pas beaucoup change les habitudes de travail

#ETUDE PAR QUOTA
maroc = newData %>%
	filter( Nationalite == "M")
View(maroc)
#Test non parametrique Integration Homme-Femme secteur Informatique au Maroc
t.test(maroc$grilleSalaire~maroc$Genre)
chisq.test(newData$Genre, newData$adaptationDifficile)
#La difficulte d'adaptation n'est pas liee au genre
chisq.test(maroc$Genre, maroc$grilleSalaire)
plot(maroc$Genre, maroc$grilleSalaire, main="Salaire Homme/Femme Maroc")
chisq.test(maroc$Genre, maroc$satisfactionSalaire)
plot(maroc$Genre, maroc$satisfactionSalaire, main="Satisfaction Salaire Maroc")
chisq.test(maroc$accesOpportuniteEmploi, maroc$Genre)
kruskal.test(maroc$accesOpportuniteEmploi, maroc$Genre)
#Pas de discrimination
plot(maroc$accesOpportuniteEmploi, maroc$Genre, main="Acces au opportunites par genre au Maroc")

#======================================================================================
#LE MODE DE REGRESSION
cor.test(newData$Age, newData$anneeDiplome, method="pearson")
cor.test(newData$Age, newData$anneeDiplome, method="spearman")
#Il y'a une association moyenne dans le sens negatif
#Entre l'age et l'annee d'obtention du diplome
#Les profils ingenieurs de plus en plus jeunes seront confrontes au marche du travail
table(newData$Age, newData$anneeDiplome)
#Verification par la table d'association

#Cote promotion et evolution des salaires
cor.test(newData$anneeDiplome, newData$grilleSalaire, method="pearson")
cor.test(newData$anneeDiplome,newData$grilleSalaire, method="spearman")
table(newData$anneeDiplome, newData$grilleSalaire)
#Il n'y a pas d'association notable

#la jeunesse du laureat impacte-elle son salaire
cor.test(newData$Age, newData$grilleSalaire, method="pearson")
cor.test(newData$Age, newData$grilleSalaire, method="spearman")
table(newData$Age, newData$grilleSalaire)
#Il y'a une faible association visible entre l'age et le salaire

par(mfrow = c(1,3))
plot(newData$anneeDiplome, newData$Age, main="Evolution Age-Promotion")
plot(newData$anneeDiplome, newData$grilleSalaire, main="Evolution Age-Promotion-Salaire")
plot(newData$Age, newData$grilleSalaire, main="Rapport Age-Salaire")


#Modele de regression lineaire age et formation des laureats ENSA Kenitra et ENSIAS Rabat
par(mfrow = c(1,1))
lm(newData$grilleSalaire~newData$anneeDiplome)
summary(lm(newData$grilleSalaire~newData$anneeDiplome))
hist(residuals(lm(newData$grilleSalaire~newData$anneeDiplome)))
shapiro.test(residuals(lm(newData$grilleSalaire~newData$anneeDiplome)))
skewness(residuals(lm(newData$grilleSalaire ~ newData$anneeDiplome)))
kurtosis(residuals(lm(newData$grilleSalaire ~ newData$anneeDiplome)))
qqnorm(residuals(lm(newData$grilleSalaire ~ newData$anneeDiplome)))
qqline(residuals(lm(newData$grilleSalaire ~ newData$anneeDiplome)))
#Modele de regression  valide car le residu  suit  la loi normale

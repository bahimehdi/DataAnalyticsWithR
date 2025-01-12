#etape 1: definition de la problematique et les données
# on souhaite étudier les notes des étudiants du module de statistique
#question de recherche :quelle est la performance des étudiants?
#dictionnaire de données:
#reformulation de la questions de recherche :y'a t-il un lien entre les caractéristiques des étudiants et sa performance?
#etape2: collecte des données
#collecte mannuelle (saisie des données)
age=c(19,20,21,50)
genre=c("H","F","H","F")
niveau=c("1A","2A","1A","3A")
filiere=c("GI","GIndus","GI","GI")
pays=c("Mauritanie","Maroc","Maroc","Caméron")
note=c(12.5,19,17,3.5)
d1=data.frame(age,genre,niveau,filiere,pays,note)
View(d1)
#M2 a partir d'un ficher
library(readxl)
d2 <- read_excel("data.xlsx")
View(d2)
#etape3:pré-traitement
#3.1codification et conversion
if (!is.numeric(d2$age)){
  d2$age=as.numeric(d2$age)
}
if (!is.numeric(d2$note)){
  d2$note=as.numeric(d2$note)
}
if (!is.character(d2$genre)){
  d2$genre=as.character(d2$genre)
}
if (!is.character(d2$pays)){
  d2$pays=as.character(d2$pays)
}
if (!is.character(d2$niveau)){
  d2$niveau=as.character(d2$niveau)
}
#variables qualitative sont a la base des textes (nominale ou ordinale(niveau))
#variables qualitatives sont par defaut nominale(oridinale cas d'un ordre conventionnel)
if (is.character(d2$niveau)){
  d2$niveau=as.ordered(d2$niveau)
}
if (is.character(d2$genre)){
  d2$genre=as.factor(d2$genre)
}
if (is.character(d2$pays)){
  d2$pays=as.factor(d2$pays)
}
if (is.character(d2$filiere)){
  d2$filiere=as.factor(d2$filiere)
}
#3.2nettoyage des données
#3.2.1traitement des valeurs aberrantes
boxplot(d2$age)
out=boxplot.stats(d2$age)$out
for (i in 1:length(d2$age))
{
  for (j in 1:length(out)){
    if(d2$age[i]==out[j] && !is.na(d2$age[i])){
    d2$age[i]<-NA
  }
  }
}
boxplot(d2$note)
#out2=boxplot.stats(d2$note)$out
#for (i in 1:length(d2$note))
#{
#  for (j in 1:length(out2)){
#   if(d2$note[i]==out2[j] && !is.na(d2$note[i])){
#      d2$note[i]<-NA
#   }
 # }
#}
#on va pas traiter les valeurs de note car c'est normal de trouve des valeurs entre 0et 20 
#3.2.2traitement des valeurs manquantes
#on traite par estimation ou suppression ou remplacement
c=0
for (i in 1:length(d2$age))
{
    if (is.na(d2$age[i])){
      c=c+1
    }
  }
propNA=c/length(d2$age)
if(propNA>=0.05){
  print("estimation")
}else{
  print("supprimer")
} 

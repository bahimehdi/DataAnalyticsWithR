#1.Etape 1 : collecte de données 
# Charger la bibliothèque pour lire Excel
library(readxl)
d2 <- read_excel("C:/Users/Mehdi/Downloads/Data-SalaryGender.xlsx")
View(d2)
#2.Etape 2: pretraitement
#2.1. conversion et recodification 
if (!is.numeric(d2$Age)){
  d2$Age=as.numeric(d2$Age)
}
if (!is.character(d2$Sexe)){
  d2$Sexe=as.character(d2$Sexe)
}
if(!is.numeric(d2$Scolarité)){
  d2$Scolarité=as.numeric(d2$Scolarité)
}
if(!is.numeric(d2$`Revenuen$`)){
  d2$`Revenuen$`=as.numeric(d2$`Revenuen$`)
}
if(!is.character(d2$Fonction)){
  for (i in 1: length(d2$Fonction)){
    if(d2$Fonction[i]==1 ) {
      d2$Fonction[i]="responsable"
    }
    if(d2$Fonction[i]==2 ) {
      d2$Fonction[i]="ingenieur"
    }
    if(d2$Fonction[i]==3 ) {
      d2$Fonction[i]="technicien"
    }
  }
  d2$Fonction=as.character(d2$Fonction)
}
if (!is.numeric(d2$Sexe)){
  d2$Sexe=as.numeric(d2$Sexe)
  for (i in 1: length(d2$Sexe)){
    if (d2$Sexe[i]==1){
      d2$Sexe[i]="homme"
    }
    if (d2$Sexe[i]==2){
      d2$Sexe[i]="femme"
    }
  }
  d2$Sexe=as.character(d2$Sexe)
}
if (!is.numeric(d2$Questionno1)){
  d2$Questionno1=as.numeric(d2$Questionno1)
  for (i in 1: length(d2$Questionno1)){
    if (d2$Questionno1[i]==1){
      d2$Questionno1[i]="oui"
    }
    if (d2$Questionno1[i]==2){
      d2$Questionno1[i]="non"
    }
  }
  d2$Questionno1=as.character(d2$Questionno1)
}
if(is.character(d2$Fonction)){
  d2$Fonction= as.ordered(d2$Fonction)
}
if (is.character(d2$Sexe)){
  d2$Sexe=as.factor(d2$Sexe)
}
if (is.character(d2$Questionno1)){
  d2$Questionno1=as.factor(d2$Questionno1)
}
# traitement des valeurs aberrantes et manquantes 
# il n'y'a pas de valeurs manquantes dans d2 
#traitement des valeurs aberrantes 
boxplot(d2$Age) #  pas de valeurs aberrantes 
boxplot(d2$Scolarité) # il ya une valeur aberrantes -- > on va l 'a traiter 
boxplot(d2$`Revenuen$`) #pas de valeurs aberrantes 

out=boxplot.stats(d2$Scolarité)$out
for (i in 1:length(d2$Scolarité)){
  if ( d2$Scolarité[i]==out && !is.na(d2$Scolarité[i])){
    d2$Scolarité[i]= NA
  }
}

for (i in 1:length(d2$Scolarité)){
  if ( is.na(d2$Scolarité[i])){
    d2$Scolarité[i]=mean(d2$Scolarité , na.rm = TRUE)
    
  }
}
#conversion en entier de scolarite 
d2$Scolarité=as.integer(d2$Scolarité)

#tester la normalite des variables quantitatives 
shapiro.test(d2$Scolarité) # il  y'a de la normalite 
shapiro.test(d2$Age)# pas de normalite 
shapiro.test(d2$`Revenuen$`)#il y a de la  normalite

# La quuasi-normalite ressemble a la loi normal en sens strict
#Si l'aplodissement (Kurtotis) et l'asymetrie (skewnes) appartient [-3;3] => La distribution est quasi-normale
#Dans ce cas on utilise les deux tests parametriques et non parametriques
#Si les conclusions des deux tests coincident, on considere le test parametrique
#Si la conclusion des deux tests divergent, on considere le non-paramatrique

# Tester la quasi-symetrie
library(moments)
skewness(d2$Age) # = 1.03... appartient a [-3;3]
kurtosis(d2$Age) # =2.81... appartient a [-3;3]

# Donc l'age est quasi-symetrique => On utilise les deux tests parametriques et non-parametriques

#etape 3 : traitement
# 3.1 Statistique univarie
summary(d2)
# l'echantillon est equilibre par rapport au genre, alors on ne peut pas conclure

chisq.test(table(d2$Sexe))
# pas de difference significative entre homme et femme
chisq.test(table(d2$Fonction))
# pas de diffrence significative en terme de scolarite
chisq.test(table(d2$Fonction,d2$Sexe))
table(d2$Fonction,d2$Sexe)  
fisher.test(table(d2$Fonction,d2$Sexe))
# conclusion : Il n'y a pas de difference entre les hommes et les femmes par fonction
plot(d2$Sexe,d2$Scolarité)
library(car)
leveneTest(d2$Scolarité~d2$Sexe)

t.test(d2$Scolarité~d2$Sexe,var.equal =TRUE)
#conclusion : Les hommes et le femmes ont les memes annees scolaires

#La verification du revenue
leveneTest(d2$`Revenuen$`~d2$Sexe)
t.test(d2$`Revenuen$`~d2$Sexe,var.equal =TRUE)
#conclusion : il y a une difference entre les femmes et les hommes, alors l'hypothese H1 est acceptee
#on etudie, s'il y a reelement une difference ou pas
chisq.test(table(d2$Questionno1))

#conclusion : les canadiens pensent qu'il n'y a pas de difference entre les revenues des femmes et des hommes mais en realite il y a

library(psych)
d3 <- read_excel("C:/Users/Mehdi/Downloads/Items-SalaryGender.xlsx")
View(d3)
alpha(data.frame(d3$Item1,d3$Item2,d3$Item3,d3$Item4,d3$Item5))
# alpha doit-etre > 0.7, mais ca nous ne concerne pas dans notre etude statistique
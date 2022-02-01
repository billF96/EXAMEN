data = read.csv("worldHappiness.csv")
View(data)


#Q!

install.packages("DataExplorer")
library(DataExplorer)
create_report(data)

# 2. Que signifie t’elle ? 

data$GDP #pour voir la variable GDP

# 3.Faire un resumé statistique de cette variable 


summary(data$GDP)

# 4. Existe t’il des données aberrantes dans cette variable ? Expliquez 

boxplot(data$GDP)
# pas de valeurs aberantes dans notre base

# 5. Cette variable suit elle la loi normale ? Expliquez 
# pour determiner si une variable suis la loi normale ou pas 
#il faut effectuer un test de shapiro

# HO : l'echantillon suit une loi normale
# H1 : l'echantillon ne suit pas une loi narmale

shapiro.test(data$GDP)
head(data$GDP)

#notre P-value est < 5% donc on rejette l'hypothese nulle. 
#Notre echantillon ne suis donc pas une loi normale'

# 6. Donnez l’intervalle de confiance à 90% de la moyenne  

t.test(data$GDP, conf.level = 0.90)

# selon les donnée on a une moyenne de GDP = 9.298247 et 
# on a 90% de chance que le GDP dans la table se trouve entre 
# (9.132107 et 9.464388) 

#  7. calcule le nombre de cas de covid moyen par pays et par region du monde
library(dplyr)
data1 = data %>% select(Country, GDP)
#REMOVE NA VALUES
data2 = na.omit(data1)
#Arrange in ascending order
data3 = arrange(data2 , GDP)
#On recupere les pays avec les GDP les plus élevé
data4 = tail(data3,10)

data3 %>% arrange(desc(GDP))

# REPONSE de PAPY (BONNE REPONSE)

data11 = data %>% 
arrange(desc(GDP)) %>%
  head(10)
View(data11)
data12 = data11 %>% select(Country, GDP)
View(data12)


# 8. Calculez la moyenne des GDP par region du monde. 
# Dessinez un graphique à barres. Interpréter.

View(data)
region <- data%>% 
  group_by(data$Region)
  region%>%summarize(mean(GDP))

  counts <- data(GDP$region)
  barplot(counts, main = "GDP By Region" ,
          xlab= "Region")
  
# 8 reponses alternative
  library("dplyr")
  data_group = data_clean2%>% group_by(Region)
  data_group = data_group%>% summarise(gdp_mean = mean(GDP))
  library("ggplot2")
  ggplot(data_group, aes(Region , gdp_mean, fill = Region)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, size = 12)) + ylim(0,12)
  
  
  
  
  #9. Étudiez les différentes variables catégorielles : 
  # fréquences, proportions, graphiques.
  # Variable : GDP et CORUPTION
  gdp
  attach(mydata)
  mytable <- data(A,B)
  
  View(data)
  #REMOVE NA VALUES
  #data_clean2 = na.omit(data)
  
  data_clean = data %>% filter(!is.na(data$GDP))
  
  freq_h=prop.table(data_clean$Happiness) # fréquence pour la variable Hapiness
  freq_h
  
  freq_G=prop.table(data_clean$GDP) # fréquance pour la variable GDP
  freq_G
  
  # proportions
  #effectif
  table(data_clean$GDP)
  
  #9 REPONSE ALTERNATIVE
  #faire liste variable 
  View(data_clean)
  
  n_occur = data.frame(table(data_clean$Region))
  n_occur[n_occur$Freq > 1,]
  
  colnames(data_clean)
  
  #10. étudiez les différentes variables quantitatives :
  # statistiques descriptives, graphiques.
  
  summary_data = summary(data_clean$Happiness)
  summary_data
  
  library(Hmisc)
  describe(data_clean2)

  # graphique pour histogram
  
  hist(data_clean$Happiness)
  
  boxplot(data_clean$Happiness)
  # on a pas de outliers on peut dire que les valeures sont 
  #situé au tour de la moyene et suivent une loi normale

  shapiro.test(data$Happiness)
  
  # notre test de shapiro nous confirme 
  #que la variable Happpiness sut une loi normale 
  
  #11. Étudiez les associations entre variables catégorielles 
  #(visualisation et test) 
  
  # pour tester deux variable qualitatives on va utiliser le test de chi-deux
  
  data_chi2 = data_clean %>% select(Country, Region)
  View(data_chi2)
  chisq.test(data_chi2$Country, data_chi2$Region)
  
  #on a une p-value = 0.424 supérieure a 5% 
  #donc on accepte l'hypothese nulle d'indepenence
  
  
  
  # 12. Étudiez les associations entre variables quantitatives 
  # (visualisation et test)
library(Hmisc)
  
cor(data_clean$Happiness, data_clean$GDP, method = c("pearson", "kendall", "spearman"))

cor.test(data_clean$Happiness, data_clean$GDP, method=c("pearson", "kendall", "spearman"))
# on rejette l'hypothese nulle cor = 76%
#visualisation correlation entre 2 variables
install.packages("metan")
library(metan)
corr_1<- corr_coef(data_clean[,2:7])
plot(corr_1)

#13 13. Étudiez les associations entre les variables quantitatives 
# et les variables catégorielles 
library(ggplot2)
library(dplyr)
Confidence_governement_mean = mean(ConfidenceInGovernment)
data_clean3<- data %>% filter(!is.na(data$ConfidenceInGovernment))
data_group <- data_clean3 %>% group_by(Region)
data_group = data_group %>% summarise(Confidence_governement_mean = mean(ConfidenceInGovernment))

ggplot(data_group, aes(Region, Confidence_governement_mean, fill=Region)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, size = 12)) + ylim(0, 1)


# regressions
# 14. Étudiez comment happiness dépend de la région et des autres variables.

a= data_clean$Happiness
b= data_clean$Generosity
c= data_clean$GDP
d= data_clean$SocialSupport
e= data_clean$Health
f= data_clean$Freedom
e= data_clean$NegativeAffect
g= data_clean$Corruption
h= data_clean$ConfidenceInGovernment
k= data_clean$PositiveAffect


model = lm(a~b+c+d+e+f+g+h+k)
model
summary(model)

install.packages("metan")
library(metan)
corr1<- corr_coef(data_clean[2:])
plot(corr1)
#15. Effectuez une régression simple pour prédire happiness en fonction du GDP. 
# Donnez l’équation de la droite 


#TRACER COURBE SCATTER PLOT POUR VISUALISER DONNEES
plot(Happiness ~ GDP, data_clean2) 
abline(lm(Happiness ~ GDP, data_clean2))

reglin = lm(Happiness~GDP,data_clean2)
summary(reg_lin)
GDP <- data.frame(GDP = c(10.03,7.56,9.5)) #creer une table test pour la prediction
View(GDP)
pred = predict(reglin, GDP, interval = "confidence", level = 0.95)
pred

#15. Effectuez une régression multiple entre happiness et certains variables 
# quantitatives que vous choisirez.

model = lm(a~b+c+d+e+f+g+h+k)
model
summary(model)
#visualise les correlation
#scatter plot
plot(a~b+c+d+e+f)

#16. Utilisez la librarie rworldmap pour créer une carte du monde avec la 
#variable happiness 

library(ggplot2)
library(dplyr)
mapdata = map_data("world") ##ggplot2
mapdata = mapdata %>% rename(Country = region)
mapdata = left_join(mapdata, data, by="Country")
mapdata1 = mapdata %>% filter(!is.na(mapdata$Happiness))
map = ggplot(mapdata1, aes( x = long, y = lat, group=group)) + geom_polygon(aes(fill = Happiness), color = "Grey")
map



















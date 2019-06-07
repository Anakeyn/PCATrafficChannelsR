##########################################################################
# Auteur : Pierre Rouarch 2019 - Licence GPL 3
# PCATrafficChannelsR
# Analyse en Composantes principales canaux trafic Web.
# Pour illustrer notre propos nous utiliserons le jeu de données de 
# l'association Networking-Morbihan 
##########################################################################
#Packages et bibliothèques utiles (décommenter au besoin)
##########################################################################
#install.packages("lubridate")  #si vous ne l'avez pas
#install.packages("tseries")
#install.packages("devtools")
#devtools::install_github("twitter/AnomalyDetection")  #pour anomalyDetection de Twitter
#devtools::install_github('sinhrks/ggfortify') #pour ggfortify
#install.packages("XML")
#install.packages("stringi")
#install.packages("BSDA")
#install.packages("BBmisc")
#install.packages("stringi")
#install.packages("FactoMineR")
#install.packages("factoextra")
#install.packages("rcorr")

#install.packages("lubridate")  #si vous ne l'avez pas
library (lubridate) #pour yday
#library(tseries) #pour ts
library(AnomalyDetection) #pour anomalydetectionVec
#library(XML) # pour xmlParse
#library(stringi) #pour stri_replace_all_fixed(x, " ", "")
library(BSDA)  #pour SIGN.test 
library(BBmisc) #pour which.first
#install.packages("stringi")
library(stringi) #pour stri_detect
library(ggfortify)  #pour ploter autoplot type ggplot
#install.packages("tidyverse")  #si vous ne l'avez pas #pour gggplot2, dplyr, tidyr, readr, purr, tibble, stringr, forcats 
#install.packages("forecast") #pour ma
#Chargement des bibliothèques utiles
library(tidyverse) #pour gggplot2, dplyr, tidyr, readr, purr, tibble, stringr, forcats 
library(forecast)  #pour  arima, ma, tsclean
library(FactoMineR) #pour ACP
library(factoextra)  #compléments ACP FactoMineR

##########################################################################
# Récupération des fichiers
##########################################################################
dfPageViews <- read.csv("dfPageViews.csv", header=TRUE, sep=";") 
#str(dfPageViews) #verif
#transformation de la date en date :-)
dfPageViews$date <- as.Date(dfPageViews$date,format="%Y-%m-%d")
#str(dfPageViews) #verif
str(dfPageViews) #72821 obs
dfPageViews$index <- 1:nrow(dfPageViews)  #création d'un pour retrouver les "articles marketing" 

#ensuite
#pour les articles
myArticles <- read.csv("myArticles.csv", header=TRUE, sep=";") 
#transformation de la date en date :-)
myArticles$date <- as.Date(myArticles$date,format="%Y-%m-%d")
str(myArticles) #verif

#recuperer le fichier avec les channels 
mySourcesChannel <- read.csv2("mySourcesChannel.csv", header=TRUE)
str(mySourcesChannel) #voyons ce que l'on récupère
#pour effectuer le left join besoin d'une chaine de caractère.
mySourcesChannel$source <- as.character(mySourcesChannel$source)

##########################################################################
# Pour le traffic Global on ajoute les canaux
##########################################################################
#recuperation de la variable channel dans le dataframe 
#principal par un left join.
dfPVChannel <- left_join(dfPageViews, mySourcesChannel, by="source")
#verifs
str(dfPVChannel)  #72821
head(dfPVChannel)  
plyr::count(as.factor(dfPVChannel$channel)) #5 canaux
plyr::count(as.factor(dfPVChannel$pagePath)) #1262 types de pages

#Préparationd des données pour l'ACP

PVDataForACP <-  dfPVChannel[, c("pagePath", "channel")]  %>%   #on ne garde que PagePath et channel
  group_by(pagePath, channel) %>%   #groupement par cleanLandingPagePath et channel
  mutate(pageviews = n()) %>%                   #on décompte les pages vues
  unique() %>%                                  #découblonnement
  spread(key=channel, value=pageviews, fill = 0, convert = FALSE, drop = TRUE,
         sep = NULL)  #eclatement du facteur channel en variables 


#Calcul de l'ACP avec FactoMineR
res.pca = PCA(PVDataForACP[, -1], scale.unit=TRUE, ncp=5, graph=F) 
summary(res.pca) #resumé des données


#############################Plots Simples individus et variables.
#plot(res.pca, choix = "ind")  #individus
#plot(res.pca, choix = "var")  #variables



#########################   ScreePlot ################################################
ScreePlot <- fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 100))
ggpubr::ggpar(ScreePlot,
              title = "ScreePlot - Toutes les Pages vues",
              subtitle = "Pratiquement toute l'information est contenue dans la première composante",
              xlab = "Dimensions",
              ylab = "Pourcentage de variance expliquée",
              caption = "ScreePlot pour toutes les Pages Vues")
ggsave(filename = "PV-ScreePlot.jpg",  dpi="print") #sauvegarde du graphique

#########################  Diagramme des variables  ################################################
# Colorer en fonction du cos2: qualité de représentation
VarPlot <- fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)
ggpubr::ggpar(VarPlot,
              title = "ACP - VarPlot - Toutes les Pages vues",
              subtitle = "Les variables sont quasiment toutes sur la première dimension \n webmail diffère légèrement",
              caption = "Diagramme des variables pour toutes les pages vues")

ggsave(filename = "PV-VarPlot.jpg",  dpi="print") #sauvegarde du graphique

##########################################################################
# Pour le traffic de base
##########################################################################
#Recréation du trafic de base
#récupere les chemins des pages pour les comparer dans dfPageViews
myArticles$pagePath <- str_split_fixed(myArticles$link, "https://www.networking-morbihan.com", 2)[,2]
patternArticlesToRemove <- unique(myArticles$pagePath)
#Pour les pages de base on enleve les pagePath de nos articles
indexPagePathToRemove <- -grep(pattern = paste(patternArticlesToRemove, collapse="|"), dfPageViews$pagePath)
dfBasePageViews <- dfPageViews[indexPagePathToRemove,]
#puis on enleve les landingPagePath de nos articles
indexLandingPagePathToRemove <- -grep(pattern = paste(patternArticlesToRemove, collapse="|"), dfBasePageViews$landingPagePath)
dfBasePageViews <- dfBasePageViews[indexLandingPagePathToRemove,]
str(dfBasePageViews) #37614 obs.

#recuperation de la variable channel dans la dataframe principale par un left join.
dfBasePVChannel <- left_join(dfBasePageViews, mySourcesChannel, by="source")
#verifs
str(dfBasePVChannel)
head(dfBasePVChannel)
plyr::count(as.factor(dfBasePVChannel$channel))

#préparation des données pour l'ACP
BasePVDataForACP <-  dfBasePVChannel[, c("pagePath", "channel")]  %>%   #on ne garde que PagePath et channel
  group_by(pagePath, channel) %>%   #groupement par cleanLandingPagePath et channel
  mutate(pageviews = n()) %>%                   #on décompte les pages vues
  unique() %>%                                  #découblonnement
  spread(key=channel, value=pageviews, fill = 0, convert = FALSE, drop = TRUE,
         sep = NULL)  #eclatement du facteur channel en variables 


#Calcul de la
res.pca = PCA(BasePVDataForACP[, -1], scale.unit=TRUE, ncp=5, graph=F) 
summary(res.pca) #resumé des données


#########################   ScreePlot ################################################
ScreePlot <- fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 100))
ggpubr::ggpar(ScreePlot,
              title = "ScreePlot - Pages de Base",
              subtitle = "L'information est encore plus concentrée dans la première composante",
              xlab = "Dimensions",
              ylab = "Pourcentage de variance expliquée",
              caption = "ScreePlot pour Les pages de Base")
ggsave(filename = "Base-PV-ScreePlot.jpg",  dpi="print") #sauvegarde du graphique

#########################   Diagramme des variables ################################################
# Colorer en fonction du cos2: qualité de représentation
VarPlot <- fviz_pca_var(res.pca, col.var = "cos2",
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                        repel = TRUE # Évite le chevauchement de texte
)
ggpubr::ggpar(VarPlot,
              title = "ACP - VarPlot - Pages de Base",
              subtitle = "Les variables sont quasiment toutes sur la première dimension \n webmail s'est même rapprochée de l'axe.",
              caption = "Diagramme des variables pour les pages de bases")

ggsave(filename = "Base-PV-VarPlot.jpg",  dpi="print") #sauvegarde du graphique

##########################################################################
#regardons pour le trafic Direct  Marketing uniquement i.e le traffic dont
# la source a dirigé vers une page Articles Marketing 
##########################################################################
#Construction du trafic Direct Marketing
#on garde uniquement les landingPagePath de nos articles  : 
#DM = Direct Marketing
patternArticlesToKeep <- unique(myArticles$pagePath)
indexLandingPagePathToKeep <- grep(pattern = paste(patternArticlesToKeep, collapse="|"), dfPageViews$landingPagePath)
dfDMPageViews <- dfPageViews[indexLandingPagePathToKeep,]
str(dfDMPageViews) #28553 obs.

dfDMPVChannel <- left_join(dfDMPageViews, mySourcesChannel, by="source")
str(dfDMPVChannel)

#recuperation de la variable channel dans la dataframe principale par un left join.
dfDMPVChannel <- left_join(dfDMPageViews, mySourcesChannel, by="source")
#verifs
str(dfDMPVChannel)
head(dfDMPVChannel)
plyr::count(as.factor(dfDMPVChannel$channel))


DMPVPVDataForACP <-  dfDMPVChannel[, c("pagePath", "channel")]  %>%   #on ne garde que PagePath et channel
  group_by(pagePath, channel) %>%   #groupement par cleanLandingPagePath et channel
  mutate(pageviews = n()) %>%                   #on décompte les pages vues
  unique() %>%                                  #découblonnement
  spread(key=channel, value=pageviews, fill = 0, convert = FALSE, drop = TRUE,
         sep = NULL)  #eclatement du facteur channel en variables 



res.pca = PCA(DMPVPVDataForACP[, -1], scale.unit=TRUE, ncp=5, graph=F) 
summary(res.pca) #resumé des données


#########################   ScreePlot ################################################
ScreePlot <- fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 100))
ggpubr::ggpar(ScreePlot,
              title = "ScreePlot - Pages Direct Marketing",
              subtitle = "Cette fois l'information est moins concentrée dans la première composante \n mais cela reste important",
              xlab = "Dimensions",
              ylab = "Pourcentage de variance expliquée",
              caption = "ScreePlot pour Les pages Direct Marketing")
ggsave(filename = "DM-PV-ScreePlot.jpg",  dpi="print") #sauvegarde du graphique

#########################   Diagramme des variables ################################################
# Colorer en fonction du cos2: qualité de représentation
VarPlot <- fviz_pca_var(res.pca, col.var = "cos2",
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                        repel = TRUE # Évite le chevauchement de texte
)
ggpubr::ggpar(VarPlot,
              title = "ACP - VarPlot - Pages Direct MArketing",
              subtitle = "Cette fois le search se détache des autres variables. Ces dernières \ncorrespondent plus aux actions Marketing ponctuelles.",
              caption = "Diagramme des variables pour les pages Direct Marketing")

ggsave(filename = "DM-PV-VarPlot.jpg",  dpi="print") #sauvegarde du graphique




##########################################################################
# MERCI pour votre attention !
##########################################################################


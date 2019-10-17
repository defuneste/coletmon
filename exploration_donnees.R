# script d'exploration des données pour le projet col&mon
# 16-10-2019
##.###################################################################################33
## I. Chargement des données de l'xp ====
##.#################################################################################33

## 1 - Les bibliotheques ================

library(sp) # ancien package spatial toujours présent
library(sf) # package spatial
library(dplyr) # manip données
library(lubridate) # les dates 
library(forcats) # pour les facteurs


## 2 - Les données ================

#celui ci est un df
fait.dat <- readRDS("data/T0New.Rds")
class(fait.dat)


#celui ci est un tible
implantation.dat <- readRDS("data/T0impl.Rds")
class(implantation.dat)

# script d'exploration des données pour le projet col&mon
# 16-10-2019
##.###################################################################################33
## I. Chargement des données de col&mon ====
##.#################################################################################33

## 1 - Les bibliotheques ================

library(sp) # ancien package spatial toujours présent
library(sf) # package spatial
library(dplyr) # manip données
library(lubridate) # les dates 
library(forcats) # pour les facteurs


## 2 - Les données ================

#celui ci est un tible
implantation.dat <- readRDS("data/T0impl.Rds")
class(implantation.dat)


#celui ci est un df
fait.dat <- readRDS("data/T0New.Rds")
class(fait.dat)
summary(fait.dat)
unique(fait.dat$caracNew)

# on essaie d'avoir un bon encoding

Encoding(fait.dat$usual_name) <- "latin1"
Encoding(fait.dat$caracteristique) <- "latin1"
Encoding(fait.dat$linked_implantation_name) <- "latin1"
           

# une exraction des relations
relation.dat <- fait.dat[fait.dat$caracNew == "Relations" ,]
dim(relation.dat)


##.###################################################################################33
## II. Exploration des donnees ====
##.#################################################################################33

## 1 - Modalite/modAgreg ================

# un peu d'exploration
table(relation.dat$modAgreg)
table(relation.dat$modalite)

t(table(relation.dat$modAgreg, relation.dat$modalite))

# on regarde pour des implantation fameuse
relation.dat[relation.dat$idimplantation == 26,]


## 2 date, duree ================

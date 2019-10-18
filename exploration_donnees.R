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
library(igraph) # package classic pour les graphs


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


## 3 - export/imporrt en csv  ================
# j'ai du rajouter cette etape car knitr a du mal à gerer le multi encoding
# write.csv(fait.dat, "data/fait.txt")
fait.dat <- read.csv("data/fait.txt")


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

# ou on a des NA
sapply(relation.dat, anyNA)
# on va les compter
relation_na.dat <- as.data.frame(apply(relation.dat, 2, function(x)length(x[is.na(x)])))
names(relation_na.dat) <- "Nbr_NA"
relation_na.dat$variables <- rownames(relation_na.dat)

#exploration des données manquantes dans les dates
NA_date <- relation.dat[is.na(relation.dat$date_start_min),]

# ventilation des NA dans modAgreg
table(NA_date$modAgreg)

#on regarde les lieux descendant present comme descendant par rapport au id des lieux d'ascendant
NA_date$fklinked_implantation[NA_date$modAgreg == "D"] %in% NA_date$idimplantation[NA_date$modAgreg == "A"]

NA_date[NA_date$modAgreg == "D",][76,]
NA_date[NA_date$idimplantation == 2483,]

rbind(NA_date[NA_date$modAgreg == "D",][76,],
relation.dat[relation.dat$idimplantation ==2483,])

table(NA_date$usual_name)

##.###################################################################################33
## III. Graphs ====
##.#################################################################################33

## 1 - Vertex/hedge ================

#on va viltrer pour n'avoir que deux colonnes de relation

relation <- relation.dat[relation.dat$modAgreg != "A",] # on enleve les doublons
dim(relation) # verif
names(relation) 
# one ne garde que les noms
relation <- subset(relation, select =  c("usual_name", "linked_implantation_name"))
# objet de graphs
graph_ensemble <- graph.edgelist(as.matrix(relation), directed = FALSE)

# oh que c'est laid
plot(graph_ensemble)

# Subset vertices and edges
V(graph_ensemble)
E(graph_ensemble)

# compte le nombre de liens
gsize(graph_ensemble)

# compte le nombre de noeuds
gorder(graph_ensemble)

# une indexation sur les noeux avec "cîteaux (2)"
E(graph_ensemble)[[inc("Cîteaux (2)")]]

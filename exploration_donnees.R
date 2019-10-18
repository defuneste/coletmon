# script d'exploration des données pour le projet col&mon
# 16-10-2019
# Attention les liens relatifs sont pour un os linux !!!!
# norme de codage :
# .dat est un df ou tible
# .shp est un sf
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
library(tmap) # un peu de carto statique d'explo


## 2 - Les données ================

#celui ci est un tible
implantation.dat <- readRDS("data/T0impl.Rds")
class(implantation.dat)


#celui ci est un df
fait.dat <- readRDS("data/T0New.Rds")
class(fait.dat)
summary(fait.dat)
unique(fait.dat$caracNew)

## on essaie d'avoir un bon encoding
# pour fait.dat
Encoding(fait.dat$usual_name) <- "latin1"
Encoding(fait.dat$caracteristique) <- "latin1"
Encoding(fait.dat$linked_implantation_name) <- "latin1"
# pour implantation
Encoding(implantation.dat$usual_name) <- "latin1"
Encoding(implantation.dat$Vocable) <- "latin1"


## 3 - export/imporrt en csv  ================
# j'ai du rajouter cette etape car knitr a du mal à gerer le multi encoding
# write.csv(fait.dat, "data/fait.txt")
write.csv(implantation.dat, "data/implantation.txt")
fait.dat <- read.csv("data/fait.txt")
implantation.dat <- read.csv("data/implantation.txt")

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
NA_date[!is.na(NA_date$date_stop_max),]

# ventilation des NA dans modAgreg
table(NA_date$modAgreg)

#on regarde les lieux descendant present comme descendant par rapport au id des lieux d'ascendant
NA_date$fklinked_implantation[NA_date$modAgreg == "D"] %in% NA_date$idimplantation[NA_date$modAgreg == "A"]

# c'est la 76 lignes de NA_date pour modagreg == D
rbind(NA_date[NA_date$modAgreg == "D",][76,],
relation.dat[relation.dat$idimplantation ==2483,])

# ou sont localise les NA

source("fonctions_carto.R") # on charge des fonctions de carto

# on passe implantation en sf
implantation.shp <- st_as_sf(implantation.dat[!is.na(implantation.dat$lat),], coords = c("lng", "lat"), crs = 4326)
# de wgs 84 au lambert 93
implantation.shp <- st_transform(implantation.shp, 2154) 
NA_date.shp <- st_sf(left_join(NA_date, implantation.shp, by = "idimplantation"))


diocese.shp <- diocese() # on charge les dioceses

plot(st_geometry(diocese.shp))
plot(st_geometry(implantation.shp), add =  T, col = "lightgrey", pch = 16, cex = .5)
plot(st_geometry(NA_date.shp), add = T, col = "red", pch = 16, cex = .5)
legend("topleft", legend=c("Avec dates", "Sans dates"), pch = 16,
       col=c("lightgrey", "red"))


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

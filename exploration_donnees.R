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
library(tidyr) # manip données /tidyverse
library(lubridate) # les dates 
library(forcats) # pour les facteurs
library(igraph) # package classic pour les graphs
library(tmap) # un peu de carto statique d'explo
library(ggplot2) # des graphiques
library(plotly) # des graphiques interactifs


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


## 2 Valeurs manquantes  ================

# Où a-t-on des NA
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

# une carte rapide
diocese.shp <- diocese() # on charge les dioceses

plot(st_geometry(diocese.shp)) # on plot les dioceses
plot(st_geometry(implantation.shp), add =  T, col = "lightgrey", pch = 16, cex = .5) # on ajoute les implantations
plot(st_geometry(NA_date.shp), add = T, col = "red", pch = 16, cex = .5) # celle qui n'ont pas de durée
legend("topleft", legend=c("Avec dates", "Sans dates"), pch = 16, 
       col=c("lightgrey", "red")) # une legende


## 3 date et durées  ================

# j'enleve ascendant car cela doublonne
relation_sans_A.dat <- relation.dat[relation.dat$modAgreg != "A",] 
relation_sans_A.dat <- relation_sans_A.dat[!is.na(relation_sans_A.dat$date_startC),]
dim(relation_sans_A.dat)

#start_min et Max
plot_ly(relation_sans_A.dat, x = relation_sans_A.dat$date_start_min, y = relation_sans_A.dat$date_start_max, type="scatter", # on def X et y
        # on fait un text
        text = paste(relation_sans_A.dat$usual_name, relation_sans_A.dat$modalite, relation_sans_A.dat$linked_implantation, sep = "\n")) 

# il y a pas de différence entre les deux dans les relations
sum((relation_sans_A.dat$date_stop_max - relation_sans_A.dat$date_stop_min), na.rm = T)

# on va utiliser ggplot2 donc il faut que cela soit en tidy, au moins un peu
relation_sans_A_tidy.dat <- gather(relation_sans_A.dat, "date_startC", "date_stopC", key = "debut_fin", value = "date")


# date de début et de fin des relation par modAgreg
ggplot(relation_sans_A_tidy.dat, aes(date, color = debut_fin)) +
    geom_freqpoly(binwidth = 50) + 
    facet_wrap(~modAgreg) +
    labs( x = "date (50 ans)" , y = "décompte" ) +
    theme_bw()
# ici par modAgreg  

# ggplot(relation_sans_A_tidy.dat, aes(date, color = modAgreg)) +
#     geom_freqpoly(binwidth = 50) + 
#     #facet_wrap(~modAgreg) +
#     labs( x = "date (50 ans)" , y = "décompte" ) +
#     theme_bw()

# un test avec plotly
# c'est pas fou il faut comprendre comment ajuster les bins
plot_ly(alpha = 0.6) %>%
    add_histogram(x = relation_sans_A.dat$date_start_min[relation_sans_A.dat$modAgreg == "D"]) %>% 
    add_histogram(x = relation_sans_A.dat$date_start_min[relation_sans_A.dat$modAgreg == "H"]) %>%
    add_histogram(x = relation_sans_A.dat$date_start_min[relation_sans_A.dat$modAgreg == "X"]) %>% 
    layout(barmode = "overlay")
    
hist(relation_sans_A.dat$DureeFact)

#une version plotly
plot_ly(alpha = 0.6) %>%
    add_histogram(x = relation_sans_A.dat$DureeFact)

# pas de durée
nrow(relation_sans_A.dat[relation_sans_A.dat$DureeFact == 0,])
# une durée
nrow(relation_sans_A.dat[relation_sans_A.dat$DureeFact != 0,])

names(relation_sans_A.dat)

# je dois dropper les factors non présent dans mon subset
relation_sans_A.dat$modalite <- factor(relation_sans_A.dat$modalite)
relation_sans_A.dat$durée01 <- 0
relation_sans_A.dat$durée01[relation_sans_A.dat$DureeFact != 0] <- 1

table(relation_sans_A.dat$modalite, relation_sans_A.dat$durée01) # puis un tableau

# un hist
plot_ly(alpha = 0.6) %>%
    add_histogram(x = relation_sans_A.dat$DureeFact[relation_sans_A.dat$DureeFact != 0])

# répartition des durée par date 
min(relation_sans_A.dat$date_startC)
max(relation_sans_A.dat$date_startC)
# on fait un facteur avec des intervales de temps
pas_de_temps <- seq(from = 400, to = 1800 , by = 50)

cut(relation_sans_A.dat$date_startC, pas_de_temps)
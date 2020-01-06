# script de fonction pour anaviz col&mon
# 07-01-2020
# !!!! Attention les liens relatifs sont pour un os linux !!!!
# norme de codage :
# .dat est un df ou tible
# .shp est un sf
# fonction en chameau : ceci_est_unefonction
##.###################################################################################33
## I. Chargement des données de col&mon ====
##.#################################################################################33

# 1 - chargement des rds =================

implantation.dat <- readRDS("data/T0impl20191126.rds")
fait.dat <- readRDS("data/T0New20191126.rds")
names(fait.dat)

# 2 - un index pour ce que l'on veut selectionner

# indexage est une liste contenant les variables pour indexer
Indexage <- list("caracNew" # le premier item est juste caracNew
                            # il est utile pour filtrer relation
                 , c("idimplantation", "usual_name", "fklinked_implantation","linked_implantation_name", 
                     "modaNiv1", "lat", "lng", "date_startC","date_stopC")) # ici c'est la liste de pas mal de variables utiles
Indexage[[1]]
Indexage[[2]]

##.###################################################################################33
## II. fonction anaviz ====
##.#################################################################################33

# 1 - une fonction pour filtrer les relations =================
# pe pas la plus utile, on peut rajouter un select pour les colonnes que l'on veut garder

filtrer_relation <- function(T0new) {
    subset(fait.dat, fait.dat[[Indexage[[1]]]] == "Relations")
    #subset(fait.dat, caracNew == "Relations")
}

# une version avec select si besoin par defaut elle prend les noms du premier tableau 
# elle a aussi un stop si ce n'est pas un df
filtrer_relation_select <- function(T0new, selection = names(T0new)) {
    if(!is.data.frame(T0new)){stop("La fonction nécessite un tableau et non un.e", class(T0new) ,"." )}
    subset(fait.dat, fait.dat[[Indexage[[1]]]] == "Relations", select = selection) # au besoin peut être utiliser Indexage[[2]] pour :  selection = Indexage[[2]]
}

# ex : 
relation.dat <- filtrer_relation_select(fait.dat, selection = Indexage[[2]])

# 2. Calculer la distance entre deux implantations liées (pour la portée) ==============================================
# On construit un nouveau tableau qui contient les lat/long de chaque cote de la relation 
# l'objectif est de dessiner les liens sur une carte : de A -> B
# Hélène indique que les lat/long dans relations.dat sont celles de l'implantation (idimplantation) et pas celle lièe (fklinked_implantation).
# on va donc renomer les lat/lomg des implantations et le noms des implantations puis les joindre pour constituer un tableau avec les coords de A et de B.  

relation <- relation.dat

# la fonction prend :
# relation un df des relations 
# implantation un df des implantations

distance_entre_implantation <- function(relation, implantation, selection = names(relation)){
    
# chargement des packages, pe cela n'est pas utile 
    if(require("dplyr") == FALSE)  
        install.packages("dplyr",  dependencies=c("Depends", "Suggests"))
    if(require("sf") == FALSE)  
        install.packages("sf",  dependencies=c("Depends", "Suggests"))
    
Relation_renomer <- dplyr::rename(relation,                                     # rename est pas mal utilisé donc on precise la library, il y a une dépendance sur dplyr
                                  idimpl_link=fklinked_implantation,            # ici on prend l'id lié
                                  usual_name_link=linked_implantation_name)     # idem pour le nom lié

# je vais decouper un peu le pipe d'Hélène avec une table produite pour la jointure 
# qui va comporter les lat/long des implantations liées
implantation_renomer <-  dplyr::select(implantation, idimpl_link = idimplantation, # selon la doc on peut directement renomer dans un select
                                       lat_link = lat, 
                                       lng_link = lng, 
                                       Dioc_link = Diocese) 
# Jointure via idimpl_link
relation_total.dat <- dplyr::left_join(Relation_renomer, implantation_renomer, by = "idimpl_link")

# Suppression des fichiers intermediaires
rm(Relation_renomer, implantation_renomer)

# si il y a des valeurs manquantes on les drop
relation_total.dat <- dplyr::filter(relation_total.dat, !is.na(lat) & !is.na(lat_link)) # on retire les NA, c'est 60 relations

# matrice de départ
from.dat  <- as.matrix(                                           # on passe tout dans une matrice
                 dplyr::select(relation_total.dat, lng, lat))     # on selectionne les lat long
#matrice d'arrivé
to.dat <- as.matrix( 
    dplyr::select(relation_total.dat, lng_link, lat_link) )

relation_total.dat$geometry <- do.call(sf::st_sfc,                                                             # on fait une fonction sfc
                                       lapply(                                                                 # on fait un apply sur chaque lignes
                                           1:nrow(relation_total.dat),
                                           function(i){                              
                                               sf::st_linestring(                                              # qui va tracer des lignes
                                                   matrix(                                                     # en prenant les points dans des matrices de from a to
                                                       c(from.dat[i,], to.dat[i,]), ncol=2, byrow=TRUE)        # composé du couple de point de départ et arrivé
                                               )}))

# Supression des deux matrices intermediaires
rm(from.dat,  to.dat)

# Pasage en en sf
relation_total.shp <- sf::st_transform(                                             # on va transformer en 2154
    sf::st_as_sf(relation_total.dat, sf_column_name = "geometry", crs = 4326),      # ici on fait onjet sf avec la colonne geometry et le crs de base qui était 4326
    2154)
# on retire relation_total.dat)
rm(relation_total.dat)

# calcul de la distance
relation_total.shp$distance_km <- round(                                    # on va arrondir le résultats à 2 chiffres
    as.numeric(                                                             # je drop units, 
        sf::st_length(relation_total.shp)/1000), 0)                         # on passe en km
return(relation_total.shp)
}

bob <- distance_entre_implantation(relation, implantation.dat)
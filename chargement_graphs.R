# script de chargement pour les données de col&mon
# 28-11-2019
# Attention les liens relatifs sont pour un os linux !!!!
# norme de codage :
# .dat est un df ou tible
# .shp est un sf
##.###################################################################################33
## I. Chargement des données de col&mon ====
##.#################################################################################33
# sf et dplyr sont utilusés 

# 2 - Imports des données =======

# je part de mes csv en UTF8
# faire une version qui part des T0imp et TOnew
fait.dat <- read.csv("data/fait.txt")
implantation.dat <- read.csv("data/implantation.txt")
# Drop des X car quand j'ai fait un export en csv 
fait.dat <- subset(fait.dat, select = - X)
implantation.dat <- subset(implantation.dat, select = - X)

# 3 - Mise en forme =======

relation.dat <- fait.dat[fait.dat$caracNew == "Relations" ,] # Extractions des relations
# Extraction des Déplacement 
relation.dat <- relation.dat[relation.dat$modaNiv1 != "Déplacement",]
relation <- subset(relation.dat, !(relation.dat$modaNiv1 == "hiérarchique asc. Ecole" | relation.dat$modaNiv1 == "hiérarchique ascendante") ) # on enleve les doublons
relation <- subset(relation, select =  c("idimplantation", "usual_name", "fklinked_implantation","linked_implantation_name", "modaNiv1", "lat", "lng", "date_startC","date_stopC", "idfactoid")) # on ne garde que les noms et noms liées
relation <- relation[!is.na(relation$lat),]
# Drop des facteurs- non pris en compte suite aux subset de relations
relation$usual_name <- factor(relation$usual_name)
relation$linked_implantation_name <- factor(relation$linked_implantation_name)
relation$modaNiv1 <- factor(relation$modaNiv1)

# 4. ajout de la distance ==============================================
# On construit un nouveau tableau qui contient les lat/long de chaque cote de la relation 
# l'objectif est de dessiner les liens sur une carte : de A -> B
# Hélène indique que les lat/long dans relations.dat sont celles de l'implantation (idimplantation) et pas celle lièe (fklinked_implantation).
# on va donc renomer les lat/lomg des implantations et le noms des implantations puis les joindre pour constituer un tableau avec les coords de A et de B.  

Relation_renomer <- dplyr::rename(relation,                                 # rename est pas mal utilisé donc on precise la library, il y a une dépendance sur dplyr
                                  idimpl_link=fklinked_implantation,            # ici on prend l'id lié
                                  usual_name_link=linked_implantation_name)     # idem pour le nom lié
  
# je vais decouper un peu le pipe d'Hélène avec une table produite pour la jointure 
# qui va comporter les lat/long des implantations liées

implantation_renomer <-  dplyr::select(implantation.dat, idimpl_link = idimplantation, # selon la doc on peut directement renomer dans un select
                                            lat_link = lat, 
                                            lng_link = lng, 
                                            Dioc_link = Diocese) 
# Jointure via idimpl_link
relation_total.dat <- dplyr::left_join(Relation_renomer, implantation_renomer, by = "idimpl_link")

# Suppression des fichiers intermediaires
rm(Relation_renomer, implantation_renomer)

# il y a des valeurs manquantes dans les lat/long de A (106) et B (107)
relation_total.dat <- dplyr::filter(relation_total.dat, !is.na(lat) & !is.na(lat_link)) # on retire les NA, c'est 60 relations

# On passe en sf 
# je m'inspire de cette solution : 
# https://stackoverflow.com/questions/58723988/create-numerous-lines-in-simple-features-from-list-of-coordinates-in-r

# matrice de départ
from.dat  <- as.matrix(                                          # on passe tout dans une matrice
                dplyr::select(relation_total.dat, lng, lat))     # on selectionne les lat long

#matrice d'arrivé
to.dat <- as.matrix( 
                dplyr::select(relation_total.dat, lng_link, lat_link) )

relation_total.dat$geometry <- do.call(sf::st_sfc,                                                             # on fait une fonction sfc
                                       lapply(                                                                 # on fait un apply sur chaque lignes
                                           1:nrow(relation_total.dat),
                                           function(i){                              
                                               sf::st_linestring(                                              # qui va tracer des lignes
                                                   matrix(                                                     # en prenant les points dans une matrices
                                                       c(from.dat[i,], to.dat[i,]), ncol=2, byrow=TRUE)        # composé du couple de point de départ et arrivé
                                               )}))

# Supression des deux matrices intermediaires
rm(from.dat,  to.dat)

# Pasage en en sf
relation_total.shp <- sf::st_transform(                                             # on va transformer en 2154
    sf::st_as_sf(relation_total.dat, sf_column_name = "geometry", crs = 4326),  # ici on fait onjet sf avec la colonne geometry et le crs de base qui était 4326
    2154)

# calcul de la distance
relation_total.shp$distance_km <- round(                                    # on va arrondir le résultats à 2 chiffres
                                    as.numeric(                             # je drop units, 
                                    sf::st_length(relation_total.shp)/1000), 0) # on passe en km



# petite description de ce que l'on charge

#ls.str() 

print("relation.dat est un subset de fait sur caracNew == Relations, on y a retirer les Déplacement")

print("relation est un subset plus leger de relation.dat retirant les relations hiérarchiques ascendantes, pour ne pas doublonner avec les descendantes. Relation comporte aussi moins de modalités")

print("implantation.dat correspond aux implantations")

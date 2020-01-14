# un script qui reprend toutes les fonctions et operations pour produire t0relation
# 14/01/2020
#c ol&mon


# Lecture des fichiers sources

implantation.dat <- readRDS("data/T0impl20191126.rds")
fait.dat <- readRDS("data/T0New20191126.rds")

# indexage est une liste contenant les variables pour indexer
# tout n'utilise malheureusement pas cet indexage
Indexage <- list(filtrerelation = "caracNew", # le premier item est juste caracNew
                 # il est utile pour filtrer relation
                 # ici c'est la liste de pas mal de variables utiles
                 selectrelation =  c("idimplantation", "usual_name", "fklinked_implantation","linked_implantation_name", 
                                     "modaNiv1", "lat", "lng", "date_startC","date_stopC"),
                 # le nom du debut et fin des relations
                 buffer = c("date_startC","date_stopC"),
                 # les identifiants de A ---- B
                 ident_relation = c("idimplantation", "fklinked_implantation"))   


# 1 - une fonction pour filtrer les relations =================

filtrer_relation_select <- function(T0new, selection = names(T0new)) {
    if(!is.data.frame(T0new)){stop("La fonction nécessite un tableau et non un.e", class(T0new) ,"." )}
    subset(fait.dat, fait.dat[[Indexage[["filtrerelation"]]]] == "Relations", select = selection) # au besoin peut être utiliser Indexage[[2]] pour :  selection = Indexage[[2]]
}


T0relation <- filtrer_relation_select(fait.dat, selection = Indexage[["selectrelation"]])

# 2. Renforcement du jeux de données T0News ====================

# 2.a Ajout de role ============================


T0relation$role <- NA
T0relation$role[T0relation$modaNiv1 == "hiérarchique ascendante"] <- "Dominé"
T0relation$role[T0relation$modaNiv1 == "hiérarchique descendante"] <- "Dominant"
T0relation$role[T0relation$modaNiv1 == "Relation horizontale"] <- "Égal"
T0relation$role[T0relation$modaNiv1 == "hiérarchique desc. Ecole"] <- "Dominant_ecole"

#2.b Calcul des degrés pour les difŕents reseaux =======

# ici on fait un identifiant pour chaque couple de liens et on filtre pour ne pas voir les doublons idimplantation --- fklinkedimplantation

T0relation <- T0relation %>% 
    mutate(lien_id = paste(idimplantation, fklinked_implantation)) %>% 
    distinct(lien_id, .keep_all = TRUE) %>% 
    select(-lien_id)


    
# comptage des liens par type de modaNiv1

hiérarchique_descendante <- subset(T0relation, T0relation$modaNiv1 == "hiérarchique descendante") %>% 
    group_by(idimplantation) %>% 
    summarize(degre_dominant = n()) 
Relation_horizontale <- subset(T0relation, T0relation$modaNiv1 == "Relation horizontale") %>% 
    group_by(idimplantation) %>% 
    summarize(degre_association = n()) 
degre_ecole <- subset(T0relation, T0relation$modaNiv1 == "hiérarchique desc. Ecole") %>% 
    group_by(idimplantation) %>% 
    summarize(degre_ecole = n())

T0relation <- T0relation %>%
    left_join(hiérarchique_descendante, by = "idimplantation") %>% 
    left_join(Relation_horizontale, by = "idimplantation") %>% 
    left_join(degre_ecole, by = "idimplantation" )

rm(hiérarchique_descendante, Relation_horizontale, degre_ecole)

# 2.c Calculer la distance entre deux implantations liées (pour la portée) ==============================================

distance_entre_implantation <- function(relation, selection = names(relation)){
    
    # chargement des packages, pe cela n'est pas utile 
    if(require("dplyr") == FALSE)  
        install.packages("dplyr",  dependencies=c("Depends", "Suggests"))
    if(require("sf") == FALSE)  
        install.packages("sf",  dependencies=c("Depends", "Suggests"))
    
    # un subset au besoin    
    relation.temp <- subset(relation, select = selection)
    
    Relation_renomer <- dplyr::rename(relation.temp,                                     # rename est pas mal utilisé donc on precise la library, il y a une dépendance sur dplyr
                                      idimpl_link=fklinked_implantation,                 # ici on prend l'id lié
                                      usual_name_link=linked_implantation_name)          # idem pour le nom lié
    
    # je vais decouper un peu le pipe d'Hélène avec une table produite pour la jointure 
    # qui va comporter les lat/long des implantations liées
    implantation_renomer <-  dplyr::select(relation.temp, idimpl_link = idimplantation, # selon la doc on peut directement renomer dans un select
                                           lat_link = lat, 
                                           lng_link = lng) 
    
    # Jointure via idimpl_link
    relation_total.dat <- dplyr::left_join(Relation_renomer, implantation_renomer, by = "idimpl_link")
    
    # Suppression des fichiers intermediaires
    rm(Relation_renomer, implantation_renomer)
    
    # si il y a des valeurs manquantes on les drop
    relation_total.dat <- dplyr::filter(relation_total.dat, !is.na(lat) & !is.na(lat_link)) # pourra être supprimer quand la base sera "propre"
    
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

T0relation <- distance_entre_implantation(T0relation)

saveRDS(T0relation, file = "data/T0relation.rds")

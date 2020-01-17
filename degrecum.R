##### DegreCum


# 1 - chargement des rds =================
T0relation <- readRDS("data/T0relation.rds")
# on vire les deplacements
T0relation <- T0relation[!T0relation$modaNiv1 == "Déplacement",]

# la fonction prend un niveau hierarchique par defaut
# relation correspond à un T0relation
# on a besoin d'igraph
# ValmodaNiv1 prend une ou plusieurs valeur de modaNiv1 pris dans T0relation$modaNiv1
# il faut igraph (>= v1), sf et dplyr

degreCum_modaNiv1 <- function(relation, niveau_hierarchique = 10000, ValmodaNiv1) {
    # on drop la geometry
    relation <- st_drop_geometry(relation) %>%
        filter( !is.na(fklinked_implantation) & modaNiv1 == ValmodaNiv1) %>%
        select(idimplantation, fklinked_implantation)
        # on fait le graph et on le siplifie
    graph_relation <- simplify(graph.data.frame(relation, 
    # ici si on est en "Relation horizontale" on est dans un graph non dirigé sinon dirigé
                                                directed = ifelse(ValmodaNiv1 == "Relation horizontale", FALSE, TRUE))) 
    # simplify=mal absolu car par defaut ne garde que la première ligne du doublon
    
    # attention fonction à partir de igraph 1.0
    voisin <- make_ego_graph(graph_relation, order = niveau_hierarchique, mode = "out")
    # un vecteur des idimplantation
    idimplantation <- V(graph_relation)$name
    # la somme des degrés pour chaque id pour pleins de voisins, ici 10000
    degreCum <- sapply(voisin, vcount)
    # on a besoin de modaNiv1
    modaNiv1 <- rep(ValmodaNiv1, length(idimplantation))
    return(data.frame(idimplantation, modaNiv1, degreCum))
}

test <- unique(T0relation$modaNiv1)

ifelse(test == "Relation horizontale", FALSE, TRUE)

relation <- T0relation

# degreCum ne prend que relation = t0relation filtré

degreCum <- function(relation) {

for(i in 1:length(unique(relation$modaNiv1))) {
  ifelse(i == 1, 
            # on initialise si i == 1
            degre_cum_modaNiv1 <- degreCum_modaNiv1(relation, ValmodaNiv1 = unique(relation$modaNiv1)[i]), 
            # puis on ajoute
            degre_cum_modaNiv1 <- bind_rows(degre_cum_modaNiv1,
                      degreCum_modaNiv1(relation, ValmodaNiv1 = unique(relation$modaNiv1)[i])))
}
     return(degre_cum_modaNiv1)
}

# 2 - pour tester ================= 

unique(T0relation$modaNiv1)

bob <- degreCum(T0relation)

# verif
bob %>% 
    filter(modaNiv1 == "Relation horizontale") %>% 
    arrange(degreCum)
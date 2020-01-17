# fonction voisinage local

# 1 - chargement des rds =================
T0relation <- readRDS("data/T0relation.rds")
# on vire les deplacements
T0relation <- T0relation[!T0relation$modaNiv1 == "Déplacement",]

unique(T0relation$modaNiv1)

relation <- T0relation[T0relation$idimplantation == 99,]
#ValmodaNiv1 <- "hiérarchique descendante"

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



comps <- decompose.graph(graph_ensemble_simplify)

# sauve la composante connexes du graph
V(graph_ensemble_simplify)$comps <- as.numeric(membership(components(graph_ensemble_simplify)))


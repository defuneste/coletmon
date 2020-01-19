# fonction voisinage local

# 1 - chargement des rds =================
T0relation <- readRDS("data/T0relation.rds")
# on vire les deplacements
T0relation <- T0relation[!T0relation$modaNiv1 == "Déplacement",]

unique(T0relation$modaNiv1)

relation <- T0relation
relation_graph <- relation %>% 
  st_drop_geometry() %>% 
  filter( !is.na(fklinked_implantation)) %>%  # au cas ou on a des NA
  select(idimplantation, fklinked_implantation, usual_name,  modaNiv1)

graph_relation <- simplify(graph.data.frame(relation_graph, 
                                            # ici si on est en "Relation horizontale" on est dans un graph non dirigé sinon dirigé
                                            directed = FALSE)) 

un_graph <- graph_relation
un_id <- 99

V(graph_relation)[name == 99]

graph_a_partir_id <- function(un_id, un_graph = graph_relation) {
  # on calcul les composantes connexes 
  V(un_graph)$comps <- as.numeric(membership(components(un_graph)))
  # on produit un sous graphes qui prends tous les noeuds de la composantes connexes 
  # il faut que les vertexes soit nommés avec "name" puisque j'indexe dessus 
  un_sous_graph <- induced_subgraph(un_graph, 
                                    vids = V(un_graph)[comps == V(un_graph)[name == un_id]$comps])
  return(un_sous_graph)
}
  

cluny <- graph_a_partir_id(99)

neighbors(cluny, "99")

length(as_ids(get_diameter(cluny)))

for(i in 1:length(as_ids(get_diameter(cluny)))) {
ifelse(i == 1, 
    idimplantation <- as_ids(ego(cluny, order = 1 , "99")[[1]])
    niveau <- rep(1, length(idimplantation))
bob <- data.frame(test, niv), )

test <- as_ids(ego(cluny, order = 2 , "99")[[1]])
niv <- rep(2, length(test))
bill <- data.frame(test, niv)

test <- as_ids(ego(cluny, order = 3 , "99")[[1]])
niv <- rep(3, length(test))
jim <-data.frame(test, niv)

test <- bind_rows(bob, bill)   %>% bind_rows(jim) %>% distinct(test, .keep_all = TRUE)

}



un_sous_graph <- cluny

graphjs(un_sous_graph,
        vertex.label = paste(V(un_sous_graph)$usual_name, V(un_sous_graph)$name), # il faut usual name
        vertex.size = 0.2, # pe modifier par degree
        brush=TRUE)



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

# fonction voisinage local
library(dplyr)
library(sf)
library(igraph)
library(threejs)


# 1 - chargement des rds =================
T0relation <- readRDS("data/T0relation.rds")
# on vire les deplacements
T0relation <- T0relation[!T0relation$modaNiv1 == "Déplacement",]

## un fonction pour faire des graph à partir d'une idimplantation et un graph 
# la fonction à besoin d'un idimplantation
# et d'un graph, par defaut elle prendre un objet graph_relation
# elle retourne un objet igraph
# elle utilise les composantes connexes et attribue un nombre à chacune d'entre elles 
# puis elle va produire un sous graph en indexant les vertex par la composante connexes d'apartenance de l'id
# elle necessite igraph 

graph_a_partir_id <- function(un_id, un_graph = graph_relation) {
  # on calcul les composantes connexes 
  V(un_graph)$comps <- as.numeric(membership(components(un_graph)))
  # on produit un sous graphes qui prends tous les noeuds de la composantes connexes 
  # il faut que les vertexes soit nommés avec "name" puisque j'indexe dessus 
  un_sous_graph <- induced_subgraph(un_graph, 
                                    vids = V(un_graph)[comps == V(un_graph)[name == un_id]$comps])
  return(un_sous_graph)
}
  


## une fonction pour obtenir un tableau d'idimplantations à partir de T0relation filtré et du niveau
## la fonction necessite la fonction graph_a_partir_id et igraph
# elle prend relation = T0relation filtré
# une idimplantation
# un niveau

voisinage_local_opt1 <- function(relation, idimplantation_saisie, niveau) {

# ici on allége T0relation et on evite des potentiels NA
relation_graph <- relation %>% 
  st_drop_geometry() %>% 
  filter( !is.na(fklinked_implantation)) %>%  # au cas ou on a des NA
  select(idimplantation, fklinked_implantation, usual_name,  modaNiv1)

# on fait un graph
graph_relation <- simplify(graph.data.frame(relation_graph, 
                                            # ici si on est en "Relation horizontale" on est dans un graph non dirigé sinon dirigé
                                            directed = FALSE)) # à noter on est en non dirigé
# la j'avoue, c'est un peu imbriqué mais la majorité est pour retourner le bon format
# as.numerics et as_ids ne servent que pour retourner les idimplantation en numeric
# on genere un sous graph directement via un appel de graph_a_partir_id
# sur lequel on va demander toutes les idimplantation  à un niveau
# si niveau = 3, ego va retourner les idimplantation de niveau 1,2 et 3
idimplantation <- as.numeric(as_ids(ego(graph_a_partir_id(idimplantation_saisie, graph_relation), 
                                        order = niveau , as.character(idimplantation_saisie))[[1]]))
# la suite est juste la constitution d'un df avec le niveau repeté et l'id
niveau <- rep(niveau, times = length(idimplantation))
return(data.frame(idimplantation, niveau))

}

#pour tester
relation <- T0relation
idimplantation_saisie <- 99
niveau <- 15

voisinage_local_opt1(T0relation, 99, 2)

#### voisinage_local_opt2 ===============================
# cette fonction produit pour un TOrelation et une idimplantation saisie un tableau avec :
# idimplantation et niveau, ici niveau correspond au niveau le plus cours connectant à l'idimplantation saisie
# cette fonction nécessite graph_a_partir_id() et voisinage_local_opt1()
# elle utilise igraph et dplyr

idimplantation_saisie <- 99
relation <- T0relation

voisinage_local_opt2 <- function(relation, idimplantation_saisie) {

## ici j'ai besoin du graph de la composantes connexes pour obtenir le diamètre
# c'est un peu redondant avec voisinage_local_opt1
    
relation_graph <- relation %>% 
  st_drop_geometry() %>% 
  filter( !is.na(fklinked_implantation)) %>%  # au cas ou on a des NA
  select(idimplantation, fklinked_implantation, usual_name,  modaNiv1)

# on fait un graph
graph_relation <- simplify(graph.data.frame(relation_graph, 
                                            # ici si on est en "Relation horizontale" on est dans un graph non dirigé sinon dirigé
                                            directed = FALSE)) # à noter on est en non dirigé
# on peut gagner en objet intermediaire en passant directement graph_relation dans graph_a_partir_id
# mais on perds en lisbilité
sous_graph <- graph_a_partir_id(idimplantation_saisie, graph_relation)

# on initialise la list
une_list <- list()

# chaque df par niveau va aller dans une_list 
for(i in 1:length(as_ids(get_diameter(sous_graph)))) {
  une_itération <- voisinage_local_opt1(relation, idimplantation_saisie, i)
  une_list[[i]] <- une_itération 
  }

# on va retourner
# on ajoute les differents tableaux
return(do.call(rbind, une_list) %>%
           # on filtre en ne gardant que la première valeur d'idimplantation
            dplyr::distinct(idimplantation, .keep_all = TRUE))
}


#### pour tester 

voisinage_local_opt2(T0relation, 99)

# le diamétre est de 15 mais avec 8 niveau on arrive à avoir toutes les implantations
table(voisinage_local_opt2(T0relation, 99)$niveau)
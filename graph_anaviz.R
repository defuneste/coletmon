# un script un graph faire un graph
# 14/01/2020
# col&mon

library(igraph)
library(threejs)
library(sf)
library(dplyr)
library(readxl)


# 1 - chargement des rds =================
T0relation <- readRDS("data/T0relation.rds")
unique(T0relation$modaNiv1)
# les couleurs : 

CaracHist <-readxl::read_excel("data/NewModelCaracModalitesColor5.xlsx",
                              sheet = "color")
ColRelations <- filter(CaracHist, caracNew == "Relations") %>% 
                    select(modaNiv1, modaNiv1_Color)


# on charge degrecum.R
source("degrecum.R")

# je vire les deplacements car cela pose pb 
T0relation <- T0relation[!T0relation$modaNiv1 == "Déplacement",]
T0relation$role[is.na(T0relation$role)] <- "Ecole"

# # A l'initialisation de l'onglet on est en modaNiv1 = "hiérarchique descendante"
T0relation <- T0relation[T0relation$modaNiv1 == "hiérarchique descendante" | T0relation$modaNiv1 == "hiérarchique asc. Ecole",]
relation <- T0relation

dessine_moi_un_graph <- function(relation) {

    # if (class(relation) == "sf") {
    #     relation <- st_drop_geometry(relation)
    # }
    
# il me faut une table ID - NOM  pour caractériser les noeuds avec leurs noms et idimplantation en visualisation
# tant qu'a faire j' ai repris le degré de degrecum, l'avantage c'est que c'est consistant le désavantage c'est que cela n'affichera pas les bon degres
# si on a plus d'une modaNiv1

partA <- relation %>% 
    st_drop_geometry() %>% 
    select(idimplantation, usual_name)

idimpl_nom <- relation %>% 
    st_drop_geometry() %>% 
    select(idimplantation = fklinked_implantation, usual_name = usual_name_link) %>% 
    bind_rows(partA) %>% 
    distinct(idimplantation, .keep_all = TRUE) %>% 
    left_join(degreCum(relation), by = "idimplantation") ### <- c'est la le degreCum ==========

rm(partA)

# ici c'est pour alleger le graph on peut au besoin ne pas garder et n'utiliser que T0relation
# il faut alors en tenir compte dans vertex_v1

relation_graph <- relation %>% 
    st_drop_geometry() %>% 
    filter( !is.na(fklinked_implantation)) %>%  # au cas ou on a des NA
    select(idimplantation, fklinked_implantation, usual_name,  modaNiv1) %>% 
    # On rajoute les couleurs
    left_join(ColRelations, by = "modaNiv1")

# permet d'ordonner, ne garder que les premiers des vertexes
vertex_v1 <-  idimpl_nom[match(unique(c(relation_graph$idimplantation, relation_graph$fklinked_implantation)), 
                               idimpl_nom$idimplantation),]

# on fait le graph
graph_modaNiv1_simplify <- graph.data.frame(relation_graph, 
                                            directed = TRUE, # ici on est dans un graph dirigé / ou non dirigé en fonction
                                            vertices = vertex_v1 ) %>% 
                            igraph::simplify(edge.attr.comb = "first") # mal absolu, ici j'ai mis first pour avoir la couleur, ce qui ne devrait pas changer

# je garde au cas ou on se pose des questions sur la couleur des vertex sinon cela peut sauter
# V(graph_modaNiv1_simplify)$colorV <- ifelse(V(graph_modaNiv1_simplify)$modaNiv1 == "hiérarchique descendante" | V(graph_modaNiv1_simplify)$modaNiv1 == "hiérarchique desc. Ecole", "blue",
#                                   ifelse(V(graph_modaNiv1_simplify)$modaNiv1 == "hiérarchique ascendante", "red", 
#                                          ifelse(V(graph_modaNiv1_simplify)$modaNiv1 == "Relation horizontale", "forestgreen", "black")))

V(graph_modaNiv1_simplify)$shape <- ifelse(V(graph_modaNiv1_simplify)$modaNiv1 == "hiérarchique asc. Ecole" , "square", "circle")

# la fonction de dessin du graph
graphjs(graph_modaNiv1_simplify,
        # pour que le nom apparaisse quand on passe dessus
        vertex.label = paste(V(graph_modaNiv1_simplify)$usual_name, V(graph_modaNiv1_simplify)$name), # il faut usual name
        vertex.color = rep("gray", vcount(graph_modaNiv1_simplify)),
        vertex.size = log(vertex_v1$degreCum + 1)/5, # ici il y a pe une amelioration à faire max(log(vertex_v1$degreCum))
        vertex.shape = V(graph_modaNiv1_simplify)$shape,
        edge.color = E(graph_modaNiv1_simplify)$modaNiv1_Color,
        edge.width = 1.25, # a modifire en fonction de l'affichage
        brush=TRUE)

}

# 2 - pour tester ================= 

dessine_moi_un_graph(T0relation)


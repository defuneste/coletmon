# un script un graph pour les relations
# 14/01/2020
# col&mon

library(igraph)
library(threejs)
library(sf)
library(dplyr)

T0relation <- readRDS("data/T0relation.rds")

#implantation.dat <- readRDS("data/T0impl20191126.rds")

# je vire les deplacements car cela pose pb 
# T0relation[ ! T0relation$idimplantation %in% T0relation$fklinked_implantation,   ]
# unique(T0relation$modaNiv1)

T0relation <- T0relation[!T0relation$modaNiv1 == "Déplacement",]
T0relation$role[is.na(T0relation$role)] <- "Ecole"

table(T0relation$role, T0relation$modaNiv1)

# me faut une table ID - NOM

partA <- T0relation %>% 
    st_drop_geometry() %>% 
    select(idimplantation, usual_name)

idimpl_nom <- T0relation %>% 
    st_drop_geometry() %>% 
    select(idimplantation = fklinked_implantation, usual_name = usual_name_link) %>% 
    bind_rows(partA) %>% 
    distinct(idimplantation, .keep_all = TRUE)

# un exemple de filtre
T0relation <- T0relation[T0relation$modaNiv1 == "Relation horizontale",]

vertex <- st_drop_geometry(T0relation)

relation_graph <- subset(vertex , fklinked_implantation != "NA",  #il y a une valeur manquante
                         select = c(idimplantation, fklinked_implantation, usual_name,  modaNiv1))


# dans les cas ou modaNiv1 == "Relation horizontale"

idimplantation <- unique(c(relation_graph$idimplantation, relation_graph$fklinked_implantation))
role <- rep("Égal", length(idimplantation))
implantation <- data.frame(idimplantation, role) %>% 
    left_join(idimpl_nom, by = "idimplantation")

# dans les cas ou modaNiv1 == "hiérarchique descendante"



# length(unique(relation_graph$idimplantation))
# 
# length(unique(relation_graph$fklinked_implantation))

# il me faut une option pour dessiner les vertex à partir des relations 
# length(unique(c(relation_graph$idimplantation, relation_graph$fklinked_implantation)))
# ou alors il me faut un moyen pour reconstituer les vertexes à prtir de la sélection

vertex_v1 <-  implantation[match(unique(c(relation_graph$idimplantation, relation_graph$fklinked_implantation)), 
             implantation$idimplantation),]


# dim(vertex_v1)

graph_relation <- graph.data.frame(relation_graph, 
                                   directed = TRUE, # ici on est dans un graph dirigé / ou non dirigé en fonction
                                   vertices = vertex_v1 )
V(graph_relation)
V(graph_relation)$role

graph_modaNiv1_simplify <- simplify(graph_relation) # mal absolu 

niveau_hierarchique <- 2 # 
voisin <- make_ego_graph(graph_modaNiv1_simplify, order = niveau_hierarchique, mode = "out") # attention la fonction evolue vers ego_size
sapply(voisin, vcount)

V(graph_modaNiv1_simplify)$colorV <- ifelse(V(graph_modaNiv1_simplify)$role == "Dominant" | V(graph_modaNiv1_simplify)$role == "Dominant_ecole", "blue",
                                  ifelse(V(graph_modaNiv1_simplify)$role == "Dominé", "red", 
                                         ifelse(V(graph_modaNiv1_simplify)$role == "Égal", "forestgreen", "black")))

V(graph_modaNiv1_simplify)$shape <- ifelse(V(graph_modaNiv1_simplify)$role == "Ecole" , "square", "circle")

graphjs(graph_modaNiv1_simplify,
        vertex.label = paste(V(graph_modaNiv1_simplify)$usual_name, V(graph_modaNiv1_simplify)$name), # il faut usual name
        vertex.color = V(graph_modaNiv1_simplify)$colorV,
        vertex.size = log(sapply(voisin, vcount))/5,
        vertex.shape = V(graph_modaNiv1_simplify)$shape,
        #edge.color = E(un_sous_graph)$colorW, 
        brush=TRUE)


graphjs(graph_relation,
        #vertex.label = paste(V(graph_relation)$usual_name, V(graph_relation)$name), # il faut usual name
        #vertex.color = V(graph_modaNiv1_simplify)$colorV,
        vertex.size = 0.2, # pe modifier par degree
        #vertex.shape = "square",
        #edge.color = E(un_sous_graph)$colorW, 
        brush=TRUE)




mise_enplace_graph <- function(relation, implantation = NULL ) {
    # verification qu igraph est bien présent
    if(require("igraph") == FALSE)  
        install.packages("igraph",  dependencies=c("Depends", "Suggests"))
    # les relations 
    relation_graph <- subset(relation, fklinked_implantation != "NA",  #il y a une valeur manquante
                             select = c(idimplantation, fklinked_implantation))
    # les implantations 
    # filtre implantation pour ordonner et ne garder que les implantations avec des relations
    if(is.null(implantation))  { implantation_vertex <- NULL } else {
        implantation_vertex <- implantation[match(unique(c(relation_graph$idimplantation, relation_graph$fklinked_implantation)), 
                                                  implantation$idimplantation),]}
    
    graph_relation <- graph.data.frame(relation_graph, 
                                       directed = FALSE,
                                       vertices = implantation_vertex )
}

mise_enplace_graph(T0relation, implantation.dat)





# un script un graph pour les relations
# 14/01/2020
# col&mon

library(igraph)
library(threejs)

T0relation <- readRDS("data/T0relation.rds")

is.simple(bob)



vertex <- st_drop_geometry(T0relation)


#vertex <- vertex[vertex$modaNiv1 == "hiérarchique descendante",]

relation_graph <- subset(vertex , fklinked_implantation != "NA",  #il y a une valeur manquante
                         select = c(idimplantation, fklinked_implantation))

length(unique(relation_graph$idimplantation))

length(unique(relation_graph$fklinked_implantation))


vertex_v1 <-  vertex[match(unique(c(relation_graph$idimplantation, relation_graph$fklinked_implantation)), 
             vertex$idimplantation),]

dim(vertex_v1)

# il me faut une option pour dessiner les vertex à partir des relations 
length(unique(c(relation_graph$idimplantation, relation_graph$fklinked_implantation)))


sum( ! relation_graph$fklinked_implantation %in% relation_graph$idimplantation )

sum(xor(relation_graph$fklinked_implantation, relation_graph$idimplantation))

T0relation[T0relation$idimplantation == 3095,]

graph_relation <- graph.data.frame(relation_graph, 
                                   directed = FALSE,
                                   vertices = vertex_v1 )

graph_relation <- graph.data.frame(relation_graph, 
                                   directed = FALSE)

is.simple(graph_relation)


graph_modaNiv1_simplify <- simplify(graph_relation) # mal absolu


graph_relation <- bob

unique(T0relation$role)

V(graph_modaNiv1_simplify)$colorV <- ifelse(V(graph_modaNiv1_simplify)$role == "Dominant", "blue",
                                  ifelse(V(graph_modaNiv1_simplify)$role == "Dominé", "red", 
                                         ifelse(V(graph_modaNiv1_simplify)$role == "Égal", "forestgreen", "black")))

graphjs(graph_relation,
        vertex.label = paste(V(graph_relation)$usual_name, V(graph_relation)$name), # il faut usual name
        vertex.color = V(graph_modaNiv1_simplify)$colorV,
        vertex.size = 0.2, # pe modifier par degree
        vertex.shape = "square",
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

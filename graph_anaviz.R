# un script un graph pour les relations
# 14/01/2020
# col&mon

library(igraph)
library(threejs)

T0relation <- readRDS("data/T0relation.rds")


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

bob <- mise_enplace_graph(T0relation, implantation.dat)

graph_relation <- bob

graphjs(graph_relation,
        vertex.label = paste(V(graph_relation)$usual_name, V(graph_relation)$name), # il faut usual name
        #vertex.color = V(un_sous_graph)$colorV, # il faut colorV
        vertex.size = 0.2, # pe modifier par degree
        #edge.color = E(un_sous_graph)$colorW, 
        brush=TRUE)


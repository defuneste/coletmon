##### DegreCum


# 1 - chargement des rds =================
T0relation <- readRDS("data/T0relation.rds")
# on vire les deplacements
T0relation <- T0relation[!T0relation$modaNiv1 == "Déplacement",]

# la fonction prend un niveau hierarchique par defaut
# relation correspond à un T0relation
# on a besoin d'igraph

degreCum <- function(relation, niveau_hierarchique = 1) {
    if(require("igraph") == FALSE)  
        install.packages("igraph",  dependencies=c("Depends", "Suggests"))
    if(require("sf") == FALSE)  
        install.packages("igraph",  dependencies=c("Depends", "Suggests"))
    # on drop la geometry
    vertex <- st_drop_geometry(relation)
    # ici c'est un subest pour alléger et virer des NA, il ne devrait plus y en avoir mais bon ...
    relation_graph <- subset(vertex, fklinked_implantation != "NA",  select = c(idimplantation, fklinked_implantation))
    # on fait le graph et on le siplifie
    graph_relation <- graph.data.frame(relation_graph, directed = TRUE)
    graph_modaNiv1_simplify <- simplify(graph_relation) # mal absolu car par defaut ne garde que la première ligne du doublon
    # attention fonction à partir de i graph 1.0
    voisin <- make_ego_graph(graph_modaNiv1_simplify, order = niveau_hierarchique, mode = "out") 
    idimplantation <- V(graph_modaNiv1_simplify)$name
    degre <- sapply(voisin, vcount)
    data.frame(idimplantation, degre)
}

# 2 - pour tester ================= 

unique(T0relation$modaNiv1)

T0relation <- T0relation[T0relation$modaNiv1 == "hiérarchique asc. Ecole",]

head(degreCum(T0relation, 0), 20)
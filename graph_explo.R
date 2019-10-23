# script d'exploration des données pour le projet col&mon
# 21-10-2019
# Attention les liens relatifs sont pour un os linux !!!!
# norme de codage :
# .dat est un df ou tible
# .shp est un sf
# Il faut definir des régles de nommage pour les graphs car cel va vite 
# devenir bordelique
##.###################################################################################33
## I. Chargement des données de col&mon ====
##.#################################################################################33

# Premiers graphes sur les relations

# ## En prenant le tout
# 
# ```{r premier_graph, message=FALSE}
# library(igraph)
# relation <- relation.dat[relation.dat$modAgreg != "A",] # on enleve les doublons
# # on ne garde que les noms
# relation <- subset(relation, select =  c("usual_name", "linked_implantation_name"))
# # objet de graphs
# graph_ensemble <- graph.edgelist(as.matrix(relation), directed = FALSE)
# ```
# 
# Attention `igraph` masque et utilise des fonctions de `dplyr` et `base` comme `as_data_frame`, `groups`, `union`.
# 
# On obtient un graphe peut lisible avec `r gsize(graph_ensemble)` liens et `r gorder(graph_ensemble)` noeuds.
# 


##.###################################################################################33
## III. Graphs ====
##.#################################################################################33

## 1 - Vertex/hedge ================
# size, label, color, and shape sont les ajustements les plus freuents sur un reseau
# qqs regles
# éviter les croissement de liens
# éviter les superposition de noeud
# faire les liens le plus uniforme
# augmenter la symétrie du réseau le plus possible
# mettre les noeuds les plus influents au centre
# dans igraph il y a plusieurs layout, definit par un argument du meme nom 
# ex : circle, tree, etc
# ex : plot(g1, vertex.label.color = "black", layout = layout_in_circle(g1))
# on peut aussi sauver le layout dans un objet : 
# m <- layout_as_tree(g1)
# plot(g1, vertex.label.color = "black", layout = m)
# delete_edges() which takes two arguments.
# The first is the graph object and the second is the subset of edges to be removed
# dans notre cas on a une direction : DN network 
# donc les degree sont outdegree et in degree
# lien entre deux : g["X", "Y"] vu que cela est une sorte de matrice
# incident() est une fonction qui va regarder toutes les liens d'un noeud
# head_of() va chercher les noeuds d'orgine d'un graph sur une selection 
# neighbors(g ,"F", mode = c("all")) retourne tout voisins de F dans le grapg g 
# paths est le chemin pour connecter des points, ont peu le mesurer
# le chemin le plus long est nommer le diamètre du réseau
# ego(g, 2, "F", mode=c("out")) retourne tous les vertex qui sont à n liens du noeud en fonction de in/out/all 
# betweenness. This is an index of how frequently the vertex lies on shortest paths between any two vertices in the network.s

#on va viltrer pour n'avoir que deux colonnes de relation

library(igraph)

relation <- relation.dat[relation.dat$modAgreg != "A",] # on enleve les doublons
dim(relation) # verif
names(relation) 
# one ne garde que les noms
relation <- subset(relation, select =  c("usual_name", "linked_implantation_name"))
# objet de graphs
graph_ensemble <- graph.edgelist(as.matrix(relation), directed = FALSE)

# oh que c'est de moins en moins laid
plot(graph_ensemble, 
     vertex.size = 0,
     vertex.label.cex = 0.2,
     layout = layout_nicely(graph_ensemble))

graph_ensemble.b <- betweenness(graph_ensemble, directed = TRUE)

plot(graph_ensemble, 
     vertex.label = NA,
     edge.color = 'black',
     vertex.size = log(graph_ensemble.b)+1,
     edge.arrow.size = 0.05,
     layout = layout_nicely(graph_ensemble))

# retourne le chemin le plus long dans le graph
farthest_vertices(graph_ensemble)
# et ici retourne le chemin pris
get_diameter(graph_ensemble)

# Subset vertices and edges
V(graph_ensemble)
E(graph_ensemble)

# compte le nombre de liens
gsize(graph_ensemble)

# compte le nombre de noeuds
gorder(graph_ensemble)

# une indexation sur les noeux avec "cîteaux (2)"
E(graph_ensemble)[[inc("Cîteaux (2)")]]

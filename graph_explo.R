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


# 1 - chargement des librairies =======

library(tidyr)
library(dplyr) # manip données
library(ggplot2) #  graphiques
library(plotly) # graphiques un peu interactif
library(igraph) # graph
library(threejs) # APi de js pour les graphs

# 2 - Imports des données =======

fait.dat <- read.csv("data/fait.txt")
implantation.dat <- read.csv("data/implantation.txt")
fait.dat <- subset(fait.dat, select = - X)
implantation.dat <- subset(implantation.dat, select = - X)

# 3 - Mise en forme =======

relation.dat <- fait.dat[fait.dat$caracNew == "Relations" ,] # on ne garde que les relations
relation <- relation.dat[relation.dat$modAgreg != "A",] # on enleve les doublons
relation <- subset(relation, select =  c("idimplantation", "usual_name", "fklinked_implantation","linked_implantation_name")) # on ne garde que les noms et noms liées
# on drop les facteurs non pris en compte suite aux subset de relations
relation$usual_name <- factor(relation$usual_name)
relation$linked_implantation_name <- factor(relation$linked_implantation_name)

##.###################################################################################33
## II. Graphs ====
##.#################################################################################33

## 0 - Vertex/hedge ================
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
# assortativity est interessante sur les ordres
# fastgreedy.communauty pour la detection de communauté 
# regarder threejs

### 1 - stats ================

# combien a t on d'implantations avec au moins une relations
length(unique(relation$idimplantation))

length(unique(relation$fklinked_implantation))

# une solution plyresque pour/puis vérifier avec igraph
relation %>% 
    group_by(idimplantation) %>% # on groupe par usual name
    summarize(nb = n()) %>%  # on compte par ce group
    arrange(desc(nb)) %>% # on passe en decroissant
    left_join(subset(implantation.shp, select = c(idimplantation, usual_name)),by = "idimplantation")
    ggplot() +
        geom_bar(aes(nb)) + 
        labs(x = "", y = "") +
        theme_bw()

dim(relation.dat[relation.dat$idimplantation == 1100,])

implantation.dat[implantation.dat$idimplantation == 1293, ] 
### 2 - Graphs non orienté  ================ 

# objet de graphs

relation <- subset(relation, select = c(idimplantation, fklinked_implantation))

graph_relation <- graph.edgelist(as.matrix(relation), directed = FALSE)

is_simple(graph_relation) # on a plusieurs liens pour un meme couple de noeud


#which_multiple retoune les liens doubles, un vecteur F/T
relation[which_multiple(graph_relation),]

# which_loop retourne les boucles : liens de noeuds à noeuds
relation[which_loop(graph_relation),] # on a aussi une loop 

length(unique(relation$idimplantation[which_multiple(graph_relation)]))
length(relation$idimplantation[which_multiple(graph_relation)])

E(graph_relation)$weight <- 1
graph_ensemble_simplify <- simplify(graph_relation, edge.attr.comb = "sum")

is_simple(graph_ensemble_simplify) 

sum(E(graph_ensemble_simplify)$weight > 1)

# oh que c'est de moins en moins laid
 plot(graph_relation)


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

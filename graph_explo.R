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

library(tidyr)
library(dplyr) # manip données
library(ggplot2) #  graphiques
library(plotly) # graphiques un peu interactif
library(sf)
library(igraph) # graph
library(threejs) # APi de js pour les graphs
library(colortools) # couleur

#chargement des données

source("chargement_graphs.R")

##.###################################################################################33
## II. Graphs ====
##.#################################################################################33

## 0 - Vertex/hedge ================
# faire les liens le plus uniforme
# augmenter la symétrie du réseau le plus possible
# mettre les noeuds les plus influents au centre
# dans igraph il y a plusieurs layout, definit par un argument du meme nom 
# ex : circle, tree, etc
# ex : plot(g1, vertex.label.color = "black", layout = layout_in_circle(g1))
# on peut aussi sauver le layout dans un objet : 
# dans notre cas on a une direction : DN network 
# donc les degree sont outdegree et in degree
# lien entre deux : g["X", "Y"] vu que cela est une sorte de matrice
# incident() est une fonction qui va regarder toutes les liens d'un noeud
# head_of() va chercher les noeuds d'orgine d'un graph sur une selection 
# neighbors(g ,"F", mode = c("all")) retourne tout voisins de F dans le grapg g 
# paths est le chemin pour connecter des points, ont peu le mesurer
# le chemin le plus long est nommer le diamètre du réseau
# ego(g, 2, "F", mode=c("out")) retourne tous les vertex qui sont à n liens du noeud en fonction de in/out/all 
# . This is an index of how frequently the vertex lies on shortest paths between any two vertices in the network.s
# assortativity est interessante sur les ordres
# fastgreedy.communauty pour la detection de communauté 
# regarder threejs

### 1 - stats ================

# combien a t on d'implantations avec au moins une relations
length(unique(relation$idimplantation))

length(unique(relation$fklinked_implantation))

# une solution plyresque pour/puis vérifier avec igraph

implantation.shp <- st_as_sf(implantation.dat[!is.na(implantation.dat$lat),], coords = c("lng", "lat"), crs = 4326)
# de wgs 84 au lambert 93
implantation.shp <- st_transform(implantation.shp, 2154) 

idimplant_court <- subset(implantation.dat, select = c(idimplantation, usual_name))

implantation_relation <- relation %>% 
    group_by(idimplantation) %>% # on groupe par usual name
    summarize(nb = n()) %>%  # on compte par ce group
    arrange(desc(nb)) %>% # on passe en decroissant
    left_join(idimplant_court,by = "idimplantation")

ggplot(implantation_relation) +
        geom_bar(aes(nb)) + 
        labs(x = "", y = "") +
        theme_bw()

### 2 - Graphs non orienté  ================ 

# préparation des données

# je prefere des caracteres donc autant garder une syntaxe V pour vertex
relation_graph <- subset(relation, select = c(idimplantation, fklinked_implantation, modaNiv1, idfactoid))
# rajout du modaNiv1 suite à update de DB du 2019/11/26
relation_graph$modaNiv1 <- factor(relation_graph$modaNiv1)
relation_graph$idimplantation <-  paste0("V", relation_graph$idimplantation)
relation_graph$fklinked_implantation <- paste0("V", relation_graph$fklinked_implantation)

implantation.dat$name <- paste0("V", implantation.dat$idimplantation)

# on garde pas tout, il est important que la première colonne contienne les noms de vertex cf. help(grap.data.frame)
implantation.dat <- implantation.dat[!is.na(implantation.dat$lat),]
implantationVertex.dat <- subset(implantation.dat, select = c(name, usual_name, date_startC_Fact ,  date_stopC_Fact ,  DureeSFact, lat, lng) )
length(unique(implantationVertex.dat$name))

names(implantation.dat)
names(implantationVertex.dat)

# le match est un peu tricky ici car utilisé pour réduire et ordonner
implantationVertexv2.dat <- implantationVertex.dat[match(unique(c(relation_graph$idimplantation, 
                                                        relation_graph$fklinked_implantation)), implantationVertex.dat$name),]
head(implantationVertexv2.dat)
dim(implantationVertexv2.dat)



graph_relation <- graph.data.frame(relation_graph, 
                                   directed = TRUE, 
                                   vertices = NULL)

 

is_simple(graph_relation) # on a plusieurs liens pour un meme couple de noeud

V(graph_relation)$name

#which_multiple retourne les liens doubles, un vecteur F/T
doublon.dat <- relation_graph[which_multiple(graph_relation),]

#verif_relation(c(40,99))
#doublon <- verif_relationv2(doublon.dat$idimplantation,doublon.dat$fklinked_implantation)
#openxlsx::write.xlsx(file = "doublon.xlsx", doublon)

# combien en a-t-il ?
nrow(relation_graph[which_multiple(graph_relation),])
# quels sont les vertex et leur nombres 
unique(relation_graph$idimplantation[which_multiple(graph_relation)])

# which_loop retourne les boucles : liens de noeuds à noeuds
relation_graph[which_loop(graph_relation),] # on a aussi une loop 

# on regarde à quoi elle correspond
relation.dat[relation.dat$idimplantation == 102 & relation.dat$fklinked_implantation == 102,]

# on simplifie le graph # en ajoutant un poids pour le nombre de relation
E(graph_relation)$weight <- 1

# on fait le nouveau graph avec le poids, poids et summer et modaNiv1 est concat 
graph_ensemble_simplify <- simplify(graph_relation, 
                                    edge.attr.comb = list(weight="sum", modaNiv1 = "concat")) 

length(E(graph_ensemble_simplify)$weight)

# il y a 189 relations qui ont une valeur supérieur à un 1
sum(E(graph_ensemble_simplify)$weight > 1)

length(V(graph_ensemble_simplify)$usual_name)

# un tableau pour montrer les noeuds avec des liens uniques
lien_unique.dat <- data.frame(V(graph_ensemble_simplify)$name, degree(graph_ensemble_simplify))
names(lien_unique.dat) <- c("name", "lien_unique")

lien_unique_join.dat <- left_join(lien_unique.dat, implantationVertex.dat, by = c("name"="name"))


#  3 - premiers graphs ================================
plot(graph_ensemble_simplify, vertex.label = NA, edge.label = NA,
     edge.color = "black", vertex.size = 2)

table(E(graph_ensemble_simplify)$weight)

un.voisin <- graph.neighborhood(graph_ensemble_simplify, order = 1) # attention la fonction evolue vers ego_size

degree(graph_ensemble_simplify, v = V(graph_ensemble_simplify))


# un vecteur de couleur pour les edges
E(graph_ensemble_simplify)$colorW <- ifelse(E(graph_ensemble_simplify)$weight == 1, "forestgreen",
                                            ifelse(E(graph_ensemble_simplify)$weight == 2, "orange", "red"))

V(graph_ensemble_simplify)$degree <- degree(graph_ensemble_simplify, v = V(graph_ensemble_simplify))

# un vecteur de couleur pour les vertexes
V(graph_ensemble_simplify)$colorV <- "gray60"

graphjs(graph_ensemble_simplify, 
        vertex.label = paste(V(graph_ensemble_simplify)$usual_name, V(graph_ensemble_simplify)$name),
        vertex.color = V(graph_ensemble_simplify)$colorV,
        vertex.size = log(sapply(un.voisin, vcount))/10,
        edge.color = E(graph_ensemble_simplify)$colorw)

#  3b - premiers graphs par type ================================
## si on veut faire des graph par relation / faire une liste des processus 

table(relation_graph$modaNiv1)

relation_graph_modaNiv1 <- relation_graph[relation_graph$modaNiv1 == "hiérarchique descendante", ]

graph_relation <- graph.data.frame(relation_graph_modaNiv1, 
                                   directed = FALSE, 
                                   vertices = implantationVertexv2.dat)

E(graph_relation)$weight <- 1

graph_modaNiv1_simplify <- simplify(graph_relation, edge.attr.comb = list(weight="sum", "ignore")) 

E(graph_modaNiv1_simplify)$weight

# un vecteur de couleur pour les edges
E(graph_modaNiv1_simplify)$colorW <- ifelse(E(graph_modaNiv1_simplify)$weight == 1, "forestgreen",
                                            ifelse(E(graph_modaNiv1_simplify)$weight == 2, "orange", "red"))

un.voisin <- graph.neighborhood(graph_modaNiv1_simplify, order = 1) # attention la fonction evolue vers ego_size

deux.voisins  <- graph.neighborhood(graph_modaNiv1_simplify, order = 2)
trois.voisins  <- graph.neighborhood(graph_modaNiv1_simplify, order = 3)
sapply(deux.voisins, vcount)
sapply(un.voisin, vcount)
sapply(trois.voisins, vcount)

# un vecteur de couleur pour les vertexes
V(graph_modaNiv1_simplify)$colorV <- "gray60"

graphjs(graph_modaNiv1_simplify, 
        vertex.label = paste(V(graph_modaNiv1_simplify)$usual_name, V(graph_modaNiv1_simplify)$name),
        vertex.color = V(graph_modaNiv1_simplify)$colorV,
        vertex.size = log(sapply(un.voisin, vcount))/10,
        edge.color = E(graph_modaNiv1_simplify)$colorW)

## je suis pas fan car c'est la meme chose je le garde au cas ou

un.voisin <- graph.neighborhood(graph_ensemble_simplify, order = 1) # attention la fonction evolue vers ego_size

# un vecteur de couleur pour les edges
E(graph_ensemble_simplify)$colorW <- ifelse(E(graph_ensemble_simplify)$weight == 1, "forestgreen",
                                            ifelse(E(graph_ensemble_simplify)$weight == 2, "orange", "red"))

V(graph_ensemble_simplify)$degree <- degree(graph_ensemble_simplify, v = V(graph_ensemble_simplify))
# un vecteur de couleur pour les vertexes
V(graph_ensemble_simplify)$colorV <- "gray60"

graphjs(graph_ensemble_simplify, 
        vertex.label = paste(V(graph_ensemble_simplify)$usual_name, V(graph_ensemble_simplify)$name),
        vertex.color = V(graph_ensemble_simplify)$colorV,
        vertex.size = log(sapply(un.voisin, vcount))/10,
        edge.color = E(graph_ensemble_simplify)$colorw)

graph_ensemble_simplify <- simplify(graph_relation, edge.attr.comb = list(weight="sum", modaNiv1 = "first") )

un.voisin <- graph.neighborhood(graph_ensemble_simplify, order = 1) # attention la fonction evolue vers ego_size

# un vecteur de couleur pour les edges
E(graph_ensemble_simplify)$colorW <- ifelse(E(graph_ensemble_simplify)$modaNiv1 == "hiérarchique descendante", "forestgreen",
                                            ifelse(E(graph_ensemble_simplify)$modaNiv1 ==  "hiérarchique desc. Ecole", "yellow", "red"))


library(crosstalk)
library(leaflet)

sd1 <- SharedData$new(data.frame(Degree = V(graph_ensemble_simplify)$degree))
sd0 <- SharedData$new(data.frame(lat = V(graph_ensemble_simplify)$lat, lng = V(graph_ensemble_simplify)$lat))


summary(data.frame(Degree = V(graph_ensemble_simplify)$degree))

bob <- filter_slider("degree", "Degree", sd1, column=~Degree, step=1)

jim <- leaflet(data = sd0) %>% 
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addMarkers( lng = ~lng, lat = ~lat)


# un vecteur de couleur pour les vertexes
V(graph_ensemble_simplify)$colorV <- "gray60"
V(graph_ensemble_simplify)$degree <- degree(graph_ensemble_simplify, v = V(graph_ensemble_simplify))

testgraph <- graphjs(graph_ensemble_simplify, 
        vertex.label = paste(V(graph_ensemble_simplify)$usual_name, V(graph_ensemble_simplify)$name),
        vertex.color = V(graph_ensemble_simplify)$colorV,
        vertex.size = log(sapply(un.voisin, vcount))/10,
        edge.color = E(graph_ensemble_simplify)$colorW, 
        crosstalk = sd0, brush = TRUE)

bscols(testgraph, jim)

#  4 - degrés et degrés des voisins ================================

sort(degree(graph_ensemble_simplify), decreasing = T)

un.voisin <- graph.neighborhood(graph_ensemble_simplify, order = 1) # attention la fonction evolue vers ego_size

graphjs(graph_ensemble_simplify,
        vertex.label = V(graph_ensemble_simplify)$usual_name,
        vertex.color = V(graph_ensemble_simplify)$colorV,
        vertex.size = log(sapply(un.voisin, vcount))/10, # pe degree plus simple ?
        edge.color = E(graph_ensemble_simplify)$colorW, brush=TRUE)

plot(degree(graph_ensemble_simplify),knn(graph_ensemble_simplify, V(graph_ensemble_simplify))$knn, log = "xy")
# c'est pas fou 

#  5 - deux trois stats sur le réseau ================================

# retourne le chemin le plus long dans le graph
farthest_vertices(graph_ensemble_simplify)
# et ici retourne le chemin pris
get_diameter(graph_ensemble_simplify)

verif_relation(21)

# une indexation sur les noeux avec "cîteaux (2)"
E(graph_ensemble)[[inc("Cîteaux (2)")]]

#  6 - cliques ================================

#Le triangle est le max dans notre graphe
table(sapply(cliques(graph_ensemble_simplify), length))

# donne les cliques avec un triangles
cliques(graph_ensemble_simplify)[sapply(cliques(graph_ensemble_simplify), length) == 3]

largest_cliques(graph_ensemble_simplify)

#  7 - communautés ================================

# doit me retourner un bon FALSE mais on ne sait jamais ...
# fonction utile par contre pour automatiser des trucs
is.connected(graph_ensemble_simplify)

comps <- decompose.graph(graph_ensemble_simplify)

# sauve la composante connexes du graph
V(graph_ensemble_simplify)$comps <- as.numeric(membership(components(graph_ensemble_simplify)))

idimpl <- "V38"

verif_relation(38)

V(graph_ensemble_simplify)[name == idimpl]$comps

un_sous_graph <- induced_subgraph()

graphjs(induced_subgraph(graph_ensemble_simplify, vids = V(graph_ensemble_simplify)[comps == 2]))

un_id <- "V38"

graph_a_partir_id <- function(un_id, un_graph = graph_ensemble_simplify) {
    # on calcul les composantes connexes 
    V(un_graph)$comps <- as.numeric(membership(components(un_graph)))
    # on produit un sous graphes qui prends tous les noeuds de la composantes connexes
    # il faut que les vertexes soit nommés avec "name" puisque j'indexe dessus 
    un_sous_graph <- induced_subgraph(un_graph, 
                                  vids = V(un_graph)[comps == V(un_graph)[name == un_id]$comps])

graphjs(un_sous_graph,
        vertex.label = paste(V(un_sous_graph)$usual_name, V(un_sous_graph)$name), # il faut usual name
        vertex.color = V(un_sous_graph)$colorV, # il faut colorV
        vertex.size = 0.2, # pe modifier par degree
        edge.color = E(un_sous_graph)$colorW, 
        brush=TRUE)
}


graph_a_partir_id("V99")

graphjs(induced_subgraph(graph_ensemble_simplify, vids = V(graph_ensemble_simplify)[comps == 4]))

class(comps)

comp_connexe <- data.frame(table(sapply(comps, vcount)),
table(sapply(comps, ecount)))
names(comp_connexe) <- c("nbr de noeud", "nbr de comp. connexe", "nbr de liens", "nbr de comp. connexe")

# un graph sur le gros groupe, attention decompose graph retoune une liste 
graph_principal <- decompose.graph(graph_ensemble_simplify, min.vertices = 599)[[1]]

vertex.connectivity(graph_principal)
# un seul noeud fait la liason 
edge.connectivity(graph_principal)

articulation.points(graph_principal)
# il y a 103 noeuds qui snot des points d'articulations, je suis pas fan de cette stats car est ce que des points en fin de réseau ne
# rentre pas dans cette case ?

##.###################################################################################33
## III. Des tests de plusieurs graphs ====
##.#################################################################################33

# c'est un peu leger, il faut regarder plus en détail ce que prend chaque fonction

#  1 - sous graph ================================

graph_principal <- decompose.graph(graph_ensemble_simplify, min.vertices = 599)[[1]]

graphjs(graph_principal,  vertex.size = 0.1)

#  2 - communauté sur le sous graph ================================
## il y a plusieurs moyens de faire du cluster

# fastgreedy.community est un type de regroupement hierarchique
graph_principal_cluster <- fastgreedy.community(graph_principal)

length(graph_principal_cluster) # il fait 20 groupes

sizes(graph_principal_cluster) # le nombre de vertexes dans chaques groupes

# un vecteur contenant l'appartenance aux groupes par vertexes
appartenance_group <- membership(graph_principal_cluster)

vingt_col <- wheel("steelblue", num = 20)

graph_principal <- set_vertex_attr(graph_principal, "couleur_group", value = vingt_col[appartenance_group])

graphjs(graph_principal, 
        vertex.size = 0.1,
        vertex.color = V(graph_principal)$couleur_group        
        )
        
#  3 - exploration threejs ================================

## test pour différent mode de représentation

graphjs(graph_ensemble_simplify, 
         vertex.label = V(graph_ensemble_simplify)$usual_name,
         vertex.color = V(graph_ensemble_simplify)$colorV,
         vertex.size = 0.1,
         edge.color = E(graph_ensemble_simplify)$colorW,
         layout=list(
            layout_randomly(graph_ensemble_simplify, dim=3),
            layout_on_sphere(graph_ensemble_simplify),
            layout_with_drl(graph_ensemble_simplify, dim=3),  # note! somewhat slow...
            layout_with_fr(graph_ensemble_simplify, dim=3, niter=30)),
        main=list("random layout", "sphere layout", "drl layout", "fr layout"),
        fpl=300)
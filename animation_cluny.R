library(dplyr)
library(igraph)
library(intergraph)


#

T0relation <- readRDS("data/T0Newchgt20200122.rds") %>% 
  # filtre sur les relations
  filter(caracNew == "Relations") %>% 
  # pas les déplacement
  filter(modaNiv1 != "Déplacement") %>%
  # on garde un nombre limité de champs
  select(idfactoid, # id du factoid
         idimplantation, # id de l'implantation
         usual_name, # nom de l'implantation
         modaNiv1, # modalité du type relation ex : Relation horizontale
         fklinked_implantation, # id de l' implantation lié, dans le cas d'un choix de graph dirigé idimplantation ---> fklinked_implantation
         linked_implantation_name, # nom de l'implantation lié
         lat, # lat de l'idimplantation
         lng, # lng de l'idimplantation
         date_startC, # date de debut de la relation
         date_stopC # date de fin de la relation
  )


T0relation <- T0relation[!is.na(T0relation$date_startC),]
T0relation <- T0relation[!is.na(T0relation$date_stopC),]


relation <- subset(T0relation, !(T0relation$modaNiv1 == "hiérarchique asc. Ecole" | T0relation$modaNiv1 == "hiérarchique ascendante") ) %>% 
  # attention c'est important d'avoir les deux premières colonnes qui indiquent les liens A ---- B 
  dplyr::select(tail = idimplantation, head = fklinked_implantation,  onset = date_startC, terminus = date_stopC, modaNiv1, usual_name, linked_implantation_name) 


partA <- relation %>% 
  select(tail, usual_name)

idimpl_nom <- relation %>% 
  select(tail = head, usual_name = linked_implantation_name) %>% 
  bind_rows(partA) %>% 
  distinct(tail, .keep_all = TRUE)

rm(partA)

vertex_v1 <-  idimpl_nom[match(unique(c(relation$tail, relation$head)), 
                               idimpl_nom$tail),]

graph_relation <- graph.data.frame(relation, directed = FALSE, vertices = vertex_v1)
comps <- decompose.graph(graph_relation)

count_multiple(graph_relation)
relation[which_multiple(graph_relation),] %>% 
        arrange(tail)

relation$multiple<- count_multiple(graph_relation)

relation_multiple <- relation[relation$multiple > 1,] %>% 
    arrange(tail)
openxlsx::write.xlsx(file = "relation_multiple.xlsx", relation_multiple)


sapply(comps, vcount)

cluny <- comps[[3]]
    
E(cluny)$onset[which_multiple(cluny)]

cluny_net <- asNetwork(cluny)

library(networkDynamic) # attention masque un paquet de truc de igraph
library(ndtv)

clunny_net %e% "date_startC"

vs <- data.frame(onset=0, terminus=50, vertex.id=1:17)
es <- data.frame(onset=1:49, terminus=50, 
                 head=as.matrix(net3, matrix.type="edgelist")[,1],
                 tail=as.matrix(net3, matrix.type="edgelist")[,2])

cluny_dyn <- networkDynamic(cluny_net)


render.d3movie(cluny_dyn, usearrows = F, displaylabels = F)




# les packages pour faire des graphs d'animation 




hc.spls <- cbind((hc$Time-20)/(60*60),  hc$Time/(60*60), hc$ID1, hc$ID2)
hc.dn <- networkDynamic(edge.spells=hc.spls)

wheel <- network.initialize(10)
add.edges.active(wheel,tail=1:9,head=c(2:9,1),onset=1:9, terminus=11)
add.edges.active(wheel,tail=10,head=c(1:9),onset=10, terminus=12)
plot(wheel)


detach(package:ndtv)
detach(package:networkDynamic)

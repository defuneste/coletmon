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

CaracHist <-readxl::read_excel("data/NewModelCaracModalitesColor5.xlsx",
                               sheet = "color")
ColRelations <- filter(CaracHist, caracNew == "Relations") %>% 
    select(modaNiv1, modaNiv1_Color) %>% 
    filter(!is.na(modaNiv1_Color)) %>% 
    distinct(modaNiv1, .keep_all = TRUE) %>% 
    arrange(modaNiv1)

T0relation <- T0relation %>% 
    left_join(ColRelations, by ="modaNiv1")

T0relation <- T0relation[!is.na(T0relation$date_startC),]
T0relation <- T0relation[!is.na(T0relation$date_stopC),]


relation <- subset(T0relation, !(T0relation$modaNiv1 == "hiérarchique asc. Ecole" | T0relation$modaNiv1 == "hiérarchique ascendante") ) %>% 
  # attention c'est important d'avoir les deux premières colonnes qui indiquent les liens A ---- B 
  dplyr::select(tail = idimplantation, head = fklinked_implantation,  onset = date_startC, terminus = date_stopC, modaNiv1_Color, usual_name, linked_implantation_name) %>% 
  dplyr::mutate(dure = terminus - onset) %>% 
  dplyr::filter(dure >= 0)    


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

count_multiple(graph_relation)
relation[which_multiple(graph_relation),] %>% 
        arrange(tail)

relation$multiple<- count_multiple(graph_relation)

relation_multiple <- relation[relation$multiple > 1,] %>% 
    arrange(tail)

# cas relation horizontale : simplify 
# cas date diff : min pour date_startC et max pour date_stopC
# idem pour chevauchement
# cas changement : prendre first ? et documenter ?

graph_relation

graph_relation_simpl <- simplify(graph_relation, edge.attr.comb = list(onset = "min", terminus = "max", modaNiv1_Color = "first", "ignore"))


E(graph_relation_simpl)$modaNiv1_Color

# openxlsx::write.xlsx(file = "relation_multiple.xlsx", relation_multiple)

comps <- decompose.graph(graph_relation_simpl)

cluny <- comps[[3]]

cluny_net <- asNetwork(cluny)



library(networkDynamic) # attention masque un paquet de truc de igraph
library(ndtv)


# vertex
vs <- data.frame(onset = 0, terminus = 1792, vertex.id=1:length(cluny_net %v% "vertex.names"), name = cluny_net %v% "vertex.names")
dim(vs)

# edge spells
es <- data.frame(onset = cluny_net %e% "onset",
                 terminus = cluny_net %e% "terminus",
                 head = as.matrix(cluny_net, matrix.type="edgelist")[,1],
                 tail = as.matrix(cluny_net, matrix.type="edgelist")[,2])
dim(es)

cluny_dyn <- networkDynamic(cluny_net, edge.spells = es, vertex.spells = vs)


compute.animation(cluny_dyn, animation.mode = "kamadakawai",
                  slice.par=list(start=0, end=1800, interval=50, 
                                 aggregate.dur=50, rule='any'))


render.d3movie(cluny_dyn, usearrows = F, displaylabels = F, 
               label = cluny_dyn %v% "usual_name",
               # vertex
               vertex.border = "#adadad",
               vertex.col = "#adadad",
               vertex.tooltip = paste("<b> usual_name :</b>", (cluny_dyn %v% "usual_name") , "<br>",
                                      "<b>Degré:</b>", (degree(cluny_dyn))),
               vertex.cex = log(degree(cluny_dyn))/10, 
               # edge
               edge.col = (cluny_dyn %e% "modaNiv1_Color"),  
               edge.lwd = 2)



detach(package:ndtv)
detach(package:networkDynamic)

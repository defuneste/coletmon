library(dplyr)
library(igraph)

library("networkDynamic") # attention masque un paquet de truc de igraph
library(ndtv)

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
  dplyr::select(idimplantation, fklinked_implantation, usual_name, linked_implantation_name, modaNiv1, date_startC, date_stopC, idfactoid) 


partA <- relation %>% 
  select(idimplantation, usual_name)

idimpl_nom <- relation %>% 
  select(idimplantation = fklinked_implantation, usual_name = linked_implantation_name) %>% 
  bind_rows(partA) %>% 
  distinct(idimplantation, .keep_all = TRUE)

rm(partA)

vertex_v1 <-  idimpl_nom[match(unique(c(relation$idimplantation, relation$fklinked_implantation)), 
                               idimpl_nom$idimplantation),]



# les packages pour faire des graphs d'animation 

library("networkDynamic") # attention masque un paquet de truc de igraph
library(ndtv)


hc.spls <- cbind((hc$Time-20)/(60*60),  hc$Time/(60*60), hc$ID1, hc$ID2)
hc.dn <- networkDynamic(edge.spells=hc.spls)

wheel <- network.initialize(10)
add.edges.active(wheel,tail=1:9,head=c(2:9,1),onset=1:9, terminus=11)
add.edges.active(wheel,tail=10,head=c(1:9),onset=10, terminus=12)
plot(wheel)


detach(package:ndtv)
detach(package:networkDynamic)

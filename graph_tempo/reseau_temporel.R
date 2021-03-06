### reseau dynamique

# deux trois conventions 
# V(t) = V pour tout les temps t

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


interval_sup <-  seq(0,1800,100)

#duplication par pas de temps
T0relation_all <- merge(T0relation, interval_sup ,all = TRUE)

# un nom plus cool 
names(T0relation_all)[names(T0relation_all) == "y"] <-  "interval_inf"

# interval_sup
T0relation_all$interval_sup <- T0relation_all$interval_inf + 100

# on fait un nouveau tableau ou exite une relation qui est dans la durée
T0relation_filtre <- dplyr::filter(T0relation_all, interval_sup > date_startC) %>% 
                        dplyr::filter(interval_inf <= date_stopC) %>% 
                        dplyr::mutate(interval = (interval_inf + interval_sup)/2)

relation <- subset(T0relation_filtre, !(T0relation_filtre$modaNiv1 == "hiérarchique asc. Ecole" | T0relation_filtre$modaNiv1 == "hiérarchique ascendante") ) %>% 
    # attention c'est important d'avoir les deux premières colonnes qui indiquent les liens A ---- B 
    dplyr::select(idimplantation, fklinked_implantation, usual_name, linked_implantation_name, modaNiv1, interval, idfactoid) 


partA <- relation %>% 
    select(idimplantation, usual_name)

idimpl_nom <- relation %>% 
    select(idimplantation = fklinked_implantation, usual_name = linked_implantation_name) %>% 
    bind_rows(partA) %>% 
    distinct(idimplantation, .keep_all = TRUE)

rm(partA)

# c'est ici la magie qui permet d'aller chercher les idimplantion/noms dans idimpl et de les mettre dans le meme ordre 
# que les vertices du graph, bien verifier le comportement de match
vertex_v1 <-  idimpl_nom[match(unique(c(relation$idimplantation, relation$fklinked_implantation)), 
                               idimpl_nom$idimplantation),]

g.relation <- graph.data.frame(relation[, c("idimplantation", "fklinked_implantation", "interval", "modaNiv1")], vertices = vertex_v1, directed = FALSE)

graphjs(g.relation,
        vertex.label = V(g.relation)$name, # il faut usual name
        #vertex.color = V(graph_relation)$colorV, # il faut colorV
        vertex.size = 0.2, # pe modifier par degree
        brush=TRUE, 
        #crosstalk=sd, 
        width=800)

interval <-  sort(unique(T0relation_filtre$interval))[
  1:length(unique(T0relation_filtre$interval))
  ]/50


### a adapter 
g.relation.100 <- lapply(interval, function(i) {
    g <- subgraph.edges(g.relation, E(g.relation)[interval > 50 * (i-1) &  interval <= 50*i],
                        delete.vertices=TRUE) # je pense garder pour avoir l'impression de rajout uoi que c'est pas dit
    # et fait un simplify
    simplify(g)
})

g.relation.100[1]

# un pas de temps prends un undex de la liste g.relation.100

un_graph_dans_le_temps <- function(un_pas_de_temps) {
graphjs(g.relation.100[[un_pas_de_temps]],
        vertex.label = V(g.relation.100[[un_pas_de_temps]])$name, # il faut usual name
        #vertex.color = V(graph_relation)$colorV, # il faut colorV
        vertex.size = 0.2, # pe modifier par degree
        brush=TRUE, 
        #crosstalk=sd, 
        width=800) # a modifier si le but n'est plus de mettre dans un rapport 
  }

sapply(g.relation.100, ecount)

sapply(g.relation.100, diameter)

#attention si delete.vertices = FALSE compte les vertex seul comme une comp connexe
sapply(g.relation.100, count_components)



### reseau dynamique

# deux trois conventions 
# V(t) = V pour tout les temps t

library("sand")
data(hc)
View(hc)

head(hc)

hist(hc$Time)

vids <- sort(unique(c(hc$ID1, hc$ID2)))

data.frame(vids)

g.week <- graph.data.frame(hc[,c("ID1", "ID2", "Time")], vertices = data.frame(vids), directed = FALSE)

E(g.week)$Time <- E(g.week)$Time / (60*60)

status <- unique(rbind(data.frame(id=hc$ID1, status=hc$S1), data.frame(id=hc$ID2, status=hc$S2)))
V(g.week)$Status <-  as.character(status[order(status[,1]),2])

# on passe par une fonction qui fait des sous graphs 

g.sl12 <- lapply(1:8, function(i) {
        g <- subgraph.edges(g.week, E(g.week)[Time > 12*(i-1) &  Time <= 12*i],
                                   delete.vertices=FALSE)
# et fait un simplify
            simplify(g)
 })

### on peut adapater au rapport

 
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

# un filtre pour avoir sur le bon pas de temps
T0relation_filtre <-  T0relation_all[T0relation_all$interval_sup > T0relation_all$date_startC & T0relation_all$interval_inf <= T0relation_all$date_stopC,]

# on fait un nouveau tableau ou exite une relation qui est dans la durée
T0relation_filtre <- dplyr::filter(T0relation_all, interval_sup > date_startC) %>% 
                        dplyr::filter(interval_inf <= date_stopC) %>% 
                        dplyr::mutate(interval = (interval_inf + interval_sup)/2)

relation <- subset(T0relation_filtre, !(T0relation_filtre$modaNiv1 == "hiérarchique asc. Ecole" | T0relation_filtre$modaNiv1 == "hiérarchique ascendante") ) %>% 
    # attention c'est important d'avoir les deux premières colonnes qui indiquent les liens A ---- B 
    dplyr::select(idimplantation, fklinked_implantation, usual_name, linked_implantation_name,modaNiv1, interval, idfactoid) 


vids <- sort(unique(c(hc$ID1, hc$ID2)))



data.frame(vids)

g.week <- graph.data.frame(hc[,c("ID1", "ID2", "Time")], vertices = data.frame(vids), directed = FALSE)

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

g.relation <- graph.data.frame(relation[, c("idimplantation", "fklinked_implantation", "interval")], vertices = vertex_v1, directed = FALSE)

graphjs(g.relation,
        vertex.label = V(g.relation)$name, # il faut usual name
        #vertex.color = V(graph_relation)$colorV, # il faut colorV
        vertex.size = 0.2, # pe modifier par degree
        brush=TRUE, 
        #crosstalk=sd, 
        width=800)

unique(T0relation_filtre$interval)

### a adapter 
g.relation.100 <- lapply(1:8, function(i) {
    g <- subgraph(g.week, E(g.week)[Time > 12*(i-1) &  Time <= 12*i],
                        delete.vertices=FALSE)
    # et fait un simplify
    simplify(g)
})


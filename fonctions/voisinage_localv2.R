# fonction voisinage local
library(dplyr)
library(sf)
library(igraph)
library(threejs)


# 1 - chargement des rds =================
T0relation <- readRDS("data/T0relation.rds")
# on vire les deplacements
T0relation <- T0relation[!T0relation$modaNiv1 == "Déplacement",]

relation <- T0relation
idimplantation_saisie <- 99


graph_a_partir_id <- function(un_graph = graph_relation, idimplantation_saisie) {
    # on calcul les composantes connexes 
    V(un_graph)$comps <- as.numeric(membership(components(un_graph)))
    # on produit un sous graphes qui prends tous les noeuds de la composantes connexes 
    # il faut que les vertexes soient nommés avec "name" puisque j'indexe dessus 
    un_sous_graph <- induced_subgraph(un_graph, 
                                      vids = V(un_graph)[comps == V(un_graph)[name == idimplantation_saisie]$comps])
    return(un_sous_graph)
}


# ======================================================================================================

graph_a_partir_usual_name <- function(un_graph = graph_relation, usual_name_saisie) {
    # on calcul les composantes connexes 
    V(un_graph)$comps <- as.numeric(membership(components(un_graph)))
    # on produit un sous graphes qui prends tous les noeuds de la composantes connexes 
    # il faut que les vertexes soient nommés avec "name" puisque j'indexe dessus 
    un_sous_graph <- induced_subgraph(un_graph, 
                                      vids = V(un_graph)[comps == V(un_graph)[usual_name == usual_name_saisie]$comps])
    return(un_sous_graph)
}

# ======================================================================================================

voisinage_local_opt1 <- function(relation, idimplantation_saisie, niveau) {
        
        # ici on allége T0relation et on evite des potentiels NA
        relation_graph <- relation %>% 
            st_drop_geometry() %>% 
            filter( !is.na(fklinked_implantation)) %>%  # au cas ou on a des NA
            select( idimplantation, fklinked_implantation, usual_name,  usual_name_link, modaNiv1, idfactoid)
        

        # on fait un graph
        graph_relation <- graph.data.frame(relation_graph, 
                                           # ici si on est en "Relation horizontale" on est dans un graph non dirigé sinon dirigé
                                           directed = FALSE) # à noter on est en non dirigé
        # a documenter
        un_sousgraph <- graph_a_partir_id(graph_relation, as.character(idimplantation_saisie))
        
        un_df <- as_data_frame(
                        make_ego_graph(un_sousgraph,
                                       order = niveau, 
                                       nodes = V(un_sousgraph)[name = as.character(idimplantation_saisie)])[[1]]) %>% 
                        anti_join(as_data_frame(
                                    make_ego_graph(un_sousgraph,
                                                   order = niveau, 
                                                   nodes = V(un_sousgraph)[name = as.character(idimplantation_saisie)], mindist = niveau)[[1]]),
                        by = "idfactoid"
            )
        
        # on ajoute le niveau
        un_df$niveau <- niveau
        # on range pour avoir une bonne tete de T0New filtre
        un_df <- un_df %>% 
            select(idfactoid) %>% 
            left_join(relation_graph, by = "idfactoid") %>% 
            mutate(niveau = niveau)
        
        return(un_df)
    }
    
# ======================================================================================================

voisinage_local_opt1_usual <- function(relation, usual_name_saisie, niveau) {
    
    # ici on allége T0relation et on evite des potentiels NA
    relation_graph <- relation %>% 
        st_drop_geometry() %>% 
        filter( !is.na(fklinked_implantation)) %>%  # au cas ou on a des NA
        select( idimplantation, fklinked_implantation, usual_name,  usual_name_link, modaNiv1, idfactoid)
    
    # ici on fait un fobjet pour avoir les idimplantations et leurs noms, c'est un besoin pour qualifier les vertex
    # surtout ici ou on veut filtre dessus
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
    
    # on fait un graph
    graph_relation <- graph.data.frame(relation_graph, 
                                       # ici si on est en "Relation horizontale" on est dans un graph non dirigé sinon dirigé
                                       directed = FALSE,  # à noter on est en non dirigé
                                       vertices = vertex_v1)
    # a documenter
    un_sousgraph <- graph_a_partir_usual_name(graph_relation, as.character(usual_name_saisie))
    
    un_df <- as_data_frame(
        make_ego_graph(un_sousgraph,
                       order = niveau, 
                       nodes = V(un_sousgraph)[name = as.character(usual_name_saisie)])[[1]]) %>% 
        anti_join(as_data_frame(
            make_ego_graph(un_sousgraph,
                           order = niveau, 
                           nodes = V(un_sousgraph)[name = as.character(usual_name_saisie)], mindist = niveau)[[1]]),
            by = "idfactoid"
        )
    
    # on ajoute le niveau
    un_df$niveau <- niveau
    # on range pour avoir une bonne tete de T0New filtre
    un_df <- un_df %>% 
        select(idfactoid) %>% 
        left_join(relation_graph, by = "idfactoid") %>% 
        mutate(niveau = niveau)
    
    return(un_df)
}



# =========================================================================

voisinage_local_opt2 <- function(relation, idimplantation_saisie) {
    
    ## ici j'ai besoin du graph de la composantes connexes pour obtenir le diamètre
    # c'est un peu redondant avec voisinage_local_opt1
    
    relation_graph <- relation %>% 
        st_drop_geometry() %>% 
        filter( !is.na(fklinked_implantation)) %>%  # au cas ou on a des NA
        select(idimplantation, fklinked_implantation, usual_name,  usual_name_link, modaNiv1)
    
    # on fait un graph
    graph_relation <- simplify(graph.data.frame(relation_graph, 
                                                # ici si on est en "Relation horizontale" on est dans un graph non dirigé sinon dirigé
                                                directed = FALSE)) # à noter on est en non dirigé
    # on peut gagner en objet intermediaire en passant directement graph_relation dans graph_a_partir_id
    # mais on perds en lisbilité
    sous_graph <- graph_a_partir_id(graph_relation, idimplantation_saisie)
    
    # on initialise la list
    une_list <- list()
    
    # chaque df par niveau va aller dans une_list 
    for(i in 1:length(as_ids(get_diameter(sous_graph)))) {
        une_itération <- voisinage_local_opt1(relation, idimplantation_saisie, i)
        une_list[[i]] <- une_itération 
    }
    
    # on va retourner
    # on ajoute les differents tableaux
    return(do.call(rbind, une_list) %>%
               # on filtre en ne gardant que la première valeur d'idimplantation
               dplyr::distinct(idfactoid, .keep_all = TRUE))
}


#### pour tester =======

unique(T0relation$modaNiv1)
T0relation_filtre <- T0relation[T0relation$modaNiv1 == "Relation horizontale" | T0relation$modaNiv1 == "hiérarchique descendante", ]

bob <- voisinage_local_opt2(T0relation_filtre, 99)

voisinage_local_opt1(T0relation_filtre, 99, 3)



bob2 <- bob %>% 
    left_join(T0relation_filtre, by = "idfactoid", suffix = c("", "_rajout")) %>% 
    st_as_sf() %>% 
    filter(niveau <= 6)

dessine_moi_un_graph(bob2)

# le diamétre est de 15 mais avec 8 niveau on arrive à avoir toutes les implantations
table(voisinage_local_opt2(T0relation, 99)$niveau)

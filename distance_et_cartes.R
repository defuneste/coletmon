
##.###################################################################################33
## I. Calculs distances/ portée du réseau de chaque implantation ====
##.#################################################################################33
# 1.  On charge les fichiers ===========================
# ici je vais reprendre et documenter le travail d'Hélène
# Hélène travaille pas mal en tidyverse, on va donc charger ses bibliotheques 
# je simplifierais en R base au besoin

library(dplyr) 
library(sf)
library(ggplot2)
library(plotly)
library(purrr)

fait.dat <- read.csv("data/fait.txt")
implantation.dat <- read.csv("data/implantation.txt")
# les relations sont un sous ensemble des faits
relation.dat <- fait.dat[fait.dat$caracNew == "Relations" ,]
dim(relation.dat)

relation.dat <- relation.dat[relation.dat$modAgreg != "A", ] # on enlève A pour n' avoir que D
relation.dat <- relation.dat[relation.dat$modAgreg != "X", ] # on enlève X
dim(relation.dat)


# 1.  On construit un nouveau tableau qui contient les lat/long de chaque cote de la relation ===========================
# l'objectif est de dessiner les liens sur une carte : de A -> B
# Hélène indique que les lat/long dans relations.dat sont celles de l'implantation (idimplantation) 
# et pas celle lièe (fklinked_implantation) 

Relation_renomer <- dplyr::rename(relation.dat,                                 # rename est pas mal utilisé donc on precise la library
                                  idimpl_link=fklinked_implantation,            # ici on prend l'id lié
                                  usual_name_link=linked_implantation_name) %>% # idem pour le nom lié
    dplyr::select(-X)                                           # on pipe pour verifier X que j' avais fait lors mon export

# je vais decouper un peu le pipe d'Hélène avec une table produite pour la jointure 
# qui va comporter les lat/long des implantations liées

implantation_renomer <- implantation.dat %>% 
    dplyr::select(idimpl_link = idimplantation, # selon la doc on peut directement renomer dans un select
                  lat_link = lat, 
                  lng_link = lng, 
                  Dioc_link = Diocese) 

# on fait la jointure 

relation_total.dat <- dplyr::left_join(Relation_renomer, implantation_renomer, by = "idimpl_link")

summary(relation_total.dat)
# il y a des valeurs manquantes dans les lat/long de A (26) et B (46)

relation_total.dat <- dplyr::filter(relation_total.dat, !is.na(lat) & !is.na(lat_link)) # on retire les NA, c'est 60 relations

# 2.  On passe en sf =============================
# je m'inspire de cette solution : 
# https://stackoverflow.com/questions/58723988/create-numerous-lines-in-simple-features-from-list-of-coordinates-in-r

# matrice de départ
from.dat  <- relation_total.dat %>% 
    select(lng, lat) %>% 
    as.matrix()

#matrice d'arrivé
to.dat <- relation_total.dat %>% 
    select(lng_link, lat_link) %>% 
    as.matrix()


relation_total.dat$geometry <- do.call(st_sfc,                                                             # on fait une fonction sfc
                                       lapply(                                                             # on fait un apply sur chaque lignes
                                           1:nrow(relation_total.dat),
                                           function(i){                              
                                               st_linestring(                                              # qui va tracer des lignes
                                                   matrix(                                                 # en prenant les points dans une matrics
                                                       c(from.dat[i,], to.dat[i,]), ncol=2,byrow=TRUE)     # composé du couple de point de départ et arrivé
                                               )  }     )  )

relation_total.shp <- st_as_sf(relation_total.dat, sf_column_name = "geometry", crs = 4326) %>% 
    st_transform(2154)

# 2.  On calcul la distance ============================= 

relation_total.shp$distance_km <- round(                                    # on va arrondir le résultats à 2 chiffres
                            as.numeric(                                     # je drop units 
                                st_length(relation_total.shp)/1000), 2)     # on mesure la distance sur la geometry 

# un ggplot rapide pour regarder
ggplot(relation_total.shp, aes(modalite, distance_km, color = modalite)) +
                                            geom_boxplot()

# on enleve les facteurs inutilisés dans modalite
relation_total.shp$modalite <- factor(relation_total.shp$modalite)

# un plotly
f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
)

y <- list(
    title = "Distance (km)",
    titlefont = f
)

# j'ai dropé la geometry car j'ai l'impression que cela me doublais dans ce cas les lignes
plot_ly(st_drop_geometry(relation_total.shp), y = ~ distance_km, color = ~ modalite, type = "box", 
        # ici on affaiche les valeur et on rajoute le nom/id des implantations dans les deux sens
        text = ~paste(paste(relation_total.shp$usual_name, relation_total.shp$idimplantation), 
                      paste(relation_total.shp$usual_name_link, relation_total.shp$idimpl_link), sep = "\n")) %>% 
    layout(yaxis = y)


library(mapview)
mapview(relation_total.shp, zcol ="modAgreg",burst=TRUE)

##.###################################################################################33
## II. des evolutions au cours du temps ====
##.#################################################################################33

names(relation_total.shp)
summary(relation_total.shp)

# l'idee est de refaire un jeux de données ou le temps est cumulatif pour une animation
# on dessine une frame par periode et il faut donc que l' ensemble des liens present à ce moment soient présent dans 
# le jeux de données 
relation_slim <- relation_total.shp %>%  # on va faire un jeux de données plus léger
    st_drop_geometry() %>%  # on drop la geometry
    # puis tout un tas de variables peux ou pas utile
    select(-c(caracteristique, caracNew, date_start_min, date_start_max, date_stop_min, date_stop_max, location_granularity, Diocese, Dioc_link, idfactoid, distance_km)) %>% 
    # on impute les NA avec la valeur mins
    mutate(date_startC = ifelse(is.na(relation_total.shp$date_startC), min(relation_total.shp$date_startC, na.rm = T), relation_total.shp$date_startC))

# j'ai fait une pause dans le pipe 
test <- relation_slim %>%  
    # on renome debut avec un cut, repasser en année 
    mutate(debut = (as.numeric(cut(relation_slim$date_startC, seq(400,1800,50))) * 50) + 400,
           date_stopC = ifelse(is.na(relation_slim$date_stopC), debut, date_stopC)) %>% 
    # on split en liste par debut
    split(.$debut) %>% 
    # on accumule les lignes passées
    purrr::accumulate(~bind_rows(.x, .y)) %>% 
    #set_names(seq(400,1800,50)) %>% 
    bind_rows(.id = "frame")  %>% 
    # on repasse en mumeric
    mutate(frame = as.numeric(frame))
   
View(test[[4]])

#### on va simplifier les implantations

implantations_slim <- implantation.dat %>% 
    select(idimplantation, usual_name, lat, lng)

geo <- list(
    scope = 'europe')


plot_geo() %>%
    add_markers(
        data = implantations_slim, x = ~lng, y = ~lat, text = ~paste(idimplantation, usual_name),
        hoverinfo = "text", alpha = 0.5
    ) %>%
    add_segments(
        data = test,
        x = ~lng, xend = ~lng_link,
        y = ~lat, yend = ~lat_link,
        alpha = 0.5, size = I(1.5), hoverinfo = "none", 
        frame = ~frame, color = ~modalite
    ) %>%
    layout(
        title = 'test',
        geo = geo, showlegend = TRUE
    )

# source d'info    https://stackoverflow.com/questions/40350925/how-to-create-a-choropleth-map-using-plot-geo
# autre option https://stackoverflow.com/questions/36554605/cant-loop-with-rs-leaflet-package-to-produce-multiple-maps/36587525#36587525

#############♣ Export pour QGIS
Tliensres1<-rename(Tliensres1,idimplantation=idimpl_link,
                   usual_name=usual_name_link)
write.csv(Tliensres1, file="testLiens/RelationsDistances.csv",na="",row.names = FALSE)
write.csv(ImplNew, file="testLiens/ImplNew.csv",na="",row.names = FALSE)

Temp <- filter(ImplNew,!is.na(lat)) %>% 
    st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
    st_transform(2154) 

st_write(Temp, "testLiens/ImplXY", driver="ESRI Shapefile")
st_write(TliensFin, "testLiens/LiensFin", driver="ESRI Shapefile")

write.csv(TliensX, file="testLiens/RelationsLiens.csv",na="",row.names = FALSE)

###########
#################test carto  avec une implantation
centreid<-5   #"Auxerre"
centreid<-1100   #"Abbaye de Charroux"

TliensXi<-TliensX %>% 
    filter(idimplantation==centreid) %>% 
    select(-date_start_min:-date_stop_max)
TliensXi<-arrange(TliensXi,idimplantation, modalite,idimpl_link)

Temp<-filter(ungroup(ImplNew),idimplantation==centreid) %>% 
    select(idimplantation,usual_name,lat,lng) %>% 
    mutate(lat_link=lat,
           lng_link=lng,
           idimpl_link=idimplantation,
           usual_name_link=usual_name)

TliensXi <-TliensXi %>% 
    filter(idimplantation==centreid)%>% 
    filter(!is.na(lat_link)) %>% 
    bind_rows(Temp)

#Après avoir compilé les fonctions de Col&MonCarto
#Avant d'envoyer à la procedure uil faut renommer les lat_link_>lat...
TCartoLiens <-rename(TliensXi,
                     lat0=lat,lng0=lng,lat=lat_link,lng=lng_link)
TCartoLiens<-as.data.frame(TCartoLiens)

nbtype<-length(unique(TCartoLiens$modalite))

initmapview(nbtype,"Set1")
m<-mapImplVite(TCartoLiens,"modalite")
m
mapshot(m, url = paste0(getwd(), "/AbbayeCharroux.html"))




############GGplot2 des liens mais problème de couleur
TCartoLiens<- st_as_sf(TCartoLiens,coords = c("lng", "lat"), crs = 4326) %>% 
    st_transform(2154)
ggplot() + 
    geom_sf(data = DioFond)+
    geom_sf(data = TCartoLiens, aes(colour=modalite,fill=modalite),cex=3)+
    scale_fill_manual(values = cols)+
    scale_colour_manual(values = cols)+
    geom_sf(data = filter(TCartoLiens,idimplantation==centreid), aes(colour = modalite, fill = modalite)) +
    coord_sf()+
    scale_fill_manual(values = cols)+
    scale_colour_manual(values = cols)

#Carto de tout les liens
ggplot() +
    geom_sf(data = DioFond)+
    geom_sf(data = TCartoLiens, 
            aes(colour = modalite, fill = modalite),size=1) +
    coord_sf()+
    scale_fill_manual(values = cols)+
    scale_colour_manual(values = cols)


# scale_colour_brewer(palette = "Set1")+
# scale_fill_brewer(palette = "Set1")
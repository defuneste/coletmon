## Script de fonctions utiles pour Col&Mon
# inspiration des scripts d'Helene Mathian
# 18-10-2019

# 1 - lecture des diocèses géometries =====
# attention utilisateur de windows les chemins d'accés sont differents

diocese <- function(cheminDioc = "data/Diocese/", 
                    FicDioFond ="Contour_diocèse_France_1317L93.shp"){
# il prend comme argument le chemin du fichier et son nom
# par defaut je prend les chemin relatif de ma struture de dossier
# on verifie que l'on a sf
# sinon on l'install avec tous
if(require("sf")==FALSE)  
    install.packages("sf",  dependencies=c("Depends", "Suggests"))

# un st_read avec une option d'encodage qui prend les deux argument nom du fichier et 
# localisation
DioFond.shp <- st_read(dsn = paste(cheminDioc, FicDioFond, sep=""), 
                   stringsAsFactors = FALSE, 
                   options = "ENCODING=latin1")
st_crs(DioFond.shp) <- 2154 # le bon crs
colnames(DioFond.shp)[2] <- "DioceseNom" # un changement de nom de var
return(DioFond.shp)
}

print("diocese() permet de charger les diocèses")

# 2. initialisation de mapview  et carto rapide ====

# mapview semble être un wrapper de leaflet, je ne sais pas si c'est sur la version js ou la version R 
# qui elle meme est une API de la version js
# cette fonction necessite RColorBrewer et permet d'initiliser mapview 
# elle prends deux plateformes comme fonds 
# d'autres sont possibles cf :https://r-spatial.github.io/mapview/articles/articles/mapview_03-options.html


## une fonction d'initialisation de mapview un peu comme leaflet()
initmapview <- function(ncol = 3, palBrew = "Set2") #la fonction prends deux arguments 
                                      # ncol est le nombre de couleurs et palBrew est le set de couleur
                                      # de RColorBrewer par ex "Set1"ou "set2" cf display.brewer.all()
                                      # attention ces sets ne sont souvent pas fait pour beaucoup de couleurs
{ 
  # chargement des libraries et dependance
  if(require("mapview") == FALSE)  
  install.packages("mapview",  dependencies=c("Depends", "Suggests"))
  if(require("RColorBrewer") == FALSE)  
  install.packages("RColorBrewer",  dependencies=c("Depends", "Suggests"))

  mapview::mapviewPalette() 
  mapview::mapviewOptions(basemaps = c("OpenStreetMap","Esri.WorldImagery"),
                             raster.palette = grey.colors,
                             vector.palette = colorRampPalette(brewer.pal(ncol,palBrew)),
                             na.color = "magenta", # les Na
                             layers.control.pos = "topleft") # ou on met/activ les layers
}

print("initmapview() initialise mapview avec un ncol et un set de couleur a def en args")


# # il faut decouper cette fonction 
# 
# 
# 
# filter(ImpSel,!is.na(lat)) %>% 
#     st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
#     st_transform(2154)
# 

## une fonction de production de carte rapide
# Impsel.shp prend un objet spatial plutot de type sf
# factor_color est un vecteur/factor qui propose une couleur en fonction d'un factor  


mapImplVite <- function(ImpSel.shp, Vcolor)
{
    # initialisation de mapview
    initmapview()
    #  affichage de 
    mapview::mapview(diocese.shp, color = "gray75",  lwd = 1, # couleur et type du contour
                     col.regions = "red",  alpha = 0.5,       # couleur du fond et type
                     layer.name = "Diocèses" ) +
        
    if (Vcolor=="") {mapview::mapview(ImpSel.shp)}
        else { mapview::mapview(ImpSel.shp, zcol = Vcolor)}
}


##.###################################################################################33
## I. Calculs distances/ portée du réseau de chaque implantation ====
##.#################################################################################33
# 1.  On charge les fichiers ===========================
# ici je vais reprendre et documenter le travail d'Hélène
# Hélène travaille pas mal en tidyverse, on va donc charger ses bibliotheques 
# je simplifierais en R base au besoin

library(dplyr) 
library(sf)

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
                select(lat, lng) %>% 
                as.matrix()

#matrice d'arrivé
to.dat <- relation_total.dat %>% 
                select(lat_link, lng_link) %>% 
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
# calcule distance 

TliensDist <- Temp %>%
    mutate(distance = st_distance(x = geom_impl, y = geom_link, by_element = TRUE)/1000) %>% 
    mutate(distance=as.numeric(distance))


Temp2<-TliensDist %>% 
    group_by(idimplantation,usual_name,modAgreg) %>% 
    summarise (meandist=mean(distance),
               mindist=min(distance),
               maxdist=max(distance))
Tliensres1<-Tliensres1 %>% 
    left_join(Temp2,by=c("idimplantation","usual_name","modAgreg"))

ggplot(filter(Tliensres1,nbLien>1),
       aes(x=nbLien,y=meandist, colour=modAgreg))+
    geom_point()
# scale_colour_manual(values = cols)
rm(Temp,Temp2)
#rm(TliensDist)

#transformation du bipoint en polyligne sur distancs non nulles  
Temp<-filter(TliensDist,distance<=0)

Temp2 <- TliensDist %>%
    filter(distance > 0) %>%
    select(idfactoid,geom_link,geom_impl) %>%
    gather(key = "type", value = "geom", -idfactoid) %>% # On passe en tableau long, pour avoir 2 lignes par entitÃ© : une qui donnera la geom du centroide, et une seconde pour la geom de l'entitÃ©
    st_sf(sf_column_name = "geom") %>% # on convertie l'objet en sf
    group_by(idfactoid) %>% # le group_by + summarise font une fusion des points, on obtient donc du multipoint avec pour chaque ligne 2 points correspondant Ã  centroide + geometrie
    summarise() %>%
    st_cast(to = "LINESTRING")# On convertie l'objet multipoint en linÃ©aire : il trace une ligne entre les 2 points

TliensFin <- Temp2 %>%
    left_join(TliensX, by = "idfactoid") %>%
    mutate(modalite = iconv(modalite, to = "UTF-8"))

mapview(TliensFin,zcol="modAgreg")
mapview(TliensFin,zcol="modalite")

mapview(TliensFin,zcol="modAgreg",burst=TRUE)
mapview(TliensFin,zcol="modalite",burst=TRUE,col.regions = cols)

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








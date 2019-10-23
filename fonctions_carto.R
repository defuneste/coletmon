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
                     col.regions = "red",  alpha = 0.5,                  # couleur du fond et type
                     layer.name = "Diocèses" ) +
        
    if (Vcolor=="") {mapview::mapview(ImpSel.shp)}
        else { mapview::mapview(ImpSel.shp, zcol = Vcolor)}
 }

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
    install.packages("un_package",  dependencies=c("Depends", "Suggests"))
# on le charge
library(sf)

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
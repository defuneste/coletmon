# script de chargement pour les données de col&mon
# 28-11-2019
# Attention les liens relatifs sont pour un os linux !!!!
# norme de codage :
# .dat est un df ou tible
# .shp est un sf
##.###################################################################################33
## I. Chargement des données de col&mon ====
##.#################################################################################33

# 2 - Imports des données =======

# je part de mes csv en UTF8
# faire une version qui part des T0imp et TOnew
fait.dat <- read.csv("data/fait.txt")
implantation.dat <- read.csv("data/implantation.txt")
# ici je drop les X car quand j'ai fait un export en csv 
fait.dat <- subset(fait.dat, select = - X)
implantation.dat <- subset(implantation.dat, select = - X)

# 3 - Mise en forme =======

relation.dat <- fait.dat[fait.dat$caracNew == "Relations" ,] # on ne garde que les relations
# on extrait les Déplacement 
relation.dat <- relation.dat[relation.dat$modaNiv1 != "Déplacement",]
relation <- subset(relation.dat, !(relation.dat$modaNiv1 == "hiérarchique asc. Ecole" | relation.dat$modaNiv1 == "hiérarchique ascendante") ) # on enleve les doublons
relation <- subset(relation, select =  c("idimplantation", "usual_name", "fklinked_implantation","linked_implantation_name", "modaNiv1")) # on ne garde que les noms et noms liées
# on drop les facteurs non pris en compte suite aux subset de relations
relation$usual_name <- factor(relation$usual_name)
relation$linked_implantation_name <- factor(relation$linked_implantation_name)
relation$modaNiv1 <- factor(relation$modaNiv1)

# petite description de ce que l'on charge

#ls.str() 

print("relation.dat est un subset de fait sur caracNew == Relations, on y a retirer les Déplacement")

print("relation est un subset plus leger de relation.dat retirant les relations hiérarchiques ascendantes, pour ne pas doublonner avec les descendantes. Relation comporte aussi moins de modalités")

print("implantation.dat correspond aux implantations")

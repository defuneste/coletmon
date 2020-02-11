
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
library(leaflet)

 # on mesure la distance sur la geometry 

source("chargement_graphs.R")

# un ggplot rapide pour regarder
ggplot(relation_total.shp, aes(modaNiv1, distance_km, color = modaNiv1)) +
                                            geom_boxplot()

# on enleve les facteurs inutilisés dans modalite
relation_total.shp$modalite <- factor(relation_total.shp$modaNiv1)

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
plot_ly(st_drop_geometry(relation_total.shp), y = ~ distance_km, color = ~ modaNiv1, type = "box", 
        # ici on affaiche les valeur et on rajoute le nom/id des implantations dans les deux sens
        text = ~paste(paste(relation_total.shp$usual_name, relation_total.shp$idimplantation), 
                      paste(relation_total.shp$usual_name_link, relation_total.shp$idimpl_link), sep = "\n")) %>% 
    layout(yaxis = y)


library(mapview)
mapview(st_transform(relation_total.shp, 4326), zcol ="modaNiv1", burst=TRUE)

##.###################################################################################33
## II. des evolutions au cours du temps ====
##.#################################################################################33

names(relation_total.shp)

# l'idee est de refaire un jeux de données ou le temps est cumulatif pour une animation
# on dessine une frame par periode et il faut donc que l' ensemble des liens present à ce moment soient présent dans 
# le jeux de données 
relation_slim <- relation_total.shp %>%  # on va faire un jeux de données plus léger
    st_drop_geometry()   # on drop la geometry
    # on ne garde que ce qui est utile
    # select(c(idimplantation, usual_name, modaNiv1, idimpl_link, usual_name_link, lat, lng,date_startC, date_stopC, lat_link, lng_link ))  
    # on impute les NA avec la valeur mins
    #mutate(date_startC = ifelse(is.na(relation_total.shp$date_startC), min(relation_total.shp$date_startC, na.rm = T), relation_total.shp$date_startC))

relation_slim <- relation_slim[!is.na(relation_slim$date_startC),]

interval_sup <-  seq(375,1825,50)

#duplication par pas de temps
relation_slim_intervall_all <- merge(relation_slim, interval_sup ,all = TRUE)

# un nom plus cool 
names(relation_slim_intervall_all)[names(relation_slim_intervall_all) == "y"] <-  "interval_inf"

# interval_sup
relation_slim_intervall_all$interval_sup <- relation_slim_intervall_all$interval_inf + 50

# un filtre pour avoir sur le bon pas de temps
test_interval <-  relation_slim_intervall_all[relation_slim_intervall_all$interval_sup > relation_slim_intervall_all$date_startC ,]
test_interval2 <- test_interval[test_interval$interval_inf <= test_interval$date_stopC,]

summary(test_interval2)

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
        data = test_interval2,
        x = ~lng, xend = ~lng_link,
        y = ~lat, yend = ~lat_link,
        alpha = 0.5, size = I(1.5), hoverinfo = "none", 
        frame = ~interval_inf, color = ~modaNiv1
    ) %>%
    layout(
        title = 'test',
        geo = geo, showlegend = TRUE
    )

# source d'info    https://stackoverflow.com/questions/40350925/how-to-create-a-choropleth-map-using-plot-geo
# autre option https://stackoverflow.com/questions/36554605/cant-loop-with-rs-leaflet-package-to-produce-multiple-maps/36587525#36587525


library(crosstalk)

shared_interval <- SharedData$new(test_interval2)

geo_relation <- plot_geo() %>%
    add_markers(
        data = implantations_slim, x = ~lng, y = ~lat, text = ~paste(idimplantation, usual_name),
        hoverinfo = "text", alpha = 0.5
    ) %>%
    add_segments(
        data = shared_interval,
        x = ~lng, xend = ~lng_link,
        y = ~lat, yend = ~lat_link,
        alpha = 0.5, size = I(1.5), hoverinfo = "none", color = ~modaNiv1
    ) %>%
    layout(
        title = 'test',
        geo = geo, showlegend = TRUE
    )

hist_relation <- plot_ly(shared_interval, x = ~interval_sup) %>% add_histogram()
temps_distance <- plot_ly(shared_interval, x = ~interval_sup, y = ~ distance_km) %>% add_markers()



bob <- filter_slider("dist", "Distance", shared_interval, column=~distance_km, step=50, width=250)

subplot(hist_relation, geo_relation) %>% 
    highlight(on = "plotly_click")

###### leaflet solution 


jim <- leaflet() %>% 
    addTiles() %>%
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addPolylines(data = shared_interval, lng = ~lng, lat = ~lat)

bscols(bob, temps_distance) 

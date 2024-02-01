rm(list=ls()); set.seed(1234)
###
pacman::p_load(osrm, plyr, tidyverse, sf, leaflet, fontawesome, GGally)
# library(osrm)
# library(plyr)
# library(tidyverse)
# library(sf)
# library(leaflet)
###

# Load data ---------------------------------------------------------------

### hospital data with coordinates
hosp <- read_csv('Data/Geospatial Data/hlbc_emergencyrooms.csv')

# remove extra row for surrey memorial
hosp <- hosp %>% group_by(Facility) %>% slice(1) %>% ungroup()

# remove special characters in facility names to avoid plotting errors
hosp$Facility <- iconv(x=hosp$Facility, from = "UTF-8", to = "UTF-8", sub = "")

# labels for mapping
hosp.labs <- sprintf("%s", hosp$Facility) %>% lapply(htmltools::HTML)

### CHSA shapefile data
chsa <- st_read('Data/Geospatial Data/CHSA_2018/CHSA_2018.shp')

# transform, add colour palette, labels
chsa <- st_transform(chsa, crs = 'WGS84')
chsa.pal <- colorFactor('RdYlBu', chsa$HA_ID)
chsa.labels <- sprintf("%s (%s)", chsa$CHSA_Title, chsa$HA_Name) %>% lapply(htmltools::HTML)

# Quick map plot ---------------------------------------------------------------

# # CHSA plus hospitals
# chsa.test.map <- leaflet() %>%
#   addTiles() %>% addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(data = chsa,
#               fillColor = ~chsa.pal(HA_ID),
#               stroke = T, weight = 1,
#               opacity = 10, color = "#666", dashArray = "1", fillOpacity = 0.1,
#               label = chsa.labels,
#               highlight = highlightOptions(
#                 weight = 1.5, color = "black", dashArray = "",fillOpacity = 0.3,
#                 bringToFront = F)
#               ) %>%
#   ## add circles after polygons so that they are plotted on top
#   addCircles(data = hosp, ~LONGITUDE, ~LATITUDE,
#              label = ~hosp.labs, popup = ~hosp.labs, 
#              color = 'red', fillColor = 'red',
#              opacity = 1, radius = 10, stroke = T,
#              # color = ~rtvs.pal(HA_Name), fillColor = ~rtvs.pal(HA_Name),
#              highlightOptions = highlightOptions(bringToFront = T)
#              )
# 
# # view map
# chsa.test.map
# 
# # save to html and try embedding in PBI
# htmlwidgets::saveWidget(chsa.test.map, file = 'Mapping/chsa_test_map.html')

# CHSA centroids ----------------------------------------------------------

# calculate centroids
chsa$centroid <- chsa %>% st_transform(32617) %>% 
  st_centroid() %>% st_transform('WGS84') %>% st_geometry()

# # quick plot to check
# plot(st_geometry(chsa))
# plot(st_set_geometry(chsa, 'centroid')[, 0], add = T, col = 'red', pch = 19)

# select centroid coordinates
chsa.centroids <- chsa %>% as_tibble() %>% 
  ## rm shape geometry
  select(-geometry) %>% 
  ## clean up and split long/lat
  mutate(centroid.char = sub('c\\(', '', as.character(centroid))) %>% 
  mutate(centroid.char = sub('\\)', '', centroid.char)) %>% 
  mutate(long = as.numeric(str_split_fixed(centroid.char, ', ', 2)[,1]),
         lat = as.numeric(str_split_fixed(centroid.char, ', ', 2)[,2])) %>% 
  select(-centroid.char)


# Prep and haversine dist -------------------------------------------------

# create grid of pairs of each CHSA to hosp
hosp.chsa.combined <- expand.grid(chsa.centroids$CHSA_Name, hosp$Facility)

# join coordinate columns
hosp.chsa.combined <- hosp.chsa.combined %>% 
  select(CHSA_Name = Var1, Facility = Var2) %>% 
  left_join(., chsa.centroids %>% select(CHSA_Name, chsa.long = long, chsa.lat = lat)) %>% 
  left_join(., hosp %>% select(Facility, hosp.long = LONGITUDE, hosp.lat = LATITUDE))

### Hospital haversine dist
# for each CHSA-hosp pair, calculate haversine distance and keep closest [5??]
hosp.chsa.combined <- hosp.chsa.combined %>% 
  ddply(., .(CHSA_Name), function(x){
    x$dist.hav <- geosphere::distHaversine(x[ , c('chsa.long', 'chsa.lat')], 
                                           x[ , c('hosp.long', 'hosp.lat')])
    x <- x %>% 
      ## convert distances to km
      mutate(dist.hav = dist.hav / 1000) %>% 
      arrange(dist.hav) %>% 
      slice(1:5)
    return(x)
  })

# check returned distances -- note distances are in kilometres
summary(hosp.chsa.combined)
hosp.chsa.combined %>% arrange(dist.hav) %>% head(10)
hosp.chsa.combined %>% arrange(dist.hav) %>% tail(10)
# ggplot(hosp.chsa.combined, aes(dist.hav)) + geom_boxplot()
qplot(hosp.chsa.combined$dist.hav)

# add combined CHSA/hosp name for matching later
hosp.chsa.combined <- hosp.chsa.combined %>% 
  mutate(CHSA.Facility = paste(CHSA_Name, Facility, sep = ' - '))

# Calculate street distances ----------------------------------------------

### NOTE - street dist/dur changes over time

### street dist - hospital
# for each CHSA-hosp pair, calculate street distance and time
# distances in km; duration in minutes
all.trips <- hosp.chsa.combined %>%
  ## trips for one of Haida Gwaii CHSA not calculating
  filter(!grepl('Haida Gwaii South', CHSA_Name)) %>%
  ## filters for testing
  # sample_n(10) %>%
  # slice(1:4) %>%
  ddply(., .(CHSA_Name, Facility, CHSA.Facility), function(x){
    x <- data.frame(
      ## Oct 22 update -- no longer accepting additional place var...
      # place = c(x$CHSA_Name, x$Facility),
                    long = c(x$chsa.long, x$hosp.long),
                    lat = c(x$chsa.lat, x$hosp.lat))

    ## use route to calculate one-way dist/dur
    trip <- osrmRoute(loc = x)

    ## osrmtrip computes both directions...
    # trip <- osrmTrip(loc = x)
    # trip <- trip[[1]]$trip

    return(trip)
  })

### SAVE OSRM output
write_rds(all.trips, 'Data/Geospatial Data/CHSA-to-Hosp-distances_OSRM-output-all.RDS')

### READ in SAVED output
all.trips <- read_rds('Data/Geospatial Data/CHSA-to-Hosp-distances_OSRM-output-all.RDS')

## OLD - for trip option above
# # remove 50% of trips where hospital is the start
# all.trips <- all.trips %>% 
#   mutate(CHSA.Facility = paste(CHSA_Name, Facility, sep = ' - ')) %>% 
#   filter(CHSA_Name == start) %>% 
#   st_as_sf()

## NEW - convert to sf, add long/lat
all.trips <- all.trips %>% 
  left_join(hosp.chsa.combined) %>% 
  select(-src, -dst) %>% 
  st_as_sf()
all.trips

# which trips were not calculated?
hosp.chsa.combined %>% filter(!(CHSA.Facility %in% all.trips$CHSA.Facility))

# which hospitals not included as end destinations? 
# >> all when considering each CHSA to nearest 5 hospitals
hosp %>% 
  filter(!(Facility %in% all.trips$Facility)) %>% 
  select(Facility)

### save shortest trips to plot
all.trips <- all.trips %>% group_by(CHSA_Name) %>% 
  arrange(distance) %>% slice(1) %>% ungroup()

# which hospitals not included as end destinations? >> 15 hospitals
hosp %>% 
  filter(!(Facility %in% all.trips$Facility)) %>% 
  select(Facility)

# Plot trips -----------------------------------------------------

# update labs
hosp.labs <- sprintf("%s", hosp %>% filter(Facility %in% all.trips$Facility) %>% pull(Facility)) %>% 
  lapply(htmltools::HTML)
chsa.centroid.labs <- sprintf("%s", chsa.centroids %>% filter(CHSA_Name %in% all.trips$CHSA_Name) %>% 
                                pull(CHSA_Name)) %>% lapply(htmltools::HTML)

# line colour by distance and label
trip.pal <- colorNumeric('viridis', domain = all.trips$distance)
trip.labels <- sprintf("%s to %s (%.0f km)", all.trips$CHSA_Name, 
                       all.trips$Facility, all.trips$distance) %>% 
  lapply(htmltools::HTML)
# trip.labels

# add icons for hospitals
hosp.icon <- makeAwesomeIcon(text = fa('hospital'), 
                             markerColor = 'red', iconColor = 'white')

### plot
leaflet() %>% 
  addTiles() %>% addProviderTiles(providers$CartoDB.Voyager) %>% 
  addPolygons(data = chsa, fillColor = ~chsa.pal(HA_ID), label = chsa.labels,
              stroke = T, weight = 1, opacity = 10, color = "#666", 
              dashArray = "1", fillOpacity = 0.1
              ## remove polygon highlighting
              # highlight = highlightOptions(weight = 1.5, color = "black", dashArray = "",
              #                              fillOpacity = 0.3, bringToFront = F)
              ) %>% 
  ## hospital icon pins >> map only hospitals used as endpoints?
  addAwesomeMarkers(data = hosp %>% filter(Facility %in% all.trips$Facility),
                    ~LONGITUDE, ~LATITUDE,
                    icon = hosp.icon,
                    label = ~as.character(hosp %>% filter(Facility %in% all.trips$Facility) %>% pull(Facility)), 
                    popup = ~as.character(hosp %>% filter(Facility %in% all.trips$Facility) %>% pull(Facility))
  ) %>%
  # addCircles(data = hosp %>% filter(Facility %in% all.trips$Facility), 
  #            ~LONGITUDE, ~LATITUDE, label = ~hosp.labs, popup = ~hosp.labs,
  #            opacity = 1, radius = 200, stroke = T, color = 'red', fillColor = 'red',
  #            highlightOptions = highlightOptions(bringToFront = T)
  #            ) %>% 
  addCircles(data = chsa.centroids %>% filter(CHSA_Name %in% all.trips$CHSA_Name), 
             ~long, ~lat, label = ~chsa.centroid.labs, popup = ~chsa.centroid.labs,
             opacity = 1, radius = 200, stroke = T, color = 'blue', fillColor = 'blue', 
             highlightOptions = highlightOptions(bringToFront = T)
             ) %>% 
  addPolylines(data = st_as_sf(all.trips), 
               label = ~trip.labels, popup = ~trip.labels,
               color = ~trip.pal(distance), opacity = 1,
               highlightOptions = highlightOptions(bringToFront = T, opacity = 1)
               ) %>% 
  addLegend("bottomleft", title = 'Street distance (km)', 
            pal = trip.pal, values = all.trips$distance, opacity = 1
            )

# Plot CHSA type heatmap -------------------------------------------------------------------------

# join chsa geometry to trip distance
chsa.dist <- all.trips %>% 
  as_tibble() %>% select(-geometry) %>% 
  left_join(chsa, .)

# update labs - use all hospitals
hosp.labs <- sprintf("%s", hosp$Facility) %>% 
  lapply(htmltools::HTML)
chsa.centroid.labs <- sprintf("%s", chsa.centroids$CHSA_Name) %>% 
  lapply(htmltools::HTML)

# chsa colour by distance and label
# if using quantile color, then need to adjust legend??
chsa.pal <- colorQuantile('viridis', domain = chsa.dist$distance, 
                          probs = seq(0, 1, 0.2))
chsa.labels <- sprintf("%s to %s (%.0f km)", chsa.dist$CHSA_Name, 
                       chsa.dist$Facility, chsa.dist$distance) %>% 
  lapply(htmltools::HTML)

leaflet() %>% 
  addTiles() %>% addProviderTiles(providers$CartoDB.Voyager) %>% 
  addPolygons(data = chsa.dist, fillColor = ~chsa.pal(distance), label = chsa.labels,
              stroke = T, weight = 1, opacity = 10, color = "#666", 
              dashArray = "1", fillOpacity = 0.5
              ## remove polygon highlighting
              # highlight = highlightOptions(weight = 1.5, color = "black", dashArray = "",
              #                              fillOpacity = 0.3, bringToFront = F)
  ) %>% 
  ## hospital icon pins >> map only hospitals used as endpoints?
  addAwesomeMarkers(data = hosp,
                    ~LONGITUDE, ~LATITUDE,
                    icon = hosp.icon,
                    label = ~as.character(hosp$Facility), 
                    popup = ~as.character(hosp$Facility)
  ) %>%
  # addCircles(data = hosp %>% filter(Facility %in% all.trips$Facility), 
  #            ~LONGITUDE, ~LATITUDE, label = ~hosp.labs, popup = ~hosp.labs,
  #            opacity = 1, radius = 200, stroke = T, color = 'red', fillColor = 'red',
  #            highlightOptions = highlightOptions(bringToFront = T)
  #            ) %>% 
  addCircles(data = chsa.centroids, 
             ~long, ~lat, label = ~chsa.centroid.labs, popup = ~chsa.centroid.labs,
             opacity = 1, radius = 200, stroke = T, color = 'blue', fillColor = 'blue', 
             highlightOptions = highlightOptions(bringToFront = T)
  ) %>% 
  addLegend("bottomleft", title = 'Road distance (km)', 
            pal = chsa.pal, values = chsa.dist$distance, opacity = 1
  )


# OLD
# plot(st_geometry(chsa))
# plot(st_set_geometry(chsa, 'centroid')[, 0], col = 'red', pch = 19, add = T)
# plot(st_geometry(st_as_sf(all.trips)), col = 'black', lwd = 4, add = T)
# plot(st_geometry(st_as_sf(all.trips)), col = c('blue', 'green', 'yellow', 'purple'), lwd = 1, add = T)

# Summary and save output ----------------------------------------------------------

# remove geometry
all.trips.nogeo <- all.trips %>% 
  as_tibble() %>% select(-geometry)

# add CHSA pop
all.trips.nogeo <- left_join(all.trips.nogeo,
                             chsa %>% as_tibble() %>% select(matches('CHSA_|HA_'))
                             )

# summary of distances and duration
all.trips.nogeo %>% 
  bind_rows(all.trips.nogeo %>% mutate(HA_Name = 'COMBINED')) %>% 
  group_by(HA_Name) %>% 
  summarise(across(.cols = c(duration, distance),
                   .fns = list(median = median, q1 = ~ quantile(.x, 0.25), q3 = ~ quantile(.x, 0.75),
                               min = min, max = max,
                               mean = mean, sd = sd),
                   .names = "{.col}.{.fn}"))
# weighted mean
all.trips.nogeo %>% 
  bind_rows(all.trips.nogeo %>% mutate(HA_Name = 'COMBINED')) %>% 
  group_by(HA_Name) %>% 
  mutate(CHSA_popweight = CHSA_Pop16 / sum(CHSA_Pop16)) %>% 
  summarise(dist.mean.w = weighted.mean(distance, CHSA_popweight),
            dist.mean = mean(distance),
            dur.mean.w = weighted.mean(duration, CHSA_popweight),
            dur.mean = weighted.mean(duration))

# summary by CHSA urban/rural class
## Urban/Rural Class summary (1-7)
all.trips.nogeo %>%
    group_by(CHSA_UR_Cl) %>%
    summarise(dist=mean(distance))
## Urban/Rural only split
all.trips.nogeo <- all.trips.nogeo %>%
    mutate (UR = case_when(
      startsWith(CHSA_UR_Cl, "1") ~ "Urban",
      startsWith(CHSA_UR_Cl, "2") ~ "Urban",
      startsWith(CHSA_UR_Cl, "3") ~ "Urban",
      startsWith(CHSA_UR_Cl, "4") ~ "Urban",
      startsWith(CHSA_UR_Cl, "5") ~ "Rural",
      startsWith(CHSA_UR_Cl, "6") ~ "Rural",
      startsWith(CHSA_UR_Cl, "7") ~ "Rural"
      ))
all.trips.nogeo %>%
    group_by(UR) %>%
    summarise(mean(distance)) 
all.trips.nogeo %>%
    group_by(UR) %>%
    summarise(mean(duration))

# boxplot
all.trips.nogeo %>% 
  bind_rows(all.trips.nogeo %>% mutate(HA_Name = 'COMBINED')) %>% 
  select(HA_Name, duration, distance) %>% 
  gather(k, v, -HA_Name) %>% 
  ggplot(aes(x = v, fill = HA_Name)) + coord_flip() +
  geom_boxplot() + facet_wrap(~ k, scales = 'free')

# longest and shortest trips by dist
all.trips.nogeo %>% 
  filter(min_rank(distance) <= 5 | min_rank(desc(distance)) <= 5) %>% 
  select(CHSA.Facility, distance, duration) %>% 
  arrange(distance)

# longest and shortest trips by duration >> some long trips but short distances
all.trips.nogeo %>% 
  filter(min_rank(duration) <= 5 | min_rank(desc(duration)) <= 5) %>% 
  select(CHSA.Facility, distance, duration) %>% 
  arrange(distance)

# correlations - dist v. dur, dist v. CHSA pop/area?
all.trips.nogeo %>% 
  select(HA_Name, duration, distance, dist.hav, CHSA_Pop16, CHSA_Area) %>% 
  mutate(CHSA_dens = CHSA_Pop16 / CHSA_Area) %>% 
  ggpairs(columns = 2:7, mapping = aes(color = HA_Name),
          diag = list(continuous = wrap('densityDiag', alpha = 0.3)),
          lower = list(continuous = wrap("smooth", alpha = 0.3, size = 0.1, se = F)))

# save data
# write_rds(all.trips.nogeo, 'Data/Geospatial Data/CHSA-to-Hosp-distances.RDS')
# write_csv(all.trips.nogeo, 'Data/Geospatial Data/CHSA-to-Hosp-distances.csv')

# TEST-old ----------------------------------------------------------------

# hosp <- hosp %>% 
#   select(Hosp, LONGITUDE, LATITUDE, Facility) %>% 
#   slice(1:5)
# hosp
# 
# osrmRoute(loc = hosp %>% select(-Facility) %>% as.data.frame(), 
#           overview = F)
# 
# trips <- osrmTrip(loc = hosp, returnclass = 'sf')
# trips
# 
# mytrip <- trips[[1]]$trip
# mytrip
# 
# plot(st_geometry(mytrip), col = 'black', lwd = 4)
# plot(st_geometry(mytrip), col = c('red', 'white'), lwd = 1, add = T)
# 
# projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# hosp <- st_as_sf(x = hosp,                         
#                coords = c("LONGITUDE", "LATITUDE"),
#                crs = projcrs)
# 
# plot(st_geometry(hosp), pch = 21, bg = 'red', cex = 3, add = T)
# 
# https://www.google.ca/maps/dir/100+Mile+District+General+Hospital,+Cedar+Avenue,+100+Mile+House,+BC/Alexis+Creek+Health+Centre,+Morton+Street,+Alexis+Creek,+BC/Arrow+Lakes+Hospital,+1+Avenue+Northeast,+Nakusp,+BC/Abbotsford+Regional+Hospital+and+Cancer+Centre,+Marshall+Road,+Abbotsford,+BC/Ashcroft+Hospital+and+Community+Health+Care+Centre,+Ashcroft,+BC/100+Mile+District+General+Hospital,+Cedar+Avenue,+100+Mile+House,+BC/@50.7305572,-122.5923965,7.27z/data=!3m1!5s0x53806c1ae2a62553:0x5eb527c7947489c6!4m43!4m42!1m5!1m1!1s0x53806c1a59b94711:0x988a6e3c3c9e95b2!2m2!1d-121.2921997!2d51.6386083!1m10!1m1!1s0x5478cd884750a715:0xd56e3290444a51f0!2m2!1d-123.2799231!2d52.083812!3m4!1m2!1d-119.7622196!2d50.470468!3s0x537e14fc41a8a215:0x287aee38c073171!1m5!1m1!1s0x537c7158d53b74b9:0xaab08c554449c996!2m2!1d-117.7952257!2d50.2387294!1m5!1m1!1s0x54844aa89774f10b:0x3025afec94d1b3a8!2m2!1d-122.3128441!2d49.0372181!1m5!1m1!1s0x54803f5ff55b1cef:0x3db1af68f1b0c20f!2m2!1d-121.2799854!2d50.7346647!1m5!1m1!1s0x53806c1a59b94711:0x988a6e3c3c9e95b2!2m2!1d-121.2921997!2d51.6386083!3e0


#remotes::install_github("GIScience/openrouteservice-r")
set.seed(1234)
library(openrouteservice)
#ors_api_key(Sys.getenv("ors_key")) ## we pass the api_key using sys.getenv("ORS_API_KEY"_)
library(osmdata)
library(mapview)
library(tidyverse)
library(sf)
library(leaflet)
library(htmlwidgets)
library(htmltools)

n_bars <- 3

st_x = function(x) st_coordinates(x)[,1]
st_y = function(x) st_coordinates(x)[,2]

# opencage_return_start <- opencage::oc_forward(placename = "Université Laval, Quebec", 
#                                               limit = 1 , # just the best result
# )
# start_lat = opencage_return_start[[1]]$oc_lat
# start_lon = opencage_return_start[[1]]$oc_lng

# opencage_return_end <- opencage::oc_forward("Parc Munn, Quebec", 
#                                             limit = 1 , # just the best result
# )
# end_lat = opencage_return_end[[1]]$oc_lat
# end_lon = opencage_return_end[[1]]$oc_lng

## let's hardcode lat-lon values for unvertsité laval and parc munn to bypass API problems with opencage
start_lat =  46.78154
start_lon = -71.27654

end_lat =46.81809
end_lon =  -71.21886

mespoints <- tibble(name = c("start", "end"),
                    lat = c(start_lat, end_lat),
                    lon = c(start_lon, end_lon),
                    osm_id = c("start", "end")) %>%
  st_as_sf(., coords= c("lon", "lat"), crs = 4326)
mespoints$x <- st_x(mespoints)
mespoints$y <- st_y(mespoints)

input_boundingbox_buffer = 0.03
bbox = c(min(start_lon, end_lon)-input_boundingbox_buffer, min(start_lat, end_lat)-input_boundingbox_buffer, max(start_lon, end_lon)+input_boundingbox_buffer, max(start_lat, end_lat)+input_boundingbox_buffer)

pubs <- opq(bbox = bbox)%>%
  add_osm_feature(key = "amenity", value = "pub")  %>%
  osmdata_sf(.) %>% 
  .$osm_points  %>% 
  select(name, osm_id)


bars <- opq(bbox = bbox)%>%
  add_osm_feature(key = "amenity", value = "bar")  %>%
  osmdata_sf(.) %>% 
  .$osm_points  %>% 
  select(name, osm_id)

pubs_bars <- pubs %>% 
  rbind(bars) %>%
  filter(!is.na(name))

pubs_bars$x <- st_x(pubs_bars)
pubs_bars$y <- st_y(pubs_bars)



if(nrow(pubs_bars)> 49){
  message("more than 50 pubs, sampling 49 to allow use of open route services duration matrix api")
  pubs_bars <- pubs_bars[sample(nrow(pubs_bars), 49), ]
}


points <- pubs_bars %>% 
  rbind(mespoints %>% filter(name == "start"))

end <- mespoints %>% filter(name == "end")

allpoints <- rbind(points, end)

distance_matrix_input  <-  pubs_bars %>% 
  filter(!is.na(name))%>%
  rbind(mespoints) %>%
  st_set_geometry(NULL)

z <- distance_matrix_input %>% 
  select(x,y) %>%
  ors_matrix(., 
             metrics = c("duration", "distance"), 
             units = "km",
             profile = "foot-walking",
             api_key = Sys.getenv("ORS_API_KEY"))

zz <- z$durations %>% as_tibble()
colnames(zz)  <- distance_matrix_input$osm_id
zz$origin = distance_matrix_input$osm_id

durations <- zz %>% 
  gather(key= destination, value  = duration, -origin)



#initialiser les trajets
open <- vector("list", 100000)
closed <- vector("list", 100000)
open[[1]] <- "start"

# get distance to data
d_parcouru <- c(rep(NA_integer_, 100000))
d_parcouru[1] <- 0

#get distance to end
d_left <- c(rep(NA_integer_, 100000))
d_left[1] = durations %>% 
  filter(origin == "start", destination == "end") %>% 
  pull(duration) 
  #as.numeric(st_distance(end, points %>% filter(name== "start")))

# get total distance
d_total <- c(rep(NA_integer_, 100000))
d_total[1] = d_parcouru[1] + d_left[1]

d_parcouru_closed <- c(rep(NA_integer_, 100000))

# while we havent found the best path and there are still open paths..
win  <- 0
stop <- 0
round = 1
while (win==0 & stop == 0){
  
  # expand best trip
  k_to_expand <- which.min(d_total)
  
  message("round ", round, " expanding ", open[k_to_expand])
  
  #last_point <- lapply(l[[k_to_expand]], tail,1)
  last_point <- open[[k_to_expand]][length(open[[k_to_expand]])]
  
  
  # get list of possible destinations  (do not go back to already visited, and only go to end after 4 points including start)
  if(length(open[[k_to_expand]] )== n_bars+1){
    dests <- "end"} else{
      dests <- points %>% filter(!(osm_id %in% open[[k_to_expand]])) %>% pull(osm_id)
    }
  #dests
  
  # find empty spots in list
  A = map_lgl(open, is.null) %>% which() %>% .[1:length(dests)]
  B = map_lgl(closed, is.null) %>% which() %>% .[1:length(dests)]
  i = 1 
  for (dest in dests){ 
    if (dest != "end"){
      open[[A[i]]] <- c(open[[k_to_expand]], dest)
      d_parcouru[A[i]] <- d_parcouru[k_to_expand] + 
        durations %>% 
        filter(origin == last_point, destination == dest) %>% 
        pull(duration)
        # as.numeric(st_distance(points %>% filter(name == last_point), 
        #                        points %>% filter(name == dest)))
      d_left[A[i]] <- durations %>% 
        filter(origin == dest, destination == "end") %>% 
        pull(duration)
        #as.numeric(st_distance(points %>% filter(name == dest), end))
      d_total[A[i]] <- d_parcouru[A[i]] + d_left[A[i]]
    }
    
    if (dest == "end"){
      closed[[B[i]]] <- c(open[[k_to_expand]], dest)
      d_parcouru_closed[B[i]] <- d_parcouru[k_to_expand] + 
        durations %>% 
        filter(origin == last_point, destination == "end") %>% 
        pull(duration)
        # as.numeric(st_distance(points %>% filter(name == last_point), 
        #                        end))
      if(d_parcouru_closed[B[i]] <= min(d_total, na.rm= T) & d_parcouru_closed[B[i]] <= min(d_parcouru_closed, na.rm = TRUE)){
        win <- 1
        message(paste0("final path = ", paste0(closed[[B[i]]]), " distance: ", d_parcouru_closed[B[i]]))}
    }
    i <- i + 1
  }
  # effacer le k qu'on vient d'expand
  closed[1:20]
  
  #l[k_to_expand] <- NA_character_
  open[k_to_expand] <- list(NULL)
  d_parcouru[k_to_expand]<- NA_integer_
  d_left[k_to_expand] <-  NA_integer_
  d_total[k_to_expand] <-  NA_integer_
  round = round+1
  if (round == 500){
    stop == 1
    message("reached round 500, cancelling")
  }
} # fin while

# create best_path as the crow flies
# best_path_linestring <- rbind(points,end) %>% 
#   left_join (tibble(osm_id = closed[[1]]) %>% 
#                mutate(rank = row_number())) %>%
#   arrange(rank) %>%
#   filter(!is.na(rank)) %>%
#   select(osm_id) %>%
#   summarize(., do_union = FALSE) %>%
#   st_cast("LINESTRING")

# create markers by use

markers <- allpoints %>% 
  left_join(tibble(osm_id = closed[[1]]) %>% 
              mutate(rank = row_number())) %>%
  mutate(type =factor(
    case_when(
      osm_id == "start"~ "start",
      osm_id == "end"~ "end",
      !is.na(rank) ~ "beer",
      TRUE ~ "nobeer")))%>%
  arrange(!is.na(rank), rank)

IconSet <- awesomeIconList(
  beer   = makeAwesomeIcon(icon= 'beer', markerColor = 'green', iconColor = 'white', library = "fa"),
  nobeer = makeAwesomeIcon(icon= 'beer', markerColor = 'red', iconColor = 'white', library = "fa"),
  start = makeAwesomeIcon(icon= 'flag-o ',  iconColor = 'white', library = "fa"),
  end = makeAwesomeIcon(icon= 'bed',  iconColor = 'white', library = "fa")
  
)


best_path_stops <-  markers %>%
  filter(!is.na(rank))%>%
  arrange(rank) %>%
  select(osm_id,x,y) %>%
  #rbind(end %>% select(osm_id,x,y))  %>%
  st_set_geometry(NULL)


x <- ors_directions(best_path_stops %>% select(x,y),
                    profile= "foot-walking",
                    api_key = Sys.getenv("ORS_API_KEY"))




tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
"))


beer <- markers %>%
  filter(osm_id != "start", osm_id != "end") %>%
  filter(!is.na(rank))%>%
  arrange(rank)  

title <- tags$div(
  tag.map.title, HTML("The shortest path home with ",
                      n_bars, 
                      " bars stops at ", 
                      paste0(beer$name, collapse = "", sep= ", "), 
                      " and will require crawling ",
                      floor(d_parcouru_closed[1] / 60),
                      " minutes.")
)  

leaflet(markers)  %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addAwesomeMarkers(icon = ~IconSet[type], popup =~ name) %>%
  addGeoJSON(x, fill=FALSE, color = "red")%>%
  addControl(title, position = "topleft")


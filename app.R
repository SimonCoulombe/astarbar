library(remotes)
library(rsconnect) # for rsconnect::deployApp()
#remotes::install_github("GIScience/openrouteservice-r")

library(openrouteservice) # needs ORS_API_KEY in .Renviron
library(opencage) # needs OPENCAGE_KEY in .Renviron
library(osmdata)
library(sf)
library(leaflet)
library(mapview)
library(dplyr)
library(tidyr)
library(purrr)
#library(htmlwidgets)
library(htmltools)
library(bslib)
library(shiny)

st_x = function(x) st_coordinates(x)[,1]
st_y = function(x) st_coordinates(x)[,2]

set.seed(1234)

basemaps <- c("CartoDB.DarkMatter","CartoDB.Positron",  "OpenStreetMap",      "Esri.WorldImagery" ,"OpenTopoMap")  

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

IconSet <- awesomeIconList(
  beer   = makeAwesomeIcon(icon= 'beer', markerColor = 'green', iconColor = 'white', library = "fa"),
  nobeer = makeAwesomeIcon(icon= 'beer', markerColor = 'red', iconColor = 'white', library = "fa"),
  start = makeAwesomeIcon(icon= 'flag-o ',  iconColor = 'white', library = "fa"),
  end = makeAwesomeIcon(icon= 'bed',  iconColor = 'white', library = "fa")
)

ui <- 
  fluidPage(
    theme = bs_theme(
      bg = "#101010", 
      fg = "#FDF7F7", 
      primary = "#ED79F9", 
      base_font = font_google("Prompt"),
      code_font = font_google("JetBrains Mono")
    ),
    fluidRow(
      column(3,  textInput("input_start", h3("Where am I?"), 
                           value = "Université Laval, Quebec"),
             textInput("input_end", h3("Where is my bed?"), 
                       value = "Parc Munn, Quebec")  ,
             sliderInput("input_n_bars", h3("How many bars?"),
                         min = 1, max = 5, value = 3),
             radioButtons("input_how", "How am I getting home?",
                          choices = list("crawling" = 1, "biking"= 2, "driving" = 3),selected = 1),
             sliderInput(
               "input_boundingbox_buffer", 
               h3("Buffer around bounding box, in degrees."),
               min = 0, max = 1, step = 0.01, value = 0.03),
             actionButton("recalc", "Get me home!", 
                      #    icon("walking"),
                          
                          style="color: #101010; background-color: #ED79F9; border-color: #101010")
                          
                          #style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
             
      ),
      column(9,    leafletOutput("mapplot",  height="600"),
             fluidRow(textOutput("text")))
    )
  )

server <- function(input, output) {
  
  my_results1 <- eventReactive( input$recalc, {
    
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0, {
                   
                   incProgress(amount = 1/15, message= "Reticulating splines")        
                   how <- input$input_how
                   
                   if(how == 1){
                     my_profile = "foot-walking"
                     how_name = "crawling"
                   } else if(how == 2){
                     my_profile = "cycling-regular"
                     how_name= "biking"
                   } else if (how == 3){
                     my_profile = "driving-car"
                     how_name = "driving"
                   }
                   n_bars <- input$input_n_bars
                   incProgress(amount = 2/15, message= "Contacting OPENCAGE API to geocode start and end")        
                   opencage_return_start <- opencage::oc_forward(placename = input$input_start, 
                                                             limit = 1 , # just the best result
                   )
                   start_lat = opencage_return_start[[1]]$oc_lat
                   start_lon = opencage_return_start[[1]]$oc_lng
                   
                   
                   opencage_return_end <- opencage::oc_forward(input$input_end, 
                                                           limit = 1 , # just the best result
                   )
                   end_lat = opencage_return_end[[1]]$oc_lat
                   end_lon = opencage_return_end[[1]]$oc_lng
                   
                   
                   mespoints <- tibble(name = c("start", "end"),
                                       lat = c(start_lat, end_lat),
                                       lon = c(start_lon, end_lon),
                                       osm_id = c("start", "end")) %>%
                     st_as_sf(., coords= c("lon", "lat"), crs = 4326)
                   mespoints$x <- st_x(mespoints)
                   mespoints$y <- st_y(mespoints)
                   
                   if (as.numeric(st_distance(  mespoints %>% filter(name== "start"),  mespoints %>% filter(name== "end"))) < 100000){
                     incProgress(amount = 3/15, message= "Contacting OSMDATA API for pubs and bars locations")        
                     
                     cat(file=stderr(), paste0("Getting pubs \n"))
                     pubs <- opq(bbox = c(min(start_lon, end_lon)-input$input_boundingbox_buffer, min(start_lat, end_lat)-input$input_boundingbox_buffer, max(start_lon, end_lon)+input$input_boundingbox_buffer, max(start_lat, end_lat)+input$input_boundingbox_buffer))%>%
                       add_osm_feature(key = "amenity", value = "pub")  %>%
                       osmdata_sf(.) %>% 
                       .$osm_points  %>% 
                       select(name, osm_id)
                     
                     cat(file=stderr(), paste0("Getting bars \n"))
                     bars <- opq(bbox = c(min(start_lon, end_lon)-input$input_boundingbox_buffer, min(start_lat, end_lat)-input$input_boundingbox_buffer, max(start_lon, end_lon)+input$input_boundingbox_buffer, max(start_lat, end_lat)+input$input_boundingbox_buffer))%>%
                       add_osm_feature(key = "amenity", value = "bar")  %>%
                       osmdata_sf(.) %>% 
                       .$osm_points  %>% 
                       select(name, osm_id)
                     cat(file=stderr(), paste0("binding bars and pubs \n"))
                     pubs_bars <- pubs %>% rbind(bars)
                     cat(file=stderr(), paste0("DONEbinding bars and pubs \n"))
                     pubs_bars$x <- st_x(pubs_bars)
                     pubs_bars$y <- st_y(pubs_bars)
                     
                     bar_count <- nrow(pubs_bars)
                     
                     # drop unnamed bars if we can afford it
                     if(sum(!is.na(pubs_bars$name)) >=n_bars){
                       pubs_bars <- pubs_bars %>% filter(!is.na(name))
                     }
                     
                     if(nrow(pubs_bars)> 49){
                       message("more than 50 pubs, sampling 49 to allow use of open route services duration matrix api")
                       
                       pubs_bars <- pubs_bars[sample(nrow(pubs_bars), 49), ]
                     }
                     
                     
                     
                     cat(file=stderr(), paste0("crs pubs_bars= ",st_crs(pubs_bars) , "\n"))
                     cat(file=stderr(), paste0("crs mespoints= ",st_crs(mespoints) , "\n"))
                     cat(file=stderr(), paste0("binding pubs_barts and mespoints to create points\n"))
                     
                     points <- pubs_bars %>% 
                       # filter(!is.na(name))%>%
                       rbind(mespoints %>% filter(name == "start"))
                     cat(file=stderr(), paste0("done binding pubs_barts and mespoints to create points\n"))
                     cat(file=stderr(), paste0("binding end to points to create allpoints \n"))
                     end <- mespoints %>% filter(name == "end")
                     
                     allpoints <- rbind(points, end)
                     cat(file=stderr(), paste0("done binding end to points to create allpoints \n"))
                     
                     cat(file=stderr(), paste0("binding pubs_bars to mespoints again to create distance matrix input   \n"))                     
                     distance_matrix_input  <-  pubs_bars %>% 
                       #filter(!is.na(name))%>%
                       rbind(mespoints) %>%
                       st_set_geometry(NULL)
                     cat(file=stderr(), paste0("DONEbinding pubs_bars to mespoints again to create distance matrix input   \n"))                     
                                          
                     incProgress(amount = 4/15, message= "Contacting OpenRouteService API for duration matrix")        
                     z <- distance_matrix_input %>% 
                       select(x,y) %>%
                       openrouteservice::ors_matrix(., 
                                  metrics = c("duration", "distance"), 
                                  units = "km",
                                  profile = my_profile,
                                  api_key = Sys.getenv("ORS_API_KEY")
                                  )
                     
                     zz <- z$durations %>% as_tibble()
                     colnames(zz)  <- distance_matrix_input$osm_id
                     zz$origin = distance_matrix_input$osm_id
                     
                     durations <- zz %>% 
                       gather(key= destination, value  = duration, -origin)
                     
                     
                     incProgress(amount = 5/15, message= "Working: finding the shortest pub crawl")        
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
                       
                       if(round == 50){
                         incProgress(amount = 6/15, message= "Working: finding the shortest path (round 50)")        
                       }
                       if(round == 100){
                         incProgress(amount = 7/15, message= "Working: finding the shortest path (round 100)")        
                       }      
                       if(round == 200){
                         incProgress(amount = 8/15, message= "Working: finding the shortest path (round 200)")        
                       }      
                       if(round == 400){
                         incProgress(amount = 9/15, message= "Working: finding the shortest path (round 400)")        
                       }      
                       
                       
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
                     incProgress(amount = 10/15, message= "Contacting OpenRouteService for directions of best path")        
                     markers <- allpoints %>% 
                       left_join(tibble(osm_id = closed[[1]]) %>% 
                                   mutate(rank = row_number())) %>%
                       mutate(type =factor(
                         case_when(
                           osm_id == "start"~ "start",
                           osm_id == "end"~ "end",
                           !is.na(rank) ~ "beer",
                           TRUE ~ "nobeer"))) %>%
                       arrange(!is.na(rank), rank) # NA first so that the markers are hidden by more important
                     
                     
                     cat(file=stderr(), paste0("crs markers", st_crs(markers)   ," \n"))
                     cat(file=stderr(), paste0("crs end", st_crs(end)   ," \n"))
                     cat(file=stderr(), paste0("rbind markers to end \n"))
                     best_path_stops <-  markers %>%
                       filter(!is.na(rank))%>%
                       arrange(rank) %>%
                       select(osm_id,x,y) %>%
                       rbind(end %>% select(osm_id,x,y))  %>%
                       st_set_geometry(NULL)
                     cat(file=stderr(), paste0("Done rbind markers to end \n"))
                     
                     itinerary <- ors_directions(best_path_stops %>% select(x,y),
                                                 profile= my_profile,
                                                 output = "sf",
                                                 api_key = Sys.getenv("ORS_API_KEY"))
                     
                     
                     beer <- markers %>%
                       filter(osm_id != "start", osm_id != "end") %>%
                       filter(!is.na(rank))%>%
                       arrange(rank)  
                     
                     
                     # title <- tags$div(
                     #   tag.map.title, HTML("The shortest path home with ",
                     #                       n_bars, 
                     #                       " bars stops at ", 
                     #                       paste0(beer$name, collapse = "", sep= ", "), 
                     #                       " and will require ",
                     #                       how_name, " ",
                     #                       floor(d_parcouru_closed[1] / 60),
                     #                       " minutes.")
                     # ) 
                     title <-paste0("The shortest path home with ",
                                    n_bars, 
                                    " bars stops at ", 
                                    paste0(beer$name, collapse = "", sep= ", "), 
                                    " and will require ",
                                    how_name, " ",
                                    floor(d_parcouru_closed[1] / 60),
                                    " minutes.")
                     
                     
                     
                     list("cancelled" = 0,
                          "markers"= markers,
                          "itinerary" = itinerary,
                          "title"= title,
                          "bar_count" = bar_count
                     )
                     
                   } else{
                     #plus de 100 km 
                     list("cancelled" = 1,
                          "markers"= mespoints %>% mutate(type=factor("bed")),
                          "itinerary" = NULL,
                          "title"= NULL,
                          "bar_count" = NULL)
                   }
                 })
  })
  
  output$text <- renderText({
    req(my_results1)
    
    if (my_results1()$cancelled==0){
      paste0(my_results1()$title, ". Optimised from ", my_results1()$bar_count, " bars.  A sample of 49 is randomly selected if the number of bars is above 50.")
    } else {"more than 100 km between origin and destination, cancelled"}
    
  })
  
  output$mapplot <- renderLeaflet({
    req(my_results1)
    if (my_results1()$cancelled==0){

      z <- mapview(my_results1()$itinerary %>% select(geometry), # only select geometry column otherwise I get  "list columns are only allowed with raw vector contents" error
                   map.types = basemaps,
                   legend= FALSE,  color = c("#ED79F9"))
      z@map %>%
          addAwesomeMarkers(data = my_results1()$markers, icon = ~IconSet[type], popup =~ name)
    } else {
      leaflet(my_results1()$markers)  %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addAwesomeMarkers(icon = ~IconSet[type], popup =~ name) %>%
        addGeoJSON(x, fill=FALSE, color = "red")%>%
        addControl(title, position = "topleft")
      
    }
  })
  
}
shinyApp(ui, server)

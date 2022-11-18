This is a fun shiny app that takes your origin and destination and will return the shortest path that will stop at N bars.

dependencies:
opencage api  to geocode your start and end point.
osmdata api to get the list of nearby pubs and bars.
OpenRouteService api to get the walking time between all points

cool self made thing: I made a custom A* pathing algorithm.  It is similar to A* but it cannot go to the destination until it has visted N bars.

You must add create a .Renviron file with values for your "ORS_API_KEY" and "OPENCAGE_KEY" before deploying to shinyapps.io.

app.R is the shiny app and it works.
astarbar.R is the script that was used to create the MVP and then converted to an app.


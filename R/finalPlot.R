#' A function to plot the choosen OSM features
#'
#' @param streets A simple feature collection of OSM-features.
#' @param river A simple feature collection of OSM-features.
#' @param forest A simple feature collection of OSM-features.
#' @param specialForest A simple feature collection of OSM-features.
#' @param wood A simple feature collection of OSM-features.
#' @param singleTree A simple feature collection of OSM-features.
#' @param treeLine A simple feature collection of OSM-features.
#' @param grassland A simple feature collection of OSM-features.
#' @param meadow A simple feature collection of OSM-features.
#' @param park A simple feature collection of OSM-features.
#' @param garden A simple feature collection of OSM-features.
#' @param natureReserve A simple feature collection of OSM-features.
#' @param pov A simple feature point.
#' @param xlim A vector.
#' @param ylim A vector.
#' @param closestNature  A simple feature point.
#' @param route A simple feature line.
#'
#' @returns The final plot of the simple feature collections, the users chosen position (POV),
#'     the closest point of public available nature and the route to get there.
#'
#' @examples
#' finalPlot(streets, river, forest,
#'           specialForest, wood, singleTree,
#'           treeLine, grassland,
#'           meadow, park, garden,
#'           natureReserve, pov, xlim, ylim, closestNature, route)
#' @keywords internal
finalPlot <- function(streets, river, forest,                                   # finalPlot becomes a function using all the osm objects and the spatial variables
                      specialForest, wood, singleTree,
                      treeLine, grassland,
                      meadow, park, garden,
                      natureReserve, pov, xlim, ylim, closestNature, route){

  #plot
  plot <- ggplot()                                                              # variable plot becomes an empty ggplot object

  if(!is_empty(streets$osm_lines)){                                             # if the streets$osm_lines object is NOT empty
    plot <- plot+geom_sf(data = streets$osm_lines,                              # add a geom_sf to the plot, based on the data in streets$osm_lines
                         aes(color="Streets"),                                  # with the color "Streets" that is defined later in scale_color_manual
                         lwd = 2)                                               # and the linewidth
  }

  if(!is_empty(river$osm_lines)){                                               # repeat for all other osm objects...
    plot <- plot+geom_sf(data = river$osm_lines,
                         aes(color="River"),
                         lwd = 2)
  }

  if(!is_empty(forest$osm_polygons)){
    plot <- plot+geom_sf(data = forest$osm_polygons,
                         aes(fill="Forest")
    )
  }

  if(!is_empty(specialForest$osm_polygons)){
    plot <- plot+geom_sf(data = specialForest$osm_polygons,
                         aes(fill="Special Forest")
    )
  }

  if(!is_empty(wood$osm_polygons)){
    plot <- plot+geom_sf(data = wood$osm_polygons,
                         aes(fill="Wood")
    )
  }

  if(!is_empty(singleTree$osm_points)){
    plot <- plot+geom_sf(data = singleTree$osm_points,
                         aes(color="Single Tree"),
                         size = 2,
                         key_glyph=draw_key_rect)
  }

  if(!is_empty(treeLine$osm_lines)){
    plot <- plot+geom_sf(data = treeLine$osm_lines,
                         aes(color="Tree Line"),
                         lwd=1.5)
  }

  if(!is_empty(grassland$osm_polygons)){
    plot <- plot+geom_sf(data = grassland$osm_polygons,
                         aes(fill="Grassland")
    )
  }

  if(!is_empty(meadow$osm_polygons)){
    plot <- plot+geom_sf(data = meadow$osm_polygons,
                         aes(fill="Meadow")
    )
  }

  if(!is_empty(park$osm_polygons)){
    plot <- plot+geom_sf(data = park$osm_polygons,
                         aes(fill="Park")
    )
  }

  if(!is_empty(garden$osm_polygons)){
    plot <- plot+geom_sf(data = garden$osm_polygons,
                         aes(fill="Garden")
    )
  }

  if(!is_empty(natureReserve$osm_polygons)){
    plot <- plot+geom_sf(data = natureReserve$osm_polygons,
                         aes(fill="Nature Reserve")
    )
  }

  plot <- plot +                                                                # the objects that are never empty are added
    geom_sf(data = route,
            aes(color = "Route"),
            lwd = 2) +
    geom_sf(data = pov,
            aes(color = "You are here"),
            size = 4) +
    geom_sf(data = closestNature,
            aes(color = "Closest Nature"),
            size = 4) +
    coord_sf(xlim,ylim,expand = FALSE)+                                         # coordinate system is added to the plot with the x and y limits

    labs(title = "Where R the Trees?",                                          # adding title, subtitle and caption
         subtitle = paste("Nature is only",
                          closestNature$dist,
                          "m away from you!",
                          sep = " "),
         caption = "This map shows the closest public nature area/point reachable by car, therefore there might
         be a gap between your position and the start of the route and points that might be closer if you are by foot.
         For foot or bike profile please set up a server.
         \nPlot made with: ggplot2, based on data from Open Street Maps.")+
    theme(plot.title = element_text(size=20, face = "bold", hjust = 0.5),       # specifiyng the appearance of  title, subtitle and caption
          plot.subtitle = element_text(size=10, face = "plain", hjust = 0.5),
          plot.caption= element_text(size=8, face = "italic", hjust = 0.5),
          panel.background = element_rect(fill= "ivory"),                       # changing background and grid colour
          panel.grid.major = element_line(color = "ivory2"),
          plot.tag = element_text(size = 8, face = "italic", hjust = 0))+
    scale_fill_manual(values=c("Forest" = "darkolivegreen",                     # manually setting the legend for polygon objects
                               "Special Forest" = "darkolivegreen4",
                               "Wood" = "darkolivegreen3",
                               "Grassland" = "green",
                               "Meadow" = "green2",
                               "Park" = "green3",
                               "Garden" = "darkseagreen3",
                               "Nature Reserve" = "green4"),
                      guide = guide_legend(title = NULL, order =2))+            # removing legend title and setting its position as second
    scale_color_manual(values = c("Streets" = "snow4",                          # manually setting the legend for the line and point objects
                                  "River" = "deepskyblue3",
                                  "Single Tree" = "springgreen3",
                                  "Tree Line" = "springgreen3",
                                  "You are here" = "red",
                                  "Closest Nature" = "orange",
                                  "Route" = "yellow"),
                       guide = guide_legend(title = "Legend", order = 1),       # setting the legend title and its position as first
                       breaks = c("You are here","Closest Nature","Route",      # reordering legend items
                                  "Streets","River","Single Tree","Tree Line"))

  return(plot)                                                                  # function returns the final plot
}

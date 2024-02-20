extractTreeLine <- function(aoi){
  treeLine <- aoi%>%
    opq()%>%
    add_osm_feature(key="natural",
                    value = "tree_row") %>%
    osmdata_sf()

  return(treeLine)
}

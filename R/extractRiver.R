# function to extract rivers and water bodies
extractRiver <- function(aoi){
  river <- aoi%>%
    opq()%>%
    add_osm_feature(key = "waterway", value = "river") %>%
    osmdata_sf()

  return(river)
}

extractSingleTree <- function(aoi){
  singleTree <- aoi%>%
    opq()%>%
    add_osm_feature(key="natural",
                    value = "tree") %>%
    osmdata_sf()

  return(singleTree)
}

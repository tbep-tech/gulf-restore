#install.packages('devtools')
#devtools::install_github('tbep-tech/tbeptools')
#install.packages("nhdplusTools")
#install.packages('tidyverse')
library(tbeptools)
library(nhdplusTools)
library(sf)
library(tidyverse)

#These functions are slated to be added to the nhdplusTools library (copied from repo)
get_nhdplus_byid <- function(comids, layer) {
  
  id_name <- list(catchmentsp = "featureid", nhdflowline_network = "comid")
  
  if (!any(names(id_name) %in% layer)) {
    stop(paste("Layer must be one of",
               paste(names(id_name),
                     collapse = ", ")))
  }
  
  post_url <- "https://cida.usgs.gov/nwc/geoserver/nhdplus/ows"
  
  # nolint start
  
  filter_1 <- paste0('<?xml version="1.0"?>',
                     '<wfs:GetFeature xmlns:wfs="http://www.opengis.net/wfs" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:gml="http://www.opengis.net/gml" service="WFS" version="1.1.0" outputFormat="application/json" xsi:schemaLocation="http://www.opengis.net/wfs http://schemas.opengis.net/wfs/1.1.0/wfs.xsd">',
                     '<wfs:Query xmlns:feature="http://gov.usgs.cida/nhdplus" typeName="feature:',
                     layer, '" srsName="EPSG:4326">',
                     '<ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">',
                     '<ogc:Or>',
                     '<ogc:PropertyIsEqualTo>',
                     '<ogc:PropertyName>',
                     id_name[[layer]],
                     '</ogc:PropertyName>',
                     '<ogc:Literal>')
  
  filter_2 <- paste0('</ogc:Literal>',
                     '</ogc:PropertyIsEqualTo>',
                     '<ogc:PropertyIsEqualTo>',
                     '<ogc:PropertyName>',
                     id_name[[layer]],
                     '</ogc:PropertyName>',
                     '<ogc:Literal>')
  
  filter_3 <- paste0('</ogc:Literal>',
                     '</ogc:PropertyIsEqualTo>',
                     '</ogc:Or>',
                     '</ogc:Filter>',
                     '</wfs:Query>',
                     '</wfs:GetFeature>')
  
  filter_xml <- paste0(filter_1, paste0(comids, collapse = filter_2), filter_3)
  
  # nolint end
  
  req_data <- httr::RETRY("POST", post_url, body = filter_xml, times = 3, pause_cap = 60)
  
  return(make_web_sf(req_data))
}


#subfunction make_web_sf(req_data)
make_web_sf <- function(content) {
  if(content$status_code == 200) {
    tryCatch(sf::read_sf(rawToChar(content$content)),
             error = function(e) {
               message(paste("Something went wrong with a web request.\n", e))
             }, warning = function(w) {
               message(paste("Something went wrong with a web request.\n", w))
             })
  } else {
    message(paste("Something went wrong with a web request.\n", content$url,
                  "\n", "returned",
                  content$status_code))
    data.frame()
  }
}


data(reststat)
data(restdat)
rest <- reststat %>% 
  full_join(restdat, by = 'id') %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)

#I'm sure this functions can be done on the complete set of points in sf, but I started on just 1st pnt

#find catchment COMID for first sf point
point <- rest[1,5]
#get_nhdplus.discover_nhdplus_id
comid <- discover_nhdplus_id(point)

#get catchment polygon
#poly = get_nhdplus.get_nhdplus_byid(comid, layer)
layer <- 'catchmentsp'
catchment <- get_nhdplus_byid(comid, layer)
#NOTE: gridcode field may be able to be used to get elevetation raster within catchment

#get flowline in catchment
layer <- 'nhdflowline_network'
fline <- get_nhdplus_byid(comid, layer)


#Plot what we have so far
ggplot() + 
  geom_sf(data = catchment) +
  geom_sf(data = fline) +
  geom_sf(data = point)


#An alternative way to get the first line segment is to chose one closest to the point
#index_nhdplu.get_flowline_index  returns nearest fline in flines for point
#get_flowline_index(flines, point)
#However, this requires flines to be local and may not be in the same catchment
#To get flines local use get_nhdplus.get_nhdplus_bybox and the extent (bbox) from rest or tbshed

#Other things we may want to calculate at this point:
#get distance from rest to nearest point on line?
#get distance from nearest point on line to downstream end of line segment?

#Next step is to get downstream lines/distance to terminal

#Option 1
#get_network.get_DD(network, comid, distance = NULL)
#dd_comid <- get_DD(flines, comid, distance = NULL)
#the above requires flines, I know there is a service to return downstream by comid

#Option 2
#I've definitely gotten downstream comid using comid via cida/usgs.gov/nldi 
#https://cida.usgs.gov/nldi/comid/16906589/navigate/DD
# see https://github.com/ACWI-SSWD/nldi-services

#create nldi feature list using nhdplusTools
nldi_feature <-list("featureSource"='comid', "featureID"=comid)
#navigatedownstream using nldi_feature
navigate_nldi(nldi_feature, mode = 'downstreamDiversions')
#the above seems like it should work but 404 errors at the moment
#the servicerepo (https://github.com/ACWI-SSWD/nldi-services) has a build error at the moment so it may just be down


#option 3 - use arc rest service for vpu and get flow table
#https://services6.arcgis.com/2TtZhmoHm5KqwqfS/arcgis/rest/services/NHDPlus_V2_BoundaryUnit/FeatureServer
#http://www.horizon-systems.com/NHDPlusData/NHDPlusV21/Data/NHDPlusSA/NHDPlus03S/NHDPlusV21_SA_03S_NHDPlusAttributes_07.7z
#table can then be used to get the catchments/flowlines
#Another option is I could process the seemless and host the flowtable for that ~250MB

#option 4
#try instead downloading 3 to create url later
#bbox <- st_bbox(rest)
#get_nhdplus.get_nhdplus_bybox (have to copy function in from dev version)
#flines <- get_nhdplus_bybbox(bbox, layer)
#layer <- 'catchmentsp'
#catchments <- get_nhdplus_bybbox(bbox, layer)
#we can get catchments and flowlines this way but not the network table

#option 5 
#download seemless using nhdplusTools
#local_dir <- 'L://Public//jbousqui//Code//GitHub//restore-gulf//data'
#download_nhdplusv2(local_dir)
# failed because status_code != 200

#option 6
#do regional manually for now (go back to )
#url1 = "https://www.horizon-systems.com/NHDPlusData/NHDPlusV21/Data/NHDPlusSA/NHDPlus03S/NHDPlusV21_SA_03S_NHDPlusAttributes_07.7z"

# downloader <- function(dir, url, type){
#   
#   if (!dir.exists(dir)) {
#     dir.create(dir, recursive = T)
#   }
#   
#   file <-  file.path(dir, basename(url))
#   
#   if (!file.exists(file)) {
#     
#     message("Downloading ", basename(url))
#     
#     resp <-  httr::GET(url,
#                        httr::write_disk(file, overwrite = TRUE),
#                        httr::progress())
#     
#     if (resp$status_code != 200) {
#       stop("Download unsuccessfull :(")
#     }
#     
#   } else {
#     message("Compressed ", toupper(type), " file already exists ...")
#   }
#   
#   return(file)
#   
# }

#downloader(local_dir, url1, "nhdplusV2")
#Timeout error


#Last attempt manually download seamless and generated flowTable from that

# ggplot() + 
#   geom_sf(data = catchment) +
#   geom_sf(data = fline) +
#   geom_sf(data = point)
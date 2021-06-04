#install.packages('devtools')
#devtools::install_github('tbep-tech/tbeptools')
#install.packages("nhdplusTools")
#devtools::install_github('cran/nhdplusTools')
#install.packages('tidyverse')
#install.packages('fansi')
library(tbeptools)
library(nhdplusTools)
library(sf)
library(tidyverse)


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

#My feeble attempt to run on the sf
points <- rest
# There is a more elegant way to create empty fields
points$'comid' <- ''
points$'pathlength' <- ''
# params for flow lines
layer <- 'nhdflowline_network'
rm(flines) #just in case (does throw warning if not found)
#loop over one sf point at a time
for(i in 1:length(points$id)){
  cat(round(i / length(points$id), 2)*100, '% complete\n')
  point <- rest[i,]
  comid <- tryCatch(discover_nhdplus_id(point),
                    message=function(m) {
                      if (startsWith(as.character(m[1]), 'No data returned')) {
                        #TODO: Confirm in Ocean Catchments?
                        #return default for OceanCatchment
                        700000000
                      } else {
                        #TODO: confirm if() not met when service down
                        message(m)}
                    })
  # Write comid to table
  points[i,'comid'] <- comid
  if (comid==700000000) {
    # Ocean Catchment
    points[i,'pathlength'] <- -1
  } else {
    # Use comid to get fline and pathlength
    fline <- nhdplusTools:::get_nhdplus_byid(comid, layer)
    points[i,'pathlength'] <- fline$'pathlength'
    # Save fline to flines to plot
    if (exists('flines')) {
      flines <- rbind(flines, fline)
    } else {
      flines <- fline #first time
      }
  }
#warning(paste("Issue with row ", i, "\n Returned: ", comid))
}
#plot results
ggplot() +
  geom_sf(data = flines) +
  geom_sf(data = points)
#save results
#st_write(flines, 'flines.geojson')
#Notes:
#Problem w/ comid for 169 points: 4, 18, 19, 20, 21, 22, 23, 31, 52, 55, 62, 65, 69, 72, 73, 74, 91, 95,
#96, 97, 100, 101, 102, 103, 122, etc
#These are over the water where there isn't a catchment.
# Options: (1) select out ahead to skip, (2) test outside after by comparing to
#ocean poly, or (3) just assume failures are not in catchment

#Some geometries repeat, it takes a while to run and removing duplicates might be worthwhile



#The following was initialy used to outline the process on 1st point, it includes catchment/downstream
#functions and some additional options of how to do the same thing

#Step 1: for a point in rest, get the corresponding catchment and flowline and catchment (optional)
point <- rest[1,5]

#find catchment COMID for first sf point using get_nhdplus.discover_nhdplus_id
comid <- discover_nhdplus_id(point)

#get flowline get_nhdplus.get_nhdplus_byid(comid, layer)
layer <- 'nhdflowline_network'
fline <- nhdplusTools:::get_nhdplus_byid(comid, layer)
#Length of full path in km?
flowDist <- fline$'pathlength'

#get catchment polygon (optional)
layer <- 'catchmentsp'
catchment <- nhdplusTools:::get_nhdplus_byid(comid, layer)
#NOTE: gridcode field may be able to be used to get elevetation raster within catchment


#Plot what we have so far
ggplot() + 
  geom_sf(data = catchment) +
  geom_sf(data = fline) +
  geom_sf(data = point)

#Additional attributes of fline that may be useful
#length of that segment
segmentLength <- fline$'lengthkm'
#Not sure what this is but may be relevant
#flowDist <- fline$'terminalpa'

#Other things we may want to calculate at this point:
#get distance from rest to nearest point on fline?
st_distance(point, fline) #in meters
#this doesn't tell us where on fline the nearest point is (need to add the rest of the segment distance)
test_line <- st_nearest_points(point, fline)
#although coordinates are longitude/latitude, st_nearest_points assumes that they are planar

#get length to check
#test_sf<-st_as_sf(test_line)
#st_length(test_sf) # this didn't work and I can't determine why...
#determine using start/edn points
st_distance(st_cast(test_line, "POINT")) #passes test

#get start and end points
st_cast(test_line, "POINT")[2] #end point
#this may not intersect the fline because of rounding...

#plot it to test it 
ggplot() + 
  geom_sf(data = catchment) +
  geom_sf(data = fline) +
  geom_sf(data = test_line)
  geom_sf(data = point)


#get distance from nearest point on fline to downstream end of line segment?


#An alternative way to get the first line segment is to chose one closest to the point
#index_nhdplu.get_flowline_index  returns nearest fline in flines for point
#get_flowline_index(flines, point)
#However, this requires flines to be local and may not be in the same catchment
#To get flines local use get_nhdplus.get_nhdplus_bybox and the extent (bbox) from rest or tbshed

#NHDPlus flowlines get tricky on the coastline, where coastal catchments are sometime networked together
#Next step is to get downstream lines and check distance to terminal, this may not be neccessary

#Option 1
#get_network.get_DD(network, comid, distance = NULL)
#dd_comid <- get_DD(flines, comid, distance = NULL)
#the above requires flines, I know there is a service to return downstream by comid (option 2)

#Option 2
#Downstream comid using comid via cida/usgs.gov/nldi
#https://cida.usgs.gov/nldi/comid/16906589/navigate/DD
# see https://github.com/ACWI-SSWD/nldi-services
#NOTE: 1/10/2020 the service was down (404 error) so a try/except with downCOM.json (option 3)

#create nldi feature list for nhdplusTools
nldi_feature <-list("featureSource"='comid', "featureID"=comid)
#navigatedownstream using nldi_feature (get_nldi.navigate_nldi)
nldi_down <- navigate_nldi(nldi_feature, mode = 'downstreamMain', data_source = "")
#Note that there is a diversion in this that then recombines
# To instead get diversions
#nldi_dd <- navigate_nldi(nldi_feature, mode = 'downstreamDiversions', data_source = "")

#Plot as a quick check
ggplot() + 
  geom_sf(data = nldi_down) +
  geom_sf(data = point)


#Total length
flowDist_calc <- sum(st_length(nldi_down))
#flowDist_calc should be about (segmentLength + flowDist)*1000

#Confirm terminal using flines['terminalfl']
downIDs <- nldi_down$'nhdplus_comid'
comid_term <- tail(downIDs, n=1)
layer <- 'nhdflowline_network'
last_fline <- nhdplusTools:::get_nhdplus_byid(comid_term, layer)

#we don't need the geometry for the catchments but could get/plot it
catchments <- catchment
layer <- 'catchmentsp'
for(id in downIDs){
  catchment_temp <- nhdplusTools:::get_nhdplus_byid(id, layer)
  catchments <- rbind(catchments, catchment_temp)
}

#Plot as a quick check
ggplot() + 
  geom_sf(data = tbshed) + 
  geom_sf(data = tbseg) + 
  # geom_sf(data = catchments) +
  geom_sf(data = nldi_down, col = 'blue') +
  geom_sf(data = point, col = 'red')

#There may be a catchment table with 'coastalfl' flagging coastal catchments but I haven't found it yet


#option 3
#I downloaded the seemless and processed the flowtable for that using python, see data/downCOMs.json
#download seemless using nhdplusTools
#local_dir <- 'L://Public//jbousqui//Code//GitHub//restore-gulf//data'
#download_nhdplusv2(local_dir)

#option 4 - use arc rest service for vpu and get flow table by vpu
#https://services6.arcgis.com/2TtZhmoHm5KqwqfS/arcgis/rest/services/NHDPlus_V2_BoundaryUnit/FeatureServer
#http://www.horizon-systems.com/NHDPlusData/NHDPlusV21/Data/NHDPlusSA/NHDPlus03S/NHDPlusV21_SA_03S_NHDPlusAttributes_07.7z
#table can then be used to download the flowlines/catchments
#bbox <- st_bbox(rest)
#get_nhdplus.get_nhdplus_bybox (have to copy function in from dev version)
#flines <- get_nhdplus_bybbox(bbox, layer)
#layer <- 'catchmentsp'
#catchments <- get_nhdplus_bybbox(bbox, layer)
#we can get catchments and flowlines this way but not the network table
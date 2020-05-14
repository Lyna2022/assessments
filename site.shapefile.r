##Script to create a shapefile for a 'site' location

library(sp)
library(rgdal)

##---------------------------------------------------

make.shape.file <- function(site) {
  lon <- site$lon
  lat <- site$lat
  region <- site$region
  shape.dir <- site$dir                
  lons <- c(lon-0.01,lon-0.01,lon+0.01,lon+0.01)
  lats <- c(lat-0.01,lat+0.01,lat+0.01,lat-0.01)

  coords <- cbind(lons,lats)
  p <- Polygon(coords)
  ps <- Polygons(list(p),1)
  sps <- SpatialPolygons(list(ps))
  proj4string(sps) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  data = data.frame(Name=toupper(region))
  spdf = SpatialPolygonsDataFrame(sps,data)
  writeOGR(spdf, shape.dir, region, driver="ESRI Shapefile",overwrite_layer=TRUE)
}

##shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/van_coastal_health'

##sites <- list(list(region='abbotsford_regional_hospital_site',lon=-122.3120961,lat=49.0367466,dir=shape.dir),
##              list(region='burnaby_hospital_site',lon=-123.0157868,lat=49.2494788,dir=shape.dir),
##              list(region='chilliwack_general_hospital_site',lon=-121.9637349,lat=49.166644,dir=shape.dir),
##              list(region='delta_hospital_site',lon=-123.0618836,lat=49.0856773,dir=shape.dir),
##              list(region='eagle_ridge_hospital_site',lon=-122.82402,lat=49.2850899,dir=shape.dir),
##              list(region='fraser_canyon_hospital_site',lon=-121.426472,lat=49.377533,dir=shape.dir),
##              list(region='langley_memorial_hospital_site',lon=-122.6124141,lat=49.0944357,dir=shape.dir),
##              list(region='mission_memorial_hospital_site',lon=-122.3327057,lat=49.1351174,dir=shape.dir),
##              list(region='peace_arch_hospital_site',lon=-122.7921925,lat=49.0301274,dir=shape.dir),
##              list(region='ridge_meadows_hospital_site',lon=-122.6300294,lat=49.2155155,dir=shape.dir),
##              list(region='riverview_hospital_site',lon=-122.7891596,lat=49.2730154,dir=shape.dir),
##              list(region='surrey_memorial_hospital_site',lon=-122.8413973,lat=49.1759547,dir=shape.dir))


##shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/okanagan'
##sites <- list(list(region='golden_hospital_site',lon=-116.96696,lat=51.297043,dir=shape.dir))

##shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/kootenays'
##sites <- list(list(region='invermere',lon=-116.0291,lat=50.5065,dir=shape.dir))

##shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/terrace'
##sites <- list(list(region='terrace',lon=-128.6032,lat=54.5182,dir=shape.dir))

##shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/skeena_hydro'
##sites <- list(list(region='skeena_cap_bank1',lon=-124.0044,lat=53.9819,dir=shape.dir),
##              list(region='skeena_cap_bank2',lon=-125.8561,lat=54.3283,dir=shape.dir),
##              list(region='skeena_cap_bank3',lon=-127.1750,lat=54.6406,dir=shape.dir),
##              list(region='skeena_cap_substation',lon=-128.6319,lat=54.4522,dir=shape.dir))

##shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/interior_health'
##sites <- list(list(region='cariboo_memorial_hospital',lon=-122.143595,lat=52.137342,dir=shape.dir),
##              list(region='helmcken_memorial_hospital',lon=-120.018194,lat=51.645362,dir=shape.dir),
##              list(region='hundred_mile_general_hospital',lon=-121.292221,lat=51.638628,dir=shape.dir),
##              list(region='lillooet_hospital',lon=-121.939659,lat=50.689413,dir=shape.dir),
##              list(region='royal_inland_hospital',lon=-120.33464,lat=50.670278,dir=shape.dir),
##              list(region='shuswap_lake_hospital',lon=-119.274552,lat=50.704443,dir=shape.dir),   
##              list(region='nicola_valley_health',lon=-120.772803,lat=50.12161,dir=shape.dir),
##              list(region='queen_victoria_hospital',lon=-118.191202,lat=50.977543,dir=shape.dir),
##              list(region='vernon_jubilee_hospital',lon=-119.271622,lat=50.256928,dir=shape.dir),
##              list(region='kelowna_general_hospital',lon=-119.49387,lat=49.873637,dir=shape.dir),
##              list(region='penticton_regional_hospital',lon=-119.577905,lat=49.481369,dir=shape.dir),
##              list(region='princeton_general_hospital',lon=-120.526739,lat=49.454102,dir=shape.dir),
##              list(region='south_okanagan_hospital',lon=-119.538896,lat=49.183654,dir=shape.dir),
##              list(region='arrow_lakes_hospital',lon=-117.795247,lat=50.238771,dir=shape.dir),
##              list(region='kootenay_lake_hospital',lon=-117.284828,lat=49.495039,dir=shape.dir),
##              list(region='kootenay_boundary_hospital',lon=-117.701485,lat=49.1032,dir=shape.dir),
##              list(region='boundary_hospital',lon=-118.468272,lat=49.029598,dir=shape.dir),
##              list(region='golden_districts_hospital',lon=-116.966476,lat=51.296795,dir=shape.dir),
##              list(region='invermere_district_hospital',lon=-116.033398,lat=50.506848,dir=shape.dir),
##              list(region='east_kootenay_hospital',lon=-115.749799,lat=49.512385,dir=shape.dir),
##              list(region='elk_valley_hospital',lon=-115.056042,lat=49.513706,dir=shape.dir),
##              list(region='creston_valley_hospital',lon=-116.507909,lat=49.098357,dir=shape.dir))

##shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/okanagan'
##sites <- list(list(region='penticton',lon=-119.603070,lat=49.460218,dir=shape.dir))

shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/vancouver_island'
##sites <- list(list(region='campbell_river',lon=-122.143595,lat=52.137342,dir=shape.dir),
##              list(region='courtenay',lon=-120.018194,lat=51.645362,dir=shape.dir),
##              list(region='port_alberni',lon=-120.018194,lat=51.645362,dir=shape.dir),
##              list(region='nanaimo',lon=-120.018194,lat=51.645362,dir=shape.dir),
##              list(region='duncan',lon=-120.018194,lat=51.645362,dir=shape.dir),
##              list(region='sooke',lon=-120.018194,lat=51.645362,dir=shape.dir),

sites <- list(list(region='cedar',lon=-123.8643,lat=49.1155,dir=shape.dir),
              list(region='saanichton',lon=-123.4169,lat=49.5963,dir=shape.dir),
              list(region='metchosin',lon=-123.5378,lat=48.3820,dir=shape.dir))
      
sites <- list(list(region='sayward',lon=-125.9602,lat=50.3837,dir=shape.dir))
for (site in sites) {
    make.shape.file(site)
}

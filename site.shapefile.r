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

shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/van_coastal_health'

sites <- list(list(region='abbotsford_regional_hospital_site',lon=-122.3120961,lat=49.0367466,dir=shape.dir),
              list(region='burnaby_hospital_site',lon=-123.0157868,lat=49.2494788,dir=shape.dir),
              list(region='chilliwack_general_hospital_site',lon=-121.9637349,lat=49.166644,dir=shape.dir),
              list(region='delta_hospital_site',lon=-123.0618836,lat=49.0856773,dir=shape.dir),
              list(region='eagle_ridge_hospital_site',lon=-122.82402,lat=49.2850899,dir=shape.dir),
              list(region='fraser_canyon_hospital_site',lon=-121.426472,lat=49.377533,dir=shape.dir),
              list(region='langley_memorial_hospital_site',lon=-122.6124141,lat=49.0944357,dir=shape.dir),
              list(region='mission_memorial_hospital_site',lon=-122.3327057,lat=49.1351174,dir=shape.dir),
              list(region='peace_arch_hospital_site',lon=-122.7921925,lat=49.0301274,dir=shape.dir),
              list(region='ridge_meadows_hospital_site',lon=-122.6300294,lat=49.2155155,dir=shape.dir),
              list(region='riverview_hospital_site',lon=-122.7891596,lat=49.2730154,dir=shape.dir),
              list(region='surrey_memorial_hospital_site',lon=-122.8413973,lat=49.1759547,dir=shape.dir))


##shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/okanagan'
##sites <- list(list(region='golden_hospital_site',lon=-116.96696,lat=51.297043,dir=shape.dir))

##shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/kootenays'
##sites <- list(list(region='invermere',lon=-116.0291,lat=50.5065,dir=shape.dir))

##shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/terrace'
##sites <- list(list(region='terrace',lon=-128.6032,lat=54.5182,dir=shape.dir))

shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/skeena_hydro'
sites <- list(list(region='skeena_cap_bank1',lon=-124.0044,lat=53.9819,dir=shape.dir),
              list(region='skeena_cap_bank2',lon=-125.8561,lat=54.3283,dir=shape.dir),
              list(region='skeena_cap_bank3',lon=-127.1750,lat=54.6406,dir=shape.dir),
              list(region='skeena_cap_substation',lon=-128.6319,lat=54.4522,dir=shape.dir))

for (site in sites) {
    make.shape.file(site)
}

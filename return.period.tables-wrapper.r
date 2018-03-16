##Script to produce tables of projected temperature and precipitation changes
source('/storage/home/ssobie/code/repos/assessments/calc.rp.tables.r')

library(sp)
##-----------------------------------------------------------------------------------------------

var.list <- c('pr') ##,'tasmax','tasmin')

rperiod <- '20'
ds.type <- 'bccaq'  ## 'rcm'
type <- 'gcm'

reg.list <- 'northeast'
title.list <- 'Northeast'
proj.dir <- '/storage/data/projects/rci/data/assessments/northeast/'

pctl <- TRUE
scenario <- 'rcp85'

past.int <- '1971-2000'

shape.dir <- '/storage/data/projects/rci/data/assessments/northeast/shapefiles/'

gcm.list <- c('ACCESS1-0',
              'CanESM2',
              'CCSM4',              
              'CNRM-CM5',
              'CSIRO-Mk3-6-0',
              'GFDL-ESM2G',
              'HadGEM2-CC',
              'HadGEM2-ES',
              'inmcm4',                            
              'MIROC5',
              'MPI-ESM-LR',
              'MRI-CGCM3')

gcm.list <- c('CNRM-CM5','CanESM2','ACCESS1-0')

scen.list <- 'rcp85'
proj.list <- c('2011-2040','2041-2070','2071-2100')

for (scenario in scen.list) {
  model.list <- gcm.list
  read.dir <- paste('/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/assessment_subsets/northeast/',scenario,'/return_periods/',sep='')
  for (proj.int in proj.list) {
    for (i in seq_along(reg.list)) {
      region <- reg.list[i]
      region.title <- title.list[i]
      shape.name <- region
      clip.shp <- readOGR(shape.dir,shape.name, stringsAsFactors=F)
      region.shp <- spTransform(clip.shp,CRS("+proj=longlat +datum=WGS84 +no_defs"))
      
      make.tables(var.list,model.list,
                  ds.type,region,region.title,region.shp,type,rperiod,scenario,
                  proj.dir,read.dir,write.dir=proj.dir,pctl,
                  past.int,proj.int)

    }
  }
}
  

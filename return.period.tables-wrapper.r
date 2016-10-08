##Script to produce tables of projected temperature and precipitation changes
source('/storage/data/projects/rci/bcgov/moti/nrcan-precip_case_studies/code/moti.climdex.robjects.r',chdir=TRUE)
source('/home/ssobie/code/hg/rci-lib/ds.return.period.tables.r',chdir=TRUE)
library(sp)
##-----------------------------------------------------------------------------------------------

var.list <- c('pr','tasmax','tasmin')
##var.list <- c('tasmax','tasmin')
##var.list <- 'snowdepth'
rperiod <- '20'
ds.type <- 'bccaq'  ## 'rcm'
type <- 'gcm'
##reg.list <-  c('skeena','northeast','kootenay','omineca','west','cariboo','thompson','south')
##title.list <- c('Skeena','Northeast','Kootenay','Omineca','West','Cariboo','Thompson','South')
##reg.list <- 'northeast'
reg.list <- 'cvrd'
title.list <- 'Cowichan Valley Regional District'
##proj.dir <- '/storage/data/projects/rci/data/cas/'
proj.dir <- '/storage/data/projects/rci/data/assessments/cvrd/'
pctl <- TRUE
#scenario <- 'rcp85'

past.int <- '1971-2000'
#proj.int <- '2011-2040'

##read.dir <- paste('/home/data/scratch/ssobie/bccaq_gcm/',scenario,'/return_periods/',sep='')  ##'/home/data/climate/downscale/NARCCAP/'



shape.dir <- paste(proj.dir,'shapefiles/',sep='')

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


  rcp26.list <- c('CCSM4',
                  'CanESM2',
                  'CNRM-CM5',
                  'CSIRO-Mk3-6-0',
                  'GFDL-ESM2G',
                  'HadGEM2-ES',
                  'MIROC5',
                  'MPI-ESM-LR',
                  'MRI-CGCM3')


scen.list <- c('rcp26','rcp45','rcp85')
##scen.list <- 'rcp85'
proj.list <- c('2011-2040','2041-2070','2071-2100')
#proj.list <- '2041-2070'
for (scenario in scen.list) {
  model.list <- gcm.list
  if (scenario=='rcp26')
    model.list <- rcp26.list
  ##read.dir <- paste('/storage/data/projects/rci/data/stat.downscaling/BCCAQ/bccaq_gcm/',scenario,'/return_periods/',sep='')
  read.dir <- paste('/storage/data/scratch/ssobie/bccaq_gcm_bc_subset/',scenario,'/return_periods/',sep='')
  
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
  

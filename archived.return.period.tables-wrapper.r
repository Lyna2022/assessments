##Script to produce tables of projected temperature and precipitation changes
source('/storage/data/projects/rci/bcgov/moti/nrcan-precip_case_studies/code/moti.climdex.robjects.r',chdir=TRUE)
source('/home/ssobie/code/hg/rci-lib/ds.return.period.tables.r',chdir=TRUE)
library(sp)
##-----------------------------------------------------------------------------------------------

##var.list <- c('pr','tasmax','tasmin')
##var.list <- c('pr','tasmax','tasmin')
var.list <- c('rx2dayETCCDI','rx5dayETCCDI')
##var.list <- 'snowdepth'
rperiod <- '10'
ds.type <- 'bccaq'  ## 'rcm'
type <- 'gcm'
##reg.list <-  c('kootenay','omineca','west','cariboo','thompson','south','skeena','northeast')
##title.list <- c('Kootenay','Omineca','West','Cariboo','Thompson','South','Skeena','Northeast')

##reg.list <-  'fraser_municipal'
##title.list <- 'Fraser District Municipalities'

reg.list <- c('mission','kent','abbotsford','chilliwack','FVRDelectoralG','FVRDelectoralH')
title.list <- c('Mission','Kent','Abbotsford','Chilliwack','FVRD Electoral G','FVRD Electoral H')

##reg.list <-  c('cvrd_developed_watersheds','cvrd_water_supply_watersheds','cvrd_west_coast_watersheds')
##title.list <- c('Developed Area Watersheds','Water Supply Watersheds','West Coast Watersheds')

##reg.list <-  c('GVRD','JDF_Electoral','victoria_water','southern_gulf_islands')
##title.list <- c('Greater Victoria','Juan de Fuca Electoral','Victoria Water Supply','Southern Gulf Islands')


##reg.list <- 'northeast'
##reg.list <- 'nanaimo'
##title.list <- 'Nanaimo Regional District'
##proj.dir <- '/storage/data/projects/rci/data/cas/'
##proj.dir <- '/storage/data/projects/rci/data/assessments/nanaimo/'
##proj.dir <- '/storage/data/projects/rci/data/forestry/regional_summaries/'
##proj.dir <- '/storage/data/projects/rci/data/assessments/bc/'
proj.dir <- '/storage/data/projects/rci/data/assessments/fraser_municipal/'

pctl <- TRUE
#scenario <- 'rcp85'

past.int <- '1971-2000'
#proj.int <- '2011-2040'

##read.dir <- paste('/home/data/scratch/ssobie/bccaq_gcm/',scenario,'/return_periods/',sep='')  ##'/home/data/climate/downscale/NARCCAP/'
shape.dir <- '/storage/data/projects/rci/data/assessments/fraser_district/shapefiles/sub_regions/'

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


##scen.list <- c('rcp26','rcp45','rcp85')
scen.list <- 'rcp85'
proj.list <- c('2011-2040','2041-2070','2071-2100')
##proj.list <- '2071-2100'
for (scenario in scen.list) {
  model.list <- gcm.list
  if (scenario=='rcp26')
    model.list <- rcp26.list

  read.dir <- paste('/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/bccaq_gcm_van_whistler_subset/',scenario,'/return_periods/',sep='')

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
  

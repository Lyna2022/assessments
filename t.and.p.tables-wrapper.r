##Script to produce tables of projected temperature and precipitation changes
source('/home/data/projects/rci/bcgov/moti/nrcan-precip_case_studies/code/moti.climdex.robjects.r',chdir=TRUE)
source('/home/ssobie/code/hg/rci-lib/ds.t.and.p.tables.r',chdir=TRUE)

##-----------------------------------------------------------------------------------------------

#var.list <- c('pr','tasmax','tasmin')
var.list <- 'snowdepth'
ds.type <- 'bccaq'  ## 'rcm'
type <- 'gcm'
##reg.list <-  c('skeena','northeast','kootenay','omineca','west','cariboo','thompson','south')
##reg.list <- 'inshuck_road_boundary_10'
region <- 'coquitlam_watershed' ##'metro_van'
region.title <- 'Coquitlam Watershed' ##'Metro Vancouver'
proj.dir <- '/home/data/projects/rci/data/assessments/metro_van/'
##scenario <- 'rcp85'

past.int <- '1971-2000'
##proj.int <- '2071-2100'
pctl <- TRUE ##Controls whether the percentiles (10,50,90) are added to the ensemble

##  '/home/data/climate/downscale/NARCCAP/'
##proj.dir <- '/home/data/projects/rci/data/cas/'
##proj.dir <- '/home/data/projects/rci/data/forestry/engineering/'


shape.dir <- paste(proj.dir,'shapefiles/',sep='')
shape.name <- region
clip.shp <- readOGR(shape.dir,shape.name, stringsAsFactors=F)  

gcm.list <- c('ACCESS1-0',              
              'CanESM2',
              'CCSM4',
              'CNRM-CM5',
              'CSIRO-Mk3-6-0',
              'GFDL-ESM2G',
              'HadGEM2-CC',
              'inmcm4',                                                
              'HadGEM2-ES',
              'MIROC5',
              'MPI-ESM-LR',
              'MRI-CGCM3')

  rcp26.list <- c('CanESM2',
                  'CCSM4',
                  'CNRM-CM5',
                  'CSIRO-Mk3-6-0',
                  'GFDL-ESM2G',
                  'HadGEM2-ES',
                  'MIROC5',
                  'MPI-ESM-LR',
                  'MRI-CGCM3')

scen.list <- 'rcp85' ##c('rcp26','rcp45','rcp85')
proj.list <- c('2011-2040','2041-2070','2071-2100')

for (scenario in scen.list) {
  ##read.dir <- paste('/home/data/projects/rci/data/stat.downscaling/BCCAQ/bccaq_gcm_bc_subset/',scenario,'/',sep='')
  read.dir <- '/home/data/scratch/ssobie/bccaq_gcm_van_whistler_subset/rcp85/snow/'
  model.list <- gcm.list
  if (scenario=='rcp26')
    model.list <- rcp26.list
  for (proj.int in proj.list) {
    make.tables(var.list,model.list,
                ds.type,region,region.title,clip.shp,type,scenario,
                proj.dir,read.dir,write.dir=proj.dir,
                past.int=past.int,proj.int=proj.int,pctl=pctl)   
  }
}
  

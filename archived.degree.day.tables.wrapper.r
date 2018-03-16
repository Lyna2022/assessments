##Wrapper script for ds.climdex.tables.r in the main RCI code directory.

source('/home/ssobie/code/hg/rci-lib/ds.degree.days.r',chdir=TRUE)
##-----------------------------------------------------------------------------------------------

reg.list <- 'fraser_municipal'
title.list <- 'Fraser Valley Municipalities'
##reg.list <- c('cariboo','kootenay','northeast','omineca','skeena','south','thompson','west')
##title.list <- c('Cariboo','Kootenay','Northeast','Omineca','Skeena','South','Thompson','West')

##reg.list <-  c('cvrd_developed_watersheds','cvrd_water_supply_watersheds','cvrd_west_coast_watersheds')
##title.list <- c('Developed Area Watersheds','Water Supply Watersheds','West Coast Watersheds')

reg.list <- c('mission','kent','abbotsford','chilliwack','FVRDelectoralG','FVRDelectoralH')
title.list <- c('Mission','Kent','Abbotsford','Chilliwack','FVRD Electoral G','FVRD Electoral H')


ds.type <- 'bccaq'  ## 'rcm'
scenario.list <- 'rcp85' ##c('rcp26','rcp45','rcp85')
past.int <- '1971-2000'
proj.ints <- c('2011-2040','2041-2070','2071-2100')

pctl <- TRUE
##proj.dir <- paste('/storage/data/projects/rci/data/assessments/',reg.list,'/',sep='')
proj.dir <- '/storage/data/projects/rci/data/assessments/fraser_municipal/'
##proj.dir <- '/storage/data/projects/rci/data/forestry/regional_summaries/'
shape.dir <- '/storage/data/projects/rci/data/assessments/fraser_district/shapefiles/sub_regions/'

for (scenario in scenario.list) {

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

  if (scenario=='rcp26') {
    gcm.list <- c('CanESM2',
                  'CCSM4',
                  'CNRM-CM5',
                  'CSIRO-Mk3-6-0',
                  'GFDL-ESM2G',
                  'HadGEM2-ES',
                  'MIROC5',
                  'MPI-ESM-LR',
                  'MRI-CGCM3')
  }


  read.dir <- paste('/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/bccaq_gcm_van_whistler_subset/',scenario,'/degree_days/',sep='')
 
  for (i in seq_along(reg.list)) {
    for (proj.int in proj.ints) {
      region <- reg.list[i]
      region.title <- title.list[i]
      shape.name <- region

      clip.shp <- readOGR(shape.dir,shape.name, stringsAsFactors=F)
     
      go <- make.dd.tables(gcm.list,ds.type,region,region.title,scenario,clip.shp,
                           past.int,proj.int,
                           proj.dir,read.dir,pctl)

     }
  }
}




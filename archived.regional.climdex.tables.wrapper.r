##Wrapper script for ds.climdex.tables.r in the main RCI code directory.

source('/home/ssobie/code/hg/rci-lib/ds.climdex.tables.r',chdir=TRUE)
##-----------------------------------------------------------------------------------------------

##climdex.list <- c('rx1dayETCCDI_yr','rx5dayETCCDI_yr','r95pETCCDI_yr','r99pETCCDI_yr')
##climdex.mon.list <- c('rx1dayETCCDI_mon','rx5dayETCCDI_mon')
##climdex.list <- c('rx1dayETCCDI','rx5dayETCCDI','r95pETCCDI','r99pETCCDI',
##                  'tx90pETCCDI','tn10pETCCDI','txxETCCDI','tnnETCCDI')



climdex.list <- c('fdETCCDI',
                  'suETCCDI',
                  'idETCCDI',
                  'trETCCDI',
                  'gslETCCDI',
                  'txxETCCDI',
                  'tnxETCCDI',
                  'txnETCCDI',
                  'tnnETCCDI',
                  'tn10pETCCDI',
                  'tx10pETCCDI',
                  'tn90pETCCDI',
                  'tx90pETCCDI',
                  'wsdiETCCDI',
                  'csdiETCCDI',
                  'dtrETCCDI',
                  'rx1dayETCCDI',
                  'rx5dayETCCDI',
                  'sdiiETCCDI',
                  'r10mmETCCDI',
                  'r20mmETCCDI',
                  'cddETCCDI',
                  'cwdETCCDI',
                  'r95pETCCDI',
                  'r99pETCCDI',
                  'r95daysETCCDI',
                  'prcptotETCCDI')

##climdex.list <- c('r95daysETCCDI','r95sepETCCDI','r95distETCCDI','r99daysETCCDI')
                  

##reg.list <-  c('skeena','northeast','kootenay','omineca','west','cariboo','thompson','south')
##title.list <- c('Skeena','Northeast','Kootenay','Omineca','West','Cariboo','Thompson','South')
##reg.list <- 'inshuck_road_boundary_10'
##reg.list <- c('coast_mountains','georgia_depression','central_interior','southern_interior','southern_mountains',
##              'boreal_interior','boreal_plains','taiga_plains','northern_boreal')
##reg.list <-  c('GVRD','JDF_Electoral','victoria_water','southern_gulf_islands')
##title.list <- c('Greater Victoria','Juan de Fuca Electoral','Victoria Water Supply','Southern Gulf Islands')

##reg.list <-  c('cvrd_developed_watersheds','cvrd_water_supply_watersheds','cvrd_west_coast_watersheds')
##title.list <- c('Developed Area Watersheds','Water Supply Watersheds','West Coast Watersheds')

##reg.list <- 'fraser_municipal'
##title.list <- 'Fraser District Municipalities'

reg.list <- c('mission','kent','abbotsford','chilliwack','FVRDelectoralG','FVRDelectoralH')
title.list <- c('Mission','Kent','Abbotsford','Chilliwack','FVRD Electoral G','FVRD Electoral H')


proj.dir <- '/storage/data/projects/rci/data/assessments/fraser_municipal/'




ds.type <- 'bccaq'  ## 'rcm'
scenario.list <- 'rcp85' ##c('rcp26','rcp45','rcp85')
past.int <- '1971-2000'
proj.ints <- c('2011-2040','2041-2070','2071-2100')

pctl <- TRUE

##proj.dir <- '/storage/data/projects/rci/data/forestry/ecoprovinces/'

shape.dir <- '/storage/data/projects/rci/data/assessments/fraser_district/shapefiles/sub_regions/'

for (scenario in scenario.list) {
  print(scenario)
  read.dir <- paste('/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/bccaq_gcm_van_whistler_subset/',scenario,'/climdex/',sep='')
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

  for (i in seq_along(reg.list)) {
    region <- reg.list[i]
    region.title <- title.list[i]
    for (proj.int in proj.ints) {
      print(proj.int)
      shape.name <- region
      clip.shp <- readOGR(shape.dir,shape.name, stringsAsFactors=F)
    
      go <- make.tables(gcm.list,ds.type,region,region.title,scenario,clip.shp,
                        past.int,proj.int,
                        proj.dir,read.dir,pctl)
    }
  }
}




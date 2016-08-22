##Wrapper script for ds.climdex.tables.r in the main RCI code directory.

source('/home/ssobie/code/hg/rci-lib/ds.degree.days.r',chdir=TRUE)
##-----------------------------------------------------------------------------------------------

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

##reg.list <- 'van_city'
##region.title <- 'City of Vancouver'
reg.list <- c('cariboo','kootenay','northeast','omineca','skeena','south','thompson','west')
title.list <- c('Cariboo','Kootenay','Northeast','Omineca','Skeena','South','Thompson','West')
proj.dir <- '/storage/data/projects/rci/data/cas/'

ds.type <- 'bccaq'  ## 'rcm'
scenario <- 'rcp85'
past.int <- '1971-2000'
proj.ints <- c('2011-2040','2041-2070','2071-2100')

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

pctl <- TRUE
##read.dir <- paste('/storage/data/projects/rci/data/stat.downscaling/BCCAQ/bccaq_gcm/',scenario,'/degree_days/',sep='')
read.dir <- paste('/storage/data/projects/rci/data/stat.downscaling/BCCAQ/bccaq_gcm/',scenario,'/pas/',sep='')
##read.dir <- '/storage/data/scratch/ssobie/bccaq_gcm_van_whistler_subset/rcp85/degree_days/'
shape.dir <- paste(proj.dir,'shapefiles/',sep='')

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





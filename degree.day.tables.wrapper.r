##Wrapper script for ds.climdex.tables.r in the main RCI code directory.

source('/storage/home/ssobie/code/repos/assessments/calc.degree.days.r',chdir=TRUE)
##-----------------------------------------------------------------------------------------------

reg.list <- 'northeast'
title.list <- 'Northeast'

ds.type <- 'bccaq' 
scenario.list <- 'rcp85'
past.int <- '1971-2000'
proj.ints <- c('2011-2040','2041-2070','2071-2100')

pctl <- TRUE
proj.dir <- '/storage/data/projects/rci/data/assessments/northeast/'
shape.dir <- '/storage/data/projects/rci/data/assessments/northeast/shapefiles/'
read.dir <- paste('/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/assessment_subsets/northeast/',scenario,'/degree_days/',sep='')
   gcm.list <- c('ACCESS1-0',
                 'CanESM2',
                'CNRM-CM5')

  for (i in seq_along(reg.list)) {
    for (proj.int in proj.ints) {
      region <- reg.list[i]
      region.title <- title.list[i]

      shape.name <- region
      clip.shp <- spTransform(readOGR(shape.dir,shape.name, stringsAsFactors=F),CRS("+init=epsg:4326"))
     
      go <- make.dd.tables(gcm.list,ds.type,region,region.title,scenario,clip.shp,
                           past.int,proj.int,
                           proj.dir,read.dir,pctl)

     }
  }





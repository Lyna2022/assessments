##Wrapper script for ds.climdex.tables.r in the main RCI code directory.

source('/storage/home/ssobie/code/repos/assessments/calc.climdex.tables.r',chdir=TRUE)
##-----------------------------------------------------------------------------------------------

##                  'gslETCCDI',

##                  'tn10pETCCDI',
##                  'tx10pETCCDI',
##                  'tn90pETCCDI',
##                  'tx90pETCCDI',
##                  'wsdiETCCDI',
##                  'csdiETCCDI',

climdex.list <- c('fdETCCDI',
                  'suETCCDI',
                  'idETCCDI',
                  'trETCCDI',
                  'txxETCCDI',
                  'tnxETCCDI',
                  'txnETCCDI',
                  'tnnETCCDI',
                  'dtrETCCDI',
                  'rx1dayETCCDI',
                  'rx5dayETCCDI',
                  'sdiiETCCDI',
                  'suETCCDI',
                  'su30ETCCDI',
                  'r10mmETCCDI',
                  'r20mmETCCDI',
                  'cddETCCDI',
                  'cwdETCCDI',
                  'r95pETCCDI',
                  'r99pETCCDI',
                  'r95daysETCCDI',
                  'r99daysETCCDI',
                  'prcptotETCCDI')


reg.list <- 'northeast'
title.list <- 'Northeast'
proj.dir <- '/storage/data/projects/rci/data/assessments/northeast/'

ds.type <- 'bccaq' 
scenario.list <- 'rcp85' ##c('rcp26','rcp45','rcp85')
past.int <- '1971-2000'
proj.ints <- c('2041-2070','2071-2100') ##'2011-2040',

pctl <- TRUE

shape.dir <- '/storage/data/projects/rci/data/assessments/northeast/shapefiles/'

for (scenario in scenario.list) {
  print(scenario)
  read.dir <- paste('/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/assessment_subsets/northeast/',scenario,'/climdex/',sep='')
  gcm.list <- c('ACCESS1-0',
                'CanESM2',
                'CNRM-CM5')

  for (i in seq_along(reg.list)) {
    region <- reg.list[i]
    region.title <- title.list[i]
    for (proj.int in proj.ints) {
      print(proj.int)
      shape.name <- region
      
      clip.shp <- spTransform(readOGR(shape.dir,shape.name, stringsAsFactors=F),CRS("+init=epsg:4326"))
    
      go <- make.tables(gcm.list,ds.type,region,region.title,scenario,clip.shp,
                        past.int,proj.int,
                        proj.dir,read.dir,pctl)

    }
  }
}




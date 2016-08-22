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
                  'prcptotETCCDI')

climdex.list <- c('fdETCCDI',
                  'suETCCDI',
                  'idETCCDI',
                  'trETCCDI',
                  'gslETCCDI',
                  'txxETCCDI',
                  'tnnETCCDI',
                  'cddETCCDI',
                  'cwdETCCDI',
                  'r95pETCCDI',
                  'r99pETCCDI')

climdex.list <- c('suETCCDI')


model.list <- list('ccsm-crcm',
                   'ccsm-mm5i',
                   'ccsm-wrfg',
                   'cgcm3-crcm',
                   'cgcm3-rcm3',
                   'cgcm3-wrfg',
                   'gfdl-ecp2',
                   'gfdl-hrm3',
                   'gfdl-rcm3',
                   'hadcm3-hrm3')

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




reg.list <-  c('skeena','northeast','kootenay','omineca','west','cariboo','thompson','south')
title.list <- c('Skeena','Northeast','Kootenay','Omineca','West','Cariboo','Thompson','South')
##reg.list <- 'inshuck_road_boundary_10'
##reg.list <- c('coast_mountains','georgia_depression','central_interior','southern_interior','southern_mountains',
##              'boreal_interior','boreal_plains','taiga_plains','northern_boreal')
##reg.list <- 'north_van'
##region.title <- 'North Vancouver'
##proj.dir <- '/storage/data/projects/rci/data/assessments/metro_van/'
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

##proj.dir <- '/storage/data/projects/rci/data/forestry/ecoprovinces/'


if (scenario=='rcp26')
  read.dir <- '/storage/data/projects/rci/data/stat.downscaling/BCCAQ/bccaq_gcm/rcp26/climdex/full/'
if (scenario=='rcp45')
  read.dir <- '/storage/data/projects/rci/data/stat.downscaling/BCCAQ/bccaq_gcm/rcp45/climdex/full/'
if (scenario=='rcp85')
  read.dir <- '/storage/data/climate/downscale/CMIP5/BCCAQ/climdex/bc_subset/'

shape.dir <- paste(proj.dir,'shapefiles/',sep='')

for (i in seq_along(reg.list)) {
  region <- reg.list[i]
  region.title <- title.list[i]
  for (proj.int in proj.ints) {
    shape.name <- region
    clip.shp <- readOGR(shape.dir,shape.name, stringsAsFactors=F)
    
    go <- make.tables(gcm.list,ds.type,region,region.title,scenario,clip.shp,
                      past.int,proj.int,
                      proj.dir,read.dir,pctl)
  }
}





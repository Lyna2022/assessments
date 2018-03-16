##Script to produce tables of projected temperature and precipitation changes
source('/storage/home/ssobie/code/repos/assessments/calc.t.and.p.tables.r')


##-----------------------------------------------------------------------------------------------

var.list <- c('pr','tasmax','tasmin')
##var.list <- 'snowdepth'
ds.type <- 'bccaq'  ## 'rcm'
type <- 'gcm'

reg.list <- 'northeast'
reg.titles <- 'Northeast'

proj.dir <- '/storage/data/projects/rci/data/assessments/northeast/'


past.int <- '1971-2000'
pctl <- TRUE ##Controls whether the percentiles (10,50,90) are added to the ensemble

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

gcm.list <- c('CNRM-CM5','CanESM2','ACCESS1-0')
scen.list <- 'rcp85' ###c('rcp26','rcp45','rcp85')
proj.list <- c('2011-2040','2041-2070','2071-2100')

for (i in seq_along(reg.list)) {        
   region <- reg.list[i]
   print(region)
   region.title <- reg.titles[i]
   shape.dir <- '/storage/data/projects/rci/data/assessments/northeast/shapefiles/'
   shape.name <- region

   clip.shp <- spTransform(readOGR(shape.dir,shape.name, stringsAsFactors=F),CRS("+init=epsg:4326"))

   for (scenario in scen.list) {
     print(scenario)
     read.dir <- paste('/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/assessment_subsets/',region,'/',scenario,'/',sep='')

     model.list <- gcm.list
     if (scenario=='rcp26') {
        model.list <- rcp26.list
     }
     for (proj.int in proj.list) {
         print(proj.int)
         make.tables(var.list,model.list,
                       ds.type,region,region.title,clip.shp,type,scenario,
                       proj.dir,read.dir,write.dir=proj.dir,
                       past.int=past.int,proj.int=proj.int,pctl=pctl)   
     }
   }
}
  

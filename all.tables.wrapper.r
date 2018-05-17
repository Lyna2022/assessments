##Script to produce tables of projected temperature and precipitation changes

source('/storage/home/ssobie/code/repos/assessments/calc.t.and.p.tables.r')
source('/storage/home/ssobie/code/repos/assessments/calc.rp.tables.r')
source('/storage/home/ssobie/code/repos/assessments/calc.degree.days.tables.r')
source('/storage/home/ssobie/code/repos/assessments/calc.climdex.tables.r')


##-----------------------------------------------------------------------------------------------
##Info to be passed in:

region <- 'richmond'
region.title <- 'Richmond'
proj.dir <- '/storage/data/projects/rci/data/assessments/van_coastal_health/production/richmond/'
read.dir <- paste('/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/assessment_subsets/van_coastal_health/rcp85/',sep='')

gcm.list <- c('ACCESS1-0','CanESM2','CCSM4','CNRM-CM5','CSIRO-Mk3-6-0','GFDL-ESM2G',
              'HadGEM2-CC','inmcm4','HadGEM2-ES','MIROC5','MPI-ESM-LR','MRI-CGCM3')

##-----------------------------------------------------------------------------------------------
ds.type <- 'bccaq2'
type <- 'gcm'
scenario <- 'rcp85'
past.int <- '1971-2000'
pctl <- TRUE ##Controls whether the percentiles (10,50,90) are added to the ensemble
proj.list <- c('2011-2040','2041-2070','2071-2100')

shape.dir <- paste0('/storage/data/projects/rci/data/assessments/shapefiles/',region,'/')
 
shape.name <- region
clip.shp <- spTransform(readOGR(shape.dir,shape.name, stringsAsFactors=F),CRS("+init=epsg:4326"))

##Loop over intervals
for (proj.int in proj.list) {
    print(proj.int)

    ##T and P
    make.seas.tables(c('pr','tasmax','tasmin'),gcm.list,
                    ds.type,region,region.title,clip.shp,type,scenario,
                    proj.dir,read.dir,write.dir=proj.dir,
                    past.int=past.int,proj.int=proj.int,pctl=pctl)   

    ##Degree Days                    
    make.dd.tables(c('cdd','gdd','fdd','hdd'),gcm.list,
                   ds.type,region,region.title,scenario,clip.shp,
                   past.int,proj.int,
                   proj.dir,read.dir,pctl)
                 
    ##Return Periods                   
    make.rp.tables(c('pr','tasmax','tasmin'),gcm.list,
                   ds.type,region,region.title,clip.shp,type,rperiod='20',scenario,
                   proj.dir,read.dir,proj.dir,pctl,
                   past.int,proj.int) 

    ##Climdex


}
  

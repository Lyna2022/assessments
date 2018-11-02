##Script to find the stations with a particular climatology
##Use Euclidean distance to compare seasonal and annual climatologies

headers <- c('station_id', 'flag', 'station_name', 
             'elevation', 'elev flag', 'long', 'lat',
             'jan', 'feb', 'mar', 'apr', 'may', 'jun',
             'jul', 'aug', 'sept', 'oct', 'nov', 'dec', 'annual')

pcds.dir <- '/storage/data/projects/rci/data/assessments/bc/pcds/'

tasmax.raw <- read.csv(paste0(pcds.dir,'tx_uscdn_8110.csv'),header=FALSE,as.is=T)
tasmin.raw <- read.csv(paste0(pcds.dir,'tn_uscdn_8110.csv'),header=FALSE,as.is=T)

names(tasmax.raw) <- headers
names(tasmin.raw) <- headers

tasmax.flag <- tasmax.raw$annual == -9999
tasmin.flag <- tasmin.raw$annual == -9999

tasmax.stns <- tasmax.raw[!tasmax.flag,]
tasmin.stns <- tasmin.raw[!tasmin.flag,]

tasmax.ids <- paste(tasmax.stns$station_id,tasmax.stns$station_name,sep='-')
tasmin.ids <- paste(tasmin.stns$station_id,tasmin.stns$station_name,sep='-')

tasmax.match <- tasmin.ids %in% tasmax.ids
tasmin.match <- tasmax.ids %in% tasmin.ids

tasmax.sub <- tasmax.stns[tasmin.match,]
tasmin.sub <- tasmin.stns[tasmax.match,]


tasmax.order <- tasmax.sub[order(tasmax.sub[,1]),]
tasmin.order <- tasmin.sub[order(tasmin.sub[,1]),]

tas <- (tasmax.order[,8:20]/10)## + tasmin.order[,8:20]/10)/2

tas.jun <- tas$jun
tas.jul <- tas$jul
tas.aug <- tas$aug

tas.sum <- (tas.jun+tas.jul+tas.aug)/3

##Fort St John

tx.2020s <- 20.9+1.4
tx.2050s <- 20.9+3.2
tx.2080s <- 20.9+5.4 

tn.2020s <- -3.7+1.8
tn.2050s <- -3.7+3.6
tn.2080s <- -3.7+5.8 

ts.2020s <- tx.2020s##+tn.2020s)/2
ts.2050s <- tx.2050s##+tn.2050s)/2
ts.2080s <- tx.2080s##+tn.2080s)/2

analogues.2020s <- tasmax.order$station_name[which(abs(ts.2020s-tas.sum)<0.1)]
analogues.2050s <- tasmax.order$station_name[which(abs(ts.2050s-tas.sum)<0.1)]
analogues.2080s <- tasmax.order$station_name[which(abs(ts.2080s-tas.sum)<0.1)]


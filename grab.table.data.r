##Script to sort out the data requested for Okanagan


read.dir <- '/home/data/projects/rci/data/assessments/okanagan/tables/okanagan/bccaq/'

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

get.scenarios <- function(var.name,seas,interval,scenario,type) {
  
  file.name <- paste(type,'.',var.name,'.',interval,'.values.csv',sep='')

  if (grepl('(hdd|gdd|ffd|pas)',var.name)) {
    file.scen <- as.matrix(read.csv(paste(read.dir,scenario,'/degree_days/',var.name,'/',file.name,sep=''),header=TRUE,as.is=TRUE))
  } else if (grepl('(rp)',var.name)) {
    file.scen <- as.matrix(read.csv(paste(read.dir,scenario,'/return_periods/',var.name,'/',file.name,sep=''),header=TRUE,as.is=TRUE))
  } else {
    file.scen <- as.matrix(read.csv(paste(read.dir,scenario,'/',var.name,'/',file.name,sep=''),header=TRUE,as.is=TRUE))
  }
  
  models <- file.scen[,1]
  mod.sub <- models %in% gcm.list
  data.names <- file.scen[1,]
  seas.sub <- data.names %in% seas
  data.scen <- file.scen[which(mod.sub),which(seas.sub)]
  print(var.name)
  print(seas)
  print(interval)
  print(scenario)

  return(data.scen)
}
 
get.data <- function(var.name,seas,interval,type) {
  data.rcp26 <- get.scenarios(var.name,seas,interval,'rcp26',type)
  data.rcp45 <- get.scenarios(var.name,seas,interval,'rcp45',type)
  data.rcp85 <- get.scenarios(var.name,seas,interval,'rcp85',type)
  data.comb <- as.numeric(c(data.rcp26,data.rcp45,data.rcp85))
  data.stats <- c(quantile(data.comb,0.1,na.rm=T),
                  quantile(data.comb,0.5,na.rm=T),
                  quantile(data.comb,0.9,na.rm=T))
  return(data.stats)
}

seasons <- c('Winter','Spring','Summer','Fall','Annual')

##Precip
var.name <- 'pr'
pr.header <- rbind(c('Season','Past (mm)','','2020s (Percent Change)','','','2050s (Percent Change)',''),
                   c('','','10th %','Average','90th %','10th %','Average','90th %'))

pr.vals <- matrix(0,nrow=5,ncol=7)
for (s in seq_along(seasons)) {
  seas <- seasons[s]
  vals <- c(round(get.data(var.name,seas,'1971-2000','past')[2]),
            round(get.data(var.name,seas,'2011-2040','percent.anomalies'),1),
            round(get.data(var.name,seas,'2041-2070','percent.anomalies'),1))
  pr.vals[s,] <- vals
}

pr.data <- cbind(seasons,pr.vals)
pr.final <- rbind(pr.header,pr.data)

##Precip | Past |        2020s       |        2050s       |
##       |      | 10th  | Avg | 90th | 10th  | Avg | 90th |
##Winter |
##Spring |
##Summer |
##Fall   |
##Annual |

##-----------------------------------------------
##tas
var.name <- 'tas'
tas.header <- rbind(c('Season','Past (degC)','','2020s (degC Change)','','','2050s (degC Change)',''),
                    c('','','10th %','Average','90th %','10th %','Average','90th %'))

tas.vals <- matrix(0,nrow=5,ncol=7)
for (s in seq_along(seasons)) {
  seas <- seasons[s]
  vals <- c(round(get.data(var.name,seas,'1971-2000','past')[2]),
            round(get.data(var.name,seas,'2011-2040','abs.anomalies'),1),
            round(get.data(var.name,seas,'2041-2070','abs.anomalies'),1))
  tas.vals[s,] <- vals
}

tas.data <- cbind(seasons,tas.vals)
tas.final <- rbind(tas.header,tas.data)


##-----------------------------------------------
##climdex
var.name <- 'rx1dayETCCDI'
clim.header <- rbind(c('Season','Past (mm/day)','','2020s (Percent Change)','','','2050s (Percent Change)',''),
                    c('','','10th %','Average','90th %','10th %','Average','90th %'))

clim.vals <- matrix(0,nrow=5,ncol=7)
for (s in seq_along(seasons)) {
  seas <- seasons[s]
  vals <- c(round(get.data(var.name,seas,'1971-2000','past')[2]),
            round(get.data(var.name,seas,'2011-2040','percent.anomalies'),1),
            round(get.data(var.name,seas,'2041-2070','percent.anomalies'),1))
  clim.vals[s,] <- vals
}

clim.data <- cbind(seasons,clim.vals)
rx1.final <- rbind(clim.header,clim.data)

var.name <- 'tx90pETCCDI'
clim.header <- rbind(c('Season','Past (% days)','','2020s (% Days Change)','','','2050s (% Days Change)',''),
                    c('','','10th %','Average','90th %','10th %','Average','90th %'))

clim.vals <- matrix(0,nrow=5,ncol=7)
for (s in seq_along(seasons)) {
  seas <- seasons[s]
  vals <- c(round(get.data(var.name,seas,'1971-2000','past')[2]),
            round(get.data(var.name,seas,'2011-2040','abs.anomalies'),1),
            round(get.data(var.name,seas,'2041-2070','abs.anomalies'),1))
  clim.vals[s,] <- vals
}
clim.data <- cbind(seasons,clim.vals)
tx90p.final <- rbind(clim.header,clim.data)
write.table(tx90p.final,file='/home/data/projects/rci/data/assessments/okanagan/tables/tx90p.final.csv',sep=',',quote=F,row.name=F,col.name=F)

##
var.name <- 'r95pETCCDI'
clim.header <- rbind(c('Season','Past (mm)','','2020s (Percent Change)','','','2050s (Percent Change)',''),
                    c('','','10th %','Average','90th %','10th %','Average','90th %'))

clim.vals <- vector(mode='numeric',length=7)
seas <- 'Annual'
vals <- c(round(get.data(var.name,seas,'1971-2000','past')[2]),
          round(get.data(var.name,seas,'2011-2040','percent.anomalies'),1),
          round(get.data(var.name,seas,'2041-2070','percent.anomalies'),1))
clim.vals <- vals
clim.data <- c(seas,clim.vals)
r95p.final <- rbind(clim.header,clim.data)


var.name <- 'r99pETCCDI'
clim.header <- rbind(c('Season','Past (mm)','','2020s (Percent Change)','','','2050s (Percent Change)',''),
                    c('','','10th %','Average','90th %','10th %','Average','90th %'))

clim.vals <- vector(mode='numeric',length=7)
seas <- 'Annual'
vals <- c(round(get.data(var.name,seas,'1971-2000','past')[2]),
          round(get.data(var.name,seas,'2011-2040','percent.anomalies'),1),
          round(get.data(var.name,seas,'2041-2070','percent.anomalies'),1))
clim.vals <- vals
clim.data <- c(seas,clim.vals)
r99p.final <- rbind(clim.header,clim.data)


write.table(pr.final,file='/home/data/projects/rci/data/assessments/okanagan/tables/pr.final.csv',sep=',',quote=F,row.name=F,col.name=F)
write.table(tas.final,file='/home/data/projects/rci/data/assessments/okanagan/tables/tas.final.csv',sep=',',quote=F,row.name=F,col.name=F)
write.table(rx1.final,file='/home/data/projects/rci/data/assessments/okanagan/tables/rx1day.final.csv',sep=',',quote=F,row.name=F,col.name=F)
write.table(r99p.final,file='/home/data/projects/rci/data/assessments/okanagan/tables/r99p.final.csv',sep=',',quote=F,row.name=F,col.name=F)
write.table(r95p.final,file='/home/data/projects/rci/data/assessments/okanagan/tables/r95p.final.csv',sep=',',quote=F,row.name=F,col.name=F)





##-----------------------------------------------
##Degree Days
var.name <- 'gdd'
clim.header <- rbind(c('Season','Past (Degree Days)','','2020s (Degree Day Change)','','','2050s (Degree Day Change)',''),
                    c('','','10th %','Average','90th %','10th %','Average','90th %'))

clim.vals <- vector(mode='numeric',length=7)
seas <- 'Annual'
vals <- c(round(get.data(var.name,seas,'1971-2000','past')[2]),
          round(get.data(var.name,seas,'2011-2040','abs.anomalies'),1),
          round(get.data(var.name,seas,'2041-2070','abs.anomalies'),1))
clim.vals <- vals
clim.data <- c(seas,clim.vals)
gdd.final <- rbind(clim.header,clim.data)

var.name <- 'hdd'
clim.header <- rbind(c('Season','Past (Degree Days)','','2020s (Degree Day Change)','','','2050s (Degree Day Change)',''),
                    c('','','10th %','Average','90th %','10th %','Average','90th %'))

clim.vals <- vector(mode='numeric',length=7)
seas <- 'Annual'
vals <- c(round(get.data(var.name,seas,'1971-2000','past')[2]),
          round(get.data(var.name,seas,'2011-2040','abs.anomalies'),1),
          round(get.data(var.name,seas,'2041-2070','abs.anomalies'),1))
clim.vals <- vals
clim.data <- c(seas,clim.vals)
hdd.final <- rbind(clim.header,clim.data)

var.name <- 'ffd'
clim.header <- rbind(c('Season','Past (Days)','','2020s (Day Change)','','','2050s (Day Change)',''),
                    c('','','10th %','Average','90th %','10th %','Average','90th %'))

clim.vals <- vector(mode='numeric',length=7)
seas <- 'Annual'
vals <- c(round(get.data(var.name,seas,'1971-2000','past')[2]),
          round(get.data(var.name,seas,'2011-2040','abs.anomalies'),1),
          round(get.data(var.name,seas,'2041-2070','abs.anomalies'),1))
clim.vals <- vals
clim.data <- c(seas,clim.vals)
ffd.final <- rbind(clim.header,clim.data)


write.table(gdd.final,file='/home/data/projects/rci/data/assessments/okanagan/tables/gdd.final.csv',sep=',',quote=F,row.name=F,col.name=F)
write.table(hdd.final,file='/home/data/projects/rci/data/assessments/okanagan/tables/hdd.final.csv',sep=',',quote=F,row.name=F,col.name=F)
write.table(ffd.final,file='/home/data/projects/rci/data/assessments/okanagan/tables/ffd.final.csv',sep=',',quote=F,row.name=F,col.name=F)

if (1==1) {
var.name <- 'pas'
clim.header <- rbind(c('Season','Past (cm)','','2020s (Percent Change)','','','2050s (Percent Change)',''),
                    c('','','10th %','Average','90th %','10th %','Average','90th %'))

clim.vals <- vector(mode='numeric',length=7)
seas <- 'Annual'
vals <- c(round(get.data(var.name,seas,'1971-2000','past')[2]),
          round(get.data(var.name,seas,'2011-2040','percent.anomalies'),1),
          round(get.data(var.name,seas,'2041-2070','percent.anomalies'),1))
clim.vals <- vals
clim.data <- c(seas,clim.vals)
pas.final <- rbind(clim.header,clim.data)

write.table(pas.final,file='/home/data/projects/rci/data/assessments/okanagan/tables/pas.final.csv',sep=',',quote=F,row.name=F,col.name=F)
}



##------------------------------------------------------------------
##Return Periods

get.rp.scenarios <- function(var.name,rperiod,interval,scenario,type) {
  
  file.name <- paste(type,'.',var.name,'.rp.',rperiod,'.ratio.',interval,'.values.csv',sep='')
  file.scen <- as.matrix(read.csv(paste(read.dir,scenario,'/return_periods/',var.name,'/',file.name,sep=''),header=TRUE,as.is=TRUE))
  models <- file.scen[,1]
  mod.sub <- models %in% gcm.list
  data.scen <- file.scen[which(mod.sub),2]
  return(data.scen)
}

get.rp.data <- function(var.name,rperiod,interval,type) {
  data.rcp26 <- get.rp.scenarios(var.name,rperiod,interval,'rcp26',type)
  data.rcp45 <- get.rp.scenarios(var.name,rperiod,interval,'rcp45',type)
  data.rcp85 <- get.rp.scenarios(var.name,rperiod,interval,'rcp85',type)
  data.comb <- as.numeric(c(data.rcp26,data.rcp45,data.rcp85))
  data.stats <- c(quantile(data.comb,0.1,na.rm=T),
                  quantile(data.comb,0.5,na.rm=T),
                  quantile(data.comb,0.9,na.rm=T))
  return(data.stats)
}

var.name <- 'pr'
rperiod <- 10

clim.header <- rbind(c('Season','Past (mm/day)','','2020s (Percent Change)','','','2050s (Percent Change)',''),
                    c('','','10th %','Average','90th %','10th %','Average','90th %'))

clim.vals <- vector(mode='numeric',length=7)
seas <- 'Annual'
vals <- round(get.rp.data(var.name,rperiod,'2041-2070','future'),1)
clim.vals <- vals
clim.data <- c(seas,clim.vals)
pr10.final <- rbind(clim.header,clim.data)

#write.table(pr10.final,file='/home/data/projects/rci/data/assessments/okanagan/tables/pr10.final.csv',sep=',',quote=F,row.name=F,col.name=F)

var.name <- 'tasmax'
clim.header <- rbind(c('Season','Past (degC)','','2020s (DegC Change)','','','2050s (DegC Change)',''),
                    c('','','10th %','Average','90th %','10th %','Average','90th %'))

clim.vals <- vector(mode='numeric',length=7)
seas <- 'Annual'
vals <-  round(get.rp.data(var.name,rperiod,'2041-2070','future'),1)
clim.vals <- vals
clim.data <- c(seas,clim.vals)
tx10.final <- rbind(clim.header,clim.data)
#write.table(tx10.final,file='/home/data/projects/rci/data/assessments/okanagan/tables/tx10.final.csv',sep=',',quote=F,row.name=F,col.name=F)


##-------------------------------------------------
##Group the similar data together

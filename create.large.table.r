##Script to sort out the data requested for Okanagan

region <- 'van_city'
read.dir <- paste('/home/data/projects/rci/data/assessments/metro_van/tables/',region,'/bccaq/',sep='')

gcm.list <- c('CanESM2',
              'CCSM4',
              'CNRM-CM5',
              'CSIRO-Mk3-6-0',
              'GFDL-ESM2G',              
              'HadGEM2-ES',              
              'MIROC5',
              'MPI-ESM-LR',
              'MRI-CGCM3')

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
 
get.var.units <- function(var.name) {

  leg.label <- NA
  if (grepl("(tas|txx|tnn|tmax|tmin)", var.name))
    leg.label <- '\u00B0C'
  if (grepl("(pr|rx|r9|RP|rp)", var.name))
    leg.label <- 'mm'
  if (grepl("(pas|snowdepth)", var.name))
    leg.label <- 'cm'
  if (grepl("(tx90|tn10)", var.name))
    leg.label <- '%'
  if (grepl("(dd)", var.name))
    leg.label <- 'Degree days'
  if (grepl("(fd|cdd|cwd|su|gsl|id|trE|s30)", var.name))
    leg.label <- 'days'
  
  return(leg.label)
} 

get.round.val <- function(var.name) {
  rd <- 0
  if (grepl("(dd)", var.name))
    rd <- 0    
  if (grepl("(tas|txx|tnn|tmax|tmin|trE|cddE|cwdE|idE)", var.name))
    rd <- 1
  if (grepl("(pr|rx|r9|RP|rp|tx90|tn10)", var.name))
    rd <- 0
  if (grepl("(pas|snowdepth)", var.name))
    rd <- 0
  return(rd)
} 

get.scenarios <- function(var.name,seas,interval,scenario,type,rp=FALSE) {
  
  file.name <- paste(type,'.',var.name,'.',interval,'.values.csv',sep='')

  if (grepl('(^cdd$|hdd|gdd|ffd|pas|s30)',var.name)) {
    file.scen <- as.matrix(read.csv(paste(read.dir,scenario,'/degree_days/',var.name,'/',file.name,sep=''),header=TRUE,as.is=TRUE))
  } else if (rp) {
    file.name <- paste(type,'.',var.name,'.rp.20.',interval,'.values.csv',sep='')
    file.scen <- as.matrix(read.csv(paste(read.dir,scenario,'/return_periods/',var.name,'/',file.name,sep=''),header=TRUE,as.is=TRUE))
    print(var.name)
    print(file.scen)
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
 
get.data <- function(var.name,seas,interval,type,scenario,rp=FALSE) {
  data.rcp <- get.scenarios(var.name,seas,interval,scenario,type,rp)
#  data.rcp26 <- get.scenarios(var.name,seas,interval,'rcp26',type)  
#  data.rcp45 <- get.scenarios(var.name,seas,interval,'rcp45',type)
#  data.rcp85 <- get.scenarios(var.name,seas,interval,'rcp85',type)
  data.comb <- as.numeric(data.rcp) ##as.numeric(c(data.rcp26,data.rcp45,data.rcp85))##
  if (var.name=='snowdepth' & type!='percent.anomalies')
    data.comb <- data.comb*100
  data.stats <- c(mean(data.comb,na.rm=T,finite=T),
                  quantile(data.comb,0.1,na.rm=T,finite=T),
                  quantile(data.comb,0.9,na.rm=T,finite=T))
  names(data.stats) <- c('avg','10%','90%')
  return(data.stats)
}
 
get.seasonal.data <- function(var.name,scenario) {
  seasons <- c('Winter','Spring','Summer','Fall','Annual')
  pr.vals <- matrix(0,nrow=5,ncol=13)
  rd <- get.round.val(var.name)
  for (s in seq_along(seasons)) {
    seas <- seasons[s]
    vals <- c(round(get.data(var.name,seas,'1971-2000','past',scenario)[1],rd),
              round(get.data(var.name,seas,'2011-2040','abs.anomalies',scenario),rd),
              round(get.data(var.name,seas,'2041-2070','abs.anomalies',scenario),rd),
              round(get.data(var.name,seas,'2071-2100','abs.anomalies',scenario),rd),              
              round(get.data(var.name,seas,'2011-2040','percent.anomalies',scenario),1),
              round(get.data(var.name,seas,'2041-2070','percent.anomalies',scenario),1),
              round(get.data(var.name,seas,'2071-2100','percent.anomalies',scenario),1))
    lower <- vals[names(vals)=='10%']
    upper <- vals[names(vals)=='90%']
    brackets <- paste('(',paste(lower,upper,sep=' to '),')',sep='')
    avgs <- vals[names(vals)=='avg']
    proj <- avgs[-1]
    len  <- length(c(proj,brackets))
    result <- rep(0,length=len)
    result[seq(1,len,2)] <- proj
    result[seq(2,len,2)] <- brackets
    result <- c(avgs[1],result)
    
    pr.vals[s,] <- result
  }
  pr.data <- cbind(seasons,pr.vals)
  
  return(pr.data)
}


get.annual.data <- function(var.name,scenario,rp) {
  seas <- 'Annual'
  rd <- get.round.val(var.name)
  vals <- c(round(get.data(var.name,seas,'1971-2000','past',scenario,rp)[1],rd),
            round(get.data(var.name,seas,'2011-2040','abs.anomalies',scenario,rp),rd),
            round(get.data(var.name,seas,'2041-2070','abs.anomalies',scenario,rp),rd),
            round(get.data(var.name,seas,'2071-2100','abs.anomalies',scenario,rp),rd),              
            round(get.data(var.name,seas,'2011-2040','percent.anomalies',scenario,rp),1),
            round(get.data(var.name,seas,'2041-2070','percent.anomalies',scenario,rp),1),
            round(get.data(var.name,seas,'2071-2100','percent.anomalies',scenario,rp),1))
  lower <- vals[names(vals)=='10%']
  upper <- vals[names(vals)=='90%']
  brackets <- paste('(',paste(lower,upper,sep=' to '),')',sep='')
  avgs <- vals[names(vals)=='avg']
  proj <- avgs[-1]
  len  <- length(c(proj,brackets))
  result <- rep(0,length=len)
  result[seq(1,len,2)] <- proj
  result[seq(2,len,2)] <- brackets
  result <- c('Annual',avgs[1],result)
  return(result)
}

seasonal.table <- function(var.name,scenario) {

  no.percent <- '(tasmax|tasmin|txxETCCDI|tnnETCCD|trETCCDI|suETCCDI)'
  var.units <- get.var.units(var.name)
  
  data.top <- c(toupper(var.name),paste('Past (',var.units,')',sep=''),'',
                paste('2020s Change (',var.units,')',sep=''),'',
                paste('2050s Change (',var.units,')',sep=''),'',
                paste('2080s Change (',var.units,')',sep=''),'',
                paste('2020s Percent Change (%)',sep=''),'',
                paste('2050s Percent Change (%)',sep=''),'',
                paste('2080s Percent Change (%)',sep=''))
  
  data.bottom <-c('Season','','Average','10th%-90th%','Average','10th%-90th%','Average','10th%-90th%',
                  'Average','10th%-90th%','Average','10th%-90th%','Average','10th%-90th%')
  data.header <- rbind(data.top,data.bottom)
  
  var.result <- rbind(data.header,get.seasonal.data(var.name,scenario))

  if (grepl(no.percent,var.name))
    var.result[3:7,9:14] <- NA
  return(var.result)
}


annual.table <- function(var.name,scenario,rp=FALSE) {
  
  no.percent <- '(tasmax|tasmin|txxETCCDI|tnnETCCD|trETCCDI|suETCCDI|s30)'
  var.units <- get.var.units(var.name)
  var.label <- var.name
  if (rp)
    var.label <- paste('RP20 ',var.name,sep='')
  data.top <- c(toupper(var.label),paste('Past (',var.units,')',sep=''),'',
                paste('2020s Change (',var.units,')',sep=''),'',
                paste('2050s Change (',var.units,')',sep=''),'',
                paste('2080s Change (',var.units,')',sep=''),'',
                paste('2020s Percent Change (%)',sep=''),'',
                paste('2050s Percent Change (%)',sep=''),'',
                paste('2080s Percent Change (%)',sep=''))
  
  data.bottom <-c('Annual','','Average','10th%-90th%','Average','10th%-90th%','Average','10th%-90th%',
                  'Average','10th%-90th%','Average','10th%-90th%','Average','10th%-90th%')
  data.header <- rbind(data.top,data.bottom)
  result <- get.annual.data(var.name,scenario,rp)
  var.result <- rbind(data.header,result)
  if (grepl(no.percent,var.name))
    var.result[3,9:14] <- NA
  return(var.result)
}


##---------------------------------------------------------


##Seasonal Data
##Precip | Past |        2020s       |        2050s       |
##       |      | 10th  | Avg | 90th | 10th  | Avg | 90th |
##Winter |
##Spring |
##Summer |
##Fall   |
##Annual |

#seasonal.vars <- c('pr','tasmax','tasmin','txx','tnn')

scenario <- 'rcp85'

if (1==1) {
pr.table <- seasonal.table('pr',scenario)
rx1.table <- seasonal.table('rx1dayETCCDI',scenario)
rx5.table <- seasonal.table('rx5dayETCCDI',scenario)
tx.table <- seasonal.table('tasmax',scenario)
tn.table <- seasonal.table('tasmin',scenario)
txx.table <- seasonal.table('txxETCCDI',scenario)
tnn.table <- seasonal.table('tnnETCCDI',scenario)
snow.table <- seasonal.table('snowdepth','rcp85')
seas.data <- rbind(pr.table,rx1.table,rx5.table,tx.table,tn.table,txx.table,tnn.table) #,snow.table)

}
##-----------------------------------------------
##Annual Data

cddE.table <- annual.table('cddETCCDI',scenario)
cwdE.table <- annual.table('cwdETCCDI',scenario)
r95.table <- annual.table('r95pETCCDI',scenario)
r99.table <- annual.table('r99pETCCDI',scenario)
su.table <- annual.table('suETCCDI',scenario)
s30.table <- annual.table('s30',scenario)
tr.table <- annual.table('trETCCDI',scenario)
id.table <- annual.table('idETCCDI',scenario)
fd.table <- annual.table('fdETCCDI',scenario)
gsl.table <- annual.table('gslETCCDI',scenario)

cdd.table <- annual.table('cdd',scenario)
gdd.table <- annual.table('gdd',scenario)
hdd.table <- annual.table('hdd',scenario)

pr.rp.table <- annual.table('pr',scenario,rp=TRUE)
tx.rp.table <- annual.table('tasmax',scenario,rp=TRUE)
tn.rp.table <- annual.table('tasmin',scenario,rp=TRUE)

#pr.rp.ratio <- annual.table('pr',scenario,rp=TRUE)
#tx.rp.ratio <- annual.table('tasmax',scenario,rp=TRUE)
#tn.rp.ratio <- annual.table('tasmin',scenario,rp=TRUE)

#snow.apr <- annual.table('snowdepth',scenario,rp=TRUE)
  
ann.data <- rbind(cddE.table,cwdE.table,r95.table,r99.table,su.table,s30.table,tr.table,
                  id.table,fd.table,gsl.table,cdd.table,gdd.table,hdd.table,pr.rp.table,tx.rp.table,tn.rp.table)

group.data <- rbind(seas.data,ann.data)

write.table(group.data,
            file=paste('/home/data/projects/rci/data/assessments/metro_van/tables/',region,'/',region,'_variable_table_',scenario,'.csv',sep=''),sep=',',
            quote=F,row.name=F,col.name=T)





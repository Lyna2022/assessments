##Read the monthly ensemble average time series of daily average temperature from
##the KMNI tool. This uses 39 GCMs as far as I can tell.

read.dir <- '/storage/data/projects/rci/data/assessments/'

rcp26.file <- paste0(read.dir,'icmip5_tas_Amon_modmean_rcp26_0-360E_-90-90N_n_su_000.dat')
rcp26.data <- as.matrix(read.table(rcp26.file,as.is=T,header=F))
rcp45.file <- paste0(read.dir,'icmip5_tas_Amon_modmean_rcp45_0-360E_-90-90N_n_su_000.dat')
rcp45.data <- as.matrix(read.table(rcp45.file,as.is=T,header=F))
rcp85.file <- paste0(read.dir,'icmip5_tas_Amon_modmean_rcp85_0-360E_-90-90N_n_su_000.dat')
rcp85.data <- as.matrix(read.table(rcp85.file,as.is=T,header=F))

intervals <- c('1861-1890','1971-2000','1986-2016','2011-2040','2041-2070','2071-2100','2091-2100')

clim.matrix <- matrix(0,nrow=3,ncol=length(intervals))
years <- rcp45.data[,1]
for (i in seq_along(intervals)) {
   
   bnds <- strsplit(intervals[i],'-')[[1]]
   st <- grep(bnds[1],years)
   en <- grep(bnds[2],years)
   clim.matrix[1,i] <- mean(rcp26.data[st:en,-1])
   clim.matrix[2,i] <- mean(rcp45.data[st:en,-1])
   clim.matrix[3,i] <- mean(rcp85.data[st:en,-1])

}
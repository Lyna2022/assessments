##Read the projections from the tables values
##and create simple bar plots for each region

##Range for the observations will need to come from
##annual ANUSPLIN to get standard deviation
##-------------------------------------------------------

get_table_seas_values <- function(region,var.name,type,interval,read.dir) {

   print(interval)                      
   table.dir <- paste0(read.dir,region,'/tables/',var.name)

   table.files <- list.files(path=table.dir,pattern=paste0(type,'.',var.name),full.name=TRUE)
   table.file <- table.files[grep(interval,table.files)]
   print(table.file)

   table.data <- read.csv(table.file,header=T,as.is=TRUE)

   seas.data <- data.matrix(table.data[c(14,15,17),14:17])

   rownames(seas.data) <- c('Mean','10th','90th')     

   colnames(seas.data) <- c('Win','Spr','Sum','Fal')

   return(seas.data)

}

get_table_standard_deviation_values <- function(region,var.name,type,interval,read.dir) {

   print(interval)                      
   table.dir <- paste0(read.dir,region,'/tables/build_code/',var.name)

   table.files <- list.files(path=table.dir,pattern=paste0(type,'.',var.name,'.standard_deviation'),full.name=TRUE)

   table.file <- table.files[grep(interval,table.files)]
   print(table.file)

   table.data <- read.csv(table.file,header=T,as.is=TRUE)

   seas.data <- data.matrix(table.data[c(14,15,17),2:5])

   rownames(seas.data) <- c('Mean','10th','90th')     

   colnames(seas.data) <- c('Win','Spr','Sum','Fal')

   return(seas.data)

}


seas_plot <- function(var.name,plot.title,xlim,xtks) {

   var.label <- switch(var.name,pr='Precipitation (mm)',tas='Temperature (\u00B0C)')

   ylim=c(0.5,5.5)
   cx <- 1.45
   plot(c(),bg='white',xlim=xlim,ylim=ylim,main=plot.title,cex.main=1.15*cx,axes=F,
        xlab=var.label,ylab='',cex.lab=cx)
   rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col='white')
   abline(h=1:5,v=xtks,col='lightgray',lty=3)
   abline(h=1.5,col='lightgray',lty=2) 
   
   axis(1,at=xtks,label=xtks,cex.lab=cx,cex.axis=cx)
   axis(4,at=c(1:5),labels=c('Var','71-00','2020s','2050s','2080s'),
          cex.axis=cx,cex.lab=cx,las=1)         
}

##--------------------------------------------------------

make_bar_plot <- function(var.name,region,input.vect,input.list,read.dir) {

   plot.dir <- paste0(read.dir,region,'/')
   plot.file <- paste0(plot.dir,region,'_bar_plot_',var.name,'_anomalies.png')
   lim.vector <- as.numeric(input.vect)
   rd <- switch(var.name,pr=50,tas=5)
   var.cols <- switch(var.name,pr=c('cyan','darkcyan','blue'),tas=c('goldenrod','orange','red'))
   seas.titles <- c('Winter','Spring','Summer','Fall')
   xlim <- c(floor(min(lim.vector)/rd)*rd,ceiling(max(lim.vector)/rd)*rd)
   xtks <- pretty(xlim)
   
   png(plot.file,width=2.5,height=5,units='in',res=300,bg='lightgray',pointsize=6)

   par(mfrow=c(4,1))
   par(mar=c(4,1.5,3,6))

   ##Seasons
   for (i in 1:4) {
      seas_plot(var.name,plot.title=seas.titles[i],xlim,xtks)
      lines(x=rep(input.list$past[1,i],2),y=c(1.8,2.2),lwd=3,lend=2)      
      lines(x=input.list$std[1:2,i],y=c(1,1),lwd=3)
      lines(x=as.numeric(input.list$one[2:3,i]),y=c(3,3),lwd=5,lend=2,col=var.cols[1])
      lines(x=(input.list$two[2:3,i]),y=c(4,4),lwd=5,lend=2,col=var.cols[2])
      lines(x=(input.list$three[2:3,i]),y=c(5,5),lwd=5,lend=2,col=var.cols[3])
      box(which='plot')
   }


   dev.off()

}


##--------------------------------------------------------
    var.name <- "pr"

read.dir <- "/storage/data/projects/rci/data/assessments/resource_regions/"
##'kootenay',
regions <- c('cariboo','northeast','omineca','skeena','south','thompson','west')

for (region in regions) {

   past.pr <- get_table_seas_values(region,var.name,type="past",interval="1971-2000",read.dir)
   past.pr.sd <- get_table_standard_deviation_values(region,var.name,type="past",interval="1971-2000",read.dir)
   pr.2020s <- get_table_seas_values(region,var.name,type="future",interval="2011-2040",read.dir)
   pr.2050s <- get_table_seas_values(region,var.name,type="future",interval="2041-2070",read.dir)
   pr.2080s <- get_table_seas_values(region,var.name,type="future",interval="2071-2100",read.dir)

   pr.var <- rbind(past.pr[1,]-past.pr.sd[1,],past.pr[1,]+past.pr.sd[1,])

   input.vect <- as.numeric(as.matrix(rbind(past.pr,pr.var,pr.2020s,pr.2050s,pr.2080s)))
   input.list <- list(past=past.pr,std=pr.var,one=pr.2020s,two=pr.2050s,three=pr.2080s)

   make_bar_plot(var.name,region,input.vect,input.list,read.dir)
}
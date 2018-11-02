
##source('/storage/home/ssobie/code/repos/assessments/vch_fraser.sites.r')
##source('/storage/home/ssobie/code/repos/assessments/interior_health.sites.r')
source('/storage/home/ssobie/code/repos/assessments/okanagan.sites.r')

for (site in sites) {

##   print(paste0(site$writeloc,site$region,'/'))
   ##Building Code
   bc.work <- paste0('qsub -N "',site$region,'.bc" -v region="',site$region,'",title="',site$title,'",readloc="',site$readloc,'",writeloc="',site$writeloc,site$region,'/" run.build.code.wrapper.pbs')
   print(paste0('Building Code Variables for ',site$region))
  system(bc.work)

   rp5.work <- paste0('qsub -N "',site$region,'.rp5" -v region="',site$region,'",title="',site$title,'",readloc="',site$readloc,'",writeloc="',site$writeloc,site$region,'/",rp="5" run.rp.wrapper.pbs')
   print(paste0('Return Period 5 Variables for ',site$region))
   system(rp5.work)

   rp50.work <- paste0('qsub -N "',site$region,'.rp50" -v region="',site$region,'",title="',site$title,'",readloc="',site$readloc,'",writeloc="',site$writeloc,site$region,'/",rp="50" run.rp.wrapper.pbs')
   print(paste0('Return Period 50 Variables for ',site$region))
   system(rp50.work)

}
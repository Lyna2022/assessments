#!/bin/bash

region='central_okanagan'
readloc='interior_health'

qsub -N "ens.tp" -v region=$region,readloc=$readloc,type="season" run.ens.mapping.pbs
qsub -N "ens.rp" -v region=$region,readloc=$readloc,type="return_periods" run.ens.mapping.pbs
qsub -N "ens.cx" -v region=$region,readloc=$readloc,type="climdex" run.ens.mapping.pbs
qsub -N "ens.dd" -v region=$region,readloc=$readloc,type="degree_days" run.ens.mapping.pbs
qsub -N "ens.qt" -v region=$region,readloc=$readloc,type="quantiles" run.ens.mapping.pbs
qsub -N "ens.pr" -v region=$region,readloc=$readloc,type="pr.vars" run.ens.mapping.pbs

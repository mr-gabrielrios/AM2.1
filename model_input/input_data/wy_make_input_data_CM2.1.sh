#!/usr/bin/env bash
# Wenchang Yang (wenchang@princeton.edu)
# Mon Jun 20 20:21:13 EDT 2022
#readme: same as input except the aerosol input data is also updated using that from the HiRAM 
##SBATCH --nodes=1                # node count
##SBATCH --ntasks-per-node=1      # number of tasks per node
# 
#SBATCH --ntasks=1               # total number of tasks across all nodes = nodes x ntasks-per-node
#SBATCH --cpus-per-task=1        # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --mem-per-cpu=4G         # memory per cpu-core (4G is default)
#SBATCH --time=24:00:00          # total run time limit (HH:MM:SS)
#SBATCH --mail-type=all          # send email when job begins/ends/fails
#SBATCH --mail-user=wenchang@princeton.edu
# 
##SBATCH --array=1-100#%32        # job array with index values 1, 2, ...,; max job # is 32 if specified
##SBATCH --output=slurm-%A.%a.out # stdout file
##SBATCH --error=slurm-%A.%a.err  # stderr file
set -ve
##env settings
#export PATH=/tigress/wenchang/miniconda3/bin:$PATH
#export PYTHONPATH=/tigress/wenchang/wython
#export PYTHONUNBUFFERED=TRUE # see https://stackoverflow.com/questions/230751/how-to-flush-output-of-print-function
#export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK #for multi-threaded job
#ii_job=$SLURM_ARRAY_TASK_ID #for job array

#base input data from MOM5
rsync -avhP /tigress/wenchang/MOM5/data/archives/CM2.1p1/INPUT/* ./
# default fv_*.res files have problems. use ESM2M fv restart files
#rm fv_*.res
#cp -f /tigress/wenchang/MOM5/data/archives/ESM2M_pi-control_C2/INPUT/fv_*.res.nc ./
#cp -f /tigress/wenchang/MOM5/data/archives/ESM2M_pi-control_C2/INPUT/atmos_tracers.res.nc ./

#rm coupler.res #use time info from namelist
#rm IC files
rm -f *.res*

#new volcanic forcing files
#rm -f extsw_data.nc extlw_data.nc omgsw_data.nc asmsw_data.nc
rsync -avhP /tigress/wenchang/modelInput/CM2.5/input/VOLCANIC/CMIP6/extsw_V3_DATATROP_RCP.nc extsw_data.nc
rsync -avhP /tigress/wenchang/modelInput/CM2.5/input/VOLCANIC/CMIP6/extlw_V3_DATATROP_RCP.nc extlw_data.nc
rsync -avhP /tigress/wenchang/modelInput/CM2.5/input/VOLCANIC/CMIP6/omgsw_V4_DATATROP_RCP.nc omgsw_data.nc
rsync -avhP /tigress/wenchang/modelInput/CM2.5/input/VOLCANIC/CMIP6/asmsw_V4_DATATROP_RCP.nc asmsw_data.nc

#longer o3 data: 1850-2099
#rm -f o3.climatology.nc
rsync -avhP /tigress/wenchang/modelInput/HIRAM/input/HIRAM.input_data/o3.climatology.nc o3.climatology.nc

#longer aerosol data: 1855-2105 (stride=10 years)
#rm -f aerosol.climatology.nc
rsync -avhP /tigress/wenchang/modelInput/HIRAM/input/HIRAM.input_data/aerosol.climatology.nc aerosol.climatology.nc

#longer radiative forcing data
for fcer in ch4 co2 f113 f11 f12 f22 n2o; do
    #rm -f ${fcer}_gblannualdata
    rsync -avhP /tigress/wenchang/modelInput/HIRAM/input/HIRAM.input_data/${fcer}_gblannualdata ${fcer}_gblannualdata
done

#make the tar file
#tar -cf input_data_CM2.1_noIC.tar ./*
chmod -w *

exit 0

#PBS -l walltime=03:00:00
#PBS -l select=2:ncpus=48:mem=64gb
#PBS -N Enet_regression_newvars

cd /rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/5_year_new_vars
module load anaconda3/personal

Rscript c_5yr_newvars_enet_regression.R


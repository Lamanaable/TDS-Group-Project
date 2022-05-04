#PBS -l walltime=03:00:00
#PBS -l select=2:ncpus=48:mem=64gb
#PBS -N Enet_5yr_regression

cd /rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/5_year
module load anaconda3/personal

Rscript c_5yr_enet_regression.R


#PBS -l walltime=03:00:00
#PBS -l select=1:ncpus=8:mem=32gb
#PBS -N Enet_regression_newvars

cd /rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/5_year_standardised
module load anaconda3/personal

Rscript 5yr_enet_regression.R


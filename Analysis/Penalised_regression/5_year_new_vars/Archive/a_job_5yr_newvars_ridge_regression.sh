#PBS -l walltime=02:00:00
#PBS -l select=1:ncpus=1:mem=40gb
#PBS -N Ridge_regression_newvars

cd /rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/5_year_new_vars
module load anaconda3/personal

Rscript a_5yr_newvars_ridge_regression.R


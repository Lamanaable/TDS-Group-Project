#PBS -l walltime=02:00:00
#PBS -l select=2:ncpus=16:mem=32gb
#PBS -N Lasso_regression_newvars

cd /rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/5_year_standardised
module load anaconda3/personal

Rscript Lasso_regression_standardised.R


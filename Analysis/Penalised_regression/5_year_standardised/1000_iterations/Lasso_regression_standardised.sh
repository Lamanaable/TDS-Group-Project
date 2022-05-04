#PBS -l walltime=06:00:00
#PBS -l select=2:ncpus=24:mem=32gb
#PBS -N Lasso_regression_1000_iterations

cd /rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/5_year_standardised/1000_iterations
module load anaconda3/personal

Rscript Lasso_regression_standardised.R


#PBS -l walltime=02:00:00
#PBS -l select=2:ncpus=16:mem=32gb
#PBS -N 5Yr_stab_lasso

cd /rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/Stability_selection/5_year
module load anaconda3/personal

Rscript sensitivity_colon_stability_selection_lasso.R


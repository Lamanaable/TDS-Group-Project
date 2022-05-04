#PBS -l walltime=05:00:00
#PBS -l select=1:ncpus=8:mem=64gb
#PBS -N colon_stab_lasso

cd /rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/Stability_selection/5_year
module load anaconda3/personal

Rscript colon_stability_selection_lasso.R


#PBS -l walltime=01:00:00
#PBS -l select=1:ncpus=8:mem=128gb
#PBS -N ridge_selected_vars

cd /rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/Stability_selection/5_year/Ridge_logistic
module load anaconda3/personal

Rscript Ridge_selected_vars.R

#PBS -l walltime=02:00:00
#PBS -l select=1:ncpus=8:mem=32gb
#PBS -N rectum_stab_lasso

cd /rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/Stability_selection/5_year
module load anaconda3/personal

Rscript rectum_stability_selection_lasso.R


#PBS -l walltime=05:00:00
#PBS -l select=2:ncpus=48:mem=64gb
#PBS -N Enet_regression

cd /rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/Subset_model
module load anaconda3/personal

Rscript b_subset_lasso_regression.R


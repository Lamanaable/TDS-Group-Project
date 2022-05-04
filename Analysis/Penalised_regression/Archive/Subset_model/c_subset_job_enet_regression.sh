#PBS -l walltime=03:00:00
#PBS -l select=1:ncpus=12:mem=32gb
#PBS -N Enet_regression

cd /rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/Subset_model
module load anaconda3/personal

Rscript c_subset_enet_regression.R



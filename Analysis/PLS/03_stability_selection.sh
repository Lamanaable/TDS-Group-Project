#PBS -l walltime=2:00:00
#PBS -l select=1:ncpus=32:mem=64gb
#PBS -N stabselection

cd /rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/PLS
module load anaconda3/personal

Rscript 03_stability_selection.R


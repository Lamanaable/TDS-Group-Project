#PBS -l walltime=10:00:00
#PBS -l select=1:ncpus=1:mem=40gb
#PBS -N SVM

cd /rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/SVM
module load anaconda3/personal

CRC_path=/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/CRC_data_final.rds

Rscript 1_SVM.R $CRC_path


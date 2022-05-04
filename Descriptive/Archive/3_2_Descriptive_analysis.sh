#PBS -l walltime=6:00:00
#PBS -l select=1:ncpus=1:mem=40gb
#PBS -N Descriptive

cd /rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration
module load anaconda3/personal

CRC_path=/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/CRC_case_control.rds

Rscript 3_2_Descriptive_analysis.R $CRC_path


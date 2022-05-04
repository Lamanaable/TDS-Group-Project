#PBS -l walltime=1:00:00
#PBS -l select=1:ncpus=1:mem=10gb
#PBS -N Matching

cd /rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration
module load anaconda3/personal

CRC_path=/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_case_data_exclusion.rds

Rscript 3_Matching.R $CRC_path


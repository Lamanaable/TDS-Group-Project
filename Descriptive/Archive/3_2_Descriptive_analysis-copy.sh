#PBS -l walltime=2:00:00
#PBS -l select=1:ncpus=1:mem=20gb
#PBS -N Descriptive

cd /rds/general/user/syl416/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration
module load anaconda3/personal

CRC_path=/rds/general/user/syl416/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/CRC_Matched_data.rds

Rscript 3_2_Descriptive_analysis-copy.R $CRC_path


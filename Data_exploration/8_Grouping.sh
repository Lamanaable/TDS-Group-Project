#PBS -l walltime=02:00:00
#PBS -l select=1:ncpus=1:mem=40gb
#PBS -N Grouping

cd /rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration
module load anaconda3/personal

CRC_path=/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_data_comorbidity.rds
women_only=/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_covars_women_only.rds

Rscript 8_Grouping.R $CRC_path $women_only


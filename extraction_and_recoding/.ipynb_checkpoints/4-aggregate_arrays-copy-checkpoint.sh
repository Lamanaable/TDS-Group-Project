#PBS -l walltime=5:00:00
#PBS -l select=1:ncpus=1:mem=20gb
#PBS -N aggregating
#PBS -q med-bio

cd /rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/extraction_and_recoding/scripts
module load anaconda3/personal

Rscript 4_aggregate_arrays_copy.R

# Creating zip file with parameters used for this project
cd ../
project_name=Group_2_CRC_project
cp -r parameters parameters_$project_name
zip -r parameters_$project_name.zip parameters_$project_name
rm -rf parameters_$project_name

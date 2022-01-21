#PBS -l walltime=5:00:00
#PBS -l select=1:ncpus=1:mem=20gb
#PBS -N dict

cd /rds/general/user/bbodinie/projects/hda_21-22/live/TDS/General/extraction_and_recoding/scripts
module load anaconda3/personal

ukb_path=/rds/general/user/bbodine/projects/hda_21-22/live/TDS/General/Data

Rscript 1-make_data_dict.R $ukb_path


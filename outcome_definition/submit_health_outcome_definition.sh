#PBS -l walltime=5:00:00
#PBS -l select=1:ncpus=1:mem=20gb
#PBS -N definition
#PBS -q med-bio

cd /rds/general/user/bbodinie/projects/hda_21-22/live/TDS/General/outcome_definition/Scripts
module load anaconda3/personal

def_path=/rds/general/user/bbodinie/projects/hda_21-22/live/TDS/General/outcome_definition/Definitions/CVD/
app_data_path=/rds/general/user/bbodinie/projects/hda_21-22/live/TDS/General/Data/ukb47946.csv
hes_main_path=/rds/general/user/bbodinie/projects/hda_21-22/live/TDS/General/Data/hesin.txt
hes_diag_path=/rds/general/user/bbodinie/projects/hda_21-22/live/TDS/General/Data/hesin_diag.txt
hes_oper_path=/rds/general/user/bbodinie/projects/hda_21-22/live/TDS/General/Data/hesin_oper.txt
death_main_path=/rds/general/user/bbodinie/projects/hda_21-22/live/TDS/General/Data/death.txt
death_cause_path=/rds/general/user/bbodinie/projects/hda_21-22/live/TDS/General/Data/death_cause.txt

Rscript extract_hes.R $def_path $app_data_path $hes_main_path $hes_diag_path $hes_oper_path

Rscript extract_death.R $def_path $app_data_path $death_main_path $death_cause_path

Rscript extract_baseline.R $def_path $app_data_path

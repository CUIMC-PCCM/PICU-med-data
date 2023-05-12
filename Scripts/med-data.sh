#\!/bin/bash
# 2023-03-13
# med management


PROJECT=$(pwd)
echo "The project folder is "
echo $PROJECT


########## set up directory structure
if [[ $1 = "setup_repo" ]]
then
  mkdir Input
  mkdir Intermediate
  mkdir Output
  mkdir StdoutStderr
  exit
fi


project_name=$(cat ./Input/med-data.yaml | shyaml get-value INPUT_VARIABLE.project_name)
echo "project name is $project_name"
########## deidentify med data
if [[ $1 = "deidentify_data" ]]
then
  echo "in deidentify_data..."
  data_file=$(cat ./Input/med-data.yaml | shyaml get-value DEIDENTIFY_DATA.data_file)
  echo $data_file
  epic_cdw=$(cat ./Input/med-data.yaml | shyaml get-value DEIDENTIFY_DATA.epic_cdw)
  echo $epic_cdw
  project_name=$(cat ./Input/med-data.yaml | shyaml get-value INPUT_VARIABLE.project_name)
  echo $project_name
  identity_header=$(cat ./Input/med-data.yaml | shyaml get-value DEIDENTIFY_DATA.identity_header)
  echo $identity_header
  key_data_file=$(cat ./Input/med-data.yaml | shyaml get-value DEIDENTIFY_DATA.key_data_file)


  # Rscript --help
  # Rscript ./Scripts/deidentify_data.R --help

	Rscript ./Scripts/deidentify_data.R --data_file $data_file --epic_cdw $epic_cdw --project_name $project_name --key_data_file $key_data_file --identity_header $identity_header
	
	  # deidentify_data.R --data_file=<data_file> --epic_cdw=<epic_cdw> --identity_header=<identity_header> --key_data_file=<key_data_file> [--project_name=<project_name>] [--debug=<debug>]


  echo "finished deidentify_data..."
  # qsub -wd $PROJECT -q centos78.q -o $PROJECT/StdoutStderr/"$time_stamp"_stdout_deidentify_data.log -e $PROJECT/Results/"$time_stamp"_stderr_cohortSelection.log -pe smp 1 -V Scripts/run_collapsing_lclust.sh "cohortSelection" $PROJECT

fi

########## deidentify med data

# if [[ $1 = "deidentify_data" ]]
# then
#   
# fi

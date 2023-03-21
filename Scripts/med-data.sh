#\!/bin/bash
# 2023-03-13
# med management


########## set up directory structure
if [[ $1 = "setup_repo" ]]
then
  mkdir Input
  mkdir Intermediate
  mkdir Output
  exit
fi


project_name=$(cat ./Input/med-data.yaml | shyaml get-value INPUT_VARIABLE.project_name)
echo "project name is $project_name"
########## deidentify med data
if [[ $1 = "deidentify_data" ]]
then
  project_name=$(cat ./Input/med-data.yaml | shyaml get-value INPUT_VARIABLE.project_name)
  identity_header=$(cat ./Input/med-data.yaml | shyaml get-value INPUT_VARIABLE.identified_data_column)
  path_to_key_data=$(cat ./Input/med-data.yaml | shyaml get-value INPUT_VARIABLE.identified_data_key)


	Rscript ./Scripts/deidentify_data.R --path_to_data $identified_data  --identity_header $identify_header --path_to_key_data path_to_key_data --project_name $project_name

fi




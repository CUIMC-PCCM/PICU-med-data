#\!/bin/bash
# 2023-03-13
# med management by josh and andy

project_name=$(cat ./Input/med-data.yaml | shyaml get-value INPUT_VARIABLE.project_name)
echo "project name is $project_name"
########## deidentify med data
if [[ $1 = "deidentify_data" ]]
then
  project_name=$(cat ./Input/med-data.yaml | shyaml get-value INPUT_VARIABLE.project_name)
  identity_header=$(cat ./Input/med-data.yaml | shyaml get-value DEIDENTIFY_DATA.identified_data_column)
  data_file=$(cat ./Input/med-data.yaml | shyaml get-value DEIDENTIFY_DATA.data_file)
  epic_cdw=$(cat ./Input/med-data.yaml | shyaml get-value DEIDENTIFY_DATA.epic_cdw)
  key_data_file=$(cat ./Input/med-data.yaml | shyaml get-value DEIDENTIFY_DATA.key_data_file)


	Rscript ./Scripts/deidentify_data.R --data_file $data_file  --identity_header $identify_header --epic_cdw $epic_cdw --project_name $project_name --key_data_file $key_data_file

fi

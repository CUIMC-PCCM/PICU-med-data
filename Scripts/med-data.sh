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



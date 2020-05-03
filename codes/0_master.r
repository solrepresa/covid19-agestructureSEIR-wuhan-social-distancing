# Preprocessing of Argenina data
source('codes/ARG_Data.r')

# load relevant the data files
source('codes/1_loadData.r')

# load of Argentina data files
source('codes/1_loadData_Arg.r')

## >> REEMPLAZAR wuhancase por localCase

# source the age-structured SEIcIscR model functions 
source('codes/function_modelSEIcIscR.r')

# source the age-structured SEIcIscR model functions 
source('codes/function_postprocessing.r')

# simulate N oubtreaks
source('codes/2_simOutbreak_ncov_SEIR.r')
source('codes/2_simOutbreak_ncov_SEIcIscR.r')

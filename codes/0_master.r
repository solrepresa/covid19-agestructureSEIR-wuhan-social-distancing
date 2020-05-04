# Preprocessing Argentina data
source('codes/ARG_Data.r')

# load relevant the data files
source('codes/1_loadData.r')

# load of Argentina data files
source('codes/1_loadData_Arg.r')

# source the age-structured SEIcIscR model functions 
source('codes/function_modelSEIcIscR.r')

# source the age-structured SEIcIscR model functions 
source('codes/function_postprocessing.r')

# simulate N oubtreaks
#source('codes/2_simOutbreak_ncov_SEIR.r')  # WUHAN 
#source('codes/2_simOutbreak_ncov_SEIcIscR.r') # WUHAN
source('codes/2_simOutbreak_ncov_SEIR_ARG.r')
source('codes/2_simOutbreak_ncov_SEIcIscR_ARG.r')

# plots
source('codes/3_plots_simOutbreak_ARG.r')

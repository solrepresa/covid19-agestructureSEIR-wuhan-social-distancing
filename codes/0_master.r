#setwd("C:\\Users\\solre\\covid19-agestructureSEIR-wuhan-social-distancing")

# Preprocessing Argentina data
source('codes/ARG_Data.r')

# load relevant the data files
source('codes/1_loadData.r')

# load of Argentina data files
source('codes/1_loadData_Arg.r')

# source the age-structured SEIcIscR model functions 
source('codes/function_modelSEIcIscR.r')

# source the age-structured SEIcIscR model functions 
# modified functions for argentina's needs
source('codes/function_modelSEIcIscR_mod.r')

# source the age-structured SEIcIscR model functions 
source('codes/function_postprocessing.r')

# function plots
source('codes/function_plots_simOutbreak.r')

# Simulation Parameters
source("codes/parameter_simulation_ARG.r")

# simulate N oubtreaks
#source('codes/2_simOutbreak_ncov_SEIR.r')  # WUHAN 
#source('codes/2_simOutbreak_ncov_SEIcIscR.r') # WUHAN
#source('codes/2_simOutbreak_ncov_SEIR_ARG.r')
#source('codes/2_simOutbreak_ncov_SEIcIscR_ARG.r')
source('codes/2_simOutbreak_ncov_SEIcIscR_ARG_mod.r')   # Modificar p_school,  p_work, p_other
source('codes/2_sim_10_Outbreak_ncov_SEIcIscR_ARG.r')    #Modificado para 10 pasos

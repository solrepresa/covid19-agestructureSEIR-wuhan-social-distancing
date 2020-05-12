# Simulation Parameters

## To simulate SEIcIscR outbreaks


##     PARAMETERS     ##
durInfSim = 7                     # duration of infection (days)
initialI = 0.000035               # initial infected  n= ~35 infected (1000000)
R0tpostoutbreak = 1.17
dateSSC = as.Date('2020-03-19')   # date Start School Closure
dateSII = as.Date('2020-03-19')   # date Start Intense Intervention 



##     NuMBER SIMULATION     ##
nsim = 10
dateStart = as.Date('2020-03-05')


# NO INTERVENTION  # # # # # # # #
pWorkOpen0 = c(1, 1, 1, 1)
pSchoolOpen0 = c(0, 0, 0, 1)
pOtherOpen0 = c(0.1, 0.1, 0.1, 0.1)
numWeekStagger0 = c(0,0,0)


# Case 1  # # # # # # # # # # # #

pWorkOpen1 = c(1, 1, 1, 1)
pSchoolOpen1 = c(0, 0, 0, 1)
pOtherOpen1 = c(0.1, 0.1, 0.1, 0.1)
numWeekStagger1 = c(0,0,0)
dateEII1 = as.Date('2020-04-10')


# Case 2  # # # # # # # # # # # #
pWorkOpen2 = c(1, 1, 1, 1)
pSchoolOpen2 = c(0, 0, 0, 1)
pOtherOpen2 = c(0.1, 0.1, 0.1, 0.1)
numWeekStagger2 = c(0,0,0)
dateEII2 =as.Date('2020-04-24')


# Case 3  # # # # # # # # # # # #
pWorkOpen3 = c(1, 1, 1, 1)
pSchoolOpen3 = c(0, 0, 0.2, 1)
pOtherOpen3 = c(0.1, 0.1, 0.1, 0.1)
numWeekStagger3 = c(0,0,0)
dateEII3 = as.Date('2020-05-10')
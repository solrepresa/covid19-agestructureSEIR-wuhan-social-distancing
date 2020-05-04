## 2_simOutbreak_ncov_SEIcIscR_ARG.r
## To simulate n_simSEIR outbreaks >>> ARGENTINA

# CUARENTENA: 05/03/2020

## To simulate SEIcIscR outbreaks: 


##     PARAMETERS     ##
durInfSim = 7                     # duration of infection (days)
initialI = 0.000035               # initial infected  n= ~35 infected
dateSSC = as.Date('2020-03-19')   # date Start School Closure
dateSII = as.Date('2020-03-19')   # date Start Intense Intervention 


##     NuMBER SIMULATION     ##
nsim = 2
dateStart = as.Date('2020-03-05')


set.seed(123)
r0postCrI = r0posterior
# hist(r0postCrI)
# summary(r0postCrI)
R0est = sample(x = r0postCrI,size = nsim)
# print(R0est)


epi_doNothingDurInf = vector('list',nsim)
epi_baseDurInf = vector('list',nsim)
epi_marchDurInf = vector('list',nsim)
epi_aprilDurInf = vector('list',nsim)
start = Sys.time()

for(sim in 1:nsim)
{
  epi_doNothingDurInf[[sim]] = simulateOutbreakSEIcIscR(R0t = R0est[sim],
                                                         R0tpostoutbreak = 1.17,
                                                         rho = c(rep(0.4,4),rep(0.8,12)),  #defect value
                                                         dateStart = dateStart,
                                                         dateStartSchoolClosure = dateSSC,
                                                         dateStartIntenseIntervention = dateSII, 
                                                         dateEndIntenseIntervention = dateSII,  #date we begin relaxing intense intervention
                                                         pWorkOpen = c(1, 1, 1, 1),   # pWorkOpen: proportion of the work force that is working (will be time-varying)
                                                         numWeekStagger = c(0,0,0),
                                                         pInfected = initialI,
                                                         durInf = durInfSim)
  
  epi_baseDurInf[[sim]] = simulateOutbreakSEIcIscR(R0t = R0est[sim],
                                                   R0tpostoutbreak = 1.17,
                                                   rho=c(rep(0.4,4),rep(0.8,12)),
                                                   dateStart = dateStart,
                                                   dateStartSchoolClosure = dateSSC,
                                                   dateStartIntenseIntervention = dateSII, 
                                                   dateEndIntenseIntervention = as.Date('2020-04-10'),
                                                   pWorkOpen = c(1, 1, 1, 1),
                                                   numWeekStagger = c(0,0,0),
                                                   pInfected = initialI,
                                                   durInf = durInfSim)
  
  epi_marchDurInf[[sim]] = simulateOutbreakSEIcIscR(R0t = R0est[sim],
                                                    R0tpostoutbreak = 1.17,
                                                    rho=c(rep(0.4,4),rep(0.8,12)),
                                                    dateStart = dateStart,
                                                    dateStartSchoolClosure = dateSSC,
                                                    dateStartIntenseIntervention = dateSII, 
                                                    dateEndIntenseIntervention = as.Date('2020-04-24'),
                                                    pWorkOpen = c(1, 1, 1, 1),
                                                    numWeekStagger = c(0,0,0),
                                                    pInfected = initialI,
                                                    durInf = durInfSim)
  
  epi_aprilDurInf[[sim]] = simulateOutbreakSEIcIscR(R0t =R0est[sim],
                                                    R0tpostoutbreak = 1.17,
                                                    rho = c(rep(0.4,4),rep(0.8,12)),
                                                    dateStart = dateStart,
                                                    dateStartSchoolClosure = dateSSC,
                                                    dateStartIntenseIntervention = dateSII, 
                                                    dateEndIntenseIntervention = as.Date('2020-05-10'),
                                                    pWorkOpen = c(1, 1, 1, 1),
                                                    numWeekStagger = c(0,0,0),
                                                    pInfected = initialI,
                                                    durInf = durInfSim)
  
  if(sim%%10==0) print(paste0('Done with simulation ',sim))
}
end = Sys.time()
print(end-start)

covid_SDurInf3sc = list() 
covid_SDurInf3sc[[1]] = summariseSimulations(VAR = 'S',CI = 50,SIMS = epi_doNothingDurInf)
covid_SDurInf3sc[[2]] = summariseSimulations(VAR = 'S',CI = 50,SIMS = epi_baseDurInf)
covid_SDurInf3sc[[3]] = summariseSimulations(VAR = 'S',CI = 50,SIMS = epi_marchDurInf)
covid_SDurInf3sc[[4]] = summariseSimulations(VAR = 'S',CI = 50,SIMS = epi_aprilDurInf)

covid_IDurInf3sc = list() 
covid_IDurInf3sc[[1]] = summariseSimulations(VAR = 'incidence',CI = 50,SIMS = epi_doNothingDurInf)
covid_IDurInf3sc[[2]] = summariseSimulations(VAR = 'incidence',CI = 50,SIMS = epi_baseDurInf)
covid_IDurInf3sc[[3]] = summariseSimulations(VAR = 'incidence',CI = 50,SIMS = epi_marchDurInf)
covid_IDurInf3sc[[4]] = summariseSimulations(VAR = 'incidence',CI = 50,SIMS = epi_aprilDurInf)

peaktime_DurInf3sc = list()
peaktime_DurInf3sc[[1]] = summarisePeakTimePeakSize(SIMS = epi_doNothingDurInf)
peaktime_DurInf3sc[[2]] = summarisePeakTimePeakSize(SIMS = epi_baseDurInf)
peaktime_DurInf3sc[[3]] = summarisePeakTimePeakSize(SIMS = epi_marchDurInf)
peaktime_DurInf3sc[[4]] = summarisePeakTimePeakSize(SIMS = epi_aprilDurInf)

covid_DurInf3sc = list() 
covid_DurInf3sc[[1]] = summariseSimulations_mid(CI = 50, SIMS = epi_doNothingDurInf)
covid_DurInf3sc[[2]] = summariseSimulations_mid(CI = 50, SIMS = epi_baseDurInf)
covid_DurInf3sc[[3]] = summariseSimulations_mid(CI = 50, SIMS = epi_marchDurInf)
covid_DurInf3sc[[4]] = summariseSimulations_mid(CI = 50, SIMS = epi_aprilDurInf)

AGEcovid_IDurInf3sc = list()
AGEcovid_IDurInf3sc[[1]] = summariseSimulationsAGE(VAR = 'incidence',CI = 50, SIMS = epi_doNothingDurInf)
AGEcovid_IDurInf3sc[[2]] = summariseSimulationsAGE(VAR = 'incidence',CI = 50, SIMS = epi_baseDurInf)
AGEcovid_IDurInf3sc[[3]] = summariseSimulationsAGE(VAR = 'incidence',CI = 50, SIMS = epi_marchDurInf)
AGEcovid_IDurInf3sc[[4]] = summariseSimulationsAGE(VAR = 'incidence',CI = 50, SIMS = epi_aprilDurInf)

epiFirstSimDurInf3sc = list(epi_doNothingDurInf = epi_doNothingDurInf[[1]],
                            epi_baseDurInf= epi_baseDurInf[[1]],
                            epi_marchDurInf = epi_marchDurInf[[1]],
                            epi_aprilDurInf = epi_aprilDurInf[[1]])

save(epiFirstSimDurInf3sc, file ='outputs/SEIcIscR/epiFirstSimDurInf_ARG.rdata')
## 2_simOutbreak_ncov_SEIR_ARG.r
## To simulate n_simSEIR outbreaks >>> ARGENTINA

# CUARENTENA: 05/03/2020

## To simulate n_simSEIR outbreaks: 


##     PARAMETERS     ##
durInfSim = 7                     # duration of infection (days)
initialI = 0.000035               # initial infected  n= ~35 infected
dateSSC = as.Date('2020-03-19')   # date Start School Closure
dateSII = as.Date('2020-03-19')   # date Start Intense Intervention 


##     NuMBER SIMULATION     ##
nsim = 200
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
  epi_doNothingDurInf[[sim]] = simulateOutbreakSEIR(R0t = R0est[sim],
                                                    R0tpostoutbreak = 1.17,
                                                    rho = rep(0.5, 3660),
                                                    dateStart = dateStart,
                                                    dateStartSchoolClosure = dateSSC,
                                                    dateStartIntenseIntervention = dateSII, 
                                                    dateEndIntenseIntervention = dateSII,  #date we begin relaxing intense intervention
                                                    pWorkOpen = c(1, 1, 1, 1),   # pWorkOpen: proportion of the work force that is working (will be time-varying)
                                                    numWeekStagger = c(0,0,0),
                                                    pInfected = initialI,
                                                    durInf = durInfSim)
  
  epi_baseDurInf[[sim]] = simulateOutbreakSEIR(R0t = R0est[sim],
                                               R0tpostoutbreak = 1.17,
                                               rho = rep(0.5, 3660),
                                               dateStart = dateStart,
                                               dateStartSchoolClosure = dateSSC,
                                               dateStartIntenseIntervention = dateSII, 
                                               dateEndIntenseIntervention = as.Date('2020-04-10'),
                                               pWorkOpen = c(1, 1, 1, 1),
                                               numWeekStagger = c(0,0,0),
                                               pInfected = initialI,
                                               durInf = durInfSim)
  
  epi_marchDurInf[[sim]] = simulateOutbreakSEIR(R0t = R0est[sim],
                                                R0tpostoutbreak = 1.17,
                                                rho = rep(0.5, 3660),
                                                dateStart = dateStart,
                                                dateStartSchoolClosure = dateSSC,
                                                dateStartIntenseIntervention = dateSII, 
                                                dateEndIntenseIntervention = as.Date('2020-04-24'),
                                                pWorkOpen = c(1, 1, 1, 1),
                                                numWeekStagger = c(0,0,0),
                                                pInfected = initialI,
                                                durInf = durInfSim)
  
  epi_aprilDurInf[[sim]] = simulateOutbreakSEIR(R0t = R0est[sim],
                                                R0tpostoutbreak = 1.17,
                                                rho = rep(0.5, 3660),
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

covid_SDurInf = list() 
covid_SDurInf[[1]] = summariseSimulations(VAR = 'S',CI = 50,SIMS = epi_doNothingDurInf)
covid_SDurInf[[2]] = summariseSimulations(VAR = 'S',CI = 50,SIMS = epi_baseDurInf)
covid_SDurInf[[3]] = summariseSimulations(VAR = 'S',CI = 50,SIMS = epi_marchDurInf)
covid_SDurInf[[4]] = summariseSimulations(VAR = 'S',CI = 50,SIMS = epi_aprilDurInf)

covid_IDurInf = list() 
covid_IDurInf[[1]] = summariseSimulations(VAR = 'incidence',CI = 50,SIMS = epi_doNothingDurInf)
covid_IDurInf[[2]] = summariseSimulations(VAR = 'incidence',CI = 50,SIMS = epi_baseDurInf)
covid_IDurInf[[3]] = summariseSimulations(VAR = 'incidence',CI = 50,SIMS = epi_marchDurInf)
covid_IDurInf[[4]] = summariseSimulations(VAR = 'incidence',CI = 50,SIMS = epi_aprilDurInf)

peaktime_DurInf = list()
peaktime_DurInf[[1]] = summarisePeakTimePeakSize(SIMS = epi_doNothingDurInf)
peaktime_DurInf[[2]] = summarisePeakTimePeakSize(SIMS = epi_baseDurInf)
peaktime_DurInf[[3]] = summarisePeakTimePeakSize(SIMS = epi_marchDurInf)
peaktime_DurInf[[4]] = summarisePeakTimePeakSize(SIMS = epi_aprilDurInf)

covid_DurInf = list() 
covid_DurInf[[1]] = summariseSimulations_mid(CI = 50,SIMS = epi_doNothingDurInf)
covid_DurInf[[2]] = summariseSimulations_mid(CI = 50,SIMS = epi_baseDurInf)
covid_DurInf[[3]] = summariseSimulations_mid(CI = 50,SIMS = epi_marchDurInf)
covid_DurInf[[4]] = summariseSimulations_mid(CI = 50,SIMS = epi_aprilDurInf)

AGEcovid_IDurInf  = list()
AGEcovid_IDurInf[[1]] = summariseSimulationsAGE(VAR = 'incidence',CI = 50,SIMS = epi_doNothingDurInf)
AGEcovid_IDurInf[[2]] = summariseSimulationsAGE(VAR = 'incidence',CI = 50,SIMS = epi_baseDurInf)
AGEcovid_IDurInf[[3]] = summariseSimulationsAGE(VAR = 'incidence',CI = 50,SIMS = epi_marchDurInf)
AGEcovid_IDurInf[[4]] = summariseSimulationsAGE(VAR = 'incidence',CI = 50,SIMS = epi_aprilDurInf)

epiFirstSimDurInf  = list(epi_doNothingDurInf = epi_doNothingDurInf[[1]],
                                epi_baseDurInf= epi_baseDurInf[[1]],
                                epi_marchDurInf = epi_marchDurInf[[1]],
                                epi_aprilDurInf = epi_aprilDurInf[[1]])


save(covid_SDurInf,file = 'outputs/SEIR/covid_SDurInf_ARG.rdata')
save(covid_IDurInf,file = 'outputs/SEIR/covid_IDurInf_ARG.rdata')
save(peaktime_DurInf,file = 'outputs/SEIR/peaktime_DurInf_ARG.rdata')
save(covid_DurInf,file ='outputs/SEIR/covid_DurInf_ARG.rdata')
save(AGEcovid_IDurInf,file ='outputs/SEIR/AGEcovid_IDurInf_ARG.rdata')
save(epiFirstSimDurInf,file ='outputs/SEIR/epiFirstSimDurInf_ARG.rdata')


#Save plots

png(file = "plots/Ninfected_SEIR.png", width=1000, height=600)
Infected_plot(covid_DurInf, covid_IDurInf, model = "SEIR")
dev.off()



# Ages = 5-10
png(file="plots/IncidenceAge_SEIR_5-10.png", width=600, height=600)
IncidenceAge_plot(epi_doNothingDurInf, epi_baseDurInf, epi_marchDurInf, epi_aprilDurInf, 
                  agegp = 2, model = "SEIR Model")
dev.off()

# Ages = 30-35
png(file="plots/IncidenceAge_SEIR_30-35.png", width=600, height=600)
IncidenceAge_plot(epi_doNothingDurInf, epi_baseDurInf, epi_marchDurInf, epi_aprilDurInf, 
                  agegp = 7, model = "SEIR Model")
dev.off()

# Ages = 45-50
png(file="plots/IncidenceAge_SEIR_45-50.png", width=600, height=600)
IncidenceAge_plot(epi_doNothingDurInf, epi_baseDurInf, epi_marchDurInf, epi_aprilDurInf, 
                  agegp = 10, model = "SEIR Model")
dev.off()

# Ages = 75 -80
png(file="plots/IncidenceAge_SEIR_75-80.png", width=600, height=600)
IncidenceAge_plot(epi_doNothingDurInf, epi_baseDurInf, epi_marchDurInf, epi_aprilDurInf, 
                  agegp = 16, model = "SEIR Model")
dev.off()


rm(epi_doNothingDurInf,epi_baseDurInf, epi_marchDurInf, epi_aprilDurInf)


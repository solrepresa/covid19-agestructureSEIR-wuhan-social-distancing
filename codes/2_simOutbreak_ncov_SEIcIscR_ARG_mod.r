## 2_simOutbreak_ncov_SEIcIscR_ARG.r


set.seed(123)
r0postCrI = r0posterior
R0est = sample(x = r0postCrI,size = nsim)

epi_doNothingDurInf = vector('list',nsim)
epi_baseDurInf = vector('list',nsim)
epi_marchDurInf = vector('list',nsim)
epi_aprilDurInf = vector('list',nsim)
start = Sys.time()

for(sim in 1:nsim){
  epi_doNothingDurInf[[sim]] = simulateOutbreakSEIcIscR_mod(R0t = R0est[sim],
                                                        R0tpostoutbreak = R0tpostoutbreak,
                                                        rho = c(rep(0.4,4),rep(0.8,12)),  #defect value
                                                        dateStart = dateStart,
                                                        dateStartSchoolClosure = dateSSC,
                                                        dateStartIntenseIntervention = dateSII, 
                                                        dateEndIntenseIntervention = dateSII,  #date we begin relaxing intense intervention
                                                        pWorkOpen = pWorkOpen0,   # pWorkOpen: proportion of the work force that is working (will be time-varying)
                                                        pSchoolOpen = pSchoolOpen0,  
                                                        pOtherOpen = pOtherOpen0,
                                                        numWeekStagger = numWeekStagger0,
                                                        pInfected = initialI,
                                                        durInf = durInfSim)
  
  epi_baseDurInf[[sim]] = simulateOutbreakSEIcIscR_mod(R0t = R0est[sim],
                                                   R0tpostoutbreak = 1.17,
                                                   rho = c(rep(0.4,4),rep(0.8,12)),
                                                   dateStart = dateStart,
                                                   dateStartSchoolClosure = dateSSC,
                                                   dateStartIntenseIntervention = dateSII, 
                                                   dateEndIntenseIntervention = dateEII1,
                                                   pWorkOpen = pWorkOpen1,
                                                   pSchoolOpen = pSchoolOpen1,  
                                                   pOtherOpen = pOtherOpen1,
                                                   numWeekStagger = numWeekStagger1,
                                                   pInfected = initialI,
                                                   durInf = durInfSim)
  
  epi_marchDurInf[[sim]] = simulateOutbreakSEIcIscR_mod(R0t = R0est[sim],
                                                    R0tpostoutbreak = 1.17,
                                                    rho=c(rep(0.4,4),rep(0.8,12)),
                                                    dateStart = dateStart,
                                                    dateStartSchoolClosure = dateSSC,
                                                    dateStartIntenseIntervention = dateSII, 
                                                    dateEndIntenseIntervention = dateEII2,
                                                    pWorkOpen = pWorkOpen2,
                                                    pSchoolOpen = pSchoolOpen2,  
                                                    pOtherOpen = pOtherOpen2,
                                                    numWeekStagger = numWeekStagger2,
                                                    pInfected = initialI,
                                                    durInf = durInfSim)
  
  epi_aprilDurInf[[sim]] = simulateOutbreakSEIcIscR_mod(R0t =R0est[sim],
                                                    R0tpostoutbreak = 1.17,
                                                    rho = c(rep(0.4,4),rep(0.8,12)),
                                                    dateStart = dateStart,
                                                    dateStartSchoolClosure = dateSSC,
                                                    dateStartIntenseIntervention = dateSII, 
                                                    dateEndIntenseIntervention = dateEII3,
                                                    pWorkOpen = pWorkOpen3,
                                                    pSchoolOpen = pSchoolOpen3,  
                                                    pOtherOpen = pOtherOpen3,
                                                    numWeekStagger = numWeekStagger3,
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
covid_DurInf[[1]] = summariseSimulations_mid(CI = 50, SIMS = epi_doNothingDurInf)
covid_DurInf[[2]] = summariseSimulations_mid(CI = 50, SIMS = epi_baseDurInf)
covid_DurInf[[3]] = summariseSimulations_mid(CI = 50, SIMS = epi_marchDurInf)
covid_DurInf[[4]] = summariseSimulations_mid(CI = 50, SIMS = epi_aprilDurInf)

AGEcovid_IDurInf = list()
AGEcovid_IDurInf[[1]] = summariseSimulationsAGE(VAR = 'incidence',CI = 50, SIMS = epi_doNothingDurInf)
AGEcovid_IDurInf[[2]] = summariseSimulationsAGE(VAR = 'incidence',CI = 50, SIMS = epi_baseDurInf)
AGEcovid_IDurInf[[3]] = summariseSimulationsAGE(VAR = 'incidence',CI = 50, SIMS = epi_marchDurInf)
AGEcovid_IDurInf[[4]] = summariseSimulationsAGE(VAR = 'incidence',CI = 50, SIMS = epi_aprilDurInf)

epiFirstSimDurInf = list(epi_doNothingDurInf = epi_doNothingDurInf[[1]],
                         epi_baseDurInf= epi_baseDurInf[[1]],
                         epi_marchDurInf = epi_marchDurInf[[1]],
                         epi_aprilDurInf = epi_aprilDurInf[[1]])

save(covid_SDurInf,file = 'outputs/SEIcIscR/covid_SDurInf_ARG.rdata')
save(covid_IDurInf,file = 'outputs/SEIcIscR/covid_IDurInf_ARG.rdata')
save(peaktime_DurInf,file = 'outputs/SEIcIscR/peaktime_DurInf_ARG.rdata')
save(covid_DurInf,file ='outputs/SEIcIscR/covid_DurInf_ARG.rdata')
save(AGEcovid_IDurInf,file ='outputs/SEIcIscR/AGEcovid_IDurInf_ARG.rdata')

save(epiFirstSimDurInf, file ='outputs/SEIcIscR/epiFirstSimDurInf_ARG.rdata')


#Save plots

png(file = "plots/Ninfected_MOD_SEIcIscR.png", width=1000, height=600)
Infected_plot(covid_DurInf, covid_IDurInf, model = "Modelo SEIcIscR")
dev.off()

# Incidence All Ages By Cases
png(file = "plots/IncidenceByCases_MOD_SEIcIscR.png", width=750, height=500)
IncidenceAllAgesByCases_plot(epi_doNothingDurInf, epi_baseDurInf, epi_marchDurInf, epi_aprilDurInf,
                             agegp = c(3,7,10,12), 
                             model = "Modelo SEIcIscR")
dev.off()


png(file = "plots/CumulativeByCases_MOD_SEIcIscR.png", width=750, height=500)
CumulativeAllAgesByCases_plot(epi_doNothingDurInf, epi_baseDurInf, epi_marchDurInf, epi_aprilDurInf,
                              agegp = c(3,7,10,12), 
                              model = "Modelo SEIcIscR")
dev.off()



png(file = "plots/IncidenseWeeks_SEIcIscR.png", width=800, height=600)
IncidenceWeeks_plot(epi_doNothingDurInf, epi_baseDurInf, epi_marchDurInf, epi_aprilDurInf,
                    model = "Modelo SEIcIscR")

dev.off()




png(file = "plots/FrecRelativaIC_SEIcIscR.png", width=800, height=600)
FrecRelativaTotalIc_plot(epi_doNothingDurInf, epi_baseDurInf, epi_marchDurInf, epi_aprilDurInf,
                    model = "Modelo SEIcIscR")

dev.off()


png(file = "plots/FrecRelativaISC_SEIcIscR.png", width=800, height=600)
FrecRelativaTotalIsc_plot(epi_doNothingDurInf, epi_baseDurInf, epi_marchDurInf, epi_aprilDurInf,
                         model = "Modelo SEIcIscR")

dev.off()




## Others plots

# Ages = 5-10
IncidenceAge_plot(epi_doNothingDurInf, epi_baseDurInf, epi_marchDurInf, epi_aprilDurInf, 
                  agegp = 2, model = "SEIcIscR Model")

# Ages = 30-35
IncidenceAge_plot(epi_doNothingDurInf, epi_baseDurInf, epi_marchDurInf, epi_aprilDurInf, 
                  agegp = 7, model = "SEIcIscR Model")

# Ages = 45-50
IncidenceAge_plot(epi_doNothingDurInf, epi_baseDurInf, epi_marchDurInf, epi_aprilDurInf, 
                  agegp = 10, model = "SEIcIscR Model")

# Ages = 75 -80
IncidenceAge_plot(epi_doNothingDurInf, epi_baseDurInf, epi_marchDurInf, epi_aprilDurInf, 
                  agegp = 16, model = "SEIcIscR Model")






rm(epi_doNothingDurInf,epi_baseDurInf,epi_marchDurInf,epi_aprilDurInf)


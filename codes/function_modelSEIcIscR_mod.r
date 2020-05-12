##  SEIcIscR outbreak modified function
# modified functions for argentina's needs
# school anf other matrix intervention

loadInterventions_mod = function(p_workopen, p_schoolopen, p_othersopen)
{
  list(
    # constraints under a DO-NOTHING scenario 
    base = list(home = diag(1,16,16),
               work = diag(1,16,16),
               school = diag(1,16,16),
               others = diag(1,16,16)),
    # Wuhan's lockdown--assume from XX Jan to XX Feb 
    wuhanlockdown = list(home = diag(1,16,16),
                         work = diag(0.1,16,16),
                         school = diag(0,16,16),
                         others = diag(c(rep(0.1,4),rep(0.1,12)))),
    # constraints under school closure + some social distancing for school-age going children but 100% workplace
    schcloseonly = list(home = diag(c(rep(1,4),rep(1,12))),
                        work = diag(1,16,16),
                        school = diag(0,16,16),
                        others = diag(c(rep(0.5,4),rep(1,12)))), 
    # constraints under work place distancing only (MAYBE UNREALISTIC, should close schools too)
    workplacedistonly = list(home = diag(1,16,16),
                             work = diag(0.5,16,16),
                             school = diag(1,16,16),
                             others = diag(0.1,16,16)) ,
    
    # constraints under work place distancing + schoolclosure 
    schcloseworkplacedist = list(home = diag(1,16,16),
                                 work = diag(p_workopen,16,16),
                                 school = diag(p_schoolopen,16,16),
                                 others = diag(p_othersopen, 16, 16)),
    # Post Outbeak, people still cautious 
    postoutbreak = list(home = diag(1,16,16),
                        work = diag(1.0,16,16),
                        school = diag(1.0,16,16),
                        others = diag(c(rep(1.0,4),rep(1.0,12)))))
  
}


# pWorkOpen regula el % de actividad de trabajo en cada periodo
# pSchoolOpen = c(0, 0, 0, 1)
# pOtherOpen = c(0.1, 0.1, 0.1, 0.1)

# dateStartSchoolClosure = as.Date('2020-01-15')


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Children less infectious and as susceptible 
simulateOutbreakSEIcIscR_mod = function(R0t,rho=c(rep(0.4,4),rep(0.8,12)), R0tpostoutbreak = 1.5,
                                        dateEndIntenseIntervention, #date we begin relaxing intense intervention 
                                    pWorkOpen = c(0.1,0.25,0.5,0.9), # pWorkOpen: proportion of the work force that is working (will be time-varying)
                                    pSchoolOpen = c(0, 0, 0, 1),  # pSchoolOpen: proportion of school open
                                    pOtherOpen = c(0.1, 0.1, 0.1, 0.1), # pOtherOpen: proportion of other activities
                                    dateStartSchoolClosure = as.Date('2020-01-15') , # cause winter term break 
                                    dateStartIntenseIntervention = as.Date('2020-01-23') , #Intense intervention: starts at Wuhan Lockdown
                                    dateStart = as.Date('2019-11-01'),POP = localpop, numWeekStagger=c(2,4,6),pInfected=0.0002,durInf = 7,contacts_china=contacts)
{
  # debug dateStartIntenseIntervention = as.Date('2020-01-23')  
  # debug dateEndIntenseIntervention = as.Date('2020-03-01')
  # debug R0est = rep(2,3660) 
  # debug rho = rep(0.8,3660) 
  # debug pWorkOpen =  c(0.1,0.25,0.5,1)
  
  
  # Load population information
  # pop = loadPopInfo(POP = localpop)
  pop = list()
  pop$N = sum(POP$popage)
  pop$p_age = localpop$propage
  N_age = pop$N*pop$p_age                                        # Population age structure (in numbers)
  # contacts_china = CONTACTS
  
  
  # Specify epi info
  durLat = 6.4;   	                                             # Mean latent period (days) from Backer, et al (2020)
  durInf = durInf;                                               # Mean duration of infectiousness (days)
  gamma = 1-exp(-1/durInf);                                      # removal rate
  alpha = 1-exp(-1/durLat);                                      # infection rate
  dt = 1;                                                        # Time step (days)
  tmax = 856 # 428;                                              # Time horizon (days) 366 days in 2020 cause of leap year
  numSteps = tmax/dt;  	                                         # Total number of simulation time steps
  # dateStart = as.Date('2019-12-01')                            # included as a function argument 
  dateEnd = dateStart+(tmax-1)
  dateStartCNY = as.Date('2020-01-25') 
  dateEndCNY = as.Date('2020-01-31') 
  
  # Declare the state variables and related variables:
  # The values of these variables change over time
  S = E = Isc = Ic = R = array(0,c(numSteps,length(pop$p_age)))
  lambda = incidence = subclinical = cumulativeIncidence = array(0,c(numSteps,length(pop$p_age)))
  time = array(0,numSteps)
  
  
  # Initialise the time-dependent variables, i.e. setting the values of the variables at time 0
  E[1,] = 0 
  Ic[1,] =  pInfected*sum(N_age)/16#rpois(length(N_age),lambda = pInfected*sum(N_age)/16)  # 100 # Assign 100 infected person in each age group (TODO RELAX?)
  Isc[1,] = 0 
  R[1,] = 0 
  S[1,] = N_age-E[1,]-Ic[1,]-Isc[1,]-R[1,]
  incidence[1,] = 0;
  subclinical[1,] = 0;
  time[1] = 0;
  
  
  ## INTERVENTIONS 
  # note that intense intervention is time-varying control by pWorkOpen: proportion of the work force that is working
  # debug pWorkOpen = c(0.1,0.25,0.5,1)
  # MODIFICATION: Schools are open in stages 
  
  tStartSchoolClosure = as.vector(dateStartSchoolClosure - dateStart)+1
  
  tStartIntenseIntervention = as.vector(dateStartIntenseIntervention - dateStart)+1 # for pw = 0.1
  tEndIntenseIntervention = as.vector(dateEndIntenseIntervention - dateStart)+1     # for pw = 0.1
  tRelaxIntervention1 = tEndIntenseIntervention + numWeekStagger[1]*7               # for pw = 0.25
  tRelaxIntervention2 = tEndIntenseIntervention + numWeekStagger[2]*7               # for pw = 0.5
  tRelaxIntervention3 = tEndIntenseIntervention + numWeekStagger[3]*7               # for pw = 1
  
  # tStartEndClosure = as.vector(dateEndSchoolClosure - dateStart)+1
  pwork = array(1,numSteps)
  pwork[1:tRelaxIntervention3] =c(rep(1,(tStartIntenseIntervention-0)), # dont know there is outbreak 
                                  rep(pWorkOpen[1],(tEndIntenseIntervention-tStartIntenseIntervention)),
                                  rep(pWorkOpen[2],(tRelaxIntervention1-tEndIntenseIntervention)),
                                  rep(pWorkOpen[3],(tRelaxIntervention2-tRelaxIntervention1)),
                                  rep(pWorkOpen[4],(tRelaxIntervention3-tRelaxIntervention2)))
  
  # tStartIntenseIntervention = tStartSchoolClosure
  
  # School Activity  ##
  pschool = array(1, numSteps)
  pschool[1:tRelaxIntervention3] =c(rep(1,(tStartSchoolClosure-0)), # dont know there is outbreak 
                                  rep(pSchoolOpen[1],(tEndIntenseIntervention-tStartSchoolClosure)),
                                  rep(pSchoolOpen[2],(tRelaxIntervention1-tEndIntenseIntervention)),
                                  rep(pSchoolOpen[3],(tRelaxIntervention2-tRelaxIntervention1)),
                                  rep(pSchoolOpen[4],(tRelaxIntervention3-tRelaxIntervention2)))
  
  
  # Other Activities  ##
  pothers = array(1, numSteps)
  pothers[1:tRelaxIntervention3] =c(rep(1,(tStartIntenseIntervention-0)), # dont know there is outbreak 
                                    rep(pOtherOpen[1],(tEndIntenseIntervention-tStartIntenseIntervention)),
                                    rep(pOtherOpen[2],(tRelaxIntervention1-tEndIntenseIntervention)),
                                    rep(pOtherOpen[3],(tRelaxIntervention2-tRelaxIntervention1)),
                                    rep(pOtherOpen[4],(tRelaxIntervention3-tRelaxIntervention2)))
  
  R0tpostoutbreak = R0t #overwrites the default reduction in R0 post-outbreak
  beta = getbeta(R0t = R0t,constraints = constraintsIntervention$base,gamma = gamma,p_age = pop$p_age)
  if(pWorkOpen[2]<1) beta_postfirstwave = getbeta(R0t = R0tpostoutbreak,constraints = constraintsIntervention$base,gamma = gamma,p_age = pop$p_age)
  if(pWorkOpen[2]>=1) beta_postfirstwave = beta #getbeta(R0t = R0t[2],constraints = constraintsIntervention$base,gamma = gamma,p_age = pop$p_age)
  for (stepIndex in 1: (numSteps-1)){ 
    
    # load plausible intervetions 
    constraintsIntervention = loadInterventions_mod(p_workopen = pwork[stepIndex],
                                                p_schoolopen = pschool[stepIndex],
                                                p_othersopen = pothers[stepIndex])
    
    ## Age- and location-specific contact rates for the given interventions 
    
    # I0: before school winter break intervention period, use base-case
    if(time[stepIndex] < tStartSchoolClosure)  
    {
      CONSTRAINT = constraintsIntervention$base
    }
    # I1:  When school winter break but before lockdown period, use 'schcloseonly'
    if(time[stepIndex] >= tStartSchoolClosure & time[stepIndex] < tStartIntenseIntervention) 
    {
      INTERVENTION = "schcloseonly"   
      CONSTRAINT = constraintsIntervention[[INTERVENTION]] 
    }  
    # I2:  Intense intervention
    if(time[stepIndex] >= tStartIntenseIntervention & time[stepIndex] < tRelaxIntervention3) 
    {
      INTERVENTION = "schcloseworkplacedist"   
      CONSTRAINT = constraintsIntervention[[INTERVENTION]] 
    }  
    # I3: post outbreak 
    if(time[stepIndex] >= tRelaxIntervention3)  
    {
      CONSTRAINT = constraintsIntervention$postoutbreak
    }
    # 
    
    C = CONSTRAINT[[1]]%*%contacts_china[[1]]+
      CONSTRAINT[[2]]%*%contacts_china[[2]]+
      CONSTRAINT[[3]]%*%contacts_china[[3]]+
      CONSTRAINT[[4]]%*%contacts_china[[4]]
    
    # calculate the force of infection
    
    # beta = getbeta(R0t = R0t[stepIndex],constraints = constraintsIntervention$base,gamma = gamma,p_age = pop$p_age)
    if(time[stepIndex] < tEndIntenseIntervention+0) lambda[stepIndex,] = as.numeric(beta)*(as.matrix(C)%*%(as.matrix(Ic[stepIndex,]/N_age) + 0.25*as.matrix(Isc[stepIndex,]/N_age)));
    if(time[stepIndex] >= tEndIntenseIntervention+0)lambda[stepIndex,] = as.numeric(beta_postfirstwave)*(as.matrix(C)%*%(as.matrix(Ic[stepIndex,]/N_age) + 0.25*as.matrix(Isc[stepIndex,]/N_age)));
    # calculate the number of infections and recoveries between time t and t+dt
    
    numStoE   = lambda[stepIndex,]*S[stepIndex,]*dt;                  # S to E
    numEtoIc  = alpha*rho*E[stepIndex,]*dt;                           # E to Ic
    numEtoIsc = alpha*(1-rho)*E[stepIndex,]*dt;                       # E to Isc
    numIctoR  = gamma*Ic[stepIndex,]*dt;                              # Ic to R
    numIsctoR = gamma*Isc[stepIndex,]*dt;                             # Isc to R
    
    # Difference equations 
    S[stepIndex+1,]   = S[stepIndex,]-numStoE;
    E[stepIndex+1,]   = E[stepIndex,]+numStoE-numEtoIc-numEtoIsc;
    Ic[stepIndex+1,]  = Ic[stepIndex,]+numEtoIc-numIctoR;
    Isc[stepIndex+1,] = Isc[stepIndex,]+numEtoIsc-numIsctoR;
    R[stepIndex+1,]   = R[stepIndex,]+numIctoR+numIsctoR;
    
    incidence[stepIndex+1,] = numEtoIc/dt;
    subclinical[stepIndex+1,] = numEtoIsc/dt;
    time[stepIndex+1] = time[stepIndex]+dt;
    
  }
  
  output = list(S = S, E = E, Ic = Ic, Isc = Isc, R = R, time = time, lambda=lambda,
                incidence = incidence, N_age= N_age, subclinical = subclinical, 
                R0t = R0t,#rho = rho,
                dateStart = dateStart, dateEnd = dateEnd,
                dateStartIntenseIntervention = dateStartIntenseIntervention, dateEndIntenseIntervention = dateEndIntenseIntervention,
                dateStartSchoolClosure = dateStartSchoolClosure, dateStartCNY = dateStartCNY,dateEndCNY = dateEndCNY)
  return(output)
}




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

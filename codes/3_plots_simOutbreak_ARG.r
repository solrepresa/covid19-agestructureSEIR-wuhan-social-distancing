# 3_plots_simOutbreak_ARG.r

### MODEL PLOTS 


# Quick checks: Simulate an outbreak for sanity checks
set.seed(666)


# test for an R0 value of 2.2
R0est = 2.2

# R0est = sample(x = r0posterior, size = 100)

nsim = 1
epi_doNothing = vector('list',nsim)
epi_base = vector('list',nsim)
epi_march = vector('list',nsim)
epi_april = vector('list',nsim)

# epiFirstSimDurInf is a .Rdata

IncidenceAge_plot <- function(epi_doNothing, epi_base, epi_march, epi_april, 
                             model,
                             agegp = 3,
                             legends = c("Sin hacer nada", "Caso 1", "Caso 2", "Caso 3")){
  
  par(mfrow = c(2,1))
  
  # incidence over time
  plot(epi_doNothing[[1]]$time, 
       epi_doNothing[[1]]$incidence[,agegp], 
       type='l', 
       lwd=2,
       main = paste0("Incidence for age [",(agegp-1)*5,',',agegp*5,')'),
       xlab = "Time(days)", 
       ylab = "Daily no. of infections")
  lines(x = epi_base[[1]]$time, y = epi_base[[1]]$incidence[,agegp],lwd=2,col='grey40')
  lines(x = epi_march[[1]]$time, y = epi_march[[1]]$incidence[,agegp],lwd=2,col='steelblue')
  lines(x = epi_april[[1]]$time, y = epi_april[[1]]$incidence[,agegp],lwd=2,col='tomato',lty='dashed')
  
  
  # cumulative incidence over time
  plot(epi_doNothing[[1]]$time, 
       (epi_doNothing[[1]]$N_age[agegp]-epi_doNothing[[1]]$S[,agegp])/epi_doNothing[[1]]$N_age[agegp], 
       lwd=2,
       type='l', 
       main=paste0("Cum incidence for age [",(agegp-1)*5,',',agegp*5,')'),
       xlab="Time(days)", 
       ylab="Cum incidence",
       ylim = c(0,1));
  lines(epi_base[[1]]$time, (epi_base[[1]]$N_age[agegp]-epi_base[[1]]$S[,agegp])/epi_base[[1]]$N_age[agegp],lwd=2,col='grey40')
  lines(epi_march[[1]]$time, (epi_march[[1]]$N_age[agegp]-epi_march[[1]]$S[,agegp])/epi_march[[1]]$N_age[agegp],lwd=2,col='steelblue')
  lines(epi_april[[1]]$time, (epi_april[[1]]$N_age[agegp]-epi_april[[1]]$S[,agegp])/epi_april[[1]]$N_age[agegp],lwd=2,col='tomato',lty='dashed')
  legend(0.25, 0.98, legend = legends,
         col = c("black", "grey40","steelblue",'tomato'), 
         bty='n',
         lty= rep(1,4),
         lwd= rep(2,4), 
         cex= 0.8,
         y.intersp = 0.15)
  
}



# epiFirstSimDurInf.rData
IncidenceAge_plot(epi_doNothingDurInf, epi_baseDurInf, epi_marchDurInf, epi_aprilDurInf, 
                 agegp = 3,
                 model = "SEIR Model")



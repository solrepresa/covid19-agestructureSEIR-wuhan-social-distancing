# function_plots_simOutbreak.r

### MODEL PLOTS 


# Quick checks: Simulate an outbreak for sanity checks
set.seed(666)



IncidenceAge_plot <- function(epi_doNothing, epi_base, epi_march, epi_april, 
                             model,
                             agegp = 3, # age group
                             legends = c("Sin intervencion", "Caso 1", "Caso 2", "Caso 3")){
  
  par(mfrow = c(2,1), oma=c(1,1,2,6))
  
  # incidence over time
  plot(epi_doNothing[[1]]$time/7, 
       epi_doNothing[[1]]$incidence[,agegp], 
       type='l', 
       lwd=2,
       main = paste0("Incidence for age [",(agegp-1)*5,',',agegp*5,')'),
       cex= 0.8,
       xlab = "Time(weeks)", 
       ylab = "Daily no. of infections")
  lines(x = epi_base[[1]]$time/7, y = epi_base[[1]]$incidence[,agegp],lwd=2,col='grey40')
  lines(x = epi_march[[1]]$time/7, y = epi_march[[1]]$incidence[,agegp],lwd=2,col='steelblue')
  lines(x = epi_april[[1]]$time/7, y = epi_april[[1]]$incidence[,agegp],lwd=2,col='tomato')
  
  
  # cumulative incidence over time
  plot(epi_doNothing[[1]]$time/7, 
       (epi_doNothing[[1]]$N_age[agegp]-epi_doNothing[[1]]$S[,agegp])/epi_doNothing[[1]]$N_age[agegp], 
       lwd=2,
       type='l', 
       main=paste0("Cum incidence for age [",(agegp-1)*5,',',agegp*5,')'),
       cex= 0.8,
       xlab="Time(weeks)", 
       ylab="Cum incidence",
       ylim = c(0,1));
  lines(epi_base[[1]]$time/7, (epi_base[[1]]$N_age[agegp]-epi_base[[1]]$S[,agegp])/epi_base[[1]]$N_age[agegp],lwd=2,col='grey40')
  lines(epi_march[[1]]$time/7, (epi_march[[1]]$N_age[agegp]-epi_march[[1]]$S[,agegp])/epi_march[[1]]$N_age[agegp],lwd=2,col='steelblue')
  lines(epi_april[[1]]$time/7, (epi_april[[1]]$N_age[agegp]-epi_april[[1]]$S[,agegp])/epi_april[[1]]$N_age[agegp],lwd=2,col='tomato')
  legend(par('usr')[2], par('usr')[4], legend = legends,
         col = c("black", "grey40","steelblue",'tomato'), 
         bty='n',
         lty= rep(1,4),
         lwd= rep(2,4), 
         #y.intersp = 0.15,
         cex= 0.7,
         xpd = NA)
  mtext(model, line=0, side=3, outer=TRUE, cex=1.5)
  
  
}


# incidence by age over time for all ages

IncidenceAllAges_plot <- function(epi_doNothing, epi_base, epi_march, epi_april, 
                              model,
                              ages = c(3,7,10,12), # age group
                              legends = c("Sin intervencion", "Caso 1", "Caso 2", "Caso 3")){
  
  par(mfrow = c(length(ages),1), oma=c(1,1,2,5))
  
  for(i in 1:length(ages)){
    agegp = ages[i]
    plot(epi_doNothing[[1]]$time/7,       
         epi_doNothing[[1]]$incidence[,agegp], 
         type='l', 
         lwd=2,
         main = paste0("Incidence for age [",(agegp-1)*5,',',agegp*5,')'),
         xlab = "Time(weeks)", 
         ylab = "Daily no. of infections")
    lines(x = epi_base[[1]]$time/7, y = epi_base[[1]]$incidence[,agegp],lwd=2,col='grey40')
    lines(x = epi_march[[1]]$time/7, y = epi_march[[1]]$incidence[,agegp],lwd=2,col='steelblue')
    lines(x = epi_april[[1]]$time/7, y = epi_april[[1]]$incidence[,agegp],lwd=2,col='tomato',lty='dashed')
    
  }
  legend(par('usr')[2], par('usr')[4], legend = legends,
         col = c("black", "grey40","steelblue",'tomato'), 
         bty='n',
         lty= rep(1,4),
         lwd= rep(2,4), 
         #y.intersp = 0.15,
         cex= 0.7,
         xpd = NA)
  mtext(model, line=0, side=3, outer=TRUE, cex=1.5)
}






#Grafica de Infectados para los cuatro casos
Infected_plot <- function(covid_DurInf, covid_IDurInf, 
                              legends = c("Sin intervencion", "Caso 1", "Caso 2", "Caso 3"),
                              model){
  l = c("Mediana", "Quartil Inferior",  "Quartil Superior")
  
  par(par(no.readonly=TRUE))
  par(mfrow=c(2,2), oma=c(4,2,2,8), mar=c(3,3,2,0), pch=16)
  
  for( i in 1:4){
    plot(covid_DurInf[[4]][["S"]][["time"]]/7, 
         covid_IDurInf[[i]][["summary"]][["uci"]],
         type="l", 
         col='tomato',
         main= legends[i],
         xlab="",
         ylab="", lty =2)
    lines(covid_DurInf[[4]][["S"]][["time"]]/7, 
          covid_IDurInf[[i]][["summary"]][["lci"]], 
          col="steelblue", lty =2)
    lines(covid_DurInf[[4]][["S"]][["time"]]/7, 
          covid_IDurInf[[i]][["summary"]][["median"]], 
          col="black")
  }
  
  legend(par('usr')[2], par('usr')[4], legend= l,
         col=c("black", "steelblue", "tomato"), 
         lty= c(1,2,2),
         bty='n',
         cex = 0.8,
         xpd = NA)
  mtext(text = "Semanas desde el caso nro. 20", side=1,line=0,outer=TRUE)
  mtext(text = "Infectados c/24hs", side=2,line=0,outer=TRUE)
  mtext(paste0(model,""), line=0, side=3, outer=TRUE, cex=1.5)
  mtext("Dia final de las intervenciones", side=1,line=2, outer=TRUE, adj = 1, cex = 0.9)
  mtext(text = paste("Caso 1:", covid_IDurInf[[1]][["Sim1"]][["dateEndIntenseIntervention"]],
                     "- Caso 2:", covid_IDurInf[[2]][["Sim1"]][["dateEndIntenseIntervention"]],
                     "- Caso 3:", covid_IDurInf[[3]][["Sim1"]][["dateEndIntenseIntervention"]],
                     "- Caso 4:", covid_IDurInf[[4]][["Sim1"]][["dateEndIntenseIntervention"]]), 
        side=1,line=3, outer=TRUE, adj = 1,cex = 0.8)
  
}




# incidence by age over time for all ages separated by cases

IncidenceAllAgesByCases_plot <- function(epi_doNothing, epi_base, epi_march, epi_april, 
                                         model,
                                         agegp = c(3,7,10,12), # age group
                                         legends = c("Sin intervencion", "Caso 1", "Caso 2", "Caso 3")){
  
  par(par(no.readonly=TRUE))
  par(mfrow=c(2,2), oma=c(2,2,2,8), mar=c(3,3,2,0), pch=16)
  
  # Caso 1
  plot(epi_doNothing[[1]]$time/7,       
       epi_doNothing[[1]]$incidence[,agegp[2]], 
       type='l', 
       lwd=2,
       main = legends[1],
       xlab = "", 
       ylab = "")
  lines(x = epi_doNothing[[1]]$time/7, y = epi_doNothing[[1]]$incidence[,agegp[1]],lwd=2,col='grey40')
  lines(x = epi_doNothing[[1]]$time/7, y = epi_doNothing[[1]]$incidence[,agegp[3]],lwd=2,col='steelblue')
  lines(x = epi_doNothing[[1]]$time/7, y = epi_doNothing[[1]]$incidence[,agegp[4]],lwd=2,col='tomato')
  
  # Caso 2
  plot(epi_base[[1]]$time/7,       
       epi_base[[1]]$incidence[,agegp[2]], 
       type='l', 
       lwd=2,
       main = legends[2],
       xlab = "", 
       ylab = "")
  lines(x = epi_base[[1]]$time/7, y = epi_base[[1]]$incidence[,agegp[1]],lwd=2,col='grey40')
  lines(x = epi_base[[1]]$time/7, y = epi_base[[1]]$incidence[,agegp[3]],lwd=2,col='steelblue')
  lines(x = epi_base[[1]]$time/7, y = epi_base[[1]]$incidence[,agegp[4]],lwd=2,col='tomato')
  
  # Caso 3
  plot(epi_march[[1]]$time/7,       
       epi_march[[1]]$incidence[,agegp[2]], 
       type='l', 
       lwd=2,
       main = legends[3],
       xlab = "", 
       ylab = "")
  lines(x = epi_march[[1]]$time/7, y = epi_march[[1]]$incidence[,agegp[1]],lwd=2,col='grey40')
  lines(x = epi_march[[1]]$time/7, y = epi_march[[1]]$incidence[,agegp[3]],lwd=2,col='steelblue')
  lines(x = epi_march[[1]]$time/7, y = epi_march[[1]]$incidence[,agegp[4]],lwd=2,col='tomato')
  
  # Caso 4
  plot(epi_april[[1]]$time/7,       
       epi_april[[1]]$incidence[,agegp[2]], 
       type='l', 
       lwd=2,
       main = legends[4],
       xlab = "", 
       ylab = "")
  lines(x = epi_april[[1]]$time/7, y = epi_april[[1]]$incidence[,agegp[1]],lwd=2,col='grey40')
  lines(x = epi_april[[1]]$time/7, y = epi_april[[1]]$incidence[,agegp[3]],lwd=2,col='steelblue')
  lines(x = epi_april[[1]]$time/7, y = epi_april[[1]]$incidence[,agegp[4]],lwd=2,col='tomato')
  legend(par('usr')[2], par('usr')[4], 
         legend = c(paste0("[", (agegp[1]-1)*5,',',agegp[1]*5,')'), paste0("[",(agegp[2]-1)*5,',',agegp[2]*5,')'),
                    paste0("[",(agegp[3]-1)*5,',',agegp[3]*5,')'), paste0("[",(agegp[4]-1)*5,',',agegp[4]*5,')')),
         col = c("grey40","black", "steelblue",'tomato'), 
         bty='n',
         lty= rep(1,4),
         lwd= rep(2,4), 
         title = "Incidencia por edad",
         #y.intersp = 0.15,
         cex= 0.9,
         xpd = NA)
  mtext(text = "Semanas desde el caso nro. 20", side=1,line=0,outer=TRUE)
  mtext(text = "Infectados c/24hs", side=2,line=0,outer=TRUE)
  mtext(model, line=0, side=3, outer=TRUE, cex=1.5)
}



# Cumulative incidence by age over time for all ages separated by cases

CumulativeAllAgesByCases_plot <- function(epi_doNothing, epi_base, epi_march, epi_april, 
                                         model,
                                         agegp = c(3,7,10,12), # age group
                                         legends = c("Sin intervencion", "Caso 1", "Caso 2", "Caso 3")){
  
  par(par(no.readonly=TRUE))
  par(mfrow=c(2,2), oma=c(2,2,2,8), mar=c(3,3,2,0), pch=16)
  
  # Caso 1
  plot(epi_doNothing[[1]]$time/7,       
       (epi_doNothing[[1]]$N_age[agegp[2]]-epi_doNothing[[1]]$S[,agegp[2]])/epi_doNothing[[2]]$N_age[agegp[2]],
       type='l', 
       lwd=2,
       main = legends[1],
       xlab = "", 
       ylab = "")
  lines(epi_doNothing[[1]]$time/7, (epi_doNothing[[1]]$N_age[agegp[1]]-epi_doNothing[[1]]$S[,agegp[1]])/epi_doNothing[[1]]$N_age[agegp[1]],lwd=2,col='grey40')
  lines(x = epi_doNothing[[1]]$time/7, (epi_doNothing[[1]]$N_age[agegp[3]]-epi_doNothing[[1]]$S[,agegp[3]])/epi_doNothing[[1]]$N_age[agegp[3]],lwd=2,col='steelblue')
  lines(x = epi_doNothing[[1]]$time/7, (epi_doNothing[[1]]$N_age[agegp[4]]-epi_doNothing[[1]]$S[,agegp[4]])/epi_doNothing[[1]]$N_age[agegp[4]],lwd=2,col='tomato')

  # Caso 2
  plot(epi_base[[1]]$time/7,       
       (epi_base[[1]]$N_age[agegp[2]]-epi_base[[1]]$S[,agegp[2]])/epi_base[[2]]$N_age[agegp[2]],
       type='l', 
       lwd=2,
       main = legends[2],
       xlab = "", 
       ylab = "")
  lines(epi_base[[1]]$time/7, (epi_base[[1]]$N_age[agegp[1]]-epi_base[[1]]$S[,agegp[1]])/epi_base[[1]]$N_age[agegp[1]],lwd=2,col='grey40')
  lines(x = epi_base[[1]]$time/7, (epi_base[[1]]$N_age[agegp[3]]-epi_base[[1]]$S[,agegp[3]])/epi_base[[1]]$N_age[agegp[3]],lwd=2,col='steelblue')
  lines(x = epi_base[[1]]$time/7, (epi_base[[1]]$N_age[agegp[4]]-epi_base[[1]]$S[,agegp[4]])/epi_base[[1]]$N_age[agegp[4]],lwd=2,col='tomato')

  # Caso 3
  plot(epi_march[[1]]$time/7,       
       (epi_march[[1]]$N_age[agegp[2]]-epi_march[[1]]$S[,agegp[2]])/epi_march[[2]]$N_age[agegp[2]],
       type='l', 
       lwd=2,
       main = legends[3],
       xlab = "", 
       ylab = "")
  lines(epi_march[[1]]$time/7, (epi_march[[1]]$N_age[agegp[1]]-epi_march[[1]]$S[,agegp[1]])/epi_march[[1]]$N_age[agegp[1]],lwd=2,col='grey40')
  lines(epi_march[[1]]$time/7, (epi_march[[1]]$N_age[agegp[3]]-epi_march[[1]]$S[,agegp[3]])/epi_march[[1]]$N_age[agegp[3]],lwd=2,col='steelblue')
  lines(epi_march[[1]]$time/7, (epi_march[[1]]$N_age[agegp[4]]-epi_march[[1]]$S[,agegp[4]])/epi_march[[1]]$N_age[agegp[4]],lwd=2,col='tomato')

  # Caso 4
  plot(epi_april[[1]]$time/7,       
       (epi_april[[1]]$N_age[agegp[2]]-epi_april[[1]]$S[,agegp[2]])/epi_april[[2]]$N_age[agegp[2]],
       type='l', 
       lwd=2,
       main = legends[4],
       xlab = "", 
       ylab = "")
  lines(epi_april[[1]]$time/7, (epi_april[[1]]$N_age[agegp[1]]-epi_april[[1]]$S[,agegp[1]])/epi_april[[1]]$N_age[agegp[1]],lwd=2,col='grey40')
  lines(epi_april[[1]]$time/7, (epi_april[[1]]$N_age[agegp[3]]-epi_april[[1]]$S[,agegp[3]])/epi_april[[1]]$N_age[agegp[3]],lwd=2,col='steelblue')
  lines(epi_april[[1]]$time/7, (epi_april[[1]]$N_age[agegp[4]]-epi_april[[1]]$S[,agegp[4]])/epi_april[[1]]$N_age[agegp[4]],lwd=2,col='tomato')
  legend(par('usr')[2], par('usr')[4], 
         legend = c(paste0("[", (agegp[1]-1)*5,',',agegp[1]*5,')'), paste0("[",(agegp[2]-1)*5,',',agegp[2]*5,')'),
                    paste0("[",(agegp[3]-1)*5,',',agegp[3]*5,')'), paste0("[",(agegp[4]-1)*5,',',agegp[4]*5,')')),
         col = c("grey40", "black","steelblue",'tomato'), 
         bty='n',
         lty= rep(1,4),
         lwd= rep(2,4), 
         title = "Incidencia por edad",
         #y.intersp = 0.15,
         cex= 0.9,
         xpd = NA)
  mtext(text = "Semanas desde el caso nro. 20", side=1,line=0,outer=TRUE)
  mtext(text = "Infectados c/24hs", side=2,line=0,outer=TRUE)
  mtext(model, line=0, side=3, outer=TRUE, cex=1.5)
}



IncidenceWeeks_plot <- function(epi_doNothing, epi_base, epi_march, epi_april, 
                                model,
                                timesp = as.numeric(Sys.Date() - as.Date('2020-03-19')),  #week
                                legends = c("Sin intervencion", "Caso 1", "Caso 2", "Caso 3")){
  
  names_arg = c("[0,5)", "[5,10)", "[10,15)", "[15,20)", "[20,25)", 
                "[25,30)", "[30,35)", "[35,40)", "[40,45)", "[45,50)", 
                "[50,55)", "[55,60)", "[60,65)", "[65,70)", "[70,75)", "[75,80)")
  
  max_plot <- max(c(epi_doNothing[[1]]$incidence[timesp,], epi_base[[1]]$incidence[timesp,], 
                    epi_march[[1]]$incidence[timesp,], epi_april[[1]]$incidence[timesp,]))
  par(par(no.readonly=TRUE))
  par(mfrow=c(2,2), oma=c(2,2,4,2), mar=c(3,3,2,0) + 0.5)
  
  
  # Caso 1
  barplot(epi_doNothing[[1]]$incidence[timesp,], 
          names.arg = names_arg,
          main = legends[1],
          xlab = "", 
          ylab = "",
          xlim = c(0, max_plot),
          las = 1,
          horiz = TRUE)
  
  # Caso 2
  barplot(epi_base[[1]]$incidence[timesp,], 
          names.arg = names_arg,
          main = legends[2],
          xlab = "", 
          ylab = "",
          xlim = c(0, max_plot),
          las = 1,
          horiz = TRUE)
  
  # Caso 3
  barplot(epi_march[[1]]$incidence[timesp,], 
          names.arg = names_arg,
          main = legends[3],
          xlab = "", 
          ylab = "",
          xlim = c(0, max_plot),
          las = 1,
          horiz = TRUE)
  
  # Caso 4
  barplot(epi_april[[1]]$incidence[timesp,], 
          names.arg = names_arg,
          main = legends[4],
          xlab = "", 
          ylab = "",
          xlim = c(0, max_plot),
          las = 1,
          horiz = TRUE)
  
  mtext(text = "Infectados nuevos c/24hs", side=1,line=0,outer=TRUE)
  mtext(model, line=2, side=3, outer=TRUE, cex=1.5)
  mtext(paste("Semanas ", round(timesp/7, digits = 1) ), line=1, side=3, outer=TRUE, cex=1)
}




### GRAFICA frecuencia relativa por Edad a un cierto t


FrecRelativaTotalIc_plot <- function(epi_doNothing, epi_base, epi_march, epi_april, 
                           model,
                           timesp = as.numeric(Sys.Date() - as.Date('2020-03-19')),  #week
                           legends = c("Sin intervencion", "Caso 1", "Caso 2", "Caso 3")){
  
  names_arg = c("[0,5)", "[5,10)", "[10,15)", "[15,20)", "[20,25)", 
                "[25,30)", "[30,35)", "[35,40)", "[40,45)", "[45,50)", 
                "[50,55)", "[55,60)", "[60,65)", "[65,70)", "[70,75)", "[75,80)")
  
  max_plot <- max(c(epi_doNothing[[1]]$Ic[timesp,]/sum(epi_doNothing[[1]]$Ic[timesp,]), 
                    epi_base[[1]]$Ic[timesp,]/sum(epi_base[[1]]$Ic[timesp,]), 
                    epi_march[[1]]$Ic[timesp,]/sum(epi_march[[1]]$Ic[timesp,]),
                    epi_april[[1]]$Ic[timesp,]/sum(epi_april[[1]]$Ic[timesp,])))
  
  par(par(no.readonly=TRUE))
  par(mfrow=c(2,2), oma=c(2,2,4,2), mar=c(3,3,2,0) + 0.5)
  
  
  # Caso 1
  barplot(epi_doNothing[[1]]$Ic[timesp,]/sum(epi_doNothing[[1]]$Ic[timesp,]), 
               names.arg = names_arg,
       main = legends[1],
       xlab = "", 
       ylab = "",
       xlim = c(0, max_plot),
       las = 1,
       horiz = TRUE)

  # Caso 2
  barplot(epi_base[[1]]$Ic[timesp,]/sum(epi_base[[1]]$Ic[timesp,]), 
          names.arg = names_arg,
       main = legends[2],
       xlab = "", 
       ylab = "",
       xlim = c(0, max_plot),
       las = 1,
       horiz = TRUE)
  
  # Caso 3
  barplot(epi_march[[1]]$Ic[timesp,]/sum(epi_march[[1]]$Ic[timesp,]), 
          names.arg = names_arg,
          main = legends[3],
          xlab = "", 
          ylab = "",
          xlim = c(0, max_plot),
          las = 1,
          horiz = TRUE)
  
  # Caso 4
  barplot(epi_april[[1]]$Ic[timesp,]/sum(epi_april[[1]]$Ic[timesp,]), 
          names.arg = names_arg,
          main = legends[4],
          xlab = "", 
          ylab = "",
          xlim = c(0, max_plot),
          las = 1,
          horiz = TRUE)
  
  mtext(text = "Frecuencia Relativa Infectados Clínicos", side=1,line=0,outer=TRUE)
  mtext(model, line=2, side=3, outer=TRUE, cex=1.5)
  mtext(paste("Semanas ", round(timesp/7, digits = 1) ), line=1, side=3, outer=TRUE, cex=1)
}


FrecRelativaTotalIsc_plot <- function(epi_doNothing, epi_base, epi_march, epi_april, 
                                     model,
                                     timesp = as.numeric(Sys.Date() - as.Date('2020-03-19')),  #week
                                     legends = c("Sin intervencion", "Caso 1", "Caso 2", "Caso 3")){
  
  names_arg = c("[0,5)", "[5,10)", "[10,15)", "[15,20)", "[20,25)", 
                "[25,30)", "[30,35)", "[35,40)", "[40,45)", "[45,50)", 
                "[50,55)", "[55,60)", "[60,65)", "[65,70)", "[70,75)", "[75,80)")
  
  max_plot <- max(c(epi_doNothing[[1]]$Isc[timesp,]/sum(epi_doNothing[[1]]$Isc[timesp,]), 
                    epi_base[[1]]$Isc[timesp,]/sum(epi_base[[1]]$Isc[timesp,]), 
                    epi_march[[1]]$Isc[timesp,]/sum(epi_march[[1]]$Isc[timesp,]),
                    epi_april[[1]]$Isc[timesp,]/sum(epi_april[[1]]$Isc[timesp,])))
  
  par(par(no.readonly=TRUE))
  par(mfrow=c(2,2), oma=c(2,2,4,2), mar=c(3,3,2,0) + 0.5)
  
  
  # Caso 1
  barplot(epi_doNothing[[1]]$Isc[timesp,]/sum(epi_doNothing[[1]]$Isc[timesp,]), 
          names.arg = names_arg,
          main = legends[1],
          xlab = "", 
          ylab = "",
          xlim = c(0, max_plot),
          las = 1,
          horiz = TRUE)
  
  # Caso 2
  barplot(epi_base[[1]]$Isc[timesp,]/sum(epi_base[[1]]$Isc[timesp,]), 
          names.arg = names_arg,
          main = legends[2],
          xlab = "", 
          ylab = "",
          xlim = c(0, max_plot),
          las = 1,
          horiz = TRUE)
  
  # Caso 3
  barplot(epi_march[[1]]$Isc[timesp,]/sum(epi_march[[1]]$Isc[timesp,]), 
          names.arg = names_arg,
          main = legends[3],
          xlab = "", 
          ylab = "",
          xlim = c(0, max_plot),
          las = 1,
          horiz = TRUE)
  
  # Caso 4
  barplot(epi_april[[1]]$Isc[timesp,]/sum(epi_april[[1]]$Isc[timesp,]), 
          names.arg = names_arg,
          main = legends[4],
          xlab = "", 
          ylab = "",
          xlim = c(0, max_plot),
          las = 1,
          horiz = TRUE)
  
  mtext(text = "Frecuencia Relativa Infectados Subclínicos", side=1,line=0,outer=TRUE)
  mtext(model, line=2, side=3, outer=TRUE, cex=1.5)
  mtext(paste("Semanas ", round(timesp/7, digits = 1) ), line=1, side=3, outer=TRUE, cex=1)
}


ProporcionIc_plot <- function(epi_doNothing, epi_base, epi_march, epi_april, 
                                      model,
                                      timesp = as.numeric(Sys.Date() - as.Date('2020-03-19')),  #week
                                      legends = c("Sin intervencion", "Caso 1", "Caso 2", "Caso 3")){
  
  names_arg = c("[0,5)", "[5,10)", "[10,15)", "[15,20)", "[20,25)", 
                "[25,30)", "[30,35)", "[35,40)", "[40,45)", "[45,50)", 
                "[50,55)", "[55,60)", "[60,65)", "[65,70)", "[70,75)", "[75,80)")
  
  max_plot <- max(c(epi_doNothing[[1]]$Ic[timesp,]/ (epi_doNothing[[1]]$Isc[timesp,] + epi_doNothing[[1]]$Ic[timesp,]), 
                    epi_base[[1]]$Ic[timesp,]/ (epi_base[[1]]$Isc[timesp,] + epi_base[[1]]$Ic[timesp,]), 
                    epi_march[[1]]$Ic[timesp,]/(epi_march[[1]]$Isc[timesp,] + epi_march[[1]]$Ic[timesp,]),
                    epi_april[[1]]$Ic[timesp,]/ (epi_april[[1]]$Isc[timesp,] + epi_april[[1]]$Ic[timesp,])))
  
  par(par(no.readonly=TRUE))
  par(mfrow=c(2,2), oma=c(2,2,4,2), mar=c(3,3,2,0) + 0.5)
  
  
  # Caso 1
  barplot(epi_doNothing[[1]]$Ic[timesp,]/ (epi_doNothing[[1]]$Isc[timesp,] + epi_doNothing[[1]]$Ic[timesp,]), 
          names.arg = names_arg,
          main = legends[1],
          xlab = "", 
          ylab = "",
          xlim = c(0, max_plot),
          las = 1,
          horiz = TRUE)
  
  # Caso 2
  barplot(epi_base[[1]]$Ic[timesp,]/ (epi_base[[1]]$Isc[timesp,] + epi_base[[1]]$Ic[timesp,]), 
          names.arg = names_arg,
          main = legends[2],
          xlab = "", 
          ylab = "",
          xlim = c(0, max_plot),
          las = 1,
          horiz = TRUE)
  
  # Caso 3
  barplot(epi_march[[1]]$Ic[timesp,]/(epi_march[[1]]$Isc[timesp,] + epi_march[[1]]$Ic[timesp,]), 
          names.arg = names_arg,
          main = legends[3],
          xlab = "", 
          ylab = "",
          xlim = c(0, max_plot),
          las = 1,
          horiz = TRUE)
  
  # Caso 4
  barplot(epi_april[[1]]$Ic[timesp,]/ (epi_april[[1]]$Isc[timesp,] + epi_april[[1]]$Ic[timesp,]), 
          names.arg = names_arg,
          main = legends[4],
          xlab = "", 
          ylab = "",
          xlim = c(0, max_plot),
          las = 1,
          horiz = TRUE)
  
  mtext(text = "Proporción Ic / Ic+Isc", side=1,line=0,outer=TRUE)
  mtext(model, line=2, side=3, outer=TRUE, cex=1.5)
  mtext(paste("Semanas ", round(timesp/7, digits = 1) ), line=1, side=3, outer=TRUE, cex=1)
}
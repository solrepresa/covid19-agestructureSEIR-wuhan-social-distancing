# 1_loadData_Arg.r

require(readxl)

# Argentina Data

loadPopData = FALSE  # cual es la base de datos?
loadContactMatrices = TRUE # estan normalizadas? hace falta normalizarlas?
loadCaseData = TRUE
loadR0posterior = TRUE



# 1) Argentina population data 
if(loadPopData) 
{ 
  # ATENTI: wuhanpop.csv la utiliza para normalizar la matriz de contacto y la llama varias veces
}



# 2) Argentina Contact Matrix  >>> LISTO
if(loadContactMatrices)
{
  contacts <- contacts_arg # normalize.contact.matrices(contacts_arg,wuhanpop$popage, make.sym=T)
  rm(contacts_arg)
}


# 3) Case age distribution  >> LISTO
if(loadCaseData)
{
  localCaseraw = ageDistribution
  caseage = rep(localCaseraw[[site]], each = 2)/2
  localCase = data.frame(agegroup = 1:16, caseage = c(caseage[1:15],sum(caseage[16:20])))
  rm(localCaseraw, caseage)
}


# 4) R0 Argentina  >> LISTO

if(loadR0posterior)
{
  # --- read in R0 posterior
  start_date <- as.Date(R0_dates[1]) # first case
  end_date <- as.Date(R0_dates[length(R0_dates)]) # period to forecast ahead
  date_range <- seq.Date(start_date,end_date,1)
  
  # extract all estimates from 05.03.2020 - 19.03.2020
  R0_posterior <- R0_plot[which(date_range == start_date ):which(date_range == end_date),]
  range(R0_posterior)
  r0posterior = as.vector((unlist(R0_posterior)))
  par(mfrow=c(2,1))
  R0_dense = (density((r0posterior)))
  plot(x = R0_dense$x,y=R0_dense$y,type='l',xlab='R0',ylab='Density',lwd=2)
  R0_dense = (density(log(r0posterior)))
  plot(x = R0_dense$x,y=R0_dense$y,type='l',xlab='ln(R0)',ylab='Density',lwd=2)
  
  
  rm(R0_dense,R0_plot,R0_posterior,date_range,end_date,start_date)
  
}


rm(loadContactMatrices,loadPopData,loadR0posterior,loadCaseData)
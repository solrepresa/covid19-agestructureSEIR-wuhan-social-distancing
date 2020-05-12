# App COVID SEIcIscR

#install.packages("shiny")
#install.packages("shinyBS")

library(shiny)
library(shinyBS)


### CARGAMOS DATA DE COVID  ###

source('codes/function_modelSEIcIscR.r')
source('codes/function_postprocessing.r')
#source('codes/function_plots_simOutbreak.r')
source('codes/ARG_Data.r')
source('codes/1_loadData_Arg.r')



ui <- fluidPage(

  titlePanel("COVID models"),
  p('Modelo SEIcIscR'),
  hr(),
  
  wellPanel(
    fluidRow(
      column(6,
             h3("General parameters"),
             
             ##     NuMBER SIMULATION     ##
             #sliderInput(inputId = "nsim",
             #            label = "Choose a number of simulations",
             #            value = 2, min = 1, max = 200, step = 10),
             
             ##     PARAMETERS     ##
             numericInput("durInfSim", label = "Duration of infection (days)", value = 7),
             
             numericInput("initialI", label = "Number of initial infected", value = 35),
             

             hr(),
             h3("Select format to save the plots"),
             radioButtons(inputId = "savePlot", label = "Select the file type", choices = list("png", "pdf")),

             hr(),
             h3("Without intervention"),
             dateInput("dateSII", label = "Date Start Intense Intervention", value = "2020-03-19"),
             dateInput("dateSSC", label = "Date Start School Closure", value = "2020-03-19")
             ),
      
      column(6,
             ## CASO 1
             h3("Case 1"),
             dateInput("dateEII1", label = "Date End Intense Intervention", value = "2020-04-10"),
             
             h4("Number of stagger weeks"),
             sliderInput('numWeekStagger_11', 'First change', 2, min = 0, max = 120, step = 1),
             sliderInput('numWeekStagger_12', 'Second change', 4, min = 0, max = 120, step = 1),
             sliderInput('numWeekStagger_13', 'Third change', 6, min = 0, max = 120, step = 1),
             
             h4("Proportion of the work force that is working"),
             sliderInput('WorkOpen_11', 'Initial Period', 1, min = 0, max = 1, step = 0.01),
             sliderInput('WorkOpen_12', 'Second Period', 1, min = 0, max = 1, step = 0.01),
             sliderInput('WorkOpen_13', 'Third Period', 1, min = 0, max = 1, step = 0.01),
             sliderInput('WorkOpen_14', 'Fourth period', 1, min = 0, max = 1, step = 0.01)
      ),
      
      column(6,
             ## CASO 2
             hr(),
             h3("Case 2"),
             dateInput("dateEII2", label = "Date End Intense Intervention", value = "2020-04-24"),

             h4("Number of stagger weeks"),
             sliderInput('numWeekStagger_21', 'First change', 2, min = 0, max = 120, step = 1),
             sliderInput('numWeekStagger_22', 'Second change', 4, min = 0, max = 120, step = 1),
             sliderInput('numWeekStagger_23', 'Third change', 6, min = 0, max = 120, step = 1),
             
             h4("Proportion of the work force that is working"),
             sliderInput('WorkOpen_21', 'Initial period', 1, min = 0, max = 1, step = 0.01),
             sliderInput('WorkOpen_22', 'Second period', 1, min = 0, max = 1, step = 0.01),
             sliderInput('WorkOpen_23', 'Third period', 1, min = 0, max = 1, step = 0.01),
             sliderInput('WorkOpen_24', 'Fourth period', 1, min = 0, max = 1, step = 0.01)
             
      ),
      column(6,
             ## CASO 3
             hr(),
             h3("Case 3"),
             dateInput("dateEII3", label = "Date End Intense Intervention", value = "2020-05-10"),

             h4("Number of stagger weeks"),
             sliderInput('numWeekStagger_31', 'First change', 2, min = 0, max = 120, step = 1),
             sliderInput('numWeekStagger_32', 'Second change', 4, min = 0, max = 120, step = 1),
             sliderInput('numWeekStagger_33', 'Third change', 6, min = 0, max = 120, step = 1),
             
             h4("Proportion of the work force that is working"),
             sliderInput('WorkOpen_31', 'Proportion of the work', 1, min = 0, max = 1, step = 0.01),
             sliderInput('WorkOpen_32', 'Proportion of the work', 1, min = 0, max = 1, step = 0.01),
             sliderInput('WorkOpen_33', 'Proportion of the work', 1, min = 0, max = 1, step = 0.01),
             sliderInput('WorkOpen_34', 'Proportion of the work', 1, min = 0, max = 1, step = 0.01),
             
             submitButton("Run the model")
             )

    )
  ),
  
  mainPanel(
    h3("New daily infected"),
    #bsModal(id= "modelplot", title="Titulo"),
    plotOutput("modelplot", height = "800px"),
    downloadButton(outputId = "down", label = "Download the plot"),
    downloadButton(outputId = "downParameters", label = "Download the parameters"),
    
    h3("Incidence for age"),
    plotOutput("modelplot2"),  #inc caso 0
    plotOutput("modelplot3"),  #inc caso 1
    plotOutput("modelplot4"), #inc caso 2
    plotOutput("modelplot5"),  #inc caso 3
    
    h3("Cumulative incidence over time"),
    plotOutput("modelplot6"),  #cum caso 0
    plotOutput("modelplot7"),  #cum caso 1
    plotOutput("modelplot8"),  #cum caso 2
    plotOutput("modelplot9"),  #cum caso 3
    numericInput("age", label = "Age to save plot", value = 10, max = 16),
    downloadButton(outputId = "down2", label = "Download the plot")
    
  )

)





server <- function(input, output){
  
  nsim = 10  # number of simulations
  
  R0est <- sample(x = r0posterior, size = nsim)
  set.seed(123)
  
  ##### Create reactive object ##
  epi_doNothing_l <- reactive({
    epi_doNothing_l = list() 

    for(sim in 1:nsim){
      epi_doNothing <- function(){simulateOutbreakSEIcIscR(R0t = R0est[sim],
                                                           R0tpostoutbreak = 1.17,
                                                           rho = c(rep(0.4,4),rep(0.8,12)),
                                                           dateStart = as.Date('2020-03-05'),
                                                           dateStartSchoolClosure = as.Date(input$dateSSC),
                                                           dateStartIntenseIntervention = as.Date(input$dateSII), 
                                                           dateEndIntenseIntervention = as.Date(input$dateSII),  #date we begin relaxing intense intervention
                                                           pWorkOpen = c(1, 1, 1, 1),   # pWorkOpen: proportion of the work force that is working (will be time-varying)
                                                           numWeekStagger = c(0,0,0),
                                                           pInfected = input$initialI / 1000000,
                                                           durInf = input$durInfSim)}
      
      epi_doNothing_l[[sim]] <- epi_doNothing()
    }
    
    epi_doNothing_l  #return
    
  })
  
  ##### Create reactive object ##
  epi_base_l <- reactive({
    epi_base_l = list() 
    for(sim in 1:nsim){
      epi_base <- function(){simulateOutbreakSEIcIscR(R0t = R0est[sim],
                                                      R0tpostoutbreak = 1.17,
                                                      rho = c(rep(0.4,4),rep(0.8,12)),
                                                      dateStart = as.Date('2020-03-05'),
                                                      dateStartSchoolClosure = as.Date(input$dateSSC),
                                                      dateStartIntenseIntervention = as.Date(input$dateSII), 
                                                      dateEndIntenseIntervention = as.Date(input$dateEII1),
                                                      pWorkOpen = c(input$WorkOpen_11, input$WorkOpen_12, input$WorkOpen_13, input$WorkOpen_14),
                                                      numWeekStagger = c(input$numWeekStagger_11,input$numWeekStagger_12, input$numWeekStagger_13),
                                                      pInfected = input$initialI / 1000000,
                                                      durInf = input$durInfSim)}
      epi_base_l[[sim]] <- epi_base()
    }
    
    epi_base_l
  })
  
  ##### Create reactive object ##
  epi_march_l <- reactive({
    epi_march_l = list() 
    
    for(sim in 1:nsim){
      epi_march <- function(){simulateOutbreakSEIcIscR(R0t = R0est[sim],
                                                       R0tpostoutbreak = 1.17,
                                                       rho = c(rep(0.4,4),rep(0.8,12)),
                                                       dateStart = as.Date('2020-03-05'),
                                                       dateStartSchoolClosure = as.Date(input$dateSSC),
                                                       dateStartIntenseIntervention = as.Date(input$dateSII), 
                                                       dateEndIntenseIntervention = as.Date(input$dateEII2),
                                                       pWorkOpen = c(input$WorkOpen_21, input$WorkOpen_22, input$WorkOpen_23, input$WorkOpen_24),
                                                       numWeekStagger = c(input$numWeekStagger_21,input$numWeekStagger_22, input$numWeekStagger_23),
                                                       pInfected = input$initialI / 1000000,
                                                       durInf = input$durInfSim)}
      epi_march_l[[sim]] <- epi_march()
    }
    epi_march_l
  })

  ##### Create reactive object ##
  epi_april_l <- reactive({
    epi_april_l = list() 
    
    for(sim in 1:nsim){
      epi_april <- function(){simulateOutbreakSEIcIscR(R0t = R0est[sim],
                                                       R0tpostoutbreak = 1.17,
                                                       rho = c(rep(0.4,4),rep(0.8,12)),
                                                       dateStart = as.Date('2020-03-05'),
                                                       dateStartSchoolClosure = as.Date(input$dateSSC),
                                                       dateStartIntenseIntervention = as.Date(input$dateSII), 
                                                       dateEndIntenseIntervention = as.Date(input$dateEII3),
                                                       pWorkOpen = c(input$WorkOpen_31, input$WorkOpen_32, input$WorkOpen_33, input$WorkOpen_34),
                                                       numWeekStagger = c(input$numWeekStagger_31,input$numWeekStagger_32, input$numWeekStagger_33),
                                                       pInfected = input$initialI / 1000000,
                                                       durInf = input$durInfSim)}
      epi_april_l[[sim]] <- epi_april()
    }
    epi_april_l
  })
  
  {
  covid_I = list() 
  covid_I[[1]] <- function(){ summariseSimulations(VAR = 'incidence',CI = 50,SIMS = epi_doNothing_l())}
  covid_I[[2]] <- function(){ summariseSimulations(VAR = 'incidence',CI = 50,SIMS = epi_base_l())}
  covid_I[[3]] <- function(){ summariseSimulations(VAR = 'incidence',CI = 50,SIMS = epi_march_l())}
  covid_I[[4]] <- function(){ summariseSimulations(VAR = 'incidence',CI = 50,SIMS = epi_april_l())}
  
  covid_DurInf = list() 
  covid_DurInf[[1]] <- function(){ summariseSimulations_mid(CI = 50,SIMS = epi_doNothing_l())}
  covid_DurInf[[2]] <- function(){ summariseSimulations_mid(CI = 50,SIMS = epi_base_l())}
  covid_DurInf[[3]] <- function(){ summariseSimulations_mid(CI = 50,SIMS = epi_march_l())}
  covid_DurInf[[4]] <- function(){ summariseSimulations_mid(CI = 50,SIMS = epi_april_l())}
  }
  
  legends = c("Sin intervencion", "Caso 1", "Caso 2", "Caso 3")
  l = c("Mediana", "Quartil Inferior",  "Quartil Superior")
  
  output$modelplot <- renderPlot({
    
    par(par(no.readonly=TRUE))
    par(mfrow=c(2,2), oma=c(4,2,2,4), mar=c(3,3,2,0), pch=16)
    
    for( i in 1:4){
      plot(covid_I[[i]]()[["Sim1"]][["time"]]/7,
           covid_I[[i]]()[["summary"]][["uci"]],
          # xlim=c(0, 61),    # ~428 days
           #  ylim=c(0,70000),
           type="l", 
           col='tomato',
           main= legends[i],
           xlab="",
           ylab="")
      lines(covid_I[[i]]()[["Sim1"]][["time"]]/7, 
            covid_I[[i]]()[["summary"]][["lci"]], 
            col="steelblue")
      lines(covid_I[[i]]()[["Sim1"]][["time"]]/7, 
            covid_I[[i]]()[["summary"]][["median"]], 
            col="black")
      legend("topright" , legend= l,
             col=c("black", "steelblue",'tomato'), 
          #  lty=2:1,
             bty='n',
             cex = 1)
      mtext(text = "Infectados c/24hs", side=2,line=0,outer=TRUE)
      mtext("Día final de las intervenciones", side=1,line=2, outer=TRUE, adj = 1)
      mtext(text = paste("Caso 1:", covid_I[[1]]()[["Sim1"]][["dateEndIntenseIntervention"]],
                         "- Caso 2:", covid_I[[2]]()[["Sim1"]][["dateEndIntenseIntervention"]],
                         "- Caso 3:", covid_I[[3]]()[["Sim1"]][["dateEndIntenseIntervention"]],
                         "- Caso 4:", covid_I[[4]]()[["Sim1"]][["dateEndIntenseIntervention"]]), 
            side=1,line=3, outer=TRUE, adj = 1)
    }  
    
    
  })
  
  
  output$down <- downloadHandler(
    
    filename = function(){ paste( "Ninfected_SEIcIscR.", input$savePlot, sep="") },     #Specify the file name
    content = function(file){    #open the device
      
      if(input$savePlot == "png"){
        png(file)
      }else{
        pdf(file)
      }
      
      
      par(par(no.readonly=TRUE))
      par(mfrow=c(2,2), oma=c(4,2,2,4), mar=c(3,3,2,0), pch=16)
      
      for( i in 1:4){
        plot(covid_I[[i]]()[["Sim1"]][["time"]]/7,
             covid_I[[i]]()[["summary"]][["uci"]],
            # xlim=c(0, 61),    # ~428 days
             #  ylim=c(0,70000),
             type="l", 
             col="red",
             main= legends[i],
             xlab="",
             ylab="")
        lines(covid_I[[i]]()[["Sim1"]][["time"]]/7, 
              covid_I[[i]]()[["summary"]][["lci"]], 
              col="green")
        lines(covid_I[[i]]()[["Sim1"]][["time"]]/7, 
              covid_I[[i]]()[["summary"]][["median"]], 
              col="black")
        legend("topleft" , legend= l,
               col=c("black", "green", "red"), 
               lty=2:1,
               bty='n',
               cex = 1)
        mtext(text = "Infectados c/24hs", side=2,line=0,outer=TRUE)
        mtext("Día final de las intervenciones", side=1,line=2, outer=TRUE, adj = 1)
        mtext(text = paste("Caso 1:", covid_I[[1]]()[["Sim1"]][["dateEndIntenseIntervention"]],
                           "- Caso 2:", covid_I[[2]]()[["Sim1"]][["dateEndIntenseIntervention"]],
                           "- Caso 3:", covid_I[[3]]()[["Sim1"]][["dateEndIntenseIntervention"]],
                           "- Caso 4:", covid_I[[4]]()[["Sim1"]][["dateEndIntenseIntervention"]]), 
              side=1,line=3, outer=TRUE, adj = 1)
      
      }
      dev.off()
    }
  )
  
  param <- reactive({ 
    c("Case 1:", 
      "Date End Intense Intervention", format(as.Date(input$dateEII1,  "%Y%m%d"), "%d-%m-%Y"),
      "Number of Weeks Staggers",
      input$numWeekStagger_11,
      input$numWeekStagger_12,
      input$numWeekStagger_13,
      "Proportion Work Open",
      input$WorkOpen_11,
      input$WorkOpen_12,
      input$WorkOpen_13,
      input$WorkOpen_14,
      "Case 2:", 
      "Date End Intense Intervention", format(as.Date(input$dateEII2,  "%Y%m%d"), "%d-%m-%Y"),
      "Number of Weeks Staggers",
      input$numWeekStagger_21,
      input$numWeekStagger_22,
      input$numWeekStagger_23,
      "Proportion Work Open",
      input$WorkOpen_21,
      input$WorkOpen_22,
      input$WorkOpen_23,
      input$WorkOpen_24,
      "Case 3:", 
      "Date End Intense Intervention", format(as.Date(input$dateEII3,  "%Y%m%d"), "%d-%m-%Y"),
      "Number of Weeks Staggers",
      input$numWeekStagger_31,
      input$numWeekStagger_32,
      input$numWeekStagger_33,
      "Proportion Work Open",
      input$WorkOpen_31,
      input$WorkOpen_32,
      input$WorkOpen_33,
      input$WorkOpen_34)
    
    })

  
  # Download simulation parameter
  
  output$downParameters <- downloadHandler(  

    filename = function(){
      paste("parameter-", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      # writeLines(paste(param, collapse = ", "), file)
      write.table(param(), file, col.names=FALSE, row.names = FALSE)
    }
  )
  
  agegp = c(2, 7, 10, 16)  # What ages do you plot?
  
  output$modelplot2 <- renderPlot({   # incidence over time
    
    plot(epi_doNothing_l()[[1]]$time/7,       
         epi_doNothing_l()[[1]]$incidence[,agegp[2]], 
         type='l', 
         lwd=2,
         main = legends[1],
         xlab = "Time(weeks)", 
         ylab = "Daily no. of infections")
    lines(x = epi_doNothing_l()[[1]]$time/7, y = epi_doNothing_l()[[1]]$incidence[,agegp[1]],lwd=2,col='grey40')
    lines(x = epi_doNothing_l()[[1]]$time/7, y = epi_doNothing_l()[[1]]$incidence[,agegp[3]],lwd=2,col='steelblue')
    lines(x = epi_doNothing_l()[[1]]$time/7, y = epi_doNothing_l()[[1]]$incidence[,agegp[4]],lwd=2,col='tomato')
    legend("topright", 
           legend = c(paste0("[", (agegp[1]-1)*5,',',agegp[1]*5,')'), paste0("[",(agegp[2]-1)*5,',',agegp[2]*5,')'),
                      paste0("[",(agegp[3]-1)*5,',',agegp[3]*5,')'), paste0("[",(agegp[4]-1)*5,',',agegp[4]*5,')')),
           col = c("grey40", "black", "steelblue",'tomato'), 
           bty='n',
           lty= rep(1,4),
           lwd= rep(2,4), 
           title = "Incidence for age",
           #y.intersp = 0.15,
           cex= 0.8)
  })
  
  output$modelplot3 <- renderPlot({   # incidence over time
    
    plot(epi_base_l()[[1]]$time/7,       
         epi_base_l()[[1]]$incidence[,agegp[2]], 
         type='l', 
         lwd=2,
         main = legends[2],
         xlab = "Time(weeks)", 
         ylab = "Daily no. of infections")
    lines(x = epi_base_l()[[1]]$time/7, y = epi_base_l()[[1]]$incidence[,agegp[1]],lwd=2,col='grey40')
    lines(x = epi_base_l()[[1]]$time/7, y = epi_base_l()[[1]]$incidence[,agegp[3]],lwd=2,col='steelblue')
    lines(x = epi_base_l()[[1]]$time/7, y = epi_base_l()[[1]]$incidence[,agegp[4]],lwd=2,col='tomato')
    legend("topright", 
           legend = c(paste0("[", (agegp[1]-1)*5,',',agegp[1]*5,')'), paste0("[",(agegp[2]-1)*5,',',agegp[2]*5,')'),
                      paste0("[",(agegp[3]-1)*5,',',agegp[3]*5,')'), paste0("[",(agegp[4]-1)*5,',',agegp[4]*5,')')),
           col = c("grey40", "black", "steelblue",'tomato'), 
           bty='n',
           lty= rep(1,4),
           lwd= rep(2,4), 
           title = "Incidence for age",
           #y.intersp = 0.15,
           cex= 0.8)
  })
  
  output$modelplot4 <- renderPlot({   # incidence over time
    
    plot(epi_march_l()[[1]]$time/7,       
         epi_march_l()[[1]]$incidence[,agegp[2]], 
         type='l', 
         lwd=2,
         main = legends[3],
         xlab = "Time(weeks)", 
         ylab = "Daily no. of infections")
    lines(x = epi_march_l()[[1]]$time/7, y = epi_march_l()[[1]]$incidence[,agegp[1]],lwd=2,col='grey40')
    lines(x = epi_march_l()[[1]]$time/7, y = epi_march_l()[[1]]$incidence[,agegp[3]],lwd=2,col='steelblue')
    lines(x = epi_march_l()[[1]]$time/7, y = epi_march_l()[[1]]$incidence[,agegp[4]],lwd=2,col='tomato')
    legend("topright", 
           legend = c(paste0("[", (agegp[1]-1)*5,',',agegp[1]*5,')'), paste0("[",(agegp[2]-1)*5,',',agegp[2]*5,')'),
                      paste0("[",(agegp[3]-1)*5,',',agegp[3]*5,')'), paste0("[",(agegp[4]-1)*5,',',agegp[4]*5,')')),
           col = c("grey40", "black", "steelblue",'tomato'), 
           bty='n',
           lty= rep(1,4),
           lwd= rep(2,4), 
           title = "Incidence for age",
           #y.intersp = 0.15,
           cex= 0.8)
  })
  
  output$modelplot5 <- renderPlot({   # incidence over time
    
    plot(epi_april_l()[[1]]$time/7,       
         epi_april_l()[[1]]$incidence[,agegp[2]], 
         type='l', 
         lwd=2,
         main = legends[4],
         xlab = "Time(weeks)", 
         ylab = "Daily no. of infections")
    lines(x = epi_april_l()[[1]]$time/7, y = epi_april_l()[[1]]$incidence[,agegp[1]],lwd=2,col='grey40')
    lines(x = epi_april_l()[[1]]$time/7, y = epi_april_l()[[1]]$incidence[,agegp[3]],lwd=2,col='steelblue')
    lines(x = epi_april_l()[[1]]$time/7, y = epi_april_l()[[1]]$incidence[,agegp[4]],lwd=2,col='tomato')
    legend("topright", 
           legend = c(paste0("[", (agegp[1]-1)*5,',',agegp[1]*5,')'), paste0("[",(agegp[2]-1)*5,',',agegp[2]*5,')'),
                      paste0("[",(agegp[3]-1)*5,',',agegp[3]*5,')'), paste0("[",(agegp[4]-1)*5,',',agegp[4]*5,')')),
           col = c("grey40", "black", "steelblue",'tomato'), 
           bty='n',
           lty= rep(1,4),
           lwd= rep(2,4), 
           title = "Incidence for age",
           #y.intersp = 0.15,
           cex= 0.8)
    
    
  })
  
  
  # cumulative incidence over time
  
  output$modelplot6 <- renderPlot({ 
    
    plot(epi_doNothing_l()[[1]]$time/7, 
         (epi_doNothing_l()[[1]]$N_age[agegp[2]]-epi_doNothing_l()[[1]]$S[,agegp[2]])/epi_doNothing_l()[[1]]$N_age[agegp[2]], 
         lwd=2,
         type='l', 
         main= legends[1],
         xlab="Time(weeks)", 
         ylab="Cum incidence",
         ylim = c(0,1));
    lines(epi_doNothing_l()[[1]]$time/7, (epi_doNothing_l()[[1]]$N_age[agegp[1]]-epi_doNothing_l()[[1]]$S[,agegp[1]])/epi_doNothing_l()[[1]]$N_age[agegp[1]],lwd=2,col='grey40')
    lines(epi_doNothing_l()[[1]]$time/7, (epi_doNothing_l()[[1]]$N_age[agegp[3]]-epi_doNothing_l()[[1]]$S[,agegp[3]])/epi_doNothing_l()[[1]]$N_age[agegp[3]],lwd=2,col='steelblue')
    lines(epi_doNothing_l()[[1]]$time/7, (epi_doNothing_l()[[1]]$N_age[agegp[4]]-epi_doNothing_l()[[1]]$S[,agegp[4]])/epi_doNothing_l()[[1]]$N_age[agegp[4]],lwd=2,col='tomato',lty='dashed')
    legend("bottomright", 
           legend = c(paste0("[", (agegp[1]-1)*5,',',agegp[1]*5,')'), paste0("[",(agegp[2]-1)*5,',',agegp[2]*5,')'),
                      paste0("[",(agegp[3]-1)*5,',',agegp[3]*5,')'), paste0("[",(agegp[4]-1)*5,',',agegp[4]*5,')')),
           col = c("grey40", "black", "steelblue",'tomato'), 
           bty='n',
           lty= rep(1,4),
           lwd= rep(2,4), 
           title = "Cumulative incidence for age",
           #y.intersp = 0.15,
           cex= 0.8)
  })
  
  output$modelplot7 <- renderPlot({  
    
    plot(epi_base_l()[[1]]$time/7,       
         (epi_base_l()[[1]]$N_age[agegp[2]]-epi_base_l()[[1]]$S[,agegp[2]])/epi_base_l()[[1]]$N_age[agegp[2]], 
         type='l', 
         lwd=2,
         main = legends[2],
         xlab = "Time(weeks)", 
         ylab = "Daily no. of infections")
    lines(epi_base_l()[[1]]$time/7, (epi_base_l()[[1]]$N_age[agegp[1]]-epi_base_l()[[1]]$S[,agegp[1]])/epi_base_l()[[1]]$N_age[agegp[1]],lwd=2,col='grey40')
    lines(epi_base_l()[[1]]$time/7, (epi_base_l()[[1]]$N_age[agegp[3]]-epi_base_l()[[1]]$S[,agegp[3]])/epi_base_l()[[1]]$N_age[agegp[3]],lwd=2,col='steelblue')
    lines(epi_base_l()[[1]]$time/7, (epi_base_l()[[1]]$N_age[agegp[4]]-epi_base_l()[[1]]$S[,agegp[4]])/epi_base_l()[[1]]$N_age[agegp[4]],lwd=2,col='tomato',lty='dashed')
    legend("bottomright", 
           legend = c(paste0("[", (agegp[1]-1)*5,',',agegp[1]*5,')'), paste0("[",(agegp[2]-1)*5,',',agegp[2]*5,')'),
                      paste0("[",(agegp[3]-1)*5,',',agegp[3]*5,')'), paste0("[",(agegp[4]-1)*5,',',agegp[4]*5,')')),
           col = c("grey40", "black", "steelblue",'tomato'), 
           bty='n',
           lty= rep(1,4),
           lwd= rep(2,4), 
           title = "Cumulative incidence for age",
           #y.intersp = 0.15,
           cex= 0.8)
  })
  
  output$modelplot8 <- renderPlot({  
    plot(epi_march_l()[[1]]$time/7,       
         (epi_march_l()[[1]]$N_age[agegp[2]]-epi_march_l()[[1]]$S[,agegp[2]])/epi_march_l()[[1]]$N_age[agegp[2]], 
         type='l', 
         lwd=2,
         main = legends[3],
         xlab = "Time(weeks)", 
         ylab = "Daily no. of infections")
    lines(epi_march_l()[[1]]$time/7, (epi_march_l()[[1]]$N_age[agegp[1]]-epi_march_l()[[1]]$S[,agegp[1]])/epi_march_l()[[1]]$N_age[agegp[1]],lwd=2,col='grey40')
    lines(epi_march_l()[[1]]$time/7, (epi_march_l()[[1]]$N_age[agegp[3]]-epi_march_l()[[1]]$S[,agegp[3]])/epi_march_l()[[1]]$N_age[agegp[3]],lwd=2,col='steelblue')
    lines(epi_march_l()[[1]]$time/7, (epi_march_l()[[1]]$N_age[agegp[4]]-epi_march_l()[[1]]$S[,agegp[4]])/epi_march_l()[[1]]$N_age[agegp[4]],lwd=2,col='tomato',lty='dashed')
    legend("bottomright", 
           legend = c(paste0("[", (agegp[1]-1)*5,',',agegp[1]*5,')'), paste0("[",(agegp[2]-1)*5,',',agegp[2]*5,')'),
                      paste0("[",(agegp[3]-1)*5,',',agegp[3]*5,')'), paste0("[",(agegp[4]-1)*5,',',agegp[4]*5,')')),
           col = c("grey40", "black", "steelblue",'tomato'), 
           bty='n',
           lty= rep(1,4),
           lwd= rep(2,4), 
           title = "Cumulative incidence for age",
           #y.intersp = 0.15,
           cex= 0.8)

  })
  
  output$modelplot9 <- renderPlot({   
    plot(epi_april_l()[[1]]$time/7,       
         (epi_april_l()[[1]]$N_age[agegp[2]]-epi_april_l()[[1]]$S[,agegp[2]])/epi_april_l()[[1]]$N_age[agegp[2]], 
         type='l', 
         lwd=2,
         main = legends[4],
         xlab = "Time(weeks)", 
         ylab = "Daily no. of infections")
    lines(epi_april_l()[[1]]$time/7, (epi_april_l()[[1]]$N_age[agegp[1]]-epi_april_l()[[1]]$S[,agegp[1]])/epi_april_l()[[1]]$N_age[agegp[1]],lwd=2,col='grey40')
    lines(epi_april_l()[[1]]$time/7, (epi_april_l()[[1]]$N_age[agegp[3]]-epi_april_l()[[1]]$S[,agegp[3]])/epi_april_l()[[1]]$N_age[agegp[3]],lwd=2,col='steelblue')
    lines(epi_april_l()[[1]]$time/7, (epi_april_l()[[1]]$N_age[agegp[4]]-epi_april_l()[[1]]$S[,agegp[4]])/epi_april_l()[[1]]$N_age[agegp[4]],lwd=2,col='tomato',lty='dashed')
    legend("bottomright", 
           legend = c(paste0("[", (agegp[1]-1)*5,',',agegp[1]*5,')'), paste0("[",(agegp[2]-1)*5,',',agegp[2]*5,')'),
                      paste0("[",(agegp[3]-1)*5,',',agegp[3]*5,')'), paste0("[",(agegp[4]-1)*5,',',agegp[4]*5,')')),
           col = c("grey40", "black", "steelblue",'tomato'), 
           bty='n',
           lty= rep(1,4),
           lwd= rep(2,4), 
           title = "Cumulative incidence for age",
           #y.intersp = 0.15,
           cex= 0.8)

    
    
  })
  
  output$down2 <- downloadHandler(
    
    filename = function(){ paste( "IncidenceAge_SEIcIscR_", input$age, ".", input$savePlot, sep="") },     #Specify the file name
    content = function(file){    #open the device
      
      if(input$savePlot == "png"){
        png(file)
      }else{
        pdf(file)
      }
      
      agegp = input$age
      
      par(mfrow = c(2,1), oma = c(1,1,2,1) )
      
      plot(epi_doNothing_l()[[1]][["time"]]/7, 
           epi_doNothing_l()[[1]][["incidence"]][,agegp], 
           type='l', 
           lwd=2,
           main = paste0("Incidence for age [",(agegp-1)*5,',',agegp*5,')'),
           xlab = "Time(weeks)", 
           ylab = "Daily no. of infections")
      lines(x = epi_base_l()[[1]][["time"]]/7, y = epi_base_l()[[1]][["incidence"]][,agegp],lwd=2,col='grey40')
      lines(x = epi_march_l()[[1]][["time"]]/7, y = epi_march_l()[[1]][["incidence"]][,agegp],lwd=2,col='steelblue')
      lines(x = epi_april_l()[[1]][["time"]]/7, y = epi_april_l()[[1]][["incidence"]][,agegp],lwd=2,col='tomato',lty='dashed')
      
      
      plot(epi_doNothing_l()[[1]][["time"]]/7, 
           (epi_doNothing_l()[[1]][["N_age"]][agegp] - epi_doNothing_l()[[1]][["S"]][,agegp])/epi_doNothing_l()[[1]][["N_age"]][agegp], 
           lwd=2,
           type='l', 
           main=paste0("Cum incidence for age [",(agegp-1)*5,',',agegp*5,')'),
           xlab="Time(weeks)", 
           ylab="Cum incidence",
           ylim = c(0,1))
      lines(epi_base_l()[[1]][["time"]]/7, (epi_base_l()[[1]][["N_age"]][agegp]-epi_base_l()[[1]][["S"]][,agegp])/epi_base_l()[[1]][["N_age"]][agegp],lwd=2,col='grey40')
      lines(epi_march_l()[[1]][["time"]]/7, (epi_march_l()[[1]][["N_age"]][agegp]-epi_march_l()[[1]][["S"]][,agegp])/epi_march_l()[[1]][["N_age"]][agegp],lwd=2,col='steelblue')
      lines(epi_april_l()[[1]][["time"]]/7, (epi_april_l()[[1]][["N_age"]][agegp]-epi_april_l()[[1]][["S"]][,agegp])/epi_april_l()[[1]][["N_age"]][agegp],lwd=2,col='tomato',lty='dashed')
      legend(0.25, 0.98, legend = legends,
             col = c("black", "grey40","steelblue",'tomato'), 
             bty='n',
             lty= rep(1,4),
             lwd= rep(2,4), 
             #y.intersp = 0.15,
             cex= 0.8)
      
      dev.off()
    }
  )
  

}


shinyApp( ui = ui, server = server)


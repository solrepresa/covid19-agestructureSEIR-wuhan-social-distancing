# App COVID SEIcIscR

library(shiny)


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
             sliderInput('numWeekStagger_11', 'First change', 2, min = 0, max = 10, step = 1),
             sliderInput('numWeekStagger_12', 'Second change', 4, min = 0, max = 10, step = 1),
             sliderInput('numWeekStagger_13', 'Third change', 6, min = 0, max = 10, step = 1),
             
             h4("Proportion of the work force that is working"),
             sliderInput('WorkOpen_11', 'Initial Period', 1, min = 0, max = 1, step = 0.25),
             sliderInput('WorkOpen_12', 'Second Period', 1, min = 0, max = 1, step = 0.25),
             sliderInput('WorkOpen_13', 'Third Period', 1, min = 0, max = 1, step = 0.25),
             sliderInput('WorkOpen_14', 'Fourth period', 1, min = 0, max = 1, step = 0.25)
      ),
      
      column(6,
             ## CASO 2
             hr(),
             h3("Case 2"),
             dateInput("dateEII2", label = "Date End Intense Intervention", value = "2020-04-24"),

             h4("Number of stagger weeks"),
             sliderInput('numWeekStagger_21', 'First change', 2, min = 0, max = 10, step = 1),
             sliderInput('numWeekStagger_22', 'Second change', 4, min = 0, max = 10, step = 1),
             sliderInput('numWeekStagger_23', 'Third change', 6, min = 0, max = 10, step = 1),
             
             h4("Proportion of the work force that is working"),
             sliderInput('WorkOpen_21', 'Initial period', 1, min = 0, max = 1, step = 0.25),
             sliderInput('WorkOpen_22', 'Second period', 1, min = 0, max = 1, step = 0.25),
             sliderInput('WorkOpen_23', 'Third period', 1, min = 0, max = 1, step = 0.25),
             sliderInput('WorkOpen_24', 'Fourth period', 1, min = 0, max = 1, step = 0.25)
             
      ),
      column(6,
             ## CASO 3
             hr(),
             h3("Case 3"),
             dateInput("dateEII3", label = "Date End Intense Intervention", value = "2020-05-10"),

             h4("Number of stagger weeks"),
             sliderInput('numWeekStagger_31', 'First change', 2, min = 0, max = 10, step = 1),
             sliderInput('numWeekStagger_32', 'Second change', 4, min = 0, max = 10, step = 1),
             sliderInput('numWeekStagger_33', 'Third change', 6, min = 0, max = 10, step = 1),
             
             h4("Proportion of the work force that is working"),
             sliderInput('WorkOpen_31', 'Proportion of the work', 1, min = 0, max = 1, step = 0.25),
             sliderInput('WorkOpen_32', 'Proportion of the work', 1, min = 0, max = 1, step = 0.25),
             sliderInput('WorkOpen_33', 'Proportion of the work', 1, min = 0, max = 1, step = 0.25),
             sliderInput('WorkOpen_34', 'Proportion of the work', 1, min = 0, max = 1, step = 0.25),
             
             submitButton("Run the model")
             )

    )
  ),
  
  mainPanel(
    h3("New daily infected"),
    plotOutput("modelplot"),
    downloadButton(outputId = "down", label = "Download the plot")
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
  
  
  output$modelplot <- renderPlot({
    
    ## EL PLOT  ##
    
    legends = c("Sin intervencion", "Caso 1", "Caso 2", "Caso 3")
    l = c("Mediana", "Quartil Inferior",  "Quartil Superior")
    
    
    par(par(no.readonly=TRUE))
    par(mfrow=c(2,2), oma=c(4,2,2,4), mar=c(3,3,2,0), pch=16)
    
    for( i in 1:4){
      plot(covid_I[[i]]()[["Sim1"]][["time"]]/7,
           covid_I[[i]]()[["summary"]][["uci"]],
           xlim=c(0, 61),    # ~428 days
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
    
    
  })
  
  output$down <- downloadHandler(
    
    filename = function(){ paste( "Ninfected_SEIcIscR.", input$savePlot, sep="") },     #Specify the file name
    content = function(file){    #open the device
      
      if(input$savePlot == "png"){
        png(file)
      }else{
        pdf(file)
      }
      
      legends = c("Sin intervencion", "Caso 1", "Caso 2", "Caso 3")
      l = c("Mediana", "Quartil Inferior",  "Quartil Superior")
      
      
      par(par(no.readonly=TRUE))
      par(mfrow=c(2,2), oma=c(4,2,2,4), mar=c(3,3,2,0), pch=16)
      
      for( i in 1:4){
        plot(covid_I[[i]]()[["Sim1"]][["time"]]/7,
             covid_I[[i]]()[["summary"]][["uci"]],
             xlim=c(0, 61),    # ~428 days
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
  

}


shinyApp( ui = ui, server = server)


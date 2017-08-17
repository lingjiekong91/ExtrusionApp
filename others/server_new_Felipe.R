server<-function(input,output,session){
  
  #Part Catalog--Sinlge Extrusion PPS Data
  
  #Checkbox
  output$PCSPN_s<-renderUI({
    as.character(checkboxInput("PCSPN_d","Part Number",value=TRUE, 
                               onchange = 'Shiny.onInputeChange(\"check_button\", this.id
                               )'))
  })
  output$PCSPD_s<-renderUI({
    checkboxInput("PCSPD_d","Part Description",value=TRUE)
  })
  output$PCSRN_s<-renderUI({
    checkboxInput("PCSRN_d","Resin Number",value=TRUE)
  })
  output$PCSRD_s<-renderUI({
    checkboxInput("PCSRD_d","Resin Description",value=TRUE)
  })
  output$PCSPPSN_s<-renderUI({
    checkboxInput("PCSPPSN_d","PPS Number",value=F)
  })
  #Tooling
  output$PCSDS_s<-renderUI({
    checkboxInput("PCSDS_d","Die Size (in)",value=F)
  })
  output$PCSDLL_s<-renderUI({
    checkboxInput("PCSDLL_d","Die Land Length (in)",value=F)
  })
  output$PCSTS_s<-renderUI({
    checkboxInput("PCSTS_d","Tip Size (in)",value=F)
  })
  output$PCSTLL_s<-renderUI({
    checkboxInput("PCSTLL_d","Tip Land Length (in)",value=F)
  })
  output$PCSSP_s<-renderUI({
    checkboxInput("PCSSP_d","Screw Print",value=F)
  })
  
  
  output$PCSFT_s<-renderUI({
    checkboxInput("PCSFT_d","Feedthroat Temperature F",value=F)
  })
  output$PCSBZT1_s<-renderUI({
    checkboxInput("PCSBZT1_d","Barrel Zone 1 Temperature F",value=F)
  })
  output$PCSBZT2_s<-renderUI({
    checkboxInput("PCSBZT2_d","Barrel Zone 2 Temperature F",value=F)
  })
  output$PCSBZT3_s<-renderUI({
    checkboxInput("PCSBZT3_d","Barrel Zone 3 Temperature F",value=F)
  })
  
  
  output$PCSCT_s<-renderUI({
    checkboxInput("PCSCT_d","Clamp Temperature F",value=F)
  })
  output$PCSAT_s<-renderUI({
    checkboxInput("PCSAT_d","Adapter Temperature F",value=F)
  })
  output$PCSDT1_s<-renderUI({
    checkboxInput("PCSDT1_d","Die 1 Temperature F",value=F)
  })
  output$PCSDT2_s<-renderUI({
    checkboxInput("PCSDT2_d","Die 2 Temperature F",value=F)
  })
  
  output$PCSIDI_s<-renderUI({
    checkboxInput("PCSIDI_d","Inner Diameter (in)",value=TRUE)
  })
  output$PCSODI_s<-renderUI({
    checkboxInput("PCSODI_d","Outer Diameter (in)",value=TRUE)
  })
  output$PCSWT_s<-renderUI({
    checkboxInput("PCSWT_d","Wall Thickness (in)",value=TRUE)
  })
  output$PCSOR_s<-renderUI({
    checkboxInput("PCSOR_d","Out of Roundness (in)",value=F)
  })
  
  
  
  output$PCSCCT_s<-renderUI({
    checkboxInput("PCSCCT_d","Concentricity",value=F)
  })
  output$PCSLength_s<-renderUI({
    checkboxInput("PCSLength_d","Length (in)",value=F)
  })
  output$PCSPPD_s<-renderUI({
    checkboxInput("PCSPPD_d","Perpendicularity (in)",value=F)
  })
  
  
  output$PCSNEXIV_s<-renderUI({
    checkboxInput("PCSNEXIV_d","NEXIV",value=F)
  })
  output$PCSAnnealed_s<-renderUI({
    checkboxInput("PCSAnnealed_d","Annealed",value=F)
  })
  output$PCSCaliper_s<-renderUI({
    checkboxInput("PCSCaliper_d","Caliper",value=F)
  })
  output$PCSOS_s<-renderUI({
    checkboxInput("PCSOS_d","OD Sort",value=F)
  })
  output$PCSMP_s<-renderUI({
    checkboxInput("PCSMP_d","Melt Pump",value=F)
  })
  output$PCSHT_s<-renderUI({
    checkboxInput("PCSHT_d","Hypo Tip",value=F)
  })
  output$PCSSPD_s<-renderUI({
    checkboxInput("PCSSPD_d","Sparker Die",value=F)
  })
  output$PCSSLD_s<-renderUI({
    checkboxInput("PCSSLD_d","Slicking Die",value=F)
  })
  output$PCSDLN_s<-renderUI({
    checkboxInput("PCSDLN_d","Delamination",value=F)
  })
  output$PCSULT_s<-renderUI({
    checkboxInput("PCSULT_d","Ultrasonic",value=F)
  })
  output$PCSVC_s<-renderUI({
    checkboxInput("PCSVC_d","Vacuum Calibration",value=F)
  })
  output$PCSIRD_s<-renderUI({
    checkboxInput("PCSIRD_d","Irradiated",value=F)
  })
  
  
  
  #Search Box
  
  #Part Resin
  output$PCSPN_input<-renderUI({
    selectizeInput("PCSPN",label = NULL,multiple=TRUE,
                   c("All",unique(as.character(single_pps_data$`Part Number`))),
                   selected="All")
  })
  output$PCSPD_input<-renderUI({
    selectInput("PCSPD",label = NULL,
                c("All",unique(as.character(single_pps_data$`Part Description`))))
  })
  output$PCSRN_input<-renderUI({
    selectInput("PCSRN",label = NULL,
                c("All",unique(as.character(single_pps_data$`Resin Number`))))
  })
  output$PCSRD_input<-renderUI({
    selectInput("PCSRD",label = NULL,
                c("All",unique(as.character(single_pps_data$`Resin Description`))))
  })
  output$PCSPPSN_input<-renderUI({
    selectInput("PCSPPSN",label = NULL,
                c("All",unique(as.character(single_pps_data$`PPS Number`))))
  })
  
  #Tooling
  output$PCSDS_min_input<-renderUI({
    numericInput("PCSDS_min",label = NULL,value=PCSDSmin,step=0.001)
  })
  output$PCSDS_max_input<-renderUI({
    numericInput("PCSDS_max",label = NULL,value=PCSDSmax,step=0.001)
  })
  output$PCSDLL_input<-renderUI({
    DLL_min=-1
    DLL_max=0.54
    selectInput("PCSDLL",label = NULL,c("All",unique(as.character(single_pps_data$`Die Land Length (in)`))))
  })
  output$PCSTS_min_input<-renderUI({
    numericInput("PCSTS_min",label = NULL,value=PCSTSmin,step=0.001)
  })
  output$PCSTS_max_input<-renderUI({
    numericInput("PCSTS_max",label = NULL,value=PCSTSmax,step=0.001)
  })
  output$PCSTLL_input<-renderUI({
    TLL_min=-1
    TLL_max=0.49
    selectInput("PCSTLL",label = NULL,c("All",unique(as.character(single_pps_data$`Tip Land Length (in)`))))
  })
  output$PCSSP_input<-renderUI({
    selectInput("PCSSP",label = NULL,
                c("All",unique(as.character(single_pps_data$`Screw Print`))))
  })
  #Attributes_1
  output$PCSFT_min_input<-renderUI({
    numericInput("PCSFT_min",label = NULL,value = PCSFTmin,step=1)
  })
  output$PCSFT_max_input<-renderUI({
    numericInput("PCSFT_max",label = NULL,value=PCSFTmax,step=1)
  })
  output$PCSBZT1_min_input<-renderUI({
    numericInput("PCSBZT1_min",label = NULL,value=PCSBZT1min,step=5)
  })
  output$PCSBZT1_max_input<-renderUI({
    numericInput("PCSBZT1_max",label = NULL,value=PCSBZT1max,step=5)
  })
  output$PCSBZT2_min_input<-renderUI({
    numericInput("PCSBZT2_min",label = NULL,value=PCSBZT2min,step=5)
  })
  output$PCSBZT2_max_input<-renderUI({
    numericInput("PCSBZT2_max",label = NULL,value=PCSBZT2max,step=5)
  })
  output$PCSBZT3_min_input<-renderUI({
    numericInput("PCSBZT3_min",label = NULL,value=PCSBZT3min,step=5)
  })
  output$PCSBZT3_max_input<-renderUI({
    numericInput("PCSBZT3_max",label = NULL,value=PCSBZT3max,step=5)
  })
  
  #Attrobites_2
  output$PCSCT_min_input<-renderUI({
    numericInput("PCSCT_min",label = NULL,value=PCSCTmin,step=5)
  })
  output$PCSCT_max_input<-renderUI({
    numericInput("PCSCT_max",label = NULL,value=PCSCTmax,step=5)
  })
  output$PCSAT_min_input<-renderUI({
    numericInput("PCSAT_min",label = NULL,value=PCSATmin,step=5)
  })
  output$PCSAT_max_input<-renderUI({
    numericInput("PCSAT_max",label = NULL,value=PCSATmax,step=5)
  })
  output$PCSDT1_min_input<-renderUI({
    numericInput("PCSDT1_min",label = NULL,value=PCSDT1min,step=5)
  })
  output$PCSDT1_max_input<-renderUI({
    numericInput("PCSDT1_max",label = NULL,value=PCSDT1max,step=5)
  })
  output$PCSDT2_min_input<-renderUI({
    numericInput("PCSDT2_min",label = NULL,value=PCSDT2min,step=5)
  })
  output$PCSDT2_max_input<-renderUI({
    numericInput("PCSDT2_max",label = NULL,value=PCSDT2max,step=5)
  })
  #Temps
  output$PCSIDI_min_input<-renderUI({
    numericInput("PCSIDI_min",label = NULL,value=PCSIDImin,step=0.001)
  })
  output$PCSIDI_max_input<-renderUI({
    numericInput("PCSIDI_max",label = NULL,value=PCSIDImax,step=0.001)
  })
  output$PCSODI_min_input<-renderUI({
    numericInput("PCSODI_min",label = NULL,value=PCSODImin,step=0.001)
  })
  output$PCSODI_max_input<-renderUI({
    numericInput("PCSODI_max",label = NULL,value=PCSODImax,step=0.001)
  })
  output$PCSWT_min_input<-renderUI({
    numericInput("PCSWT_min",label = NULL,value=PCSWTmin,step=0.001)
  })
  output$PCSWT_max_input<-renderUI({
    numericInput("PCSWT_max",label = NULL,value=PCSWTmax,step=0.001)
  })
  output$PCSOR_min_input<-renderUI({
    numericInput("PCSOR_min",label = NULL,value=PCSORmin,step=0.001)
  })
  output$PCSOR_max_input<-renderUI({
    numericInput("PCSOR_max",label = NULL,value=PCSORmax,step=0.001)
  })
  output$PCSCCT_min_input<-renderUI({
    numericInput("PCSCCT_min",label = NULL,value=PCSCCTmin,step=0.0001)
  })
  output$PCSCCT_max_input<-renderUI({
    numericInput("PCSCCT_max",label = NULL,value=PCSCCTmax,step=0.0001)
  })
  output$PCSLength_min_input<-renderUI({
    numericInput("PCSLength_min",label = NULL,value=PCSLengthmin,step=1)
  })
  output$PCSLength_max_input<-renderUI({
    numericInput("PCSLength_max",label = NULL,value=PCSLengthmax,step=1)
  })
  output$PCSPPD_input<-renderUI({
    selectInput("PCSPPD",label = NULL,
                c("All",unique(as.character(single_pps_data$`Perpendicularity (in)`))))
  })
  #Special_1
  output$PCSNEXIV_input<-renderUI({
    selectInput("PCSNEXIV",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCSAnnealed_input<-renderUI({
    selectInput("PCSAnnealed",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCSCaliper_input<-renderUI({
    selectInput("PCSCaliper",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCSOS_input<-renderUI({
    selectInput("PCSOS",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCSMP_input<-renderUI({
    selectInput("PCSMP",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCSHT_input<-renderUI({
    selectInput("PCSHT",label = NULL,choices=c("All","yes","NA"))
  })
  #Special_2
  output$PCSSPD_input<-renderUI({
    selectInput("PCSSPD",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCSSLD_input<-renderUI({
    selectInput("PCSSLD",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCSDLN_input<-renderUI({
    selectInput("PCSDLN",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCSULT_input<-renderUI({
    selectInput("PCSULT",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCSVC_input<-renderUI({
    selectInput("PCSVC",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCSIRD_input<-renderUI({
    selectInput("PCSIRD",label = NULL,choices=c("All","yes","NA"))
  })
  
  df <- reactiveValues(data = data.frame(single_pps_data))
  
  observeEvent(input$check_button,{
    if (PCSPN_d == TRUE){
      df$data <- df$data[,1]
    }
  })
  
  output$testtable <- renderDataTable(df$data)
  

  output$mytable1 <- DT::renderDataTable({
    DT::datatable({
      data<-single_pps_data
      data},
      options = list(orderClasses = TRUE,
                     columnDefs = list(list(className = 'dt-center',
                                            targets = "_all"
                     )
                     ),
                     scrollX=TRUE,
                     scrollY=500,
                     autoWidth=TRUE)
    )
  }) 
}






# Run the application 
shinyApp(ui = ui, server = server)

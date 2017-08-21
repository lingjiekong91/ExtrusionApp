server<-function(input, output, session) {
  #create six columns in three tabs
  output$show_vars1_input<-renderUI({
    checkboxGroupInput('show_vars1', 'Columns to Show:',
                       choices = names(single_pps_data),
                       selected = c("Part Number", "Part Description", 
                                    "Resin Number", "Resin Description",
                                    "Die Size", "Tip Size",
                                    "Inner Diameter (in)", "Outer Diameter (in)",
                                    "Wall Thickness (in)", "Length (in)")
                       )
  })
  output$show_vars2_input<-renderUI({
    checkboxGroupInput('show_vars2', 'Columns to Show:',
                       choices = names(multi_pps_data),
                       selected=c("Part Number", "Part Description", 
                                  "Resin Number", "Resin Description",
                                  "Die Size (in)", "Tip Size (in)",
                                  "Inner Diameter (in)", "Outer Diameter (in)",
                                  "Inner Wall Thickness (in)", "Middle Wall Thickness (in)",
                                  "Outer Wall Thickness (in)", "Total Wall Thickness (in)", "Total Length")
                       )
  })
  output$show_vars3_input<-renderUI({
    checkboxGroupInput('show_vars3', 'Columns to Show:',
                       choices = names(tapered_pps_data), 
                       selected=c("Part Number", "Part Description", 
                         "Resin Number", "Resin Description",
                         "Die Size", "Tip Size",
                         "Proximal Inner Diameter (in)", "Proximal Inner Diameter (in)",
                         "Proximal Wall Thickness (in)",
                         "Distal Inner Diameter (in)", "Distal Outer Diameter (in)",
                         "Distal Wall Thickness (in)",
                         "Proximal Length (in)", "Transition Length (in)", 
                         "Transition Length (in)", "Total Length (in)")
                       )
  })
  output$show_vars4_input<-renderUI({
    checkboxGroupInput('show_vars4', 'Columns to Show:',
                       choices = names(single_tari_data),
                       selected = names(single_tari_data)
    )
  })
  output$show_vars5_input<-renderUI({
    checkboxGroupInput('show_vars5', 'Columns to Show:',
                       choices = names(resin_data),
                       selected = names(resin_data)
    )
  })
  output$show_vars6_input<-renderUI({
    checkboxGroupInput('show_vars6', 'Columns to Show:',
                       choices = names(screw_data),
                       selected = names(screw_data)
    )
  })
  
  #create Server side of input box for Part Catalog Single Extrusion PPS Data---Table 1
  
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
  
  
  
  #Create Server side of Part Catalog Single Extrusion PPS Data's table-----mytable1 
  output$mytable1 <- DT::renderDataTable({
    DT::datatable({
      data_PCS<-single_pps_data[, input$show_vars1]
      if(input$PCSPN!="All"){
        data_PCS<-data_PCS[data_PCS$`Part Number`==input$PCSPN,]
      }
      if(input$PCSPD!="All"){
        data_PCS<-data_PCS[data_PCS$`Part Description`==input$PCSPD,]
      }
      if(input$PCSRN!="All"){
        data_PCS<-data_PCS[data_PCS$`Resin Number`==input$PCSRN,]
      }
      if(input$PCSRD!="All"){
        data_PCS<-data_PCS[data_PCS$`Resin Descriptionr`==input$PCSRD,]
      }
      if(input$PCSPPSN!="All"){
        data_PCS<-data_PCS[data_PCS$`PPS Number`==input$PCSPPSN,]
      }
      if(input$PCSDS_min!=PCSDSmin || input$PCSDS_max!=PCSDSmax){
        data_PCS<-data_PCS[data_PCS$`Die Size (in)`>=input$PCSDS_min & data_PCS$`Die Size (in)`<=input$PCSDS_max,]
      }
      if(input$PCSDLL!="All"){
        data_PCS<-data_PCS[data_PCS$`Die Land Length (in)`==input$PCSDLL,]
      }
      if(input$PCSTS_min!=PCSTSmin || input$PCSTS_max!=PCSTSmax){
        data_PCS<-data_PCS[data_PCS$`Die Size (in)`>=input$PCSTS_min & data_PCS$`Die Size (in)`<=input$PCSTS_max,]
      }
      if(input$PCSTLL!="All"){
        data_PCS<-data_PCS[data_PCS$`Tip Land Length (in)`==input$PCSTLL,]
      }      
      if(input$PCSSP!="All"){
        data_PCS<-data_PCS[data_PCS$`Screw Print`==input$PCSSP,]
      }
      if(input$PCSFT_min!=PCSFTmin || input$PCSFT_max!=PCSFTmax){
        data_PCS<-data_PCS[data_PCS$`Feedthroat Temperature  F`>=input$PCSFT_min & data_PCS$`Feedthroat Temperature  F`<=input$PCSFT_max,]
      }
      if(input$PCSBZT1_min!=PCSBZT1min || input$PCSBZT1_max!=PCSBZT1max){
        data_PCS<-data_PCS[data_PCS$`Barrel Zone 1 Temperature  F`>=input$PCSBZT1_min & data_PCS$`Barrel Zone 1 Temperature  F`<=input$PCSBZT1_max,]
      }
      if(input$PCSBZT2_min!=PCSBZT2min || input$PCSBZT2_max!=PCSBZT2max){
        data_PCS<-data_PCS[data_PCS$`Barrel Zone 2 Temperature  F`>=input$PCSBZT2_min & data_PCS$`Barrel Zone 2 Temperature  F`<=input$PCSBZT2_max,]
      }
      if(input$PCSBZT3_min!=PCSBZT3min || input$PCSBZT3_max!=PCSBZT3max){
        data_PCS<-data_PCS[data_PCS$`Barrel Zone 3 Temperature  F`>=input$PCSBZT3_min & data_PCS$`Barrel Zone 3 Temperature  F`<=input$PCSBZT3_max,]
      }
      
      
      
      if(input$PCSCT_min!=PCSCTmin || input$PCSCT_max!=PCSCTmax){
        data_PCS<-data_PCS[data_PCS$`Clamp Temperature  F`>=input$PCSCT_min & data_PCS$`Clamp Temperature  F`<=input$PCSCT_max,]
      }
      if(input$PCSAT_min!=PCSATmin || input$PCSAT_max!=PCSATmax){
        data_PCS<-data_PCS[data_PCS$`Adapter Temperature  F`>=input$PCSAT_min & data_PCS$`Adapter Temperature  F`<=input$PCSAT_max,]
      }
      if(input$PCSDT1_min!=PCSDT1min || input$PCSDT1_max!=PCSDT1max){
        data_PCS<-data_PCS[data_PCS$`Die 1 Temperature  F`>=input$PCSDT1_min & data_PCS$`Die 1 Temperature  F`<=input$PCSDT1_max,]
      }
      if(input$PCSDT2_min!=PCSDT2min || input$PCSDT2_max!=PCSDT2max){
        data_PCS<-data_PCS[data_PCS$`Die 2 Temperature  F`>=input$PCSDT2_min & data_PCS$`Die 2 Temperature  F`<=input$PCSDT2_max,]
      }
      if(input$PCSIDI_min!=PCSIDImin || input$PCSIDI_max!=PCSIDImax){
        data_PCS<-data_PCS[data_PCS$`Inner Diameter (in)`>=input$PCSIDI_min & data_PCS$`Inner Diameter (in)`<=input$PCSIDI_max,]
      }
      if(input$PCSODI_min!=PCSODImin || input$PCSODI_max!=PCSODImax){
        data_PCS<-data_PCS[data_PCS$`Outer Diameter (in)`>=input$PCSODI_min & data_PCS$`Outer Diameter (in)`<=input$PCSODI_max,]
      }
      if(input$PCSWT_min!=PCSWTmin || input$PCSWT_max!=PCSWTmax){
        data_PCS<-data_PCS[data_PCS$`Wall Thickness (in)`>=input$PCSWT_min & data_PCS$`Wall Thickness (in)`<=input$PCSWT_max,]
      }
      if(input$PCSOR_min!=PCSORmin || input$PCSOR_max!=PCSORmax){
        data_PCS<-data_PCS[data_PCS$`Out of Roundness (in)`>=input$PCSOR_min & data_PCS$`Out of Roundness (in)`<=input$PCSOR_max,]
      }
      if(input$PCSCCT_min!=PCSCCTmin || input$PCSCCT_max!=PCSCCTmax){
        data_PCS<-data_PCS[data_PCS$`Concentricity (in)`>=input$PCSCCT_min & data_PCS$`Concentricity (in)`<=input$PCSCCT_max,]
      }
      if(input$PCSLength_min!=PCSLengthmin || input$PCSLength_max!=PCSLengthmax){
        data_PCS<-data_PCS[data_PCS$`Length (in)`>=input$PCSLength_min & data_PCS$`Length (in)`<=input$PCSLength_max,]
      }
      
    
    
       
      if(input$PCSNEXIV!="All"){
        if(input$PCSNEXIV=="yes"){
          data_PCS<-data_PCS[data_PCS$`NEXIV`=="yes",]
        } else{
          data_PCS<-data_PCS[data_PCS$`NEXIV`=="",]
        }
      }
      if(input$PCSAnnealed!="All"){
        if(input$PCSAnnealed=="yes"){
          data_PCS<-data_PCS[data_PCS$`Annealed`=="yes",]
        } else{
          data_PCS<-data_PCS[data_PCS$`Annealed`=="",]
        }
      }
      if(input$PCSCaliper!="All"){
        if(input$PCSCaliper=="yes"){
          data_PCS<-data_PCS[data_PCS$`Caliper`=="yes",]
        } else{
          data_PCS<-data_PCS[data_PCS$`Caliper`=="",]
        }
      }
      if(input$PCSOS!="All"){
        if(input$PCSOS=="yes"){
          data_PCS<-data_PCS[data_PCS$`OD Sort`=="yes",]
        } else{
          data_PCS<-data_PCS[data_PCS$`OD Sort`=="",]
        }
      }
      if(input$PCSMP!="All"){
        if(input$PCSMP=="yes"){
          data_PCS<-data_PCS[data_PCS$`Melt Pump`=="yes",]
        } else{
          data_PCS<-data_PCS[data_PCS$`Melt Pump`=="",]
        }
      }
      if(input$PCSHT!="All"){
        if(input$PCSHT=="yes"){
          data_PCS<-data_PCS[data_PCS$`Hypo Tip`=="yes",]
        } else{
          data_PCS<-data_PCS[data_PCS$`Hypo Tip`=="",]
        }
      }
      if(input$PCSSPD!="All"){
        if(input$PCSSPD=="yes"){
          data_PCS<-data_PCS[data_PCS$`Sparker Die`=="yes",]
        } else{
          data_PCS<-data_PCS[data_PCS$`Sparker Die`=="",]
        }
      }
      if(input$PCSSLD!="All"){
        if(input$PCSSLD=="yes"){
          data_PCS<-data_PCS[data_PCS$`Slicking Die`=="yes",]
        } else{
          data_PCS<-data_PCS[data_PCS$`Slicking Die`=="",]
        }
      }
      if(input$PCSDLN!="All"){
        if(input$PCSDLN=="yes"){
          data_PCS<-data_PCS[data_PCS$`Delamination`=="yes",]
        } else{
          data_PCS<-data_PCS[data_PCS$`Delamination`=="",]
        }
      }
      if(input$PCSULT!="All"){
        if(input$PCSULT=="yes"){
          data_PCS<-data_PCS[data_PCS$`Ultrasonic`=="yes",]
        } else{
          data_PCS<-data_PCS[data_PCS$`Ultrasonic`=="",]
        }
      }
      if(input$PCSVC!="All"){
        if(input$PCSVC=="yes"){
          data_PCS<-data_PCS[data_PCS$`Vacuum Calibration`=="yes",]
        } else{
          data_PCS<-data_PCS[data_PCS$`Vacuum Calibration`=="",]
        }
      }
      if(input$PCSIRD!="All"){
        if(input$PCSIRD=="yes"){
          data_PCS<-data_PCS[data_PCS$`Irradiated`=="yes",]
        } else{
          data_PCS<-data_PCS[data_PCS$`Irradiated`=="",]
        }
      }
      
      data_PCS
    },
    options = list(orderClasses = TRUE, 
                   columnDefs = list(list(className = 'dt-center', 
                                          targets = "_all"
                   )
                   ),
                   scrollX=TRUE,
                   scrollY=500,
                   autoWidth=TRUE))
  })
  
  #printFun1<-renderText({input$PCSPN})
  #printFun2<-renderText({input$PCSRN})
  #printFun()<-c(printFun1,printFun2)
  # Keep it for the test using
  # output$Summary1<-renderPrint({printFun<-printFun1()  printFun })
  
  
  #create Server side of input box for Part Catalog Multi-Layered Extrusion PPS Data----table 2
  #Part Resin
  output$PCMPN_input<-renderUI({
    selectizeInput("PCMPN",label = NULL,multiple=TRUE,
                   c("All",unique(as.character(multi_pps_data$`Part Number`))),
                   selected="All")
  })
  output$PCMPD_input<-renderUI({
    selectInput("PCMPD",label = NULL,
                c("All",unique(as.character(multi_pps_data$`Part Description`))))
  })
  output$PCMRN_input<-renderUI({
    selectInput("PCMRN",label = NULL,
                c("All",unique(as.character(multi_pps_data$`Resin Number`))))
  })
  output$PCMRD_input<-renderUI({
    selectInput("PCMRD",label = NULL,
                c("All",unique(as.character(multi_pps_data$`Resin Description`))))
  })
  output$PCMPPSN_input<-renderUI({
    selectInput("PCMPPSN",label = NULL,
                c("All",unique(as.character(multi_pps_data$`PPS Number`))))
  })
  
  #Tooling
  output$PCMDS_min_input<-renderUI({
    numericInput("PCMDS_min",label = NULL,value=PCMDSmin,step=0.001)
  })
  output$PCMDS_max_input<-renderUI({
    numericInput("PCMDS_max",label = NULL,value=PCMDSmax,step=0.001)
  })
  output$PCMDLL_input<-renderUI({
    DLL_min=-1
    DLL_max=0.54
    selectInput("PCMDLL",label = NULL,c("All",unique(as.character(multi_pps_data$`Die Land Length (in)`))))
  })
  output$PCMTS_min_input<-renderUI({
    numericInput("PCMTS_min",label = NULL,value=PCMTSmin,step=0.001)
  })
  output$PCMTS_max_input<-renderUI({
    numericInput("PCMTS_max",label = NULL,value=PCMTSmax,step=0.001)
  })
  output$PCMTLL_input<-renderUI({
    TLL_min=-1
    TLL_max=0.49
    selectInput("PCMTLL",label = NULL,c("All",unique(as.character(multi_pps_data$`Tip Land Length (in)`))))
  })
  output$PCMSP_input<-renderUI({
    selectInput("PCMSP",label = NULL,
                c("All",unique(as.character(multi_pps_data$`Screw Print`))))
  })
  
  #Attributes_1
  output$PCMFT_min_input<-renderUI({
    numericInput("PCMFT_min",label = NULL,value = PCMFTmin,step=1)
  })
  output$PCMFT_max_input<-renderUI({
    numericInput("PCMFT_max",label = NULL,value=PCMFTmax,step=1)
  })
  output$PCMBZT1_min_input<-renderUI({
    numericInput("PCMBZT1_min",label = NULL,value=PCMBZT1min,step=5)
  })
  output$PCMBZT1_max_input<-renderUI({
    numericInput("PCMBZT1_max",label = NULL,value=PCMBZT1max,step=5)
  })
  output$PCMBZT2_min_input<-renderUI({
    numericInput("PCMBZT2_min",label = NULL,value=PCMBZT2min,step=5)
  })
  output$PCMBZT2_max_input<-renderUI({
    numericInput("PCMBZT2_max",label = NULL,value=PCMBZT2max,step=5)
  })
  output$PCMBZT3_min_input<-renderUI({
    numericInput("PCMBZT3_min",label = NULL,value=PCMBZT3min,step=5)
  })
  output$PCMBZT3_max_input<-renderUI({
    numericInput("PCMBZT3_max",label = NULL,value=PCMBZT3max,step=5)
  })
  
  #Attrobites_2
  output$PCMCT_min_input<-renderUI({
    numericInput("PCMCT_min",label = NULL,value=PCMCTmin,step=5)
  })
  output$PCMCT_max_input<-renderUI({
    numericInput("PCMCT_max",label = NULL,value=PCMCTmax,step=5)
  })
  output$PCMAT_min_input<-renderUI({
    numericInput("PCMAT_min",label = NULL,value=PCMATmin,step=5)
  })
  output$PCMAT_max_input<-renderUI({
    numericInput("PCMAT_max",label = NULL,value=PCMATmax,step=5)
  })
  output$PCMDT1_min_input<-renderUI({
    numericInput("PCMDT1_min",label = NULL,value=PCMDT1min,step=5)
  })
  output$PCMDT1_max_input<-renderUI({
    numericInput("PCMDT1_max",label = NULL,value=PCMDT1max,step=5)
  })
  output$PCMDT2_min_input<-renderUI({
    numericInput("PCMDT2_min",label = NULL,value=PCMDT2min,step=5)
  })
  output$PCMDT2_max_input<-renderUI({
    numericInput("PCMDT2_max",label = NULL,value=PCMDT2max,step=5)
  })
  #Temps
  output$PCMIDI_min_input<-renderUI({
    numericInput("PCMIDI_min",label = NULL,value=PCMIDImin,step=0.001)
  })
  output$PCMIDI_max_input<-renderUI({
    numericInput("PCMIDI_max",label = NULL,value=PCMIDImax,step=0.001)
  })
  output$PCMODI_min_input<-renderUI({
    numericInput("PCMODI_min",label = NULL,value=PCMODImin,step=0.001)
  })
  output$PCMODI_max_input<-renderUI({
    numericInput("PCMODI_max",label = NULL,value=PCMODImax,step=0.001)
  })
  
  output$PCMIWT_min_input<-renderUI({
    numericInput("PCMIWT_min",label = NULL,value=PCMIWTmin,step=0.001)
  })
  output$PCMIWT_max_input<-renderUI({
    numericInput("PCMIWT_max",label = NULL,value=PCMIWTmax,step=0.001)
  })
  output$PCMMWT_min_input<-renderUI({
    numericInput("PCMMWT_min",label = NULL,value=PCMMWTmin,step=0.001)
  })
  output$PCMMWT_max_input<-renderUI({
    numericInput("PCMMWT_max",label = NULL,value=PCMMWTmax,step=0.001)
  })
  output$PCMOWT_min_input<-renderUI({
    numericInput("PCMOWT_min",label = NULL,value=PCMOWTmin,step=0.001)
  })
  output$PCMOWT_max_input<-renderUI({
    numericInput("PCMOWT_max",label = NULL,value=PCMOWTmax,step=0.001)
  })
  output$PCMTWT_min_input<-renderUI({
    numericInput("PCMTWT_min",label = NULL,value=PCMTWTmin,step=0.001)
  })
  output$PCMTWT_max_input<-renderUI({
    numericInput("PCMTWT_max",label = NULL,value=PCMTWTmax,step=0.001)
  })
  
  output$PCMOR_min_input<-renderUI({
    numericInput("PCMOR_min",label = NULL,value=PCMORmin,step=0.001)
  })
  output$PCMOR_max_input<-renderUI({
    numericInput("PCMOR_max",label = NULL,value=PCMORmax,step=0.001)
  })
  output$PCMCCT_min_input<-renderUI({
    numericInput("PCMCCT_min",label = NULL,value=PCMCCTmin,step=0.0001)
  })
  output$PCMCCT_max_input<-renderUI({
    numericInput("PCMCCT_max",label = NULL,value=PCMCCTmax,step=0.0001)
  })
  output$PCMLength_min_input<-renderUI({
    numericInput("PCMLength_min",label = NULL,value=PCMLengthmin,step=1)
  })
  output$PCMLength_max_input<-renderUI({
    numericInput("PCMLength_max",label = NULL,value=PCMLengthmax,step=1)
  })
  output$PCMPPD_input<-renderUI({
    selectInput("PCMPPD",label = NULL,
                c("All",unique(as.character(multi_pps_data$`Perpendicularity (in)`))))
  })
  
  #Special_1
  output$PCMNEXIV_input<-renderUI({
    selectInput("PCMNEXIV",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCMAnnealed_input<-renderUI({
    selectInput("PCMAnnealed",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCMCaliper_input<-renderUI({
    selectInput("PCMCaliper",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCMOS_input<-renderUI({
    selectInput("PCMOS",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCMMP_input<-renderUI({
    selectInput("PCMMP",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCMHT_input<-renderUI({
    selectInput("PCMHT",label = NULL,choices=c("All","yes","NA"))
  })
  #Special_2
  output$PCMSPD_input<-renderUI({
    selectInput("PCMSPD",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCMSLD_input<-renderUI({
    selectInput("PCMSLD",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCMDLN_input<-renderUI({
    selectInput("PCMDLN",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCMULT_input<-renderUI({
    selectInput("PCMULT",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCMVC_input<-renderUI({
    selectInput("PCMVC",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCMIRD_input<-renderUI({
    selectInput("PCMIRD",label = NULL,choices=c("All","yes","NA"))
  })
  
  #Create Server side of Part Catalog Single Extrusion PPS Data's table-----mytable1 
  output$mytable2 <- DT::renderDataTable({
    DT::datatable({
      data_PCM<-multi_pps_data[, input$show_vars2]
      if(input$PCMPN!="All"){
        data_PCM<-data_PCM[data_PCM$`Part Number`==input$PCMPN,]
      }
      if(input$PCMPD!="All"){
        data_PCM<-data_PCM[data_PCM$`Part Description`==input$PCMPD,]
      }
      if(input$PCMRN!="All"){
        data_PCM<-data_PCM[data_PCM$`Resin Number`==input$PCMRN,]
      }
      if(input$PCMRD!="All"){
        data_PCM<-data_PCM[data_PCM$`Resin Descriptionr`==input$PCMRD,]
      }
      if(input$PCMPPSN!="All"){
        data_PCM<-data_PCM[data_PCM$`PPS Number`==input$PCMPPSN,]
      }
      if(input$PCMDS_min!=PCMDSmin || input$PCMDS_max!=PCMDSmax){
        data_PCM<-data_PCM[data_PCM$`Die Size (in)`>=input$PCMDS_min & data_PCM$`Die Size (in)`<=input$PCMDS_max,]
      }
      if(input$PCMDLL!="All"){
        data_PCM<-data_PCM[data_PCM$`Die Land Length (in)`==input$PCMDLL,]
      }
      if(input$PCMTS_min!=PCMTSmin || input$PCMTS_max!=PCMTSmax){
        data_PCM<-data_PCM[data_PCM$`Die Size (in)`>=input$PCMTS_min & data_PCM$`Die Size (in)`<=input$PCMTS_max,]
      }
      if(input$PCMTLL!="All"){
        data_PCM<-data_PCM[data_PCM$`Tip Land Length (in)`==input$PCMTLL,]
      }      
      if(input$PCMSP!="All"){
        data_PCM<-data_PCM[data_PCM$`Screw Print`==input$PCMSP,]
      }
      if(input$PCMFT_min!=PCMFTmin || input$PCMFT_max!=PCMFTmax){
        data_PCM<-data_PCM[data_PCM$`Feedthroat Temperature  F`>=input$PCMFT_min & data_PCM$`Feedthroat Temperature  F`<=input$PCMFT_max,]
      }
      if(input$PCMBZT1_min!=PCMBZT1min || input$PCMBZT1_max!=PCMBZT1max){
        data_PCM<-data_PCM[data_PCM$'Part Number' %in%  unique(data_PCM$'Part Number'[data_PCM$`Barrel Zone 1 Temperature  F`>=input$PCMBZT1_min & data_PCM$`Barrel Zone 1 Temperature  F`<=input$PCMBZT1_max]),]
      }
      if(input$PCMBZT2_min!=PCMBZT2min || input$PCMBZT2_max!=PCMBZT2max){
        data_PCM<-data_PCM[data_PCM$`Barrel Zone 2 Temperature  F`>=input$PCMBZT2_min & data_PCM$`Barrel Zone 2 Temperature  F`<=input$PCMBZT2_max,]
      }
      if(input$PCMBZT3_min!=PCMBZT3min || input$PCMBZT3_max!=PCMBZT3max){
        data_PCM<-data_PCM[data_PCM$`Barrel Zone 3 Temperature  F`>=input$PCMBZT3_min & data_PCM$`Barrel Zone 3 Temperature  F`<=input$PCMBZT3_max,]
      }
      if(input$PCMCT_min!=PCMCTmin || input$PCMCT_max!=PCMCTmax){
        data_PCM<-data_PCM[data_PCM$`Clamp Temperature  F`>=input$PCMCT_min & data_PCM$`Clamp Temperature  F`<=input$PCMCT_max,]
      }
      if(input$PCMAT_min!=PCMATmin || input$PCMAT_max!=PCMATmax){
        data_PCM<-data_PCM[data_PCM$`Adapter Temperature  F`>=input$PCMAT_min & data_PCM$`Adapter Temperature  F`<=input$PCMAT_max,]
      }
      if(input$PCMDT1_min!=PCMDT1min || input$PCMDT1_max!=PCMDT1max){
        data_PCM<-data_PCM[data_PCM$`Die 1 Temperature  F`>=input$PCMDT1_min & data_PCM$`Die 1 Temperature  F`<=input$PCMDT1_max,]
      }
      if(input$PCMDT2_min!=PCMDT2min || input$PCMDT2_max!=PCMDT2max){
        data_PCM<-data_PCM[data_PCM$`Die 2 Temperature  F`>=input$PCMDT2_min & data_PCM$`Die 2 Temperature  F`<=input$PCMDT2_max,]
      }
      if(input$PCMIDI_min!=PCMIDImin || input$PCMIDI_max!=PCMIDImax){
        data_PCM<-data_PCM[data_PCM$`Inner Diameter (in)`>=input$PCMIDI_min & data_PCM$`Inner Diameter (in)`<=input$PCMIDI_max,]
      }
      if(input$PCMODI_min!=PCMODImin || input$PCMODI_max!=PCMODImax){
        data_PCM<-data_PCM[data_PCM$`Outer Diameter (in)`>=input$PCMODI_min & data_PCM$`Outer Diameter (in)`<=input$PCMODI_max,]
      }
      if(input$PCMIWT_min!=PCMIWTmin || input$PCMIWT_max!=PCMIWTmax){
        data_PCM<-data_PCM[data_PCM$`Inner Wall Thickness (in)`>=input$PCMIWT_min & data_PCM$`Inner Wall Thickness (in)`<=input$PCMIWT_max,]
      }
      if(input$PCMMWT_min!=PCMMWTmin || input$PCMMWT_max!=PCMMWTmax){
        data_PCM<-data_PCM[data_PCM$`Middle Wall Thickness (in)`>=input$PCMMWT_min & data_PCM$`Middle Wall Thickness (in)`<=input$PCMMWT_max,]
      }
      if(input$PCMOWT_min!=PCMOWTmin || input$PCMOWT_max!=PCMOWTmax){
        data_PCM<-data_PCM[data_PCM$`Outer Wall Thickness (in)`>=input$PCMOWT_min & data_PCM$`Outer Wall Thickness (in)`<=input$PCMOWT_max,]
      }
      if(input$PCMTWT_min!=PCMTWTmin || input$PCMTWT_max!=PCMTWTmax){
        data_PCM<-data_PCM[data_PCM$`Total Wall Thickness (in)`>=input$PCMTWT_min & data_PCM$`Total Wall Thickness (in)`<=input$PCMTWT_max,]
      }
      if(input$PCMOR_min!=PCMORmin || input$PCMOR_max!=PCMORmax){
        data_PCM<-data_PCM[data_PCM$`Out of Roundness (in)`>=input$PCMOR_min & data_PCM$`Out of Roundness (in)`<=input$PCMOR_max,]
      }
      if(input$PCMCCT_min!=PCMCCTmin || input$PCMCCT_max!=PCMCCTmax){
        data_PCM<-data_PCM[data_PCM$`Concentricity (in)`>=input$PCMCCT_min & data_PCM$`Concentricity (in)`<=input$PCMCCT_max,]
      }
      if(input$PCMLength_min!=PCMLengthmin || input$PCMLength_max!=PCMLengthmax){
        data_PCM<-data_PCM[data_PCM$`Length (in)`>=input$PCMLength_min & data_PCM$`Length (in)`<=input$PCMLength_max,]
      }
      if(input$PCMNEXIV!="All"){
        if(input$PCMNEXIV=="yes"){
          data_PCM<-data_PCM[data_PCM$`NEXIV`=="yes",]
        } else{
          data_PCM<-data_PCM[data_PCM$`NEXIV`=="",]
        }
      }
      if(input$PCMAnnealed!="All"){
        if(input$PCMAnnealed=="yes"){
          data_PCM<-data_PCM[data_PCM$`Annealed`=="yes",]
        } else{
          data_PCM<-data_PCM[data_PCM$`Annealed`=="",]
        }
      }
      if(input$PCMCaliper!="All"){
        if(input$PCMCaliper=="yes"){
          data_PCM<-data_PCM[data_PCM$`Caliper`=="yes",]
        } else{
          data_PCM<-data_PCM[data_PCM$`Caliper`=="",]
        }
      }
      if(input$PCMOS!="All"){
        if(input$PCMOS=="yes"){
          data_PCM<-data_PCM[data_PCM$`OD Sort`=="yes",]
        } else{
          data_PCM<-data_PCM[data_PCM$`OD Sort`=="",]
        }
      }
      if(input$PCMMP!="All"){
        if(input$PCMMP=="yes"){
          data_PCM<-data_PCM[data_PCM$`Melt Pump`=="yes",]
        } else{
          data_PCM<-data_PCM[data_PCM$`Melt Pump`=="",]
        }
      }
      if(input$PCMHT!="All"){
        if(input$PCMHT=="yes"){
          data_PCM<-data_PCM[data_PCM$`Hypo Tip`=="yes",]
        } else{
          data_PCM<-data_PCM[data_PCM$`Hypo Tip`=="",]
        }
      }
      if(input$PCMSPD!="All"){
        if(input$PCMSPD=="yes"){
          data_PCM<-data_PCM[data_PCM$`Sparker Die`=="yes",]
        } else{
          data_PCM<-data_PCM[data_PCM$`Sparker Die`=="",]
        }
      }
      if(input$PCMSLD!="All"){
        if(input$PCMSLD=="yes"){
          data_PCM<-data_PCM[data_PCM$`Slicking Die`=="yes",]
        } else{
          data_PCM<-data_PCM[data_PCM$`Slicking Die`=="",]
        }
      }
      if(input$PCMDLN!="All"){
        if(input$PCMDLN=="yes"){
          data_PCM<-data_PCM[data_PCM$`Delamination`=="yes",]
        } else{
          data_PCM<-data_PCM[data_PCM$`Delamination`=="",]
        }
      }
      if(input$PCMULT!="All"){
        if(input$PCMULT=="yes"){
          data_PCM<-data_PCM[data_PCM$`Ultrasonic`=="yes",]
        } else{
          data_PCM<-data_PCM[data_PCM$`Ultrasonic`=="",]
        }
      }
      if(input$PCMVC!="All"){
        if(input$PCMVC=="yes"){
          data_PCM<-data_PCM[data_PCM$`Vacuum Calibration`=="yes",]
        } else{
          data_PCM<-data_PCM[data_PCM$`Vacuum Calibration`=="",]
        }
      }
      if(input$PCMIRD!="All"){
        if(input$PCMIRD=="yes"){
          data_PCM<-data_PCM[data_PCM$`Irradiated`=="yes",]
        } else{
          data_PCM<-data_PCM[data_PCM$`Irradiated`=="",]
        }
      }
      
      data_PCM
    },
    options = list(orderClasses = TRUE, 
                   columnDefs = list(list(className = 'dt-center', 
                                          targets = "_all"
                   )
                   ),
                   scrollX=TRUE,
                   scrollY=500,
                   autoWidth=TRUE))
  })
  
  
  #
  
  #Create Server side of input box for Part Catalog Tapered Extrusion PPS Data---table 3
  output$PCTPN_input<-renderUI({
    selectizeInput("PCTPN","Part Number",
                   c("All",unique(as.character(tapered_pps_data$`Part Number`))),
                   selected="All")
  })
  output$PCTPD_input<-renderUI({
    selectInput("PCTPD","Part Description",
                c("All",unique(as.character(tapered_pps_data$`Part Description`))))
  })
  output$PCTRN_input<-renderUI({
    selectInput("PCTRN","Resin Number",
                c("All",unique(as.character(tapered_pps_data$`Resin Number`))))
  })
  output$PCTRD_input<-renderUI({
    selectInput("PCTRD","Resin Description",
                c("All",unique(as.character(tapered_pps_data$`Resin Description`))))
  })
  output$PCTPPSN_input<-renderUI({
    selectInput("PCTPPSN","PPS Number",
                c("All",unique(as.character(tapered_pps_data$`PPS Number` ))))
  })
  
  output$PCTDS_min_input<-renderUI({
    numericInput("PCTDS_min",label = NULL,value=PCTDSmin,step=0.001)
  })
  output$PCTDS_max_input<-renderUI({
    numericInput("PCTDS_max",label = NULL,value=PCTDSmax,step=0.001)
  })
  
  output$PCTDLL_input<-renderUI({
    DLL_min=-1
    DLL_max=0.54
    sliderInput("PCTDLL",label = NULL,min=DLL_min,max=DLL_max,value=c(DLL_min,DLL_max))
  })
  output$PCTTS_input<-renderUI({
    TS_min=-1
    TS_max=0.49
    sliderInput("PCTTS",label = NULL,min=TS_min,max=TS_max,value=c(TS_min,TS_max))
  })
  output$PCTTLL_input<-renderUI({
    TLL_min=-1
    TLL_max=0.49
    sliderInput("PCTTLL",label = NULL,min=TLL_min,max=TLL_max,value=c(TLL_min,TLL_max))
  })
  
  output$PCTSP_input<-renderUI({
    selectInput("PCTSP",label = NULL,
                c("All",unique(as.character(tapered_pps_data$`Screw Print`))))
  })
  
  
  output$PCTFT_input<-renderUI({
    FT_min=1
    FT_max=2
    sliderInput("PCTFT","Feedthroat Temperature F",min=FT_min,max=FT_max,value=c(FT_min,FT_max),sep="",round=-4)
  })
  output$PCTBZT1_input<-renderUI({

    selectInput("PCTBZT1","Barrel Zone 1 Temperature F",
                c("All",unique(as.character(tapered_pps_data$`Barrel Zone 1 Temperature  F`))))
  })
  output$PCTBZT2_input<-renderUI({
    selectInput("PCTBZT2","Barrel Zone 2 Temperature F",
                c("All",unique(as.character(tapered_pps_data$`Barrel Zone 2 Temperature  F`))))
  })
  output$PCTBZT3_input<-renderUI({
    selectInput("PCTBZT3","Barrel Zone 3 Temperature F",
                c("All",unique(as.character(tapered_pps_data$`Barrel Zone 3 Temperature  F`))))
  })
  output$PCTDT1_input<-renderUI({
    DT1_min=0.0009
    DT1_max=0.353
    sliderInput("PCTDT1","Die 1 Temperature F",min=DT1_min,max=DT1_max,value=c(DT1_min,DT1_max),sep="",round=-4)
  })
  output$PCTDT2_input<-renderUI({
    DT2_min=0.0009
    DT2_max=0.353
    sliderInput("PCTDT2","Die 2 Temperature F",min=DT2_min,max=DT2_max,value=c(DT2_min,DT2_max),sep="",round=-4)
  })
  
  output$PCTPID_input<-renderUI({
    PID_min=-1
    PID_max=0.54
    sliderInput("PCTPID","Proximal Inner Diameter (in)",min=PID_min,max=PID_max,value=c(PID_min,PID_max))
  })
  output$PCTPOD_input<-renderUI({
    POD_min=-1
    POD_max=0.54
    sliderInput("PCTPOD","Proximal Outer Diameter (in)",min=POD_min,max=POD_max,value=c(POD_min,POD_max))
  })
  output$PCTPWT_input<-renderUI({
    PWT_min=-1
    PWT_max=0.49
    sliderInput("PCTPWT","Proximal Wall Thickness (in)",min=PWT_min,max=PWT_max,value=c(PWT_min,PWT_max))
  })
  output$PCTPOR_input<-renderUI({
    POR_min=-1
    POR_max=0.49
    sliderInput("PCTPOR","Proximal Out of Roundness (in)",min=POR_min,max=POR_max,value=c(POR_min,POR_max))
  })
  output$PCTPC_input<-renderUI({
    PC_min=-1
    PC_max=0.49
    sliderInput("PCTPC","Proximal Concentricity (in)",min=PC_min,max=PC_max,value=c(PC_min,PC_max))
  })
  
  output$PCTDID_input<-renderUI({
    DID_min=0.0009
    DID_max=0.353
    sliderInput("PCTDID","Distal Inner Diameter(in)",min=DID_min,max=DID_max,value=c(DID_min,DID_max),sep="",round=-4)
  })
  output$PCTDOD_input<-renderUI({
    DOD_min<-(min(tapered_pps_data$`Distal Outer Diameter (in)`))
    DOD_max<-(max(tapered_pps_data$`Distal Outer Diameter (in)`))
    sliderInput("PCTDOD","Distal Outer Diameter(in)",min=DOD_min,max=DOD_max,value=c(DOD_min,DOD_max),sep="",round=-4)
  })
  output$PCTDWT_input<-renderUI({
    DWT_min=0.0005
    DWT_max=0.053
    sliderInput("PCTDWT","Distal Wall Thickness(in)",min=DWT_min,max=DWT_max,value=c(DWT_min,DWT_max),sep="",round=-4)
  })
  output$PCTDOR_input<-renderUI({
    DOR_min=0
    DOR_max=0.01
    sliderInput("PCTDOR","Distal Out of Roundness(in)",min=DOR_min,max=DOR_max,value=c(DOR_min,DOR_max),sep="",round=-4)
  })
  output$PCTDC_input<-renderUI({
    DC_min=1
    DC_max=2
    sliderInput("PCTDC","Distal Concentricity(in)",min=DC_min,max=DC_max,value=c(DC_min,DC_max),sep="",round=-4)
  })
  
  output$PCTPL_input<-renderUI({
    PL_min=0.015
    PL_max=1
    sliderInput("PCTPL","Proximal Length (in)",min=PL_min,max=PL_max,value=c(PL_min,PL_max),sep="",round=-4)
  })
  output$PCTTL_input<-renderUI({
    TL_min=0.015
    TL_max=1
    sliderInput("PCTTL","Transition Length (in)",min=TL_min,max=TL_max,value=c(TL_min,TL_max),sep="",round=-4)
  })
  output$PCTDL_input<-renderUI({
    DL_min=0.015
    DL_max=1
    sliderInput("PCTDL","Distal Length (in)",min=DL_min,max=DL_max,value=c(DL_min,DL_max),sep="",round=-4)
  })
  output$PCTTL_input<-renderUI({
    TL_min=0.015
    TL_max=1
    sliderInput("PCTTL","Total Length (in)",min=TL_min,max=TL_max,value=c(TL_min,TL_max),sep="",round=-4)
  }) 
  output$PCTPPD_input<-renderUI({
    selectInput("PCTPPD","Perpendicularity (in)",
                c("All",unique(as.character(tapered_pps_data$`Perpendicularity (in)`))))
  })
  
  output$PCTNEXIV_input<-renderUI({
    selectInput("PCTNEXIV","NEXIV",choices=c("All","yes","No"))
  })
  output$PCTAnnealed_input<-renderUI({
    selectInput("PCTAnnealed","Annealed",choices=c("All","yes","No"))
  })
  output$PCTCaliper_input<-renderUI({
    selectInput("PCTCaliper","Caliper",choices=c("All","yes","No"))
  })
  output$PCTOS_input<-renderUI({
    selectInput("PCTOS","OD Sort",choices=c("All","yes","No"))
  })
  output$PCTMP_input<-renderUI({
    selectInput("PCTMP","Melt Pump",choices=c("All","yes","No"))
  })
  
  output$PCTHT_input<-renderUI({
    selectInput("PCTHT","Hypo Tip",choices=c("All","yes","No"))
  })
  output$PCTSPD_input<-renderUI({
    selectInput("PCTSPD","Spark Die",choices=c("All","yes","No"))
  })
  output$PCTSLD_input<-renderUI({
    selectInput("PCTSLD","Slicking Die",choices=c("All","yes","No"))
  })
  output$PCTDLN_input<-renderUI({
    selectInput("PCTDLN","Delamination",choices=c("All","yes","No"))
  })
  output$PCTULT_input<-renderUI({
    selectInput("PCTULT","Ultrasonic",choices=c("All","yes","No"))
  })
  output$PCTVC_input<-renderUI({
    selectInput("PCSVC","Vacuum Calibration",choices=c("All","yes","No"))
  })
  output$PCTIRD_input<-renderUI({
    selectInput("PCTIRD","Irradiated",choices=c("All","yes","No"))
  })
  
  
  
  #create Server side of Tapered Extrusion PPS Data-----mytable3
  output$mytable3 <- DT::renderDataTable({
    DT::datatable({
      data_PCT<-tapered_pps_data[, input$show_vars3]
      if(input$PCTPN!="All"){
        data_PCT<-data_PCT[data_PCT$`Part Number`==input$PCTPN,]
      }
      if(input$PCTPD!="All"){
        data_PCT<-data_PCT[data_PCT$`Part Description`==input$PCTPD,]
      }
      if(input$PCTRN!="All"){
        data_PCT<-data_PCT[data_PCT$`Resin Number`==input$PCTRN,]
      }
      if(input$PCTRD!="All"){
        data_PCT<-data_PCT[data_PCT$`Resin Description`==input$PCTRD,]
      }
      if(input$PCTPPSN!="All"){
        data_PCT<-data_PCT[data_PCT$`PPS Number`==input$PCTPPSN,]
      }
      if(input$PCTDS_min!=PCTDSmin || input$PCTDS_max!=PCTDSmax){
        data_PCT<-data_PCT[data_PCT$`Die Size (in)`>=input$PCTDS_min & data_PCT$`Die Size (in)`<=input$PCTDS_max,]
      }
      
      if(input$PCTBZT1!="All"){
        data_PCT<-data_PCT[data_PCT$`Barrel Zone 1 Temperature  F`==input$PCTBZT1,]
      }
      if(input$PCTBZT2!="All"){
        data_PCT<-data_PCT[data_PCT$`Barrel Zone 2 Temperature  F`==input$PCTBZT2,]
      }
      if(input$PCTBZT3!="All"){
        data_PCT<-data_PCT[data_PCT$`Barrel Zone 3 Temperature  F`==input$PCTBZT3,]
      }
      
      data_PCT
    },
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
  

  #Create Server side of input box for Output MES---table 4
  output$OPMMN_input<-renderUI({
    selectInput("OPMMN","Material Number",c("All",unique(as.character(single_tari_data$`Material Number`))))
  })
  output$OPMBN_input<-renderUI({
    selectInput("OPMBN","SAP Batch Number",c("All",unique(as.character(single_tari_data$`SAP Batch Number`))))
    
  })
  output$OPMSN_input<-renderUI({
    selectInput("OPMSN","SWR Number",c("All",unique(as.character(single_tari_data$`SWR Number`))))
    
  })
  output$OPMOI_input<-renderUI({
    selectInput("OPMOI","Operator ID",c("All",unique(as.character(single_tari_data$`Operator ID`))))
    
  })
  output$OPMDR_input<-renderUI({
    dateRangeInput("OPMDR","Start Date Range",start=Time_Start,end=Time_End)
    
  })
  #create Server side of Output MES table-----mytable4 
  output$mytable4 <- DT::renderDataTable({
    DT::datatable({
      if (input$PCSPN=="All"){
        data_OPM<-single_tari_data[,input$show_vars4]
      }
      else {data_OPM<-single_tari_data[single_tari_data$`Material Number`==input$PCSPN,input$show_vars4]
      }
      
      if(input$OPMMN!="All"){
        data_OPM<-data_OPM[data_OPM$`Material Number`==input$OPMMN,]
      }
      if(input$OPMBN!="All"){
        data_OPM<-data_OPM[data_OPM$`SAP Batch Number`==input$OPMBN,]
      }
      if(input$OPMSN!="All"){
        data_OPM<-data_OPM[data_OPM$`SWR Number`==input$OPMSN,]
      }
      if(input$OPMOI!="All"){
        data_OPM<-data_OPM[data_OPM$`Operator ID`==input$OPMOI,]
      }
      if(input$OPMDR[1]!=Time_Start || input$OPMDR[2]!=Time_End){
        data_OPM<-data_OPM[(data_OPM$`Start Date`<=input$OPMDR[2] & data_OPM$`Start Date`>=input$OPMDR[1]),]
      }
      data_OPM
    },
    options = list(orderClasses = TRUE, 
                   columnDefs = list(list(className = 'dt-center', 
                                          targets = "_all"
                   )
                   ),
                   scrollX=TRUE,
                   scrollY=500,
                   autoWidth=TRUE) )
  })
  
  output$mytable5 <- DT::renderDataTable({
    DT::datatable(resin_data[, input$show_vars5], 
                  options = list(orderClasses = TRUE, 
                                 columnDefs = list(list(className = 'dt-center', 
                                                        targets = "_all"
                                 )
                                 ),
                                 scrollX=TRUE,
                                 scrollY=500,
                                 autoWidth=TRUE), 
                  filter = "top")
  })
  
  output$mytable6 <- DT::renderDataTable({
    DT::datatable(screw_data[, input$show_vars6], 
                  options = list(orderClasses = TRUE, 
                                 columnDefs = list(list(className = 'dt-center', 
                                                        targets = "_all"
                                 )
                                 ),
                                 scrollX=TRUE,
                                 scrollY=500,
                                 autoWidth=TRUE), 
                  filter = "top")
  })
  #
  
}#end server

# Run the application 
shinyApp(ui = ui, server = server)

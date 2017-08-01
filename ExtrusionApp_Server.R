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
  output$PCSCT_input<-renderUI({
    CT_min=-1
    CT_max=0.49
    sliderInput("PCSCT",label = NULL,min=CT_min,max=CT_max,value=c(CT_min,CT_max))
  })
  output$PCSAT_input<-renderUI({
    AT_min=-1
    AT_max=0.49
    sliderInput("PCSAT",label = NULL,min=AT_min,max=AT_max,value=c(AT_min,AT_max))
  })
  output$PCSDT1_input<-renderUI({
    DT1_min=-1
    DT1_max=0.49
    sliderInput("PCSDT1",label = NULL,min=DT1_min,max=DT1_max,value=c(DT1_min,DT1_max))
  })
  output$PCSDT2_input<-renderUI({
    DT2_min=-1
    DT2_max=0.49
    sliderInput("PCSDT2",label = NULL,min=DT2_min,max=DT2_max,value=c(DT2_min,DT2_max))
  })
#Temps
  output$PCSIDI_input<-renderUI({
    IDI_min=0.0009
    IDI_max=0.353
    sliderInput("PCSIDI",label = NULL,min=IDI_min,max=IDI_max,value=c(IDI_min,IDI_max),sep="",round=-4)
  })
  output$PCSODI_input<-renderUI({
    ODI_min=0.0133
    ODI_max=0.495
    sliderInput("PCSODI",label = NULL,min=ODI_min,max=ODI_max,value=c(ODI_min,ODI_max),sep="",round=-4)
  })
  output$PCSWT_input<-renderUI({
    WT_min=0.0005
    WT_max=0.053
    sliderInput("PCSWT",label = NULL,min=WT_min,max=WT_max,value=c(WT_min,WT_max),sep="",round=-4)
  })
  output$PCSOR_input<-renderUI({
    OR_min=0
    OR_max=0.01
    sliderInput("PCSOR",label = NULL,min=OR_min,max=OR_max,value=c(OR_min,OR_max),sep="",round=-4)
  })
  output$PCSCCT_input<-renderUI({
    CCT_min=1
    CCT_max=2
    sliderInput("PCSCCT",label = NULL,min=CCT_min,max=CCT_max,value=c(CCT_min,CCT_max),sep="",round=-4)
  })
  output$PCSLength_input<-renderUI({
    Length_min=0.015
    Length_max=1
    sliderInput("PCSLength",label = NULL,min=Length_min,max=Length_max,value=c(Length_min,Length_max),sep="",round=-4)
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
      if(input$PCSBZT1_min!=PCSBZT1min || input$PCSBZT1_max!=PCSBZT1max){
        data_PCS<-data_PCS[data_PCS$`Barrel Zone 2 Temperature  F`>=input$PCSBZT2_min & data_PCS$`Barrel Zone 2 Temperature  F`<=input$PCSBZT2_max,]
      }
      if(input$PCSBZT3_min!=PCSBZT3min || input$PCSBZT3_max!=PCSBZT3max){
        data_PCS<-data_PCS[data_PCS$`Barrel Zone 3 Temperature  F`>=input$PCSBZT3_min & data_PCS$`Barrel Zone 3 Temperature  F`<=input$PCSBZT3_max,]
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
  output$PCMPN_input<-renderUI({
    selectizeInput("PCMPN","Part Number",
                   c("All",unique(as.character(multi_pps_data$`Part Number`))),
                   selected="All")
  })
  output$PCMPD_input<-renderUI({
    selectizeInput("PCMPD","Part Description",
                   c("All",unique(as.character(multi_pps_data$`Part Description`))),
                   selected="All")
  })
  output$PCMRN_input<-renderUI({
    selectizeInput("PCMRN","Resin Number",
                   c("All",unique(as.character(multi_pps_data$`Resin Number`))),
                   selected="All")
  })
  output$PCMRD_input<-renderUI({
    selectizeInput("PCMRD","Resin Description",
                   c("All",unique(as.character(multi_pps_data$`Resin Description`))),
                   selected="All")
  })
  output$PCMDS_input<-renderUI({
    selectizeInput("PCMDS","Die Size",
                   c("All",unique(as.character(multi_pps_data$`Die Size (in)`))),
                   selected="All")
  })
  output$PCMTS_input<-renderUI({
    selectizeInput("PCMTS","Tip Size",
                   c("All",unique(as.character(multi_pps_data$`Tip Size (in)`))),
                   selected="All")
  })
  output$PCMFT_input<-renderUI({
    FT_min=1
    FT_max=2
    sliderInput("PCMFT","Feedthroat Temperature F",min=FT_min,max=FT_max,value=c(FT_min,FT_max),sep="",round=-4)
  })
  output$PCMBZT1_input<-renderUI({
    selectInput("PCMBZT1","Barrel Zone 1 Temperature F",
                c("All",unique(as.character(multi_pps_data$`Barrel Zone 1 Temperature  F`))))
  })
  output$PCMBZT2_input<-renderUI({
    selectInput("PCMBZT2","Barrel Zone 2 Temperature F",
                c("All",unique(as.character(multi_pps_data$`Barrel Zone 2 Temperature  F`))))
  })
  output$PCMBZT3_input<-renderUI({
    selectInput("PCMBZT3","Barrel Zone 3 Temperature F",
                c("All",unique(as.character(multi_pps_data$`Barrel Zone 3 Temperature  F`))))
  })
  output$PCMID_input<-renderUI({
    selectizeInput("PCMID","Inner Diameter (in)",
                   c("All",unique(as.character(multi_pps_data$`Inner Diameter (in)`))),
                   selected="All")
  })
  output$PCMOD_input<-renderUI({
    selectizeInput("PCMOD","Outer Diameter (in)",
                   c("All",unique(as.character(multi_pps_data$`Outer Diameter (in)`))),
                   selected="All")
  })
  output$PCMIWT_input<-renderUI({
    selectizeInput("PCMIWT","Inner Wall Thickness (in)",
                   c("All",unique(as.character(multi_pps_data$`Inner Wall Thickness (in)`))),
                   selected="All")
  })
  output$PCMMWT_input<-renderUI({
    selectizeInput("PCMMWT","Middle Wall Thickness (in)",
                   c("All",unique(as.character(multi_pps_data$`Middle Wall Thickness (in)`))),
                   selected="All")
  })
  output$PCMOWT_input<-renderUI({
    selectizeInput("PCMOWT","Outer Wall Thickness (in)",
                   c("All",unique(as.character(multi_pps_data$`Outer Wall Thickness (in)`))),
                   selected="All")
  })
  output$PCMTWT_input<-renderUI({
    selectizeInput("PCMTWT","Total Wall Thickness (in)",
                   c("All",unique(as.character(multi_pps_data$`Total Wall Thickness (in)`))),
                   selected="All")
  })
  output$PCMTL_input<-renderUI({
    selectizeInput("PCMTL","Total Length",
                   c("All",unique(as.character(multi_pps_data$`Total Length`))),
                   selected="All")
  })
  
  #Create Server side of Part Catalog Multi-Layered Extrusion PPS Data's table-----mytable2  
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
        data_PCM<-data_PCM[data_PCM$`Resin Description`==input$PCMRD,]
      }
      if(input$PCMDS!="All"){
        data_PCM<-data_PCM[data_PCM$`Die Size (in)`==input$PCMDS,]
      }
      if(input$PCMTS!="All"){
        data_PCM<-data_PCM[data_PCM$`Tip Size (in)`==input$PCMTS,]
      }
      if(input$PCMBZT1!="All"){
        PCMBZT1_v<-input$PCMBZT1
        PCMPN_v<-unique(data_PCM$`Part Number`[data_PCM$`Barrel Zone 1 Temperature  F`==PCMBZT1_v])
        data_PCM<-data_PCM[data_PCM$`Part Number`==PCMPN_v,]
      }
 
      if(input$PCMID!="All"){
        data_PCM<-data_PCM[data_PCM$`Inner Diameter (in)`==input$PCMID,]
      }
      if(input$PCMOD!="All"){
        data_PCM<-data_PCM[data_PCM$`Outer Diameter (in)`==input$PCMOD,]
      }
      if(input$PCMIWT!="All"){
        data_PCM<-data_PCM[data_PCM$`Inner Wall Thickness (in)`==input$PCMIWT,]
      }
      if(input$PCMMWT!="All"){
        data_PCM<-data_PCM[data_PCM$`Middle Wall Thickness (in)`==input$PCMMWT,]
      }
      if(input$PCMOWT!="All"){
        data_PCM<-data_PCM[data_PCM$`Outer Wall Thickness (in)`==input$PCMOWT,]
      }
      if(input$PCMTWT!="All"){
        data_PCM<-data_PCM[data_PCM$`Total Wall Thickness (in)`==input$PCMTWT,]
      }
      if(input$PCMTL!="All"){
        data_PCM<-data_PCM[data_PCM$`Total Length`==input$PCMTL,]
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

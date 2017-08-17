#Multi Extrusion Table Part Codes, saved for later use

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
  data_PCM<-data_PCM[data_PCM$'Part Number' %in%  unique(data_PCM$'Part Number'[data_PCM$`Barrel Zone 2 Temperature  F`>=input$PCMBZT2_min & data_PCM$`Barrel Zone 2 Temperature  F`<=input$PCMBZT2_max]),]
}
if(input$PCMBZT3_min!=PCMBZT3min || input$PCMBZT3_max!=PCMBZT3max){
  data_PCM<-data_PCM[data_PCM$'Part Number' %in%  unique(data_PCM$'Part Number'[data_PCM$`Barrel Zone 3 Temperature  F`>=input$PCMBZT3_min & data_PCM$`Barrel Zone 3 Temperature  F`<=input$PCMBZT3_max]),]
}
if(input$PCMCT_min!=PCMCTmin || input$PCMCT_max!=PCMCTmax){
  data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`Clamp Temperature  F`>=input$PCMCT_min & data_PCM$`Clamp Temperature  F`<=input$PCMCT_max]),]
}
if(input$PCMAT_min!=PCMATmin || input$PCMAT_max!=PCMATmax){
  data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`Adapter Temperature  F`>=input$PCMAT_min & data_PCM$`Adapter Temperature  F`<=input$PCMAT_max]),]
}
if(input$PCMDT1_min!=PCMDT1min || input$PCMDT1_max!=PCMDT1max){
  data_PCM<-data_PCM[data_PCM$'Part Number'%in% unique(data_PCM$'Part Number'[data_PCM$`Die 1 Temperature  F`>=input$PCMDT1_min & data_PCM$`Die 1 Temperature  F`<=input$PCMDT1_max]),]
}
if(input$PCMDT2_min!=PCMDT2min || input$PCMDT2_max!=PCMDT2max){
  data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`Die 2 Temperature  F`>=input$PCMDT2_min & data_PCM$`Die 2 Temperature  F`<=input$PCMDT2_max]),]
}
if(input$PCMIDI_min!=PCMIDImin || input$PCMIDI_max!=PCMIDImax){
  data_PCM<-data_PCM[data_PCM$'Part Number'%in% unique(ddata_PCM$'Part Number'[data_PCM$`Inner Diameter (in)`>=input$PCMIDI_min & data_PCM$`Inner Diameter (in)`<=input$PCMIDI_max]),]
}
if(input$PCMODI_min!=PCMODImin || input$PCMODI_max!=PCMODImax){
  data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`Outer Diameter (in)`>=input$PCMODI_min & data_PCM$`Outer Diameter (in)`<=input$PCMODI_max]),]
}
if(input$PCMIWT_min!=PCMIWTmin || input$PCMIWT_max!=PCMIWTmax){
  data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`Inner Wall Thickness (in)`>=input$PCMIWT_min & data_PCM$`Inner Wall Thickness (in)`<=input$PCMIWT_max]),]
}
if(input$PCMMWT_min!=PCMMWTmin || input$PCMMWT_max!=PCMMWTmax){
  data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`Middle Wall Thickness (in)`>=input$PCMMWT_min & data_PCM$`Middle Wall Thickness (in)`<=input$PCMMWT_max]),]
}
if(input$PCMOWT_min!=PCMOWTmin || input$PCMOWT_max!=PCMOWTmax){
  data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`Outer Wall Thickness (in)`>=input$PCMOWT_min & data_PCM$`Outer Wall Thickness (in)`<=input$PCMOWT_max]),]
}
if(input$PCMTWT_min!=PCMTWTmin || input$PCMTWT_max!=PCMTWTmax){
  data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number' [data_PCM$`Total Wall Thickness (in)`>=input$PCMTWT_min & data_PCM$`Total Wall Thickness (in)`<=input$PCMTWT_max]),]
}
if(input$PCMOR_min!=PCMORmin || input$PCMOR_max!=PCMORmax){
  data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`Out of Roundness (in)`>=input$PCMOR_min & data_PCM$`Out of Roundness (in)`<=input$PCMOR_max]),]
}
if(input$PCMCCT_min!=PCMCCTmin || input$PCMCCT_max!=PCMCCTmax){
  data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`Concentricity (in)`>=input$PCMCCT_min & data_PCM$`Concentricity (in)`<=input$PCMCCT_max]),]
}
if(input$PCMLength_min!=PCMLengthmin || input$PCMLength_max!=PCMLengthmax){
  data_PCM<-data_PCM[data_PCM$`Length (in)` %in% unique(data_PCM$'Part Number'[data_PCM$`Length (in)`>=input$PCMLength_min & data_PCM$`Length (in)`<=input$PCMLength_max]),]
}
if(input$PCMToLength_min!=PCMToLengthmin || input$PCMToLength_max!=PCMToLengthmax){
  data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`Total Length`>=input$PCMToLength_min & data_PCM$`Total Length`<=input$PCMToLength_max]),]
}
if(input$PCMNEXIV!="All"){
  if(input$PCMNEXIV=="yes"){
    data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`NEXIV`=="yes"]),]
  } else{
    data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`NEXIV`==""]),]
  }
}
if(input$PCMAnnealed!="All"){
  if(input$PCMAnnealed=="yes"){
    data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`Annealed`=="yes"]),]
  } else{
    data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`Annealed`==""]),]
  }
}
if(input$PCMCaliper!="All"){
  if(input$PCMCaliper=="yes"){
    data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`Caliper`=="yes"]),]
  } else{
    data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`Caliper`==""]),]
  }
}
if(input$PCMOS!="All"){
  if(input$PCMOS=="yes"){
    data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`OD Sort`=="yes"]),]
  } else{
    data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`OD Sort`==""]),]
  }
}
if(input$PCMMP!="All"){
  if(input$PCMMP=="yes"){
    data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`Melt Pump`=="yes"]),]
  } else{
    data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`Melt Pump`==""]),]
  }
}
if(input$PCMHT!="All"){
  if(input$PCMHT=="yes"){
    data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`Hypo Tip`=="yes"]),]
  } else{
    data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`Hypo Tip`==""]),]
  }
}
if(input$PCMSPD!="All"){
  if(input$PCMSPD=="yes"){
    data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`Sparker Die`=="yes"]),]
  } else{
    data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`Sparker Die`==""]),]
  }
}
if(input$PCMSLD!="All"){
  if(input$PCMSLD=="yes"){
    data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`Slicking Die`=="yes"]),]
  } else{
    data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`Slicking Die`==""]),]
  }
}
if(input$PCMDLN!="All"){
  if(input$PCMDLN=="yes"){
    data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`Delamination`=="yes"]),]
  } else{
    data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`Delamination`==""]),]
  }
}
if(input$PCMULT!="All"){
  if(input$PCMULT=="yes"){
    data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`Ultrasonic`=="yes"]),]
  } else{
    data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`Ultrasonic`==""]),]
  }
}
if(input$PCMVC!="All"){
  if(input$PCMVC=="yes"){
    data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`Vacuum Calibration`=="yes"]),]
  } else{
    data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`Vacuum Calibration`==""]),]
  }
}
if(input$PCMIRD!="All"){
  if(input$PCMIRD=="yes"){
    data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`Irradiated`=="yes"]),]
  } else{
    data_PCM<-data_PCM[data_PCM$'Part Number' %in% unique(data_PCM$'Part Number'[data_PCM$`Irradiated`==""]),]
  }
}






# Tapered table part codes, saved for later use


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
  data_PCT<-data_PCT[data_PCT$`Resin Descriptionr`==input$PCTRD,]
}
if(input$PCTPPSN!="All"){
  data_PCT<-data_PCT[data_PCT$`PPS Number`==input$PCTPPSN,]
}
if(input$PCTDS_min!=PCTDSmin || input$PCTDS_max!=PCTDSmax){
  data_PCT<-data_PCT[data_PCT$`Die Size (in)`>=input$PCTDS_min & data_PCT$`Die Size (in)`<=input$PCTDS_max,]
}
if(input$PCTDLL!="All"){
  data_PCT<-data_PCT[data_PCT$`Die Land Length (in)`==input$PCTDLL,]
}
if(input$PCTTS_min!=PCTTSmin || input$PCTTS_max!=PCTTSmax){
  data_PCT<-data_PCT[data_PCT$`Die Size (in)`>=input$PCTTS_min & data_PCT$`Die Size (in)`<=input$PCTTS_max,]
}
if(input$PCTTLL!="All"){
  data_PCT<-data_PCT[data_PCT$`Tip Land Length (in)`==input$PCTTLL,]
}      
if(input$PCTSP!="All"){
  data_PCT<-data_PCT[data_PCT$`Screw Print`==input$PCTSP,]
}
if(input$PCTFT_min!=PCTFTmin || input$PCTFT_max!=PCTFTmax){
  data_PCT<-data_PCT[data_PCT$`Feedthroat Temperature  F`>=input$PCTFT_min & data_PCT$`Feedthroat Temperature  F`<=input$PCTFT_max,]
}
if(input$PCTBZT1_min!=PCTBZT1min || input$PCTBZT1_max!=PCTBZT1max){
  data_PCT<-data_PCT[data_PCT$`Barrel Zone 1 Temperature  F`>=input$PCTBZT1_min & data_PCT$`Barrel Zone 1 Temperature  F`<=input$PCTBZT1_max,]
}
if(input$PCTBZT2_min!=PCTBZT2min || input$PCTBZT2_max!=PCTBZT2max){
  data_PCT<-data_PCT[data_PCT$`Barrel Zone 2 Temperature  F`>=input$PCTBZT2_min & data_PCT$`Barrel Zone 2 Temperature  F`<=input$PCTBZT2_max,]
}
if(input$PCTBZT3_min!=PCTBZT3min || input$PCTBZT3_max!=PCTBZT3max){
  data_PCT<-data_PCT[data_PCT$`Barrel Zone 3 Temperature  F`>=input$PCTBZT3_min & data_PCT$`Barrel Zone 3 Temperature  F`<=input$PCTBZT3_max,]
}


if(input$PCTCT_min!=PCTCTmin || input$PCTCT_max!=PCTCTmax){
  data_PCT<-data_PCT[data_PCT$`Clamp Temperature  F`>=input$PCTCT_min & data_PCT$`Clamp Temperature  F`<=input$PCTCT_max,]
}
if(input$PCTAT_min!=PCTATmin || input$PCTAT_max!=PCTATmax){
  data_PCT<-data_PCT[data_PCT$`Adapter Temperature  F`>=input$PCTAT_min & data_PCT$`Adapter Temperature  F`<=input$PCTAT_max,]
}
if(input$PCTDT1_min!=PCTDT1min || input$PCTDT1_max!=PCTDT1max){
  data_PCT<-data_PCT[data_PCT$`Die 1 Temperature  F`>=input$PCTDT1_min & data_PCT$`Die 1 Temperature  F`<=input$PCTDT1_max,]
}
if(input$PCTDT2_min!=PCTDT2min || input$PCTDT2_max!=PCTDT2max){
  data_PCT<-data_PCT[data_PCT$`Die 2 Temperature  F`>=input$PCTDT2_min & data_PCT$`Die 2 Temperature  F`<=input$PCTDT2_max,]
}


if(input$PCTPIDI_min!=PCTPIDImin || input$PCTPIDI_max!=PCTPIDImax){
  data_PCT<-data_PCT[data_PCT$`Proximal Inner Diameter (in)`>=input$PCTPIDI_min & data_PCT$`Proximal Inner Diameter (in)`<=input$PCTPIDI_max,]
}
if(input$PCTPODI_min!=PCTPODImin || input$PCTPODI_max!=PCTPODImax){
  data_PCT<-data_PCT[data_PCT$`Proximal Outer Diameter (in)`>=input$PCTPODI_min & data_PCT$`Proximal Outer Diameter (in)`<=input$PCTPODI_max,]
}
if(input$PCTPWT_min!=PCTPWTmin || input$PCTPWT_max!=PCTPWTmax){
  data_PCT<-data_PCT[data_PCT$`Proximal Wall Thickness (in)`>=input$PCTPWT_min & data_PCT$`Proximal Wall Thickness (in)`<=input$PCTPWT_max,]
}
if(input$PCTPOR_min!=PCTPORmin || input$PCTPOR_max!=PCTPORmax){
  data_PCT<-data_PCT[data_PCT$`Proximal Out of Roundness (in)`>=input$PCTPOR_min & data_PCT$`Proximal Out of Roundness (in)`<=input$PCTPOR_max,]
}
if(input$PCTPCCT_min!=PCTPCCTmin || input$PCTPCCT_max!=PCTPCCTmax){
  data_PCT<-data_PCT[data_PCT$`Proximal Concentricity (in)`>=input$PCTPCCT_min & data_PCT$`Proximal Concentricity (in)`<=input$PCTPCCT_max,]
}


if(input$PCTDIDI_min!=PCTDIDImin || input$PCTDIDI_max!=PCTDIDImax){
  data_PCT<-data_PCT[data_PCT$`Distal Inner Diameter (in)`>=input$PCTDIDI_min & data_PCT$`Distal Inner Diameter (in)`<=input$PCTDIDI_max,]
}
if(input$PCTDODI_min!=PCTDODImin || input$PCTDODI_max!=PCTDODImax){
  data_PCT<-data_PCT[data_PCT$`Distal Outer Diameter (in)`>=input$PCTDODI_min & data_PCT$`Distal Outer Diameter (in)`<=input$PCTDODI_max,]
}
if(input$PCTDWT_min!=PCTDWTmin || input$PCTDWT_max!=PCTDWTmax){
  data_PCT<-data_PCT[data_PCT$`Distal Wall Thickness (in)`>=input$PCTDWT_min & data_PCT$`Distal Wall Thickness (in)`<=input$PCTDWT_max,]
}
if(input$PCTDOR_min!=PCTDORmin || input$PCTDOR_max!=PCTDORmax){
  data_PCT<-data_PCT[data_PCT$`Distal Out of Roundness (in)`>=input$PCTDOR_min & data_PCT$`Distal Out of Roundness (in)`<=input$PCTDOR_max,]
}
if(input$PCTDCCT_min!=PCTDCCTmin || input$PCTDCCT_max!=PCTDCCTmax){
  data_PCT<-data_PCT[data_PCT$`Distal Concentricity (in)`>=input$PCTDCCT_min & data_PCT$`Distal Concentricity (in)`<=input$PCTDCCT_max,]
}

if(input$PCTPLength_min!=PCTPLengthmin || input$PCTPLength_max!=PCTPLengthmax){
  data_PCT<-data_PCT[data_PCT$`Proximal Length (in)`>=input$PCTPLength_min & data_PCT$`Proximal Length (in)`<=input$PCTPLength_max,]
}
if(input$PCTTLength_min!=PCTTLengthmin || input$PCTTLength_max!=PCTTLengthmax){
  data_PCT<-data_PCT[data_PCT$`Transition Length (in)`>=input$PCTTLength_min & data_PCT$`Transition Length (in)`<=input$PCTTLength_max,]
}
if(input$PCTDLength_min!=PCTDLengthmin || input$PCTDLength_max!=PCTDLengthmax){
  data_PCT<-data_PCT[data_PCT$`Distal Length (in)`>=input$PCTDLength_min & data_PCT$`Distal Length (in)`<=input$PCTDLength_max,]
}
if(input$PCTToLength_min!=PCTToLengthmin || input$PCTToLength_max!=PCTToLengthmax){
  data_PCT<-data_PCT[data_PCT$`Total Length (in)`>=input$PCTToLength_min & data_PCT$`Total Length (in)`<=input$PCTToLength_max,]
}


if(input$PCTNEXIV!="All"){
  if(input$PCTNEXIV=="yes"){
    data_PCT<-data_PCT[data_PCT$`NEXIV`=="yes",]
  } else{
    data_PCT<-data_PCT[data_PCT$`NEXIV`=="",]
  }
}
if(input$PCTAnnealed!="All"){
  if(input$PCTAnnealed=="yes"){
    data_PCT<-data_PCT[data_PCT$`Annealed`=="yes",]
  } else{
    data_PCT<-data_PCT[data_PCT$`Annealed`=="",]
  }
}
if(input$PCTCaliper!="All"){
  if(input$PCTCaliper=="yes"){
    data_PCT<-data_PCT[data_PCT$`Caliper`=="yes",]
  } else{
    data_PCT<-data_PCT[data_PCT$`Caliper`=="",]
  }
}
if(input$PCTOS!="All"){
  if(input$PCTOS=="yes"){
    data_PCT<-data_PCT[data_PCT$`OD Sort`=="yes",]
  } else{
    data_PCT<-data_PCT[data_PCT$`OD Sort`=="",]
  }
}
if(input$PCTMP!="All"){
  if(input$PCTMP=="yes"){
    data_PCT<-data_PCT[data_PCT$`Melt Pump`=="yes",]
  } else{
    data_PCT<-data_PCT[data_PCT$`Melt Pump`=="",]
  }
}
if(input$PCTHT!="All"){
  if(input$PCTHT=="yes"){
    data_PCT<-data_PCT[data_PCT$`Hypo Tip`=="yes",]
  } else{
    data_PCT<-data_PCT[data_PCT$`Hypo Tip`=="",]
  }
}
if(input$PCTSPD!="All"){
  if(input$PCTSPD=="yes"){
    data_PCT<-data_PCT[data_PCT$`Sparker Die`=="yes",]
  } else{
    data_PCT<-data_PCT[data_PCT$`Sparker Die`=="",]
  }
}
if(input$PCTSLD!="All"){
  if(input$PCTSLD=="yes"){
    data_PCT<-data_PCT[data_PCT$`Slicking Die`=="yes",]
  } else{
    data_PCT<-data_PCT[data_PCT$`Slicking Die`=="",]
  }
}
if(input$PCTDLN!="All"){
  if(input$PCTDLN=="yes"){
    data_PCT<-data_PCT[data_PCT$`Delamination`=="yes",]
  } else{
    data_PCT<-data_PCT[data_PCT$`Delamination`=="",]
  }
}
if(input$PCTULT!="All"){
  if(input$PCTULT=="yes"){
    data_PCT<-data_PCT[data_PCT$`Ultrasonic`=="yes",]
  } else{
    data_PCT<-data_PCT[data_PCT$`Ultrasonic`=="",]
  }
}
if(input$PCTVC!="All"){
  if(input$PCTVC=="yes"){
    data_PCT<-data_PCT[data_PCT$`Vacuum Calibration`=="yes",]
  } else{
    data_PCT<-data_PCT[data_PCT$`Vacuum Calibration`=="",]
  }
}
if(input$PCTIRD!="All"){
  if(input$PCTIRD=="yes"){
    data_PCT<-data_PCT[data_PCT$`Irradiated`=="yes",]
  } else{
    data_PCT<-data_PCT[data_PCT$`Irradiated`=="",]
  }
}
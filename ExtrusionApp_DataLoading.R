#This R code is used to load csv or xls file into R. And clean the raw data based on demand.
#And we need to add more code to update our database daily
library(shiny)
library(bootstrap)
library(jpeg)
library(ggplot2)
library(DT)
library(stringr)
library(gsubfn)
library(proto)
library(sqldf)


#LOADING DATA

#Creating variables across all sessions
path <- "C:/Users/kongl5/Desktop/Shiny/ExtrusionApp/Extrusion Application/PPS_document_contents/ListOfSheets/GTE"
single_pps_file <- "Single PPS Data Filled GTE.csv"
#single_tari_file <- "Single Tari Data.csv"
single_tari_file <- "MES Data_Single Tari Data.csv"
multi_pps_file <- "Multi-Layered PPS Data Filled GTE.csv"
tapered_pps_file <- "Tapered PPS Data Filled GTE.csv"
resin_file <- "Resin Information.csv"
screw_file <- "Screw Properties.csv"

single_pps_pathfile <- paste(path, single_pps_file, sep = "/")
single_tari_pathfile <- paste(path, single_tari_file, sep = "/")
multi_pps_pathfile <- paste(path, multi_pps_file, sep = "/")
tapered_pps_pathfile <- paste(path, tapered_pps_file, sep = "/")
resin_pathfile <- paste(path, resin_file, sep = "/")
screw_pathfile <- paste(path, screw_file, sep = "/")

single_pps_data <- read.csv(single_pps_pathfile, header = TRUE, stringsAsFactors = FALSE, 
                            check.names = FALSE)
single_tari_data <- read.csv(single_tari_pathfile, header = TRUE, stringsAsFactors = FALSE, 
                             check.names = FALSE)
multi_pps_data <- read.csv(multi_pps_pathfile, header = TRUE, stringsAsFactors = FALSE, 
                           check.names = FALSE)
tapered_pps_data <- read.csv(tapered_pps_pathfile, header = TRUE, stringsAsFactors = FALSE, 
                             check.names = FALSE)
resin_data <- read.csv(resin_pathfile, header = TRUE, stringsAsFactors = FALSE, 
                       check.names = FALSE)
screw_data <- read.csv(screw_pathfile, header = TRUE, stringsAsFactors = FALSE, 
                       check.names = FALSE)



#DATA CLEANING

#Change all NA to blank
single_pps_data[is.na(single_pps_data)]<-""
single_tari_data[is.na(single_tari_data)]<-""
multi_pps_data[is.na(multi_pps_data)]<-""
tapered_pps_data[is.na(tapered_pps_data)]<-""
resin_data[is.na(resin_data)]<-""
screw_data[is.na(screw_data)]<-""

#single_pps_data---change a non-numeric value to blank
#single_pps_data[,single_pps_data$`Die Land Length (in)`==word]<=""
#single_pps_data[single_pps_data$`Die Size (in)`=="Profile",]<-""
#single_pps_data[single_pps_data$`Die Land Length (in)`=="Tooling was not found",]<-""
#single_pps_data[single_pps_data$`Die Land Length (in)`=="Standard",]<-""





#Output--MES--get the start date from Start Time
temp=as.data.frame(matrix(0,nrow=nrow(single_tari_data),ncol=2))
colnames(temp)=c("Start Date","Start Time")
temp[,1:2]=str_split_fixed(single_tari_data$`Start Time`,' ',2)
temp[,1]=as.Date(temp[,1],"%m/%d/%Y",origin="1970-01-01")
single_tari_data=cbind(single_tari_data[,1:which(colnames(single_tari_data)=="Start Time")-1],
                       temp,single_tari_data[,(which(colnames(single_tari_data)=="Start Time")+1):ncol(single_tari_data)])
Time_Start=sqldf("select Min([Start Date]) from single_tari_data")
Time_Start<-as.numeric(Time_Start)
Time_Start<-as.Date(Time_Start,origin="1970-01-01")
Time_End<-sqldf("select Max([Start Date]) from single_tari_data")
Time_End<-as.numeric(Time_End)
Time_End<-as.Date(Time_End,origin="1970-01-01")


#Get the range of all tabs---single Extrusion PPS DATA
PCSDSmin=0.035;PCSDSmax=0.54;PCSTSmin=0.01;PCSTSmax=0.49;PCSFTmin=20;PCSFTmax=100;PCSBZT1min=245;PCSBZT1max=650
PCSBZT2min=315;PCSBZT2max=700;PCSBZT3min=320;PCSBZT3max=735;PCSCTmin=325;PCSCTmax=735;PCSATmin=330;PCSATmax=740
PCSDT1min=330;PCSDT1max=740;PCSDT2min=330;PCSDT2max=740;PCSIDImin=0.0009;PCSIDImax=0.275;PCSODImin=0.0194;PCSODImax=0.41
PCSWTmin=0.001;PCSWTmax=0.0375;PCSORmin=0.001;PCSORmax=0.007;PCSCCTmin=0.0005;PCSCCTmax=0.0025;PCSLengthmin=0.4;PCSLengthmax=84
#Get the range of all tabs---Multi Extrusion PPS DATA
PCMDSmin=0.035;PCMDSmax=0.54;PCMTSmin=0.01;PCMTSmax=0.49;PCMFTmin=20;PCMFTmax=100;PCMBZT1min=245;PCMBZT1max=650
PCMBZT2min=315;PCMBZT2max=700;PCMBZT3min=320;PCMBZT3max=735;PCMCTmin=325;PCMCTmax=735;PCMATmin=330;PCMATmax=740
PCMDT1min=330;PCMDT1max=740;PCMDT2min=330;PCMDT2max=740;PCMIDImin=0.0009;PCMIDImax=0.275;PCMODImin=0.0194;PCMODImax=0.41
PCMIWTmin=0.0004;PCMIWTmax=0.0262;PCMMWTmin=0.003;PCMMWTmax=0.001;PCMOWTmin=0.0005;PCMOWTmax=0.017;PCMTWTmin=0.0015;PCMTWTmax=0.096;
PCMORmin=0.001;PCMORmax=0.007;PCMCCTmin=0.0005;PCMCCTmax=0.0025;PCMLengthmin=0.4;PCMLengthmax=84
#Get the range of all tabs---Tapered Extrusion PPS DATA
PCTDSmin=0.035;PCTDSmax=0.54;PCTTSmin=0.01;PCTTSmax=0.49;PCTFTmin=20;PCTFTmax=100;PCTBZT1min=245;PCTBZT1max=650
PCTBZT2min=315;PCTBZT2max=700;PCTBZT3min=320;PCTBZT3max=735;PCTCTmin=325;PCTCTmax=735;PCTATmin=330;PCTATmax=740
PCTDT1min=330;PCTDT1max=740;PCTDT2min=330;PCTDT2max=740;
PCTPIDImin=0.0009;PCTPIDImax=0.275;PCTPODImin=0.0194;PCTPODImax=0.41;PCTPWTmin=0.0004;PCTPWTmax=0.0262;
PCTPORmin=0.001;PCTPORmax=0.007;PCTPCCTmin=0.0005;PCTPCCTmax=0.0025;
PCTDIDImin=0.0009;PCTDIDImax=0.275;PCTDODImin=0.0194;PCTDODImax=0.41;PCTDWTmin=0.0004;PCTDWTmax=0.0262;
PCTDORmin=0.001;PCTDORmax=0.007;PCTDCCTmin=0.0005;PCTDCCTmax=0.0025;
PCTPLengthmin=0.4;PCTPLengthmax=84;PCTTRLengthmin=0.4;PCTTRLengthmax=84;PCTDLengthmin=0.4;PCTDLengthmax=84;
PCTTOLengthmin=0.4;PCTTOLengthmax=84






#Get the range of all tabs---Tapered Extrusion PPS DATA
PCTDSmin=min(tapered_pps_data$`Die Size (in)`)
PCTDSmax=max(tapered_pps_data$`Die Size (in)`)



#Single Extrusion PPS Data---Special Parameter---Change the blank to No

#Catalog--Multi Extrusion PPS Table---Fill the Partnumber and PPS number for each single row in the table
for (i in 1:nrow(multi_pps_data)){
  if(multi_pps_data[i,"Part Number"]==""){
    multi_pps_data[i,"Part Number"]=multi_pps_data[i-1,"Part Number"]
  }
  if(multi_pps_data[i,"PPS Number"]==""){
    multi_pps_data[i,"PPS Number"]=multi_pps_data[i-1,"PPS Number"]
  }
}
#Special Parameter---use NA to replace blank







#display all rows which share the same partnumbers where the parameter meets the requirements
temp=sqldf("select * from multi_pps_data where multi_pps_data.[Part Number] in 
           (select [Part Number] from multi_pps_data 
           where [Barrel Zone 1 Temperature  F]=345)")
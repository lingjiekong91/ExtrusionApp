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
PCSDSmin=0.035
PCSDSmax=0.54
PCSTSmin=0.01
PCSTSmax=0.49
PCSFTmin=20
PCSFTmax=100
PCSBZT1min=245
PCSBZT1max=650
PCSBZT2min=315
PCSBZT2max=700
PCSBZT3min=320
PCSBZT3max=735


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
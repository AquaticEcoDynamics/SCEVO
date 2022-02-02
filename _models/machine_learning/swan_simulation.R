## forcast model

## forcast for five days ahead 
## modelling rainfall --> flow 
## load the package 

list.of.packages <- c("mlr", "RCurl","EcoHydRology","caret","magrittr","xts","reshape2",
  "lubridate","zoo","EGRET","JBTools","rmatio","dplyr","corrplot","data.table","ncdf4","ncdf4.helpers",
  "downloader","rvest")
  
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,lib="/usr/share/R/library/",repos="https://cran.rstudio.com")



if(!('h2o' %in% installed.packages()[,"Package"])) {

if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")))

}

library(mlr)
library(RCurl)
library(EcoHydRology)
library(caret)
library(magrittr)
library(xts)
library(reshape2)
library(h2o)
library(lubridate)
library(zoo) 
library(EGRET)
library(JBTools)
library(rmatio)
library(dplyr)
library(corrplot)
library(data.table)
library(ncdf4)
library(ncdf4.helpers)
library(downloader)
library(rvest)

## preprocess the date to get dd/mm/yyyy format
create_date<-function(data_set){
  data_set[,"Date"]=paste0(data_set[,"Day"],"/",data_set[,"Month"],"/",data_set[,"Year"])
  data_set$Date<-dmy(data_set$Date)
  data_set[,1:3]<-NULL
  return(data_set)
}

## function to generate lagged data
generate_lag<-function(data_set,var,start=1,end=5,by=1,func){
  
  if(func=="max"){    
    for (d in seq(start, end, by)) {
      data_set[, paste0("max_",var,"_", 
                        as.character(d))] <- rollmaxr(data_set[,var],
                                                      k = d, fill = 0,align = "right")
    }
  } else if (func=="sum"){
    for (d in seq(start, end, by)) {
      data_set[, paste0("sum_",var,"_", as.character(d))] <- rollsumr(data_set[,var], k = d, fill = 0)
    }
  } else if (func=="mean"){
    for (d in seq(start, end, by)) {
      data_set[, paste0("mean_",var,"_", as.character(d))] <- rollmeanr(data_set[,var], k = d, fill = 0)
    }}
  
  return(data_set)
}


standardize<-function(train_d,valid_d,test_d){
  scaled_train=train_d
  scaled_valid=valid_d
  scaled_test=test_d
  
  for(ii in seq(1,ncol(scaled_train),1)){    
    min_v<-min(scaled_train[,ii])    
    max_v<-max(scaled_train[,ii])    
    scaled_train[,ii]=(scaled_train[,ii]-min_v)/(max_v-min_v)
    scaled_valid[,ii]=(scaled_valid[,ii]-min_v)/(max_v-min_v)
    scaled_test[,ii]=(scaled_test[,ii]-min_v)/(max_v-min_v)
  }
  return(list(scaled_train,scaled_valid,scaled_test))
}

standardize_reverse<-function(data_set,max_v,min_v){
  data_set<-data_set*(max_v-min_v)+min_v
  return(data_set)
}

remove_outlier<-function(dataset,target){
  dataset<-subset(dataset,dataset[,target]!='NA')
  Q1=as.numeric(quantile(dataset[,target],c(0.25,0.75))[1])
  Q3=as.numeric(quantile(dataset[,target],c(0.25,0.75))[2])
  IQR=Q3-Q1
  upper_limit<-Q3+10*IQR
  lower_limit<-Q1-10*IQR
  new_data<-subset(dataset,(dataset[,target]<=upper_limit) & (dataset[,target]>=lower_limit))
  print(dim(new_data))
  return(new_data)
}


shift_function<-function(data_set,var,days){
  
  for(day in seq(1,days,1)){
    data_set[, paste0("",var,"_", 
                      as.character(day))] <- shift(data_set[,var],n = day,
                                                   fill = 0)
  }
  return(data_set)
}

combined_nutrient_unscaled <- function(dataset, n_target) {
  
  Nutrient_target <- dataset[is.na(dataset[[n_target]]) == F, c("Date", n_target)]
  Nutrient_target[["Date"]] <- ymd(Nutrient_target[["Date"]])
  
  names(Nutrient_target) <- c("Date", n_target)
  
  Nutrient_target$Date<-as.character(Nutrient_target$Date)
  Flow_DC$Date<-as.character(Flow_DC$Date)
  all_rainfall$Date<-as.character(all_rainfall$Date)
  ## combine datasets 
  combined_target <- merge(Nutrient_target, Flow_DC, by = 'Date', all.y = T) 
  combined_target<-merge(all_rainfall,combined_target, by = 'Date', all.y = T)  
  
  combined_target$Date<-as.numeric(ymd(combined_target$Date))
  combined_target<-combined_target[order(combined_target$Date),]
  
  ## define data frame for baseflow sepration 
  baseflow_sep<-data.frame(Date=as.Date(combined_target$Date, origin="1970-01-01"),
                           P_mm=combined_target$P,
                           Streamflow_m3s=combined_target$Q)
  
  baseflow_sep[is.na(baseflow_sep$Streamflow_m3s),"Streamflow_m3s"]=median(baseflow_sep$Streamflow_m3s,na.rm = T)
  
  bfs<-BaseflowSeparation(baseflow_sep$Streamflow_m3s, passes=3)
  ## lagged data for baseflow and quickflow
  for (d in c(3,7,15)) {
    bfs[, paste0("mean_QF", as.character(d))] <- rollmeanr(bfs[,2], k = d, fill = 0)
  }  
  
  for (d in c(3,7,15)) {
    bfs[, paste0("mean_BF", as.character(d))] <- rollmeanr(bfs[,1], k = d, fill = 0)
  }
  ## combine with flow, rainfall, and nutrient data 
  combined_target<-cbind(combined_target,bfs)
  
  ## add 0.00001 to avoid error in log transformation
  combined_target$logQ<-log(combined_target$Q+0.00001)
  
  combined_target$DecYear<-NULL
  return(combined_target)
  
}

## function to build the model 
model_build_simple<- function(dataset, n_target) {
  
  y <- n_target
  x <- setdiff(names(dataset), y)
  ## can also add max_depth, min_row, learning_rate
  dataset<-as.h2o(dataset)
  
  gbm<- h2o.gbm(x = x,
                y = y,
                training_frame = dataset,
                nfolds=3)    
  return(gbm)
}


## function to find important variables
find_importVar<-function(train_data,test_data,n_target,var_number){
  
  gbm_var<- model_build_simple(as.h2o(train_data), n_target)
  var_imp<-h2o.varimp(gbm_var)
  
  import_var<-var_imp[1:var_number,1]
  
  if (!("Date" %in% import_var)) {
    import_var<-c(import_var,n_target,"Date")
  } else {
    import_var<-c(import_var,n_target)
  }
  
  training_out<-train_data[,import_var]
  testing_out<-test_data[,import_var]
  ## remove highly correlated variables 
  high_cor<-findCorrelation(cor(training_out), cutoff = 0.9, verbose = F)
  
  if (length(high_cor)==0){
    training_out <- training_out
    testing_out <- testing_out
    
  } else{
    training_out <- training_out[,-high_cor]
    testing_out <- testing_out[,-high_cor]
  }
  
  if (!("Date" %in% names(training_out))) {
    training_out$Date<-train_data$Date
    testing_out$Date<-test_data$Date
  } 
  
  out_data<-list(training_out,testing_out)
  
  return(out_data)
}

## function to generate daily nutrient
generate_nutrient_daily <- function(data_set,n_target) {
  
  print(n_target)
  ## select nutrient_target
  data_set<-remove_outlier(data_set,n_target)
  combined_target <- combined_nutrient_unscaled(data_set, n_target)
  sampled_target_selected <- subset(combined_target,combined_target[,n_target]!='NA')
  ## remove the outliers
  #sampled_target_selected<-remove_outlier(sampled_target_selected,n_target)
  combined_target$Date <- as.numeric(combined_target$Date)
  
  ## build the model_build
  gbm <- model_build_simple(sampled_target_selected, n_target)
  predict_rf_n_nutrient <- data.frame(combined_target["Date"], 
                                      as.data.frame(h2o::h2o.predict(gbm,as.h2o(combined_target))[,"predict"]))
  names(predict_rf_n_nutrient) <- c("Date", paste0(n_target, "_p"))
  return(predict_rf_n_nutrient)
}  


## function to obtain variable importance in intermediate models 
h2o.init(nthreads = -1,max_mem_size = "4g")

## load the data  
setwd("/ARMS/Workspaces/dpaw/dpaw/models/machine_learning/Inflow_Nutrient/")
#setwd("D:/PhD proram/ACOMO/debug/")
## forecast rainfall 
WRF_output <- nc_open('/ARMS/Workspaces/dpaw/dpaw/models/machine_learning/WRF_file/swan_met.nc')
#WRF_output <- nc_open('swan_met.nc')

## decimal hours since 01/01/1990 00:00
dname="Precip"
tmp_array <- ncvar_get(WRF_output,dname)
airport_rainfall<-tmp_array[26,64,]

t <- ncvar_get(WRF_output,"time")
ts=t/(24*365.25)+1990
ts<-date_decimal(ts,tz = "Australia/Perth")

forecasted_rainfall<-data.frame(Time=ts,P=airport_rainfall)
forecasted_rainfall$Date<-as.Date(forecasted_rainfall$Time)

forecasted_rainfall<-aggregate(forecasted_rainfall$P, 
                               by=list(Date=forecasted_rainfall$Date), FUN=sum)

colnames(forecasted_rainfall)<-c("Date","P")

## 

## historical rainfall 

webpage <- read_html("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=136&p_display_type=dailyDataFile&p_startYear=&p_c=&p_stn_num=009021")

rainfall_link<-as.character(html_nodes(webpage,"li:nth-child(2) a")[20]) 
rainfall_link<-strsplit(rainfall_link,";")[[1]][3]

rainfall_link<-paste0("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_display_type=dailyZippedDataFile&p_stn_num=009021&",
                      rainfall_link,"&p_nccObsCode=136&p_startYear=2018")

download(rainfall_link, dest="historical_rainfall.zip", mode="wb") 
unzip ("historical_rainfall.zip")

historical_rainfall<-read.csv("IDCJAC0009_009021_1800_Data.csv",header=T)
historical_rainfall<-historical_rainfall[,c(3,4,5,6)]

historical_rainfall<-create_date(historical_rainfall)

colnames(historical_rainfall)<-c("P","Date")
historical_rainfall<-subset(historical_rainfall,(historical_rainfall$Date>=as.Date("1988-01-01")))

historical_rainfall[is.na(historical_rainfall$P),"P"]<-median(historical_rainfall$P,
                                                              na.rm = T)


historical_rainfall<-historical_rainfall[,colnames(forecasted_rainfall)]

start_date<-range(forecasted_rainfall$Date)[1]
end_date<-range(forecasted_rainfall$Date)[2]

all_rainfall<-rbind(historical_rainfall,forecasted_rainfall)
all_rainfall$P<-round(all_rainfall$P,digits = 7)

all_rainfall[all_rainfall$P<0,"P"]=0


## forecasted temp 
dname="temp"
tmp_array <- ncvar_get(WRF_output,dname)
airport_temp<-tmp_array[26,64,]

t <- ncvar_get(WRF_output,"time")
ts=t/(24*365.25)+1990
ts<-date_decimal(ts,tz = "Australia/Perth")

forecasted_temp<-data.frame(Time=ts,Temp=airport_temp)
forecasted_temp$Date<-as.Date(forecasted_temp$Time)

forecasted_temp<-aggregate(forecasted_temp$Temp, 
                           by=list(Date=forecasted_temp$Date), FUN=max)

colnames(forecasted_temp)<-c("Date","min_air_temp")


webpage <- read_html("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=122&p_display_type=dailyDataFile&p_startYear=&p_c=&p_stn_num=009021")

temp_link<-as.character(html_nodes(webpage,"li:nth-child(2) a")[20]) 
temp_link<-strsplit(temp_link,";")[[1]][3]

temp_link<-paste0("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_display_type=dailyZippedDataFile&p_stn_num=009021&",
                  temp_link,"&p_nccObsCode=122&p_startYear=2018")

download(temp_link, dest="historical_temp.zip", mode="wb") 
unzip ("historical_temp.zip")

historical_temp<-read.csv("IDCJAC0010_009021_1800_Data.csv",header=T)
historical_temp<-historical_temp[,c(3,4,5,6)]

historical_temp<-create_date(historical_temp)

colnames(historical_temp)<-c("min_air_temp","Date")
historical_temp<-subset(historical_temp,(historical_temp$Date>=as.Date("1988-01-01"))&
                          (historical_temp$Date<start_date))

historical_temp[is.na(historical_temp$P),"min_air_temp"]<-median(historical_temp$Temp,
                                                                 na.rm = T)

historical_temp<-historical_temp[,c("Date","min_air_temp")]

all_temp<-rbind(historical_temp,forecasted_temp)

river_list<-c("bayswater","canning","bennet","ellenbrook","helena","jane","susannah","upperswan")

for (river in river_list){
  
  
  if(river %in% c("bayswater","canning","ellenbrook","helena","jane","susannah")){
    all_nutrient_targets<-c("TN","AMM","NOx","DON","TP",
                            "PHS_FRP","DOC","POC","SS")
    
    nutrient_pools<-c("TP","DON","DOC","TN","POC","NOx","AMM")
    
  } else {
    all_nutrient_targets<-c("TN","AMM","NOx","TP","PHS_FRP","POC") 
    
    nutrient_pools<-c("TP","TN","POC","NOx","AMM")
    
  }
  
  
  generated_nutrient_list<-function(dataset,n_target){
    
    nutrient_pool=nutrient_pools
    generated_nutrient<-nutrient_pool[!(nutrient_pool %in% n_target)]
    nutrient_list=vector(mode = "list",length = length(generated_nutrient))
    
    for(ii in 1:length(generated_nutrient)){
      g_nutrient<- generate_nutrient_daily(dataset,generated_nutrient[ii])
      nutrient_list[[ii]]<-g_nutrient
    }
    return(nutrient_list)
  }
  
  
  ## load the nutrient and flow data 
  Nutrient <- read.csv(paste0(river,".csv"), header = T)
  
  ## rename the col
  names(Nutrient)[1] <- c("Date")
  #Nutrient$Flow<-NULL
  
  ## change missing data type 
  Nutrient[Nutrient == 'missing'] <- NA
  
  ## change date type 
  Nutrient$Date<-lubridate::dmy(Nutrient$Date)
  #Flow$Date<-lubridate::dmy(Flow$Date)
  
  for (i in seq(2,ncol(Nutrient),1)){
    Nutrient[,i]<-as.numeric(as.character(Nutrient[,i]))
  }
  
  ## change date type 
  ## lagged Q 
  Flow<-Nutrient[,1:2]
  colnames(Flow)[2]<-c("Q")
  
  Flow[is.na(Flow$Q),"Q"]=median(Flow$Q,na.rm = T)
  
  ## 
  historical_flow<-read.csv(paste0(river,"_Inflow.csv"),header=T)
  historical_flow<-historical_flow[,1:2]
  historical_flow$ISOTime<-dmy_hms(historical_flow$ISOTime)
  historical_flow$Date<-as.Date(historical_flow$ISOTime)
  
  historical_flow<-aggregate(historical_flow$Flow, 
                             by=list(Date=historical_flow$Date), FUN=mean)
  
  colnames(historical_flow)<-c("Date","Q")
  
  #subset(forecasted_rainfall,forecasted_rainfall$Date)
  historical_flow[is.na(historical_flow$Q),"Q"]=median(historical_flow$Q,na.rm = T)
  
  historical_flow<-subset(historical_flow,(historical_flow$Date>range(Flow$Date)[2])&
                            (historical_flow$Date<=end_date))
  
  
  Flow<-rbind(Flow,historical_flow)
  
  
  Flow_DC<-Flow
  ### water temperature
  
  historical_temp<-Nutrient[,c("Date","TEMP")]
  historical_temp<-merge(historical_temp,all_temp,by="Date",all.x=T)
  historical_temp<-merge(historical_temp,Flow_DC,by="Date",all.x=T)
  historical_temp<-historical_temp[,1:4]
  
  ## preprocess rainfall
  
  historical_temp<-generate_lag(historical_temp,"min_air_temp",start = 1,end = 5,func = "mean")
  
  historical_temp$DecYear<-decimal_date(historical_temp$Date)
  historical_temp$SinDY<-sin(2*pi*historical_temp$DecYear)
  historical_temp$CosDY<-cos(2*pi*historical_temp$DecYear)
  
  historical_temp$DecYear<-NULL
  
  historical_temp$Date<-as.numeric(historical_temp$Date)
  
  measured_Temp<-subset(historical_temp,historical_temp$TEMP>=0)
  ## build the model 
  
  GBM_temp<- model_build_simple(as.h2o(measured_Temp), "TEMP")
  
  all_water_temp<-h2o::h2o.predict(GBM_temp,as.h2o(historical_temp))
  colnames(all_water_temp)<-c("Temp")
  
  ## combine the forecasted flow and all_data
  ## all_data<-rbind(all_data,forecast_dataset)
  n_day=as.numeric(end_date-start_date)+1
  all_hind_fore_results<-data.frame(Date=seq(start_date,end_date,by=1),tail(Flow_DC,n_day)[2],tail(all_water_temp,n_day)[1])
  
  for (n_target in all_nutrient_targets){
    #n_target="AMM"
    
    ## remove outliers
    Nutrient2<-remove_outlier(Nutrient,n_target)
    #Nutrient2<-Nutrient
    Nutrient2$Date<-format(as.Date(Nutrient2$Date), "%Y-%m-%d")
    
    ## get combined flow, rainfall, nutrient, baseflow, quickflow dataset
    All_combined<-combined_nutrient_unscaled(Nutrient2,n_target)
    All_combined<-All_combined[!duplicated(All_combined$Date),]
    all_date<-All_combined$Date
    
    All_combined$DecYear<-decimal_date(as.Date(All_combined$Date, origin="1970-01-01"))
    All_combined$SinDY<-sin(2*pi*All_combined$DecYear)
    All_combined$CosDY<-cos(2*pi*All_combined$DecYear)
    All_combined$DecYear<-NULL
    
    
    Nutrient_sampled <- subset(All_combined, All_combined[[n_target]] != 'NA')
    Nutrient_sampled<-Nutrient_sampled[order(Nutrient_sampled$Date),]
    
    training_p1 <- Nutrient_sampled
    training_p1 <- round(training_p1, digits =7)
    train_date<-training_p1$Date
    
    
    var_number=10
    important_val=find_importVar(training_p1,training_p1,n_target,var_number)
    training_p2<-important_val[[1]]
    #training_p2<-training_p1
    #testing_p2<-testing_p1
    
    training_p2<-training_p2[order(training_p2$Date),] 
    
    training_p2$Date<-NULL
    
    if("Q" %in% colnames(training_p2)){
      training_p2$Q<-NULL
      
    }
    
    ## generate daily nutrient
    nutrient_data <- generated_nutrient_list(Nutrient,n_target)
    
    training_p3<-training_p2
    
    training_p3$Date<-train_date 
    # testing_p3$Date<-test_date    
    
    for (i in seq(1,length(nutrient_data))){
      
      #nutrient_data[[i]][2]<-log(abs(nutrient_data[[i]][2]))
      Nutrient_g=nutrient_data[[i]]
      Nutrient_g<-Nutrient_g[!duplicated(Nutrient_g$Date),]
      
      training <- merge(Nutrient_g, training_p3, by = "Date", all.y = T)
      
      All_combined2<-merge(Nutrient_g, All_combined, by = "Date", all.y = T)
      
      training_p3 <- training
      All_combined<-All_combined2
      #print(dim(All_combined))
    }
    
    
    min_date<-min(training_p3[,"Date"])
    max_date<-max(training_p3[,"Date"])
    
    training_p3[,"Date"]<-(training_p3[,"Date"]-min_date)/(max_date-min_date)
    
    All_combined[,"Date"]<-(All_combined[,"Date"]-min_date)/(max_date-min_date)
    
    
    training_p3<-training_p3[order(training_p3$Date),] 
    All_combined<-All_combined[order(All_combined$Date),]    
    
    training_p3$Date<-NULL
    All_combined$Date<-NULL
    
    All_combined<-All_combined[,colnames(training_p3)]
    #all_e<-data.frame()
    GBM_p3 <- model_build_simple(as.h2o(training_p3), n_target)
    
    pred_all_train=as.data.frame(h2o::h2o.predict(GBM_p3,as.h2o(All_combined)))
    pred_all_train<-data.frame(Date=all_date,Pred=pred_all_train$predict)
    pred_all_train$Date<-as.Date(pred_all_train$Date, origin="1970-01-01")
    pred_all_train[pred_all_train$Pred<0,"Pred"]=0
    
    Nutrient2$Date<-as.Date(Nutrient2$Date)
    #c("#00B2EE", "#66CD00", "#FF0000", "#FF6347")"
    #pred_hybrid_daily$Date <- as.Date(pred_hybrid_daily$Date)
    pred_all_train$Date <- as.Date(pred_all_train$Date)
    
    
    hind_forecast_results<-subset(pred_all_train,(pred_all_train$Date>=start_date)&
                                    (pred_all_train$Date<=end_date))
    colnames(hind_forecast_results)[2]<-n_target
    
    all_hind_fore_results<-cbind(all_hind_fore_results,hind_forecast_results[2])
    
  }
  write.csv(all_hind_fore_results,paste0("/ARMS/Workspaces/dpaw/dpaw/models/machine_learning/Output/",
                                         river,"_simulation.csv"),row.names=FALSE)
}



firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


setwd("/ARMS/Workspaces/dpaw/dpaw/models/machine_learning/Output/")

river_list<-c("bayswater","ellenbrook","helena","canning","bennet","jane","susannah","upperswan")

dir.create("/ARMS/Workspaces/dpaw/dpaw/models/machine_learning/Output/BC files")

for(river in river_list){
  results<-read.csv(paste0(river,"_simulation.csv"),header=T)
  
  if(!("DOC" %in% colnames(results))){
    results$DOC<-results$POC*0.3
  } 
  
  if(!("DON" %in% colnames(results))){
    results$DON<-results$DOC*16/106
  } 
  if(!("SS" %in% colnames(results))){
    results$SS<-1
  } 
  
  
  BC_file<-data.frame(ISOTime=results$Date,Flow=results$Q,Sal=0,Temp=results$Temp,TRACE_1=1,AGE=0,SS1=results$SS*0.7,
                      SS2=results$SS*0.3,Oxy=200,Sil=100,Amm=results$AMM,Nit=results$NOx,FRP=results$PHS_FRP,
                      FRP_ADS=results$PHS_FRP*0.1,DOC=results$DOC,POC=results$POC,DON=results$DON,
                      PON=results$POC*16/106,DOP=results$DOC/106,POP=results$POC/106,DOCR=results$DOC*9,
                      DONR=results$DON*9,DOPR=results$DOC/106*9,CPOM=results$POC*0.1,GRN=2,CRYPT=2,DIATOM=3,
                      DINO=10,DINO_IN=10*16/106,BGA=2,BGA_RHO=1000,BGA_IN=2*16/106,BGA_IP=2/106)
  
  BC_file$ISOTime<-ymd(BC_file$ISOTime)
  
  BC_file$ISOTime<-format(BC_file$ISOTime, "%d/%m/%Y %H:%M:%S")
  
  
  write.csv(BC_file,paste0("/ARMS/Workspaces/dpaw/dpaw/models/machine_learning/Output/BC files/",
                           firstup(river),"_Inflow.csv"),row.names = FALSE,quote = FALSE)
  
}




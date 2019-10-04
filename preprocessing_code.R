list.of.packages <- c("data.table","dplyr","lubridate","readr")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(data.table)
library(dplyr)
library(lubridate)
library(readr)

#저장경로 설정 
raw_moving_path<-'./raw_moving_data/'
raw_remaining_path<-'./raw_remaining_data/'
preprocessAll_path<-'./preprocess_moving_data/'


#원천 이동 데이터 불러오기
data_names<-list.files(raw_moving_path)
data_list<-list()

for(i in 1:length(data_names)){
  
  encoding_name<-guess_encoding(paste0('./raw_moving_data/',data_names[i]))[1] %>% unlist()
  
  if(encoding_name[1]=='UTF-8'){
    data_list[[i]]<-fread(paste0(raw_moving_path,data_names[i]),encoding = 'UTF-8')
  }else{
    data_list[[i]]<-fread(paste0(raw_moving_path,data_names[i]))
  }
  
}

#칼럼 이름 변경
data_list1<-lapply(data_list,function(x){
  colnames(x)<-c("bicycle_number","rental_time","rental_shop_number","rental_shop_name","rental_holder_number"
                 ,"back_time","back_shop_number","back_shop_name","back_holder_number","used_time","used_distance")
  return(x)
})


raw_frame_d<-do.call(rbind,data_list1)


col_change_name<-c("bicycle_number","rental_time","rental_shop_number","rental_shop_name","back_time","back_shop_number","back_shop_name")

#데이터 전처리 (특수기호' ' 제거 )
for(j in 1:length(col_change_name)){
  raw_frame_d[,which(colnames(raw_frame_d)==col_change_name[j])]<-gsub("'","",unlist(raw_frame_d[,col_change_name[j],with=F]))
  print(paste0(j,'/',length(col_change_name)))
}

raw_frame_d[ , rental_day_all := as.numeric(gsub("-","",substr(raw_frame_d$rental_time,1,11)))]
raw_frame_d[ , back_day_all := as.numeric(gsub("-","",substr(raw_frame_d$back_time,1,11)))]

#전처리된 이동 데이터 저장
ifelse(!dir.exists(file.path(preprocessAll_path)), dir.create(file.path(preprocessAll_path)), FALSE)
write.csv(raw_frame_d,paste0(preprocessAll_path,'/','preprocessing_moving_data.csv'),row.names = F)

###########################################################################################################

#원천 잔여량 데이터 불러오기
data_names_re<-list.files(raw_remaining_path)
data_list_re<-list()

for(i in 1:length(data_names_re)){
  data_list_re[[i]]<-fread(paste0(raw_remaining_path,data_names_re[i]),encoding = 'UTF-8')
  colnames(data_list_re[[i]])<-c("shared","stationLatitude","stationLongitude","stationName","parkingBikeTotCnt","time","rackTotCnt","stationId")
  print(paste0('data_load_remaining',data_names_re[i]))
}

#잔여량 데이터 전처리 (정류소 이름과 번호 분리 )
raw_remaining_d<-do.call(rbind,data_list_re)
raw_remaining_d<-arrange(raw_remaining_d[,c(4:7)],stationName,time)
raw_remaining_d$shop_number<-unlist(lapply(strsplit(raw_remaining_d$stationName,"\\."),function(x){return(x[1])}))
raw_remaining_d$shop_name<-unlist(lapply(strsplit(raw_remaining_d$stationName,"\\."),function(x){return(x[2])}))

#잔여량 데이터 전처리 (시간데이터를 character에서 lubridate 형식으로변경  )
raw_remaining_d$time<-paste0(substr(raw_remaining_d$time,1,10)," ",substr(raw_remaining_d$time,12,16),":00")
raw_remaining_d$time<-ymd_hms(raw_remaining_d$time)
raw_remaining_d$stationName<-NULL
raw_remaining_d$day_all<-substr(raw_remaining_d$time,1,11)

#잔여량 데이터 전처리 (년,월,일,시간,분 칼럼 생성)
raw_remaining_d$remaining_year<-year(raw_remaining_d$time)
raw_remaining_d$remaining_month<-month(raw_remaining_d$time)
raw_remaining_d$remaining_day<-day(raw_remaining_d$time)

raw_remaining_d$remaining_hour<-hour(raw_remaining_d$time)
raw_remaining_d$remaining_minute<-minute(raw_remaining_d$time)

#잔여량 데이터를 일별로 분할하여 저장
day_uni<-unique(raw_remaining_d$day_all)
day_uni<-day_uni[!is.na(day_uni)]

for(i in day_uni){

  raw_remaining_d_temp<-raw_remaining_d %>% filter(day_all==i)
  
  day_each<-gsub('-','',i)
  day_each<-gsub(' ','',day_each)
  
  write.csv(raw_remaining_d_temp,paste0('./preprocess_remaining_data/remaining_',day_each,'.csv'),row.names = F)
  print(paste0('complete_',i,'_','remaining_data'))
}


#########################################################################################################
#전처리된 이동데이터 로드
raw_frame_d_act<-fread('./preprocess_moving_data/preprocessing_moving_data.csv')
#raw_frame_d_act<-raw_frame_d_act %>% filter(rental_day_all>=20190401)

#이동 데이터 전처리 (대여 시간 년,월,일,시간,분 칼럼 생성)
raw_frame_d_act$rental_time<-ymd_hms(raw_frame_d_act$rental_time)
raw_frame_d_act$rental_year<-year(raw_frame_d_act$rental_time)
raw_frame_d_act$rental_month<-month(raw_frame_d_act$rental_time)
raw_frame_d_act$rental_day<-day(raw_frame_d_act$rental_time)
raw_frame_d_act$rental_hour<-hour(raw_frame_d_act$rental_time)
raw_frame_d_act$rental_half_hour<-2*hour(raw_frame_d_act$rental_time)
raw_frame_d_act$rental_minute<-minute(raw_frame_d_act$rental_time)
raw_frame_d_act$rental_half_hour<-ifelse(raw_frame_d_act$rental_minute>30,raw_frame_d_act$rental_half_hour+1,raw_frame_d_act$rental_half_hour)

#이동 데이터 전처리 (반납 시간 년,월,일,시간,분 칼럼 생성)
raw_frame_d_act$back_time<-ymd_hms(raw_frame_d_act$back_time)
raw_frame_d_act$back_year<-year(raw_frame_d_act$back_time)
raw_frame_d_act$back_month<-month(raw_frame_d_act$back_time)
raw_frame_d_act$back_day<-day(raw_frame_d_act$back_time)
raw_frame_d_act$back_hour<-hour(raw_frame_d_act$back_time)
raw_frame_d_act$back_half_hour<-2*hour(raw_frame_d_act$back_time)
raw_frame_d_act$back_minute<-minute(raw_frame_d_act$back_time)
raw_frame_d_act$back_half_hour<-ifelse(raw_frame_d_act$back_minute>30,raw_frame_d_act$back_half_hour+1,raw_frame_d_act$back_half_hour)

#대여량 데이터 30분단위로 추출하여 생성   
raw_frame_d_rental_act<-raw_frame_d_act %>% 
  group_by(rental_shop_number,rental_day_all,rental_hour,rental_half_hour) %>%
  summarise(count_rental=n()) %>%
  filter(rental_half_hour%%2==0) %>%
  arrange(.,rental_shop_number,rental_day_all,rental_half_hour) %>%
  as.data.table()

#반납량 데이터 30분단위로 추출하여 생성   
raw_frame_d_back_act<-raw_frame_d_act %>% 
  group_by(back_shop_number,back_day_all,back_hour,back_half_hour) %>%
  summarise(count_back=n()) %>%
  filter(back_half_hour%%2==0) %>%
  arrange(.,back_shop_number,back_day_all,back_half_hour) %>%
  as.data.table()

write.csv(raw_frame_d_rental_act,'./preprocess_moving_data/rental_act_all.csv',row.names = F)
write.csv(raw_frame_d_back_act,'./preprocess_moving_data/back_act_all.csv',row.names = F)


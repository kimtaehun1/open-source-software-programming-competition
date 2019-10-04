#대여 데이터 에서 대여가 발생하지 않은 정류소와 시간에 0을 채워넣는 함수
interpolation_data_half_hour<-function(temp_data){
  
  half_hour<-rep(c(0:47),length(unique(temp_data$rental_day_all)))
  unique_Day<-rep(sort(unique(temp_data$rental_day_all)),each=48)
  key_table_day_hour<-data.frame(key=paste0(unique_Day,'_',half_hour),rental_day_all=unique_Day,rental_half_hour=half_hour)
  
  temp_data_all<-data.frame()
  for(i in unique(temp_data$rental_shop_number)){
    temp_data_sep <-temp_data %>% filter(rental_shop_number==i)
    temp_data_sep$key<-paste0(temp_data_sep$rental_day_all,'_',temp_data_sep$rental_half_hour)
    temp_data_sep$rental_day_all<-NULL
    temp_data_sep$rental_half_hour<-NULL
    temp_data_sep<-merge(key_table_day_hour,temp_data_sep,by = c('key') ,all.x = T)
    
    temp_data_sep$count_rental[is.na(temp_data_sep$count_rental)]<-0
    temp_data_sep$rental_year<-substr(temp_data_sep$key,1,4)
    temp_data_sep$rental_month<-substr(temp_data_sep$key,5,6)
    temp_data_sep$rental_day<-substr(temp_data_sep$key,7,8)
    
    temp_data_sep<-apply(temp_data_sep,2,function(col_data){
      #col_data=temp_data_sep[,2]
      if(any(is.na(col_data))){
        fill_unique_value=col_data[!is.na(col_data)]
        col_data[is.na(col_data)]<-fill_unique_value
        return(col_data)
        
      }else{
        return(col_data)
      }
    })
    temp_data_sep<-temp_data_sep %>% data.frame()
    temp_data_sep<-arrange(temp_data_sep,rental_day_all,rental_half_hour)
    temp_data_sep$count_rental<-as.numeric(as.character(temp_data_sep$count_rental))
    temp_data_all<-rbind(temp_data_all,temp_data_sep)
    print(paste0('interpolation_processing_:',which(unique(temp_data$rental_shop_number)==i),'/',length(unique(temp_data$rental_shop_number))))
    
  }
  
  return(temp_data_all)
}


making_model<-function( raw_frame_d=raw_frame_d,
                        start_date=start_date,
                        end_date=end_date,
                        model_type=model_type,
                        model_method=model_method
                        )
  
  
{
#전처리된 이동 데이터 로드(설정된 날짜에 포함되는 데이터만)
raw_frame_d <-raw_frame_d[rental_day_all>=start_date & rental_day_all<=end_date,] %>% as.data.table()


if(model_type=='Demand'){
#대여량 예측모형(평균) 생성코드  
  col_need<-c('rental_time','rental_shop_number','rental_shop_name','rental_day_all')
  rental_model_data<-raw_frame_d[,col_need,with=F]
  
#필요한 시간 변수들 생성  
  rental_model_data$rental_time<-ymd_hms(rental_model_data$rental_time)
  rental_model_data$rental_year<-year(rental_model_data$rental_time)
  rental_model_data$rental_month<-month(rental_model_data$rental_time)
  rental_model_data$rental_day<-day(rental_model_data$rental_time)
  rental_model_data$rental_hour<-hour(rental_model_data$rental_time)
  rental_model_data$rental_half_hour<-2*hour(rental_model_data$rental_time)
  rental_model_data$rental_minute<-minute(rental_model_data$rental_time)
  
  rental_model_data$rental_half_hour<-ifelse(rental_model_data$rental_minute>30,rental_model_data$rental_half_hour+1,rental_model_data$rental_half_hour)

#대여량 데이터를 30분 단위로 통합  
  rental_model_data<-rental_model_data %>% 
    group_by(rental_shop_number,rental_shop_name,rental_day_all,rental_year,rental_month,rental_day,rental_half_hour) %>%
    summarise(count_rental=n()) %>%
    arrange(.,rental_shop_number,rental_day_all,rental_half_hour) %>%
    as.data.table()
  
#interpolation 과정을 통해 대여가 일어나지 않은 구간에 0을 채움
  rental_model_data<-interpolation_data_half_hour(rental_model_data)
  print('interpolation_done')
  
  if(model_method=='ALL'){
#정류소별 대여량 평균 모형 
        model_make_data  <-  rental_model_data %>%
                      group_by(rental_shop_number,rental_shop_name) %>%
                       summarise(mean_rental=mean(count_rental)) %>% 
                        as.data.table()
    model_make_data$model_flag<-'ALL'
    
  }else if(model_method=='TIME'){
#정류소별 대여량 시간단위 평균 모형 (정류소와 시간을 key로 평균을 냄) 
    
      rental_model_data$rental_hour<-as.numeric(as.character(rental_model_data$rental_half_hour)) %/% 2
  
      model_make_data  <-  rental_model_data %>%
      group_by(rental_shop_number,rental_shop_name,rental_hour) %>%
      summarise(mean_rental=mean(count_rental)) %>% 
      as.data.table()
      model_make_data$model_flag<-model_make_data$rental_hour
      model_make_data$rental_hour<-NULL
  }
  
  write.csv(model_make_data,paste0('./model/',model_type,'/',model_method,'/',model_type,'_',model_method,'_',start_date,'_',end_date,'.csv'),row.names = F)
  
  
}else if(model_type=='Markov'){
  
#반납량 예측모형 생성코드(마코프)  
#마코프 모형 행렬 생성 (정류소 x 정류소)  
if(model_method=='ALL'){
  model_make_data<-raw_frame_d %>% 
    group_by(rental_shop_number,back_shop_number,rental_shop_name,back_shop_name) %>%
    summarise(count_n=n()) %>%
    arrange(.,rental_shop_number,back_shop_number)%>%
    as.data.table()
  model_make_data<-model_make_data[model_make_data$count_n>5,]
  
  prop_table_all<-data.frame()
  for(k in 1:length(unique(model_make_data$rental_shop_number))){
    
    prop_tab_temp<-prop.table(model_make_data[model_make_data$rental_shop_number==unique(model_make_data$rental_shop_number)[k],5])
    prop_table_all<-rbind(prop_table_all,prop_tab_temp)
  }
  
  model_make_data$prop<-prop_table_all$count_n
  model_make_data<-model_make_data[c(order(model_make_data$rental_shop_number,-model_make_data$prop)),]
  }else if(model_method=='Month'){
 }
write.csv(model_make_data,paste0('./model/',model_type,'/',model_method,'/',model_type,'_',model_method,'_',start_date,'_',end_date,'.csv'),row.names = F)
  }

}

demand_data_making<-function( raw_frame_d=raw_frame_d,
                              start_date=start_date,
                              end_date=end_date
)
  
  
{
#마코프 모형에 사용될 대여량 데이터 분할(설정된 기간 만큼)  
  raw_frame_d <-raw_frame_d[rental_day_all>=start_date & rental_day_all<=end_date,] %>% as.data.table()
  
  
#필요한 칼럼 추출
    col_need<-c('rental_time','back_time','rental_shop_number','rental_day_all')
    demand_data_made_f<-raw_frame_d[,col_need,with=F]

#대여량 데이터 전처리 (대여 시간 년,월,일,시간,분 칼럼 생성)
    demand_data_made_f$rental_time<-ymd_hms(demand_data_made_f$rental_time)
    demand_data_made_f$back_time<-ymd_hms(demand_data_made_f$back_time)
    
    demand_data_made_f$rental_hour<-hour(demand_data_made_f$rental_time)
    demand_data_made_f$back_hour<-hour(demand_data_made_f$back_time)
    demand_data_made_f$rental_minute<-minute(demand_data_made_f$rental_time)
    
    #demand_data_made_f$key<-paste0(demand_data_made_f$rental_day_all,demand_data_made_f$rental_hour)
    
    days<-unique(demand_data_made_f$rental_day_all)

#대여량 데이터를 하루 단위로 분할 후 저장      
    for(i in days){
        temp_demand_data<-demand_data_made_f[demand_data_made_f$rental_day_all==i]
        temp_demand_day_one<-data.frame()
        for(j in unique(demand_data_made_f$rental_hour)){
            if(j!=23){

#t시점에서 t-1시점까지 발생한 대여량을 계산후 시간단위로 합하여 저장함              
              temp_demand_data_time<-temp_demand_data[temp_demand_data$rental_hour==j & temp_demand_data$back_hour!=j,]  
              temp_demand_data_time_group<-temp_demand_data_time %>%
                                           group_by(rental_shop_number,rental_hour) %>%
                                           summarise(sum_demand=n()) %>%
                                           as.data.table()
              
              temp_demand_data_time_group$rental_hour<-temp_demand_data_time_group$rental_hour+1
              
              #add the unique rental_shop_number_all with merge
              temp_demand_day_one<-rbind(temp_demand_day_one,temp_demand_data_time_group)
            }
          } 
      write.csv(temp_demand_day_one,paste0('./preprocess_demand_data_markov/demand_',i,'.csv'),row.names = F)
      print(paste0('complete_',i,'_','demand_data_making'))
    }  
  
}


Validation_remain<-function(predict_day){
#예측모형에 비교될 실제 대여량 및 반납량 데이터 로드  
  raw_frame_d_rental_act<-fread('./preprocess_moving_data/rental_act_all.csv')
  raw_frame_d_back_act<-fread('./preprocess_moving_data/back_act_all.csv')
  
  raw_frame_d_rental_act<-raw_frame_d_rental_act[raw_frame_d_rental_act$rental_day_all==predict_day,]
  raw_frame_d_back_act<-raw_frame_d_back_act[raw_frame_d_back_act$back_day_all==predict_day,]
  
  raw_frame_d_rental_act$rental_shop_number<-as.character(raw_frame_d_rental_act$rental_shop_number)
  raw_frame_d_back_act$back_shop_number<-as.character(raw_frame_d_back_act$back_shop_number)

#실제 잔여량 데이터 로드  
  remaining_path<-paste0('./preprocess_remaining_data/','remaining_',predict_day,'.csv')
  remaining_data_use_act<-fread(remaining_path)

#00시 15~45분 잔여량 데이터 중 한번이라도 품절이 발생하면 1로 입력 아니면 0   
 binary_var <-  remaining_data_use_act %>% 
              filter(remaining_minute>15 & remaining_minute<45) %>%
              group_by(shop_number,remaining_hour) %>%
              summarise(binary_soldout=as.numeric(any(parkingBikeTotCnt==0))) %>%
              as.data.table()
#00시 15~45분 평균 잔여량
 remaining_data_use_act_2<- remaining_data_use_act %>% 
                            filter(remaining_minute>15 & remaining_minute<45) %>%
                            group_by(shop_number,remaining_hour) %>%
                            summarise(mean_remain_act=mean(parkingBikeTotCnt,na.rm=T)) %>%
                            as.data.table()
   
#예측한 잔여량 데이터 로드  
  predict_path<-paste0('./predict_remain_data/predict_remain_',predict_day,'.csv')
  remaining_data_use_predict<-fread(predict_path)
  
  evaluation_result<-merge(remaining_data_use_act_2,remaining_data_use_predict,by.x = c('shop_number','remaining_hour'),by.y = c('shop_number','hour'),all.x = T) 
  evaluation_result<-evaluation_result[!is.na(evaluation_result$shop_name),]
  evaluation_result<-merge(evaluation_result,binary_var,by.x = c('shop_number','remaining_hour'),by.y = c('shop_number','remaining_hour'),all.x = T)

#예측데이터와 실제 데이터 병합    
  evaluation_result$abs_error<-round(abs(evaluation_result$mean_remain_act-evaluation_result$remain_predict)) 
  evaluation_result<-merge(evaluation_result,raw_frame_d_rental_act[,c(1,3,5)],all.x = T,by.x=c("shop_number","remaining_hour"),by.y = c('rental_shop_number','rental_hour'))
  evaluation_result<-merge(evaluation_result,raw_frame_d_back_act[,c(1,3,5)],all.x = T,by.x=c("shop_number","remaining_hour"),by.y = c('back_shop_number','back_hour'))
  evaluation_result$count_rental[is.na(evaluation_result$count_rental)]<-0
  evaluation_result$count_back[is.na(evaluation_result$count_back)]<-0
  evaluation_result$binary_soldout_predict<-ifelse(evaluation_result$remain_predict<0.5,1,0)
  evaluation_result<-evaluation_result[!is.na(evaluation_result$binary_soldout_predict),]

#예측결과 confusion Matrix 및 성능 평가  
  result_eval<-confusionMatrix(as.factor(evaluation_result$binary_soldout_predict), as.factor(evaluation_result$binary_soldout),positive='1')
  print(result_eval)
  print(paste0("predict_day:",predict_day,' Precision :',round(result_eval$byClass[5],3 )))
  print(paste0("predict_day:",predict_day,' Recall :',round(result_eval$byClass[6],2 )))
  print(paste0("predict_day:",predict_day,' F1 Score :',round(result_eval$byClass[7],2 )))
  
  write.csv(evaluation_result,paste0('./validation_data/','evaluation_remain_',predict_day,'.csv'),row.names = F)
}


# Definition Function : 'Current residual quantity data' x 'Markov Transition Matrix' = 'Calculated return amount'
Markov_Function <- function(markov_model,demand) 
{ 
  
  
  demand$shop_number = as.character(demand$shop_number)
  markov_model$rental_shop_number = as.character(markov_model$rental_shop_number)
  
  # Merge : Between Markov Model and Demand Data
  merged_df = merge(markov_model,demand,by.x='rental_shop_number',by.y='shop_number')
  
  # Compute : Predicted Return amount
  merged_df$'Pred_return_byRental' = merged_df$sum_demand * merged_df$prop

  # Compute : Add all of the predicted amounts returned by the return station
  Pred_return = tapply(merged_df$Pred_return_byRental, merged_df$back_shop_number, sum)
  Pred_return = data.frame(shop_number=names(Pred_return),Pred_return_byBack=Pred_return)
  rownames(Pred_return)<-1:nrow(Pred_return)
  
  return(Pred_return)
}



predict_remain<-function(markov_model_type,
                         markov_model_name,
                         demand_model_type,
                         demand_model_name,
                         predict_day
                         ){

#경로설정  
  markov_model_path<-paste0('./model/Markov/',markov_model_type,'/')
  demand_model_path<-paste0('./model/Demand/',demand_model_type,'/')

#마코프모형과 평균대여량 모형 데이터 로드  
  markov_model_list<-list.files(markov_model_path)
  demand_model_list<-list.files(demand_model_path)
 
  markov_model_list<-markov_model_list[grep(markov_model_name,markov_model_list)]
  demand_model_list<-demand_model_list[grep(demand_model_name,demand_model_list)]
  
  markov_model<-fread(paste0(markov_model_path,markov_model_list))
  demand_model<-fread(paste0(demand_model_path,demand_model_list))
  colnames(demand_model)[c(1,3)]<-c('shop_number','mean_demand_pre')

#마코프 모형에 사용될 대여량 데이터 로드  
  demand_data_use_in_markov_path<-paste0('./preprocess_demand_data_markov/','demand_',predict_day,'.csv')
  demand_data_use_in_markov<-fread(demand_data_use_in_markov_path)

#현재 잔여량 데이터 로드  
  remaining_path<-paste0('./preprocess_remaining_data/','remaining_',predict_day,'.csv')
  remaining_data_use<-fread(remaining_path)
  
  predict_data_all<-data.frame()
  for(i in unique(demand_data_use_in_markov$rental_hour)){

    if(demand_model_type=='ALL'){
        demand_model_temp<-demand_model  
    }else if(demand_model_type=='TIME'){
        demand_model_temp<-demand_model[demand_model$model_flag==i]
    }
    
#예측할 1~24시00분 잔여량 및 대여량 데이터만 추출    
    demand_data_use_in_markov_temp<-demand_data_use_in_markov %>% filter(rental_hour==i)  
    remaining_data_use_temp<-remaining_data_use %>% filter(remaining_hour==i & remaining_minute==0)
    
    demand_data_use_in_markov_temp_RM_NA<-merge(remaining_data_use_temp[,c(4,5)],demand_data_use_in_markov_temp[,-2],by.x ='shop_number' ,by.y ='rental_shop_number' ,all.x=T)
    demand_data_use_in_markov_temp_RM_NA$sum_demand[is.na(demand_data_use_in_markov_temp_RM_NA$sum_demand)]<-0

#마코프 모형을 이용해 반납량 예측    
    Markov_return_result<-Markov_Function(markov_model,demand_data_use_in_markov_temp_RM_NA)
    
    predict_data<-data.frame(shop_number=remaining_data_use_temp$shop_number,shop_name=remaining_data_use_temp$shop_name,remain_now=remaining_data_use_temp$parkingBikeTotCnt)
    predict_data<-merge(predict_data,Markov_return_result,by=c('shop_number'),all.x = T)
    predict_data<-merge(predict_data,demand_model_temp[,c(1,3)],by=c('shop_number'),all.x = T)

#마코프 모형을 이용해 예측된 반납량에 평균대여 모형을 이용한 대여량을 빼서 미래 잔여량 예측     
    predict_data$remain_predict<-predict_data$remain_now+predict_data$Pred_return_byBack-predict_data$mean_demand_pre
    predict_data$hour<-i
    predict_data_all<-rbind(predict_data_all,predict_data)
  }
  
  predict_data_all<-arrange(predict_data_all,shop_number,hour)
  
  write.csv(predict_data_all,paste0('./predict_remain_data/','predict_remain_',predict_day,'.csv'),row.names = F)
}



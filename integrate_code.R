######################################################################################
#파라미터 변경 방법
#
#1)마코프 모형 생성코드 start_date , end_date 변경(단, 데이터가 20190401-20190429 사이의 날짜여야함)
#2)평균수요 모형 생성코드 start_date , end_date 변경(단, 데이터가 20190401-20190429 사이의 날짜여야함)
#3)데여데이터 생성코드 start_date , end_date 변경(단, 데이터가 20190401-20190429 사이의 날짜여야함)
#4)잔여량 예측 코드 markov_model_name 과 demand_model_name 값 변경 
#   (단, 생성한 마코프 모형 생성과 평균수요 모형의 날짜에 '_'를 붙여서 입력해야함 
#    ex) 20180401_20180410
#5)잔여량 예측 평가 코드 predict_day 변경
#######################################################################################
list.of.packages <- c("data.table","dplyr","lubridate","caret","e1071","readr")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(data.table)
library(dplyr)
library(lubridate)
library(caret)
library(e1071)
library(readr)

source('preprocessing_code.R')
source('model_making.R')
source('demand_data_making.R')
source('predict_remain.R')
source('validation_remain.R')

###########################################################################################
raw_frame_d<-fread('./preprocess_moving_data/preprocessing_moving_data.csv')

#마코프 모형 생성코드
making_model(raw_frame_d = raw_frame_d ,
             start_date = 20190401 ,
             end_date = 20190427 ,
             model_type = 'Markov' ,
             model_method = 'ALL'
               )
#평균수요 모형 생성코드
making_model(raw_frame_d = raw_frame_d ,
             start_date = 20190401 ,
             end_date = 20190427 ,
             model_type = 'Demand' ,
             model_method = 'ALL'
)

#모델에 입력되는 대여데이터 생성코드
demand_data_making(raw_frame_d = raw_frame_d,
                   start_date = 20190401,
                   end_date = 20190430)

#잔여량 예측 코드
predict_remain( markov_model_type='ALL',
                demand_model_type='TIME',
                markov_model_name='20190401_20190427',
                demand_model_name='20190401_20190427',
                predict_day='20190428')

#잔여량 예측 평가 코드
Validation_remain(predict_day='20190428')

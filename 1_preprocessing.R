## Preparing

#install package!!!
# devtools::install_github("talbano/epmr")
# devtools::install_github("strengejacke/strengejacke")

library(tidyverse) #install.packages("tidyverse")
library(readxl);library(xlsx);library(openxlsx); #install.packages("readXL") : excel, csv 파일 열기


## Set working directory and Loading dataset
rm(list=ls())
setwd("C:/Users/thinkpad/Documents/housing_price_change/0_DataSet/7_한국노동패널 1-24차 release (Excel)/data")

getwd()
options(warn=-1) # Ignore less important warning messages
graphics.off()
# cat('/014')
list.files()


## data24h_preprocessing
data24h <- read.csv("klips24h.csv", , header = T, stringsAsFactors = F, fileEncoding = "euc-kr") #가구 코드북
data24h <- subset(data24h, select = c(h240221, h241406, h241407, h241401, 
                                      h242562, h241410, h241415, h241412, h242566,
                                      h242632, h242502, h242513, h240150, h242102,
                                      h241407, h242502)) 
names(data24h) <- c('pid', 'residence_type', 'house_type', 'move24', 
                    'f_assets_deposit', 'housing_area', 'start_stay', 'housing_price24', 'f_savings_insurance',
                    'debt', 'more_housing', 'p_all_property_price', 'family_num', 'income_year',
                    'house_type', 'another_real_estate')

## data24p_Import data
# data24p <- read_excel("klips24p.xlsx") #가구원 코드북
data24p <- read.csv("klips24p.csv", , header = T, stringsAsFactors = F, fileEncoding = "euc-kr") #가구원 코드북

# Data Selection_data24p
data24p <- subset(data24p, select = c(
  pid,
  p240107, #연령
  p240101, #성별 1:남, 2:여
  p240121, #거주지역, 1:서울, 2:부산, 3:대구, 4:대전, 5:인천, 6:광주, 7:울산, 8:경기, 9:강원,
                   # 10:충북, 11:충남, 12:전북, 13:전남, 14:경북, 15:경남, 16:제주도, 17:이북, 18:외국, 19:세종
  p240110, #학력, 1:미취학, 2:무학, 3:초등학교, 4:중학교, 5:고등학교, 6:2년제, 7:4년제, 8:석사, 9:박사
  p240108, #동거여부
  p240102, #가구주와의 관계 10:가구주
  p245501, #결혼(혼인상태)
  p241701, #근로소득 여부
  p246101, #건강상태, 1:아주 건강하다, 2;건강한 편, 3:보통, 4:건강하지 않음, 5:아주 안좋음
  p246508 #전반적 생활만족도
  # p246503, #생활만족도-주거환경, 1:매우만족, 2:만족, 3:보통, 4:불만족, 5:매우 불만족
))

# ownhome, #입주형태 1:자가, 2:전세, 3:월세, 4:기타
# home_type, #주택종류 1:단독주택, 2:아파트, 3:연립주택, 4:다세대주택, 5:상가주택, 6:기타
#more housing
# h242102, #(작년한해)총근로소득(만)
# h242502, #부동산 소유 1:주택, 2:건물, 3:임야, 4:토지
# h242513, #소유부동산 총액(범주), 1:1천만 미만, 2:1천~2천5백만, 3:2천5백~5천만 미만,
#4:5천~7천5백만미만, 5:7천5백만~1억원 미만, 6:1~2억원 미만, 7:2~3억원 미만, 
#8:3~4억원 미만, 9:4~5억원 미만, 10:5억~10억원 미만, 11:10억원 이상))

data24p <- data24p |> rename(
  age = p240107, #연령
  gender = p240101, #성별
  sido = p240121,  #거주지역
  edu = p240110, #학력
  cohabitation = p240108, #동거여부
  heads = p240102, #가구주와의 관계 10:가구주
  marriage = p245501, #작년말 기준 혼인상태 - 가구원01. 1:기혼(배우자있음), 2:기혼(사별), 3:기혼(이혼), 미혼(결혼한적없음)
  now_worker = p241701, #근로소득 여부
  health = p246101, #건강
  sati = p246508) #전반적 생활만족도

data24 <- merge(data24h, data24p, by = "pid")
# df <- raw_df





## DATA Conversion
data24$gender = factor(data24$gender, levels = c(1, 2),
                   labels = c('1: male', '2: female'))
data24$sido = factor(data24$sido, levels = c(1, 5, 8),
                 labels = c('1: Seoul', '2: Inchon', '3: Gyeonggi-do'))
data24$health[data24$health == -1] <- NA
data24$health_f = factor(data24$health, levels = c(1, 2, 3, 4, 5),
                     labels = c('1: Very_Bad', '2: Bad', 'Normal', '4: Good', '5: Very_Good'))
data24$age_f = ifelse(data24$age <= 39, 1,
                  ifelse(data24$age <= 49, 2,
                         ifelse(data24$age <= 59, 3,
                                ifelse(data24$age <= 69, 4,
                                       ifelse(data24$age <= 79, 5, 
                                              ifelse(data24$age <= 89, 6, 7))))))
data24$age_f = factor(data24$age_f, levels = c(1, 2, 3, 4, 5, 6, 7),
                  labels = c('30s','40s', '50s', '60s', '70s', '80s', 'over90'))
data24$sati[data24$sati==(-1)] <- NA
data24$sati = ifelse(data24$sati==1, 1, 
                 ifelse(data24$sati==2, 2, 
                        ifelse(data24$sati==3, 3,
                               ifelse(data24$sati==4, 4, 5))))
data24$sati_f = factor(data24$sati, levels = c(1, 2, 3, 4, 5),
                   labels = c('1: Very Bad', '2: Bad', '3: Normal', '4: Good', '5: Very Good'))
data24$house_type_f = factor(data24$house_type, levels = c(1, 2, 3, 4, 5, 6),
                         labels = c('1: detached_house', '2: apt', '3: row_house', '4: multi_complex_house', 
                                    '5: mixed_apt', '6: etc'))
data24$cohabitation_f = factor(data24$cohabitation, levels = c(1, 2),
                           labels = c('livewith', 'non-livewith'))
data24$edu = ifelse(data24$edu <= 5, 1, 
                ifelse(data24$edu <= 7, 2, 3))
data24$edu_f = factor(data24$edu, levels = c(1, 2, 3),
                  labels = c('1: Middle', '2: University', '3: Graduate'))
data24$another_real_estate_f = factor(data24$another_real_estate, levels = c(1,2,3,4,5),
                                  labels = c('1: house', '2: architect','3: woods&field','4: land','5: etc'))
# mutate(data24, income_month = income_year/12)
# data24$income_month = ifelse(data24$income_month<100, 1,
#                          ifelse(data24$income_month<200, 2,
#                                 ifelse(data24$income_month<300, 3,
#                                        ifelse(data24$income_month<400, 4,
#                                               ifelse(data24$income_month<500, 5,
#                                                      ifelse(data24$income_month<600, 6,
#                                                             ifelse(data24$income_month<700, 7,
#                                                                    ifelse(data24$income_month<800, 8,
#                                                                           ifelse(data24$income_month<900, 9,
#                                                                                  ifelse(data24$income_month<1000, 10, 11))))))))))
# data24$income_month_f = factor(data24$income_month, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
#                            labels = c('1: A_less than 1 million won', '2: Around 1 million won',
#                                       '3: Around 2 million won', '4: Around 3 million won',
#                                       '5: Around 4 million won', '6: Around 5 million won',
#                                       '7: Around 6 million won', '8: Around 7 million won',
#                                       '9: Around 8 million won', '10: Around 9 million won',
#                                       '11: Over 10 million won'))
# data24 <-  data24 |> filter(data24$move24 == 2) #이사 여부, 1:있다. 2:없다


## data20h_preprocessing
data20h <- read.csv("klips20h.csv", header = T, stringsAsFactors = F, fileEncoding = "euc-kr")
data20h <- subset(data20h, select = c(h200221, h201401, h201412)) 
names(data20h) <- c('pid', 'housing_price20')

## data21h_preprocessing
data21h <- read.csv("klips21h.csv", header = T, stringsAsFactors = F, fileEncoding = "euc-kr")
data21h <- subset(data21h, select = c(h210221, h211401, h211412)) 
names(data21h) <- c('pid', 'move21', 'housing_price21')
# data21h <- data21h |> filter(move21 == 2) #이사 여부, 1:있다. 2:없다

## data22h_preprocessing
data22h <- read.csv("klips22h.csv", header = T, stringsAsFactors = F, fileEncoding = "euc-kr")
data22h <- subset(data22h, select = c(h220221, h221401, h221412)) 
names(data22h) <- c('pid', 'move22', 'housing_price22')
# data22h <- data22h |> filter(move22 == 2) #이사 여부, 1:있다. 2:없다


## data23h_preprocessing
data23h <- read.csv("klips23h.csv", header = T, stringsAsFactors = F, fileEncoding = "euc-kr")
data23h <- subset(data23h, select = c(h230221, h231401, h231412)) 
names(data23h) <- c('pid', 'move23', 'housing_price23')
# data23h <- data23h |> filter(move23 == 2) #이사 여부, 1:있다. 2:없다



df1 <- merge(data24, data23h, by = "pid")
df2 <- merge(df1, data22h, by = "pid")
df <- merge(df2, data21h, by = "pid")
str(df)
# df <- Reduce(function(x, y) merge(x, y, by= 'pid', all=TRUE), list(data20h, data21h, data22h, data23h, df))
# df <- Reduce(function(x, y) merge(x, y, by= 'pid'), list(data20h, data21h, data22h, data23h, df))

df <- data.frame(df)
df <- df |> mutate(housing_price_change = housing_price24 - housing_price21)
df$housing_price_change_f = ifelse(df$housing_price_change < 0, 1,
                                 ifelse(df$housing_price_change == 0, 2, 3))
df$housing_price_change_f = factor(df$housing_price_change_f, levels = c(1, 2, 3),
                     labels = c('1: Price_Down', '2: No_Change', '3: Price_Up'))  

df <- df |> filter(!is.na(housing_price24))
df <- df |> filter(!is.na(housing_price21))

save.image()
# write.csv(df, "./test_df_preprocess_klips.csv")

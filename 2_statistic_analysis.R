## Preparing

#install package!!!
# devtools::install_github("talbano/epmr")
# devtools::install_github("strengejacke/strengejacke")

library(tidyverse) #install.packages("tidyverse")
library(readxl);library(xlsx);library(openxlsx); #install.packages("readXL") : excel, csv 파일 열기
library(psych) #install.packages("psych") : kurtosi, skew
library(descr) #install.packages("descr") : freq(빈도)
library(car) #install.packages("car") :다중공선성테스트, vif, ncvtest
library(outreg) #install.packages("outreg") : model1+model2+model3
library(lm.beta) #install.packages("lm.beta") : 표준화계수
library(lawstat) #install.packages("lawstat") : 등분산성 검정
library(agricolae) #install.packages("agricolae") : anova 사후검정(turkey검정)
library(ggfortify) #install.packages("ggfortify") : 잔차분석 산점도 그릴때
library(lmtest) #install.packages("lmtest") : 회귀결과 이분산성 테스트
library(ggmosaic) #install.packages("ggmosaic") : 막대 그래프 그리기1
library(gmodels) #install.packages(“gmodels”) : 막대 그래프 그리기2
library(DMwR2) # 결측 값 처리를 지원하는 패키지
library(patchwork) # ggplot 플롯 붙여서 그리기
library(dagitty) # causal diagrams 그리기, install_github("jtextor/dagitty/r")
library(ggbump) # 일반적인 선그래프와는 달리 랭킹간 변화추이 그리기
library(DiagrammeR) #경로모형 그리기,  GraphViz와 Mermaid를 묶어서 연동해 놓은 패키지
library(gtsummary)
library(sjlabelled);library(sjmisc);library(sjstats);library(ggeffects);library(sjPlot);
library(corrplot);library(foreign);library(plyr);library(knitr);library(jmv);
library(lavaan);library(semPlot);library(summarytools);library(vcd);
library(epmr);library(ggplot2);library(GGally);library(DescTools);library(nord);
library(DT);library(fastDummies);library(plotly);library(lattice);
library(highcharter);library(kableExtra);library(stargazer);library(RColorBrewer);
library(diagram);library(nFactors);library(leaps);library(RColorBrewer);library(nnet);
library(MASS);library(AER);library(oglmx);library(VGAM);

## Set working directory and Loading dataset
rm(list=ls())
setwd("C:/Users/thinkpad/Documents/0_논문작성/0_DataSet/7_한국노동패널 1-24차 release (Excel)/data")
getwd()
options(warn=-1) # Ignore less important warning messages
graphics.off()
# cat('/014')
list.files()

some(data24h$h242402)
table(data24h$h242402)
col(data24h$h242402)

###########################################
############### 가구주 ####################
###########################################
## DATA Grouping, 가구주
df_heads <- df |> filter(heads == 10)

############
## 시각화 ##
############

# 가구주 연령대별로 만족도가 어떻게 다른지?
plot_xtab(df_heads$age_f, df_heads$sati_f, margin='row', bar.pos="stack", geom.colors = NULL)

# 18-21 무이동 자가가구, 연령대별로 삶의 만족도
df_heads_nomove <- df_heads |> filter(move24 == 2&move23 == 2&move22 == 2&move21 == 2) #이사 여부, 1:있다. 2:없다
plot_xtab(df_heads_nomove$age_f, df_heads_nomove$sati_f, margin='row', bar.pos="stack", geom.colors = NULL)
plot_scatter(df_heads_nomove, age_f, sati_f, rank, fit.grps = "lm", grid = T)

## 연령대별 주택가격 상승/하락 만족도
# 주택가격 상승집단 - 연령대별 만족도
df_heads_age_sati_priceup <- df_heads_nomove |> filter(housing_price_change_f == '3: Price_Up')
plot_xtab(df_heads_age_sati_priceup$age_f, df_heads_age_sati_priceup$sati_f, 
          margin='row', bar.pos="stack", geom.colors = NULL)
# 주택가격 하락집단 - 연령대별 만족도
df_heads_age_sati_priceDown <- df_heads_nomove |> filter(housing_price_change_f == '1: Price_Down')
plot_xtab(df_heads_age_sati_priceDown$age_f, df_heads_age_sati_priceDown$sati_f, 
          margin='row', bar.pos="stack", geom.colors = NULL)




##########
## 분석 ##
##########

## Ordinal Logistic Regression - 가구주
olm1 <- oglmx(sati_f ~ housing_price21*housing_tax_change_price+
                age_f+edu_f+gender+marriage+health_f+income_year+sido,
              data=df_heads, link="logit", constantMEAN = FALSE, constantSD = FALSE, 
              delta=0, threshparam = NULL);summary(olm1)



#############################################
############### 비가구주 ####################
#############################################
## DATA Grouping, 비가구주
df_N_heads <- df |> filter(heads != 10)


## 시각화 ##

# 비가구주 연령대별로 만족도가 어떻게 다른지?
plot_xtab(df_N_heads$age_f, df_N_heads$sati_f, margin='row', bar.pos="stack", geom.colors = NULL)

# 임차가구 만족도




############
## 분석 ##
############

## Ordinal Logistic Regression - 비가구주
olm10 <- oglmx(sati_f ~ housing_price21*housing_tax_change_price+
                 age_f+edu_f+gender+marriage+health_f+income_year+sido,
               data=df_N_heads, link="logit", constantMEAN = FALSE, constantSD = FALSE, 
               delta=0, threshparam = NULL);summary(olm10)


##################
## 기초통계분석 ##
##################

t.test(df$housing_price_change, df$sati_f)
t.test(df$housing_tax_change_price, df$sati_f)
aov(df$housing_price_change, df$sati_f)
var.test(df$housing_price_change, df$sati_f)
var.test(df$housing_tax_change_price, df$sati_f)
cor(df$housing_price24, df$sati_f)
cor(df$housing_tax_change_price, df$sati_f)
corr.test(df$housing_price24, df$sati_f)
table(df$home_type, df$housing_property_tax21,df$housing_price_change)

str(df$housing_property_tax21)
names(df)








## 기
#############################################################
## data24h_preprocessing
# df <- read.csv("data1.csv", header = T, stringsAsFactors = F, fileEncoding = "euc-kr")
# df <- df |> filter(!is.na(sati_f))
# df <- df |> filter(!is.na(X22_housing_price))
# df <- df |> filter(!is.na(X23_housing_price))
# df <- df |> filter(!is.na(housing_price_change))
# df <- df |> filter(!is.na(housing_property_tax21))
# 
# df$sati_f = factor(df$sati_f)
# df$gender = factor(df$gender)
# df$sido = factor(df$sido)
# df$residence_type = factor(df$residence_type)
# df$f_assets_deposit = factor(df$f_assets_deposit)
# df$f_savings_insurance = factor(df$f_savings_insurance)
# df$health_f = factor(df$health_f)
# df$age_f = factor(df$age_f)
# df$cohabitation_f = factor(df$cohabitation_f)
# df$edu_f = factor(df$edu_f)
# df$another_real_estate_f = factor(df$another_real_estate_f)
# df$income_month_f = factor(df$income_month_f)
# df$health_f = factor(df$health_f)
# df$house_type_f = factor(df$house_type_f)
# df$home_type = factor(df$home_type)
# df$residence_type = factor(df$residence_type, levels = c(1, 2, 3, 4),
#                            labels = c('1: Own', '2: Jeonse', '3: monthly_rent', '4: Etc'))
# df$debt = factor(df$debt, levels = c(1, 2),
#                            labels = c('1: have_debt', '2: No_debt'))
# df$p_all_property_price = factor(df$p_all_property_price, levels = c(1,2,3,4,5,6,7,8,9,10,11))
# df$more_housing = factor(more_housing)


# df <- df |> select(pid, age_f, edu_f, gender, sido, health_f, cohabitation_f, sati_f, 
#                    residence_type, house_type, f_assets_deposit, f_savings_insurance,
#                    housing_area, housing_price21, housing_price24, debt, p_all_property_price, income_year,
#                    housing_price_change, housing_property_tax21)


# write.csv(df, "./data1.csv")
save.image()

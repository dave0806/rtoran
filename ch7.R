# Ch7 빠진 데이터를 찾아라! 
## 1.1. 결측치 정제하기

df <- data.frame(sex=c("M", "F", NA, "M", "F"), score=c(5,4,3,4,NA)) #데이터프레임 만들기

is.na(df) #결측치가 있어요?

table(is.na(df)) #결측치인 데이터의 총 개수가 몇개인가요?

table(is.na(df$sex)) # 셩별 컬럼에서 결측치인 데이터의 총 개수는 몇개인가요?

table(is.na(df$score)) #점수 컬럼에서 결측치인 데이터의 총 개수는 몇개인가요?


## 1.2. 결측치 제외하기

library(dplyr) #dplyr 이라는 함수 꾸러미 쓰겠습니다.

df %>% filter(is.na(score))  # 점수 컬럼에서 결측치인 데이터만 출력해 주세요

df %>% filter(!is.na(score))  # 점수 컬럼에서 결측치인 데이터를 제외하고 출력해 주세요


## 1.3. 결측치 없는 데이터만 추출하기

df_nomiss <- df %>% filter(!is.na(score) & !is.na(sex))  # filter 함수를 사용하여 
                                                         ##하나 하나 결측치 제외하기 (ㅠㅠ)
df_nomiss

df_nomiss2 <- na.omit(df) #결측치면 제외해라 라는 함수식 (na.omit) 을 사용하여 결측치 제외하기
df_nomiss2

##1.4. 집계 함수에서 결측치 제외 기능 이용하기

mean(df$score, na.rm = T) # 평균 산출할 때 결측치 제외하고 

sum(df$score, na.rm = T) # 합계 산출할 때 결측치 제외하고 

getwd() #어디서 삽질하는지 확인

setwd(dir = "E:/R/RKOR") # 여기서 해야지라고 설정

getwd() # 잘 했는지 확인

exam <- read.csv("csv_exam.csv") #미리 만들어 저장해논 예제 파일 불러오기

exam[c(3,8,15), "math"] <- NA

exam

exam %>% summarise(
                    mean_math = mean(math, na.rm =T),
                    sum_math=sum(math, na.rm =T),
                    median_math=median(math, na.rm=T)
                    )

##1.4. 집계 함수에서 결측치 대체하기
exam$math <- ifelse(is.na(exam$math), mean(exam$math, na.rm=T), exam$math) #수학의 평균값으로 
                                                                           ##결측치 대체하기

table(is.na(exam$math))  #결측치 몇개 있어요?

exam



## 2.이상치 정제하기

##2.1. 이상치 확인하기
outlier <- data.frame(sex=c(1,2,1,3,2,1),
                      score=c(5,4,3,4,2,6)
                      )

outlier

table(outlier$sex)

table(outlier$score)

##2.2. 결측치로 대체하기

outlier$sex <- ifelse(outlier$sex ==3, NA, outlier$sex)

outlier

outlier$score <- ifelse(outlier$score >5, NA, outlier$score)

outlier

outlier %>%
  filter(!is.na(sex) & !is.na(score)) %>%
  group_by(sex) %>%
  summarise(mean_score = mean(score))    # 결측치 제외하고 성별로 점수 평균내기
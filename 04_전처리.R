
#--------------------------
# 4-1 불필요한 정보 지우기
#--------------------------
  
#---# [1단계: 아파트 실거래 자료 불러오기]

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
options(warn=-1)

load("./03_integrated/03_apt_price.rdata")  # 실거래 자료 불러오기
head(apt_price, 2)                          # 확인

#---# [2단계: 결측값 확인]

table(is.na(apt_price))         # 현재 결측값 확인
apt_price <- na.omit(apt_price) # 결측값 제거
table(is.na(apt_price))         # 결측값 제거 확인

head(apt_price$price, 2)      # 현재 결측값 확인  

library(stringr)              # 문자열 처리 패키지 실행
apt_price <- as.data.frame(apply(apt_price, 2, str_trim)) # 공백 제거
head(apt_price$price, 2)                                  # 확인

#--------------------------
# 4-2 항목별 데이터 다듬기
#--------------------------

#---# [1단계: 매매 연월일, 연월 데이터 만들기]

library(lubridate)  # install.packages("lubridate") / install.packages("cli") 
library(dplyr)      # install.packages("dplyr")
apt_price <- apt_price %>% mutate(ymd=make_date(year, month, day))  # 연월일
apt_price$ym <- floor_date(apt_price$ymd, "month")                  # 연월
head(apt_price, 2)                                                  # 확인

#---# [2단계: 매매가 확인]

head(apt_price$price, 3)

apt_price$price <- apt_price$price %>% sub(",","",.) %>% as.numeric() # 매매가 변환(문자 → 숫자)
head(apt_price$price, 3)  # 확인

#---# [3단계: 주소 조합]

head(apt_price$apt_nm, 30)  # 아파트 이름 현황

apt_price$apt_nm <- gsub("\\(.*","", apt_price$apt_nm) # 괄호이후 삭제
head(apt_price$apt_nm, 30)                             # 아파트 이름 확인

loc <- read.csv("./01_code/sigun_code/sigun_code.csv", fileEncoding="UTF-8")  # 지역코드 불러오기
apt_price <- merge(apt_price, loc, by = 'code')         # 지역명 결합하기
apt_price$juso_jibun <- paste0(apt_price$addr_2, " ", apt_price$dong," ",
                               apt_price$jibun," ",apt_price$apt_nm) # 주소조합
head(apt_price, 2)                                      # 확인

#---# [4단계: 건축연도 변환]

head(apt_price$con_year, 3)

apt_price$con_year <- apt_price$con_year %>% as.numeric()   # 건축연도 숫자변환
head(apt_price$con_year, 3)   # 건축연도 확인

#---# [5단계: 평당 매매가 만들기]

head(apt_price$area, 3)   # 확인

apt_price$area <- apt_price$area %>% as.numeric() %>% round(0)  # 전용면적 숫자변환
head(apt_price$area, 3)          # 확인

apt_price$py <- round(((apt_price$price/apt_price$area) * 3.3), 0) # 평당가격 계산
head(apt_price$py, 3)           # 확인


#---# [6단계: 층수 변환]

min(apt_price$floor)   # 확인

apt_price$floor <- apt_price$floor %>% as.numeric() %>% abs() # 층수 숫자변환
min(apt_price$floor)

apt_price$cnt <- 1   # 모든 데이터에 숫자 1 할당
head(apt_price, 2)   # 확인


#---------------
# 4-3 저장하기
#---------------

#---# [1단계: 필요칼럼 추출]

apt_price <- apt_price %>% select(ymd, ym, year, code, addr_1, apt_nm, 
              juso_jibun, price, con_year,  area, floor, py, cnt) # 칼럼 추출
head(apt_price, 2)  # 확인

#---# [2단계: 저장]

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
dir.create("./04_preprocess")   # 새로운 폴더 생성
save(apt_price, file = "./04_preprocess/04_preprocess.rdata") # 저장
write.csv(apt_price, "./04_preprocess/04_preprocess.csv") 

#----------------------
# 4-4 단골코드 정리하기
#----------------------

#---# 1) 날짜 만들기

year <- 2011:2020
month <- 2:11
day <- 3: 12
date <- data.frame(cbind(year, month, day))
date <- date %>% mutate(ymd=make_date(year, month, day))
head(date, 2)

#---# 2) 여러단계를 거쳐서 다항식 계산하기

a <- filter(mtcars, carb > 1)          # 계산 1: 필터링
b <- group_by(a, cyl)                  # 계산 2: 그룹화
c <- summarise(b, Avg_mpg = mean(mpg)) # 계산 3: 요약변수 생성
d <- arrange(c, desc(Avg_mpg))         # 계산 4: 높은순서 정렬
print(d)

#---# 3) 파이프라인 연산자를 활용하여 다항식 연산을 한번에 계산하기

mtcars %>%
  filter(carb > 1) %>%                 # 계산 1: 필터링
  group_by(cyl) %>%                    #         그룹화 
  summarise(Avg_mpg = mean(mpg)) %>%   #         요약변수 생성
  arrange(desc(Avg_mpg))               #         높은순서 정렬

#---# 4) 특수문자 제거: 공백과 콤마 제거 

library(stringr)
tmp <- " 324,135"     # 공백과 쉼표가 포함된 문자열
tmp %>% str_trim() %>% sub(",","",.) %>% as.numeric()  # 공백, 쉼표 제거







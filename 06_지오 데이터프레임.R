
#-------------------------
# 6-2 주소와 좌표 결합하기 
#-------------------------
  
#---# [1단계: 데이터 불러오기]

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
load("./04_preprocess/04_preprocess.rdata")    # 주소 불러오기
load("./05_geocoding/05_juso_geocoding.rdata") # 좌표 불러오기

#---# [2단계: 주소 + 좌표 결합]

library(dplyr)   # install.packages('dplyr')
apt_price <- left_join(apt_price, juso_geocoding, 
                       by = c("juso_jibun" = "apt_juso")) # 결합
apt_price <- na.omit(apt_price)   # 결측치 제거


#------------------------------
# 6-3: 지오 데이터프레임 만들기
#------------------------------
  
#---# [1단계: 지오데이터프레임 생성]

library(sp)    # install.packages('sp')
coordinates(apt_price) <- ~coord_x + coord_y    # 좌표값 할당
proj4string(apt_price) <- "+proj=longlat +datum=WGS84 +no_defs" # 좌표계(CRS) 정의
library(sf)    # install.packages('sf')
apt_price <- st_as_sf(apt_price)     # sp형 => sf형 변환

#---# [2단계: 지오데이터프레임 시각화]

plot(apt_price$geometry, axes = T, pch = 1)        # 플롯 그리기 
library(leaflet)   # install.packages('leaflet')   # 지도 그리기
leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data=apt_price[1:1000,], label=~apt_nm) # 일부분(1000개)만 그리기

#---# [3단계: 지오 데이터프레임 저장]

dir.create("06_geodataframe")   # 새로운 폴더 생성
save(apt_price, file="./06_geodataframe/06_apt_price.rdata") # rdata 저장
write.csv(apt_price, "./06_geodataframe/06_apt_price.csv")   # csv 저장


#----------------------
# 6-4 단골코드 정리하기
#----------------------

#---# 1) 좌표가 포함된 데이터프레임 만들기

df <- structure(
  list(longitude = c(128.6979, 153.0046, 104.3261, 124.9019, 
                     126.7328, 153.2439, 142.8673, 152.689), 
       latitude = c(-7.4197, -4.7089, -6.7541, 4.7817, 
                    2.1643, -5.65, 23.3882, -5.571)),
  .Names = c("coord_x", "coord_y"), class = "data.frame", row.names = c(NA, -8L))
head(df)

#---# 2) 지오데이터프레임으로 변환

library(sp)    
coordinates(df) <- ~coord_x + coord_y   # 좌표값 할당(sp형)
proj4string(df) <- "+proj=longlat +datum=WGS84 +no_defs" # 좌표계(CRS) 정의(sp형)
library(sf)    # install.packages('sf')
df <- st_as_sf(df)     # sp형 => sf형 변환
head(df)

#---# 3) 지도 시각화 

library(leaflet)  
leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data=df) 


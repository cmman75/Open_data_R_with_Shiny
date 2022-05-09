
#-----------------------------
# 7-1 어느 지역이 제일 비쌀까?
#-----------------------------

#---# [1단계: 지역별 평균 가격 구하기]

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # 작업폴더 설정
load("./06_geodataframe/06_apt_price.rdata")   # 실거래 불러오기
library(sf)    # install.packages("sf") 
grid <- st_read("./01_code/sigun_grid/seoul.shp")     # 서울시 1km 그리드 불러오기
apt_price <-st_join(apt_price, grid, join = st_intersects)  # 실거래 + 그리드 결합
head(apt_price)

kde_high <- aggregate(apt_price$py, by=list(apt_price$ID), mean) # 그리드별 평균가격(평당) 계산
colnames(kde_high) <- c("ID", "avg_price")   # 컬럼명 변경
head(kde_high, 2)     # 확인

#---# [2단계: 그리드 + 평균가격 결합]

kde_high <- merge(grid, kde_high,  by="ID")   # ID 기준으로 결합
library(ggplot2) # install.packages("ggplot2")
library(dplyr)   # install.packages("dplyr")
kde_high %>% ggplot(aes(fill = avg_price)) + # 그래프 시각화
  geom_sf() + 
  scale_fill_gradient(low = "white", high = "red")

#---# [3단계: 지도 경계 그리기]

library(sp) # install.packages("sp")
kde_high_sp <- as(st_geometry(kde_high), "Spatial")    # sf형 => sp형 변환
x <- coordinates(kde_high_sp)[,1]  # 그리드 x, y 좌표 추출
y <- coordinates(kde_high_sp)[,2] 

l1 <- bbox(kde_high_sp)[1,1] - (bbox(kde_high_sp)[1,1]*0.0001) # 그리드 기준 경계지점 설정
l2 <- bbox(kde_high_sp)[1,2] + (bbox(kde_high_sp)[1,2]*0.0001)
l3 <- bbox(kde_high_sp)[2,1] - (bbox(kde_high_sp)[2,1]*0.0001)
l4 <- bbox(kde_high_sp)[2,2] + (bbox(kde_high_sp)[1,1]*0.0001)

library(spatstat)  # install.packages("spatstat")
win <- owin(xrange=c(l1,l2), yrange=c(l3,l4)) # 지도 경계선 생성
plot(win)         # 지도 경계선 확인
rm(list = c("kde_high_sp", "apt_price", "l1", "l2", "l3", "l4")) # 변수 정리

#---# [4단계: 밀도 그래프 표시]

p <- ppp(x, y, window=win)  # 경계창 위에 좌표값 포인트 생성
d <- density.ppp(p, weights=kde_high$avg_price, # 포인트를 커널밀도 함수로 변환
                 sigma = bw.diggle(p), 
                 kernel = 'gaussian')  
plot(d)   # 확인
rm(list = c("x", "y", "win","p")) # 변수 정리

#---# [5단계: 픽셀 이미지를 레스터 이미지로 변환]

d[d < quantile(d)[4] + (quantile(d)[4]*0.1)] <- NA   # 노이즈 제거
library(raster)      #  install.packages("raster")
raster_high <- raster(d)  # 레스터 변환
plot(raster_high)

#---# [6단계: 클리핑]

bnd <- st_read("./01_code/sigun_bnd/seoul.shp")    # 서울시 경계선 불러오기
raster_high <- crop(raster_high, extent(bnd))      # 외곽선 자르기
crs(raster_high) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 + towgs84=0,0,0") # 좌표계 정의
plot(raster_high)  # 지도확인
plot(bnd, col=NA, border = "red", add=TRUE)

#---# [7단계: 지도 위에 래스터 이미지 올리기]
library(rgdal)    # install.packages("rgdal")
library(leaflet)  # install.packages("leaflet")
leaflet() %>% 
  #---# 베이스맵 불러오기
  addProviderTiles(providers$CartoDB.Positron) %>% 
  #---# 서울시 경계선 불러오기
  addPolygons(data = bnd, weight = 3, color= "red", fill = NA) %>% 
  #---# 레스터 이미지 불러오기
  addRasterImage(raster_high, 
   colors = colorNumeric(c("blue", "green","yellow","red"), 
   values(raster_high), na.color = "transparent"), opacity = 0.4) 

#---# [8단계: 저장하기]

dir.create("07_map")  # 새로운 폴더 생성
save(raster_high, file="./07_map/07_kde_high.rdata") # 저장
rm(list = ls()) # 메모리 정리  

#-------------------------------
# 7-2 요즘 뜨는 지역은 어디일까?
#-------------------------------

#---# [1단계: 데이터 준비]

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # 작업폴더 설정
load("./06_geodataframe/06_apt_price.rdata")     # 실거래 불러오기
grid <- st_read("./01_code/sigun_grid/seoul.shp")  # 서울시 1km 그리드 불러오기
apt_price <-st_join(apt_price, grid, join = st_intersects)  # 실거래 + 그리드 공간결합
head(apt_price, 2)

#---# [2단계: 이전/이후 데이터 세트 만들기]

kde_before <- subset(apt_price, ymd < "2021-07-01")  # 이전 데이터 필터링
kde_before <- aggregate(kde_before$py, by=list(kde_before$ID),mean)  # 평균가격
colnames(kde_before) <- c("ID", "before")   # 컬럼명 변경

kde_after  <- subset(apt_price, ymd > "2021-07-01")  # 이후 데이터 필터링
kde_after <- aggregate(kde_after$py, by=list(kde_after$ID),mean) # 평균가격 
colnames(kde_after) <- c("ID", "after")  # 컬럼명 변경

kde_diff <- merge(kde_before, kde_after, by="ID")    # 이전 + 이후 데이터 결합
kde_diff$diff <- round((((kde_diff$after-kde_diff$before)/
                           kde_diff$before)* 100), 0) # 변화율 계산

head(kde_diff, 2) # 변화율 확인

#---# [3단계: 가격이 오른 지역 찾기]

library(sf)        # install.packages("sf")
kde_diff <- kde_diff[kde_diff$diff > 0,]    # 상승지역만 추출
kde_hot <- merge(grid, kde_diff,  by="ID")  # 그리드에 상승지역 결합
library(ggplot2)   # install.packages("ggplot2")
library(dplyr)     # install.packages("dplyr")
kde_hot %>%        # 그래프 시각화
  ggplot(aes(fill = diff)) + 
  geom_sf() + 
  scale_fill_gradient(low = "white", high = "red")

#---# [4단계: 지도경계선 그리기]

library(sp)   # install.packages("sp")
kde_hot_sp <- as(st_geometry(kde_hot), "Spatial") # sf형 => sp형 변환
x <- coordinates(kde_hot_sp)[,1]  # 그리드 x, y 좌표 추출
y <- coordinates(kde_hot_sp)[,2] 

l1 <- bbox(kde_hot_sp)[1,1] - (bbox(kde_hot_sp)[1,1]*0.0001) # 그리드 기준 경계지점 설정
l2 <- bbox(kde_hot_sp)[1,2] + (bbox(kde_hot_sp)[1,2]*0.0001)
l3 <- bbox(kde_hot_sp)[2,1] - (bbox(kde_hot_sp)[2,1]*0.0001)
l4 <- bbox(kde_hot_sp)[2,2] + (bbox(kde_hot_sp)[1,1]*0.0001)

library(spatstat)  # install.packages("spatstat")
win <- owin(xrange=c(l1,l2), yrange=c(l3,l4))  # 경계지점 기준 외곽선 만들기(bounding-box)
plot(win)                                      # 확인
rm(list = c("kde_hot_sp", "apt_price", "l1", "l2", "l3", "l4")) # 메모리 정리

#---# [5단계: 밀도 그래프 변환하기]

p <- ppp(x, y, window=win, marks=kde_hot$diff) # 경계창 위에 좌표값 포인트 생성
d <- density.ppp(p, weights=kde_hot$diff,      # 포인트를 커널밀도 함수로 변환
                 sigma = bw.diggle(p), 
                 kernel = 'gaussian')
plot(d)   # 확인
rm(list = c("x", "y", "win","p")) # 변수 정리

#---# [6단계: 픽셀 -> 레스터 변환]

d[d < quantile(d)[4] + (quantile(d)[4]*0.1)] <- NA  # 노이즈 제거
library(raster)         # install.packages("raster")
raster_hot <- raster(d) # 레스터 변환
plot(raster_hot) #  확인

#---# [7단계: 클리핑]

bnd <- st_read("./01_code/sigun_bnd/seoul.shp") # 서울시 경계선 불러오기
raster_hot <- crop(raster_hot, extent(bnd))            # 외곽선 클리핑
crs(raster_hot) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84  
                        +towgs84=0,0,0")  # 좌표계 정의
plot(raster_hot)   #  확인
plot(bnd, col=NA, border = "red", add=TRUE)

#---# [8단계: 지도 그리기]

library(leaflet)   # install.packages("leaflet")
leaflet() %>%
  #---# 베이스맵 불러오기
  addProviderTiles(providers$CartoDB.Positron) %>% 
  #---# 서울시 경계선 불러오기
  addPolygons(data = bnd, weight = 3, color= "red", fill = NA) %>% 
  #---# 레스터 이미지 불러오기
  addRasterImage(raster_hot, 
   colors = colorNumeric(c("blue", "green", "yellow","red"), 
   values(raster_hot), na.color = "transparent"), opacity = 0.4)

#---# [9단계: 평균 가격 변화율 정보 저장하기]

save(raster_hot, file="./07_map/07_kde_hot.rdata") # 저장하기
rm(list = ls())      # 메모리 정리하기 

#-------------------------------------
# 7-3 우리 동네가 옆 동네보다 비쌀까?
#-------------------------------------

#---# [1단계: 데이터 준비]

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # 작업폴더 설정
load("./06_geodataframe/06_apt_price.rdata")   # 실거래 불러오기
load("./07_map/07_kde_high.rdata")    # 최고가 레스터 이미지
load("./07_map/07_kde_hot.rdata")     # 급등지 레스터 이미지

library(sf)    # install.packages("sf") 
bnd <- st_read("./01_code/sigun_bnd/seoul.shp")    # 서울시 경계선
grid <- st_read("./01_code/sigun_grid/seoul.shp")  # 서울시 1km 그리드 불러오기

#---# [2단계: 마커 클러스터링 옵션 설정]

#---# 이상치 설정(하위 10%, 상위 90% 지점)
pcnt_10 <- as.numeric(quantile(apt_price$py, probs = seq(.1, .9, by = .1))[1])
pcnt_90 <- as.numeric(quantile(apt_price$py, probs = seq(.1, .9, by = .1))[9])
#---# 마커 클러스터링 함수 등록
load("./01_code/circle_marker/circle_marker.rdata")
#---# 마커 클러스터링 컬러 설정: 상, 중, 하
circle.colors <- sample(x=c("red","green","blue"),size=1000, replace=TRUE)

#---# [3단계: 마커 클러스터링 시각화]  

library(purrr)  # install.packages("purrr")
leaflet() %>% 
  #---# 오픈스트리트맵 불러오기
  addTiles() %>%  
  #---# 서울시 경계선 불러오기
  addPolygons(data = bnd, weight = 3, color= "red", fill = NA) %>%
  #---# 최고가 레스터 이미지 불러오기
  addRasterImage(raster_high, 
    colors = colorNumeric(c("blue","green","yellow","red"), values(raster_high), 
    na.color = "transparent"), opacity = 0.4, group = "2021 최고가") %>% 
  #---# 급등지 레스터 이미지 불러오기
  addRasterImage(raster_hot, 
    colors = colorNumeric(c("blue", "green", "yellow","red"), values(raster_hot), 
    na.color = "transparent"), opacity = 0.4, group = "2021 급등지") %>%   
  #---# 최고가 / 급등지 선택 옵션 추가하기
  addLayersControl(baseGroups = c("2021 최고가", "2021 급등지"), options = layersControlOptions(collapsed = FALSE)) %>%
  #---# 마커 클러스터링 불러오기
  addCircleMarkers(data = apt_price, lng =unlist(map(apt_price$geometry,1)), 
                   lat = unlist(map(apt_price$geometry,2)), radius = 10, stroke = FALSE, 
                   fillOpacity = 0.6, fillColor = circle.colors, weight=apt_price$py, 
                   clusterOptions = markerClusterOptions(iconCreateFunction=JS(avg.formula))) 

#---# 메모리 정리하기 
rm(list = ls())  


#-----------------------
# 7-4 단골코드 정리하기
#----------------------

#---# 1) 포인트 데이터 불러오기

library(spatstat)    # install.packages("spatstat")
data(cells)   
density(cells, 0.05, at="points")
plot(cells)  # 확인

#---# 2) density.ppp로 밀도 데이터로 변환하기

d <- density.ppp(cells, 0.05) # 밀도데이터로 변환
plot(d)

#---# 3) 레스터 이미지로 변환하기

library(raster)     
raster <- raster(d)  # 레스터 변환
plot(raster) # 확인


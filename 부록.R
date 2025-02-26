
#----------------------
# 01_전처리 1: 지역정보
#----------------------

# 데이터 세트 다운로드: https://compas.lh.or.kr/subj/example/data?subjNo=SBJ_2003_001
# 분석코드 예시: https://compas.lh.or.kr/subj/past/code?subjNo=SBJ_1910_001&teamNo=85

# 지도 시각화를 위한 라이브러리인 tmap에서 qtm 기능이 일부 변경되어 본 분석에 적용이 어렵게 되었습니다.
# 따라서 qtm 대신 leaflet 을 사용하여 분석하도록 하였습니다(교재 대비 일부 기능 변경) 

#---# 01-1_fishnet(집계구) 만들기

# (1) 화성시 경계 데이터 불러오기

# 라이브러리 불러오기
library(sp)         # install.packages("sp")
library(geojsonio)  # install.packages("geojsonio")

# 작업폴더 설정
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 
getwd()
dir.create("./01_save")

# 행정동 geojson 불러오기
admin <- geojsonio::geojson_read("./SBJ_2003_001/tl_scco_emd.geojson", what = "sp")  
save(admin, file="./01_save/01_001_admin.rdata")    # 저장 

# 플로팅
plot(admin)        

# (2) fishnet(집계구) 외곽경계 만들기 

# 라이브러리 불러오기
library(sp)       # install.packages("sp")     
library(raster)   # install.packages("raster") 
library(leaflet)  # install.packages("leaflet") 

# 외곽경계 만들기: x_min, x_max, y_min, y_max
fishnet <- as(raster::extent(126.50625, 127.42245, 36.99653, 37.483419), "SpatialPolygons")

# WGS84 좌표계 투영
proj4string(fishnet) <- "+proj=longlat +datum=WGS84"

# 플로팅
plot(fishnet, border="red")
plot(admin, add=T)     

# (3) fishnet(집계구) 만들기

# 레스터 변환
fishnet <- raster(fishnet)  

# 0.1도 단위로 분할
res(fishnet) <- .01         

# 투영
crs(fishnet) <- CRS("+proj=longlat +datum=WGS84") 

# 폴리곤으로 변환
fishnet <- rasterToPolygons(fishnet)  

# 지도 시각화
leaflet() %>% addTiles() %>%          
  addPolygons(data = fishnet, weight = 0.4, fillOpacity = 0) %>%
  addPolygons(data = admin, color = "red")

# (4) fishnet 일련번호 부여하기

# 현재 번호 없음 
fishnet$id      

# 일련번호 부여
fishnet@data <- data.frame(id = 1:nrow(fishnet))

# 저장
save(fishnet, file="./01_save/01_002_fishnet.Rdata") 

# 일련번호 확인
head(fishnet$id, 10)  

#---# 01-2_정류장-버스노선 매핑 테이블 생성

# (1) 정류장 정보 불러오기

# csv 불러오기
sta_table <- read.csv("./SBJ_2003_001/stations_table.csv", fileEncoding = "UTF-8")   

# 필요한 컬럼만 추출
keeps <- c("표준정류장ID", "이비카드정류장ID", "WGS84위도", "WGS84경도","시군명", "정류소명") 
sta_table <- sta_table[keeps]   

# NA 제거 
sta_table <- na.omit(sta_table) 

# 저장 
save(sta_table, file="./01_save/01_003_sta_table.rdata")  

# 불필요 변수 지우기 
rm("keeps")                     
head(sta_table, 2)

# (2) 버스 노선(route)별 정류장 정보 불러오기

# csv 불러오기
route_sta <- read.csv("./SBJ_2003_001/routestationinfo.csv", fileEncoding = "UTF-8") 

# 필요 컬럼 추출
keeps <- c("bus_line_no", "bus_line_no_seq", "station_id", "station_nm")   
route_sta <- route_sta[keeps]   
head(route_sta, 2)

# (3) 매핑 테이블 생성: 정류장 - 버스노선 

# 정류장 id(station_id) + 좌표값(위도, 경도) 결합
route_sta <- merge(route_sta, sta_table, by.x = "station_id", by.y = "표준정류장ID") 

# 결측치 확인
sum(is.na(route_sta$정류소명)) 

# 저장
save(route_sta, file="./01_save/01_004_route_sta.rdata") ; rm("keeps") 
head(route_sta[,c(1,2,3,8,9)], 2) 


#-----------------------------
# 02_전처리 2: 교통카드 데이터
#-----------------------------

#---# 02-1_개인별 이동 데이터(trip_chain) 생성

# (1) 변수명 한글로 변경

# 작업폴더 설정
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 

# 개인별 이동 데이터(trip_chain) 불러오기 
trip_chain <- read.csv("./SBJ_2003_001/TripChain.csv", fileEncoding = "UTF-8") 

# 변수명 한글로 변경
colnames(trip_chain) <-  
  c("암호화카드번호", "트랜잭션ID", "환승횟수", "교통카드발행사ID", "총이용객수", 
  "사용자구분", "교통수단CD1", "교통수단CD2", "교통수단CD3", "교통수단CD4", 
  "교통수단CD5", "버스노선ID1", "버스노선ID2", "버스노선ID3", "버스노선ID4", 
  "버스노선ID5", "차량ID1", "차량ID2", "차량ID3", "차량ID4", "차량ID5", 
  "총통행거리", "총탑승시간", "총소요시간", "승차일시1", "승차일시2", 
  "승차일시3",  "승차일시4", "승차일시5", "하차일시1", "하차일시2",  
  "하차일시3", "하차일시4", "하차일시5", "최초승차일시", "최종하차일시",
  "승차역ID1", "승차역ID2", "승차역ID3",  "승차역ID4", "승차역ID5", 
  "하차역ID1", "하차역ID2",  "하차역ID3", "하차역ID4", "하차역ID5", 
  "최초승차역ID", "최종하차역ID", "총이용금액", "수집건수", "트립체인완료코드")
colnames(trip_chain) 
 
# (2) trip 시작일시

# 옵션 변경(지수 => 숫자)
options("scipen" = 100) 

# Trip 시작 날짜(start_day) => 1일 ~ 4일 중 하나
trip_chain$start_day  <- as.factor(as.numeric(substr(trip_chain[,35], 7, 8))) 
trip_chain$start_day  <- as.numeric(trip_chain$start_day)  

# Trip 시작 시간(start_time) => 새벽 4시 ~ 밤 23시 중 하나  
trip_chain$start_hour <- as.factor(as.numeric(substr(trip_chain[,35], 9, 10))) 
trip_chain$start_hour <- as.numeric(trip_chain$start_hour)  
head(trip_chain[,c(1,52,53)], 2)

# (3) trip 종료일시

# Trip 종료 날짜(end_day) => 1일 ~ 4일 중 하나
trip_chain$end_day <- as.factor(as.numeric(substr(trip_chain[,35], 7, 8)))  
trip_chain$end_day <- as.numeric(trip_chain$end_day) 

# Trip 종료 시간(start_time) => 새벽 4시 ~ 밤 23시 중 하나  
trip_chain$end_hour  <- as.factor(as.numeric(substr(trip_chain[,35], 9, 10))) 
trip_chain$end_hour  <- as.numeric(trip_chain$end_hour) 

# 저장
save(trip_chain, file="./01_save/02_001_trip_chain_full.rdata") 
head(trip_chain[,c(1, 52, 53, 54,55)], 2)

#---# 02-2_trip_chain + 정류장 정보 매핑

# (1) trip_chain 출발-도착 정류장 정보에 좌표값 결합
# 정류장-버스노선 매핑 테이블 불러오기

# library(dplyr) 
load("./01_save/01_003_sta_table.rdata")  

# 버스 정류장 정보(sta_table) 컬럼 이름 변경
colnames(sta_table) <- c("표준정류장ID", "이비카드정류장ID", "S_WGS84위도", "S_WGS84경도","S_시군명", "S_정류소명") 

# 출발점 기준 => 이비카드정류장 ID + station ID join 하기  
trip_chain <- merge(trip_chain, sta_table, by.x = "승차역ID1", by.y = "이비카드정류장ID")  

# 결측치 확인
sum(is.na(trip_chain$S_정류소명))        

# 버스 정류장 정보(sta_table) 컬럼 이름 변경
colnames(sta_table) <-  c("표준정류장ID", "이비카드정류장ID", "E_WGS84위도", "E_WGS84경도","E_시군명", "E_정류소명") 

# 도착점 기준 => 이비카드정류장 ID + station ID가 join 
trip_chain <- merge(trip_chain, sta_table, by.x = "최종하차역ID", by.y = "이비카드정류장ID") 

# 저장
save(trip_chain, file="./01_save/02_002_trip_chain_full.rdata") 

# 결측치 확인
sum(is.na(trip_chain$E_정류소명)) 

# (2) 승하차가 많은 지역은 어디인가?

# 피벗테이블 만들기
as.matrix(table(trip_chain$S_시군명, trip_chain$E_시군명))     

# 어느 지역에서 가장 많이 승차 하였는가? 
sort(colSums(as.matrix(table(trip_chain$S_시군명, trip_chain$E_시군명))), decreasing = TRUE)

# 어느 지역에서 가장 많이 하차 하였는가? 
sort(rowSums(as.matrix(table(trip_chain$S_시군명, trip_chain$E_시군명))), decreasing = TRUE)

#---# 02-3_분석의 공간적 범위 결정: 승하차가 가장 많은 지역

# (1) 데이터 필터링

# 출발 또는 도착지가 화성시인 경우만 필터링
trip_chain <- trip_chain[(trip_chain$S_시군명 == "화성시" | trip_chain$E_시군명 == "화성시"), ] 

# 저장
save(trip_chain, file="./01_save/02_003_trip_chain.rdata") 
nrow(trip_chain)

# (2) OD 매트릭스 생성

# OD 매트릭스 만들기
OD <- as.matrix(table(trip_chain$S_시군명, trip_chain$E_시군명)) 

# OD 매트릭스를 데이터프레임으로 변환
OD <- as.data.frame(OD)  

# 통행발생 순서대로 정렬
OD <- OD[order(OD$Freq, decreasing = TRUE), ]

# 일련번호 재정렬
rownames(OD) <- 1 : length(rownames(OD))  
head(OD) 

# (3) 지역별 O-D 발생량 누적비율 보기

library(dplyr)  # install.packages("dplyr")

# 누적비율 보기 
OD <- OD %>% mutate(cum = round(((cumsum((Freq))/sum(Freq))*100), 1))  

# 컬럼명 변경
colnames(OD)<- c("From", "To", "Freq", "Cum")  

# OD 메트릭스 저장
save(OD, file="./01_save/02_003_OD.rdata") 
head(OD)

# (4) 통행량 집중도 확인: 지니계수와 로렌츠 곡선

library(ineq) # install.packages("ineq")    

# 지니계수 
ineq(OD$Freq,type="Gini") 

# 로렌츠 곡선 그리기
plot((OD$Freq), col="red", type = 'l', lwd=2) 

# (5) 대상지역 교통량 데이터만 추출

# 출발 또는 도착 지역 지정 
patterns <- c("수원시","화성시", "용인시", "오산시") 

# 필요 변수 지정 
colnames(sta_table) <-  c("표준정류장ID", "이비카드정류장ID", "WGS84위도", "WGS84경도","시군명", "정류소명") 

# 필요한 지역 - 변수만 필터링
sta_table <- filter(sta_table, grepl(paste(patterns, collapse="|"), 시군명)) 

# 일련번호 다시 부여하기
rownames(sta_table) <- seq(1:nrow(sta_table)) 

# 저장
save(sta_table, file="./01_save/02_003_sta_table.rdata")   

# 대상지 필터링 결과 확인
unique(sta_table$시군명)   

# (6) 정리

rm(list = ls())                   # 메모리 정리 
dev.off(dev.list()["RStudioGD"])  # 플롯창 정리

#---------------------------------------------------
# 03_기초분석 I: 기초분석(EDA) I: 노선별·시간대별 이용량
#---------------------------------------------------

#---# 03-1_노선별·시간대별 이용량 특성 분석

# (1) 이용자가 몇 번 버스를 타고 어디에서 어디로 이동하였는지 알아내기 
# 개인 이동 데이터 불러오기
load("./01_save/02_003_trip_chain.rdata")    

# 버스 노선정보 불러오기
route_map <- read.csv("./SBJ_2003_001/routestationmapping.csv", fileEncoding = "UTF-8")  

# 노선 ID와 버스번호(노선명) 추출
route_map <- route_map[,5:6]     

# 개인 이동 데이터와 버스번호(노선명) 결합
route_sta <- merge(trip_chain, route_map, by.x = "버스노선ID1", by.y = "표준노선ID")  

# 개인별 이동정보 확인
head(route_sta[1:2,c(4, 66, 60, 65)],) 

# (2) 이용자가 많은 버스 노선 추출

# 노선명을 문자형(character) 변수로 속성 변환
route_sta$노선명 <- as.character(route_sta$노선명)   

# 노선별 이용건수를 피벗테이블로 작성하기 
bus_usr <- as.data.frame(table(route_sta$노선명))  

# 노선별 이용건수 sorting => 빈도수가 많은 것 부터 
bus_usr <- bus_usr[order(-bus_usr$Freq), ]   

# 컬럼명 변경
colnames(bus_usr) <- c("line", "Freq")

# 일련번호 재정렬
rownames(bus_usr) <- 1 : length(rownames(bus_usr))

# 비율(percentage) 보기
bus_usr$pcnt = round(bus_usr$Freq/ sum(bus_usr$Freq), 3) * 100 

# 노선별 이용건수 1~10위까지 불러오기
head(bus_usr[1:10,], 5)

# (3) 노선별 누적 이용자 비율(cumulative percentage) 분석 

# 노선별 누적비율 계산
bus_usr <- bus_usr %>% mutate(cum = round(((cumsum((Freq))/sum(Freq))*100), 1))

# 저장
save(bus_usr, file="./01_save/03_001_bus_usr.rdata")   

# 상위 42개 노선이 전체 이동량의 87.3%를 차지하고 있음 
head(bus_usr[39:42,], 5)

# 누적 비율 차트
plot(bus_usr$cum, type='l', xlim=c(0, 100))  
abline(v = 42, col="red", lwd=3, lty=2)

# (4) Trip 발생 시간대 분석 (맥 사용자 한글폰트 설치 필요)

hist <- hist(trip_chain$start_hour, plot = FALSE)
plot(hist, xaxt = "n", xlab = "시간", ylab = "건수",
     main = "시간대별 trip 발생량", col = "blue")
axis(1, hist$mids, labels = c(5:23))
abline(v = c(3, 5, 13, 15), col="red", lwd=3, lty=2)

#---# 03-2_이용량 많은 버스노선 정류장 위치 알아보기 

# (1) 정류장 정보 추출

# 버스노선 번호 추출
bus_line <- as.character(bus_usr[1:42,1])  

# 노선 - 정류장 매핑 테이블 불러오기
load("./01_save/01_004_route_sta.rdata")   

# 이용량 많은 42개 버스노선의 정류장만 추출
bus_line <- filter(route_sta, grepl(paste(as.character(bus_line), collapse="|"), route_sta$bus_line_no)) 

# 버스 노선 중 대상지역에 위치하는 정류장 정보만 추출 
patterns <- c("수원시","화성시", "용인시", "오산시")   # 대상지 지정
bus_line <- filter(bus_line, grepl(paste(patterns, collapse="|"), 시군명))  # 필터링

# 중복되는 라인 지우기 
bus_line <- bus_line[!duplicated(bus_line[c(2,4)]),] 
bus_line <- bus_line[with(bus_line, order(bus_line_no, bus_line_no_seq)), ] 

# 순서대로 정렬하기 
bus_line <- merge(bus_line, bus_usr[1:42,1:3], by.x = "bus_line_no", by.y = "line", all.x = TRUE) 
bus_line <- na.omit(bus_line)
save(bus_line, file="./01_save/03_002_bus_line.rdata") # 저장
head(bus_line[,c(1,4,9)], 2)

# (2) 정류장 정보를 공간 포인트(spatialpoints)로 만들기

# 좌표값 추출
library(tidyr)  # install.packages("tidyr")
library(sp)     # install.packages("sp")
coords <- bus_line %>% dplyr::select(WGS84경도, WGS84위도)
data <- bus_line[,1:11]

# 투영
crs <- CRS("+proj=longlat +datum=WGS84")
sta_pnt <- SpatialPointsDataFrame(coords = coords, data = data, proj4string = crs)

# 저장
save(sta_pnt, file="./01_save/03_003_sta_pnt.rdata") 

# 시각화 (기존 qtm 라이브러리가 문제가 있어서 leaflet으로 시각화를 대체합니다)

library(sf)
sta_pnt_2 <- st_as_sf(sta_pnt)              # sp형 => sf형 변환

pcnt_1 <- sta_pnt_2[sta_pnt_2$pcnt > 1,]   # 필터링 1 % 이상 
pcnt_2 <- sta_pnt_2[sta_pnt_2$pcnt > 2,]   # 필터링 2 % 이상 
pcnt_3 <- sta_pnt_2[sta_pnt_2$pcnt > 3,]   # 필터링 3 % 이상 
pcnt_4 <- sta_pnt_2[sta_pnt_2$pcnt > 4,]   # 필터링 4 % 이상 

library(leaflet)
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data=pcnt_1, col = "blue",   radius = 0.5) %>%
  addCircleMarkers(data=pcnt_2, col = "purple", radius = 1) %>%
  addCircleMarkers(data=pcnt_3, col = "orange", radius = 1.5) %>% 
  addCircleMarkers(data=pcnt_4, col = "red",    radius = 2)  

# (3) 정리 
rm(list = ls())                   # 메모리 정리 
dev.off(dev.list()["RStudioGD"])  # 플롯창 정리


#--------------------------------------------
# 04_기초분석 II: 집계구(fishnet)별 이동특성 
#--------------------------------------------

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # 작업폴더 설정

#---# 04-1_집계구별 Trip 계산

# (1) 정류장(point) 데이터와 fishnet(polygon)의 공간조인(spatial join)
# 파일 불러오기

load("./01_save/02_003_trip_chain.rdata")
load("./01_save/01_002_fishnet.Rdata")
load("./01_save/03_003_sta_pnt.rdata")
load("./01_save/01_001_admin.rdata")

# sta_pnt(정류장 위치) / fishnet(분석단위: 격자망) 을 sp형에서 sf형으로 변환
# spatialEco 라이브러리가 2.0으로 업데이트 되면서 기존의 point.in.poly 명령어가 제거되었음
# 따라서 이 문제를 해결하기 위하여 sp형의 point.in.poly 대신 sf형의 st_intersection 명령어를 사용

library(sf)
sta_pnt <- st_as_sf(sta_pnt)                  # 정류장 위치 sp를 sf 객체로 변환
fishnet <- st_as_sf(fishnet)                  # 격자망      sp를 sf 객체로 변환
sta_pnt <- st_intersection(sta_pnt, fishnet)  # 해당 정류장 포인트(sta_pnt)가 몇번째 격자망(fishnet) 에 속하는지
                                              # 추출하기 위하여 st_intersection 사용 (시간이 많이걸립니다. 커피 한잔 하시면서 기다리세요)
head(sta_pnt)
sta_pnt <- as(sta_pnt, Class = "Spatial")     # 다시 sf형을 sp형으로 변환

# 저장 
save(sta_pnt, file="./01_save/04_001_sta_pnt.rdata") 
head(sta_pnt@data[, c(1, 4, 10, 12)], 2)      

# (2) 승하차 정류장 데이터에 fishnet id 추가하기

# trip_chain 번호 다시 매기기
rownames(trip_chain) <- seq(1:nrow(trip_chain)) 
trip_chain <- merge(trip_chain, sta_pnt@data[,c(5,12)], by.x = "승차역ID1", by.y = "이비카드정류장ID")
trip_chain <- merge(trip_chain, sta_pnt@data[,c(5,12)], by.x = "최종하차역ID", by.y = "이비카드정류장ID")
save(trip_chain, file="./01_save/04_002_trip_chain.rdata") 
tail(trip_chain[,c(3, 60, 65, 66, 67)], 2)

# (3) trip_chain에서 필요한 정보만 추출

# 필요한 정보만 추출하기 
keeps <- c( "id.x", "id.y", "승차역ID1", "최종하차역ID", "총이용객수", "환승횟수")  
grid_chain <- trip_chain[keeps]  
rm("trip_chain") ; rm("keeps")
save(grid_chain, file="./01_save/04_003_grid_chain.rdata") 
head(grid_chain, 2)

#---# 04-2_집계구별 출발 / 도착 이동량 특성 분석

# (1) 출발지 기준 총이용객수 / 평균환승횟수 fishnet 분석 
library(dplyr) # install.packages("dplyr")    
library(sp)    # install.packages("sp")
library(sf)    # install.packages("sf")

# 출발지 기준 fishnet 분석 
grid_in <- grid_chain %>%             
  group_by(id.x) %>%                  #  fishnet에서 id.x별로 총이용객수, 
  summarize_if(is.numeric, sum) %>%   # 환승횟수 데이터를 sum 하기 
  dplyr::select(id.x, 총이용객수, 환승횟수) 

# 평균환승횟수 계산 
grid_in$평균환승 <- round((grid_in$환승횟수 / grid_in$총이용객수),1)  

# 컬럼 이름 정리하기
colnames(grid_in) <- c("id", "총이용객수", "환승횟수", "평균환승")    

# s3(spatial polygon dataframe) => sf 포맷으로 변환
fishnet_2 <- as(fishnet,'sf')  

# fishnet_2 + "총이용객수", "환승횟수", "평균환승" 결합
fishnet_2 <- full_join(fishnet_2, grid_in, by = "id")  

# 저장
save(fishnet_2, file="./01_save/04_004_fishnet_2.rdata") 
head(fishnet_2, 2)

# (2) 출발지 기준 => "총이용객수", "환승횟수", "평균환승" 플로팅 

# 총이용객수
quantile(fishnet_2$총이용객수, na.rm=T)
breaks <- c(0, 10, 70, 500, 1000, Inf) 
pal <- colorBin(palette = "YlOrRd", domain = fishnet_2$총이용객수, bins = breaks,  na.color = "transparent")           # 변수 1

library(leaflet)
leaflet() %>%
  addTiles() %>%
  addPolygons(data = fishnet_2, weight = 0.5, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.6, fillColor = ~pal(총이용객수), color = 'lightgrey')

# 평균환승
quantile(fishnet_2$평균환승, na.rm=T)
breaks <- c(0, 1, 2, 3, 4, Inf) 
pal <- colorBin(palette = "YlOrRd", domain = fishnet_2$평균환승, bins = breaks,  na.color = "transparent")           # 변수 1

library(leaflet)
leaflet() %>%
  addTiles() %>%
  addPolygons(data = fishnet_2, weight = 0.5, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5, fillColor = ~pal(평균환승), color = 'lightgrey')


# (3) 도착지 기준 총이용객수 / 평균환승횟수 fishnet 분석 

# 도착지 기준 fishnet 분석 
grid_out <- grid_chain %>%   
  group_by(id.y) %>%                 # grid에서 id.xy별로 총이용객수,  
  summarize_if(is.numeric, sum) %>%  # 환승횟수 데이터를 sum 하기 
  dplyr::select(id.y, 총이용객수, 환승횟수) 

# 평균환승횟수 계산 
grid_out$평균환승 <- round((grid_out$환승횟수 / grid_out$총이용객수),1) 

# 컬럼 이름 정리하기
colnames(grid_out) <- c("id", "총이용객수", "환승횟수", "평균환승")     

# sp => sf 포맷으로 변환
fishnet_2 <- as(fishnet,'sf')    

# fishnet_2 + "총이용객수", "환승횟수", "평균환승" 결합
fishnet_2 <- full_join(fishnet_2, grid_out, by = "id")

# 저장
save(fishnet_2, file="./01_save/04_005_fishnet_2.rdata") 
head(fishnet_2, 2)

# (4) 도착지 기준 => "총이용객수", "환승횟수", "평균환승" 플로팅 

# 총이용객수
quantile(fishnet_2$총이용객수, na.rm=T)
breaks <- c(0, 10, 50, 300, 1000, Inf) 
pal <- colorBin(palette = "YlOrRd", domain = fishnet_2$총이용객수, bins = breaks,  na.color = "transparent")           # 변수 1

library(leaflet)
leaflet() %>%
  addTiles() %>%
  addPolygons(data = fishnet_2, weight = 0.5, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.6, fillColor = ~pal(총이용객수), color = 'lightgrey')

# 평균환승
quantile(fishnet_2$평균환승, na.rm=T)
breaks <- c(0, 1, 1.25, 2, 3, Inf) 
pal <- colorBin(palette = "YlOrRd", domain = fishnet_2$평균환승, bins = breaks,  na.color = "transparent")           # 변수 1

library(leaflet)
leaflet() %>%
  addTiles() %>%
  addPolygons(data = fishnet_2, weight = 0.5, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5, fillColor = ~pal(평균환승), color = 'lightgrey')

# (5) 정리

rm(list = ls())                   # 메모리 정리 
dev.off(dev.list()["RStudioGD"])  # 플롯창 정리


#--------------------------------
# 05_교통흐름 분석 1: 통근 시간대
#---------------------------------

#---# 05-1_통근 시간대 교통흐름(flow) 분석

# (1) 통근 시간대 데이터 필터링

# 파일 불러오기
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 
load("./01_save/04_002_trip_chain.rdata")  
load("./01_save/04_001_sta_pnt.rdata")
load("./01_save/01_002_fishnet.Rdata")
load("./01_save/01_001_admin.rdata")

# 통근통행 필터링: 오전(7, 8, 9시), 오후(17, 18, 19시)
library(dplyr)
trip_cmt <-  trip_chain[grep("6|7|8|9|17|18|19", trip_chain$start_hour),]  

# trip_chain 번호 다시 매기기
rownames(trip_cmt) <- seq(1:nrow(trip_cmt))      
save(trip_cmt, file="./01_save/06_001_trip_cmt.rdata")     

# trip_chain에서 필요한 정보만 추출하기
keeps <- c( "id.x", "id.y", "승차역ID1", "최종하차역ID", "총이용객수", "환승횟수") 
grid_chain <- trip_cmt[keeps]  
save(grid_chain, file="./01_save/05_003_grid_chain.rdata") 
rm("trip_chain") ; rm("keeps")
head(grid_chain, 2)

# (2) 집계구 간(inter) 이동만 남기기 [집계구 내(intra) 이동 지우기]

# 그리드 간(inter) 이동만 남기기 [그리드 내(intra) 이동 지우기]
library(stplanr)  # install.packages("stplanr")
od_intra <- filter(grid_chain, id.x != id.y)  

# 그리드 간(intra) 이동별 총이용객수, 환승횟수 집계하기 
library(dplyr)
od_intra2 <- od_intra %>%   
  group_by(id.x, id.y) %>% 
  summarise_each(funs(sum)) %>% 
  dplyr::select(id.x, id.y, 총이용객수, 환승횟수)

# 평균환승횟수 계산 
od_intra2$평균환승 <- round((od_intra2$환승횟수 / od_intra2$총이용객수),1) 

# 컬럼 이름 정리하기
colnames(od_intra2) <- c("id.x", "id.y", "총이용객수", "환승횟수", "평균환승") 
head(od_intra2, 5)

# (3) 시각화 위하여 공간 데이터 형식(OD2LINE)으로 변경

# 공간 데이터 형식(OD2LINE) 만들기 
fishnet <- st_as_sf(fishnet)         # 격자망  sp를 sf 객체로 변환
od_line <- od2line(od_intra2, fishnet)   

# 이용자 수 20건 이상인 O-D 라인 필터링
od_line <- od_line[od_line$총이용객수 %in% 20:3000, ]  

# 저장
save(od_line, file="./01_save/05_003_od_line.rdata")  

# 총이용객수 시각화

line_1 <- od_line[od_line$총이용객수 > 30,]
line_2 <- od_line[od_line$총이용객수 > 100,]
line_3 <- od_line[od_line$총이용객수 > 400,]
line_4 <- od_line[od_line$총이용객수 > 1000,]

library(leaflet)
leaflet() %>%
  addTiles() %>%
  addPolylines(data = line_1, weight = 0.5, color = 'grey') %>%
  addPolylines(data = line_2, weight = 1, color = 'blue')   %>%
  addPolylines(data = line_3, weight = 3, color = 'orange') %>%
  addPolylines(data = line_4, weight = 4, color = 'red')


# 커뮤니티 탐지 안됨
# (패키지의 community detection 기능 중 일부가 제거되어 해당 분석이 어렵습니다. 따라서 이 부분은 넘어가겠습니다.)

# (4) 정리

par(mfrow=c(1,1))                
rm(list = ls())                   # 메모리 정리 
dev.off(dev.list()["RStudioGD"])  # 플롯창 정리

#-----------------------------------
# 06_교통흐름 분석 2: 비통근 시간대
#------------------------------------  

#---# 06-1_비통근 시간대 교통흐름(flow) 분석

# (1) 비통근 시간대 필터링

# 파일 불러오기
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 
load("./01_save/04_002_trip_chain.rdata")  
load("./01_save/04_001_sta_pnt.rdata")
load("./01_save/01_002_fishnet.Rdata")
load("./01_save/01_001_admin.rdata")

# 비통근통행 추출 (10~16시)
library(dplyr)
trip_cmt <-  trip_chain[grep("10|11|12|13|14|15|16", trip_chain$start_hour),] 

# trip_chain 번호 다시 매기기
rownames(trip_cmt) <- seq(1:nrow(trip_cmt))     

#비통근통행: 6.8만건 => 6.8 / 16.1(전체통행) => 약 42.2%

# 저장
save(trip_cmt, file="./01_save/07_001_trip_cmt.rdata") 

# 필요한 정보만 추출
keeps <- c( "id.x", "id.y", "승차역ID1", "최종하차역ID", "총이용객수", "환승횟수") 
grid_chain <- trip_cmt[keeps]  
save(grid_chain, file="./01_save/07_001_grid_chain.rdata") 
rm("trip_chain") ; rm("keeps")
head(grid_chain, 2)

# (2) 집계구 간(inter) 이동만 남기기 [집계구 내(intra) 이동 지우기]

# inter 이동만 남기기(intra 이동 필터링)
library(stplanr)  # install.packages("stplanr")
od_intra <- filter(grid_chain, id.x != id.y)  

# 이동별 총이용객수, 환승횟수 집계하기 
library(dplyr)
od_intra2 <- od_intra %>%   
  group_by(id.x, id.y) %>% 
  summarise_each(funs(sum)) %>% 
  dplyr::select(id.x, id.y, 총이용객수, 환승횟수)

# 평균환승횟수 계산 
od_intra2$평균환승 <- round((od_intra2$환승횟수 / od_intra2$총이용객수),1) 

# 컬럼 이름 정리
colnames(od_intra2) <- c("id.x", "id.y", "총이용객수", "환승횟수", "평균환승") 
head(od_intra2, 2)

# (3) 시각화 위하여 공간 데이터 형식(OD2LINE)으로 변경

# 공간 데이터 형식(OD2LINE) 만들기 
fishnet <- st_as_sf(fishnet)         # 격자망  sp를 sf 객체로 변환
od_line <- od2line(od_intra2, fishnet)   

# 이용자 수 20건 이상인 O-D 라인 필터링
od_line <- od_line[od_line$총이용객수 %in% 20:3000, ]  

# 저장
save(od_line, file="./01_save/05_004_od_line.rdata")  

# 총이용객수 시각화
line_1 <- od_line[od_line$총이용객수 > 30,]
line_2 <- od_line[od_line$총이용객수 > 100,]
line_3 <- od_line[od_line$총이용객수 > 400,]
line_4 <- od_line[od_line$총이용객수 > 1000,]

library(leaflet)
leaflet() %>%
  addTiles() %>%
  addPolylines(data = line_1, weight = 0.5, color = 'grey') %>%
  addPolylines(data = line_2, weight = 1, color = 'blue') %>%
  addPolylines(data = line_3, weight = 3, color = 'orange') %>%
  addPolylines(data = line_4, weight = 4, color = 'red')

# 커뮤니티 탐지 안됨
# (패키지의 community detection 기능 중 일부가 제거되어 해당 분석이 어렵습니다. 따라서 이 부분은 넘어가겠습니다.)

# (4) 정리
par(mfrow=c(1,1))                
rm(list = ls())                   # 메모리 정리 
dev.off(dev.list()["RStudioGD"])  # 플롯창 정리


#---# [참고] 맥 사용자를 위한 한글폰트 #-------------------------#
require(showtext)  # install.packages("showtext")
font_add_google(name='Nanum Gothic', regular.wt=400, bold.wt=700)
showtext_auto()
showtext_opts(dpi=112)
#----------------------------------------------------------------#


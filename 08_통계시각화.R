
#--------------------------------
# 8-1 관심 지역 데이터만 추출하기
#--------------------------------

#---# [1단계: 데이터 준비]

library(sf) 
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
load("./06_geodataframe/06_apt_price.rdata")   # 실거래가
load("./07_map/07_kde_high.rdata")     # 최고가 레스터 이미지
grid <- st_read("./01_code/sigun_grid/seoul.shp") # 서울시그리드

#---# [2단계: 관심지역 그리드 찾기]  

library(tmap)       # install.packages("tmap") 
tmap_mode('view')
#---# 그리드 그리기 
tm_shape(grid) + tm_borders() + tm_text("ID", col = "red") + 
  #---# 레스터 이미지 그리기
  tm_shape(raster_high) + 
  #---# 레스터 이미지 컬러패턴 설정
  tm_raster(palette = c("blue", "green", "yellow","red"), alpha =.4) + 
  #---# 배경지도 선택하기
  tm_basemap(server = c('OpenStreetMap'))

#---# [3단계: 전체지역 / 관심지역 저장]

library(dplyr)
apt_price <-st_join(apt_price, grid, join = st_intersects)  # 실거래 + 그리드 공간결합
apt_price <- apt_price %>% st_drop_geometry()               # 실거래 공간속성 지우기
all <- apt_price                         # 전체지역(all) 추출
sel <- apt_price %>% filter(ID == 81016) # 관심지역(sel) 추출
dir.create("08_chart")   # 새로운 폴더 생성
save(all, file="./08_chart/all.rdata") # 저장
save(sel, file="./08_chart/sel.rdata") 
rm(list = ls())  # 메모리 정리하기 


#--------------------------------------------------
# 8-2 확률 밀도 함수: 이 지역 아파트는 비싼 편일까?
#--------------------------------------------------

#---# [1단계: 그래프 준비하기]

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
load("./08_chart/all.rdata")  # 전체지역
load("./08_chart/sel.rdata")  # 관심지역
max_all <- density(all$py) ; max_all <- max(max_all$y) 
max_sel <- density(sel$py) ; max_sel <- max(max_sel$y) 
plot_high <- max(max_all, max_sel)  # y 축 최대값 찾기
rm(list = c("max_all", "max_sel"))
avg_all <- mean(all$py)  # 전체지역 평당 평균가 계산
avg_sel <- mean(sel$py)  # 선택지역 평당 평균가 계산
avg_all ; avg_sel ; plot_high  # 전체/관심 평균 가격과 y축 최댓값 확인

#---# [2단계: 확률밀도함수 그리기]

plot(stats::density(all$py), ylim=c(0, plot_high), 
  col="blue", lwd=3, main= NA) # 전체(all) 밀도함수 플로팅
abline(v = mean(all$py), lwd = 2, col = "blue", lty=2) # 전체(all) 평균 수직선 그리기  
text(avg_all + (avg_all) * 0.15, plot_high * 0.1, 
  sprintf("%.0f",avg_all), srt=0.2, col = "blue")  # 전체(all) 평균 텍스트 입력
lines(stats::density(sel$py), col="red", lwd=3)  # 선택(sel) 확률밀도함수 플로팅   
abline(v = avg_sel, lwd = 2, col = "red", lty=2) # 선택(sel) 평균 수직선 그리기 
text(avg_sel + avg_sel * 0.15 , plot_high * 0.1, 
  sprintf("%.0f", avg_sel), srt=0.2, col = "red") # 선택(sel) 평균 텍스트 입력
rm(list = ls())  # 메모리 정리하기 

#----------------------------------------------
# 8-3 회귀분석: 이 지역은 일년에 얼마나 오를까?
#----------------------------------------------

#---# [1단계: 월별 평당 거래가 요약]

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
load("./08_chart/all.rdata")  # 전체지역
load("./08_chart/sel.rdata")  # 관심지역 
library(dplyr)      # install.packages("dplyr") 
library(lubridate)  # install.packages("lubridate") 
all <- all %>% group_by(month=floor_date(ymd, "month")) %>% 
  summarize(all_py = mean(py)) # 전체(all) 카운팅
sel <- sel %>% group_by(month=floor_date(ymd, "month")) %>% 
  summarize(sel_py = mean(py)) # 관심(sel) 카운팅

#---# [2단계: 회귀식 모델링]

fit_all <- lm(all$all_py ~ all$month)   # 전체(all) 회귀식
fit_sel <- lm(sel$sel_py ~ sel$month)   # 관심(sel) 회귀식
coef_all <- round(summary(fit_all)$coefficients[2], 1) * 365  # 전체(all) 회귀계수
coef_sel <- round(summary(fit_sel)$coefficients[2], 1) * 365  # 관심(sel) 회귀계수

#---# [3단계: 회귀분석 그리기]

#---# 분기별 평당 가격변화 주석 만들기
library(grid)   # install.packages("grid")
grob_1 <- grobTree(textGrob(paste0("전체지역: ", coef_all, "만원(평당)"), x=0.05, 
                            y=0.88, hjust=0, gp=gpar(col="blue", fontsize=13, fontface="italic")))
grob_2 <- grobTree(textGrob(paste0("관심지역: ", coef_sel, "만원(평당)"), x=0.05,  
                            y=0.95, hjust=0, gp=gpar(col="red", fontsize=16, fontface="bold")))
#---# 선택지역 회귀선 그리기                            
library(ggpmisc)   # install.packages("ggpmisc")
gg <- ggplot(sel, aes(x=month, y=sel_py)) + 
  geom_line() + xlab("월")+ ylab("가격") +
  theme(axis.text.x=element_text(angle=90)) + 
  stat_smooth(method='lm', colour="dark grey", linetype = "dashed") +
  theme_bw()
#---# 서울전체 회귀선 그리기
gg + geom_line(color= "red", size=1.5) +
  geom_line(data=all, aes(x=month, y=all_py), color="blue", size=1.5) +
  #---#  주석 추가하기 (맥 사용자는 한글폰트 추가 필요)
  annotation_custom(grob_1) +
  annotation_custom(grob_2)
rm(list = ls())  # 메모리 정리하기 

#---------------------------------------------
# 8-4 주성분 분석: 이 동네 단지별 특징은 뭐야?
#---------------------------------------------

#---# [1단계: 주성분분석]

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
load("./08_chart/sel.rdata")  # 선택지역 데이터 불러오기
pca_01 <- aggregate(list(sel$con_year, sel$floor, sel$py, sel$area), 
                    by=list(sel$apt_nm), mean)  # 아파트별 평균값 구하가기
colnames(pca_01) <- c("apt_nm", "신축", "층수","가격", "면적")
m <- prcomp(~ 신축 + 층수 + 가격 + 면적, data= pca_01, scale=T)  # 주성분 분석
summary(m)

#---# [2단계: 그리기]

library(ggfortify)  # install.packages("ggfortify")
autoplot(m, loadings.label=T, loadings.label.size=6)+
  geom_label(aes(label=pca_01$apt_nm), size=4)  
  # (맥 사용자는 한글폰트 추가 필요)



#-----------------------
# 8-5 단골코드 정리하기
#----------------------

#---# 1) 히스토그램 => 연속확률밀도 그래프 변환

faithful <- faithful
hist(faithful$waiting)          # 히스토그램 그리기
plot(density(faithful$waiting)) # 연속확률밀도 변환

#---# 2) 회귀분석 => 배기량(displ)과 시내연비(cty) 관계 분석

mpg_lm <- lm(mpg$cty ~ mpg$displ)  # 회귀식 모델링
mpg_lm
ggplot(mpg, aes(x=displ, y=cty)) +  # 회귀모델 차트
  geom_point() +
  stat_smooth(method='lm', colour="red", linetype = "dashed")

#---# 3) 주성분 분석 => iris 데이터 PCA 분석

library(ggfortify)
df <- iris[1:4]     # 데이터 추출
pca <- prcomp(df, scale. = TRUE) # 주성분 분석
pca

# 바이플롯 시각화
autoplot(pca, data = iris, colour = 'Species', loadings.label=T, loadings.label.size=3)


#---# [참고] 맥 사용자를 위한 한글폰트 #-------------------------#
require(showtext)  # install.packages("showtext")
font_add_google(name='Nanum Gothic', regular.wt=400, bold.wt=700)
showtext_auto()
showtext_opts(dpi=112)
#----------------------------------------------------------------#
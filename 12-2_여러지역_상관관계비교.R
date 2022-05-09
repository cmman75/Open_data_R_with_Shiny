
#------------------------------
# 12-2_여러지역_상관관계비교
#------------------------------
# 실행할 때 [Run App] 버튼을 누르지 말고 Ctrl + Enter(실행 단축키) 키를 사용하세요
#---# [1단계: 데이터 준비]

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
load("./06_geodataframe/06_apt_price.rdata")
library(sf)
apt_price <- st_drop_geometry(apt_price)   # 공간정보 제거
apt_price$py_area <- round(apt_price$area / 3.3, 0)   # 크기변환 (㎡->평)
head(apt_price$py_area)

#---# [2단계: 샤이니 UI]

library(shiny)    # install.packages("shiny")  
library(ggpmisc)  # install.packages("ggpmisc")  

ui <- fluidPage(     
  #---# 타이틀 입력
  titlePanel("여러지역 상관관계비교"),  
  #---# 
  fluidRow(
    #---# 선택메뉴 1: 지역선택
    column(6,
      selectInput(
        inputId = "region",           # 입력 아이디
        label = "지역을 선택하세요",  # 라벨
        unique(apt_price$addr_1),     # 선택범위
        multiple = TRUE)),            # 복수선택 옵션
    #---# 선택메뉴 2: 크기선택
    column(6,
      sliderInput(
        inputId = "range_py",         # 입력 아이디
        label = "평수를 선택하세요",  # 라벨  
        min = 0,                      # 선택범위 최솟값
        max = max(apt_price$py_area), # 선택범위 최댓값
        value = c(0, 30))),           # 디폴트 선택범위
    #---# 출력    
    column(12,
      plotOutput(outputId = "gu_Plot", height="600")))  # 차트 출력
)

#---# [3단계: 샤이니 Server]

server <- function(input, output, session){
  #---# 반응식
  apt_sel = reactive({
    apt_sel = subset(apt_price,
      addr_1 == unlist(strsplit(paste(input$region, collapse =','),",")) &              
      py_area >= input$range_py[1] & 
      py_area <= input$range_py[2]) 
    return(apt_sel)
    })
  #---# 지역별 회귀선(linear regression line) 그리기
  output$gu_Plot <- renderPlot({
    if (nrow(apt_sel()) == 0)  # 무선택시 에러 방지
       return(NULL)
    ggplot(apt_sel(), aes(x = py_area, y = py, col="red")) +  # 축 설정
      geom_point() +                                # 플롯 유형: 포인트
      geom_smooth(method="lm", col="blue") +        # 회귀선
      facet_wrap(~addr_1, scale='free_y', ncol=3) + # 카테고리별로 그리기
      theme(legend.position="none") +               # 테마설정
      xlab('크기(평)') +         # x축 설정
      ylab('평당가격(만원)') +   # y축 설정
      stat_poly_eq(aes(label = paste(..eq.label..)),  # 회귀식 설정 
                   label.x = "right", label.y = "top",
                   formula = y ~ x, parse = TRUE, size = 5, col="black") 
  })
}

#---# [4단계: 실행]
shinyApp(ui, server) 


#---# [참고] 맥 사용자를 위한 한글폰트 #-------------------------#
require(showtext)  # install.packages("showtext")
font_add_google(name='Nanum Gothic', regular.wt=400, bold.wt=700)
showtext_auto()
showtext_opts(dpi=112)
#----------------------------------------------------------------#

# https://cmman75.shinyapps.io/apt_regression/
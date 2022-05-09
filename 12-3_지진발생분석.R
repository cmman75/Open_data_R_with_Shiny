
#--------------------
# 12-3_지진발생분석
#--------------------
# 실행할 때 [Run App] 버튼을 누르지 말고 Ctrl + Enter(실행 단축키) 키를 사용하세요
#---# [1단계: 데이터 준비]

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
load("./01_code/earthquake/earthquake_16_21.rdata")
head(quakes, 2)

#---# [2단계: 샤이니 UI]

library(shiny)
library(leaflet)
library(ggplot2)
library(ggpmisc)

ui <- bootstrapPage( 
  #---# 사용자 화면 페이지 스타일 설정 
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  #---# 지도생성
  leafletOutput("map", width = "100%", height = "100%"),
  #---# 메뉴 패널
  absolutePanel(top = 10, right = 10,
    #---# 슬라이드 입력(진도)
    sliderInput(
      inputId = "range",         # 입력 아이디
      label = "진도",            # 라벨
      min = min(quakes$mag),     # 선택범위 최솟값
      max = max(quakes$mag),     # 선택범위 최댓값
      value = range(quakes$mag), # 디폴트 선택범위
      step = 0.5                 # 단계
    ),
    #---# 슬라이드 입력(기간)
    sliderInput(
      inputId = "time",         # 입력 아이디
      label = "기간",           # 라벨
      sep = "",
      min = min(quakes$year),     # 선택범위 최솟값
      max = max(quakes$year),     # 선택범위 최댓값
      value = range(quakes$year), # 디폴트 선택범위
      step = 1                 # 단계
    ),
    #---# 출력: 빈도 히스토그램
    plotOutput("histCentile", height = 230),
    #---# 출력: 빈도 - 깊이 산점도 
    plotOutput("depth", height = 230),
    # p(span("자료출처: 기상청", align = "center",
    #   style = "color:black;background-color:white"))
  )
)

#---# [3단계: 샤이니 Server]

server <- function(input, output, session) {
  #---# 반응식
  filteredData <- reactive({
    quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2] &
           quakes$year >= input$time[1] & quakes$year <= input$time[2],]
  })
  output$map <- renderLeaflet({
    leaflet(quakes) %>% addTiles() %>%
    fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
  })
  #---# 히스토그램
  output$histCentile <- renderPlot({
    if (nrow(filteredData()) == 0)
     return(NULL)
    centileBreaks <- hist(plot = FALSE, filteredData()$mag, breaks = 20)$breaks
    hist(filteredData()$mag,
         breaks = centileBreaks,
         main = "지진발생정보", xlab = "진도",  ylab = "빈도",
         col = 'blue',  border = 'grey')
  })
  #---# 회귀분석
  output$depth <- renderPlot({
    ggplot(filteredData(), aes(x=mag, y=-depth)) + 
      geom_point(size=3, col="red") +
      geom_smooth(method="lm", col="blue") +          # 회귀선
      xlab('진도') +  ylab('깊이') +
      stat_poly_eq(aes(label = paste(..eq.label..)),  # 회귀식
        label.x = "right", label.y = "top",
        formula = y ~ x, parse = TRUE, size = 5, col="black") 
   })
  #---# 입력값 변경에 따른 지도 업데이트
  observe({
    leafletProxy("map", data = filteredData()) %>% clearShapes() %>%
    addCircles(
      radius = ~log((quakes$mag))^20, 
      weight = 1, color = "grey70",
      fillColor = "red", fillOpacity = 0.6, popup = ~paste(mag)
      )
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

#  https://cmman75.shinyapps.io/earthquake/
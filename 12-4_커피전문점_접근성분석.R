
#--------------------------------------
# 12-4_앱 프렌차이즈 까페 입지특성 분석
#--------------------------------------
# 실행할 때 [Run App] 버튼을 누르지 말고 Ctrl + Enter(실행 단축키) 키를 사용하세요
#---# [1단계: 데이터 준비]

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # 작업폴더 설정
load( "./01_code/coffee/coffee_shop.rdata")
head(coffee_shop, 2)

#---# [2단계: 샤이니 UI]

library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(ggplot2)

ui <- bootstrapPage( 
  #---# 사용자 화면 페이지 스타일 설정 
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  #---# 지도생성
  leafletOutput("map", width = "100%", height = "100%"),
  #---# 메뉴 패널 
  absolutePanel(top = 10, right = 10,
    #---# 선택 입력 
    selectInput(
      inputId = "sel_brand",  # 입력 아이디
      label = tags$span(      # 라벨
        style="color: black;","프렌차이즈를 선택하시오"), 
      choices = unique(coffee_shop$brand),       # 선택리스트
      selected = unique(coffee_shop$brand)[2]),  # 디폴트 선택
    #---# 슬라이드 입력
    sliderInput(           # 입력 아이디
      inputId = "range",   # 라벨
      label = tags$span(   # 선택 리스트
        style="color: black;","접근성 범위를 선택하시오"), 
      min = 0,             # 선택범위 최솟값 
      max = 100,           # 선택범위 최댓값
      value = c(60, 80),  # 디폴트 선택범위
      step = 10),          # 단계
    #---# 출력
    plotOutput("density", height = 230),
  )
)

#---# [3단계: 샤이니 Server]

server <- function(input, output, session) {
  #---# 반응식 1: 브랜드 선택 + 접근성 범위
  brand_sel = reactive({
    brand_sel = subset(coffee_shop, 
      brand == input$sel_brand &
      metro_idx >= input$range[1] & 
      metro_idx <= input$range[2] 
    )
  })
  #---# 반응식 2: 브랜드 선택
  plot_sel = reactive({
    plot_sel = subset(coffee_shop, 
                       brand == input$sel_brand
    )
  })
  #---# 밀도함수 출력
  output$density <- renderPlot({
    ggplot(data = with(density(plot_sel()$metro_idx), 
      data.frame(x, y)), mapping = aes(x = x, y = y)) +
      geom_line() +
      xlim(0, 100) +
      xlab('접근성 지수') +  ylab('빈도') +
      geom_vline(xintercept=input$range[1], color='red', size = 0.5) +
      geom_vline(xintercept=input$range[2], color='red', size = 0.5) +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
  })
  #---# 지도출력
  output$map <- renderLeaflet({
    leaflet(brand_sel(), width = "100%", height = "100%") %>%
      addTiles() %>%
      setView(lng = 127.0381 , lat = 37.59512, zoom = 11) %>% 
      addPulseMarkers(lng = ~x, lat = ~y,
                      label = ~name,
                      icon = makePulseIcon())
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

# https://cmman75.shinyapps.io/coffee/




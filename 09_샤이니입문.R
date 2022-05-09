
#------------------------
# 9-1 처음 만나는 샤이니
#------------------------

#---# [1단계: 샤이니 기본구조의 이해]
# 실행할 때 [Run App] 버튼을 누르지 말고 Ctrl + Enter(실행 단축키) 키를 사용하세요
  
library(shiny)  # install.packages("shiny")  
ui <- fluidPage("사용자 인터페이스")  # 구성 1: ui
server <- function(input, output, session){}  # 구성 2: server
shinyApp(ui, server)  # 구성 3: 실행

#---# [2단계: 샤이니가 제공하는 샘플 확인하기]

library(shiny)    # 라이브러리 등록
runExample()      # 샘플 보여주기
runExample("01_hello")   # 첫 번째 샘플 실행하기

#---# [참고: 올드 페이스풀 간혈천 관측자료]

faithful <- faithful
head(faithful, 2)

#---# [3단계: 01_hello 샘플의 사용자 인터페이스 부분]

library(shiny)       # 라이브러리 등록
ui <- fluidPage(     # 사용자 인터페이스 시작: fluidPage 정의
  titlePanel("샤이니 1번 샘플"),  # 타이틀 입력
  #---# 레이아웃 구성: 사이드바 패널 + 메인패널 
  sidebarLayout(
    sidebarPanel(  # 사이드바 패널 시작
      #--- 입력값: input$bins 저장
      sliderInput(inputId = "bins",         # 입력 아이디  
                  label = "막대(bin)갯수:",  # 텍스트 라벨  
                  min = 1, max = 50,        # 선택 범위(1-50)
                  value = 30)),             # 기본 선택 값 30
    mainPanel(   # 메인패널 시작
      #---# 출력값: output$distPlot 저장
      plotOutput(outputId = "distPlot"))  # 차트 출력
  ))

#---# [4단계: 01_hello 샘플의 서버 부분]

server <- function(input, output, session){
  #---# 랜더링한 플롯을 output 인자의 distPlot에 저장
  output$distPlot <- renderPlot({
    x <- faithful$waiting # 분출대기시간 정보 저장
    #---# input$bins을 플롯으로 랜더링
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #---# 히스토그램 그리기 (맥 사용자 폰트 추가 필요)
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "다음 분출때까지 대기시간(분)",  
         main = "대기시간 히스토그램")
  })
}
#---# 실행
shinyApp(ui, server)
rm(list = ls())  # 메모리 정리하기 


#---------------------
# 9-2 입력과 출력하기
#---------------------

#---# [1단계: 데이터 입력]

library(shiny) 
ui <- fluidPage(   
  sliderInput("range", "연비", min = 0, max = 35, value = c(0, 10))) # 입력

server <- function(input, output, session){}  # 반응 없음

shinyApp(ui, server)  # 실행

#---# [2단계: 데이터 출력]

library(shiny) 
ui <- fluidPage(
  sliderInput("range", "연비", min = 0, max = 35, value = c(0, 10)), # 입력
  textOutput("value"))  # 결괏값 갱신 안됨

server <- function(input, output, session){
  output$value <- renderText((input$range[1] + input$range[2]))}  # 랜더링 함수 없어서 오류 발생

shinyApp(ui, server)

#---# [참고: 렌더링 함수의 중요성]

library(shiny) 
ui <- fluidPage(
  sliderInput("range", "연비", min = 0, max = 35, value = c(0, 10)), # 입력
  textOutput("value"))    # 결과 출력

server <- function(input, output, session){
  output$value <- (input$range[1] + input$range[2])}   # 반응값 => 오류발생

shinyApp(ui, server)


#-------------------------------
# 9-3 반응성: 입력과 출력의 연결
#-------------------------------

#---# [1단계: 데이터 준비]

library(DT)      # install.packages("DT")
library(ggplot2) # install.packages("ggplot2")
mpg <- mpg
head(mpg)

#---# [2단계: 반응식 작성]

library(shiny) 
ui <- fluidPage(
  sliderInput("range", "연비", min = 0, max = 35, value = c(0, 10)), # 입력
  DT::dataTableOutput("table"))   # 출력

server <- function(input, output, session){
  #---# 반응식
  cty_sel = reactive({  
    cty_sel = subset(mpg, cty >= input$range[1] & cty <= input$range[2])
    return(cty_sel)})    
  #---# 반응결과 렌더링
  output$table <- DT::renderDataTable(cty_sel()) }

shinyApp(ui, server)


#-------------------------
# 9-4 화면설정과 레이아웃
#-------------------------

#---# [1단계: 단일 페이지 화면]

library(shiny)
#---# 전체 페이지 정의
ui <- fluidPage(  
  #---# 행 row 구성 정의
  fluidRow(    
    #---# 첫번째 열: 붉은색(red) 박스로 높이 450 픽셀, 폭 9
    column(9, div(style = "height:450px;border: 4px solid red;","폭 9")),
    #---# 두번째 열: 보라색(purple) 박스로 높이 450 픽셀, 폭 3
    column(3, div(style = "height:450px;border: 4px solid purple;","폭 3")),
    #---# 세번째 열: 파란색(blue) 박스로 높이 400 픽셀, 폭 12
    column(12, div(style = "height:400px;border: 4px solid blue;","폭 12"))))
server <- function(input, output, session) {}
shinyApp(ui, server)

#---# [2단계: 탭 페이지 추가]

library(shiny)
ui <- fluidPage(
  fluidRow(
    column(9, div(style = "height:450px;border: 4px solid red;","폭 9")),
    column(3, div(style = "height:450px;border: 4px solid red;","폭 3")),
    #---# 탭패널 1~2번 추가 
    tabsetPanel(
      tabPanel("탭1",   
               column(4, div(style = "height:300px;border: 4px solid red;","폭 4")),
               column(4, div(style = "height:300px;border: 4px solid red;","폭 4")),           
               column(4, div(style = "height:300px;border: 4px solid red;","폭 4")), ),              
      tabPanel("탭2", div(style = "height:300px;border: 4px solid blue;","폭 12")))))
server <- function(input, output, session) {}
shinyApp(ui, server)


#---# [참고] 맥 사용자를 위한 한글폰트 #-------------------------#
require(showtext)  # install.packages("showtext")
font_add_google(name='Nanum Gothic', regular.wt=400, bold.wt=700)
showtext_auto()
showtext_opts(dpi=112)
#----------------------------------------------------------------#

#------------------------------
# 12-1_아파트가격_상관관계분석
#------------------------------
# 실행할 때 [Run App] 버튼을 누르지 말고 Ctrl + Enter(실행 단축키) 키를 사용하세요
#---# [1단계: 데이터 준비]

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # 작업폴더 설정
load("./06_geodataframe/06_apt_price.rdata")              # 실거래 불러오기
library(sf)
apt_price <- st_drop_geometry(apt_price)   # 공간정보 제거
apt_price$py_area <- round(apt_price$area / 3.3, 0)   # 크기변환 (㎡ -> 평)
head(apt_price$py_area)

require(showtext)
font_add_google(name='Nanum Gothic', regular.wt=400, bold.wt=700)
showtext_auto()
showtext_opts(dpi=112)

#---# [2단계: 샤이니 UI]

library(shiny)
ui <- fluidPage(
  #---# 제목 설정
  titlePanel("아파트가격 상관관계분석"),
  #---# 사이드 판넬
  sidebarPanel(
    #---# 선택메뉴 1: 지역
    selectInput(
      inputId = "sel_gu",           # 입력 아이디
      label = "지역을 선택하세요",  # 라벨 
      choices = unique(apt_price$addr_1),       # 선택리스트
      selected = unique(apt_price$addr_1)[1]),  # 디폴트 선택
    #---# 선택메뉴 2: 크기(평)
    sliderInput(
      inputId = "range_py", # 입력 아이디
      label = "평수",       # 라벨
      min = 0,                       # 선택범위 최솟값
      max = max(apt_price$py_area),  # 선택범위 최댓값
      value = c(0, 30)),             # 디폴트 선택범위
    #---# 선택메뉴 3: x축 변수
    selectInput(
      inputId = "var_x",               # 입력 아이디
      label = "X축 변수를 선택하시오", # 라벨 
      choices = list(                  # 선택 리스트
        "매매가(평당)"="py",        
        "크기(평)"="py_area",
        "건축연도"="con_year",
        "층수"="floor"),
      selected = "py_area"),           # 디폴트 선택
    #---# 선택메뉴 4: y축 변수
    selectInput(
      inputId = "var_y",               # 입력 아이디
      label = "Y축 변수를 선택하시오", # 라벨
      choices = list(                  # 선택 리스트
        "매매가(평당)"="py",        
        "크기(평)"="py_area",
        "건축연도"="con_year",
        "층수"="floor"),
      selected = "py"),                # 디폴트 선택
    #---# 체크박스 1: 상관계수
    checkboxInput(                 
      inputId = "corr_checked",     # 입력 아이디
      label = strong("상관계수"),   # 라벨
      value = TRUE),                # 디폴트 선택
    #---# 체크박스 2: 회귀계수
    checkboxInput(
      inputId = "reg_checked",    # 입력 아이디
      label = strong("회귀계수"), # 라벨
      value = TRUE)               # 디폴트 선택
     ), 
  #---# 메인 판넬
  mainPanel(
    #---#
    h4("플로팅"),               # 라벨
    plotOutput("scatterPlot"),  # 플롯출력
    #---#
    h4("상관계수"),                  # 라벨
    verbatimTextOutput("corr_coef"), # 텍스트 출력
    #---#
    h4("회귀계수"),                # 라벨
    verbatimTextOutput("reg_fit")  # 텍스트 출력
  )  
)  

#---# [3단계: 샤이니 Server]

server <- function(input, output, session) {
  #---# 반응식
  apt_sel = reactive({               
    apt_sel = subset(apt_price,       
      addr_1 == input$sel_gu &        # 조건 1
      py_area >= input$range_py[1] &  # 조건 2
      py_area <= input$range_py[2])   # 조건 3
    return(apt_sel)
    })
  #---# 회귀선(linear regression line) 그리기
  output$scatterPlot <- renderPlot({
    var_name_x <- as.character(input$var_x)  # x축 이름
    var_name_y <- as.character(input$var_y)  # y축 이름 설정
    #---# 
    plot(
      apt_sel()[, input$var_x], #  x축 설정
      apt_sel()[, input$var_y], #  y축 설정
      xlab = var_name_x,   # x축 이름 라벨
      ylab = var_name_y,   # y축 이름 라벨
      main = "플로팅")     # 플롯 제목
      fit <- lm(apt_sel()[, input$var_y] ~ apt_sel()[, input$var_x]) # 회귀식
      abline(fit, col="red")    # 회귀선 그리기
    })
  #---# 상관계수
  output$corr_coef <- renderText({
    if(input$corr_checked){          # 체크박스 1 유무 확인
      cor(apt_sel()[, input$var_x],  # 상관분석 x축 설정
          apt_sel()[, input$var_y])  # 상관분석 y축 설정
      }
  })
  #---# 회귀계수
  output$reg_fit <- renderPrint({
    if(input$reg_checked){          # 체크박스 2 유무 확인
      fit <- lm(apt_sel()[, input$var_y] ~ apt_sel()[, input$var_x]) # 회귀분석
      names(fit$coefficients) <- c("Intercept", input$var_x)         # 회귀계수 추출
      summary(fit)$coefficients                                      # 회귀계수 요약
      }
    })
}

#---# [4단계: 실행]

shinyApp(ui = ui, server = server)

# https://cmman75.shinyapps.io/apt_dashboard/

# 독자분들께서 교재 실습 도중에 질문이 오는 경우가 종종 있어서 관련 부분 정리하여 공유합니다.


#---# [P.43] 60번 라인 #---#

# Q. 공공데이터 포털에 URL로 크롤링 요청시 아래와 같이 SERVICE KEY IS NOT REGISTERED ERROR 라는 메시지만 나옵니다.

  <resultCode>99</resultCode>
  <resultMsg>SERVICE KEY IS NOT REGISTERED ERROR.</resultMsg>

# A. 서비스키를 방금 신청하신 것 같습니다.
        처음 발급받은 경우 최소 1시간 기다리셔야 등록작업이 완료됩니다.
        잠깐 쉰 다음에(1~2시간 후) 다시 시도하시면 정상적으로 크롤링 하실 수 있습니다.
        
        
        
#---# [P.47] 114번 라인 #---#

# Q1. 숫자 115, 119  그리고 130, 135 무슨 의미이며 왜 필요한가요?

# A1. 이 코드는 내가 지금 크롤링 하는 지역이 어디인지 모니터에 출력하기 위한 코드입니다.
      다시 말해서, url_list 내에서 요청한 지역 코드(11110)가 어딘지 추출해서 지역명(서울_종로)으로 돌려주고자 만든 코드입니다.
      샘플로 설명드리면 다음과 같습니다.
      str_sub로 url_list에 있는 문자열의 일부(115번 ~ 119번)를 추출할 수 있습니다.
    #------------------------------------#
    > str_sub(url_list[1],115, 119)
    [1] "11110"
    #------------------------------------#
      이제 subset() 함수를 이용하면 해당 지역의 코드번호가 어느 지역인지 알 수 있습니다.
      loc 파일의 code 컬럼 11110에 해당하는 addr_1이 "서울_종로" 이기 때문입니다.
    #------------------------------------#
    > subset(loc, code== str_sub("11110"))$addr_1
    [1] "서울_종로"
    #------------------------------------#
     이를 통해서 현재 내가 수집하는 데이터가 어느 지역의 데이터인지 알 수 있을 뿐만 아니라 파일 이름 역시 아래와 같이 지역이름으로 설정할 수 있습니다.
     크롤링하다 보면 데이터가 꼬일 때가 있는데 파일 이름을 숫자코드가 아닌 지역명(서울_종로_202101.csv)으로 하는 것이 나중에 문제가 발생하였을 때 빠르게 원인파악을 할 수 있습니다.



#---# [P.228] #---#

# Q1. od_line <- od2line(od_intra2, fishnet) 실행시 다음과 같은 에러가 발생합니다. 어떻게 해결해야 하나요?
  #------------------------------------#
  Error in UseMethod("od2line", object = zones) :
  no applicable method for 'od2line' applied to an object of class "c('SpatialPolygonsDataFrame', 'SpatialPolygons', 'Spatial', 'SpatialPolygonsNULL', 'SpatialVector')"
  #------------------------------------#
  
# A1. 일단 에러 원인부터 말씀드리자면 od2line(A , B)에서 A 와 B 의 공간 포맷 형식이 달라서 나타나는 문제 입니다.
      해당 에러의 원인을 파악하고 정상적 실행을 위하여 다음의 네 단계를 따라 진행하여 보십시오.

  1) 두 파일 A 와 B 의 속성을 보시면 형식이 다름니다.
    > class(fishnet)     # sp 포맷
    > class(od_intra2)   # sf 포맷(데이터프레임)

  2) 따라서 다음의 명령어로 강제로 fishnet 을 sf 포맷으로 변환하였습니다(두 데이터의 형태를 일치시키기 위함).
    > fishnet <- as(fishnet, "sf")

  3) 그런 다음 od2line 명령어를 실행하면 아무런 문제없이 Origin - Destination 를 연결하는 선이 그려질 것입니다.
    > od_line <- od2line(od_intra2, fishnet)  

  4) 일정 건수 이상의 선만 필터링 하기 위해서는 다음의 명령어를 사용해 보시면 됩니다.
     이용자 수 20건 이상인 O-D 라인 필터링
    > od_line <- od_line[od_line$총이용객수 > 20, ]  


#---# [P.229] #---#

# Q1. od_line_sln <- SpatialLinesNetwork(od_line) 실행시 다음과 같은 에러가 발생합니다. 어떻게 해결해야 하나요?
  #------------------------------------#
  Error in SpatialLinesNetwork(od_line) :
  could not find function "SpatialLinesNetwork"
  #------------------------------------#

# A1. 이 문제는 stplanr 라이브러리의 버전 문제인것 같습니다.
      packageVersion("stplanr") 해보면 1.0XXXX 버전 나오시지 않나요?
      버전이 1.0 x 라면 아래 순서대로 실행하여 다운 그레이드(1.0 => 0.85) 해 주십시오.
      
  1) 패키지 삭제 명령어를 실행하십시오
  > remove.packages("stplanr")

  2) R 스튜디오를 종료 후 재실행 하여 주십시오
  
  3) 0.85 버전을 설치하여 주십시오
  > remotes::install_github("ropensci/stplanr", ref = "v0.8.5")

  이 때 옵션으로 1번하면 에러가 나는 경우가 있더라구요 / 2번으로 하면 괜찮은것 같습니다.

  1: All                            
  2: CRAN packages only              
  3: None                            
  4: .....

  4) 패키지 버전을 확인하여 주십시오
  > packageVersion("stplanr")
  [1] ‘0.8.5’

  5) 이제 에러가 발생하였던 코드를 실행하여 보면 정상적으로 실행될 겁니다.
  > od_line_sln <- SpatialLinesNetwork(od_line)    

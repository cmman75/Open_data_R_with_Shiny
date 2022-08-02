# [Do it!] 공공데이터로 배우는 R 데이터 분석 with 샤이니


챕터별 스크립트 파일 입니다.


2022-07-30  배포한 샤이니 애플리케이션을 보면, 해당 그리드와 지역적으로 상관없는 아파트들까지 테이블에 잡힙니다.




2022-07-18 부록.R 의 stplanr 라이브러리의 버전 문제로 일부 기능이 실행되지 않는다는 제보를 주셔서 보완하였습니다.

  Q: p.229 2단계 <통근 시간대 커뮤니티 탐지> 부분에서 library(stplanr)까지 잘 했는데, SpatialLinesNetwork 함수를 찾을 수 없다는 오류가 발생합니다.
     확인 부탁드립니다.
     
  A: 이 문제는 stplanr 라이브러리의 버전 문제(1.0 => 0.85)인 것으로 파악되었습니다.
     packageVersion("stplanr") 해보면 1.0XXXX 버전 나오시지 않으신가요?
     만약 버전이 1.0 x 라면 아래 순서대로 실행하여 주십시오.
     
     (1) 패키지 삭제 명령어를 실행하십시오
     > remove.packages("stplanr")

     (2) R 스튜디오를 종료 후 재실행 하여 주십시오

     (3) 0.85 버전을 설치하여 주십시오
     > remotes::install_github("ropensci/stplanr", ref = "v0.8.5")

     이 때 옵션으로 1번하면 에러가 나는 경우가 있어 2번으로 하면 괜찮은것 같습니다.
     1: All                            
     2: CRAN packages only              
     ....
     
     (4) 패키지 버전을 확인하여 주십시오
     > packageVersion("stplanr")
     [1] ‘0.8.5’
     
     이제 이 부분을 실행하여 보면 에러 없이 정상적으로 실행될 겁니다.
     > od_line_sln <- SpatialLinesNetwork(od_line)  

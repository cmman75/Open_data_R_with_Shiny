### [Do it!] 공공데이터로 배우는 R 데이터 분석 with 샤이니

8) [23-12-28]  ## 긴급공지 !!!!

   공공데이터포털에서 아파트 실거래가 정보 구조가 일부 변경되어 '02_자료수집.R' 스크립트의 일부를 다음과 같이 변경하였습니다.
   (독자분들의 제보로 변경사실을 알게 되었습니다. 감사합니다. !!!)
   지금부터는 새로운 스크립트를 받아서 연습하여 보시면 됩니다.
   엑셀에서 csv 파일을 열어보았을 때 정렬이 잘 안되어 있는 것 같지만 끝까지 실행하여 보시면 정확하게 결과가 도출됩니다.
   (그리고 csv 파일을 여실 때 엑셀보다는 메모장을 추천해드립니다.)

   --- 변경이전 스크립트 ---
   item_temp_dt <- data.table(year = item_temp[4],     # 거래 년
                              month = item_temp[7],    # 거래 월
                              day = item_temp[8],      # 거래 일
                              price = item_temp[1],    # 거래금액
                              code = item_temp[12],    # 지역코드
                              dong_nm = item_temp[5],  # 법정동
                              jibun = item_temp[11],   # 지번
                              con_year = item_temp[3], # 건축연도 
                              apt_nm = item_temp[6],   # 아파트 이름   
                              area = item_temp[9],     # 전용면적
                              floor = item_temp[13])   # 층수

   
   --- 변경이후 스크립트 ---
   item_temp_dt <- data.table(year = item_temp[4],     # 거래 년 
                              month = item_temp[8],    # 거래 월
                              day = item_temp[9],      # 거래 일
                              price = item_temp[1],    # 거래금액
                              code = item_temp[13],    # 지역코드
                              dong_nm = item_temp[6],  # 법정동
                              jibun = item_temp[12],   # 지번
                              con_year = item_temp[3], # 건축연도 
                              apt_nm = item_temp[7],   # 아파트 이름   
                              area = item_temp[10],     # 전용면적
                              floor = item_temp[14])   # 층수 




7) [23-12-06]
   
   부록에 들어있는 화성시 교통카드 데이터 분석 예제 데이터에 문제가 생겨서 다시 업로드하였습니다.
   SBJ_2003_001.zip ~ SBJ_2003_001.z02 까지 세 개 파일을 다운받으시면 됩니다.
   파일이 잘 안풀리는 경우가 있어서 원본 위치 링크 올려드립니다. 전체 다운받기로 다운 받으시면 됩니다.
   
   https://compas.lh.or.kr/subj/past/data?subjNo=SBJ_1910_001

6) [23-12-06] 

   전국 시군별 부동산 데이터 분석
   서울 외 다른 지역의 데이터 분석을 원하시는 독자분들이 많아서 전국 시군별 분석 가능한 스크립트를 업로드 하였습니다.
   깃허브에 분할 업로드 되어 있습니다.
   2208_응용 스크립트_전국 부동산 데이터 분석.zip ~ 2208_응용 스크립트_전국 부동산 데이터 분석.z04 까지 다섯 개 파일 다운로드 받으시면 됩니다.

 5) [22-12-26]
   
    공공데이터포털에서 API 신청하실때「아파트매매 실거래 상세 자료」가 아닌「아파트매매 실거래」를 신청하셔야 합니다.
    다른 API를 신청하신 다음에 크롤링 하시면 SERVICE ACCESS DENIED ERROR가 나옵니다.
    
    이 때 아래 링크에 있는「아파트매매 실거래」API를 신청하시면 정상 작동 될겁니다.
    https://www.data.go.kr/tcs/dss/selectApiDataDetailView.do?publicDataPk=15058747

4) [22-08-15] 

   독자분들께서 이메일로 문의하신 내용과 답변들을 정리하여 업로드할 예정입니다.
   https://github.com/cmman75/Open_data_R_with_Shiny/blob/main/13_%EC%A7%88%EB%AC%B8%EA%B3%BC%20%EB%8B%B5%EB%B3%80(Q%26A)
   
3) [22-08-06] 

    챕터별 강의 PPT 업로드: 깃허브에서 파워포인트(01-12강).zip 다운로드 받으시면 됩니다.
    https://github.com/cmman75/Open_data_R_with_Shiny/blob/main/%ED%8C%8C%EC%9B%8C%ED%8F%AC%EC%9D%B8%ED%8A%B8(01-12%EA%B0%95).zip
              
2) [22-06-01] 

   전체 데이터.zip: 최종 완성본 파일을 원하시는 독자분들이 있어 압축파일을 올립니다.

1) [22-05-25] 

   챕터별 스크립트 파일 업로드 하였습니다.
   
#-----#   
   
@ 문의 또는 질문이 있으시면 아래 주소로 보내 주세요

  김철민 cmman75@gmail.com



# 지식인크롤링
ksg  
2017년 8월 28일  

# 지식인에서 질문답변 크롤링하기
## 검색결과 url만들기위한 준비

```r
keyword ='다이어트'
keyword = toupper(unlist(iconv(keyword, "cp949", "UTF-8", toRaw = TRUE)))
keyword = paste(keyword, collapse = "%")  
keyword = paste0("%", keyword)

# URL과 표적 태그 세팅 
link_front = 'http://kin.naver.com/search/list.nhn?query='
page_num = '&page='
```

## 1단계 : 질문URL 20개 가져오기(1~2페이지만)

```r
library(httr) 
library(rvest)
```

```
## Loading required package: xml2
```

```r
link = NULL
for (i in (1:2)){
  urls = paste0(link_front, keyword, page_num,i)  #url만들기
  html = read_html(urls)
  htxt = html_nodes(html, 'dt')
  htxt = html_nodes(htxt , 'a')
  q_url = html_attr(htxt, 'href') #각 글의 주소 
  link = append(q_url, link)
}

head(link)
```

```
## [1] "http://kin.naver.com/qna/detail.nhn?d1id=7&dirId=70404&docId=282170119&qb=64uk7J207Ja07Yq4&enc=utf8&section=kin&rank=11&search_sort=0&spq=0"  
## [2] "http://kin.naver.com/qna/detail.nhn?d1id=7&dirId=71102&docId=280356123&qb=64uk7J207Ja07Yq4&enc=utf8&section=kin&rank=12&search_sort=0&spq=0"  
## [3] "http://kin.naver.com/qna/detail.nhn?d1id=7&dirId=710&docId=282697075&qb=64uk7J207Ja07Yq4&enc=utf8&section=kin&rank=13&search_sort=0&spq=0"    
## [4] "http://kin.naver.com/qna/detail.nhn?d1id=7&dirId=710&docId=282765173&qb=64uk7J207Ja07Yq4&enc=utf8&section=kin&rank=14&search_sort=0&spq=0"    
## [5] "http://kin.naver.com/qna/detail.nhn?d1id=8&dirId=8030401&docId=281832319&qb=64uk7J207Ja07Yq4&enc=utf8&section=kin&rank=15&search_sort=0&spq=0"
## [6] "http://kin.naver.com/qna/detail.nhn?d1id=7&dirId=703&docId=283026231&qb=64uk7J207Ja07Yq4&enc=utf8&section=kin&rank=16&search_sort=0&spq=0"
```

## 2단계 : 20개 URL에서 질문내용과 답변내용 따오기 

```r
title = NULL
q_date = NULL
q_cont = NULL
a_date = NULL
a_cont = NULL
infor = NULL

for (i in (1:20)){
  htxt = read_html(link[i])
  q_title = html_text(html_node(htxt, 'h3._endTitleText'))
  q_time = html_text(html_nodes(htxt, 'dd.date'))[4]
  q = html_text(html_nodes(htxt, 'div.end_content._endContents'))[1]
  a_time = html_text(html_nodes(htxt, 'dd.date'))[6]
  a = html_text(html_nodes(htxt, 'div.end_content._endContents'))[2]
  
  #각각누적
  title = append(q_title, title)
  q_date = append(q_time, q_date)
  q_cont = append(q, q_cont)
  a_date = append(a_time, a_date)
  a_cont = append(a, a_cont)
}
```

## 데이터프레임으로 합쳐서 저장하기

```r
kin = data.frame(title,q_date,q_cont,a_date, a_cont)

library(qdapRegex)
pre_pro = function(x){
  x = as.character(x)
  x = gsub("[[:digit:]]"," ", x) #숫자
  x = gsub("[[:punct:]]"," ", x) #기호
  x = gsub("[[:cntrl:]]"," ", x) #제어문자
  x = gsub("[[:space:]]"," ", x) #공백문자
  x = gsub("[[:blank:]]"," ", x) #간격문자
  x = rm_white(x)
}

kin$title = pre_pro(kin$title)
kin$q_cont = pre_pro(kin$q_cont)
kin$a_cont = pre_pro(kin$a_cont)

head(kin$q_cont)
```

```
## [1] "아침 현미콩밥 버섯볶음 호박나물 멸치볶음 콩나물무침 토마토 점심 현미콩밥 양배추샐러드 토마토 브로콜리 호박나물 멸치볶음저녁 바나나 개 라이트업시리얼이렇게 항상 같은 시간에 아침 시 시 저녁 시에 먹고있구요간식은 거의 안먹고 먹으면 플레인요거트에 블루베리 한 개 넣어 먹거나 아메리카노 한두잔 정도 마시고 있어요 그리고 운동은 아침에 일어나자마자 시간 정도 빠른 걸음 걷기하고 하루에 윗몸일으키기 번씩 세트다이어트 댄스 곡팔운동 틈틈이 하는중인데이렇게 하면 한달에 얼마나 빠질까요 지금 다이어트 시작한지 주차 되었는데 첫째주엔 거의 kg빠졌어요 식단은 위에거랑 달랐지만요 근데 다이어트 처음 시작할땐 수분이 빠져나가는 거라고 들었는데 그럼 이제부터 이런 식단과 운동이면 얼마동안 어느정도 빠질까요"                                                                                                             
## [2] "다이어트약이라구 병원에서 지어왔는데 검색해보니까 영 이상하길래 여쭤봐요 당뇨병약은 왜 있는거죠"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
## [3] "이번에 다이어트한약으로 다이어트를 제대로 시작해보려고하는데요다이어트한약으로 정말 살이 빠질가요 주위에서는 다이어트한약을 먹어본사람은 없고인터넷에 다이어트한약이라고 검색해봐도 무슨말인지 모르겠고혹시 다이어트한약 에 대해 자세하게 설명해주실분"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
## [4] "한방다이어트를 진행 하는데에 있어서 중요한 부분을 좀 알려주세요 어떻게 한방다이어트를 진행하는게 좋은지를좀 알고 싶습니다 무엇보다 안전하게 다이어트를 진행 하면서더 멋지게 되고 싶으니깐요 한방다이어트를 안전하게 하게 된다면어떠한 반응이 생기고 또 한방다이어트는 요요현상이 없는지도 좀 알고 싶어요 좀 알려주세요 ㅠㅠ아 그리고 한방다이어트 가격도 알려주세요"                                                                                                                                                                                                                                                                                                                                                                                                                                                         
## [5] "선생님들 제 고민 좀 들어주세요저는 중학교 학년 키 몸무게 입니다제가 이번주 월요일부터 다이어트를 했어요아직 성장기라 먹는것은 아침 점심 저녁만 먹고 있습니다 시이후러 아무것도 안먹고 있어요 수박 조각 먹은적 한번 있었어요운동은 학교가기 전 공복에 분 줄넘기를 하고요저녁에 시간 반 정도 근력 분 유산소 분정도 근력운동과 뛰기 유산소 를 합니다 근력은 팔굽혀 펴기 개 set 그리고 필굽혀펴기 비슷한거 종류 더 합니다 제가 주전에도 일주일간만 다이어트를 했는데 때는 먹는것도 조금 거 먹었는데 일주일만에 운동만으로 키러를 뺏어요근데 이번에는 꾸준히 할려고 하는데 살이 안빠짐니다 물도 하루에 리터는 먹는거 같아요운동 하기전 한 후 차이는 정도 바께 없거나 그대로에요몸에 수분이 많으면 물 먹는거 안좋다던데 맞는가요 인바디 같은거 해보고 싶은데 없어서 못해요제가 정상인건가요 꾸준히 하면 되나요 선생님들 알려주세요"
## [6] "다이어트하면요뭐하면요살이빠지나요궁금하내요"
```


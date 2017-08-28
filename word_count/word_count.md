# Word_count
ksg  
2017년 8월 28일  
# Q&A 데이터 중요단어 파악하기
## 데이터 로드
  - 엑셀파일을 로그하기위해서 readxl패키지사용 

```r
library(readxl)
qa = read_excel("D:/KMAC_2017/Data_Analysis/data.xlsx")
str(qa)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	1066 obs. of  4 variables:
##  $ title   : chr  "11월 1일에 미로방 예약한 팀입니다.:-)" "문의드립니다." "드디어 이번주네요^^" "조식은 어떻게 제공되나요?" ...
##  $ author  : chr  "이혜운" "가을가을" "정윤경" "궁금" ...
##  $ question: chr  "안녕하세요! 11월 1일에 미로방 예약한 이혜운입니다.\r\n\r\n<U+00A0>저희 숯불 신청 해 놓으려고요.\r\n\r\n<U+00A0>"| __truncated__ "안녕하세요\r\n\r\n내일 예약해서 체크인 예정입니다. 룸에 기본적인 세면도구가 있는지 궁금해서요...^^;;\r\n\r\n수"| __truncated__ "안녕하세요^^ \r\n\r\n잘 지내셨어요? 갑자기 아이가 입원하는 바람에 예약을 변경했네요...변경해 주셔서 너무 감사해"| __truncated__ "조식 사진이나 메뉴구성에 대한 이야기가 없어서..\r\n\r\n12월주중에 가려고 합니다.\r\n\r\n\r\n\r\n와이파이나 DVD "| __truncated__ ...
##  $ answer  : chr  "안녕하세요 미술관옆동물원 입니다.\r\n\r\n\r\n\r\n숯불만 하실경우 따로 예약은 안하셔도 됩니다.\r\n\r\n숯불 준비"| __truncated__ "안녕하세요<U+00A0> 미술관옆동물원 입니다.\r\n\r\n\r\n\r\n수건 헤어드라이기 샴푸 린스 치약 바디 비누 준비되어 있"| __truncated__ "안녕하세요 미술관옆동물원 입니다.\r\n\r\n아이는 이제 괜찮아 졌나요??^^\r\n\r\n바베큐 세트 말씀하신대로 준비되시"| __truncated__ "안녕하세요 미술관옆동물원 입니다.\r\n\r\n\r\n\r\n토스트와 커피를 제공해 드리고 있습니다.\r\n\r\n\r\n\r\n전객실 "| __truncated__ ...
```

## 텍스트 전처리
  - 전처리 함수
  - 그때그때 상황에 따라 삭제할것 선택하여 사용
  - 전처리후 공백을 말끔히 정리할 수 있게 도와주는 rm_white함수
  - https://www.rdocumentation.org/packages/qdapRegex/versions/0.6.0/topics/rm_white

```r
library(stringr)
library(qdapRegex)

pre_pro = function(x){
  x = as.character(x)
  # x = gsub("[[:digit:]]"," ", x) #숫자
  x = gsub("[[:punct:]]"," ", x) #기호
  x = gsub("[[:cntrl:]]"," ", x) #제어문자
  x = gsub("[[:space:]]"," ", x) #공백문자
  x = gsub("[[:blank:]]"," ", x) #간격문자
  x = rm_white(x)
} 

qa$title = pre_pro(qa$title)
qa$answer = pre_pro(qa$answer)
qa$question = pre_pro(qa$question)
head(qa$question)
```

```
## [1] "안녕하세요 11월 1일에 미로방 예약한 이혜운입니다 저희 숯불 신청 해 놓으려고요 그리고 베드민턴 라켓 등의 운동기구도 사용하고자 하는데 사용이 가능한가요 아 그리고 픽업은 그 날 3시 6시 사이에 역에 도착하면 전화 드리면 되나요 펜션 사진이 너무 좋아서 기대되네요 그럼 답변 부탁드립니다"                                                                                                                                                                                                                                                                                                                             
## [2] "안녕하세요 내일 예약해서 체크인 예정입니다 룸에 기본적인 세면도구가 있는지 궁금해서요 수건 헤어드라이기 샴푸 린스 치약 등등 없으면 챙겨가려고 하는데 답변 부탁드립니다"                                                                                                                                                                                                                                                                                                                                                                                                                                              
## [3] "안녕하세요 잘 지내셨어요 갑자기 아이가 입원하는 바람에 예약을 변경했네요 변경해 주셔서 너무 감사해요 이번주 토요일날 놀러갈 생각에 추운것도 모르겠어요 저희 바베큐 셋트 주문하려구요 2인분 할께요 바베큐셋트하면 숯불가격이 포함인거죠 고기랑 된장찌개랑 밥이랑 야채랑 반찬포함인거죠 2인분 토요일저녁에 먹을 수 있게 준비해주세요 저희가 따로 고기는 조금 더 챙겨가도 되는거죠 1 그날 저녁에 4만원 현금으로 드릴께요 2 숙박비 현금영수증 되나요 되면 부탁좀 드릴께요 그럼 토요일에 뵙겠습니다 저희 혹시 전날 예약없으면 미리 짐 보관 될까요 짐만 넣어놓고 바로 제이드가든으로 가려고요 암튼 추운데 감기조심하시구요"
## [4] "조식 사진이나 메뉴구성에 대한 이야기가 없어서 12월주중에 가려고 합니다 와이파이나 DVD 시청은 가능한가요"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
## [5] "객실내에서 촛불 사용 금지라고 써있는걸 봤는데요 혹시 말씀 드리고 사용할수는 없을까요 티라이트 촛불이라 위험하진 않은데 이벤트용이라 이벤트끝나면 물론 바로 끌꺼구요 답변주세요 ㅎㅎ"                                                                                                                                                                                                                                                                                                                                                                                                                                 
## [6] "안녕하세요 어제 요코방에 묵은 심지은이라고 합니다 제가 핸드폰 충전기를 놓고 온 것 같은데 혹시 보셨나요 가능하시면 착불로 좀 보내주시면 안될까요 6"
```

## 단어 빈도 산출 
  - NIADic이 좋다고 하니깐 useNIADic()
  - KoNLP패키지의 SimplePos09 함수로 형태소분석
  - table 함수로 빈도를 세서 데이터프레임으로 저장

```r
library(KoNLP)
```

```
## Checking user defined dictionary!
```

```r
library(tm)
```

```
## Loading required package: NLP
```

```r
useNIADic()
```

```
## Backup was just finished!
## 983012 words dictionary was built.
```

```r
token = function(x){
  x = sapply(x, FUN = SimplePos09, USE.NAMES = F) 
  x = str_match(x, '([가-힣]+)/[NP]')
  x = x[,2] 
  x = unlist(x)
  x = x[!is.na(x)]
}

q_token = data.frame(table(token(qa$question)))
t_token = data.frame(table(token(qa$title)))
```

## 확인 및 저장
  - 엑셀에서 주요단어 선택하는 작업하기 위해 csv파일로 저장

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following object is masked from 'package:qdapRegex':
## 
##     explain
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
q_token %>% arrange(desc(Freq)) %>% head()
```

```
##     Var1 Freq
## 1 안녕하  254
## 2     월  112
## 3     일   77
## 4   펜션   23
## 5   저희   21
## 6   일날   18
```

```r
t_token %>% arrange(desc(Freq)) %>% head()
```

```
##         Var1 Freq
## 1 문의드립니   95
## 2         문   71
## 3     문의드   56
## 4     바베큐   50
## 5       예약   34
## 6     이용문   27
```

```r
write.csv(q_token, "q_word.csv")
write.csv(t_token, "t_word.csv")
```

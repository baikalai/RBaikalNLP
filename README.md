# RBaikalNLP

* R package for baikalNLP
* About baikalNLP: https://license.baikal.ai/

## Install

library(devtools)  
devtools::install_github("baikalai/RBaikalNLP")  

➡️ [INSTALL](https://github.com/baikalai/RBaikalNLP/blob/main/INSTALL.md) 파일 내용 참고 

## Usage

library(RProtoBuf)  
library(baikalnlp)  

## Functions

- tagger: baikalNLP 서버를 호출하여 문장(들)을 분석
- postag: 분석한 문장의 음절, 태그 배열 보기
- morphs: 분석한 문장의 음절 배열 보기
- nouns: 분석한 문장의 명사 배열 보기
- verbs: 분석한 문장의 동사 배열 보기
- as_json_string: 분석 결과를 JSON 문자열 반환
- print_as_json: 분석 결과를 JSON 으로 표시

## Examples

- 로드/호출
```
> library(RProtoBuf)
> library(baikalnlp)
> m <- tagger("문장을 입력합니다.\n여럿은 이렿게 넣습니다.")
```
- 형태소 분석을 매트릭스로 출력
```
> postag(m, TRUE)
[[1]]
     [,1]     [,2]
[1,] "문장"   "NNG"
[2,] "을"     "JKO"
[3,] "입력"   "NNG"
[4,] "하"     "XSV"
[5,] "ㅂ니다" "EF"
[6,] "."      "SF"

[[2]]
      [,1]     [,2]
[1,] "여럿"   "NNG"
[2,] "은"     "JX"
[3,] "이"     "VCP"
[4,] "렿거"   "EC"
[5,] "이"     "JKS"
[6,] "넣"     "VV"
[7,] "습니다" "EF"
[8,] "."      "SF"
```
- 1번째 문장의 5번째 형태소 출력
```
> postag(m)[[1]][[5]]
$morpheme
[1] "ㅂ니다"

$tag
[1] "EF"
```
- 어절, 명사, 동사 출력(해당 없는 경우 빈 문자열 배열 반환)
```
> morphs(m)
[[1]]
[1] "문장"   "을"     "입력"   "하"     "ㅂ니다" "."

[[2]]
[1] "여럿"   "은"     "이"     "렿거"   "이"     "넣"     "습니다" "."

> nouns(m)
[[1]]
[1] "문장" "입력"

[[2]]
[1] "여럿"

> verbs(m)
[[1]]
[1] ""

[[2]]
[1] "넣"
```

---

by [baikal.ai](https://baikal.ai)

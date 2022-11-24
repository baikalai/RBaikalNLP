# RBaikalNLP

* R package for baikalNLP
* About baikalNLP: https://license.baikal.ai/

## Install

library(devtools)  
devtools::install_github("baikalai/RBaikalNLP")  

➡️ [INSTALL](https://github.com/baikalai/RBaikalNLP/blob/main/INSTALL.md) 파일 내용 참고 

## Load

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

## Example

```
> library(RProtoBuf)
> library(baikalnlp)
> m <- tagger("문장을 입력합니다.\n여러 문장은 이렿게 넣습니다.")
> print_as_json(m)
...

> postag(m)
$pos
     [,1]     [,2]
[1,] "문장"   "NNG"
[2,] "을"     "JKO"
[3,] "입력"   "NNG"
[4,] "하"     "XSV"
[5,] "ㅂ니다" "EF"
[6,] "."      "SF"

$pos
      [,1]     [,2]
 [1,] "여러"   "MMN"
 [2,] "문장"   "NNG"
 [3,] "은"     "JX"
 [4,] "이"     "VCP"
 [5,] "렿거"   "EC"
 [6,] "이"     "JKS"
 [7,] "넣"     "VV"
 [8,] "습니다" "EF"
 [9,] "."      "SF"

> morphs(m)
$morph
[1] "문장"   "을"     "입력"   "하"     "ㅂ니다" "."

$morph
[1] "여러"   "문장"   "은"     "이"     "렿거"   "이"     "넣"     "습니다"
[9] "."

> nouns(m)
$nouns
[1] "문장" "입력"

$nouns
[1] "문장"

> verbs(m)
$verbs
[1] ""

$verbs
[1] "넣"
```

---

by https://baikal.ai/

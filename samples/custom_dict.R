library(curl)
library(grpc)
library(RProtoBuf)
library(baikalnlp)

test <- "localhost"
example <- "본보기"

t <- tagger(, server = test)
np <- c("청하", "트와이스", "티키타카", "TIKITAKA", "오마이걸")
cp <- c("자유여행", "방역당국", "코로나19", "주술부", "완전주의")
caret <- c("주어^역할", "주어^술어^구조", "하급^공무원")
vv <- c("카톡하다", "인스타하다")
va <- c("혜자스럽다", "창렬하다")
make_custom_dict(t, "예시", np, cp, caret, vv, va)
get_dict(t, "예시")
print_dict_all(t)

t <- tagger(, server = test, domain = example)

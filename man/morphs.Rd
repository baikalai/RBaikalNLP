% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/baikalnlp.R
\name{morphs}
\alias{morphs}
\title{Return array of Morphemes}
\usage{
morphs(tagged = NULL, text = "")
}
\arguments{
\item{tagged}{baikalNLP tagger result}
}
\value{
returns array of list for morphemes
}
\description{
형태소 분석 결과의 음절 배열 반환.
}
\examples{
> morphs(, "결과를 문자열로 바꾼다.")
[[1]]
[1] "결과"   "를"     "문자열" "로"     "바꾸"   "ㄴ다"   "."
}

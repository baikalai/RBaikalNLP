% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/baikalnlp.R
\name{nouns}
\alias{nouns}
\title{Return array of Nouns}
\usage{
nouns(tagged = NULL, text = "")
}
\arguments{
\item{tagged}{baikalNLP tagger result}
}
\value{
returns array of list for nouns
}
\description{
형태소 분석 결과의 명사 배열 반환.
}
\examples{
> nouns(, "결과를 문자열로 바꾼다.")
[[1]]
[1] "결과"   "문자열"
}

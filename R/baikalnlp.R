# package baikalnlp: baikalNLP grpc client

library(grpc)
library(RProtoBuf)

tag_labels <- c("EC", "EF", "EP", "ETM", "ETN", "IC",
                "JC", "JKB", "JKC", "JKG", "JKO", "JKQ", "JKS", "JKV", "JX",
                "MAG", "MAJ", "MMA", "MMD", "MMN",
                "NA", "NF", "NNB", "NNG", "NNP", "NP", "NR", "NV",
                "SE", "SF", "SH", "SL", "SN", "SO", "SP", "SS", "SW",
                "VA", "VCN", "VCP", "VV", "VX",
                "XPN", "XR", "XSA", "XSN", "XSV", "_SP_", "PAD")

#' Call baikalNLP server to read postag result message for the sentences
#'
#' baikalNLP grpc 서버를 호출하여 입력 문장(들)의 분석 결과를 가져 온다.
#'
#' @param str - subject sentences splitted by newline(\n)
#' @param host - baikalNLP grpc server address
#' @return returns response message of baikalNLP.AnalyzeSyntax
#' @importFrom grpc read_services grpc_client
#' @importFrom RProtoBuf P
#' @export
tagger <- function(str, host = "nlp.baikal.ai:5656") {
  spec <- system.file("protos/language_service.proto", package = "baikalnlp")
  impl <- read_services(spec)
  client <- grpc_client(impl, host)
  document <- P("baikal.language.Document", file = spec)
  doc <- new(document)
  doc$content <- str
  doc$language <- "ko_KR"
  example <- client$AnalyzeSyntax$build(
    document = doc, encoding_type = 1, auto_split_sentence = 0)
  client$AnalyzeSyntax$call(example)
}

#' Return JSON string for response message
#'
#' 결과를 JSON 문자열로 변환.
#'
#' @param message baikalNLP response message
#' @return returns JSON string
#' @export
as_json_string <- function(message) {
  toJSON(message)
}

#' Print JSON string for response message
#'
#' 결과를 JSON 문자열로 출력.
#'
#' @param message baikalNLP response message
#' @return prints JSON string
#' @export
print_as_json <- function(message) {
  cat(as_json_string(message))
}

.tagging <- function(m) {
  tags <- c()
  ms <- m$sentences
  for (s in ms) {
    tx <- as.list(s)
    sen <- c()
    tokens <- as.list(tx$tokens)
    for (t in tokens) {
      tk <- as.list(t)
      for (m in tk$morphemes) {
        mol <- as.list(m)
        ts <- as.list(mol$text)
        sen <- c(sen, c(ts$content, tag_labels[mol$tag]))
      }
    }
    t <- list()
    t$tag <- sen
    tags <- c(tags, t)
  }
  tags
}

#' Return array of (word, postag) pairs
#'
#' 결과에서 (음절, 형태소 태그)의 배열을 반환.
#'
#' @param message baikalNLP response message
#' @return returns array of list for (morpheme, postag)
#' @export
postag <- function(m) {
  tags <- .tagging(m)
  pos <- c()
  for (t in tags) {
    p <- list()
    p$pos <- matrix(t, ncol = 2, byrow = TRUE)
    pos <- c(pos, p)
  }
  pos
}

#' Return array of Morphemes
#'
#' 형태소 분석 결과의 음절 배열 반환.
#'
#' @param message baikalNLP response message
#' @return returns array of list for morphemes
#' @export
morphs <- function(m) {
  tags <- .tagging(m)
  mo <- c()
  for (t in tags) {
    m <- list()
    m$morph <- t[seq(1, length(t), by = 2)]
    mo <- c(mo, m)
  }
  mo
}

#' Return array of Nouns
#'
#' 형태소 분석 결과의 명사 배열 반환.
#'
#' @param message baikalNLP response message
#' @return returns array of list for nouns
#' @export
nouns <- function(m) {
  nouns1 <- function(t) {
    ns <- c()
    num <- length(t) / 2
    for (i in 1:num) {
      if (!is.na(match(t[i * 2], c("NNP", "NNG", "NP", "NNB")))) {
        ns <- c(ns, t[i * 2 - 1])
      }
    }
    if (length(ns) == 0) {
      ns <- c("")
    }
    ns
  }

  tags <- .tagging(m)
  nns <- c()
  for (t in tags) {
    n <- list()
    n$nouns <- nouns1(t)
    nns <- c(nns, n)
  }
  nns
}

#' Return array of Verbs
#'
#' 형태소 분석 결과의 동사 배열 반환.
#'
#' @param message baikalNLP response message
#' @return returns array of list for verbs
#' @export
verbs <- function(m) {
  verbs1 <- function(t) {
    vs <- c()
    num <- length(t) / 2
    for (i in 1:num) {
      if (t[i * 2] == "VV") {
        vs <- c(vs, t[i * 2 - 1])
      }
    }
    if (length(vs) == 0) {
      vs <- c("")
    }
    vs
  }

  tags <- .tagging(m)
  vbs <- c()
  for (t in tags) {
    v <- list()
    v$verbs <- verbs1(t)
    vbs <- c(vbs, v)
  }
  vbs
}

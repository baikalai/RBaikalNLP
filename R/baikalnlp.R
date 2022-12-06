# package baikalnlp: baikalNLP grpc client

library(curl)
library(grpc)
library(RProtoBuf)

tag_labels <- c("EC", "EF", "EP", "ETM", "ETN", "IC",
                "JC", "JKB", "JKC", "JKG", "JKO", "JKQ", "JKS", "JKV", "JX",
                "MAG", "MAJ", "MMA", "MMD", "MMN",
                "NA", "NF", "NNB", "NNG", "NNP", "NP", "NR", "NV",
                "SE", "SF", "SH", "SL", "SN", "SO", "SP", "SS", "SW",
                "VA", "VCN", "VCP", "VV", "VX",
                "XPN", "XR", "XSA", "XSN", "XSV", "_SP_", "PAD")

.analyze_text <- function(text, proto, host) {
  client <- grpc_client(read_services(proto), host)
  document <- P("baikal.language.Document", file = proto)
  doc <- new(document)
  doc$content <- text
  doc$language <- "ko_KR"
  example <- client$AnalyzeSyntax$build(
    document = doc, encoding_type = 1, auto_split_sentence = 0)
  client$AnalyzeSyntax$call(example)
}

#' Call baikalNLP server to read postag result message for the sentences
#'
#' baikalNLP grpc 서버를 호출하여 입력 문장(들)의 분석 결과를 가져 온다.
#'
#' @param text string - subject sentences splitted by newlines(\\n)
#' @param server string - baikalNLP grpc server address
#' @param port number - baikalNLP grpc server port
#' @param local bool - use local protobuf files, if TRUE
#' @return returns tagged object
#' @examples
#' tagged <- tagger("결과를 문자열로 바꾼다.")
#' @importFrom grpc read_services grpc_client
#' @importFrom RProtoBuf P
#' @importFrom curl nslookup
#' @export
tagger <- function(
    text = "", server = "nlp.baikal.ai", port = 5656, local = FALSE) {
  host <- paste(nslookup(server), ":", as.character(port), sep = "")
  if (local) {
    lang_proto <- "protos/language_service.proto"
    dict_proto <- "protos/custom_dict.proto"
  } else {
    lang_proto <- system.file("protos/language_service.proto",
      package = "baikalnlp")
    dict_proto <- system.file("protos/custom_dict.proto",
      package = "baikalnlp")
  }
  response <- NULL
  dict <- NULL
  if (text != "") {
    response <- .analyze_text(text, lang_proto, host)
  }
  tagged <- list(text = text,
    result = response,
    dict = dict,
    host = host,
    lang_proto = lang_proto,
    dict_proto = dict_proto)
  class(tagged) <- "tagged"
  tagged
}

#' Return JSON string for response message
#'
#' 결과를 JSON 문자열로 변환.
#'
#' @param tagged baikalNLP tagger result
#' @return returns JSON string
#' @examples
#' t <- tagger("결과를 문자열로 바꾼다.")
#' json <- as_json_string(t)
#' @export
as_json_string <- function(tagged) {
  toJSON(tagged$result)
}

#' Print JSON string for response message
#'
#' 결과를 JSON 문자열로 출력.
#'
#' @param tagged baikalNLP tagger result
#' @return prints JSON string
#' @examples
#' t <- tagger("결과를 문자열로 바꾼다.")
#' print_as_json(t)
#' @export
print_as_json <- function(tagged) {
  cat(as_json_string(tagged$result))
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

#' Return array of (morpheme, postag) pairs
#'
#' 결과에서 (음절, 형태소 태그)의 배열을 반환.
#'
#' @param tagged baikalNLP tagger result
#' @param matrix if TRUE, result output to matrix not list (default = FALSE)
#' @return returns array of lists for (morpheme, tag)
#' @examples
#' > t <- tagger("결과를 문자열로 바꾼다.")
#' > postag(t, matrix = TRUE)
#' [[1]]
#'      [,1]     [,2]
#' [1,] "결과"   "NNG"
#' [2,] "를"     "JKO"
#' [3,] "문자열" "NNG"
#' [4,] "로"     "JKB"
#' [5,] "바꾸"   "VV"
#' [6,] "ㄴ다"   "EF"
#' [7,] "."      "SF"
#' @export
postag <- function(tagged, matrix = FALSE) {
  tags <- .tagging(tagged$result)
  pos_list <- c(list(), seq_along(tags))
  pos_mat <- pos_list
  tag_i <- 0
  for (t in tags) {
    tag_i <- tag_i + 1
    pos_mat[[tag_i]] <- matrix(t, ncol = 2, byrow = TRUE)
    tag_a <- 1 : (length(t) / 2)
    tag_list <- c(list(), tag_a)
    a_i <- 0
    for (i in tag_a) {
      a_i <- a_i + 1
      tag_list[[a_i]] <- list(morpheme = t[i * 2 - 1], tag = t[i * 2])
    }
    pos_list[[tag_i]] <- tag_list
  }
  if (matrix) {
    pos_mat
  } else {
    pos_list
  }
}

#' Return array of Morphemes
#'
#' 형태소 분석 결과의 음절 배열 반환.
#'
#' @param tagged baikalNLP tagger result
#' @return returns array of list for morphemes
#' @examples
#' > t <- tagger("결과를 문자열로 바꾼다.")
#' > morphs(t)
#' [[1]]
#' [1] "결과"   "를"     "문자열" "로"     "바꾸"   "ㄴ다"   "."
#' @export
morphs <- function(tagged) {
  tags <- .tagging(tagged$result)
  morp <- c(list(), seq_along(tags))
  tag_i <- 0
  for (t in tags) {
    tag_i <- tag_i + 1
    morp[[tag_i]] <- t[seq(1, length(t), by = 2)]
  }
  morp
}

.findtag <- function(t, c) {
  out <- c()
  num <- length(t) / 2
  for (i in 1:num) {
    if (!is.na(match(t[i * 2], c))) {
      out <- c(out, t[i * 2 - 1])
    }
  }
  if (length(out) == 0) {
    out <- c("")
  }
  out
}

#' Return array of Nouns
#'
#' 형태소 분석 결과의 명사 배열 반환.
#'
#' @param tagged baikalNLP tagger result
#' @return returns array of list for nouns
#' @examples
#' > t <- tagger("결과를 문자열로 바꾼다.")
#' > nouns(t)
#' [[1]]
#' [1] "결과"   "문자열"
#' @export
nouns <- function(tagged) {
  tags <- .tagging(tagged$result)
  nns <- c(list(), seq_along(tags))
  tag_i <- 0
  for (t in tags) {
    tag_i <- tag_i + 1
    nns[[tag_i]] <- .findtag(t, c("NNP", "NNG", "NP", "NNB"))
  }
  nns
}

#' Return array of Verbs
#'
#' 형태소 분석 결과의 동사 배열 반환.
#'
#' @param tagged baikalNLP tagger result
#' @return returns array of list for verbs
#' @examples
#' > t <- tagger("결과를 문자열로 바꾼다.")
#' > verbs(t)
#' [[1]]
#' [1] "바꾸"
#' @export
verbs <- function(tagged) {
  tags <- .tagging(tagged$result)
  vbs <- c(list(), seq_along(tags))
  tag_i <- 0
  for (t in tags) {
    tag_i <- tag_i + 1
    vbs[[tag_i]] <- .findtag(t, c("VV"))
  }
  vbs
}

library(RProtoBuf)
library(baikalnlp)

host <- "localhost:5656"
text <- "토크나이저를 추가했습니다."
proto <- system.file("protos/language_service.proto", package = "baikalnlp")
client <- grpc_client(read_services(proto), host)
doc <- new(P("baikal.language.Document", file = proto))
doc$content <- text
doc$language <- "ko_KR"
example <- client$Tokenize$build(document = doc, encoding_type = 1)
tok <- client$Tokenize$call(example)

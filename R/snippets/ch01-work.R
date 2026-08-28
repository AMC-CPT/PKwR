# R 은 표현식을 받아 개체를 만들고, 개체는 작업공간에 이름으로 남는다.
CL <- 5.2; V <- 40                            # 개체 둘. <- 가 R 의 대입 연산자다
ls()                                          # 작업공간에 무엇이 있는가
c(exists = exists("CL"), class = class(CL))

# 함수가 어떤 인자를 어떤 기본값으로 받는지는 args 로, 자세한 설명은 ? 로 본다
args(round)                                   # ?round, example(round) 도 함께
args(seq.default)

# 이름 짓기: 대소문자를 구분하고, 이미 쓰이는 이름은 피한다
c(t = is.function(t), c = is.function(c), df = is.function(df))
T <- FALSE                                    # T·F 는 덮어쓸 수 있는 변수다
c(TRUE. = TRUE, T. = T)                       # 그래서 언제나 TRUE/FALSE 를 쓴다
rm(T); ls()

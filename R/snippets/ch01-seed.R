# 난수는 씨앗에서 결정적으로 자란다. 씨앗을 고정하면 언제나 같은 수가 나온다.
set.seed(1); a <- rnorm(3)
set.seed(1); b <- rnorm(3)
identical(a, b)
round(rbind(a = a, b = b), 4)

set.seed(2); round(rnorm(3), 4)               # 씨앗이 다르면 다른 수열
round(rnorm(3), 4)                            # 씨앗을 다시 심지 않으면 이어서 나온다

# 이 책의 규칙: 난수를 쓰는 스니펫은 자체 set.seed 를 갖고, 그 코드가
# 결과와 같은 저장소에 남는다. 결과 파일만 남기면 절차는 재현되지 않는다.

# Dedrick 도표: 종마다 다른 곡선을 좌표변환으로 하나에 겹친다.
# CL = a W^0.75, V = c W^1 이면 k = CL/V 가 W^-0.25 에 비례하므로,
# 시간을 t/W^0.25 로, 농도를 C/(D/W) 로 바꾸면 모든 종이 한 곡선에 놓인다.
Wd  <- c(mouse = 0.02, rat = 0.25, monkey = 5, dog = 12)
aCL <- 0.9; bCL <- 0.75; cV <- 5                  # CL (L/hr), V (L)
kW  <- function(W) aCL*W^bCL/(cV*W)               # 제거속도상수
Cd  <- function(t, W) 10*W/(cV*W)*exp(-kW(W)*t)   # 10 mg/kg 정맥 일시주입
round(rbind(W = Wd, k = kW(Wd), t.half = log(2)/kW(Wd)), 4)

par(mfrow = c(1, 2), mar = c(4.2, 5.2, 2.4, 0.8))
tg <- seq(0.01, 40, 0.05); ii <- seq(1, length(tg), 40)
matplot(tg, sapply(Wd, function(W) Cd(tg, W)), type = "l", log = "y", lty = 1:4,
        col = 1, las = 1, bty = "l", ylim = c(0.005, 3), xlab = "t (hr)",
        ylab = "C (mg/L)", main = "(a) 원래 좌표")
legend("topright", bty = "n", cex = 0.75, lty = 1:4, legend = names(Wd))
matplot(sapply(Wd, function(W) tg[ii]/W^(1 - bCL)),
        sapply(Wd, function(W) Cd(tg[ii], W)/10), type = "p", pch = 1:4,
        cex = 0.7, col = 1, log = "y", las = 1, bty = "l", xlim = c(0, 12),
        ylim = c(0.005, 0.3), xlab = expression(t/W^0.25),
        ylab = "C/(D/W)", main = "(b) Dedrick 좌표")
legend("topright", bty = "n", cex = 0.75, pch = 1:4, legend = names(Wd))

# 겹쳐진 곡선을 사람(70 kg)의 좌표로 되돌리면 사람의 예측이 된다
Wh <- 70; th <- c(1, 6, 12, 24)
teq <- th/Wh^(1 - bCL)                            # 종 무관 시간
round(rbind(t.human = th, t.equivalent = teq,
            from.mouse = Cd(teq*Wd[["mouse"]]^(1 - bCL), Wd[["mouse"]]),
            exact.human = Cd(th, Wh)), 4)

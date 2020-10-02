install.packages("renv")
library(renv)
renv::init()
install.packages("tidyverse")
library(tidyverse)
renv::snapshot()

# 種子のばらつき
data <- read.csv("data.csv")$x

# 統計量を確認
length(data)
summary(data)
table(data)
hist(data, breaks = seq(-0.5, 9.5, 1))
var(data)
mean(data)

# 種子データを表すにはポアソン分布が便利…ということにする
# 平均 mean(data) である確率分布を作る

y <- 0:9

# 統計モデル
# データは mean(data) = 3.56 に従う
prob <- dpois(y, lambda = mean(data))
df = data.frame(value = y, prob = prob, data = data)

# ヒストグラムを書く
ggplot(df) +
   geom_histogram(mapping = aes(x= data)) 

ggplot(df) +
    #50個種子があるとき、0個の種である確率は 50 * dpois(0, lambda = mean(data))
    geom_line(mapping = aes(x= value, y = 50 * prob)) +
    geom_point(mapping = aes(x= value, y = 50 * prob)) +
    geom_histogram(mapping = aes(x= data))

# 最尤推定する
logL <- function(m) sum(dpois(data, m, log = TRUE))
lambda <- seq(2, 5, 0.1)

#3.5付近で大数尤度が最大になっていることを確認する
plot(lambda, sapply(lambda, logL), type = "l")

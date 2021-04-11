data.csv <- read.csv("data.csv") 
mean <- mean(data.csv$x)
z.critical.value <- qnorm((100-95)/2/100,lower.tail = FALSE)

confidence.interval.lowerbound <- mean - (z.critical.value * sd(data.csv$x)/sqrt(length(data.csv$x)))

confidence.interval.upperbound <- mean + (z.critical.value * sd(data.csv$x)/sqrt(length(data.csv$x)))

t.test(data.csv$x)


confidence.interval.lowerbound <- mean - (z.critical.value * 2/sqrt(length(data.csv$x)))

confidence.interval.upperbound <- mean + (z.critical.value * 2/sqrt(length(data.csv$x)))
?qqnorm
qqnorm(data.csv$x)

data2.csv <- read.csv("data2.csv")
qqnorm(data2.csv$x)
hist(data2.csv$x)

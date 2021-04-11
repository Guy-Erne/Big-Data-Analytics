data<-read.csv("C:/Users/Tommylee/Desktop/phone/data.csv")
mean(data$x)
sd(data$x)
View(data)
t.test(data$x)

# to calculate the confidence interval manually ??
mn=mean(data$x)
SD= sd(data$x)
c_v<-1.96
n<-250
confidence_interval_lower= mn-(c_v*(SD/(sqrt(n))))
confidence_interval_higher= mn+(c_v*(SD/(sqrt(n))))

confidence_interval_lower
confidence_interval_higher

t.test(data$x)

#when sd equals 2
confidence_interval_lower2= mn-(c_v*(2/(sqrt(n))))
confidence_interval_higher2= mn+(c_v*(2/(sqrt(n))))

confidence_interval_lower2
confidence_interval_higher2



hist(data$x)
plot(density(data$x))
qqnorm(data$x)
qqline(data$x)


data2<-read.csv("C:/Users/Tommylee/Desktop/phone/data2.csv")
View(data2)
plot(density(data2$x))
hist(data2$x)
qqnorm(data2$x)
qqline(data2$x)
boxplot(data$x)

xl<-data
xl<-mice(xl)
View(xl)
boxplot(xl$x)
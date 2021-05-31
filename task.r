library(readr)
library(ggplot2)

mode_func <- function(x){
  uninq_val <- unique(x)
  uninq_val[which.max(tabulate(match(x, uninq_val)))]
}

world_happiness_report_raw <- read_csv("Desktop/kaggle/world-happiness-report.csv")
world_happiness_report <- na.omit(world_happiness_report_raw)
world_happiness_report$`Country name` <- NULL

corrplot(cor(world_happiness_report, use="pairwise.complete.obs"))

my_data1 <- data.frame(GDP = world_happiness_report$`Log GDP per capita`, HLE=world_happiness_report$`Healthy life expectancy at birth`)
my_data2 <- data.frame(SS= world_happiness_report$`Social support`, LL=world_happiness_report$`Life Ladder`)

ggplot(my_data1, aes(x=GDP, y=HLE))+geom_point()

summary(my_data1)
mode_func(my_data1$GDP)
mode_func(my_data1$HLE)
sd(my_data1$GDP)
sd(my_data1$HLE)

ggplot(my_data3, aes(x=LL, y=SS))+geom_point()

summary(my_data2)
mode_func(my_data2$LL)
mode_func(my_data2$SS)
sd(my_data2$LL)
sd(my_data2$SS)

shapiro.test(my_data1$HLE)
shapiro.test(my_data1$GDP)
cor.test(my_data1$GDP, my_data1$HLE, method = "spearman")
ggplot(my_data1, aes(x=GDP, y=HLE))+geom_point()+geom_smooth(method="lm")

shapiro.test(my_data2$LL)
shapiro.test(my_data2$SS)
cor.test(my_data2$LL, my_data2$SS, method = "spearman")
ggplot(my_data2, aes(x=LL, y=SS))+geom_point()+geom_smooth(method="lm")
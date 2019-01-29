fw07p06 = read.table("data_lab1.txt", header = FALSE, sep = "", dec = ".")
colnames(fw07p06)=c("CITY","STATE","LAT","RANGE")

ggplot(data = fw07p06,aes(x = LAT,y = RANGE))+
  geom_point()+
  theme_classic()

model1 <- lm(RANGE ~ LAT, data=fw07p06)

summary(model1)

model1$coefficients

confint(model1)

confint(model1,level = 0.90)


ggplot(data = fw07p06,aes(x = LAT,y = RANGE))+
  geom_point()+
  geom_smooth(method=lm)+
  theme_classic()
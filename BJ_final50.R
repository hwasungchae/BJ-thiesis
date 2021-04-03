install.packages("readxl")
library("readxl")
bj <- read_excel("R_Corr_Reg.xlsx", sheet =7, col_names = T)
head(bj)
plot(bj$L1length,bj$CKA)
plot(bj$`L1toA-pog`, bj$CKA)
#correlation#
cor.test (bj$L1length, bj$CKA)
cor.test (bj$`L1toA-pog`,bj$CKA)
cor.test (bj$`L1toA-pog`,bj$`CKH angle`)
cor.test (bj$L1length,bj$`CKH angle`)

# Regression #
L <- lm(bj$CKA~bj$L1length,data=bj)
A <- lm(bj$CKA~bj$`L1toA-pog`,                 data=bj)
lapply(list(L, A), coef)  
summary(L)
summary(A)
#interactive garph#
install.packages("plotly")
library(plotly)
library(ggplot2)
p <- ggplot(data = bj, aes(x = CKA , y = L1length)) +
  geom_point(color='gray75') + 
  stat_smooth(method = 'lm', se=F, color='black') + 
  geom_text(x=85, y=450, label="y=0.9134x-1.3954") 
p
ggplotly(p)  #Working Good#
#Treand line#
ggplot(data = bj , aes(x=CKA, y=L1length )) + 
  geom_point(color='gray75') + 
  geom_smooth(method = 'lm', se=F, color='black') + 
  geom_text(x=85, y=450, label="y=0.9134x-1.3954") 
  






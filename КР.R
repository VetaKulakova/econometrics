# Контрольная работа
# Выполнила Кулакова Елизавета, БИ-3-20-03

# Задача_1
# импорт данных
df1<-read.csv('/Users/elizavetakulakova/Desktop/kr1.csv', sep = ';')

# линейная модель
mod1<-lm(Y~K, data = df1)
mod2<-lm(Y~L, data = df1)

cov(df1) # ковариация
cor(df1) # корреляция

# инфо
summary(mod1)
summary(mod2)
summary(mod1$coefficients)
summary(mod2$coefficients)
# графики для оценки качества
par(mfrow = c(2, 2), pty = "s")  
plot(mod1) #Y~K
plot(mod2) #Y~L

# оценка аномалий
influencePlot(mod1, id.method='identify', main='Диаграмма влияния для Y~K')
influencePlot(mod2, id.method='identify', main='Диаграмма влияния для Y~L')

# проверка распределения остатков
# 'ящик с усами'
boxplot(mod1$residuals)
boxplot(mod2$residuals)
plot(mod1$residuals)
plot(mod2$residuals)
# qq plot
qqnorm(mod1$residuals)
qqnorm(mod2$residuals)


# Задача_2
# импорт данных
df2<-read.csv('/Users/elizavetakulakova/Desktop/kr2.csv', sep = ';')

# линейная модель
mod1<-lm(Y~I(1/X), data = df2)
mod2<-lm(I(log(Y))~I(log(X)), data = df2)

# инфо
summary(mod1)
summary(mod2)


# Задача 3
# импорт данных
df3<-read.csv('/Users/elizavetakulakova/Desktop/kr1.csv', sep = ';')

mod3 <- lm(Y~K+L, data = df3) # множественная линейная модель
library(gvlma)
mod3_gv <- gvlma(mod3) 
summary(mod3_gv)

# с учетом нелинейности
mod31 <- lm(I(log(Y/L))~I(log(K/L)), data = df3) # множественная линейная модель
library(gvlma)
mod31_gv <- gvlma(mod31) 
summary(mod31_gv)

# Кр. Акаике
AIC(mod3)
AIC(mod31)


# Задача 4
cor(df1) # корреляционная матрица
corrgram(df1, order=TRUE, lower.panel = panel.shade, upper.panel = panel.pie) # диаграмма корреляционной матрицы
pcor(df1) # частные коэф. кор.


# Задача 5
# импорт данных
df5<-read.csv('/Users/elizavetakulakova/Desktop/kr5.csv', sep = ';')
x<-df[,1]
y<-df[,2]
start.param <- list(a=1,b=0.2)
nls_mod5 <- nls(y~b*(x**a), data = df5, start = start.param, trace = TRUE)
summary(nls_mod5)
df5


# Задача 6
mod6<-lm(Y~L+K, data = df1)
# Кр. Гольфельда
gqtest(mod6)
# Кр. Бройша-Пагана
bptest(mod6) 
ncvTest(mod6) 

# Исследовать наблюдения на наличие аномальных, влиятельных и наблюдений сильной напряженности 
influencePlot(mod6, id.method='identify', main='Диаграмма влияния для Y~K+L')

#Выполнить общую проверку качества модели с помощью функции gvmla()
library(gvlma)
mod6_gv <- gvlma(mod6) 
summary(mod6_gv)
# диаграммы проверки кач-ва
par(mfrow=c(2,2))
plot(mod6)















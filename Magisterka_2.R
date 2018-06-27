library(readxl)
Zanieczyszczenia_2016 <- read_excel("j:/Desktop/Dane/Zanieczyszczenia_2016.xlsx")
Zanieczyszczenia_2015 <- read_excel("j:/Desktop/Dane/Zanieczyszczenia_2015.xlsx")
Zanieczyszczenia_2014 <- read_excel("j:/Desktop/Dane/Zanieczyszczenia_2014.xlsx")
Zanieczyszczenia_2013 <- read_excel("j:/Desktop/Dane/Zanieczyszczenia_2013.xlsx")
Zanieczyszczenia_2012 <- read_excel("j:/Desktop/Dane/Zanieczyszczenia_2012.xlsx")
Zanieczyszczenia_2011 <- read_excel("j:/Desktop/Dane/Zanieczyszczenia_2011.xlsx")
Zanieczyszczenia_2010 <- read_excel("j:/Desktop/Dane/Zanieczyszczenia_2010.xlsx")
Zanieczyszczenia_2009 <- read_excel("j:/Desktop/Dane/Zanieczyszczenia_2009.xlsx")
Zanieczyszczenia_2008 <- read_excel("j:/Desktop/Dane/Zanieczyszczenia_2008.xlsx")
Zanieczyszczenia_2007 <- read_excel("j:/Desktop/Dane/Zanieczyszczenia_2007.xlsx")

t2016 <- Zanieczyszczenia_2016[c(2,8,14,19,22,28,35,44,47,52,56,62,72,75,79,86),-c(1,3,4,5,6,7,8)]
t2015 <- Zanieczyszczenia_2015[-c(1,3,4,5,6,7,8)]
t2014 <- Zanieczyszczenia_2014[-c(1,3,4,5,6,7,8)]
t2013 <- Zanieczyszczenia_2013[-c(1,3,4,5,6,7,8)]
t2012 <- Zanieczyszczenia_2012[-c(1,3,4,5,6,7,8)]
t2011 <- Zanieczyszczenia_2011[-c(1,3,4,5,6,7,8)]
t2010 <- Zanieczyszczenia_2010[-c(1,3,4,5,6,7,8)]
t2009 <- Zanieczyszczenia_2009[-c(1,3,4,5,6)]
t2008 <- Zanieczyszczenia_2008[-c(1,3,4,5,6)]
t2007 <- Zanieczyszczenia_2007[-c(1,3,4,5,6)]

rownames(t2016) <- c('Dolno零kie','Kujawsko-pomorskie','Lubelskie','Lubuskie'
                             ,'�dzkie','Ma這polskie','Mazowieckie','Opolskie','Podkarpackie',
                             'Podlaskie','Pomorskie','零kie','i皻okrzyskie',
                             'Warmi雟ko-mazurskie','Wielkopolskie','Zachodniopomorskie')
rownames(t2015) <- c('Dolno零kie','Kujawsko-pomorskie','Lubelskie','Lubuskie'
                             ,'�dzkie','Ma這polskie','Mazowieckie','Opolskie','Podkarpackie',
                             'Podlaskie','Pomorskie','零kie','i皻okrzyskie',
                             'Warmi雟ko-mazurskie','Wielkopolskie','Zachodniopomorskie')
rownames(t2014) <- c('Dolno零kie','Kujawsko-pomorskie','Lubelskie','Lubuskie'
                             ,'�dzkie','Ma這polskie','Mazowieckie','Opolskie','Podkarpackie',
                             'Podlaskie','Pomorskie','零kie','i皻okrzyskie',
                             'Warmi雟ko-mazurskie','Wielkopolskie','Zachodniopomorskie')
rownames(t2013) <- c('Dolno零kie','Kujawsko-pomorskie','Lubelskie','Lubuskie'
                             ,'�dzkie','Ma這polskie','Mazowieckie','Opolskie','Podkarpackie',
                             'Podlaskie','Pomorskie','零kie','i皻okrzyskie',
                             'Warmi雟ko-mazurskie','Wielkopolskie','Zachodniopomorskie')
rownames(t2012) <- c('Dolno零kie','Kujawsko-pomorskie','Lubelskie','Lubuskie'
                             ,'�dzkie','Ma這polskie','Mazowieckie','Opolskie','Podkarpackie',
                             'Podlaskie','Pomorskie','零kie','i皻okrzyskie',
                             'Warmi雟ko-mazurskie','Wielkopolskie','Zachodniopomorskie')
rownames(t2011) <- c('Dolno零kie','Kujawsko-pomorskie','Lubelskie','Lubuskie'
                             ,'�dzkie','Ma這polskie','Mazowieckie','Opolskie','Podkarpackie',
                             'Podlaskie','Pomorskie','零kie','i皻okrzyskie',
                             'Warmi雟ko-mazurskie','Wielkopolskie','Zachodniopomorskie')
rownames(t2010) <- c('Dolno零kie','Kujawsko-pomorskie','Lubelskie','Lubuskie'
                             ,'�dzkie','Ma這polskie','Mazowieckie','Opolskie','Podkarpackie',
                             'Podlaskie','Pomorskie','零kie','i皻okrzyskie',
                             'Warmi雟ko-mazurskie','Wielkopolskie','Zachodniopomorskie')
rownames(t2009) <- c('Dolno零kie','Kujawsko-pomorskie','Lubelskie','Lubuskie'
                             ,'�dzkie','Ma這polskie','Mazowieckie','Opolskie','Podkarpackie',
                             'Podlaskie','Pomorskie','零kie','i皻okrzyskie',
                             'Warmi雟ko-mazurskie','Wielkopolskie','Zachodniopomorskie')
rownames(t2008) <- c('Dolno零kie','Kujawsko-pomorskie','Lubelskie','Lubuskie'
                             ,'�dzkie','Ma這polskie','Mazowieckie','Opolskie','Podkarpackie',
                             'Podlaskie','Pomorskie','零kie','i皻okrzyskie',
                             'Warmi雟ko-mazurskie','Wielkopolskie','Zachodniopomorskie')
rownames(t2007) <- c('Dolno零kie','Kujawsko-pomorskie','Lubelskie','Lubuskie'
                     ,'�dzkie','Ma這polskie','Mazowieckie','Opolskie','Podkarpackie',
                     'Podlaskie','Pomorskie','零kie','i皻okrzyskie',
                     'Warmi雟ko-mazurskie','Wielkopolskie','Zachodniopomorskie')

model <- prcomp(t2007)
summary(model)
cov2012 <- cov(t2007)
eig2012 <- eigen(cov2012)
sum(round(eig2012$vectors[,1],6))
sum(round(eig2012$vectors[,2],6))
sum(round(eig2012$vectors[,3],6))
sum(round(eig2012$vectors[,4],6))
sum(round(eig2012$vectors[,5],6))
sum(round(eig2012$vectors[,6],6))

r2007.1 = max(t2007[,1]) - min(t2007[,1])
r2007.2 = max(t2007[,2]) - min(t2007[,2])
r2007.3 = max(t2007[,3]) - min(t2007[,3])
r2007.4 = max(t2007[,4]) - min(t2007[,4])
r2007.5 = max(t2007[,5]) - min(t2007[,5])
r2007.6 = max(t2007[,6]) - min(t2007[,6])

u2007.1 <- (max(t2007[,1]) - t2007[,1])/r2007.1 
u2007.2 <- (max(t2007[,2]) - t2007[,2])/r2007.2 
u2007.3 <- (max(t2007[,3]) - t2007[,3])/r2007.3 
u2007.4 <- (t2007[,4] - min(t2007[,4]))/r2007.4 
u2007.5 <- (t2007[,5] - min(t2007[,5]))/r2007.5 
u2007.6 <- (t2007[,6] - min(t2007[,6]))/r2007.6 

r2008.1 = max(t2008[,1]) - min(t2008[,1])
r2008.2 = max(t2008[,2]) - min(t2008[,2])
r2008.3 = max(t2008[,3]) - min(t2008[,3])
r2008.4 = max(t2008[,4]) - min(t2008[,4])
r2008.5 = max(t2008[,5]) - min(t2008[,5])
r2008.6 = max(t2008[,6]) - min(t2008[,6])

u2008.1 <- (max(t2008[,1]) - t2008[,1])/r2008.1 
u2008.2 <- (max(t2008[,2]) - t2008[,2])/r2008.2 
u2008.3 <- (max(t2008[,3]) - t2008[,3])/r2008.3 
u2008.4 <- (t2008[,4] - min(t2008[,4]))/r2008.4 
u2008.5 <- (t2008[,5] - min(t2008[,5]))/r2008.5 
u2008.6 <- (t2008[,6] - min(t2008[,6]))/r2008.6 

r2009.1 = max(t2009[,1]) - min(t2009[,1])
r2009.2 = max(t2009[,2]) - min(t2009[,2])
r2009.3 = max(t2009[,3]) - min(t2009[,3])
r2009.4 = max(t2009[,4]) - min(t2009[,4])
r2009.5 = max(t2009[,5]) - min(t2009[,5])
r2009.6 = max(t2009[,6]) - min(t2009[,6])

u2009.1 <- (max(t2009[,1]) - t2009[,1])/r2009.1 
u2009.2 <- (max(t2009[,2]) - t2009[,2])/r2009.2 
u2009.3 <- (max(t2009[,3]) - t2009[,3])/r2009.3 
u2009.4 <- (t2009[,4] - min(t2009[,4]))/r2009.4 
u2009.5 <- (t2009[,5] - min(t2009[,5]))/r2009.5 
u2009.6 <- (t2009[,6] - min(t2009[,6]))/r2009.6 

r2010.1 = max(t2010[,1]) - min(t2010[,1])
r2010.2 = max(t2010[,2]) - min(t2010[,2])
r2010.3 = max(t2010[,3]) - min(t2010[,3])
r2010.4 = max(t2010[,4]) - min(t2010[,4])
r2010.5 = max(t2010[,5]) - min(t2010[,5])
r2010.6 = max(t2010[,6]) - min(t2010[,6])

u2010.1 <- (max(t2010[,1]) - t2010[,1])/r2010.1 
u2010.2 <- (max(t2010[,2]) - t2010[,2])/r2010.2 
u2010.3 <- (max(t2010[,3]) - t2010[,3])/r2010.3 
u2010.4 <- (t2010[,4] - min(t2010[,4]))/r2010.4 
u2010.5 <- (t2010[,5] - min(t2010[,5]))/r2010.5 
u2010.6 <- (t2010[,6] - min(t2010[,6]))/r2010.6 

r2011.1 = max(t2011[,1]) - min(t2011[,1])
r2011.2 = max(t2011[,2]) - min(t2011[,2])
r2011.3 = max(t2011[,3]) - min(t2011[,3])
r2011.4 = max(t2011[,4]) - min(t2011[,4])
r2011.5 = max(t2011[,5]) - min(t2011[,5])
r2011.6 = max(t2011[,6]) - min(t2011[,6])

u2011.1 <- (max(t2011[,1]) - t2011[,1])/r2011.1 
u2011.2 <- (max(t2011[,2]) - t2011[,2])/r2011.2 
u2011.3 <- (max(t2011[,3]) - t2011[,3])/r2011.3 
u2011.4 <- (t2011[,4] - min(t2011[,4]))/r2011.4 
u2011.5 <- (t2011[,5] - min(t2011[,5]))/r2011.5 
u2011.6 <- (t2011[,6] - min(t2011[6]))/r2011.1


r2012.1 = max(t2012[,1]) - min(t2012[,1])
r2012.2 = max(t2012[,2]) - min(t2012[,2])
r2012.3 = max(t2012[,3]) - min(t2012[,3])
r2012.4 = max(t2012[,4]) - min(t2012[,4])
r2012.5 = max(t2012[,5]) - min(t2012[,5])
r2012.6 = max(t2012[,6]) - min(t2012[,6])

u2012.1 <- (max(t2012[,1]) - t2012[,1])/r2012.1 
u2012.2 <- (max(t2012[,2]) - t2012[,2])/r2012.2 
u2012.3 <- (max(t2012[,3]) - t2012[,3])/r2012.3 
u2012.4 <- (t2012[,4] - min(t2012[,4]))/r2012.4 
u2012.5 <- (t2012[,5] - min(t2012[,5]))/r2012.5 
u2012.6 <- (t2012[,6] - min(t2012[,6]))/r2012.5

r2013.1 = max(t2013[,1]) - min(t2013[,1])
r2013.2 = max(t2013[,2]) - min(t2013[,2])
r2013.3 = max(t2013[,3]) - min(t2013[,3])
r2013.4 = max(t2013[,4]) - min(t2013[,4])
r2013.5 = max(t2013[,5]) - min(t2013[,5])
r2013.6 = max(t2013[,6]) - min(t2013[,6])

u2013.1 <- (max(t2013[,1])- t2013[,1])/r2013.1
u2013.2 <- (max(t2013[,2]) - t2013[,2])/r2013.2 
u2013.3 <- (max(t2013[,3]) - t2013[,3])/r2013.3
u2013.4 <- (t2013[,4] - min(t2013[,4]))/r2013.4
u2013.5 <- (t2013[,5] - min(t2013[,5]))/r2013.5 
u2013.6 <- (t2013[,6] - min(t2013[,6]))/r2013.6 

r2014.1 = max(t2014[,1]) - min(t2014[,1])
r2014.2 = max(t2014[,2]) - min(t2014[,2])
r2014.3 = max(t2014[,3]) - min(t2014[,3])
r2014.4 = max(t2014[,4]) - min(t2014[,4])
r2014.5 = max(t2014[,5]) - min(t2014[,5])
r2014.6 = max(t2014[,6]) - min(t2014[,6])

u2014.1 <- (max(t2014[,1]) - t2014[,1])/r2014.1 
u2014.2 <- (max(t2014[,2]) - t2014[,2])/r2014.2 
u2014.3 <- (max(t2014[,3]) - t2014[,3])/r2014.3 
u2014.4 <- (t2014[,4] - min(t2014[,4]))/r2014.4 
u2014.5 <- (t2014[,5] - min(t2014[,5]))/r2014.5 
u2014.6 <- (t2014[,6] - min(t2014[,6]))/r2014.6 

r2015.1 = max(t2015[,1]) - min(t2015[,1])
r2015.2 = max(t2015[,2]) - min(t2015[,2])
r2015.3 = max(t2015[,3]) - min(t2015[,3])
r2015.4 = max(t2015[,4]) - min(t2015[,4])
r2015.5 = max(t2015[,5]) - min(t2015[,5])
r2015.6 = max(t2015[,6]) - min(t2015[,6])

u2015.1 <- (max(t2015[,1]) - t2015[,1])/r2015.1 
u2015.2 <- (max(t2015[,2]) - t2015[,2])/r2015.2 
u2015.3 <- (max(t2015[,2]) - t2015[,3])/r2015.3 
u2015.4 <- (t2015[,4] - min(t2015[,4]))/r2015.4 
u2015.5 <- (t2015[,5] - min(t2015[,5]))/r2015.5 
u2015.6 <- (t2015[,6] - min(t2015[,6]))/r2015.6 

r2016.1 = max(t2016[,1]) - min(t2016[,1])
r2016.2 = max(t2016[,2]) - min(t2016[,2])
r2016.3 = max(t2016[,3]) - min(t2016[,3])
r2016.4 = max(t2016[,4]) - min(t2016[,4])
r2016.5 = max(t2016[,5]) - min(t2016[,5])
r2016.6 = max(t2016[,6]) - min(t2016[,6])

u2016.1 <- (max(t2016[,1]) - t2016[,1])/r2016.1 
u2016.2 <- (max(t2016[,2]) - t2016[,2])/r2016.2 
u2016.3 <- (max(t2016[,3]) - t2016[,3])/r2016.3 
u2016.4 <- (t2016[,4] - min(t2016[,4]))/r2016.4 
u2016.5 <- (t2016[,5] - min(t2016[,5]))/r2016.5 
u2016.6 <- (t2016[,6] - min(t2016[,6]))/r2016.6 

u2008 <- cbind(u2008.1, u2008.2,u2008.3,u2008.4,u2008.5, u2008.6)
u2009 <- cbind(u2009.1, u2009.2,u2009.3,u2009.4,u2009.5, u2009.6)
u2010 <- cbind(u2010.1, u2010.2,u2010.3,u2010.4,u2010.5, u2010.6)
u2011 <- cbind(u2011.1, u2011.2,u2011.3,u2011.4,u2011.5, u2011.6)
u2012 <- cbind(u2012.1, u2012.2,u2012.3,u2012.4,u2012.5, u2012.6)
u2013 <- cbind(u2013.1, u2013.2,u2013.3,u2013.4,u2013.5, u2013.6)
u2014 <- cbind(u2014.1, u2014.2,u2014.3,u2014.4,u2014.5, u2014.6)
u2015 <- cbind(u2015.1, u2015.2,u2015.3,u2015.4,u2015.5, u2015.6)
u2016 <- cbind(u2016.1, u2016.2,u2016.3,u2016.4,u2016.5, u2016.6)
u2007 <- cbind(u2007.1, u2007.2,u2007.3,u2007.4,u2007.5, u2007.6)

View(t2013)
View(u2009)
model <- prcomp(u2008)
summary(model)
plot(model$x, cex = 2.25, xlab = 'PC1 (45,91%)', ylab = 'PC2 (23,89%)', main = 'Rok 2007')
text(model$x, cex = 0.75)
model <- prcomp(u2016)
summary(model)
plot(model$x, cex = 2.25, xlab = 'PC1 (36,94%)', ylab = 'PC2 (22,80%)', main = 'Rok 2016')
text(model$x, cex = 0.75)

cov2012 <- cov(u2016)
eig2012 <- eigen(cov2012)
round(eig2012$vectors[,2],6)

model <- prcomp(u2016)
var <- get_pca_var(model)
round(var$contrib[,2],6)

#Preworking

X <- rbind(t(u2007[,1]),t(u2008[,1]),t(u2009[,1]),t(u2010[,1]),t(u2011[,1]),
           t(u2012[,1]),t(u2013[,1]),t(u2014[,1]),t(u2015[,1]),
           t(u2016[,1]))


colnames(X) <- c('Prework.Dolnosl.', 'Prework.Kuj-pom','Prework.Lubel',
                 'Prework.Lubus','Prework.Lodz','Prework.Malop','Prework.Mazow',
                 'Prework.Opol','Prework.Podkar','Prework.Podl','Prework.Pomor',
                 'Prework.Slask','Prework.Swiet','Prework.War-maz',
                 'Prework.Wielk','Prework.Zac-pom')


#Mobility

Y <- rbind(t(u2007[,2]),t(u2008[,2]),t(u2009[,2]),t(u2010[,2]),t(u2011[,2]),
           t(u2012[,2]),t(u2013[,2]),t(u2014[,2]),t(u2015[,2]),
           t(u2016[,2]))

colnames(Y) <- c('Mobility.Dolnosl.', 'Mobility.Kuj-pom','Mobility.Lubel',
                 'Mobility.Lubus','Mobility.Lodz','Mobility.Malop','Mobility.Mazow',
                 'Mobility.Opol','Mobility.Podkar','Mobility.Podl','Mobility.Pomor',
                 'Mobility.Slask','Mobility.Swiet','Mobility.War-maz',
                 'Mobility.Wielk','Mobility.Zac-pom')

#Nonmobility
A <- rbind(t(u2007[,3]),t(u2008[,3]),t(u2009[,3]),t(u2010[,3]),t(u2011[,3]),
           t(u2012[,3]),t(u2013[,3]),t(u2014[,3]),t(u2015[,3]),
           t(u2016[,3]))

colnames(A) <- c('Nonmobility.Dolnosl.', 'Nonmobility.Kuj-pom','Nonmobility.Lubel',
                 'Nonmobility.Lubus','Nonmobility.Lodz','Nonmobility.Malop','Nonmobility.Mazow',
                 'Nonmobility.Opol','Nonmobility.Podkar','Nonmobility.Podl','Nonmobility.Pomor',
                 'Nonmobility.Slask','Nonmobility.Swiet','Nonmobility.War-maz',
                 'Nonmobility.Wielk','Nonmobility.Zac-pom')

#Postworking

a <- rbind(t(u2007[,4]),t(u2008[,4]),t(u2009[,4]),t(u2010[,4]),t(u2011[,4]),
           t(u2012[,4]),t(u2013[,4]),t(u2014[,4]),t(u2015[,4]),
           t(u2016[,4]))

colnames(a) <- c('Postworking.Dolnosl.', 'Postworking.Kuj-pom','Postworking.Lubel',
                 'Postworking.Lubus','Postworking.Lodz','Postworking.Malop','Postworking.Mazow',
                 'Postworking.Opol','Postworking.Podkar','Postworking.Podl','Postworking.Pomor',
                 'Postworking.Slask','Postworking.Swiet','Postworking.War-maz',
                 'Postworking.Wielk','Postworking.Zac-pom')

#100work

b <- rbind(t(u2007[,5]),t(u2008[,5]),t(u2009[,5]),t(u2010[,5]),t(u2011[,5]),
           t(u2012[,5]),t(u2013[,5]),t(u2014[,5]),t(u2015[,5]),
           t(u2016[,5]))

colnames(b) <- c('100work.Dolnosl.', '100work.Kuj-pom','100work.Lubel',
                 '100work.Lubus','100work.Lodz','100work.Malop','100work.Mazow',
                 '100work.Opol','100work.Podkar','100work.Podl','100work.Pomor',
                 '100work.Slask','100work.Swiet','100work.War-maz',
                 '100work.Wielk','100work.Zac-pom')

c <- rbind(t(u2007[,6]),t(u2008[,6]),t(u2009[,6]),t(u2010[,6]),t(u2011[,6]),
           t(u2012[,6]),t(u2013[,6]),t(u2014[,6]),t(u2015[,6]),
           t(u2016[,6]))

colnames(c) <- c('100work.Dolnosl.', '100work.Kuj-pom','100work.Lubel',
                 '100work.Lubus','100work.Lodz','100work.Malop','100work.Mazow',
                 '100work.Opol','100work.Podkar','100work.Podl','100work.Pomor',
                 '100work.Slask','100work.Swiet','100work.War-maz',
                 '100work.Wielk','100work.Zac-pom')


View(X)
colnames(X) <- c('Dolno零kie','Kujawsko-pomorskie','Lubelskie','Lubuskie'
                 ,'�dzkie','Ma這polskie','Mazowieckie','Opolskie','Podkarpackie',
                 'Podlaskie','Pomorskie','零kie','i皻okrzyskie',
                 'Warmi雟ko-mazurskie','Wielkopolskie','Zachodniopomorskie')


library(Funclustering)

CWtime <- 2007:2016
CWtime
CWrange <- c(2007,2016)
CWbasis <- create.fourier.basis(CWrange, nbasis=9)
harmaccelLfd <- vec2Lfd(c(0,(2*pi/10)^(1/2),0), rangeval=CWrange)
CWfd1 <- smooth.basisPar(
  CWtime, X,CWbasis,
  Lfdobj=harmaccelLfd, lambda=1e-2)$fd

CWfd2 <- smooth.basisPar(
  CWtime, Y,CWbasis,
  Lfdobj=harmaccelLfd, lambda=1e-2)$fd
CWfd3 <- smooth.basisPar(
  CWtime, A,CWbasis,
  Lfdobj=harmaccelLfd, lambda=1e-2)$fd

CWfd4 <- smooth.basisPar(
  CWtime, a,CWbasis,
  Lfdobj=harmaccelLfd, lambda=1e-2)$fd
CWfd5 <- smooth.basisPar(
  CWtime, b,CWbasis,
  Lfdobj=harmaccelLfd, lambda=1e-2)$fd
CWfd6 <- smooth.basisPar(
  CWtime, c,CWbasis,
  Lfdobj=harmaccelLfd, lambda=1e-2)$fd
CWfd=list(CWfd1,CWfd2,CWfd3,CWfd4,CWfd5,CWfd6)
fd <- Data2fd(data, argvals=t)
pca=mfpca(CWfd,nharm=2)
mfpcaPlot.without(pca, grid=c(1,2))

plot(pca[[1]]$scores[,1],pca[[2]]$scores[,2], xlab = 'MFPC1 (39,28%)',ylab = 'MFPC2 (25,79%)', cex = 2.5)

fdX <- Data2fd(X, argvals=CWtime, basisobj=CWbasis)
fdY <- Data2fd(Y, argvals=CWtime, basisobj=CWbasis)
fdA <- Data2fd(A, argvals=CWtime, basisobj=CWbasis)
fda <- Data2fd(a, argvals=CWtime, basisobj=CWbasis)
fdb <- Data2fd(b, argvals=CWtime, basisobj=CWbasis)
fdc <- Data2fd(c, argvals=CWtime, basisobj=CWbasis)

pca[[1]]$varprop
plotfd(fdc, main = 'Nak豉dy s逝蕨ce gospodarce wodnej')


plot(pca[[1]]$scores, xlab = 'MFPC1 (39,28%)',ylab = 'MFPC2 (25,79%)', cex = 2.5)
text(-pca[[1]]$scores[,1], pca[[2]]$scores[,2], cex = 0.75)


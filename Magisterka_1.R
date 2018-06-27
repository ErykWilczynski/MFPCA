library(readxl)

tabela2008 <- read_excel("j:/Desktop/R/Multivariate data analysis/tabela06_2008.06.xlsx")
tabela2009 <- read_excel("j:/Desktop/R/Multivariate data analysis/tabela06_2009.06.xlsx")
tabela2010 <- read_excel("j:/Desktop/R/Multivariate data analysis/tabela06_2010.06.xlsx")
tabela2011 <- read_excel("j:/Desktop/R/Multivariate data analysis/tabela06_2011.06.xlsx")
tabela2012 <- read_excel("j:/Desktop/R/Multivariate data analysis/tabela06_2012.06.xlsx")
tabela2013 <- read_excel("j:/Desktop/R/Multivariate data analysis/tabela06_2013.06.xlsx")
tabela2014 <- read_excel("j:/Desktop/R/Multivariate data analysis/tabela06_2014.06.xlsx")
tabela2015 <- read_excel("j:/Desktop/R/Multivariate data analysis/tabela06_2015.06.xlsx")
tabela2016 <- read_excel("j:/Desktop/R/Multivariate data analysis/tabela06_2016.06.xlsx")
tabela2017 <- read_excel("j:/Desktop/R/Multivariate data analysis/tabela06_2017.06.xlsx")

t2017 <- tabela2017[-1,-c(2,3,5)]
t2017 <- t2017[c(1,7,13,18,21,27,34,43,46,51,55,61,70,73,77,84),]
t2016 <- tabela2016[-1,-c(2,4)]
t2016 <- t2016[c(1,7,13,18,21,27,34,43,46,51,55,61,70,73,77,84),]
t2015 <- tabela2015[-1,-c(2,4)]
t2015 <- t2015[c(1,7,13,18,21,27,34,43,46,51,55,61,70,73,77,84),]
t2014 <- tabela2014[-1,-c(2,4)]
t2014 <- t2014[c(1,7,11,16,19,25,31,38,41,46,50,55,64,67,71,78),]
t2013 <- tabela2013[-1,-c(2,4)]
t2013 <- t2013[c(1,7,11,16,19,25,31,38,41,46,50,55,64,67,71,78),]
t2012 <- tabela2012[-1,-c(2,4)]
t2012 <- t2012[c(1,7,11,16,19,25,31,38,41,46,50,55,64,67,71,78),]
t2011 <- tabela2011[-1,-c(2,4)]
t2011 <- t2011[c(1,7,11,16,19,25,31,38,41,46,50,55,64,67,71,78),]
t2010 <- tabela2010[-1,-c(2,4)]
t2010 <- t2010[c(1,7,11,16,19,25,31,38,41,46,50,55,64,67,71,78),]
t2009 <- tabela2009[-1,-c(2,4)]
t2009 <- t2009[c(1,7,11,16,19,25,31,38,41,46,50,55,64,67,71,78),]
t2008 <- tabela2008[-1,-c(2,4)]
t2008 <- t2008[c(1,7,11,16,19,25,31,38,41,46,50,55,64,67,71,78),]

library(Funclustering)

r2008.1 = max(t2008[,2]) - min(t2008[,2])
r2008.2 = max(t2008[,3]) - min(t2008[,3])
r2008.3 = max(t2008[,4]) - min(t2008[,4])
r2008.4 = max(t2008[,5]) - min(t2008[,5])
r2008.5 = max(t2008[,6]) - min(t2008[,6])

u2008.1 <- (t2008[,2] - min(t2008[,2]))/r2008.1 
u2008.2 <- (max(t2008[,3]) - t2008[,3])/r2008.2 
u2008.3 <- (max(t2008[,4]) - t2008[,4])/r2008.3 
u2008.4 <- (t2008[,5] - min(t2008[,5]))/r2008.4 
u2008.5 <- (t2008[,6] - min(t2008[,6]))/r2008.5

r2009.1 = max(t2009[,2]) - min(t2009[,2])
r2009.2 = max(t2009[,3]) - min(t2009[,3])
r2009.3 = max(t2009[,4]) - min(t2009[,4])
r2009.4 = max(t2009[,5]) - min(t2009[,5])
r2009.5 = max(t2009[,6]) - min(t2009[,6])

u2009.1 <- (t2009[,2] - min(t2009[,2]))/r2009.1 
u2009.2 <- (max(t2009[,3]) - t2009[,3])/r2009.2 
u2009.3 <- (max(t2009[,4]) - t2009[,4])/r2009.3 
u2009.4 <- (t2009[,5] - min(t2009[,5]))/r2009.4 
u2009.5 <- (t2009[,6] - min(t2009[,6]))/r2009.5 

r2010.1 = max(t2010[,2]) - min(t2010[,2])
r2010.2 = max(t2010[,3]) - min(t2010[,3])
r2010.3 = max(t2010[,4]) - min(t2010[,4])
r2010.4 = max(t2010[,5]) - min(t2010[,5])
r2010.5 = max(t2010[,6]) - min(t2010[,6])

u2010.1 <- (t2010[,2] - min(t2010[,2]))/r2010.1 
u2010.2 <- (max(t2010[,3]) - t2010[,3])/r2010.2 
u2010.3 <- (max(t2010[,4]) - t2010[,4])/r2010.3 
u2010.4 <- (t2010[,5] - min(t2010[,5]))/r2010.4 
u2010.5 <- (t2010[,6] - min(t2010[,6]))/r2010.5

r2011.1 = max(t2011[,2]) - min(t2011[,2])
r2011.2 = max(t2011[,3]) - min(t2011[,3])
r2011.3 = max(t2011[,4]) - min(t2011[,4])
r2011.4 = max(t2011[,5]) - min(t2011[,5])
r2011.5 = max(t2011[,6]) - min(t2011[,6])

u2011.1 <- (t2011[,2] - min(t2011[,2]))/r2011.1 
u2011.2 <- (max(t2011[,3]) - t2011[,3])/r2011.2 
u2011.3 <- (max(t2011[,4]) - t2011[,4])/r2011.3 
u2011.4 <- (t2011[,5] - min(t2011[,5]))/r2011.4 
u2011.5 <- (t2011[,6] - min(t2011[,6]))/r2011.5 

r2012.1 = max(t2012[,2]) - min(t2012[,2])
r2012.2 = max(t2012[,3]) - min(t2012[,3])
r2012.3 = max(t2012[,4]) - min(t2012[,4])
r2012.4 = max(t2012[,5]) - min(t2012[,5])
r2012.5 = max(t2012[,6]) - min(t2012[,6])

u2012.1 <- (t2012[,2] - min(t2012[,2]))/r2012.1 
u2012.2 <- (max(t2012[,3]) - t2012[,3])/r2012.2 
u2012.3 <- (max(t2012[,4]) - t2012[,4])/r2012.3 
u2012.4 <- (t2012[,5] - min(t2012[,5]))/r2012.4 
u2012.5 <- (t2012[,6] - min(t2012[,6]))/r2012.5 

r2013.1 = max(t2013[,2]) - min(t2013[,2])
r2013.2 = max(t2013[,3]) - min(t2013[,3])
r2013.3 = max(t2013[,4]) - min(t2013[,4])
r2013.4 = max(t2013[,5]) - min(t2013[,5])
r2013.5 = max(t2013[,6]) - min(t2013[,6])

u2013.1 <- (t2013[,2] - min(t2013[,2]))/r2013.1 
u2013.2 <- (max(t2013[,3]) - t2013[,3])/r2013.2 
u2013.3 <- (max(t2013[,4]) - t2013[,4])/r2013.3 
u2013.4 <- (t2013[,5] - min(t2013[,5]))/r2013.4 
u2013.5 <- (t2013[,6] - min(t2013[,6]))/r2013.5 

r2014.1 = max(t2014[,2]) - min(t2014[,2])
r2014.2 = max(t2014[,3]) - min(t2014[,3])
r2014.3 = max(t2014[,4]) - min(t2014[,4])
r2014.4 = max(t2014[,5]) - min(t2014[,5])
r2014.5 = max(t2014[,6]) - min(t2014[,6])

u2014.1 <- (t2014[,2] - min(t2014[,2]))/r2014.1 
u2014.2 <- (max(t2014[,3]) - t2014[,3])/r2014.2 
u2014.3 <- (max(t2014[,4]) - t2014[,4])/r2014.3 
u2014.4 <- (t2014[,5] - min(t2014[,5]))/r2014.4 
u2014.5 <- (t2014[,6] - min(t2014[,6]))/r2014.5 

r2015.1 = max(t2015[,2]) - min(t2015[,2])
r2015.2 = max(t2015[,3]) - min(t2015[,3])
r2015.3 = max(t2015[,4]) - min(t2015[,4])
r2015.4 = max(t2015[,5]) - min(t2015[,5])
r2015.5 = max(t2015[,6]) - min(t2015[,6])

u2015.1 <- (t2015[,2] - min(t2015[,2]))/r2015.1 
u2015.2 <- (max(t2015[,3]) - t2015[,3])/r2015.2 
u2015.3 <- (max(t2015[,4]) - t2015[,4])/r2015.3 
u2015.4 <- (t2015[,5] - min(t2015[,5]))/r2015.4 
u2015.5 <- (t2015[,6] - min(t2015[,6]))/r2015.5 

r2016.1 = max(t2016[,2]) - min(t2016[,2])
r2016.2 = max(t2016[,3]) - min(t2016[,3])
r2016.3 = max(t2016[,4]) - min(t2016[,4])
r2016.4 = max(t2016[,5]) - min(t2016[,5])
r2016.5 = max(t2016[,6]) - min(t2016[,6])

u2016.1 <- (t2016[,2] - min(t2016[,2]))/r2016.1 
u2016.2 <- (max(t2016[,3]) -t2016[,3])/r2016.2 
u2016.3 <- (max(t2016[,4]) - t2016[,4])/r2016.3 
u2016.4 <- (t2016[,5] - min(t2016[,5]))/r2016.4 
u2016.5 <- (t2016[,6] - min(t2016[,6]))/r2016.5 

r2017.1 = max(t2017[,2]) - min(t2017[,2])
r2017.2 = max(t2017[,3]) - min(t2017[,3])
r2017.3 = max(t2017[,4]) - min(t2017[,4])
r2017.4 = max(t2017[,5]) - min(t2017[,5])
r2017.5 = max(t2017[,6]) - min(t2017[,6])

u2017.1 <- (t2017[,2] - min(t2017[,2]))/r2017.1 
u2017.2 <- (max(t2017[,3]) - t2017[,3])/r2017.2 
u2017.3 <- (max(t2017[,4]) - t2017[,4])/r2017.3 
u2017.4 <- (t2017[,5] - min(t2017[,5]))/r2017.4 
u2017.5 <- (t2017[,6] - min(t2017[,6]))/r2017.5 


u2008 <- cbind(u2008.1, u2008.2,u2008.3,u2008.4,u2008.5)
u2009 <- cbind(u2009.1, u2009.2,u2009.3,u2009.4,u2009.5)
u2010 <- cbind(u2010.1, u2010.2,u2010.3,u2010.4,u2010.5)
u2011 <- cbind(u2011.1, u2011.2,u2011.3,u2011.4,u2011.5)
u2012 <- cbind(u2012.1, u2012.2,u2012.3,u2012.4,u2012.5)
u2013 <- cbind(u2013.1, u2013.2,u2013.3,u2013.4,u2013.5)
u2014 <- cbind(u2014.1, u2014.2,u2014.3,u2014.4,u2014.5)
u2015 <- cbind(u2015.1, u2015.2,u2015.3,u2015.4,u2015.5)
u2016 <- cbind(u2016.1, u2016.2,u2016.3,u2016.4,u2016.5)
u2017 <- cbind(u2017.1, u2017.2,u2017.3,u2017.4,u2017.5)

par(mfrow = c(2,1))
st2008 <- scale(t2008[,-1])
models <- prcomp(st2008)
model <- prcomp(t2007)
model2 <- prcomp(u2017)
summary(model2)

var <- get_pca_var(model)
round(var$contrib[,1],6)
plot(grid=c(1,2))
plot(model$x, cex = 2.25, xlab = 'PC1 (71,31%)', ylab = 'PC2 (28,08%)', main = 'Rok 2008')
text(model$x, cex = 0.75)
plot(model2$x, cex = 2.25, xlab = 'PC1 (88%)', ylab = 'PC2 (11,42%)', main = 'Rok 2017')
text(model2$x, cex = 0.75)
t2012 <- scale(t2012[,-1], center = TRUE, scale = TRUE)
View(t2012)

cov2012 <- cov(u2017)
eig2012 <- eigen(cov2012)
round(eig2012$vectors[,2],6)

install.packages('factoextra')
library("factoextra")

get_eigenvalue(cov2012)

View(CanadianWeather$dailyAv)
View(t2017)
#Preworking

X <- rbind(t(u2008[,1]),t(u2009[,1]),t(u2010[,1]),t(u2011[,1]),
           t(u2012[,1]),t(u2013[,1]),t(u2014[,1]),t(u2015[,1]),
           t(u2016[,1]), t(u2017[,1]))


colnames(X) <- c('Prework.Dolnosl.', 'Prework.Kuj-pom','Prework.Lubel',
                 'Prework.Lubus','Prework.Lodz','Prework.Malop','Prework.Mazow',
                 'Prework.Opol','Prework.Podkar','Prework.Podl','Prework.Pomor',
                 'Prework.Slask','Prework.Swiet','Prework.War-maz',
                 'Prework.Wielk','Prework.Zac-pom')


#Mobility

Y <- rbind(t(u2008[,2]),t(u2009[,2]),t(u2010[,2]),t(u2011[,2]),
           t(u2012[,2]),t(u2013[,2]),t(u2014[,2]),t(u2015[,2]),
           t(u2016[,2]), t(u2017[,2]))

colnames(Y) <- c('Mobility.Dolnosl.', 'Mobility.Kuj-pom','Mobility.Lubel',
                 'Mobility.Lubus','Mobility.Lodz','Mobility.Malop','Mobility.Mazow',
                 'Mobility.Opol','Mobility.Podkar','Mobility.Podl','Mobility.Pomor',
                 'Mobility.Slask','Mobility.Swiet','Mobility.War-maz',
                 'Mobility.Wielk','Mobility.Zac-pom')

#Nonmobility
A <- rbind(t(u2008[,3]),t(u2009[,3]),t(u2010[,3]),t(u2011[,3]),
           t(u2012[,3]),t(u2013[,3]),t(u2014[,3]),t(u2015[,3]),
           t(u2016[,3]), t(u2017[,3]))

colnames(A) <- c('Nonmobility.Dolnosl.', 'Nonmobility.Kuj-pom','Nonmobility.Lubel',
                 'Nonmobility.Lubus','Nonmobility.Lodz','Nonmobility.Malop','Nonmobility.Mazow',
                 'Nonmobility.Opol','Nonmobility.Podkar','Nonmobility.Podl','Nonmobility.Pomor',
                 'Nonmobility.Slask','Nonmobility.Swiet','Nonmobility.War-maz',
                 'Nonmobility.Wielk','Nonmobility.Zac-pom')

#Postworking

a <- rbind(t(u2008[,4]),t(u2009[,4]),t(u2010[,4]),t(u2011[,4]),
           t(u2012[,4]),t(u2013[,4]),t(u2014[,4]),t(u2015[,4]),
           t(u2016[,4]), t(u2017[,4]))

colnames(a) <- c('Postworking.Dolnosl.', 'Postworking.Kuj-pom','Postworking.Lubel',
                 'Postworking.Lubus','Postworking.Lodz','Postworking.Malop','Postworking.Mazow',
                 'Postworking.Opol','Postworking.Podkar','Postworking.Podl','Postworking.Pomor',
                 'Postworking.Slask','Postworking.Swiet','Postworking.War-maz',
                 'Postworking.Wielk','Postworking.Zac-pom')

#100work

b <- rbind(t(u2008[,5]),t(u2009[,5]),t(u2010[,5]),t(u2011[,5]),
           t(u2012[,5]),t(u2013[,5]),t(u2014[,5]),t(u2015[,5]),
           t(u2016[,5]), t(u2017[,5]))

colnames(b) <- c('100work.Dolnosl.', '100work.Kuj-pom','100work.Lubel',
                 '100work.Lubus','100work.Lodz','100work.Malop','100work.Mazow',
                 '100work.Opol','100work.Podkar','100work.Podl','100work.Pomor',
                 '100work.Slask','100work.Swiet','100work.War-maz',
                 '100work.Wielk','100work.Zac-pom')


rownames(X) <- c('2008','2009','2010','2011','2012','2013','2014','2015','2016','2017')
rownames(Y) <- c('2008','2009','2010','2011','2012','2013','2014','2015','2016','2017')
rownames(A) <- c('2008','2009','2010','2011','2012','2013','2014','2015','2016','2017')
rownames(a) <- c('2008','2009','2010','2011','2012','2013','2014','2015','2016','2017')
rownames(b) <- c('2008','2009','2010','2011','2012','2013','2014','2015','2016','2017')

CWtime <- 2008:2017
CWtime
CWrange <- c(2008,2017)
CWbasis <- create.fourier.basis(CWrange, nbasis = 9)
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

CWfd=list(CWfd1,CWfd2,CWfd3,CWfd4,CWfd5)
fd <- Data2fd(data, argvals=t)
pca=mfpca(CWfd,nharm=2)
mfpcaPlot.without(pca, grid = c(1,2))
Outputclass(pca)
summary(pca)
plotfd(pca$scores)
View(CWfd1$fd$coefs)

plot.pca.fd(CWfd1)

bazowe <- function(x)
{
  abs(eval.basis(x, CWbasis)%*%pca[[3]]$harmonics$coefs[,1])
}

pca[[1]]$harmonics$coefs[,1]

U.var1 = c()

U.var1[1] = round(integrate(bazowe, 2008, 2017)$value, digits = 5)
U.var1[2] = round(integrate(bazowe, 2008, 2017)$value, digits = 5)
U.var1[3] = round(integrate(bazowe, 2008, 2017)$value, digits = 5)
U.var1[4] = round(integrate(bazowe, 2008, 2017)$value, digits = 5)
U.var1[5] = round(integrate(bazowe, 2008, 2017)$value, digits = 5)

U.var1
U.var2 = c()
round(U.var1 / sum(U.var1) * 100, 2)

for (i in 1:5)
{
  bazowe2 <- function(x)
  {
    abs(eval.basis(x, CWbasis)%*%pca[[i]]$harmonics$coefs[,2])
  }
  U.var2[i] = round(integrate(bazowe2, 2008, 2017)$value, digits = 5)
}

U.var2

round(U.var2 / sum(U.var2) * 100, 2)


Vectorize(bazowe)
Vectorize

bazowe

coef(CWfd1)
scoreplot(pca)

pca[[1]]

plot(score)

score <- -pca[[1]]$scores

plot(pca[[1]]$scores, xlab = 'MFPC1 (80,41%)',ylab = 'MFPC2 (17,18%)', cex = 2.5)
text(pca[[1]]$scores, cex = 0.75)
summary(pca)

fdX <- Data2fd(X, argvals=CWtime, basisobj=CWbasis)
fdY <- Data2fd(Y, argvals=CWtime, basisobj=CWbasis)
fdA <- Data2fd(A, argvals=CWtime, basisobj=CWbasis)
fda <- Data2fd(a, argvals=CWtime, basisobj=CWbasis)
fdb <- Data2fd(b, argvals=CWtime, basisobj=CWbasis)

var.fd(fdX)
mean.fd(fdX)
pca.fd(fdX)

plot(pca.fd(fd)$scores)

plotfd(fdX)
plotfd(fdX, main = 'Liczba ludnoci w wieku przedprodukcyjnym', ylab = '')
plotfd(fdY, main = 'Liczba ludnoci w wieku mobilnym', ylab = '')
plotfd(fdA, main = 'Liczba ludnoci w wieku niemobilnym', ylab = '')
plotfd(fda, main = 'Liczba ludnoci w wieku poprodukcyjnym', ylab = '')
plotfd(fdb, main = 'Liczba ludnoci w wieku nieprodukcyjnym na 100 w wieku produkcyjnym', ylab='')
View(sdata)
dane <- cbind(matrix(X,10),matrix(Y,10),matrix(A,10),matrix(a,10),matrix(b,10))
View(dane)
dim(dane)
is.data.frame(dane)
fd <- Data2fd(dane, argvals = CWtime, basisobj = CWbasis)
pca = mfpca(fd, nharm=2)
pca

daybasis65 <- create.fourier.basis(c(0, 365), nbasis=65, period=365)

harmaccelLfd <- vec2Lfd(c(0,(2*pi/365)^2,0), c(0, 365))
harmfdPar     <- fdPar(daybasis65, harmaccelLfd, lambda=1e5)
daytempfd <- smooth.basis(day.5, CanadianWeather$dailyAv[,,"Temperature.C"],
                          daybasis65, fdnames=list("Day", "Station", "Deg C"))$fd

daytemppcaobj <- pca.fd(daytempfd, nharm=4, harmfdPar)


pcafd <- daytempfd
harmfd <- pcafd[[1]]
basisfd <- harmfd$basis
rangex <- basisfd$rangeval
nx = 128
length(nx)


percentvar <- round(100 * pcafd$varprop[iharm], 
                    1)
plot(argvals, meanmat, type = "l", ylim = c(min(pcmat), 
                                            max(pcmat)), ylab = paste("Harmonic", iharm), 
     main = paste("PCA function", iharm, "(Percentage of variability", 
                  percentvar, ")"))

plot.pca.fd.without <- function(x, nx = 128, harm = 0, expand = 0, cycle = FALSE, ...)
{
  pcafd <- x
  if (!(inherits(pcafd, "pca.fd"))) 
    stop("Argument 'x' is not a pca.fd object.")
  harmfd <- pcafd[[1]]
  basisfd <- harmfd$basis
  rangex <- basisfd$rangeval
  if (length(nx) > 1) {
    argvals <- nx
    nx <- length(x)
  }
  else {
    argvals <- seq(rangex[1], rangex[2], length = nx)
  }
  fdmat <- eval.fd(argvals, harmfd)
  meanmat <- eval.fd(argvals, pcafd$meanfd)
  dimfd <- dim(fdmat)
  nharm <- dimfd[2]
  plotsPerPg <- sum(par("mfrow"))
  harm <- as.vector(harm)
  if (harm[1] == 0) 
    harm <- (1:nharm)
  if (length(dimfd) == 2) {
    for (jharm in 1:length(harm)) {
      if (jharm == 2) {
        op <- par(ask = TRUE)
        on.exit(par(op))
      }
      iharm <- harm[jharm]
      if (expand == 0) {
        fac <- sqrt(pcafd$values[iharm])
      }
      else {
        fac <- expand
      }
      vecharm <- fdmat[, iharm]
      pcmat <- cbind(meanmat + fac * vecharm, meanmat - 
                       fac * vecharm)
      percentvar <- round(100 * pcafd$varprop[iharm], 1)
      plot(argvals, meanmat, type = "l", ylim = c(min(pcmat), 
                                                  max(pcmat)), ylab = paste("Harmonic", iharm), 
           main = paste("PCA function", iharm, "(Percentage of variability", 
                        percentvar, ")"))
          }
  }
  else {
    if (cycle && dimfd[3] == 2) {
      meanmat <- drop(meanmat)
      for (jharm in 1:length(harm)) {
        if (jharm == 2) {
          op <- par(ask = TRUE)
          on.exit(par(op))
        }
        iharm <- harm[jharm]
        {
          if (expand == 0) 
            fac <- 2 * sqrt(pcafd$values[iharm])
          else fac <- expand
        }
        matharm <- fdmat[, iharm, ]
        mat1 <- meanmat + fac * matharm
        mat2 <- meanmat - fac * matharm
        percentvar <- round(100 * pcafd$varprop[iharm], 
                            1)
        plot(meanmat[, 1], meanmat[, 2], type = plottype, 
             xlim = c(min(c(mat1[, 1], mat2[, 1])), max(c(mat1[, 
                                                               1], mat2[, 1]))), ylim = c(min(c(mat1[, 2], 
                                                                                                mat2[, 2])), max(c(mat1[, 2], mat2[, 2]))), 
             main = paste("PCA function", iharm, "(Percentage of variability", 
                          percentvar, ")"), ...)
      }
    }
    else {
      for (jharm in 1:length(harm)) {
        if (jharm == 2) {
          op <- par(ask = TRUE)
          on.exit(par(op))
        }
        iharm <- harm[jharm]
        fac <- {
          if (expand == 0) 
            sqrt(pcafd$values[iharm])
          else expand
        }
        meanmat <- drop(meanmat)
        matharm <- fdmat[, iharm, ]
        nvar <- dim(matharm)[2]
        for (jvar in 1:nvar) {
          pcmat <- cbind(meanmat[, jvar] + fac * matharm[, 
                                                         jvar], meanmat[, jvar] - fac * matharm[, 
                                                                                                jvar])
          percentvar <- round(100 * pcafd$varprop[iharm], 
                              1)
          plot(argvals, meanmat[, jvar], type = plottype, 
               ylab = paste("Harmonic", iharm), sub = paste("PCA function", 
                                                            iharm, "(Percentage of variability", percentvar, 
                                                            ")"), main = dimnames(fdmat)[[3]][jvar], 
               ...)
        }
      }
    }
  }
  invisible(NULL)
}

mfpcaPlot.without <- function(pca, grid = c())
{
  if (class(pca) == "pca.fd") {
    if (length(grid) == 0) {
      plot.pca.fd(pca)
    }
    else {
      dev.new()
      par(mfrow = grid)
      plot.pca.fd(pca)
    }
  }
  else {
    dimFd = length(pca)
    for (i in 1:dimFd) {
      cat("\n", "Dimension ", i, " of the functional data", 
          "\n")
      dev.new()
      if (length(grid) == 0) {
        plot.pca.fd.without(pca[[i]])
      }
      else {
        par(mfrow = grid)
        plot.pca.fd.without(pca[[i]])
      }
    }
  }
}

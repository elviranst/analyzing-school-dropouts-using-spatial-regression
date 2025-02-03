### Analisis Regresi Spasial ###
library(sf)
library(sp)
library(spdep)
library(readxl)
library(spatialreg)
library(ggplot2)

#Input data
apts_sumut <- read_excel("D:/Data/Data Sumut.xlsx")
View(apts_sumut)

#Peta
petasumut <- st_read("D:/Data/sumaterautara.shp")
View(petasumut)
st_geometry_type(petasumut)
st_crs(petasumut)

sumutshp<- st_make_valid(petasumut)

#Merge data
data_joined <- merge(sumutshp, apts_sumut, by = "WADMKK")
sumut <- as(data_joined, "Spatial")

#Definisi variabel
Y<- log(sumut$JPS)
X1<- sumut$RMG
X2<- sumut$RMS
X3<- sumut$RLS
X4<- sumut$TPT
X5<- sumut$APM
X6<- sumut$KEPADATAN_PENDUDUK

#Eksplorasi Data
summary(apts_sumut)

library(RColorBrewer)

#JPS: Y
coljps <- colorRampPalette(c("limegreen", "yellow", "red"))(3)
breaks<- seq(min(sumut$JPS), max(sumut$JPS), length.out = 4)
spplot(sumut, "JPS",
       col.regions = coljps, 
       at = breaks, 
       main = "Peta Sebaran Angka Putus Sekolah Sumatera Utara",
       sp.layout = list(           
         list("sp.text", coordinates(sumut), 
              sumut$WADMKK, cex = 0.4, col = "black")  
       ))

#RMG: X1
colrmg <- brewer.pal(3, "Greens")
spplot(sumut, "RMG",
       col.regions = colrmg, 
       cuts = 2, 
       main = "Peta Sebaran Rasio Murid Guru Sumatera Utara",
       sp.layout = list(           
         list("sp.text", coordinates(sumut), 
              sumut$WADMKK, cex = 0.4, col = "black")  
       ))

#RMS: X2
colrms <- brewer.pal(3, "Blues")
spplot(sumut, "RMS",
       col.regions = colrms, 
       cuts = 2, 
       main = "Peta Sebaran Rasio Murid Sekolah Sumatera Utara",
       sp.layout = list(           
         list("sp.text", coordinates(sumut), 
              sumut$WADMKK, cex = 0.4, col = "black")  
       ))

#RLS: X3
colrls <- brewer.pal(3, "Reds")
spplot(sumut, "RLS",
       col.regions = colrls, 
       cuts = 2, 
       main = "Peta Sebaran Rata-Rata Lama Sekolah Sumatera Utara",
       sp.layout = list(           
         list("sp.text", coordinates(sumut), 
              sumut$WADMKK, cex = 0.4, col = "black")  
       ))

#TPT: X4
coltpt <- brewer.pal(3, "Oranges")
spplot(sumut, "TPT",
       col.regions = coltpt, 
       cuts = 2, 
       main = "Peta Sebaran Tingkat Pengangguran Terbuka Sumatera Utara",
       sp.layout = list(           
         list("sp.text", coordinates(sumut), 
              sumut$WADMKK, cex = 0.4, col = "black")  
       ))

#APM: X5
colapm <- brewer.pal(3, "Purples")
spplot(sumut, "APM",
       col.regions = colapm, 
       cuts = 2, 
       main = "Peta Sebaran Angka Partisipasi Murni Sumatera Utara",
       sp.layout = list(           
         list("sp.text", coordinates(sumut), 
              sumut$WADMKK, cex = 0.4, col = "black")  
       ))

#KP: X6
colkp <- brewer.pal(3, "YlOrBr")
spplot(sumut, "KEPADATAN_PENDUDUK",
       col.regions = colkp, 
       cuts = 2, 
       main = "Peta Sebaran Kepadatan Penduduk Sumatera Utara",
       sp.layout = list(           
         list("sp.text", coordinates(sumut), 
              sumut$WADMKK, cex = 0.4, col = "black")  
       ))

# Model regresi berganda
model_lm <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6, data = sumut)
summary(model_lm)

# Cek asumsi regresi berganda
## normalitas
# Q-Q Plot
qqnorm(residuals(model_lm))
qqline(residuals(model_lm), col = "red")

#jarque bera
library(tseries)
normalitas <- jarque.bera.test(residuals(model_lm))
normalitas

# multikolinearitas
library(olsrr)
multikolinearitas <- ols_vif_tol(model_lm)
multikolinearitas 

# heterokedastisitas
library(lmtest)
heterokedastisitas <- bptest(model_lm)
heterokedastisitas

# plot residual vs nilai prediksi untuk melihat pola heteroskedastisitas
plot(fitted(model_lm), residuals(model_lm), 
     main = "Residual vs Fitted", 
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")

# autokorelasi
autokorelasi <- dwtest(model_lm)
autokorelasi

### Matriks pembobot spasial
# Queen
queen <- read_excel("D:/Data/matriks_queen.xlsx")
colnames <- queen[[1]]
rownames <- colnames
nb_queen <- as.matrix(queen[,-1])
listw_queen<- mat2listw(nb_queen, zero.policy = TRUE)
listw_queen

# Rook
rook <- read_excel("D:/Data/matriks_rook.xlsx")
colnames <- rook[[1]]
rownames <- colnames
nb_rook <- as.matrix(rook[,-1])
listw_rook<- mat2listw(nb_rook, zero.policy = TRUE)
listw_rook

#Bishop
bishop <- read_excel("D:/Data/matriks_bishop.xlsx")
colnames <- bishop[[1]]
rownames <- colnames
nb_bishop <- as.matrix(bishop[,-1])
listw_bishop<- mat2listw(nb_bishop, zero.policy = TRUE)
listw_bishop

# Indeks moran
moran_queen <- moran.test(Y, listw = listw_queen)
moran_rook <- moran.test(Y, listw = listw_rook)
moran_bishop <- moran.test(Y, listw = listw_bishop)

moranI_queen <- round(moran_queen$estimate[1], 4)
moranI_rook <- round(moran_rook$estimate[1], 4)

# Visualisasi autokorelasi spasial (Moran Plot)
moran.plot(Y, listw = listw_queen, labels = sumut$WADMKK, 
           pch = 1, ylab = "Spatial lag of Y")
text(x = min(Y), y = max(lag.listw(listw_queen, Y)), 
     labels = paste("Moran's I Queen:", moranI_queen),
     pos = 4, col = "black", font = 1, cex = 0.5) 

moran.plot(Y, listw = listw_rook, labels = sumut$WADMKK, 
           pch = 1, ylab = "Spatial lag of Y")
text(x = min(Y), y = max(lag.listw(listw_rook, Y)), 
     labels = paste("Moran's I Rook:", moranI_rook),
     pos = 4, col = "black", font = 1, cex = 0.5) 

# Lagrange Multiplier
# Uji Lagrange Multiplier (LM)
lm.RStests(model_lm, listw_queen, test = "all")
lm.RStests(model_lm, listw_rook, test = "all")
lm.RStests(model_lm, listw_bishop, test = "all")

# Model Regresi Spasial: SEM
model_sem_queen <- errorsarlm(Y ~ X1 + X2 + X3 + X4 + X5 + X6, data = sumut, 
                              listw = listw_queen)
summary(model_sem_queen)

#Uji kebaikan model
#R²
#Model regresi berganda
ypred<- model_lm$fitted.values
sst <- sum((Y - mean(Y))^2)
ssr <- sum((Y - ypred)^2)
rsquare_lm <- 1 - (ssr/sst)

#Model regresi spasial
#SEM
#queen
ypred_queen<- model_sem_queen$fitted.values
sst <- sum((Y - mean(Y))^2)
ssr <- sum((Y - ypred_queen)^2)
rsquare_sem_queen <- 1 - (ssr/sst)

rsquare_lm
rsquare_sem_queen

#AIC
aic_lm <- AIC (model_lm)
aic_sem_queen <- AIC(model_sem_queen)

aic_lm
aic_sem_queen

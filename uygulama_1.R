#çalışma dizini (working directory) tespiti ve değişimi
getwd()
#setwd("C:\Users\yenil\Downloads\Veri Madenciliği")
setwd("C:/Users/yenil/Downloads")
getwd()

#gerekli paketlerin indirilmesi ve çalıştırılması

#install.packages("readxl")
#install.packages("readr")
#install.packages("tidyverse")
#install.packages("glmnet")
#install.packages("ggplot2")
#install.packages("caret")
#install.packages("corrplot")

library(caret)
library(corrplot)
library(ggplot2)
library(readxl)
library(readr)
library(tidyverse)
library(glmnet)

#verinin excel den okutulması ve izlenmesi, değişken isimlerinin kontrolü
veri=read_excel("C:/Users/yenil/Downloads/veriler_i_t.xlsx")
View(veri)
names(veri)

#yeni isimlerin atamması için 1den 90 a dizi oluşturma
new_names= paste("x_", 1:90, sep="")

#verinin yedeklenmesi
veri_rn <- veri
#yeni (ilk verinin kopyası) verinin değişken isimlerinin atanması
names(veri_rn)  <- new_names

#verilerin kontrolü 
View(veri)
View(veri_rn)
#aynımı sınaması - tek fark değişken - sütun isimleri farklı (kontrol hücreler için)
veri == veri_rn

#data frame oluşturma ve tibble 
veri_df = data.frame(veri_rn)
veri_df_tib <- as_tibble(veri_df)
#başlıkların ve sınıfların kontrolü
head(veri_df_tib)

#y nin atanması yalnızca 14. sütun
y=veri_df_tib[14]
#3 tane y ataması
y3=select(veri_df_tib, x_12, x_13, x_14)

#x lerin oluşturulması (x_1 ve x_2 de etiket olduğu için çıkarıldı)
x <- subset(veri_df_tib, select = -c(x_1, x_2, x_12, x_13, x_14 ))
#aynı x leri farklı bir şekilde oluşturma
x2 <- veri_df_tib[ , c(3:11, 15:90)]
#kontrol
x==x2

#x lerin dataframe olarak tutulması
x_df = data.frame(x)

#Nümerik değerlerin filtrelenmesi
all_num <- Filter(is.numeric, veri_df_tib)
x_num <- Filter(is.numeric, x)

#müstakil bir fonksiyon. Numerik ve NA değerlerini eleyen
is_numeric_nu_na <- function(col){
  is_numeric <- is.numeric(col)
  no_na <- !anyNA(col)
  return(is_numeric & no_na)
}

#tüm değişkenler ve x ler oluşturulan fonksiyona giriliyor
num_colu_no_na_all <- Filter(is_numeric_nu_na, all_num)
num_colu_no_na_x <- Filter(is_numeric_nu_na, x_num)

#tüm değişkenlerin tanımlayıcı istatistikleri elde ediliyor
summ_stat <- summary(num_colu_no_na_all)

#---

#kullanılacak değişkenler
all_used <- num_colu_no_na_all[, c("x_12", "x_13", "x_14", "x_6", "x_9", "x_11", "x_16", "x_18", "x_19", "x_20", "x_27", "x_28", "x_52", "x_63")]
x_used <- num_colu_no_na_x[, c("x_6", "x_9", "x_11", "x_16", "x_18", "x_19", "x_20", "x_27", "x_28", "x_52", "x_63")]
#x_used2 <- subset(all_num, select = c("x_6", "x_9", "x_11", "x_16", "x_18", "x_19", "x_20", "x_27", "x_28", "x_52", x_55, "x_63", "x_68", "x_71", "x_72", "x_73"))
      

      #araya giren iris verisi ile basit bir grafik
      data <- iris[,1:5]
      
      ggplot(data, aes(x=iris$Sepal.Length, y=iris$Sepal.Width)) + 
      geom_point() +
      facet_wrap(~ Species)


      
#filtreleme ile korelasyon
corr_mat_all <- cor(all_used)
highly_cor_mat_all <- findCorrelation(corr_mat_all, cutoff = 0.7)
print(all_used[, highly_cor_mat_all])
corr_all_plot = corrplot(corr_mat_all, method = "color")


corr_mat_all_used <- cor(all_used)
high_corr_all_used <- findCorrelation(corr_mat_all_used, cutoff = 0.7)
corr_all_used_plot = corrplot(corr_mat_all_used, method = "color")


corr_mat_x_used <- cor(x_used)
high_corr_x_used <- findCorrelation(corr_mat_x_used, cutoff = 0.7)
corr_x_used_plot = corrplot(corr_mat_x_used, method = "color")


        #araya giren iris verisi için korelasyon grafiği
        data4 <- iris[,1:4]
        corr_mat <- cor(data4)
        corrplot(corr_mat, method = "color")


        
        #------
        
        # iris datası için
        data(iris)
        
        # x ler ilk 4 mümerik değer y ise Species
        predictors <- iris[, 1:4]  # Selecting all numeric features
        response <- iris$Species
        
        # model kontrol parametlerinin tanımlanması
        ctrl <- trainControl(method = "cv", number = 5)
        
        # glmnet ile LASSO modeli
        lasso_model <- train(predictors, response,
                             method = "glmnet",
                             trControl = ctrl,
                             tuneGrid = expand.grid(alpha = 1, lambda = seq(0.001, 1, length = 100)))
        
        # değişkenlerin seçimi
        selected_features <- coef(lasso_model, s = 0)  # s = 0 for the best lambda value
        
        # seçilen değişkenlerin basılması
        print(selected_features)
        

#lasso ile feature selection (embedded tech)

y= all_used$x_14

# y yanıt değişkeni x ler ise feature matrisi alınsın
# LASSO regression modeli
lasso_model <- glmnet(x_used, y, alpha = 1)  # alpha = 1 for LASSO

# LASSO grafik
plot(lasso_model)

# katsayısı 0 olmayan değişkenleri seç
selected_features <- coef(lasso_model, s = 0)  # s = 0 for the best lambda value

# seçilen değişkenleri tanımla
selected_features <- selected_features[selected_features != 0]

#seçilenleri bas
print(selected_features)




        
        


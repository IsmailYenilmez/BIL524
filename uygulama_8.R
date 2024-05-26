# Gerekli Kütüphaneleri Yükle
install.packages("quantmod")
install.packages("neuralnet")
install.packages("keras")

library(quantmod)
library(neuralnet)
library(keras)

# 1. Hisse Senedi Verisi Çekme
# Aşağıdaki örnek, Apple Inc. (AAPL) hisse senedi verisini Yahoo Finance'den çeker
getSymbols("AAPL", src = "yahoo", from = "2020-01-01", to = "2023-12-31")
stock_data <- AAPL

# Veriyi Göster
head(stock_data)

# 2. Veriyi Hazırlama
# Kapanış fiyatını kullanarak basit bir veri seti oluşturma
data <- data.frame(Date = index(stock_data), Close = as.numeric(Cl(stock_data)))
data <- na.omit(data)

# Normalize Etme Fonksiyonu
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data$Close <- normalize(data$Close)

# Eğitim ve Test Verisi Ayırma
set.seed(123)
train_indices <- sample(1:nrow(data), round(0.8 * nrow(data)))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# 3. Sinir Ağı Uygulaması
# Basit bir sinir ağı modeli oluşturma
nn_model <- neuralnet(Close ~ Date, data = train_data, hidden = c(5, 3), linear.output = TRUE)

# Modeli Göster
plot(nn_model)


# 4. Kullanılan Yöntemler ve Özellikler
# - quantmod: Finansal verileri çekmek ve analiz etmek için kullanılır.
# - neuralnet: Basit sinir ağı modelleri oluşturmak ve eğitmek için kullanılır.
# - keras: Derin öğrenme modelleri (özellikle LSTM) oluşturmak ve eğitmek için kullanılır.
# - Normalizasyon: Verileri aynı ölçeğe getirmek için kullanılır.
# - Eğitim/Test Ayrımı: Modelin performansını değerlendirmek için veriyi ayırma işlemi.


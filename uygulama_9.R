# Gerekli Kütüphanelerin Yüklenmesi
install.packages("neuralnet")
library(neuralnet)

# Örnek Veri Seti: iris veri seti
data(iris)

# 1. Çok Katmanlı Algılayıcı (MLP)
# MLP, genellikle sınıflandırma ve regresyon problemleri için kullanılır. neuralnet kütüphanesini kullanarak basit bir MLP modeli oluşturabiliriz.
# Veriyi Hazırlama
set.seed(123)
index <- sample(1:nrow(iris), nrow(iris) * 0.8)
train_data <- iris[index, ]
test_data <- iris[-index, ]

# Neuralnet için Veriyi Ölçekleme
maxs <- apply(train_data[, 1:4], 2, max)
mins <- apply(train_data[, 1:4], 2, min)
scaled_train_data <- as.data.frame(scale(train_data[, 1:4], center = mins, scale = maxs - mins))
scaled_test_data <- as.data.frame(scale(test_data[, 1:4], center = mins, scale = maxs - mins))

# Neuralnet için Veri Hazırlama
scaled_train_data$Species <- as.factor(train_data$Species)
scaled_test_data$Species <- as.factor(test_data$Species)

# Modeli Oluşturma
nn <- neuralnet(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                data = scaled_train_data, hidden = c(5, 3), linear.output = FALSE)

# Modeli Gösterme
plot(nn)

# Tahmin Yapma
nn_results <- compute(nn, scaled_test_data[, 1:4])
predictions <- apply(nn_results$net.result, 1, which.max)
predicted_species <- levels(iris$Species)[predictions]

# Sonuçları Karşılaştırma
table(Predicted = predicted_species, Actual = scaled_test_data$Species)
table(Predicted = predicted_species, Actual = test_data$Species)



# 2. Evrişimli Sinir Ağları (CNN)
# R'da RSNNS gibi kütüphaneler kullanarak daha ileri düzey sinir ağı modelleri oluşturulabilir, 
# ancak CNN'ler için genellikle keras veya tensorflow tercih edilir. 
# RSNNS kütüphanesini kullanarak basit sinir ağı modelleri oluşturabiliriz.
# Gerekli Kütüphanelerin Yüklenmesi
install.packages("RSNNS")
library(RSNNS)

# Örnek Veri Seti: iris veri seti
data(iris)

# Veriyi Hazırlama
set.seed(123)
index <- sample(1:nrow(iris), nrow(iris) * 0.8)
train_data <- iris[index, ]
test_data <- iris[-index, ]

# RSNNS için Veriyi Ölçekleme
maxs <- apply(train_data[, 1:4], 2, max)
mins <- apply(train_data[, 1:4], 2, min)
scaled_train_data <- as.data.frame(scale(train_data[, 1:4], center = mins, scale = maxs - mins))
scaled_test_data <- as.data.frame(scale(test_data[, 1:4], center = mins, scale = maxs - mins))

# RSNNS için Veri Hazırlama
train_data_matrix <- as.matrix(scaled_train_data[, 1:4])
test_data_matrix <- as.matrix(scaled_test_data[, 1:4])

# Modeli Oluşturma
model <- mlp(train_data_matrix, decodeClassLabels(train_data$Species), size = c(5, 3), maxit = 100)

# Tahmin Yapma
predictions <- predict(model, test_data_matrix)
predicted_species <- decodeClassLabels(predictions)




# 3. Tekrarlayan Sinir Ağları (RNN)
# R'da RSNNS kütüphanesini kullanarak basit bir RNN modeli oluşturabiliriz. Aşağıda sine dalgası tahmini için RNN modeli gösterilmiştir.

# Örnek Veri: Sine dalgası
set.seed(42)
timesteps <- 100
x <- seq(0, 2 * pi, length.out = timesteps)
y <- sin(x)

# Eğitim ve Test Verisi
x_train <- matrix(y[1:80], nrow = 1)
y_train <- matrix(y[2:81], nrow = 1)
x_test <- matrix(y[81:100], nrow = 1)
y_test <- matrix(y[82:101], nrow = 1)

# Modeli Oluşturma
model <- elman(x_train, y_train, size = c(5, 3), maxit = 100)


# 4. Otomatik Kodlayıcılar (Autoencoders)
# R'da h2o kütüphanesi kullanarak otomatik kodlayıcılar oluşturabiliriz. Aşağıda basit bir otomatik kodlayıcı modeli gösterilmiştir.
# Gerekli Kütüphanelerin Yüklenmesi
install.packages("h2o")
library(h2o)

# H2O'yu Başlatma
h2o.init()

# Örnek Veri Seti: MNIST
mnist <- dataset_mnist()
x_train <- as.h2o(mnist$train$x / 255)
x_test <- as.h2o(mnist$test$x / 255)

# Otomatik Kodlayıcı Modeli
autoencoder <- h2o.deeplearning(
  x = names(x_train),
  training_frame = x_train,
  autoencoder = TRUE,
  hidden = c(256, 128, 256),
  epochs = 50
)

# Kodlanmış Veriyi Elde Etme
encoded_data <- h2o.deepfeatures(autoencoder, x_test, layer = 2)

# Kod Çözülmüş Veriyi Elde Etme
decoded_data <- h2o.predict(autoencoder, encoded_data)

# Sonuçları Gösterme
decoded_data <- as.matrix(decoded_data)
plot(as.vector(mnist$test$x[1, , , 1]), type = 'l', col = 'blue')
lines(as.vector(decoded_data[1, ]), col = 'red')
legend('topright', legend = c('Actual', 'Decoded'), col = c('blue', 'red'), lwd = 2)


# 5. Generative Adversarial Networks (GANs)
# R'da GAN'ler için uygun bir kütüphane bulunmamakla birlikte, teorik olarak GAN mimarisi benzeri modeller oluşturabilirsiniz. 
# GAN'ler genellikle Python ve TensorFlow/Keras kullanılarak geliştirilir.

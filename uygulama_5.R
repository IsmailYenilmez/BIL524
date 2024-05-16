# caret paketini yükle
install.packages("caret")
library(caret)

# Örnek veri setini yükle
data(iris)

# Model eğitimine ve testine bölmek için veri setini hazırla
set.seed(123)
trainIndex <- createDataPartition(iris$Species, p = .8, list = FALSE, times = 1)
trainData <- iris[trainIndex,]
testData  <- iris[-trainIndex,]

#---
# Yaygın kullanılan sınıflandırma algoritmaları arasında Naive Bayes, 
# Karar Ağaçları, Destek Vektör Makineleri (SVM), K-En Yakın Komşu (KNN), 
# Lojistik Regresyon ve Derin Öğrenme yöntemleri listelenebilir.
# Hangi algoritmanın kullanılacağı, veri setinin boyutu, kompleksliği,
# doğası ve hedeflenen performans ölçütlerine bağlı olarak değişir.
#---

# Lojistik regresyon modelini eğit
model <- train(Species ~ ., data = trainData, family = "logit")

# Model performansını değerlendir
predictions <- predict(model, testData)
c1<-confusionMatrix(predictions, testData$Species)

#---
# Lojistik regresyon, iki veya daha fazla sınıf arasında ayırıcı bir sınırlama oluşturmak için kullanılır.
# Sınıflandırma işlemi, girdi özelliklerinin bir lineer kombinasyonu üzerinde bir lojistik fonksiyon kullanılarak gerçekleştirilir.
# Lojistik regresyon, temel olarak doğrusal sınırlara sahip olduğu için basit ve yorumlanabilir bir sınıflandırma yöntemidir.
#---


#---karar agaclari
# rpart paketini yükle
install.packages("rpart")
library(rpart)

# Karar ağacı modelini eğit
model2 <- rpart(Species ~ ., data = trainData, method = "class")

# Model performansını değerlendir
predictions2 <- predict(model2, testData, type = "class")
c2<- confusionMatrix(predictions2, testData$Species)

#---
# Karar ağaçları, veri kümesindeki özelliklerin değerlerine göre karar düğümleri oluşturarak sınıflandırma yapar.
# Her bir karar düğümü, veri kümesini daha küçük alt kümelerine böler.
# Karar ağaçları, doğrusal olmayan ilişkileri yakalama yeteneği sayesinde daha karmaşık sınıflandırma problemleri için uygun bir seçenektir.
#---

#---destek vektor makineleri (Support Vector Machine-SVM)
# e1071 paketini yükle
install.packages("e1071")
library(e1071)

# SVM modelini eğit
model3 <- svm(Species ~ ., data = trainData)

# Model performansını değerlendir
predictions3 <- predict(model3, testData)
c3<- confusionMatrix(predictions3, testData$Species)

#---
# SVM, veri noktalarını ayıran en iyi hiper düzlemi bulmaya çalışır.
# Bu düzlem, sınıflar arasındaki marjinal boşluğu (maksimum) maksimize etmeye çalışır.
# SVM, yüksek boyutlu veri setlerinde ve karmaşık sınıflandırma problemlerinde etkili olabilir, ancak büyük veri setlerinde hesaplama maliyeti yüksek olabilir.
#---

#---k en yakın komsu
# caret paketini yükle
library(caret)

# KNN modelini eğit
model4 <- train(Species ~ ., data = trainData, method = "knn")

# Model performansını değerlendir
predictions4 <- predict(model4, testData)
c4<- confusionMatrix(predictions4, testData$Species)

#---
# KNN, yeni bir veri noktasını sınıflandırmak için en yakın komşularının etiketlerine dayanır.
# Sınıflandırma işlemi, yeni veri noktasının en yakın k komşusu tarafından belirlenir ve çoğunluk etiketi olarak atanır.
# KNN, basit ve yüksek boyutlu veri setlerinde etkili olabilir, ancak hesaplama maliyeti kritik olabilir.
#---

#--- extra credit: Naive Bayes
# Naive Bayes sınıflandırıcıları, Bayes teoremi ve basit olasılık modellemesi kullanarak sınıflandırma yapar.
# Her özelliğin birbirinden bağımsız olduğunu varsayar (bu nedenle "naive" olarak adlandırılır).
# Naive Bayes, büyük boyutlu özellik uzaylarıyla etkili bir şekilde çalışır ve özellikle doğal dil işleme gibi alanlarda yaygın olarak kullanılır.

#PS:
#Bu yöntemler arasındaki farklar, temel algoritmanın işleyiş şekli, özellikler,
#performans ve kullanım durumlarına dayanır. 
#Bazı yöntemler doğrusal sınırlar oluştururken, diğerleri doğrusal olmayan sınıflandırma
#problemleri için daha uygundur. Ayrıca, her bir yöntemin avantajları ve dezavantajları
#vardır ve doğru yöntemi seçmek için veri setinizin özelliklerini ve ihtiyaçlarınızı dikkate almalısınız.


#--- hepsi
# Load required libraries
library(caret)
library(rpart)
library(e1071)
library(class)

# Load iris dataset
data(iris)

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(iris$Species, p = .8, list = FALSE, times = 1)
trainData <- iris[trainIndex,]
testData  <- iris[-trainIndex,]

# Train and evaluate logistic regression model
model_logistic <- train(Species ~ ., data = trainData, family = "logit")
pred_logistic <- predict(model_logistic, testData)
acc_logistic <- confusionMatrix(pred_logistic, testData$Species)$overall["Accuracy"]

# Train and evaluate decision tree model
model_tree <- rpart(Species ~ ., data = trainData, method = "class")
pred_tree <- predict(model_tree, testData, type = "class")
acc_tree <- confusionMatrix(pred_tree, testData$Species)$overall["Accuracy"]

# Train and evaluate SVM model
model_svm <- svm(Species ~ ., data = trainData)
pred_svm <- predict(model_svm, testData)
acc_svm <- confusionMatrix(pred_svm, testData$Species)$overall["Accuracy"]

# Train and evaluate KNN model
model_knn <- knn(train = trainData[, -5], test = testData[, -5], cl = trainData$Species, k = 3)
#pred_knn <- predict(model_knn, testData)
acc_knn <- confusionMatrix(model_knn, testData$Species)$overall["Accuracy"]

# Display results
cat("Logistic Regression Accuracy:", acc_logistic, "\n")
cat("Decision Tree Accuracy:", acc_tree, "\n")
cat("SVM Accuracy:", acc_svm, "\n")
cat("KNN Accuracy:", acc_knn, "\n")

predictions
testData
testData$Species
predictions == testData$Species





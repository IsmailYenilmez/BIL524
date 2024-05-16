# Örnek veri seti oluşturma
set.seed(123)
# Kredi skoru
credit_score <- round(runif(100, min = 300, max = 850))
# Gelir (binlerce dolar cinsinden)
income <- round(runif(100, min = 20, max = 200), digits = 1)
# Kredi başvurusunun sonucu (0: Reddedildi, 1: Onaylandı)
approval <- ifelse((0.7 * credit_score + 0.3 * income) > 500, 1, 0)

# Veri setini bir veri çerçevesine dönüştürme
data <- data.frame(credit_score = credit_score, income = income, approval = as.factor(approval))

# Veri setini görselleştirme (Opsiyonel)
# library(ggplot2)
# ggplot(data, aes(x = credit_score, y = income, color = approval)) + geom_point() + theme_minimal()

# Veri setini eğitim ve test setlerine ayırma
trainIndex <- sample(1:nrow(data), 0.8 * nrow(data))
trainData <- data[trainIndex,]
testData <- data[-trainIndex,]

# Logistik regresyon modelini eğitme
model <- glm(approval ~ credit_score + income, data = trainData, family = "binomial")

# Model performansını değerlendirme
predictions <- ifelse(predict(model, newdata = testData, type = "response") > 0.5, 1, 0)

# Gerçek değerleri yeniden oluştur
true_values <- ifelse(testData$approval == "1", 1, 0)



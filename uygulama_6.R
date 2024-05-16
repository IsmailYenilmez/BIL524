# Doğrusal regresyon modelini oluşturma
linear_model <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=iris)

# Model özetini görüntüleme
summary(linear_model)

# Tahminleri görüntüleme
predicted_linear <- predict(linear_model)




# Lojistik regresyon modelini oluşturma
logistic_model <- glm(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=iris, family=binomial)

# Model özetini görüntüleme
summary(logistic_model)

# Tahminleri görüntüleme
predicted_logistic <- predict(logistic_model, type="response")




# Polinom regresyon modelini oluşturma
poly_model <- lm(Sepal.Length ~ poly(Sepal.Width, degree=2) + poly(Petal.Length, degree=2) + poly(Petal.Width, degree=2), data=iris)

# Model özetini görüntüleme
summary(poly_model)

# Tahminleri görüntüleme
predicted_poly <- predict(poly_model)




      
# Çoklu regresyon modelini oluşturma
multiple_model <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species, data=iris)

# Model özetini görüntüleme
summary(multiple_model)

# Tahminleri görüntüleme
predicted_multiple <- predict(multiple_model)


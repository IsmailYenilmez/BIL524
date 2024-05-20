# R'ı k-means paketi ile yükle
install.packages("cluster")
library(cluster)

# Örnek veri oluşturma
set.seed(123)
data <- matrix(rnorm(100), ncol=2)

# K-means uygulama
kmeans_result <- kmeans(data, centers=3)

# Küme merkezlerini ve atamalarını görselleştirme
plot(data, col=kmeans_result$cluster)
points(kmeans_result$centers, col=1:3, pch=8, cex=2)




# R'ı k-means paketi ile yükle
install.packages("cluster")
library(cluster)

# Örnek veri oluşturma
set.seed(123)
data <- matrix(rnorm(100), ncol=2)

# Boş bir liste oluştur
withinss <- vector("numeric", length = 10)

# Farklı küme sayıları için K-means uygulama ve Within-cluster sum of squares (WCSS) değerlerini hesaplama
for (k in 1:10) {
  kmeans_result <- kmeans(data, centers = k)
  withinss[k] <- kmeans_result$tot.withinss
}

# WCSS değerlerini görselleştirme
plot(1:10, withinss, type="b", xlab="Number of clusters", ylab="WCSS")

# Optimum küme sayısını belirleme (ör. dirsek kuralı veya başka bir yöntem kullanılabilir)


# R'ı k-means paketi ile yükle
install.packages("cluster")
library(cluster)

# Örnek veri oluşturma
set.seed(123)
data <- matrix(rnorm(100), ncol=2)

# Boş bir vektör oluştur
wcss <- vector("numeric", length = 10)

# Farklı küme sayıları için K-means uygulama ve WCSS değerlerini hesaplama
for (k in 1:10) {
  kmeans_result <- kmeans(data, centers = k)
  wcss[k] <- kmeans_result$tot.withinss
}

# Elbow Method'u uygulayarak optimal küme sayısını belirleme
elbow_point <- which(diff(wcss) < mean(diff(wcss)))

# Optimal küme sayısını yazdırma
cat("Optimal küme sayısı:", elbow_point, "\n")

# WCSS değerlerini görselleştirme
plot(1:10, wcss, type="b", xlab="Küme Sayısı", ylab="WCSS")
points(elbow_point, wcss[elbow_point], col="red", pch=19)





# R'ı k-means paketi ile yükle
install.packages("cluster")
library(cluster)

# Örnek veri oluşturma
set.seed(123)
data <- matrix(rnorm(100), ncol=2)

# Boş bir vektör oluştur
wcss <- vector("numeric", length = 10)

# Farklı küme sayıları için K-means uygulama ve WCSS değerlerini hesaplama
for (k in 1:10) {
  kmeans_result <- kmeans(data, centers = k)
  wcss[k] <- kmeans_result$tot.withinss
}

# Gap İstatistiği fonksiyonunu tanımlama
gap_stat <- function(data, k.max = 10, B = 50) {
  gap <- vector(length = k.max)
  maxgap <- vector(length = k.max)
  for (k in 1:k.max) {
    gap[k] <- sum(abs(wcss[1:k] - wcss[k + 1])) / B
    maxgap[k] <- max(t(sapply(1:B, function(i) {
      cl <- kmeans(data, centers = k)$cluster
      cl2 <- sample(cl)
      sum(abs(wcss[1:k] - wcss[k + 1])[cl == cl2])
    })))
  }
  list(gap = gap, maxgap = maxgap)
}

# Gap İstatistiği uygulama
gap_result <- gap_stat(data)

# Optimal küme sayısını bulma
optimal_k <- which(gap_result$gap >= gap_result$maxgap)

# Optimal küme sayısını yazdırma
cat("Optimal küme sayısı:", optimal_k, "\n")

# Grafiği çizme
plot(1:10, gap_result$gap, type="b", xlab="Number of clusters", ylab="Gap statistic")
points(optimal_k, gap_result$gap[optimal_k], col="red", pch=19)

#--- Kmeans the end




# Hiyerarşik kümeleme uygulama
hc <- hclust(dist(data))
plot(hc)

# Belirli bir küme sayısına kadar kümeleme
k <- 3
clusters <- cutree(hc, k)

# Küme atamalarını görselleştirme
plot(data, col=clusters)




# Hiyerarşik kümeleme uygulama ve optimal küme sayısını belirleme
# Hiyerarşik kümeleme uygulama
hc <- hclust(dist(data))

# WCSS değerlerini hesaplayarak Elbow Method'u uygulama
wss <- rep(0, 10)
for (i in 1:10) {
  wss[i] <- sum((hclust(dist(data))$height)^2)
}

# Optimal küme sayısını bulma
optimal_k <- which(diff(wss) == max(diff(wss)))

# Grafiği çizme
plot(1:10, wss, type="b", xlab="Number of clusters", ylab="Within-cluster sum of squares")
points(optimal_k, wss[optimal_k], col="red", pch=19)
text(optimal_k, wss[optimal_k], labels = c(optimal_k), pos = 3)



# Gap İstatistiği fonksiyonu tanımlama
gap_stat <- function(data, k.max = 10, B = 50) {
  gap <- vector(length = k.max)
  maxgap <- vector(length = k.max)
  for (k in 1:k.max) {
    gap[k] <- sum(abs(hclust(dist(data))$height[1:(k - 1)] - hclust(dist(data))$height[k])) / B
    maxgap[k] <- max(t(sapply(1:B, function(i) {
      cl <- cutree(hclust(dist(data)), k = k)
      cl2 <- sample(cl)
      sum(abs(hclust(dist(data))$height[1:(k - 1)] - hclust(dist(data))$height[cl == cl2][k]))
    })))
  }
  list(gap = gap, maxgap = maxgap)
}

# Gap İstatistiği uygulama
gap_result <- gap_stat(data)

# Optimal küme sayısını bulma
optimal_k <- which(gap_result$gap >= gap_result$maxgap)

# Grafiği çizme
plot(1:length(gap_result$gap), gap_result$gap, type="b", xlab="Number of clusters", ylab="Gap statistic")
points(optimal_k, gap_result$gap[optimal_k], col="red", pch=19)
text(optimal_k, gap_result$gap[optimal_k], labels = c(optimal_k), pos = 3)


#--- HCCLUST the end


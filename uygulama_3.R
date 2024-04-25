# Birliktelik kuralı - Association rule

# arules paketini yükleyin
install.packages("arules")
library(arules)

#---kod-1

# Veri setini oluşturun
transactions <- list(
  c("ekmek", "süt", "yumurta"),
  c("ekmek", "peynir", "yoğurt"),
  c("ekmek", "süt"),
  c("peynir", "yoğurt"),
  c("ekmek", "süt", "peynir"),
  c("ekmek", "süt", "peynir", "yoğurt"),
  c("süt", "yoğurt")
)

# Veri setini uygun formata dönüştürün
transactions <- as(transactions, "transactions")

# Apriori algoritmasını kullanarak birliktelik kurallarını çıkarın
rules <- apriori(transactions, parameter = list(support = 0.3, confidence = 0.8))

# Elde edilen kuralları görüntüleyin
inspect(rules)

#---diğer ilkel örnek
#---kod-2

# Örnek bir veri seti oluşturalım (alışveriş sepeti verisi)
transactions <- list(
  c("kahve", "şeker", "süt"),
  c("kahve", "çay", "bisküvi"),
  c("çay", "süt", "bisküvi"),
  c("kahve", "şeker", "süt"),
  c("kahve", "çay", "bisküvi")
)

# Veri setini uygun formata dönüştürelim
transactions <- as(transactions, "transactions")

# Birliktelik kuralını çıkaralım
rules <- apriori(transactions, parameter = list(support = 0.2, confidence = 0.8))

# Elde edilen kuralları görüntüleyelim
inspect(rules)

#--- kod-3 , FP-Growth ile çözüm



# transaction dataset üretimi
transactions <- as(sapply(1:10000, function(i) sample(letters[1:10], sample(3:6, 1))), "transactions")

# İlk birkaç işlemi inceleyin
inspect(transactions[1:5])

# FP-Büyüme algoritmasını çalıştır
frequent_itemsets <- fpgrowth(transactions, support = 0.1, verbose = TRUE)
frequent_itemsets <- apriori(transactions, parameter = list(support = 0.1, confidence = 0.8))
# Sık görülen öğe kümelerini inceleyin
inspect(frequent_itemsets)

# Birliktelik kurallarını sık kullanılan öğe kümelerinden de çıkarabilirsiniz
association_rules <- as(rules(frequent_itemsets), "data.frame")

# Birliktelik kurallarını inceleyin
head(association_rules)

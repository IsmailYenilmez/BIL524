# bu kod positte yazılmıştı dolayısı ile 
#working direktör olarak alacağınız çıktı:
# [1] "/cloud/project"
getwd()


# R'de basit bir matematiksel kod temel fonksiyonlar ile yazılmıştır
küp <- function(x) {
  return(x^3)
}

print(küp(5)) # Küp alma işlemi


# R'de basit bir diğer matematiksel kod temel fonksiyonlar ile yazılmıştır

gerisayım <- function(den)
{
  print(den)
  while(den!=0)
  {
    Sys.sleep(1)
    den <- den - 1
    print(den)
  }
}

gerisayım(10) # geri sayım fonksiyonu oluşturma işlemi

#rm() ile daha önceki environment ı temizleyebilirsiniz:
#rm(list=ls())

#ayrıca console u ctrl+l ile silebilirsiniz

#yazdığınız her satırı ise ctrl+enter ile çalıştırabilirsiniz.
#Tüm scripti çalıştırmak için (run) ise alt+enter kullanılabilir.

# verinin çekilmesi linkten:
veri =  read.csv("http://johnmuschelli.com/intro_to_r/data/Youth_Tobacco_Survey_YTS_Data.csv")
# verinin çekilmesi diskten (yönden dolayı hatalı):
veri2 = reas.csv("C:\Users\yenil\Downloads\Youth_Tobacco_Survey_YTS_Data.csv")
# verinin çekilmesi diskten:
veri3 = read.csv("C:/Users/yenil/Downloads/Youth_Tobacco_Survey_YTS_Data.csv")

#veriyi izleme ilk 6 satırı görme
View(veri)
head(veri)

  #diğer bir veri olarak built-in data olan mtcars örneği
  View(mtcars)

#bıyutlar x y olarak; x olarak; y olarak
dim(veri) 
nrow(veri) 
ncol(veri)

#yardım alma seçenekleri örneği
?dim 
help("dim")


#sütün yeniden adlandırma
library(dplyr) 
veri_rename = rename(veri, year = YEAR) 
names(veri_rename)


#çıkarma işlemi, en sonda yapılması sağlıklı olabilir. 
#write_delim(veri_rename, path, delim = " ", na = "NA", append = FALSE, col_names = !append)

library(readr)

veri_rename2 = rename(veri_rename, Year = year) 
veri_rename_orjinale_donus = rename(veri_rename2, YEAR = Year) 

#uygun path yazılarak aktarım
write_csv(veri_rename2, path = "YouthTobacco_newNames.csv")

#listeleme
ls()
??ls



library(tidyverse)

#paket içi kullanım
head(stats::filter,2)

#veri çekildi 
data(jhu_cars)
remotes::install_github("muschellij2/jhur")

#temel inceleme ve atama
data(mtcars)
View(mtcars)
head(mtcars) 

#tibble ile
veri2_tibble = as_tibble(mtcars)
head(veri2_tibble)
head(mtcars,3)
head(as_tibble(mtcars),3)

#yeniden adlandırma farklı şekilde
veri2_rename = dplyr::rename(veri2_tibble, MPG = mpg)

#hepsinin adını bir satırda büyütmek
veri2_upper = dplyr::rename_all(veri2_rename, toupper)

#veri incelemeleri ve adlandırmalar
veri2=data("mtcars")
View(veri2)
veri2=data(mtcars)
veri2=data.frame(mtcars)
View(veri2)
veri22=as_tibble(mtcars)
View(veri22)

#Basit bir filtre olarak $ 
veri22$carb

#seçim fonkdiyonu
select(veri22, mpg)

#pull ile select aynı anda bir satırda kullarak alt küme
pull(select(veri22, mpg))
select(veri22, mpg, cyl)

#koşulları sağlayan filtre örneği
filter(veri22, mpg > 20 | mpg < 14)

#koşullu ve filtreye ek select desteği ve çoklu seçim
select(filter(veri22, mpg > 20 & cyl == 4), cyl, hp)

#filtreleme
veri22_filtered=filter(veri22, mpg > 20 | mpg < 14)

#pipe "%>%" kullanımı 
veri22_piped= veri22 %>% filter(mpg > 20 & cyl == 4) %>% select(cyl, hp)

veri22_piped

#veriye işlem ile yeni sütunda tutma
veri22$newcol = veri22$wt/2.2

head(veri22)

#mutate ile yeni sütun
veri22_mut = mutate(veri22, newcol = wt/2.2)


#---mutate ve ifelse ile kategorik hale getirme
veri22_mut2 = mutate(veri22,
                     disp_cat = ifelse(
                       disp <= 200,
                       "Low",
                       ifelse(disp <= 400,
                              "Medium",
                              "High")
                     )
)


head(veri22_mut2$disp_cat)

#farklı şekilde atama 
veri22$newcol = NULL

#düşürme - çıkarma
select(veri22_mut2, -newcol)

select(veri22_mut2, -one_of("newcol", "drat"))

select(veri22_mut2, newcol, everything())

#arrange örnekleri
arrange(veri22, desc(mpg))
arrange(veri22, mpg, desc(hp))

#transmute örneği
transmute(veri22, newcol2 = wt/2.2, mpg, hp)

#sütun isim fonksiyonu ile düzenlemeler
colnames(veri22)
colnames(veri22)[1:3]=c("MPG", "CYL", "DISP")
colnames(veri22)

sutun_isim=colnames(veri22)
sutun_isim[sutun_isim == "drat"] = "DRAT"
head(veri22)

#bir satırı ve sütunu tamamiyle alma
veri22[,1] #tüm satırlar
veri22[1,] #tüm sütunlar

#temel istatistik fonksiyon örnekleri
mean(veri22$MPG)
quantile(veri22$CYL)
median(veri22$CYL)


file_path <- "C:/arasinav.txt"

veri_kitle <- read.table(file_path, header = TRUE, sep = " ")  
set.seed(123)  # Rastgelelik sabitlemek çin
satir_no <- sample(1:nrow(veri_kitle),size = 2000)
#vektor,size(boyut)

veri_orneklem <- veri_kitle[satir_no,]

veri <- veri_orneklem
veri[c("Occupation","City_Tier")] <- lapply(veri[c("Occupation","City_Tier")],as.factor)

names(veri)    #  Verinin içindeki değişkenleri görmek için kullanılır. 
attach(veri)

# Korelasyonu bulmak için vektör oluştur
yuksek_korelasyonlar <- c()

# Sütun isimlerini döngüyle gez
for (n in colnames(veri)) {
  if (n != "Income" && is.numeric(veri[[n]])) {  # "Income" sütunuyla kendini kıyaslama ve sayısal kontrol
    korelasyon <- cor(veri$Income, veri[[n]], use = "complete.obs")
    #If use is complete.obs then missing values are handled by casewise deletion 
    #(and if there are no complete cases, that gives an error)."
    if (korelasyon > 0.9) {  # 0.9'dan büyük olanları kontrol et
      yuksek_korelasyonlar <- c(yuksek_korelasyonlar, n)
    }
  }
}

# Sonuçları yazdır
print("Income ile 0.9'dan büyük korelasyona sahip değişkenler:")
print(yuksek_korelasyonlar)

par(mfrow = c(3, 5), mar = c(5, 5, 2, 2))  # Adjust margins
for(n in colnames(veri)){
  if(n != "Income"){
    plot(veri[[n]],Income ,xlab = n, ylab = "Income")
  }
}

par(mfrow = c(1,1))

# Gerekli değişkenlerin seçimi
predictors <- colnames(veri)

# Regresyona uygunluk için sayısallık kontrolü
valid_predictors <- predictors[sapply(veri[predictors], is.numeric)]

# Multicollinearity analizi (VIF)
library(car)  # VIF analizi için car paketi
model <- lm(Income ~ ., data = veri[, c("Income", valid_predictors)])  # İlk model
vif_values <- vif(model)

# Yüksek korelasyona sahip değişkenleri kaldır
final_predictors <- names(vif_values[vif_values < 10])

# Uygun değişkenler
print("Performing regression with these predictors:")
print(final_predictors)


# Model oluşturma
model <- lm(Income ~ Age + Dependents + Occupation + City_Tier + Rent + Loan_Repayment + Insurance + 
              Transport + Eating_Out + Entertainment + Healthcare + Education + Miscellaneous)
#lm = linear model
# y= 42023.96+30.29*Age-2667.28*OccupationRetired+....

summary(model)

#Bölüm 11.6 -income ile çok fazla korelasyon olmuşlar ile Multiple Linear Regression modeli
library(dplyr)
library(purrr)

"%>%: Pipe (boru) operatörü, bir işlemin sonucunu bir sonraki işleme giriş olarak aktarmaya yarar. 
Bu, kodu daha okunabilir hale getirir."

best_pred <- veri %>%
  select(-Income & where(is.numeric)) %>%  # Income Sütunu alınmayacak ve sayısal sutunlar alır
  map_dbl(cor, y = Income) %>%  # Korelasyon hesaplama
  #we use map_dbl from purrr to perform a pairwise correlation on each column relative 
  #to the response variable:
  sort(decreasing = TRUE) %>%  # Azalan sırayla sırala
  .[1:4] %>%  # En yüksek 4 korelasyona sahip olan sütunları seç
  names() %>%  # Bu sütunların adlarını al
  veri[.]  # Orijinal veri çerçevesinden bu sütunları seç


mod <- lm(Income ~ as.matrix(best_pred))

summary(mod)


#11.8 Selecting the Best Regression Variables

full.model <- lm(Income ~ Age+Dependents+Occupation+City_Tier+Rent+Loan_Repayment+Insurance+Groceries+Transport+Eating_Out+Entertainment+Utilities+Healthcare+Education+Miscellaneous)
#Income ile tüm değişkenler arasında bir model kurduk bu full modeldir

reduced.model <- step(full.model, direction = "backward")
#step ile adım adım regresyon yapıcağız ve backwards ile kötü performanslı olanları çıkartacağız.

summary(reduced.model) #* ile başladı
# The summary of the reduced model shows that it contains only significant predictors:

min.model <- lm(Income ~ 1) #modeli hiçbirşeyden başlatalım

"This is a model with a response variable (Income) but no predictor variables. 
(All the fitted values for Income are simply the mean of Income, which is what you would guess 
if no predictors were available.)"

"In that case use forward stepwise regression, which will start with nothing and incrementally 
add variables that improve the regression. It stops when no further improvement is possible."

fwd.model <- step(
  min.model,
  direction = "forward",
  scope = (~ Age+Dependents+Occupation+City_Tier+Rent+Loan_Repayment+Insurance+Groceries+Transport+Eating_Out+Entertainment+Utilities+Healthcare+Education+Miscellaneous),
  # scope is a formula with nothing on the lefthand side of the tilde (~) and candidate variables 
  #on the righthand side:
  trace = 0
  # (We also included trace = 0 to inhibit the voluminous output from step.)
)

summary(fwd.model) #*** yıldızlar başta

#11.9 Regressing on a Subset of Your Data

sorted_rent <- sort(Rent)

dusuk_kira_model <- lm(Income ~ sorted_rent, subset = 1:floor(length(sorted_rent) / 2))
#sorted_rent yarısına göre lm yap
summary(dusuk_kira_model)

#11.14 Forming Confidence Intervals for Regression Coefficients

#bizim  Best Regression Variables deki lm nin regresyon katsayıları için güven aralıkları bulalım

confint(fwd.model) #default olarak %95 güven aralığı kullanıyor yani level = 0.95

#11.22 Creating an Interaction Plot
"You are performing multiway ANOVA: using two or more categorical variables as predictors. 
You want a visual check of possible interaction between the predictors."

interaction.plot(Occupation,City_Tier,Income,col = c("red", "blue", "green"))
"Occupation,City_Tier iki kategorik veri Income : response variable(bağımlı değişken)"

"
ANOVA is a form of linear regression, so ideally there is a linear relationship between every 
predictor and the response variable. One source of nonlinearity is an interaction between 
two predictors: as one predictor changes value, the other predictor changes its relationship 
to the response variable. Checking for interaction between predictors is a basic diagnostic.
"

"grafiğe göre meslek ve gelir arasındaki ilişki City_tier e de bağlıdır "

#11.23 Finding Differences Between Means of Groups

# Kolmogorov-Smirnov testi

# Sayısal sütunları seçme (sadece sayısal veri içeren sütunlar)
numeric_columns <- sapply(veri, is.numeric)

# Test sonuçlarını saklayacak bir vektör
normal_columns <- c()

# Her sayısal sütun için Kolmogorov-Smirnov testi uygulama
for (col_name in names(veri)[numeric_columns]) {
  # Kolmogorov-Smirkov testi
  ks_test <- ks.test(veri[[col_name]], "pnorm", mean(veri[[col_name]], na.rm = TRUE), sd(veri[[col_name]], na.rm = TRUE))
  
  # p-değeri kontrolü, normal dağılıma uygun olan sütunları kaydetme
  if (ks_test$p.value > 0.05) {
    normal_columns <- c(normal_columns, col_name)
  }
}

# Normal dağılıma uygun sütunları yazdırma
normal_columns  #hiçbiri normal değil


#verimizde değişkenler NORMAL DEĞİL 


# Sayısal sütunlara log dönüşümü uygula ve yeni sütun isimleriyle sakla
log_transformed_data <- veri %>%
  select(where(is.numeric)) %>%           # Sadece sayısal sütunları seç
  mutate(across(everything(), ~ log(ifelse(. <= 0, NA, . + 1)), .names = "Log_{.col}"))
# Log dönüşümü yap              

# Test sonuçlarını saklayacak bir vektör
normal_columns_log <- c()
numeric_columns_log <- sapply(log_transformed_data, is.numeric)
# Her sayısal sütun için Kolmogorov-Smirnov testi uygulama
for (col_name in names(log_transformed_data)[numeric_columns]) {
  # Kolmogorov-Smirkov testi
  ks_test <- ks.test(log_transformed_data[[col_name]], "pnorm", mean(log_transformed_data[[col_name]], na.rm = TRUE), sd(log_transformed_data[[col_name]], na.rm = TRUE))
  
  # p-değeri kontrolü, normal dağılıma uygun olan sütunları kaydetme
  if (ks_test$p.value > 0.05) {
    normal_columns_log <- c(normal_columns_log, col_name)
  }
}

# Normal dağılıma uygun sütunları yazdırma
normal_columns_log  #hiçbiri normal değil



library(car)
leveneTest(log_transformed_data$Log_Eating_Out ~ Occupation)  #varyanslar homojen
leveneTest(log_transformed_data$Log_Income ~ Occupation) 

#anova F testi kullanır normallik ve varyansların homojenliği varsayımları altında yapılır.
anova_model <- aov(log_transformed_data$Log_Eating_Out ~ Occupation)
"                  bağımlı değişken sayısal , bağımsız değişken kategorik"                 
TukeyHSD(anova_model)

anova_model2 <- aov(log_transformed_data$Log_Income ~ Occupation)
TukeyHSD(anova_model2)
"
H0 = Mu1-Mu2 = 0
H1 = Mu1-Mu2 != 0

Tier_2-Tier_1:

Ortalama fark: -121.61 (Tier_2, Tier_1'den daha düşük bir ortalamaya sahip).
Güven aralığı: [-283.89, 40.66] (fark, sıfır değerini içeriyor).
p-değeri: 0.1843 (anlamlı değil).
H0 kabul

Tier_3-Tier_1:

Ortalama fark: 10.18 (Tier_3, Tier_1'den biraz daha yüksek, ancak fark küçük).
Güven aralığı: [-187.62, 207.99] (fark, sıfırı içeriyor).
p-değeri: 0.9920 (anlamlı değil).
H0 kabul

Tier_3-Tier_2:

Ortalama fark: 131.80 (Tier_3, Tier_2'den daha yüksek).
Güven aralığı: [-48.55, 312.15] (fark, sıfırı içeriyor).
p-değeri: 0.2001 (anlamlı değil).
H0 kabul

Bu analiz, City_Tier (Tier_1, Tier_2, Tier_3) gruplarının dışarıda yemek yeme davranışı 
(Eating_Out) üzerinde belirgin bir etkisi olmadığını göstermektedir. Eğer bu farklar önemli olsaydı, 
p-değerlerinden biri 0.05'in altında olurdu ve güven aralığı sıfırı içermezdi.
"
plot(TukeyHSD(anova_model))

#11.24 Performing Robust ANOVA (Kruskal–Wallis Test)

"Your data is divided into groups. The groups are *not* normally distributed, 
but their distributions have similar shapes. You want to perform a test similar 
to ANOVA—you want to know if the group medians are significantly different.
"

#bağımsız K örneklem konum testi
"
verimizdeki değerler normal olmadığından dolayı non-parametrik test uygulayacağız
"

#aynı yukardaki testi non-parametrik test ile çözelim
kruskal.test(Eating_Out ~ City_Tier)

"Sonuç:
Kruskal-Wallis chi-squared = 3.5882: Bu, test istatistiğidir. Kruskal-Wallis testi, bağımsız gruplar 
arasında sıralı farkların olup olmadığını test eder.
df = 2: Serbestlik derecesi (df), grupların sayısından bir eksik olan değeri ifade eder. 
Burada 3 grup (City_Tier) olduğundan df = 3 - 1 = 2 olmuştur.
p-value = 0.1663: Bu, testin p-değeridir. p-değeri 0.05'ten büyük olduğu için, gruplar arasında 
istatistiksel olarak anlamlı bir fark bulunmamaktadır.

Yorum:
Null Hipotez (H₀): City_Tier grupları arasında eşit sıralı medyanlar vardır (yani gruplar arasında
anlamlı bir fark yoktur).
Alternatif Hipotez (H₁): City_Tier grupları arasında eşit olmayan sıralı medyanlar vardır 
(yani gruplar arasında anlamlı bir fark vardır).
Çünkü p-değeri 0.1663 olup 0.05'ten büyük olduğu için, null hipotezi reddedilemiyor. 
Yani, City_Tier grupları arasında istatiksel olarak anlamlı bir fark bulunmamaktadır.

Sonuç:

Bu sonuç, Eating_Out değişkeninin City_Tier grupları arasında anlamlı bir fark oluşturmadığını 
gösterir. Gruplar arasında sıralı farkların önemli olmadığı sonucuna varılabilir.
"



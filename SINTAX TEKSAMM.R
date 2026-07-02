#=========================================================
# Estimasi Tingkat Resiliensi Akademik 
# TWO STAGE CLUSTER SAMPLING
#=========================================================

#=========================================================
# MEMANGGIL PACKAGE
#=========================================================

library(readxl)
library(psych)
library(survey)

#=========================================================
# IMPORT DATA
#=========================================================

data <- read_excel("C:/Users/MyBook Hype AMD/Downloads/TEKSAM UAS.xlsx")

View(data)

#=========================================================
# ITEM PERTANYAAN
#=========================================================

item <- c("P1","P2","P3","P4","P5",
          "P6","P7","P8","P9","P10")

#=========================================================
# UBAH KE NUMERIC
#=========================================================

data[item] <- lapply(data[item],
                     function(x) as.numeric(as.character(x)))

#=========================================================
# UJI VALIDITAS
#=========================================================

data$TOTAL <- rowSums(data[item])

hasil_validitas <- data.frame()

for(i in item){
  
  total_koreksi <- data$TOTAL - data[[i]]
  
  uji <- cor.test(data[[i]],
                  total_koreksi,
                  method="pearson")
  
  hasil_validitas <- rbind(
    hasil_validitas,
    data.frame(
      Item=i,
      r_hitung=round(uji$estimate,3),
      p_value=round(uji$p.value,4)
    )
  )
  
}

r_tabel <- 0.361

hasil_validitas$Keputusan <-
  ifelse(hasil_validitas$r_hitung>r_tabel,
         "Valid",
         "Tidak Valid")

hasil_validitas

#=========================================================
# UJI RELIABILITAS
#=========================================================

hasil_alpha <- alpha(data[item])

hasil_alpha

hasil_alpha$total$raw_alpha

#=========================================================
# MENGHITUNG SKOR RATA-RATA
#=========================================================

data$Skor <- rowMeans(data[item])

#=========================================================
# STATISTIK DESKRIPTIF
#=========================================================

summary(data$Skor)

mean(data$Skor)

sd(data$Skor)

min(data$Skor)

max(data$Skor)

#=========================================================
# FREKUENSI RESPONDEN
#=========================================================

table(data$KELAS)

table(data$ANGKATAN)

table(data$JK)

#=========================================================
# INFORMASI SAMPLING
#=========================================================

M <- 6
m <- 2

NB25 <- 31
NB24 <- 28

nB25 <- 22
nB24 <- 8

#=========================================================
# PELUANG PEMILIHAN
#=========================================================

# Peluang Tahap 1
P1 <- 2/6

# Peluang Tahap 2
P24 <- 8/28
P25 <- 22/31

# Peluang Terpilih Keseluruhan
Pi24 <- P1 * P24
Pi25 <- P1 * P25

# Tampilkan Hasil
Pi24
Pi25
#=========================================================

Bobot24 <- 1/Pi24
Bobot25 <- 1/Pi25

Bobot24
Bobot25

#=========================================================
# RESPONSE RATE
#=========================================================

Target <- 30

ResponseRate <- nrow(data)/Target

#=========================================================
# BOBOT AKHIR
#=========================================================
data$Bobot_Akhir <- ifelse(
  data$KELAS == "24 B",
  Bobot24/ResponseRate,
  Bobot25/ResponseRate
)

#=========================================================
# DESAIN SURVEI
#=========================================================

design_cluster <-
  svydesign(
    id=~KELAS,
    weights=~Bobot_Akhir,
    data=data
  )

#=========================================================
# ESTIMASI RATA-RATA
#=========================================================

mean_kb <-
  svymean(~Skor,
          design_cluster)

mean_kb

#=========================================================
# STANDARD ERROR
#=========================================================

SE(mean_kb)

#=========================================================
# CONFIDENCE INTERVAL
#=========================================================

confint(mean_kb)

#=========================================================
# RELATIVE STANDARD ERROR
#=========================================================

RSE <-
  (SE(mean_kb)/coef(mean_kb))*100

RSE

cat("Nilai RSE =",
    round(RSE,2),
    "%/n")

#=========================================================
# DESIGN EFFECT
#=========================================================

svymean(~Skor,
        design_cluster,
        deff=TRUE)

#=========================================================
# ESTIMASI PER CLUSTER
#=========================================================

svyby(~Skor,
      ~KELAS,
      design_cluster,
      svymean)

#=========================================================
# KATEGORI TINGKAT
#=========================================================

data$Kategori <- cut(
  data$Skor,
  breaks=c(1,1.75,2.50,3.25,4),
  labels=c("Sangat Rendah",
           "Rendah",
           "Tinggi",
           "Sangat Tinggi"),
  include.lowest=TRUE)

design_cluster <- svydesign(
  id = ~KELAS,
  weights = ~Bobot_Akhir,
  data = data
)

svytable(~Kategori, design_cluster)

prop.table(svytable(~Kategori, design_cluster))

#=========================================================
# ESTIMASI KATEGORI
#=========================================================

svytable(~Kategori,
         design_cluster)

prop.table(
  svytable(~Kategori,
           design_cluster))
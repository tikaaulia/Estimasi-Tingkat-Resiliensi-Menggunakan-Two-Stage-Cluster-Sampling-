Estimasi Tingkat Resiliensi Akademik Mahasiswa Program Studi Statistika Universitas Mataram Menggunakan Two-Stage Cluster Sampling

DESKRIPSI PEROYEK
Penelitian ini bertujuan untuk mengestimasi tingkat resiliensi akademik mahasiswa Program Studi Statistika Universitas Mataram menggunakan metode Two-Stage Cluster Sampling. Resiliensi akademik merupakan kemampuan mahasiswa dalam menghadapi, beradaptasi, dan bangkit kembali dari berbagai tantangan atau tekanan selama proses pembelajaran di perguruan tinggi.

Pengumpulan data dilakukan menggunakan kuesioner skala Likert yang terdiri atas 10 butir pertanyaan yang mewakili lima indikator resiliensi akademik. Sampel penelitian dipilih melalui dua tahap, yaitu pemilihan cluster (kelas) secara acak, kemudian pemilihan mahasiswa secara acak pada setiap cluster terpilih.

Analisis data dilakukan menggunakan perangkat lunak R dengan bantuan package readxl, psych, dan survey. Tahapan analisis meliputi uji validitas, uji reliabilitas, statistik deskriptif, perhitungan peluang pemilihan dan bobot sampel, pembentukan desain survei, serta estimasi rata-rata tingkat resiliensi akademik beserta ukuran ketelitiannya, seperti Standard Error (SE), Confidence Interval (CI), Relative Standard Error (RSE), dan Design Effect (DEFF).

Hasil penelitian diharapkan dapat memberikan gambaran mengenai tingkat resiliensi akademik mahasiswa Program Studi Statistika Universitas Mataram serta menjadi informasi yang bermanfaat bagi program studi dalam menyusun kebijakan atau program yang mendukung peningkatan kemampuan mahasiswa dalam menghadapi tantangan akademik.

STRUKTUR RIPOSTORY
├── Data/
│   └── DATA TEKSAM.xlsx
│
├── Script/
│   └── Analisis_R.R
│
├── Output/
│   ├── Hasil Validitas.csv
│   ├── Hasil Reliabilitas.csv
│   ├── Estimasi.csv
│   └── Grafik.png
│
├── README.md
│
└── Laporan.pdf

TUJUAN PENELITIAN
Mengestimasi tingkat resiliensi akademik mahasiswa Program Studi Statistika Universitas Mataram.
Menghitung estimasi rata-rata tingkat resiliensi akademik menggunakan metode Two-Stage Cluster Sampling.
Menghitung ukuran ketelitian estimasi berupa Standard Error (SE), Confidence Interval (CI), Relative Standard Error (RSE), dan Design Effect (DEFF).
Memberikan gambaran mengenai kondisi resiliensi akademik mahasiswa sebagai bahan evaluasi bagi Program Studi.

METODOLOGI PENELITIAN 
Penelitian ini menggunakan pendekatan kuantitatif dengan metode survei untuk mengumpulkan data mengenai resiliensi akademik mahasiswa Program Studi Statistika Universitas Mataram. Data diperoleh melalui penyebaran kuesioner menggunakan skala Likert dengan empat pilihan jawaban.

Teknik pengambilan sampel yang digunakan adalah Two-Stage Cluster Sampling. Pada tahap pertama, populasi yang terdiri atas enam kelas (kelas A dan B dari tiga angkatan) dikelompokkan sebagai cluster, kemudian dipilih dua cluster secara acak. Pada tahap kedua, dilakukan pemilihan mahasiswa secara acak dari masing-masing cluster terpilih hingga diperoleh 30 responden sebagai sampel penelitian.

Data yang telah terkumpul dianalisis menggunakan bahasa pemrograman R dengan bantuan package readxl, psych, dan survey. Tahapan analisis meliputi uji validitas, uji reliabilitas, statistik deskriptif, perhitungan peluang pemilihan sampel, pembobotan, pembentukan desain survei, serta estimasi tingkat resiliensi akademik menggunakan metode Two-Stage Cluster Sampling. Hasil estimasi kemudian dievaluasi melalui Standard Error (SE), Confidence Interval (CI), Relative Standard Error (RSE), dan Design Effect (DEFF) untuk menilai ketelitian hasil estimasi.

ANALISIS DATA
Analisis data pada penelitian ini dilakukan menggunakan bahasa pemrograman R. Tahapan analisis yang dilakukan adalah sebagai berikut.
1. Import Data
Tahap ini bertujuan untuk membaca data hasil kuesioner yang tersimpan dalam file Microsoft Excel ke dalam lingkungan kerja R. Selain itu, package yang dibutuhkan dipanggil agar seluruh fungsi yang digunakan dalam analisis dapat berjalan dengan baik.
library(readxl)
library(psych)
library(survey)
data <- read_excel("C:/Users/MyBook Hype AMD/Downloads/TEKSAM UAS.xlsx")
View(data) Keterangan:Package readxl digunakan untuk membaca file Microsoft Excel, psych digunakan untuk melakukan uji validitas dan uji reliabilitas instrumen penelitian, sedangkan survey digunakan untuk melakukan analisis data survei menggunakan metode Two-Stage Cluster Sampling. Selanjutnya, fungsi read_excel() digunakan untuk mengimpor data hasil kuesioner ke dalam R, sedangkan fungsi View() digunakan untuk menampilkan data dalam bentuk spreadsheet sehingga memudahkan pemeriksaan awal terhadap data yang akan dianalisis.
2. Persiapan Data
Tahap ini bertujuan untuk menentukan variabel yang digunakan sebagai indikator dalam mengukur tingkat resiliensi akademik mahasiswa.
item <- c("P1","P2","P3","P4","P5",
          "P6","P7","P8","P9","P10") Keterangan: Objek item berisi daftar nama variabel P1 sampai P10 yang merupakan butir pertanyaan dalam kuesioner penelitian. Penyimpanan nama variabel ke dalam satu objek bertujuan untuk mempermudah proses pengolahan data karena seluruh item dapat dipanggil secara bersamaan pada setiap tahapan analisis.
3. Mengubah Data Menjadi Numerik
Tahap ini bertujuan untuk mengubah tipe data seluruh item menjadi numerik agar dapat dilakukan analisis statistik.data[item] <- lapply(data[item],
                     function(x) as.numeric(as.character(x))) Keterangan:Perintah data[item] digunakan untuk memilih seluruh variabel P1 sampai P10. Fungsi lapply() digunakan untuk mengubah seluruh item secara otomatis, sedangkan as.character() mengubah data menjadi karakter terlebih dahulu sebelum dikonversi menjadi numerik menggunakan as.numeric(). Proses ini dilakukan agar seluruh data dapat diolah menggunakan analisis statistik di dalam R.
4. Uji Validitas
Uji validitas dilakukan untuk mengetahui apakah setiap butir pertanyaan mampu mengukur tingkat resiliensi akademik mahasiswa secara tepat.
Kode data$TOTAL <- rowSums(data[item])

hasil_validitas <- data.frame()

for(i in item){

  total_koreksi <- data$TOTAL - data[[i]]

  uji <- cor.test(
    data[[i]],
    total_koreksi,
    method="pearson"
  )

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
ifelse(
hasil_validitas$r_hitung>r_tabel,
"Valid",
"Tidak Valid")
hasil_validitas
Keterangan:Perintah rowSums() digunakan untuk menghitung jumlah skor seluruh item untuk setiap responden. Objek hasil_validitas digunakan untuk menyimpan hasil uji validitas, sedangkan perulangan for() digunakan untuk menghitung korelasi setiap item dari P1 sampai P10. Variabel total_koreksi merupakan skor total yang telah dikurangi skor item yang sedang diuji sehingga menghasilkan Corrected Item-Total Correlation. Selanjutnya, fungsi cor.test() digunakan untuk menghitung korelasi Pearson antara skor item dengan skor total terkoreksi. Hasil pengujian kemudian disimpan ke dalam tabel menggunakan rbind(). Keputusan validitas ditentukan dengan membandingkan nilai r hitung dengan r tabel sebesar 0,361, sehingga item dinyatakan valid apabila nilai r hitung lebih besar daripada r tabel.
5. Uji Reliabilitas
Uji reliabilitas dilakukan untuk mengetahui tingkat konsistensi instrumen penelitian dalam mengukur tingkat resiliensi akademik mahasiswa.
hasil_alpha <- alpha(data[item])

hasil_alpha
hasil_alpha$total$raw_alpha
Keterangan:Fungsi alpha() dari package psych digunakan untuk menghitung nilai Cronbach's Alpha sebagai ukuran reliabilitas instrumen penelitian. Nilai Cronbach's Alpha yang dihasilkan menunjukkan tingkat konsistensi antarbutir pertanyaan dalam mengukur resiliensi akademik mahasiswa. Semakin tinggi nilai Cronbach's Alpha, maka semakin baik tingkat reliabilitas instrumen. Nilai tersebut ditampilkan melalui hasil_alpha$total$raw_alpha sebagai dasar dalam menentukan apakah kuesioner layak digunakan pada tahap analisis selanjutnya.
6. Menghitung Skor Resiliensi Akademik
Tahap ini bertujuan untuk menghitung skor resiliensi akademik setiap responden berdasarkan jawaban pada 10 butir pertanyaan yang telah diisi pada kuesioner. data$Skor <- rowMeans(data[item]) Keterangan:Fungsi rowMeans() digunakan untuk menghitung nilai rata-rata dari seluruh butir pertanyaan (P1 sampai P10) pada setiap responden. Nilai rata-rata tersebut disimpan ke dalam variabel Skor yang merepresentasikan skor resiliensi akademik masing-masing responden. Skor ini selanjutnya digunakan sebagai variabel utama dalam analisis statistik deskriptif, pembentukan desain survei, serta proses estimasi tingkat resiliensi akademik menggunakan metode Two-Stage Cluster Sampling.
7. Statistik Deskriptif
Tahap ini bertujuan untuk memberikan gambaran umum mengenai data resiliensi akademik responden. summary(data$Skor)
mean(data$Skor)
sd(data$Skor)
min(data$Skor)
max(data$Skor)
Keterangan: Fungsi summary() digunakan untuk menampilkan ringkasan statistik yang meliputi nilai minimum, kuartil, median, rata-rata, dan maksimum. Fungsi mean() digunakan untuk menghitung rata-rata skor resiliensi akademik, sd() digunakan untuk menghitung standar deviasi, sedangkan min() dan max() digunakan untuk mengetahui nilai terkecil dan terbesar dari skor responden.
8. Frekuensi Responden
Tahap ini bertujuan untuk mengetahui distribusi responden berdasarkan karakteristik penelitian.
table(data$KELAS)
table(data$ANGKATAN)
table(data$JK)
Keterangan: Fungsi table() digunakan untuk menghitung jumlah responden berdasarkan kelas, angkatan, dan jenis kelamin sehingga diperoleh gambaran karakteristik responden yang menjadi sampel penelitian.
9. Menentukan Informasi Sampling
Tahap ini bertujuan untuk menentukan informasi dasar yang digunakan dalam perhitungan peluang pemilihan sampel. M <- 6
m <- 2
NB25 <- 31
NB24 <- 28
nB25 <- 22
nB24 <- 8 Keterangan:Variabel M menyatakan jumlah seluruh cluster pada populasi, sedangkan m merupakan jumlah cluster yang dipilih. Variabel NB25 dan NB24 menunjukkan jumlah populasi pada masing-masing cluster terpilih, sedangkan nB25 dan nB24 merupakan jumlah responden yang dijadikan sampel pada setiap cluster.
10. Menghitung Peluang Pemilihan Sampel
Tahap ini bertujuan untuk menghitung peluang terpilihnya setiap responden berdasarkan desain Two-Stage Cluster Sampling. 
P1 <- 2/6
P24 <- 8/28
P25 <- 22/31
Pi24 <- P1 * P24
Pi25 <- P1 * P25
Pi24
Pi25
Keterangan:Nilai P1 merupakan peluang pemilihan cluster pada tahap pertama. Nilai P24 dan P25 merupakan peluang pemilihan responden pada tahap kedua untuk masing-masing cluster. Selanjutnya, Pi24 dan Pi25 merupakan peluang keseluruhan responden terpilih yang diperoleh dari hasil perkalian peluang tahap pertama dan tahap kedua.
11. Menghitung Bobot Sampel
Tahap ini bertujuan untuk menghitung bobot dasar dan bobot akhir yang digunakan pada proses estimasi.
Bobot24 <- 1/Pi24
Bobot25 <- 1/Pi25
Target <- 30
ResponseRate <- nrow(data)/Target
data$Bobot_Akhir <- ifelse(
data$KELAS=="24 B",
Bobot24/ResponseRate,
Keterangan: Bobot dasar diperoleh dari kebalikan peluang pemilihan responden. Selanjutnya dilakukan penyesuaian terhadap response rate sehingga diperoleh bobot akhir yang akan digunakan dalam proses estimasi menggunakan desain survei.
Bobot25/ResponseRate)
12. Membentuk Desain Survei dan Mengestimasi Rata-rata
Tahap ini bertujuan untuk membentuk desain survei berdasarkan metode Two-Stage Cluster Sampling dan menghitung estimasi rata-rata resiliensi akademik.
design_cluster <-
svydesign(
id=~KELAS,
weights=~Bobot_Akhir,
data=data)
mean_kb <-
svymean(~Skor,
design_cluster)
mean_kb
Keterangan: Fungsi svydesign() digunakan untuk membentuk desain survei dengan memasukkan informasi cluster dan bobot sampel. Selanjutnya, fungsi svymean() digunakan untuk mengestimasi rata-rata tingkat resiliensi akademik berdasarkan desain sampling yang digunakan.

HASIL DAN PEMBAHASAN 
4.1 Uji Validitas
Uji validitas dilakukan untuk mengetahui apakah setiap butir pertanyaan mampu mengukur variabel resiliensi akademik mahasiswa. Pengujian dilakukan menggunakan korelasi Pearson antara skor setiap item dengan skor total terkoreksi. Item dinyatakan valid apabila nilai r hitung > r tabel (0,361).
| Item | r hitung | r tabel | Keputusan |
| ---- | -------: | ------: | --------- |
| P1   |    0,468 |   0,361 | Valid     |
| P2   |    0,407 |   0,361 | Valid     |
| P3   |    0,472 |   0,361 | Valid     |
| P4   |    0,733 |   0,361 | Valid     |
| P5   |    0,780 |   0,361 | Valid     |
| P6   |    0,603 |   0,361 | Valid     |
| P7   |    0,657 |   0,361 | Valid     |
| P8   |    0,581 |   0,361 | Valid     |
| P9   |    0,708 |   0,361 | Valid     |
| P10  |    0,692 |   0,361 | Valid     |

Interpretasi
Berdasarkan Tabel 4.1 diketahui bahwa seluruh butir pertanyaan memiliki nilai r hitung lebih besar daripada r tabel (0,361). Dengan demikian seluruh item pertanyaan dinyatakan valid, sehingga mampu mengukur variabel resiliensi akademik mahasiswa dengan baik dan layak digunakan dalam penelitian.
4.2 Uji Reliabilitas
Uji reliabilitas dilakukan menggunakan metode Cronbach's Alpha untuk mengetahui tingkat konsistensi instrumen penelitian.

| Cronbach's Alpha | Kriteria        | Kesimpulan |
| ---------------: | --------------- | ---------- |
|            0,876 | Sangat Reliabel | Reliabel   |

Interpretasi
Hasil uji reliabilitas menunjukkan nilai Cronbach's Alpha sebesar 0,876. Nilai tersebut lebih besar dari 0,70 sehingga instrumen penelitian memiliki tingkat konsistensi yang sangat baik. Dengan demikian kuesioner yang digunakan dinyatakan reliabel dan layak digunakan untuk mengukur resiliensi akademik mahasiswa.
 4.3 Statistik Deskriptif Skor Resiliensi Akademik
Statistik deskriptif digunakan untuk memberikan gambaran umum mengenai skor resiliensi akademik responden.
 Tabel 4.3 Statistik Deskriptip
| Statistik       | Nilai |
| --------------- | ----: |
| Minimum         | 2,500 |
| Maksimum        | 4,000 |
| Rata-rata       | 3,393 |
| Standar Deviasi | 0,443 |
Interpretasi
Berdasarkan Tabel 4.3 diperoleh rata-rata skor resiliensi akademik sebesar 3,393 dengan standar deviasi sebesar 0,443. Nilai minimum sebesar 2,500 dan maksimum sebesar 4,000 menunjukkan bahwa tingkat resiliensi akademik mahasiswa berada pada rentang kategori sedang hingga sangat tinggi. Standar deviasi yang relatif kecil menunjukkan bahwa skor responden tidak terlalu bervariasi sehingga tingkat resiliensi akademik antarresponden relatif homogen.
4.4 Estimasi Tingkat Resiliensi Akademik Menggunakan Two-Stage Cluster Sampling
Estimasi dilakukan menggunakan metode Two-Stage Cluster Sampling dengan memperhitungkan bobot sampling pada setiap cluster.
 Tabel 4.4 Hasil Estimasi
| Parameter               |           Nilai |
| ----------------------- | --------------: |
| Mean                    |           3,476 |
| Standard Error          |           0,168 |
| Confidence Interval 95% | (3,146 ; 3,806) |
| Relative Standard Error |           4,85% |
| Design Effect           |           5,604 |
 Interpretasi
Hasil estimasi menunjukkan bahwa rata-rata tingkat resiliensi akademik mahasiswa Program Studi Statistika Universitas Mataram sebesar 3,476. Interval kepercayaan 95% menunjukkan bahwa rata-rata tingkat resiliensi akademik populasi diperkirakan berada pada rentang 3,146 hingga 3,806. Nilai Relative Standard Error (RSE) sebesar 4,85% menunjukkan bahwa hasil estimasi memiliki tingkat ketelitian yang sangat baik karena berada di bawah 25%. Sementara itu, nilai Design Effect (DEFF) sebesar 5,604 menunjukkan bahwa penggunaan desain Two-Stage Cluster Sampling menghasilkan varians yang lebih besar dibandingkan Simple Random Sampling, yang merupakan karakteristik umum pada desain sampling berkelompok.
4.5 Estimasi Tingkat Resiliensi Akademik Berdasarkan Kelas
Estimasi juga dilakukan pada masing-masing cluster yang terpilih untuk melihat perbedaan tingkat resiliensi akademik antar kelas.
 Tabel 4.5 Estimasi per Kelas
| Kelas |  Mean |
| ----- | ----: |
| 24 B  | 3,714 |
| 25 B  | 3,309 |
Interpretasi
Berdasarkan hasil estimasi diketahui bahwa mahasiswa pada kelas 24 B memiliki rata-rata skor resiliensi akademik sebesar 3,714, sedangkan mahasiswa pada kelas 25 B memiliki rata-rata sebesar 3,309. Hal ini menunjukkan bahwa tingkat resiliensi akademik mahasiswa kelas 24 B cenderung lebih tinggi dibandingkan mahasiswa kelas 25 B, meskipun kedua kelas masih berada pada kategori tinggi.
4.6 Distribusi Kategori Tingkat Resiliensi Akademik
Pengelompokan dilakukan berdasarkan skor rata-rata responden ke dalam empat kategori tingkat resiliensi akademik.
Tabel 4.6 Distribusi Kategori Tingkat Resiliensi Akademik
| Kategori      | Proporsi (%) |
| ------------- | -----------: |
| Sangat Rendah |         0,00 |
| Rendah        |         2,48 |
| Tinggi        |        35,86 |
| Sangat Tinggi |        61,66 |
 Interpretasi
Berdasarkan hasil estimasi kategori, 61,66% mahasiswa berada pada kategori Sangat Tinggi, 35,86% berada pada kategori Tinggi, sedangkan 2,48% berada pada kategori Rendah. Tidak terdapat mahasiswa yang termasuk dalam kategori Sangat Rendah. Hasil ini menunjukkan bahwa secara umum mahasiswa Program Studi Statistika Universitas Mataram memiliki tingkat resiliensi akademik yang tinggi hingga sangat tinggi, yang mengindikasikan kemampuan yang baik dalam menghadapi tantangan dan tekanan selama proses pembelajaran.

KESIMPULAN
Berdasarkan hasil penelitian mengenai Estimasi Tingkat Resiliensi Akademik Mahasiswa Program Studi Statistika Universitas Mataram Menggunakan Two-Stage Cluster Sampling, diperoleh beberapa kesimpulan sebagai berikut.
1. Hasil uji validitas menunjukkan bahwa seluruh butir pertanyaan (P1–P10) memiliki nilai r hitung lebih besar daripada r tabel (0,361), sehingga seluruh item dinyatakan valid dan mampu mengukur tingkat resiliensi akademik mahasiswa. Selanjutnya, hasil uji reliabilitas menunjukkan nilai Cronbach's Alpha sebesar 0,876, sehingga instrumen penelitian dinyatakan sangat reliabel dan layak digunakan dalam penelitian.
2. Berdasarkan hasil estimasi menggunakan metode Two-Stage Cluster Sampling, diperoleh estimasi rata-rata tingkat resiliensi akademik mahasiswa Program Studi Statistika Universitas Mataram sebesar 3,476 dengan Standard Error sebesar 0,168 serta Confidence Interval 95% sebesar 3,146–3,806. Nilai Relative Standard Error (RSE) sebesar 4,85% menunjukkan bahwa hasil estimasi memiliki tingkat ketelitian yang baik sehingga dapat digunakan untuk menggambarkan kondisi populasi.
3. Hasil estimasi berdasarkan kategori menunjukkan bahwa sebagian besar mahasiswa berada pada kategori Sangat Tinggi sebesar 61,66%, diikuti kategori Tinggi sebesar 35,86%, kategori Rendah sebesar 2,48%, dan tidak terdapat mahasiswa pada kategori Sangat Rendah. Dengan demikian, dapat disimpulkan bahwa tingkat resiliensi akademik mahasiswa Program Studi Statistika Universitas Mataram secara umum berada pada kategori sangat tinggi, yang menunjukkan bahwa mahasiswa memiliki kemampuan yang baik dalam menghadapi tekanan, tantangan, dan kesulitan selama proses pembelajaran.

SARAN
Berdasarkan hasil penelitian, terdapat beberapa saran yang dapat diberikan sebagai berikut.
1. Program Studi Statistika Universitas Mataram diharapkan dapat mempertahankan dan meningkatkan resiliensi akademik mahasiswa melalui kegiatan akademik maupun nonakademik yang mendukung kemampuan mahasiswa dalam menghadapi tantangan selama perkuliahan.
2. Penelitian selanjutnya disarankan menggunakan jumlah sampel yang lebih besar atau melibatkan seluruh angkatan agar hasil estimasi dapat menggambarkan kondisi populasi secara lebih menyeluruh.
3. Penelitian berikutnya juga dapat menambahkan variabel lain, seperti motivasi belajar, self-regulated learning, atau dukungan sosial, sehingga faktor-faktor yang memengaruhi resiliensi akademik dapat dianalisis secara lebih komprehensif.

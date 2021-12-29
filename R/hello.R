# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

Bantuan <- function() {
  print("Selamat datang di Package SDGs Pertanian")
  print("==========================================")
  print("Package ini dapat menghitung 3 Indikator:")
  print("")
  print("1. Indikator 2.3.1 dengan fungsi SDGs231(datasitasi)")
  print("2. Indikator 2.3.2 dengan fungsi SDGs232(Data_232)")
  print("3. Indikator 2.4.1 dengan fungsi SDGs241(data_241, Data_R554, Data_R559, Data_R567, Data_R572, Data_R574, Tabel_Konversi, Tabel_UMP)")
  print("")
  print("Semoga dapat membantu")
}

SDGs231 <- function(datasitasi){
  # MENGHITUNG AAU =========================

  # membuat kolom AAU
  AAUhit <- matrix(nrow=nrow(datasitasi), ncol=1)
  AAUhit <- as.data.frame(AAUhit)

  # menghitung AAU
  for (i in 1:nrow(datasitasi)){
    AAUhit[i,1] <- datasitasi[i,11]+datasitasi[i,12]
  }

  # menggabungkan AAU yang dihitung dengan dataframe
  colnames(AAUhit) <- "AAUhit"
  datasitasi <- cbind(datasitasi,AAUhit)


  # MENGHITUNG TLU ===========================

  # membuat kolom AAU
  TLUhit <- matrix(nrow=nrow(datasitasi), ncol=1)
  TLUhit <- as.data.frame(TLUhit)

  # menghitung TLU
  for (i in 1:nrow(datasitasi)){
    TLUhit[i,1] <- (datasitasi$R404A_01[i]*0.65)+(datasitasi$R404A_03[i]*0.1)
  }

  # menggabungkan TLU dengan dataframe
  colnames(TLUhit) <- "TLUhit"
  datasitasi <- cbind(datasitasi, TLUhit)


  # MENGHITUNG REVENUE ================================

  # membuat kolom Revenue
  revhit <- matrix(nrow=nrow(datasitasi), ncol=1)
  revhit <- as.data.frame(revhit)

  # menghitung revenue
  for (i in 1:nrow(datasitasi)){
    revhit[i,1] <- datasitasi$R509AK7[i]+datasitasi$R509BK7[i]+datasitasi$R509CK7[i]+datasitasi$R516K7[i]+datasitasi$R524CK7[i]+datasitasi$R524DK7[i]+datasitasi$R530AK7[i]+datasitasi$R530BK7[i]+datasitasi$R535A_K7[i]+datasitasi$R535B_K7[i]+datasitasi$R540B_K7[i]+datasitasi$R540C_K7[i]
  }

  # menggabungkan revenue dengan dataframe
  colnames(revhit) <- "Revhit"
  datasitasi <- cbind(datasitasi, revhit)


  # summary ===========

  # menghitung treshold
  threshold_AAU <- qnorm(0.4, mean(datasitasi$AAUhit), sd(datasitasi$AAUhit))
  threshold_TLU <- qnorm(0.4, mean(datasitasi$TLUhit), sd(datasitasi$TLUhit))
  threshold_rev <- qnorm(0.4, mean(datasitasi$Revhit), sd(datasitasi$Revhit))

  # mengkategorikan AAU
  kategoriAAU <- as.data.frame(matrix(nrow=nrow(datasitasi), ncol=1))
  colnames(kategoriAAU) <- "kategoriAAU"

  for (i in 1:nrow(datasitasi)){
    if (datasitasi$AAUhit[i]<threshold_AAU){
      kategoriAAU[i,1] <- 1
    }else{
      kategoriAAU[i,1] <- 0
    }
  }

  # mengkategorikan TLU
  kategoriTLU <- as.data.frame(matrix(nrow=nrow(datasitasi), ncol=1))
  colnames(kategoriTLU) <- "kategoriTLU"

  for (i in 1:nrow(datasitasi)){
    if (datasitasi$TLUhit[i]<threshold_TLU){
      kategoriTLU[i,1] <- 1
    }else{
      kategoriTLU[i,1] <- 0
    }
  }

  # mengkategorikan Rev
  kategoriREV <- as.data.frame(matrix(nrow=nrow(datasitasi), ncol=1))
  colnames(kategoriREV) <- "kategoriREV"

  for (i in 1:nrow(datasitasi)){
    if (datasitasi$Revhit[i]<threshold_rev){
      kategoriREV[i,1] <- 1
    }else{
      kategoriREV[i,1] <- 0
    }
  }

  # menggabungkan kategori AAU TLU dan Rev dengan dataframe
  datasitasi <- cbind(datasitasi, kategoriAAU, kategoriTLU, kategoriREV)

  # mengkategorikan SSP
  # kategori AAU =1, kategori TLU =1, kategorei Rev =1 -> SSP
  SSP <- as.data.frame(matrix(nrow=nrow(datasitasi), ncol=1))
  colnames(SSP) <- "katSSP"

  for (i in 1:nrow(datasitasi)){
    if (sum(kategoriAAU[i,1]+kategoriTLU[i,1]+kategoriREV[i,1]) == 3){
      SSP[i,1] <- 1
    }else{
      SSP[i,1] <- 0
    }
  }

  # menggabungkan SSP dengan dataframe
  head(SSP)
  datasitasi <- cbind(datasitasi, SSP)
  head(datasitasi)



  # MENGHITUNG HARI KERJA =========================

  # R701AB
  R701AB <- matrix(nrow=nrow(datasitasi), ncol=1)
  R701AB <- as.data.frame(R701AB)
  colnames(R701AB) <- "R701AB"

  for (i in 1:nrow(datasitasi)){
    R701AB[i,1] <- datasitasi$R701A[i]*datasitasi$R701B[i]
  }



  # R702AB
  R702AB <- matrix(nrow=nrow(datasitasi), ncol=1)
  R702AB <- as.data.frame(R702AB)
  colnames(R702AB) <- "R702AB"

  for (i in 1:nrow(datasitasi)){
    R702AB[i,1] <- datasitasi$R702A[i]*datasitasi$R702B[i]
  }



  # jumlah hari kerja
  jumlah_hari_kerja <- matrix(nrow = nrow(datasitasi), ncol=1)
  jumlah_hari_kerja <- as.data.frame(jumlah_hari_kerja)
  colnames(jumlah_hari_kerja) <- "jumlah_hari_kerja"

  for (i in 1:nrow(datasitasi)){
    jumlah_hari_kerja[i,1] <- max(R701AB[i,1], R702AB[i,1])
  }



  datasitasi <- cbind(datasitasi, R701AB, R702AB,jumlah_hari_kerja)


  # REV PPPP ===================

  rev_ppp <- matrix(nrow=nrow(datasitasi), ncol=1)
  rev_ppp <- as.data.frame(rev_ppp)
  colnames(rev_ppp) <- "revPPP"

  for (i in 1:nrow(datasitasi)){
    rev_ppp[i,1] <- datasitasi$Revhit[i]/4743.337
  }

  datasitasi <- cbind(datasitasi, rev_ppp)



  # SDGs 231 =============================

  sdg231 <- matrix(nrow=nrow(datasitasi), ncol=1)
  sdg231 <- as.data.frame(sdg231)
  colnames(sdg231) <- "sdg231"

  for (i in 1:nrow(datasitasi)){
    sdg231[i,1] <- datasitasi$revPPP[i]/datasitasi$jumlah_hari_kerja[i]
  }


  datasitasi <-cbind(datasitasi, sdg231)



  # Custom table ===============
  SSP <- subset(datasitasi, datasitasi$katSSP==1)
  NonSSP <- subset(datasitasi, datasitasi$katSSP==0)
  hasil_231 <- matrix(c("SSP", "NonSSP", mean(SSP$sdg231, na.rm = T), mean(NonSSP$sdg231, na.rm = T)), ncol=2, nrow=2)
  hasil_231 <- as.data.frame(hasil_231)
  colnames(hasil_231) <- c("Kategori", "Mean SDGs 231")
  hasil_231

  # ===========
  library('openxlsx')
  library('rio')
  export(datasitasi,"Data_231.xlsx")
  export(hasil_231,"Hasil_231.xlsx")

  print("============Data telah diexport=================")

  hasil_231

}




# =============================================

SDGs241 <- function(data_241, Data_R554, Data_R559, Data_R567, Data_R572, Data_R574, Tabel_Konversi, Tabel_UMP){
  #Persiapan Data
  #Olah R554
  Data_R554$R554_KD_A[is.na(Data_R554$R554_KD_A)] <- 0
  Data_R554$R554_KD_B[is.na(Data_R554$R554_KD_B)] <- 0
  Data_R554$R554_KD_C[is.na(Data_R554$R554_KD_C)] <- 0

  Data_R554$min <- apply(Data_R554[,4:6], 1, FUN = function(x) {min(x[x > 0])})
  Data_R554$max <- apply(Data_R554[,4:6], 1, FUN = function(x) {max(x[x > 0])})
  Data_R554$R554_olah <- ifelse(Data_R554$min==1 & Data_R554$max==2, 1,
                                ifelse(Data_R554$min==1 & Data_R554$max==1, 2,
                                       ifelse(Data_R554$min==2 & Data_R554$max==2,2,
                                              ifelse(Data_R554$min==2 & Data_R554$max==3,2,
                                                     ifelse(Data_R554$min==3 & Data_R554$max==3,3,0)))))
  #Masukkan R554_olah ke Data_241
  Data_241 <- merge(Data_241, Data_R554[,c("ID","R554_olah")],
                    by.x="ID", by.y="ID",all.x=TRUE)

  #Olah R559
  Data_R559$R559_olah <- ifelse(Data_R559$R559_6==1,6,"TERDAMPAK")

  #Masukkan R559_olah ke Data_241
  Data_241 <- merge(Data_241, Data_R559[,c("ID","R559_olah")],
                    by.x="ID", by.y="ID",all.x=TRUE)

  #Olah R567
  Data_R567$R567_olah <- apply(Data_R567[,4:11], 1, FUN = sum)

  #Masukkan R567_olah ke Data_241
  Data_241 <- merge(Data_241, Data_R567[,c("ID","R567_olah")],
                    by.x="ID", by.y="ID",all.x=TRUE)

  #Olah R572
  Data_R572$R572_olah <- apply(Data_R572[,4:6], 1, FUN = sum)
  #Masukkan R572_olah ke Data_241
  Data_241 <- merge(Data_241, Data_R572[,c("ID","R572_olah")],
                    by.x="ID", by.y="ID",all.x=TRUE)

  #Olah R574
  Data_R574$R574_olah <- apply(Data_R574[,4:15], 1, FUN = sum)
  #Masukkan R574_olah ke Data_241
  Data_241 <- merge(Data_241, Data_R574[,c("ID","R574_olah")],
                    by.x="ID", by.y="ID",all.x=TRUE)

  ##CLEANING DATA
  Data_241$R554_olah[which(Data_241$R552 == 0)] = 0
  Data_241$R554_olah[is.na(Data_241$R554_olah)] <- 0
  Data_241$R560_K2[which(Data_241$R559_olah == 6)] = 0
  Data_241$R560_K3[which(Data_241$R559_olah == 6)] = 0
  Data_241$R560_K2 <- as.numeric(Data_241$R560_K2)
  Data_241$R561_1_K2[which(Data_241$R561 != 1)] = 0
  Data_241$R561_1_K3[which(Data_241$R561 != 1)] = 0
  Data_241$R562[which(Data_241$R561 != 1)] = 0
  Data_241$R563[which(Data_241$R562 == 1)] = 0
  Data_241$R567_olah[which(Data_241$R566 == 0)] = 0
  Data_241$R574_olah[which(Data_241$R573 == 0)] = 0
  Data_241$R561_1_K2 <- as.numeric(Data_241$R561_1_K2)
  Data_241$R576_K2 <- as.numeric(Data_241$R576_K2)
  Data_241$SUM_OF_R708_1 <- as.numeric(Data_241$SUM_OF_R708_1)
  Data_241$SUM_OF_R708_2 <- as.numeric(Data_241$SUM_OF_R708_2)

  ##SUBINDIKATOR 1
  #Hitung AAU dan Produktivitas
  Data_241$AAU <- Data_241$R313i+Data_241$R508K5+Data_241$R521K5
  Data_241$Produktivitas <- c(((Data_241$Nilai_Prod*Data_241$WEIGHT)/(Data_241$AAU*Data_241$WEIGHT))*1000)
  Data_241 <- do.call(data.frame, lapply(Data_241, function(x){
    replace(x, is.infinite(x),NaN)
  }))

  #Hitung Persentil ke-90
  NP_Percentile <- quantile(Data_241$Produktivitas,probs = c(0.90), na.rm=TRUE, type=6)
  sprintf("%.f",NP_Percentile)

  #Klasifikasi Subindikator 1
  Data_241$SUB1 <- ifelse(Data_241$Produktivitas>=2/3*NP_Percentile, "DESIRABLE",
                          (ifelse(Data_241$Produktivitas<1/3*NP_Percentile,"UNSUSTAINABLE","ACCEPTABLE")))

  #Menjumlahkan lahan sesuai klasifikasi
  SUB1 <- setNames(aggregate(LAHAN_WEIGHTED ~ SUB1, data=Data_241, sum), c("Kategori","Sub.01"))

  ##SUBINDIKATOR 2
  #Klasifikasi Subindikator 2
  Data_241$SUB2 <- ifelse(Data_241$R545 == 4, "DESIRABLE",
                          (ifelse(Data_241$R545 == 1, "UNSUSTAINABLE","ACCEPTABLE")))

  #Menjumlahkan lahan sesuai klasifikasi
  SUB2 <- setNames(aggregate(LAHAN_WEIGHTED ~ SUB2, data=Data_241, sum), c("Kategori","Sub.02"))

  ##SUBINDIKATOR 3
  #Klasifikasi
  Data_241$SUB3 <- ifelse(Data_241$R552==0 | Data_241$R554_olah==1,"DESIRABLE",
                          ifelse(Data_241$R554_olah==2,"ACCEPTABLE","UNSUSTAINABLE"))

  #Menjumlahkan lahan sesuai klasifikasi
  SUB3 <- setNames(aggregate(LAHAN_WEIGHTED ~ SUB3, data=Data_241, sum), c("Kategori","Sub.03"))

  ##SUBINDIKATOR 4
  #Vlookup Konversi
  Data_241 <- merge(Data_241, Tabel_Konversi[,c("Index","Konversi")],
                    by.x="R560_K3", by.y="Index",all.x=TRUE)
  names(Data_241)[names(Data_241) == 'Konversi'] <- 'Konversi_R560_K3'
  Data_241$Konversi_R560_K3[is.na(Data_241$Konversi_R560_K3)] <- 0

  #Luas Lahan Terdampak
  Data_241$Lahan_Terdampak <- (Data_241$R560_K2 * Data_241$Konversi_R560_K3)
  Data_241$Rasio_Lahan_Terdampak<-(Data_241$Lahan_Terdampak*Data_241$WEIGHT)/Data_241$LAHAN_WEIGHTED

  #Klasifikasi
  Data_241$SUB4 <- ifelse(Data_241$R559_olah==6, "DESIRABLE",
                          ifelse(Data_241$Rasio_Lahan_Terdampak<0.10, "DESIRABLE",
                                 ifelse(Data_241$Rasio_Lahan_Terdampak>0.50, "UNSUSTAINABLE", "ACCEPTABLE")))

  #Menjumlahkan lahan sesuai klasifikasi
  SUB4 <- setNames(aggregate(LAHAN_WEIGHTED ~ SUB4, data=Data_241, sum), c("Kategori","Sub.04"))

  ##SUBINDIKATOR 5
  #Vlookup Konversi
  Data_241 <- merge(Data_241,
                    Tabel_Konversi[,c("Index","Konversi")],
                    by.x="R561_1_K3", by.y="Index",all.x=TRUE)
  names(Data_241)[names(Data_241) == 'Konversi'] <- 'Konversi_R561_1_K3'
  Data_241$Konversi_R561_1_K3[is.na(Data_241$Konversi_R561_1_K3)] <- 0

  #Lahan irigasi
  Data_241$Lahan_Irigasi <- (Data_241$R561_1_K2 * Data_241$Konversi_R561_1_K3)
  Data_241$Rasio_Lahan_Irigasi <- (Data_241$Lahan_Irigasi*Data_241$WEIGHT)/Data_241$LAHAN_WEIGHTED

  #Klasifikasi
  Data_241$SUB5 <- ifelse(Data_241$R561 > 1, "DESIRABLE",
                          ifelse(Data_241$R561==1 & Data_241$Rasio_Lahan_Irigasi<0.10, "DESIRABLE",
                                 ifelse(Data_241$R561==1 & Data_241$Rasio_Lahan_Irigasi>=0.10 & Data_241$R562==1,"DESIRABLE",
                                        ifelse(Data_241$Rasio_Lahan_Irigasi>=0.10 & Data_241$R562>1 & Data_241$R563==1,"ACCEPTABLE",
                                               ifelse(Data_241$Rasio_Lahan_Irigasi>=0.10 & Data_241$R562>1 & Data_241$R563==2,"ACCEPTABLE","UNSUSTAINABLE")))))

  #Menjumlahkan lahan sesuai klasifikasi
  SUB5 <- setNames(aggregate(LAHAN_WEIGHTED ~ SUB5, data=Data_241, sum), c("Kategori","Sub.05"))

  ##SUBINDIKATOR 6
  #Klasifikasi
  Data_241$SUB6 <- ifelse(Data_241$R564==1 & Data_241$R566==1 & Data_241$R567_olah>=4, "DESIRABLE",
                          ifelse(Data_241$R564==0,"DESIRABLE",
                                 ifelse(Data_241$R564==1 & Data_241$R566==0, "UNSUSTAINABLE","ACCEPTABLE")))

  #Menjumlahkan lahan sesuai klasifikasi
  SUB6 <- setNames(aggregate(LAHAN_WEIGHTED ~ SUB6, data=Data_241, sum), c("Kategori","Sub.06"))

  ##SUBINDIKATOR 7
  #Klasifikasi
  Data_241$SUB7 <- ifelse(Data_241$R568==0, "DESIRABLE",
                          ifelse(Data_241$R568==1 & Data_241$R569==1 & Data_241$R572_olah==3 & Data_241$R574_olah>=4, "DESIRABLE",
                                 ifelse(Data_241$R568==1 & Data_241$R569==2 & Data_241$R571==0 & Data_241$R573==0, "UNSUSTAINABLE","ACCEPTABLE")))

  #Menjumlahkan lahan sesuai klasifikasi
  SUB7 <- setNames(aggregate(LAHAN_WEIGHTED ~ SUB7, data=Data_241, sum), c("Kategori","Sub.07"))

  ##SUBINDIKATOR 8
  #Vlookup Konversi
  Data_241 <- merge(Data_241, Tabel_Konversi[,c("Index","Konversi")], by.x="R576_K3", by.y="Index",all.x=TRUE)
  names(Data_241)[names(Data_241) == 'Konversi'] <- 'Konversi_R576_K3'
  Data_241$Konversi_R576_K3[is.na(Data_241$Konversi_R576_K3)] <- 0

  #Lahan Tertutup
  Data_241$Lahan_Tertutup <- (Data_241$R576_K2 * Data_241$Konversi_R576_K3)
  Data_241$Rasio_Lahan_Tertutup <- (Data_241$Lahan_Tertutup*Data_241$WEIGHT)/Data_241$LAHAN_WEIGHTED
  Data_241$Rasio_Lahan_Tertutup[is.na(Data_241$Rasio_Lahan_Tertutup)] <- 0

  #Klasifikasi
  Data_241$Kriteria_SUB8 <- ifelse(Data_241$Rasio_Lahan_Tertutup>=0.1,1,0)+
    ifelse(Data_241$R577==0,1,0)+
    ifelse(Data_241$R578>=80,1,0)+
    ifelse(Data_241$R579==1,1,0)

  Data_241$SUB8 <- ifelse(Data_241$Kriteria_SUB8 > 1, "DESIRABLE",
                          ifelse(Data_241$Kriteria_SUB8==1, "ACCEPTABLE", "UNSUSTAINABLE"))

  #Menjumlahkan lahan sesuai klasifikasi
  SUB8 <- setNames(aggregate(LAHAN_WEIGHTED ~ SUB8, data=Data_241, sum), c("Kategori","Sub.08"))

  ##SUBINDIKATOR 9
  #Hitung Upah Per Bulan
  Data_241$Upah_Sebulan <- (Data_241$SUM_OF_R708_1 + Data_241$SUM_OF_R708_2)*30
  Data_241$Upah_Sebulan[is.na(Data_241$Upah_Sebulan)] <- 0

  #Vlookup UMP
  Data_241 <- merge(Data_241, Tabel_UMP[,c("Provinsi","UMP")], by.x="Kode_Prov", by.y="Provinsi",all.x=TRUE)

  #Klasifikasi
  Data_241$SUB9 <- ifelse(Data_241$R704==0, "DESIRABLE",
                          ifelse(Data_241$Upah_Sebulan>Data_241$UMP, "DESIRABLE",
                                 ifelse(Data_241$Upah_Sebulan==Data_241$UMP, "ACCEPTABLE", "UNSUSTAINABLE")))

  #Menjumlahkan lahan sesuai klasifikasi
  SUB9 <- setNames(aggregate(LAHAN_WEIGHTED ~ SUB9, data=Data_241, sum), c("Kategori","Sub.09"))

  ##SUBINDIKATOR 10
  #Klasifikasi
  Data_241$SUB10 <- ifelse(Data_241$Prob_Mod_Sev<0.5 & Data_241$Prob_Sev<0.5, "DESIRABLE",
                           ifelse(Data_241$Prob_Mod_Sev>0.5 & Data_241$Prob_Sev<0.5, "ACCEPTABLE","UNSUSTAINABLE"))

  #Menjumlahkan lahan sesuai klasifikasi
  SUB10 <- setNames(aggregate(LAHAN_WEIGHTED ~ SUB10, data=Data_241, sum), c("Kategori","Sub.10"))

  ##SUBINDIKATOR 11
  #Klasifikasi
  Data_241$SUB11 <- ifelse(Data_241$R806A==1 | Data_241$R806C==1 | Data_241$R806D==1 | Data_241$R806E==1, "DESIRABLE",
                           ifelse(Data_241$R806A>1 & Data_241$R806C>1 & Data_241$R806D>1 & Data_241$R806E>1, "UNSUSTAINABLE","ACCEPTABLE"))

  #Menjumlahkan lahan sesuai klasifikasi
  SUB11 <- setNames(aggregate(LAHAN_WEIGHTED ~ SUB11, data=Data_241, sum), c("Kategori","Sub.11"))

  #Menggabungkan hasil semua subindikator
  a <- merge(SUB1, SUB2,  by=1, all=TRUE)
  a <- merge(a, SUB3,  by=1, all=TRUE)
  a <- merge(a, SUB4,  by=1, all=TRUE)
  a <- merge(a, SUB5,  by=1, all=TRUE)
  a <- merge(a, SUB6,  by=1, all=TRUE)
  a <- merge(a, SUB7,  by=1, all=TRUE)
  a <- merge(a, SUB8,  by=1, all=TRUE)
  a <- merge(a, SUB9,  by=1, all=TRUE)
  a <- merge(a, SUB10,  by=1, all=TRUE)
  a <- merge(a, SUB11, by=1, all=TRUE)
  Hasil_Absolut <- a

  #Hitung proporsi lahan pertanian
  library('dplyr')
  Hasil_Absolut[is.na(Hasil_Absolut)] <- 0
  Hasil_Persen <- as.data.frame(prop.table(data.matrix(Hasil_Absolut[,-1]), 2)*100)
  Hasil_Persen$Kategori <- c("ACCEPTABLE","DESIRABLE","UNSUSTAINABLE")
  Hasil_Persen <- Hasil_Persen[, c(12, 1:11)]
  Hasil_Persen <- Hasil_Persen[c(3,1,2),]

  #Membuat grafik hasil akhir
  library('ggplot2')
  par(xpd=T, mar=par()$mar+c(0,0,0,3))
  pdf(file="Plot241.pdf")
  barplot(as.matrix(Hasil_Persen[,-1]), border="white",
          col = c("red","yellow","green"), main = "Stacked Barplot: Indikator 241",
          ylab = " Persentase (%)", xlab = "Sub-indikator",
          legend.text = Hasil_Persen$Kategori,
          args.legend = list(title="Kategori",x="topright",inset=c(-0.20,0)), beside=FALSE)
  dev.off()
  #Export Tabel Hasil_Persen
  library('openxlsx')
  library('rio')
  export(Hasil_Persen,"Hasil_241.xlsx")

  print("============Data telah diexport=================")

  Hasil_Persen
}

SDGs232 <- function(Data_232){
  attach(Data_232)
  rev_tanaman=(R306E_K2 * R306E_K6)+ (R307E_K2 * R307E_K6)+ (R308E_K2 * R308E_K6)+ (R309E_K2* R309E_K6)+(R311A_K6+R311B_K6+R311C_K6+R311D_K6+R311E_K6+R311F_K6+R311G_K6+R311H_K6)
  rev_ternak=(R405F_K2*R405F_K3)+(R405G_K2*R405G_K3)+ (R405H_K2*R405H_K3)+((R407A_K2 + R407B_K2 + R407C_K2 + R407D_K2 + R407E_K2)*R407C_K3)+ R431_K6
  rev_ikan=R509_K7A + R509_K7B + R516_K7A + R516_K7B
  rev_hutan=R524_K7 + R530_K7 + R535_K7 + R540_K7

  rev_all=rev_tanaman + rev_ternak + rev_ikan + rev_hutan

  cost_tanaman=(R310D_K2 + R310E_K2) *R310C_K3
  cost_ternak=(R405F_K2 * R405F_K3)+ (R405H_K2 * R405H_K3)+ ((R407D_K2 + R407E_K2) * R407C_K3)+ ((R414D + R414E) * R414H)
  cost_ikan=((R510D_K2A+R510E_K2A) * R510C_K3A)+ ((R510D_K2B+R510E_K2B)*R510C_K3B)+((R517D_K2A+R517E_K2A) * R517C_K3A)+ ((R517D_K2B+R517E_K2B)*R517C_K3B)
  cost_hutan=((R525D_K2+R525E_K2) * R525C_K3)+((R531D_K2+R531E_K2)*R531C_K3)+((R536D_K2+R536E_K2) * R536C_K3)+((R541D_K2+R541E_K2)*R541C_K3)

  cost_allkor=cost_tanaman+cost_ternak+cost_ikan+cost_hutan

  cost_alleko=(R301A+R301B+R302A+R302B+ R302C+R303A_K6+ R303B_K6+ R303C_K6+R305A_K3+ R305B_K3+ R305C_K3+ R305D_K3+R305E_K3+ R305F_K3+ R305G_K3+R306A_K3+ R306B_K3+ R306C_K3+ R306D_K3+R306E_K3+ R306F_K3+R307A1_K5+  R307A2_K5+  R307A31_K5+ R307A32_K5+ R307A4_K5+R307A5_K5+  R307A6_K5+  R307A7_K5+  R307B1_K5+  R307B2_K5+R307B3_K5 + R307B4_K5 + R307B5_K5+  R307B6_K5+R308A1_K6+  R308A2_K6+ R308B_K6+   R308C1_K6+  R308C2_K6+R308D_K6+   R308E_K6 +R308EL_K6+  R308F_K6+R309A1 +  R309A2+  R309A3+ R309A4+R309A5+ R309A6+R312A_K2+  R312B_K2 + R312C_K2+  R312D_K2 )

  pendapatan=rev_all - cost_allkor - cost_alleko

  incomeppp=pendapatan / 4743.337

  Data_232 <- cbind(Data_232, rev_tanaman, rev_ternak, rev_ikan, rev_hutan, rev_all, cost_tanaman, cost_ternak, cost_ikan, cost_hutan, cost_allkor, cost_alleko, pendapatan, incomeppp)


  SSP <- subset(Data_232, Data_232$Status=="SSP")
  NonSSP <- subset(Data_232, Data_232$Status=="NON SSP")
  SSP.weighted <- svymean(~SSP$incomeppp, design = svydesign(ids = ~0, weights = SSP$FK, data = SSP))
  NonSSP.weighted <- svymean(~NonSSP$incomeppp, design = svydesign(ids = ~0, weights = NonSSP$FK, data = NonSSP))

  hasil_232 <- matrix(c("SSP", "NonSSP", as.data.frame(SSP.weighted)$mean, as.data.frame(NonSSP.weighted)$mean), ncol=2, nrow=2)
  hasil_232 <- as.data.frame(hasil_232)
  colnames(hasil_232) <- c("Kategori", "MeanSDGs232")
  hasil_232

  print("============Data telah diexport=================")


  #Export Tabel Hasil_Persen
  library('openxlsx')
  library('rio')
  export(hasil_232,"Hasil_232.xlsx")
  export(Data_232,"Data_232.xlsx")

  hasil_232
}

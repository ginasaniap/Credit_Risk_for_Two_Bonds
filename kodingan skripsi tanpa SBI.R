aset<-scan("C:/Users/ASUS/Documents/aset/asetfix.txt")
aset<-matrix(scan("C:/Users/ASUS/Documents/aset/asetfix.txt"))
aset


# Deskripsi Data
desc=function(aset)
  
{
  n<-length(aset)
  return<-matrix(nrow=n-1,ncol=1)
  for(i in 1:n-1)
  {
    return[i]<-aset[i+1,]/aset[i,]
    lnreturn<-log(return)
  }
  
  rataaset<-mean(aset)
  varaset<-var(aset)
  sdaset<-sqrt(varaset)
  minaset<-min(aset)
  maksaset<-max(aset)
  
  ratareturn<-mean(return)
  varreturn<-var(return)
  sdreturn<-sqrt(varreturn)
  minreturn<-min(return)
  maksreturn<-max(return)
  ratalnr<-mean(lnreturn)
  varlnr<-var(lnreturn)
  sdlnr<-sqrt(var(lnreturn))
  minlnr<-min(lnreturn)
  makslnr<-max(lnreturn)
  
  volatilitas<-sqrt(12*var(lnreturn))
  
  cat("--------------------------","\n")
  cat("       Nilai Return       ","\n")
  print(return)
  cat("--------------------------","\n")
  cat("\n")
  
  cat("\n")
  cat("--------------------------","\n")
  cat("         Ln Return        ","\n")
  print(lnreturn)
  cat("--------------------------","\n")
  cat("\n")
  
  cat("\n")
  cat("-------------------------------------","\n")
  cat("Statistik Deskriptif Data Aset","\n")
  cat("-------------------------------------","\n")
  cat("Jumlah data aset = ",n,"\n")
  cat("Rata-Rata        = ",rataaset,"\n")
  cat("Variansi         = ",varaset,"\n")
  cat("Standar Deviasi  = ",sdaset,"\n")
  cat("Harga Terendah   = ",minaset,"\n")
  cat("Harga Tertinggi  = ",maksaset,"\n")
  cat("-------------------------------------","\n")
  
  cat("\n")
  cat("-------------------------------------","\n")
  cat("Statistik Deskriptif Data Return Aset","\n")
  cat("-------------------------------------","\n")
  cat("Rata-Rata        = ",ratareturn,"\n")
  cat("Variansi         = ",varreturn,"\n")
  cat("Standar Deviasi  = ",sdreturn,"\n")
  cat("Harga Terendah   = ",minreturn,"\n")
  cat("Harga Tertinggi  = ",maksreturn,"\n")
  cat("-------------------------------------","\n")
  
  cat("\n")
  cat("----------------------------------------","\n")
  cat("Statistik Deskriptif Data Ln Return Aset","\n")
  cat("----------------------------------------","\n")
  cat("Rata-Rata        = ",ratalnr,"\n")
  cat("Variansi         = ",varlnr,"\n")
  cat("Standar Deviasi  = ",sdlnr,"\n")
  cat("Harga Terendah   = ",minlnr,"\n")
  cat("Harga Tertinggi  = ",makslnr,"\n")
  cat("Volatilitas      = ",volatilitas,"\n")
  cat("----------------------------------------","\n")
  
  qqnorm(lnreturn)
  qqline(lnreturn)
  
  jb<-jarque.bera.test(lnreturn)
  cat("\n")
  cat("---------------------------------------------------","\n")
  print(jb)
  cat("---------------------------------------------------","\n")
}

desc(aset)

#Menghitung Kupon
cb<-function(F,c,ct,T)
{	
  kupon<-F*(c/ct)
  K1<-F+kupon
  cat("\n")
  cat("----------------------------------------------------------","\n")
  cat("              Nilai Kupon       ","\n")
  cat("----------------------------------------------------------","\n")
  cat("Hutang pokok obligasi             = ",F,"\n")
  cat("Suku bunga obligasi               = ",c,"\n")
  cat("Jangka waktu obligasi             = ",T,"bulan","\n")
  cat("Nilai kupon obligasi              = ",kupon,"\n")
  cat("Nilai hutang pokok ditambah kupon = ",K1,"\n")
  cat("----------------------------------------------------------","\n")
}
cb(435000000000,0.0775,4,84)
cb(65000000000,0.08,4,144)

# Valuasi Obligasi dengan Kupon Satu Periode 
# Berdasarkan Default at Maturity 

valuation1=function(x,A0,F,F1,kupon,ct,T,c,alpha)
  
{
  cat("\n")
  cat("-----------------------OUTPUT----------------------","\n")
  cat("\n")
  cat("ONE PERIOD COUPON BOND BASED ON DEFAULT AT MATURITY","\n")
  cat("\n")	
  
  cat("RINGKASAN DATA PERUSAHAAN","\n")
  cat("---------------------------------------------------","\n")
  cat("\tData   \t\t\tNilai\t\t","\n")
  cat("---------------------------------------------------","\n")
  cat("Nilai Awal Aset           Rp.",A0,"\t","\n")
  cat("Nilai Face Value          Rp.",F,"\t\t","\n") 
  cat("Jangka Waktu Jatuh Tempo ",T,"bulan\t\t","\n")
  cat("Kupon                    ",kupon,"\t","\n")
  cat("---------------------------------------------------","\n")
  cat("\n")
  
  # Plot qq Norm
  n=nrow(x)
  i=1:n
  s=x[i+1,1]/x[i,1]  #return
  log.s=log(s[i-1])  #ln return
  qqnorm(log.s,main="Normal Q-Q Plot",xlab="Theoritical Quantiles",ylab="ln return");qqline(log.s,col=2)
  
  # Mencari Volatilitas
  c=sum((log.s-mean(log.s))^2)
  d=length(log.s)-1
  nilaivar1=c/d
  nilaivar2=12*nilaivar1
  sigma=nilaivar2^0.5 # Volatilitas Bulanan
  
  
  # Mencari nilai ekuitas berdasarkan Default at Maturity
  a11=log(A0/F1)+(0.5*sigma)^2*ct
  a122=sigma*(T^0.5)
  a21=log(A0/F1)+(-0.5*sigma)^2*ct
  a1=a11/a122
  a2=a21/a122
  Na1=pnorm(a1) # Nilai distribusi kumulatif untuk a1
  Na2=pnorm(a2) # Nilai distribusi kumulatif untuk a2
  eq=(A0*Na1)-(F1*Na2*exp(ct))
  
  # Mencari nilai liabilitas berdasarkan Default at Maturity
  li=A0-eq
  
  # Mencari probabilitas kebangkrutan berdasarkan Default at Maturity
  edf=1-Na2
  edfpersen=edf*100
  
  cat("RINGKASAN HASIL            ","\n")
  cat("---------------------------------------------------","\n")
  cat("\t\tData\t\t\tNilai\t\t","\n")
  cat("---------------------------------------------------","\n")
  cat("Volatilitas Aset\t\t",sigma,"\t\t","\n")
  cat("Nilai Ekuitas\t\t\t",eq,"\t\t","\n")
  cat("Nilai Liabilitas\t\t",li,"\t\t","\n")
  cat("Probabilitas Kebangkrutan\t",edf, "\t\t","\n")
  cat("\t\t\t\t atau",edfpersen,"%","\t\t","\n")
  cat("---------------------------------------------------","\n")
  cat("\n")
}

asset<-read.table("C:/Users/ASUS/Documents/aset/asetfix.txt")
valuation1(asset,784192878000000,435000000000,443428125000,8428125000,4,84,0.0775,0.05)
valuation1(asset,784192878000000,65000000000,66300000000,1300000000,4,144,0.08,0.05)


# Valuasi Obligasi dengan Kupon Dua Periode 
# Berdasarkan Default at Maturity

#Menghitung A1star dengan biseksi
biseksi=function(F1,utang,sig,T1,T2,a,b)
{
  c=(a+b)/2
  d1a=((log(a/utang))+(((0.5*(sig^2)))*(T2-T1)))/(sig*sqrt(T2-T1))
  d2a=((log(a/utang))+((-(0.5*(sig^2)))*(T2-T1)))/(sig*sqrt(T2-T1))
  d1b=((log(b/utang))+(((0.5*(sig^2)))*(T2-T1)))/(sig*sqrt(T2-T1))
  d2b=((log(b/utang))+((-(0.5*(sig^2)))*(T2-T1)))/(sig*sqrt(T2-T1))
  d1c=((log(c/utang))+(((0.5*(sig^2)))*(T2-T1)))/(sig*sqrt(T2-T1))
  d2c=((log(c/utang))+((-(0.5*(sig^2)))*(T2-T1)))/(sig*sqrt(T2-T1))
  Nd1a=pnorm(d1a)
  Nd2a=pnorm(d2a)
  Nd1b=pnorm(d1b)
  Nd2b=pnorm(d2b)
  Nd1c=pnorm(d1c)
  Nd2c=pnorm(d2c)
  fa=(a*Nd1a)-(F1*Nd2a*exp((T2-T1)))-utang
  fb=(b*Nd1b)-(F1*Nd2b*exp((T2-T1)))-utang
  fc=(c*Nd1c)-(F1*Nd2c*exp((T2-T1)))-utang
  if((fa*fb)<0)
  {
    for (i in 1:30)
    {
      c=(a+b)/2
      d1a=((log(a/utang))+(((0.5*(sig^2)))*(T2-T1)))/(sig*sqrt(T2-T1))
      d2a=((log(a/utang))+((-(0.5*(sig^2)))*(T2-T1)))/(sig*sqrt(T2-T1))
      d1b=((log(b/utang))+(((0.5*(sig^2)))*(T2-T1)))/(sig*sqrt(T2-T1))
      d2b=((log(b/utang))+((-(0.5*(sig^2)))*(T2-T1)))/(sig*sqrt(T2-T1))
      d1c=((log(c/utang))+(((0.5*(sig^2)))*(T2-T1)))/(sig*sqrt(T2-T1))
      d2c=((log(c/utang))+((-(0.5*(sig^2)))*(T2-T1)))/(sig*sqrt(T2-T1))
      Nd1a=pnorm(d1a)
      Nd2a=pnorm(d2a)
      Nd1b=pnorm(d1b)
      Nd2b=pnorm(d2b)
      Nd1c=pnorm(d1c)
      Nd2c=pnorm(d2c)
      fa=(a*Nd1a)-(F1*Nd2a*exp((T2-T1)))-utang
      fb=(b*Nd1b)-(F1*Nd2b*exp((T2-T1)))-utang
      fc=(c*Nd1c)-(F1*Nd2c*exp((T2-T1)))-utang
      if((fa[i]%*%fc[i])<0)
      {
        a[i+1]=a[i]
        b[i+1]=c[i]
        c[i+1]=(a[i+1]+b[i+1])/2
      }
      else
      {
        a[i+1]=c[i]
        b[i+1]=b[i]
        c[i+1]=(a[i+1]+b[i+1])/2
      }
    }
    cat("\n")
    cat("-----------------------------------------------------------------------","\n")
    cat("Nilai Taksiran V* : ","\n")
    print(c) 
    cat("-----------------------------------------------------------------------","\n")
    cat("\n")
  }
  else 
  {
    cat("tidak Ada Akar","\n")
  }
}

biseksi(66300000000,444728125000,0.0367334800,1,2,199990000000,2000000000000)


#Valuasi obligasi pada saat Pembayaran Kupon (T1)
valuation4<-function(A0,V1star,utang,K1,sig,T1,T2)
{
  library(pbivnorm)
  u1=(-(log(A0/V1star))+(((0.5*(sig^2)))*T1))/(sig*sqrt(T1))
  d1=(-(log(V1star/K1))+((-(0.5*(sig^2)))*(T2-T1)))/(sig*sqrt(T2-T1))
  d2=(-(log(V1star/K1))+(((0.5*(sig^2)))*(T2-T1)))/(sig*sqrt(T2-T1))
  rho=sqrt(T1/T2)
  Nu1=pnorm(u1)
  Nd21=pbivnorm(u1,d1,rho)
  Nd22=pbivnorm(u1,d2,rho)
  edf1=Nu1
  edf1persen=edf1*100
  
  cat("\n")
  cat("----------------------------------------------------------------","\n")
  cat("       Valuasi Obligasi Saat Pembayaran Kupon     ","\n")
  cat("----------------------------------------------------------------","\n")
  cat("Nilai awal aset                                 = ",A0,"\n")
  cat("V*                                              = ",V1star,"\n")
  cat("Nilai pembayaran saat T1                        = ",utang,"\n")
  cat("Nilai pembayaran saat T2                        = ",K1,"\n")
  cat("Volatilitas aset                                = ",sig,"\n")
  cat("Probabilitas kebangkrutan saat pembayaran kupon = ",edf1,"\n", "\t\t\t\n\t\t\t\t\t","atau",edf1persen,"%","\t\t","\n")
  cat("----------------------------------------------------------------","\n")
}
valuation4(784192878000000,624950200000,444728125000,66300000000,0.0367334800,1,2)


#Valuasi obligasi pada saat Jatuh Tempo (T2)
valuation5<-function(A0,A1star,utang,K1,sig,T1,T2)
{
  library(pbivnorm)
  U1=((-log(A0/A1star))+(((0.5*(sig^2)))*(T1)))/(sig*sqrt(T1))
  D3=((-log(A0/K1))+(((0.5*(sig^2)))*(T2)))/(sig*sqrt(T2))
  rho=sqrt(T1/T2)	
  NU1=pnorm(-U1)
  ND3<-pnorm(D3)
  N23=pbivnorm(-U1,-D3,rho)
  edf2=1-(N23/NU1)
  edf2persen=edf2*100
  
  cat("\n")
  cat("----------------------------------------------------------------","\n")
  cat("       Valuasi Obligasi Saat Jatuh Tempo     ","\n")
  cat("----------------------------------------------------------------","\n")
  cat("Nilai awal aset                             = ",A0,"\n")
  cat("V*                                          = ",A1star,"\n")
  cat("Nilai pembayaran saat T1                    = ",utang,"\n")
  cat("Nilai pembayaran saat T2                    = ",K1,"\n")
  cat("Volatilitas aset                            = ",sig,"\n")
  cat("Probabilitas Kebangkrutan Saat Jatuh Tempo  = ",edf2,"\n", "\t\t\t\n\t\t\t\t\t","atau",edf2persen,"%","\t\t","\n")
  cat("----------------------------------------------------------------","\n")
}
valuation5(784192878000000,624950200000,444728125000,66300000000,0.0367334800,1,2)

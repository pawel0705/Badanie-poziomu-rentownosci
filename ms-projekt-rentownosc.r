#### ZADANIE 1 ####

# WCZYTYWANIE DANYCH
rent <- read.csv2("dane_ms.csv", header = TRUE, sep=";")
rent94 <- as.vector(rent[[1]], mode = "double")
rent95 <- as.vector(rent[[2]], mode = "double")

# Liczba danych:
iloscRent94 <- length(rent94)
iloscRent95 <- length(rent95)

# Sumy wartoscii:
lacznaRent94 <- sum(rent94)
lacznaRent95 <- sum(rent95)

# wartosci maksymalne i minimalne:
maxRent94=max(rent94)
maxRent95=max(rent95)
minRent94=min(rent94)
minRent95=min(rent95)

##### MIARY POLOZENIA: #####

# srednia rentownosc¡:
sredniaRent94 <- lacznaRent94 / iloscRent94
sredniaRent95 <- lacznaRent95 / iloscRent95

# Funkcja obliczajaca mediane:
mediana <- function(a, poczatek, koniec) {
  a = sort(a)
  poczatek = ceiling(poczatek)
  koniec = ceiling(koniec)
  dlugosc <- koniec - poczatek + 1
  if(dlugosc %% 2 == 0)
    return((a[poczatek + dlugosc / 2 - 1] + a[poczatek + dlugosc / 2]) / 2)
  else
    return(a[poczatek + (dlugosc + 1) / 2 - 1])
}

# Kwartyl rzedu 1/2 (mediana):
medianaRent94 <- mediana(rent94, 1, iloscRent94)
medianaRent95 <- mediana(rent95, 1, iloscRent95)

# Kwartyl rzedu 1/4:
kwartyl14Rent94 <- mediana(rent94, 1, iloscRent94/2)
kwartyl14Rent95 <- mediana(rent95, 1, iloscRent95/2)

# Kwartyl rzedu 3/4:
kwartyl34Rent94 <- mediana(rent94, iloscRent94/2, iloscRent94)
kwartyl34Rent95 <- mediana(rent95, iloscRent95/2, iloscRent95)

##### MIARY ZROZNICOWANIA #####

# rostep
rozstepRent94=maxRent94-minRent94
rozstepRent95=maxRent95-minRent95

# rozstep miedzykwantylowy:
rozstepmkRent94 = kwartyl34Rent94 - kwartyl14Rent94
rozstepmkRent95 = kwartyl34Rent95 - kwartyl14Rent95

# Funkcja do obliczania momentu centralnego n-tego rzedu:
momentCentralny <- function(a, n) {
  liczba <- length(a)
  srednia <- sum(a) / liczba
  wynik <- 0
  for(i in 1:liczba) {
    wynik <- wynik + (a[i] - srednia) ^ n
  }
  wynik <- wynik / liczba
  return(wynik)
}

# Wariancja:
wariancjaRent94 <- momentCentralny(rent94, 2)
wariancjaRent95 <- momentCentralny(rent95, 2)

# Odchylenie standardowe:
odchStandRent94 <- sqrt(wariancjaRent94)
odchStandRent95 <- sqrt(wariancjaRent95)

# Wspolczynnik zmiennosci:
wspZmienRent94 <- odchStandRent94 / sredniaRent94
wspZmienRent95 <- odchStandRent95 / sredniaRent95



##### MIARY ASYMETRII #####

# skosnosc (Trzeci moment centralny)
skosRent94 <- momentCentralny(rent94, 3) / odchStandRent94 ^ 3
skosRent95 <- momentCentralny(rent95, 3) / odchStandRent95 ^ 3



##### MIARY SKUPIENIA #####

# Kurtoza
kurtozaRent94 <- momentCentralny(rent94, 4) / odchStandRent94 ^ 4
kurtozaRent95 <- momentCentralny(rent95, 4) / odchStandRent95 ^ 4


##### SZEREGI ROZDZIELCZE #####

# Ustalenie liczby klas:

liczbaKlas=sqrt(25) # Liczba obserwacji wynosi 25

# szerokosci przedzialow klasowych:

szerRent94 <- rozstepRent94 / liczbaKlas 
szerRent95 <- rozstepRent95 / liczbaKlas

# Punkty przeciecia:

punktPrzecRent94 = seq(minRent94, maxRent94, by=szerRent94)
punktPrzecRent95 = seq(minRent95, maxRent95, by=szerRent95)

# przedzialy
przedzialRent94 <- cut(rent94, punktPrzecRent94, right = FALSE, include.lowest = TRUE)
przedzialRent95 <- cut(rent95, punktPrzecRent94, right = FALSE, include.lowest = TRUE)

# Szereg rozdzielczy
szRozdzRent94 <- table(przedzialRent94)
szRozdzRent95 <- table(przedzialRent95)

# HISTOGRAMY
histogramRent94 = hist(rent94, breaks = punktPrzecRent94, col = "gray", main = "Rok 1994", xlab = "Rentownosc sprzedazy wyrobow w procentach", ylab = "Czestosc wystepowania", axes = F)
axis(2)
axis(1, at = seq(minRent94, maxRent94, by=szerRent94), labels = seq(minRent94, maxRent94, by = szerRent94))
histogramRent95 = hist(rent95, breaks = punktPrzecRent95, col = "gray", main = "Rok 1995", xlab = "Rentownosc sprzedazy wyrobow w procentach", ylab = "Czestosc wystepowania", axes = F)
axis(2)
axis(1, at = seq(minRent95, maxRent95, by=szerRent95), labels = seq(minRent95, maxRent95, by = szerRent95))



## Zadanie 2

#sortowanie danych
posortowaneDane<-sort(rent94)

#zmienne i wektory
n<-length(posortowaneDane)
standaryzacja <- posortowaneDane
dystrybuanta_empiryczna <- posortowaneDane
dystrybuanta_hipotetyczna <-posortowaneDane
roznica <- posortowaneDane
sredniaRent94<-mean(posortowaneDane)
odchStandRent94<-sd(posortowaneDane) #sd = wektor odchylenia standardowego
tablica_rozkladu = 0.264 #statystyka z tablicy rozkladu Kolmogorowa

#standaryzacja
for(i in 1:n){
  standaryzacja[i] <- (posortowaneDane[i]-sredniaRent94)/odchStandRent94
}
#dystrybuanta rozkladu hipotetycznego
for(i in 1:n){
  dystrybuanta_hipotetyczna[i] <- pnorm(standaryzacja[i]) #pnorm zwraca funkcje dystrybuanty
}
#dystrybuanta empiryczna
for(i in 1:n){
  dystrybuanta_empiryczna[i]<-i/length(posortowaneDane)
}
#roznica dystrybuant
for(i in 1:n)
{
  roznica[i] <- abs(dystrybuanta_hipotetyczna[i] - dystrybuanta_empiryczna[i])
}
#wartosc statystyki testowej
wartosc_statystyki_testowej <- max(roznica)


writeLines("H0 - rozklad normalny\n")
writeLines("H1 - brak rozkladu normalnego\n")

writeLines("Rentownosc w 1994:\n")
if(wartosc_statystyki_testowej < tablica_rozkladu || wartosc_statystyki_testowej> 1){
  writeLines("Brak podstaw do odrzucenia hipotezy H0\n")
  writeLines("Dane maja rozklad normalny\n")
}else{
  writeLines("Istnieja podstawy do odrzucenia hipotezy H0\n")
  writeLines("Dane nie maja rozkladu normalnego\n")
}

################################################################
###                        Rok 1995                          ###
################################################################

#sortowanie danych
posortowaneDane<-sort(rent95)

#zmienne i wektory
n<-length(posortowaneDane)
standaryzacja <- posortowaneDane
dystrybuanta_empiryczna <- posortowaneDane
dystrybuanta_hipotetyczna <-posortowaneDane
roznica <- posortowaneDane
sredniaRent95<-mean(posortowaneDane)
odchStandRent95<-sd(posortowaneDane) #sd = wektor odchylenia standardowego
tablica_rozkladu = 0.264 #statystyka z tablicy rozkladu Kolmogorowa

#standaryzacja
for(i in 1:n){
  standaryzacja[i] <- (posortowaneDane[i]-sredniaRent95)/odchStandRent95
}
#dystrybuanta rozkladu hipotetycznego
for(i in 1:n){
  dystrybuanta_hipotetyczna[i] <- pnorm(standaryzacja[i]) #pnorm zwraca funkcje dystrybuanty
}
#dystrybuanta empiryczna
for(i in 1:n){
  dystrybuanta_empiryczna[i]<-i/length(posortowaneDane)
}
#roznica dystrybuant
for(i in 1:n)
{
  roznica[i] <- abs(dystrybuanta_hipotetyczna[i] - dystrybuanta_empiryczna[i])
}
#wartosc statystyki testowej
wartosc_statystyki_testowej <- max(roznica)


writeLines("H0 - rozklad normalny\n")
writeLines("H1 - brak rozkladu normalnego\n")

writeLines("Rentownosc w roku 1995:\n")
if(wartosc_statystyki_testowej < tablica_rozkladu || wartosc_statystyki_testowej> 1){
  writeLines("Brak podstaw do odrzucenia hipotezy H0\n")
  writeLines("Dane maja rozklad normalny\n")
}else{
  writeLines("Istnieja podstawy do odrzucenia hipotezy H0\n")
  writeLines("Dane nie maja rozkladu normalnego\n")
}

#### ZADANIE 3 ####

# WspÃ³Å‚czynnik ufnoÅ›ci 1 - alfa = 0.98
# stÄ…d alfa = 0.02
# WartoÅ›Ä‡ tablicowa dla t(0.02, 25-1) wynosi 2.492
wspTStudenta <- function(WspUfn, n) {
  return(qt((1 - WspUfn)/2, n - 1, lower.tail = FALSE, log.p = FALSE))
}

# PrzedziaÅ‚ ufnoÅ›ci dla Å›redniej rentownosci w 94
dolnaGranicaSred <- function(srednia, wspTS, odchylenie, liczba) 
{
  dolnaGranica = srednia - wspTS * (odchylenie / sqrt(liczba - 1))
  return(dolnaGranica)
}

gornaGranicaSred <- function(srednia, wspTS, odchylenie, liczba) {
  gornaGranica = srednia + wspTS * (odchylenie / sqrt(liczba - 1))
  return(gornaGranica)
}

# WzglÄ™dna precyzja oszacowania
precyzjaSred <- function(gornaGranica, dolnaGranica, srednia){
  precyzjaOszacowania94 = 0.5 * (gornaGranica -dolnaGranica) / srednia
  return(precyzjaOszacowania94)
}

# PrzedziaÅ‚ ufnoÅ›ci dla rentownosci w 94
granicaDolna94 <- dolnaGranicaSred(sredniaRent94, wspTStudenta(0.98, 25), odchStandRent94, iloscRent94)
granicaGorna94 <- gornaGranicaSred(sredniaRent94, wspTStudenta(0.98, 25), odchStandRent94, iloscRent94)

# Precyzja oszacowania dla rentownosci w 94
precyzja94 <- precyzjaSred(granicaGorna94, granicaDolna94, sredniaRent94)

sprintf("Wzglêdna precyzja oszacowania wynosi %f%% \n", precyzja94*100)
if(precyzja94 <= 0.05 ){
  writeLines("Oszacowanie charakteryzuje siê du¿¹ dok³adnoœci¹. Bez problemu mo¿na uogólniæ przedzia³ ufnoœci na ca³¹ populacjê.\n")
}else if(precyzja94 <= 0.1){
  writeLines("Uogólniena otrzymanego przedzia³u ufnoœci na ca³¹ populacjê nale¿y dokonywaæ ostro¿nie.\n")
}else{
  writeLines("Nie mamy podstaw do uogólnienia otrzymanego przedzia³u ufnoœci na ca³¹ populacjê.\n")
}


### ZADANIE 4###
# Alfa = 0.02
# Wspó³czynnik ufnoœci 1 - alfa = 0.98
# Wartoœci tablicowe dla chikwadrat: chi(0.99,25-1)=10.8563, chi(0.01,25-1)=42.9798

#iloscRent95 wyliczone wyzej
#wariancjaRent95 wyliczona wyzej

wspChiKwadrat<-function(wspUfn,n){
  return (qchisq(wspUfn,n-1))
}
#Granice przedzia³u ufnoœci dla wariancji firm bran¿y metalowej (w 1995)
GranicaWar<-function(n,war,wspChi){
  return(n*war/wspChi)
}
#Wzglêdna precyzja oszacowania
wzgPrecyzjaWar<-function(dolnaGranica,gornaGranica,war){
  return(0.5*((gornaGranica-dolnaGranica)/war))
}

#Wspó³czynniki chiKwadrat dla: chi(0.99,24), chi(0.01,24)
wspAlfa<-0.02
ChiKwadrat1<-wspChiKwadrat(wspAlfa/2,iloscRent95) #podajemy dok³adn¹ wartoœæ, poniewa¿ zmniejszenie stopni swobody znajduje siê wewn¹trz cia³a funkcji
ChiKwadrat2<-wspChiKwadrat(1-(wspAlfa/2),iloscRent95)

#Przedzia³ ufnoœci dla firm bran¿y metalowej w 1995 r.
gornaGranicaWar<-GranicaWar(iloscRent95,wariancjaRent95,ChiKwadrat1)
dolnaGranicaWar<-GranicaWar(iloscRent95,wariancjaRent95,ChiKwadrat2)

#Precyzja oszacowania dla firm bran¿y metalowej w 1995 r.
precyzja95<-wzgPrecyzjaWar(dolnaGranicaWar,gornaGranicaWar,wariancjaRent95)


sprintf("Wzglêdna precyzja oszacowania wynosi %f%% \n", precyzja95*100)
if(precyzja95 <= 0.05 ){
  writeLines("Oszacowanie charakteryzuje siê du¿¹ dok³adnoœci¹. Bez problemu mo¿na uogólniæ przedzia³ ufnoœci na ca³¹ populacjê.\n")
}else if(precyzja95 <= 0.1){
  writeLines("Uogólniena otrzymanego przedzia³u ufnoœci na ca³¹ populacjê nale¿y dokonywaæ ostro¿nie.\n")
}else{
  writeLines("Nie mamy podstaw do uogólnienia otrzymanego przedzia³u ufnoœci na ca³¹ populacjê.\n")
}



#### ZADANIE 5 ####

#Model I Test istotnosci dla dwoch srednich 
#Dane sa dwie populacje generalne majace rozklady normalne przy czym ich odchylenia standardowe sa znane

#Hipoteza zerowa: m1 = m2
#Hipoteza alternatywna: m1 < m2

U <-(sredniaRent94 - sredniaRent95) / sqrt((wariancjaRent94/iloscRent94)+(wariancjaRent95/iloscRent95))

kwantyl95 <- qnorm(0.95)

sprintf("Obszar krytyczny dla m1 < m2 (-INF,-%s)", kwantyl95)
sprintf("Statystyka U = %s", U)
cat("Brak podstaw do odrzucenia hipotezy zerowej mowiacej o nie poprawieniu sredniej rentownosci firmy w 1995r.")





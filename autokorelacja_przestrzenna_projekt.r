# ============================================================
#  Autokorelacja przestrzenna - województwa 2004-2024
#  Test Morana I dla wszystkich zmiennych
# ============================================================

library(sf)
library(spdep)
library(ggplot2)
library(dplyr)

# ── Wczytanie danych ─────────────────────────────────────────
mapa <- st_read("wojewodztwa.shp")
dane <- read.csv("panel_wojewodztwa_2004_2024.csv", header = TRUE,
                 sep = ",", dec = ".", encoding = "UTF-8")

dane$teryt      <- sprintf("%02d", as.numeric(dane$teryt))
mapa$JPT_KOD_JE <- sprintf("%02d", as.numeric(mapa$JPT_KOD_JE))

# autokorelacja przestrzenna macierz odleglosci ekonomicznej
# Pakiety
library(readxl)
# 1. Wczytanie danych z Excela
data <- read_excel("projekt_eko.xlsx", sheet = "euklides")

# Sprawdzenie struktury
str(data)

# Zakładamy strukturę:
# kolumna 1 = wojewodztwo
# reszta = zmienne (m2, osoby itd.)

# 2. Standaryzacja zmiennych (z-score)
data_z <- data
data_z[ , -1] <- scale(data[ , -1])

# 3. Macierz cech (bez nazwy województwa)
X <- as.matrix(data_z[ , -1])
rownames(X) <- data_z[[1]]

# 4. Macierz odległości euklidesowej
dist_matrix <- as.matrix(dist(X, method = "euclidean"))

# 5. Zamiana na macierz wag (inverse distance)
W <- 1 / (1 + dist_matrix)

# 6. Diagonala = 0
diag(W) <- 0

# 7. Standaryzacja wierszowa
W <- W / rowSums(W)

# 8. Podgląd
print(round(W, 3))

library(spdep)

# zamiana macierzy na format spdep
lw <- mat2listw(W, style = "W", zero.policy = TRUE)

# Sprawdzenie kolejności - WAŻNE: mapa musi być posortowana tak samo jak dane
print(mapa$JPT_KOD_JE)
# Jeśli kolejność nie zgadza się z teryt w danych, posortuj:
mapa <- mapa[order(mapa$JPT_KOD_JE), ]

# ============================================================
#  FUNKCJA: test Morana dla wybranego roku i zmiennej
# ============================================================
test_moran <- function(zmienna, rok_wybrany, dane, lw) {
  df_rok <- dane %>%
    filter(rok == rok_wybrany) %>%
    arrange(teryt)               # kolejność musi odpowiadać mapie!
  
  x <- df_rok[[zmienna]]
  
  cat(sprintf("\n--- %s | Rok: %d ---\n", zmienna, rok_wybrany))
  wynik <- moran.test(x, lw, alternative = "two.sided", zero.policy = TRUE)
  print(wynik)
  return(wynik)
}

# ============================================================
#  1. ZUŻYCIE ENERGII ELEKTRYCZNEJ
# ============================================================
# Wyniki: I ≈ -0.27, p > 0.10 we wszystkich latach → BRAK autokorelacji
# Interpretacja: rozkład zużycia energii między województwami jest LOSOWY -
# sąsiadujące województwa nie są do siebie podobne pod względem zużycia.
# Wynika to ze struktury demograficzno-przemysłowej: duże województwa
# (Mazowieckie, Śląskie) sąsiadują z mniejszymi, co wygasza wzorzec przestrzenny.

ROK <- 2015   # <-- zmień na dowolny rok 2004-2024

dane_rok <- dane %>% filter(rok == ROK) %>% arrange(teryt)

moran.test(dane_rok$zuzycie_energii_GWh, lw,
           alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$zuzycie_energii_GWh, lw,
           labels    = dane_rok$wojewodztwo,
           pch       = 20,
           col       = "#2166ac",
           main      = paste("Wykres Morana - Zużycie energii elektrycznej [GWh] -", ROK),
           xlab      = "Zużycie energii (standaryzowane)",
           ylab      = "Przestrzenne opóźnienie zużycia energii")

# ============================================================
#  2. CENA ENERGII ELEKTRYCZNEJ
# ============================================================
# Wyniki: I rośnie z -0.10 (2004) do +0.50 (2024)
# Od 2008 silna dodatnia autokorelacja (p < 0.01 ***)
# Interpretacja: sąsiadujące województwa mają coraz bardziej PODOBNE ceny energii.
# To odzwierciedla strukturę rynku - obszary obsługiwane przez tego samego
# dystrybutora (np. Tauron na południu, Energa na północy) tworzą skupienia
# przestrzenne. Wzrost I w czasie sugeruje konsolidację rynku energetycznego.

moran.test(dane_rok$cena_energii_zl_kWh, lw,
           alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$cena_energii_zl_kWh, lw,
           labels    = dane_rok$wojewodztwo,
           pch       = 20,
           col       = "#d6604d",
           main      = paste("Wykres Morana - Cena energii elektrycznej [zł/kWh] -", ROK),
           xlab      = "Cena energii (standaryzowana)",
           ylab      = "Przestrzenne opóźnienie ceny energii")

# ============================================================
#  3. PKB
# ============================================================
# Wyniki: I ≈ -0.28, marginalnie istotne (*) od 2012
# Interpretacja: SŁABA UJEMNA autokorelacja - województwa z wysokim PKB
# (Mazowieckie, Śląskie, Dolnośląskie) sąsiadują z województwami o niższym PKB.
# Klasyczny wzorzec centrum-peryferie: "wyspa bogactwa" w otoczeniu biedniejszych sąsiadów.
# Wartość ujemna I może wskazywać na efekt konkurencji przestrzennej lub
# "efekt metropolitalny" - duże ośrodki wysysają zasoby z sąsiednich regionów.

moran.test(dane_rok$pkb_mln_zl, lw,
           alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$pkb_mln_zl, lw,
           labels    = dane_rok$wojewodztwo,
           pch       = 20,
           col       = "#4dac26",
           main      = paste("Wykres Morana - PKB [mln zł] -", ROK),
           xlab      = "PKB (standaryzowany)",
           ylab      = "Przestrzenne opóźnienie PKB")

# ============================================================
#  4. LUDNOŚĆ
# ============================================================
# Wyniki: I ≈ -0.30, brak istotności (p > 0.10)
# Interpretacja: rozkład ludności między województwami jest LOSOWY przestrzennie.
# Duże województwa (Mazowieckie, Śląskie) są otoczone przez mniejsze,
# co generuje ujemną wartość I, ale efekt jest nieistotny statystycznie.

moran.test(dane_rok$ludnosc, lw,
           alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$ludnosc, lw,
           labels    = dane_rok$wojewodztwo,
           pch       = 20,
           col       = "#7b3294",
           main      = paste("Wykres Morana - Liczba ludności -", ROK),
           xlab      = "Ludność (standaryzowana)",
           ylab      = "Przestrzenne opóźnienie ludności")

# ============================================================
#  5. URBANIZACJA
# ============================================================
# Wyniki: I ≈ +0.20, marginalnie istotne (*) w większości lat
# Interpretacja: SŁABA DODATNIA autokorelacja - sąsiadujące województwa
# mają podobny poziom urbanizacji. Skupienia wysoko zurbanizowanych
# województw na południu (Śląskie-Małopolskie) i Mazowieckie tworzą wzorce.
# Urbanizacja jest zjawiskiem historycznym i zmienia się powoli, stąd
# stabilność przestrzenna wskaźnika.

moran.test(dane_rok$urbanizacja_pct, lw,
           alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$urbanizacja_pct, lw,
           labels    = dane_rok$wojewodztwo,
           pch       = 20,
           col       = "#e08214",
           main      = paste("Wykres Morana - Urbanizacja [%] -", ROK),
           xlab      = "Urbanizacja (standaryzowana)",
           ylab      = "Przestrzenne opóźnienie urbanizacji")

# ============================================================
#  6. HDD - STOPNIODNI GRZEWCZE
# ============================================================
# Wyniki: I ≈ +0.53, SILNA ISTOTNA autokorelacja (p < 0.001 ***) we WSZYSTKICH latach
# Interpretacja: NAJSILNIEJSZA autokorelacja ze wszystkich zmiennych.
# Sąsiadujące województwa mają bardzo podobne warunki termiczne - klimat
# jest zjawiskiem ciągłym geograficznie i zmienia się gradientami.
# Warmia i Mazury/Podlaskie (mroźniejsze) tworzą skupienie wysokich HDD na NE,
# a Dolnośląskie/Opolskie (łagodniejsze) skupienie niskich HDD na SW.
# To czyni HDD idealną zmienną w modelu przestrzennym - ma silną strukturę przestrzenną.

moran.test(dane_rok$hdd, lw,
           alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$hdd, lw,
           labels    = dane_rok$wojewodztwo,
           pch       = 20,
           col       = "#081d58",
           main      = paste("Wykres Morana - HDD -", ROK),
           xlab      = "HDD (standaryzowane)",
           ylab      = "Przestrzenne opóźnienie HDD")

# ============================================================
#  7. CDD - STOPNIODNI CHŁODZENIA
# ============================================================
# Wyniki: I ≈ +0.35 średnio, zmienna w czasie (od 0.04 do 0.65)
# Istotne statystycznie w ~80% lat
# Interpretacja: UMIARKOWANA, zmienna autokorelacja.
# W Polsce CDD jest małe i silnie zależne od konkretnego przebiegu lata -
# gorące lata (2012, 2015, 2019) tworzą wyraźniejszy wzorzec przestrzenny
# (południe cieplejsze), zimne lata - wzorzec zanika.
# Wysoka zmienność I w czasie odzwierciedla losowość ekstremalnych upałów.

moran.test(dane_rok$cdd, lw,
           alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$cdd, lw,
           labels    = dane_rok$wojewodztwo,
           pch       = 20,
           col       = "#67001f",
           main      = paste("Wykres Morana - CDD -", ROK),
           xlab      = "CDD (standaryzowane)",
           ylab      = "Przestrzenne opóźnienie CDD")

#DOCHÓD
# Wyniki: I ≈ +0.02, brak istotności (p > 0.10)
# Interpretacja: rozkład dochodu na osobę między województwami jest LOSOWY przestrzennie.
# w polsce dochody są zróżnicowane, ale nie tworzą wyraźnych skupisk przestrzennych


# ============================================================
#  PĘTLA: Moran I dla wszystkich zmiennych i wszystkich lat
# ============================================================
zmienne <- c("zuzycie_energii_GWh", "cena_energii_zl_kWh",
             "pkb_mln_zl", "ludnosc", "urbanizacja_pct", "hdd", "cdd")

lata    <- 2004:2024
wyniki  <- data.frame()

for (zm in zmienne) {
  for (yr in lata) {
    df_yr <- dane %>% filter(rok == yr) %>% arrange(teryt)
    test  <- moran.test(df_yr[[zm]], lw,
                        alternative = "two.sided", zero.policy = TRUE)
    wyniki <- rbind(wyniki, data.frame(
      zmienna  = zm,
      rok      = yr,
      moran_I  = round(test$estimate["Moran I statistic"], 4),
      p_value  = round(test$p.value, 4),
      istotna  = test$p.value < 0.05
    ))
  }
}

# Wykres zmiany I w czasie dla wszystkich zmiennych
ggplot(wyniki, aes(x = rok, y = moran_I, color = zmienna)) +
  geom_line(linewidth = 0.9) +
  geom_point(aes(shape = istotna), size = 2.5) +
  scale_shape_manual(values = c(1, 16),
                     labels = c("p ≥ 0.05", "p < 0.05"),
                     name   = "Istotność") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_x_continuous(breaks = seq(2004, 2024, 2)) +
  labs(
    title    = "Statystyka I Morana w czasie (2004–2024)",
    subtitle = "Wypełniony punkt = istotne statystycznie (p < 0.05)",
    x        = "Rok",
    y        = "Moran's I",
    color    = "Zmienna"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position  = "right"
  )

# Tabela podsumowująca
library(dplyr)
wyniki %>%
  group_by(zmienna) %>%
  summarise(
    I_srednie    = round(mean(moran_I), 4),
    I_min        = round(min(moran_I),  4),
    I_max        = round(max(moran_I),  4),
    pct_istotnych = paste0(round(mean(istotna) * 100), "%")
  ) %>%
  print()

library(plm)

pdata <- pdata.frame(dane, index = c("teryt", "rok"))

model_fe <- plm(
  zuzycie_energii_GWh ~ dochod_os + urbanizacja_pct + cena_energii_zl_kWh + hdd + cdd,
  data = pdata,
  model = "within"
)

summary(model_fe)




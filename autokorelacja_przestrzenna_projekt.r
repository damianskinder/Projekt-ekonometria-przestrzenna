# ============================================================
#  Autokorelacja przestrzenna - województwa 2004-2024
#  Test Morana I dla wszystkich zmiennych
# ============================================================

library(sf)
library(spdep)
library(ggplot2)
library(dplyr)

mapa <- st_read("wojewodztwa.shp")
dane <- read.csv("panel_wojewodztwa_2004_2024.csv", header = TRUE,
                 sep = ",", dec = ".", encoding = "UTF-8")

dane$teryt      <- sprintf("%02d", as.numeric(dane$teryt))
mapa$JPT_KOD_JE <- sprintf("%02d", as.numeric(mapa$JPT_KOD_JE))

mapa <- mapa[order(mapa$JPT_KOD_JE), ]

# ── Macierz wag przestrzennych — sąsiedztwo Queen ────────────
nb <- poly2nb(mapa, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# ============================================================
#  FUNKCJA: test Morana dla wybranego roku i zmiennej
# ============================================================
test_moran <- function(zmienna, rok_wybrany, dane, lw) {
  df_rok <- dane %>% filter(rok == rok_wybrany) %>% arrange(teryt)
  x <- df_rok[[zmienna]]
  cat(sprintf("\n--- %s | Rok: %d ---\n", zmienna, rok_wybrany))
  wynik <- moran.test(x, lw, alternative = "two.sided", zero.policy = TRUE)
  print(wynik)
  return(wynik)
}

ROK <- 2015

dane_rok <- dane %>% filter(rok == ROK) %>% arrange(teryt)

# ============================================================
#  1. ZUŻYCIE ENERGII ELEKTRYCZNEJ
# ============================================================
moran.test(dane_rok$zuzycie_energii_GWh, lw, alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$zuzycie_energii_GWh, lw, labels = dane_rok$wojewodztwo,
           pch = 20, col = "#2166ac",
           main = paste("Wykres Morana - Zużycie energii elektrycznej [GWh] -", ROK),
           xlab = "Zużycie energii (standaryzowane)",
           ylab = "Przestrzenne opóźnienie zużycia energii")

# ============================================================
#  2. CENA ENERGII ELEKTRYCZNEJ
# ============================================================
moran.test(dane_rok$cena_energii_zl_kWh, lw, alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$cena_energii_zl_kWh, lw, labels = dane_rok$wojewodztwo,
           pch = 20, col = "#d6604d",
           main = paste("Wykres Morana - Cena energii elektrycznej [zł/kWh] -", ROK),
           xlab = "Cena energii (standaryzowana)",
           ylab = "Przestrzenne opóźnienie ceny energii")

# ============================================================
#  3. PKB
# ============================================================
moran.test(dane_rok$pkb_mln_zl, lw, alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$pkb_mln_zl, lw, labels = dane_rok$wojewodztwo,
           pch = 20, col = "#4dac26",
           main = paste("Wykres Morana - PKB [mln zł] -", ROK),
           xlab = "PKB (standaryzowany)", ylab = "Przestrzenne opóźnienie PKB")

# ============================================================
#  4. LUDNOŚĆ
# ============================================================
moran.test(dane_rok$ludnosc, lw, alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$ludnosc, lw, labels = dane_rok$wojewodztwo,
           pch = 20, col = "#7b3294",
           main = paste("Wykres Morana - Liczba ludności -", ROK),
           xlab = "Ludność (standaryzowana)", ylab = "Przestrzenne opóźnienie ludności")

# ============================================================
#  5. URBANIZACJA
# ============================================================
moran.test(dane_rok$urbanizacja_pct, lw, alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$urbanizacja_pct, lw, labels = dane_rok$wojewodztwo,
           pch = 20, col = "#e08214",
           main = paste("Wykres Morana - Urbanizacja [%] -", ROK),
           xlab = "Urbanizacja (standaryzowana)", ylab = "Przestrzenne opóźnienie urbanizacji")

# ============================================================
#  6. HDD
# ============================================================
moran.test(dane_rok$hdd, lw, alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$hdd, lw, labels = dane_rok$wojewodztwo,
           pch = 20, col = "#081d58",
           main = paste("Wykres Morana - HDD -", ROK),
           xlab = "HDD (standaryzowane)", ylab = "Przestrzenne opóźnienie HDD")

# ============================================================
#  7. CDD
# ============================================================
moran.test(dane_rok$cdd, lw, alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$cdd, lw, labels = dane_rok$wojewodztwo,
           pch = 20, col = "#67001f",
           main = paste("Wykres Morana - CDD -", ROK),
           xlab = "CDD (standaryzowane)", ylab = "Przestrzenne opóźnienie CDD")

# ============================================================
#  PĘTLA: Moran I dla wszystkich zmiennych i lat
# ============================================================
zmienne <- c("zuzycie_energii_GWh", "cena_energii_zl_kWh",
             "pkb_mln_zl", "ludnosc", "urbanizacja_pct", "hdd", "cdd")

lata   <- 2004:2024
wyniki <- data.frame()

for (zm in zmienne) {
  for (yr in lata) {
    df_yr <- dane %>% filter(rok == yr) %>% arrange(teryt)
    test  <- moran.test(df_yr[[zm]], lw, alternative = "two.sided", zero.policy = TRUE)
    wyniki <- rbind(wyniki, data.frame(
      zmienna = zm, rok = yr,
      moran_I = round(test$estimate["Moran I statistic"], 4),
      p_value = round(test$p.value, 4),
      istotna = test$p.value < 0.05
    ))
  }
}

ggplot(wyniki, aes(x = rok, y = moran_I, color = zmienna)) +
  geom_line(linewidth = 0.9) + geom_point(aes(shape = istotna), size = 2.5) +
  scale_shape_manual(values = c(1, 16), labels = c("p ≥ 0.05", "p < 0.05"), name = "Istotność") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_x_continuous(breaks = seq(2004, 2024, 2)) +
  labs(title = "Statystyka I Morana w czasie (2004–2024)",
       subtitle = "Wypełniony punkt = istotne statystycznie (p < 0.05)",
       x = "Rok", y = "Moran's I", color = "Zmienna") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

library(dplyr)
wyniki %>%
  group_by(zmienna) %>%
  summarise(I_srednie     = round(mean(moran_I), 4),
            I_min         = round(min(moran_I),  4),
            I_max         = round(max(moran_I),  4),
            pct_istotnych = paste0(round(mean(istotna) * 100), "%")) %>%
  print()

library(plm)

pdata    <- pdata.frame(dane, index = c("teryt", "rok"))
model_fe <- plm(
  zuzycie_energii_GWh ~ dochod_os + urbanizacja_pct + cena_energii_zl_kWh + hdd + cdd,
  data = pdata, model = "within"
)

summary(model_fe)

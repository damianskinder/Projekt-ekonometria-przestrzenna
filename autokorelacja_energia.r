# ============================================================
#  Autokorelacja przestrzenna — zuzycie energii elektrycznej
#  Dane: panel_wojewodztwa_2004_2024.csv | N=16 T=2004-2024
#  Zawiera: test Morana I (rok wybrany + petla 2004-2024),
#           wykresy Morana, model FE
# ============================================================

library(sf)
library(spdep)
library(ggplot2)
library(dplyr)
library(plm)

dane <- read.csv("panel_wojewodztwa_2004_2024.csv", header = TRUE,
                 sep = ",", dec = ".", encoding = "UTF-8")
dane$teryt <- sprintf("%02d", as.numeric(dane$teryt))

mapa <- st_read("wojewodztwa.shp", quiet = TRUE)
mapa$JPT_KOD_JE <- sprintf("%02d", as.numeric(mapa$JPT_KOD_JE))
mapa <- mapa[order(mapa$JPT_KOD_JE), ]

# ── Macierz wag przestrzennych — sasiedztwo Queen ────────────
nb <- poly2nb(mapa, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

ROK      <- 2015
dane_rok <- dane %>% filter(rok == ROK) %>% arrange(teryt)

# ============================================================
#  1. ZUZYCIE ENERGII ELEKTRYCZNEJ
# ============================================================
moran.test(dane_rok$zuzycie_energii_GWh, lw, alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$zuzycie_energii_GWh, lw, labels = dane_rok$wojewodztwo,
           pch = 20, col = "#2166ac",
           main = paste("Wykres Morana - Zuzycie energii elektrycznej [GWh] -", ROK),
           xlab = "Zuzycie energii (standaryzowane)",
           ylab = "Przestrzenne opoznienie zuzycia energii")

# ============================================================
#  2. CENA ENERGII ELEKTRYCZNEJ
# ============================================================
moran.test(dane_rok$cena_energii_zl_kWh, lw, alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$cena_energii_zl_kWh, lw, labels = dane_rok$wojewodztwo,
           pch = 20, col = "#d6604d",
           main = paste("Wykres Morana - Cena energii elektrycznej [zl/kWh] -", ROK),
           xlab = "Cena energii (standaryzowana)",
           ylab = "Przestrzenne opoznienie ceny energii")

# ============================================================
#  3. PKB
# ============================================================
moran.test(dane_rok$pkb_mln_zl, lw, alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$pkb_mln_zl, lw, labels = dane_rok$wojewodztwo,
           pch = 20, col = "#4dac26",
           main = paste("Wykres Morana - PKB [mln zl] -", ROK),
           xlab = "PKB (standaryzowany)", ylab = "Przestrzenne opoznienie PKB")

# ============================================================
#  4. LUDNOSC
# ============================================================
moran.test(dane_rok$ludnosc, lw, alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$ludnosc, lw, labels = dane_rok$wojewodztwo,
           pch = 20, col = "#7b3294",
           main = paste("Wykres Morana - Liczba ludnosci -", ROK),
           xlab = "Ludnosc (standaryzowana)", ylab = "Przestrzenne opoznienie ludnosci")

# ============================================================
#  5. URBANIZACJA
# ============================================================
moran.test(dane_rok$urbanizacja_pct, lw, alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$urbanizacja_pct, lw, labels = dane_rok$wojewodztwo,
           pch = 20, col = "#e08214",
           main = paste("Wykres Morana - Urbanizacja [%] -", ROK),
           xlab = "Urbanizacja (standaryzowana)", ylab = "Przestrzenne opoznienie urbanizacji")

# ============================================================
#  6. HDD
# ============================================================
moran.test(dane_rok$hdd, lw, alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$hdd, lw, labels = dane_rok$wojewodztwo,
           pch = 20, col = "#081d58",
           main = paste("Wykres Morana - HDD -", ROK),
           xlab = "HDD (standaryzowane)", ylab = "Przestrzenne opoznienie HDD")

# ============================================================
#  7. CDD
# ============================================================
moran.test(dane_rok$cdd, lw, alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$cdd, lw, labels = dane_rok$wojewodztwo,
           pch = 20, col = "#67001f",
           main = paste("Wykres Morana - CDD -", ROK),
           xlab = "CDD (standaryzowane)", ylab = "Przestrzenne opoznienie CDD")

# ============================================================
#  PETLA: Moran I dla wszystkich zmiennych i lat 2004-2024
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
  scale_shape_manual(values = c(1, 16), labels = c("p >= 0.05", "p < 0.05"), name = "Istotnosc") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_x_continuous(breaks = seq(2004, 2024, 2)) +
  labs(title = "Statystyka I Morana w czasie (2004-2024)",
       subtitle = "Wypelniony punkt = istotne statystycznie (p < 0.05)",
       x = "Rok", y = "Moran's I", color = "Zmienna") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

wyniki %>%
  group_by(zmienna) %>%
  summarise(I_srednie     = round(mean(moran_I), 4),
            I_min         = round(min(moran_I),  4),
            I_max         = round(max(moran_I),  4),
            pct_istotnych = paste0(round(mean(istotna) * 100), "%")) %>%
  print()

# ============================================================
#  Model panelowy FE (weryfikacja wskaznikow resztowych)
# ============================================================
pdata    <- pdata.frame(dane, index = c("teryt", "rok"))
model_fe <- plm(
  zuzycie_energii_GWh ~ dochod_os + urbanizacja_pct + cena_energii_zl_kWh + hdd + cdd,
  data = pdata, model = "within"
)

summary(model_fe)

# Test Morana na resztach FE (przekrojowo dla wybranego roku)
reszty_fe <- residuals(model_fe)
dane$reszty_fe <- as.numeric(reszty_fe)

dane_rok_res <- dane %>% filter(rok == ROK) %>% arrange(teryt)
moran.test(dane_rok_res$reszty_fe, lw, alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok_res$reszty_fe, lw, labels = dane_rok_res$wojewodztwo,
           pch = 20, col = "#2166ac",
           main = paste("Wykres Morana - Reszty modelu FE -", ROK),
           xlab = "Reszty FE (standaryzowane)",
           ylab = "Przestrzenne opoznienie reszt FE")

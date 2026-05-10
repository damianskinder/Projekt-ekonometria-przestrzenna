# ============================================================
#  Autokorelacja przestrzenna - nowe dane - województwa 2004-2024
#  Test Morana I dla wszystkich zmiennych
# ============================================================

library(sf)
library(spdep)
library(ggplot2)
library(dplyr)
library(readxl)
library(plm)

SCIEZKA_DANE <- "nowe_dane.xlsx"

mapa <- st_read("wojewodztwa.shp")
dane <- read_excel(SCIEZKA_DANE, sheet = "Ludnosc_25_34")
names(dane)[names(dane) == "WSK25-34"] <- "WSK25_34"

dane$teryt      <- sprintf("%02d", as.numeric(dane$teryt))
mapa$JPT_KOD_JE <- sprintf("%02d", as.numeric(mapa$JPT_KOD_JE))
mapa <- mapa[order(mapa$JPT_KOD_JE), ]

# ── Macierz wag przestrzennych — sąsiedztwo Queen ────────────
nb <- poly2nb(mapa, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# ── Etykiety zmiennych ────────────────────────────────────────
etykiety <- c(
  "MO"       = "Mieszkania oddane do użytkowania",
  "WSK25_34" = "Ludność w wieku 25-34 lat",
  "WSK_URB"  = "Wskaźnik urbanizacji [%]",
  "NAKŁ"     = "Nakłady inwestycyjne w sektorze prywatnym",
  "WYNAGR"   = "Średnie wynagrodzenie [zł]",
  "SM"       = "Saldo migracji"
)

zmienne <- names(etykiety)
lata    <- sort(unique(dane$rok))

ROK      <- 2015
dane_rok <- dane %>% filter(rok == ROK) %>% arrange(teryt)

# ── 1. Mieszkania oddane do użytkowania ──────────────────────
moran.test(dane_rok$MO, lw, alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$MO, lw, labels = dane_rok$wojewodztwo, pch = 20, col = "#2166ac",
           main = paste("Wykres Morana - Mieszkania oddane do użytkowania -", ROK),
           xlab = "Mieszkania oddane (standaryzowane)",
           ylab = "Przestrzenne opóźnienie mieszkań oddanych")

# ── 2. Ludność w wieku 25-34 lat ─────────────────────────────
moran.test(dane_rok[["WSK25_34"]], lw, alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok[["WSK25_34"]], lw, labels = dane_rok$wojewodztwo, pch = 20, col = "#d6604d",
           main = paste("Wykres Morana - Ludność w wieku 25-34 lat -", ROK),
           xlab = "Ludność 25-34 (standaryzowana)",
           ylab = "Przestrzenne opóźnienie ludności 25-34")

# ── 3. Wskaźnik urbanizacji ──────────────────────────────────
moran.test(dane_rok$WSK_URB, lw, alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$WSK_URB, lw, labels = dane_rok$wojewodztwo, pch = 20, col = "#4dac26",
           main = paste("Wykres Morana - Wskaźnik urbanizacji -", ROK),
           xlab = "Urbanizacja (standaryzowana)",
           ylab = "Przestrzenne opóźnienie urbanizacji")

# ── 4. Nakłady inwestycyjne ───────────────────────────────────
moran.test(dane_rok[["NAKŁ"]], lw, alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok[["NAKŁ"]], lw, labels = dane_rok$wojewodztwo, pch = 20, col = "#7b3294",
           main = paste("Wykres Morana - Nakłady inwestycyjne w sektorze prywatnym -", ROK),
           xlab = "Nakłady (standaryzowane)",
           ylab = "Przestrzenne opóźnienie nakładów")

# ── 5. Średnie wynagrodzenie ─────────────────────────────────
moran.test(dane_rok$WYNAGR, lw, alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$WYNAGR, lw, labels = dane_rok$wojewodztwo, pch = 20, col = "#e08214",
           main = paste("Wykres Morana - Średnie wynagrodzenie -", ROK),
           xlab = "Wynagrodzenie (standaryzowane)",
           ylab = "Przestrzenne opóźnienie wynagrodzenia")

# ── 6. Saldo migracji ────────────────────────────────────────
moran.test(dane_rok$SM, lw, alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$SM, lw, labels = dane_rok$wojewodztwo, pch = 20, col = "#081d58",
           main = paste("Wykres Morana - Saldo migracji -", ROK),
           xlab = "Saldo migracji (standaryzowane)",
           ylab = "Przestrzenne opóźnienie salda migracji")

# ============================================================
#  PĘTLA: Moran I dla wszystkich zmiennych i lat
# ============================================================
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
  scale_x_continuous(breaks = seq(min(lata), max(lata), 2)) +
  scale_color_discrete(labels = etykiety) +
  labs(title = "Statystyka I Morana w czasie (2004–2024)",
       subtitle = "Wypełniony punkt = istotne statystycznie (p < 0.05)",
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
#  Model panelowy FE
# ============================================================
pdata    <- pdata.frame(as.data.frame(dane), index = c("teryt", "rok"))
model_fe <- plm(
  MO ~ WSK_URB + `NAKŁ` + WYNAGR + SM + WSK25_34,
  data = pdata, model = "within"
)

summary(model_fe)

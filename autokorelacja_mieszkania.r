# ============================================================
#  Autokorelacja przestrzenna — mieszkania oddane do uzytkowania
#  Dane: nowe_dane.xlsx (sheet: Ludnosc_25_34) | N=16 T=2004-2024
#  Zawiera: test Morana I (rok wybrany + petla 2004-2024),
#           wykresy Morana, model FE
# ============================================================

library(sf)
library(spdep)
library(ggplot2)
library(dplyr)
library(readxl)
library(plm)

dane <- read_excel("nowe_dane.xlsx", sheet = "Ludnosc_25_34")
names(dane)[names(dane) == "WSK25-34"]     <- "WSK25_34"
names(dane)[grep("^NAK", names(dane))]     <- "NAKL"
dane <- as.data.frame(dane)
dane$teryt <- sprintf("%02d", as.numeric(dane$teryt))

mapa <- st_read("wojewodztwa.shp", quiet = TRUE)
mapa$JPT_KOD_JE <- sprintf("%02d", as.numeric(mapa$JPT_KOD_JE))
mapa <- mapa[order(mapa$JPT_KOD_JE), ]

# ── Macierz wag przestrzennych — sasiedztwo Queen ────────────
nb <- poly2nb(mapa, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

etykiety <- c(
  "MO"       = "Mieszkania oddane do uzytkowania",
  "WSK25_34" = "Ludnosc w wieku 25-34 lat",
  "WSK_URB"  = "Wskaznik urbanizacji [%]",
  "NAKL"     = "Naklady inwestycyjne w sektorze prywatnym",
  "WYNAGR"   = "Srednie wynagrodzenie [zl]",
  "SM"       = "Saldo migracji"
)

zmienne  <- names(etykiety)
lata     <- sort(unique(dane$rok))

ROK      <- 2015
dane_rok <- dane %>% filter(rok == ROK) %>% arrange(teryt)

# ============================================================
#  1. MIESZKANIA ODDANE DO UZYTKOWANIA
# ============================================================
moran.test(dane_rok$MO, lw, alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$MO, lw, labels = dane_rok$wojewodztwo, pch = 20, col = "#2166ac",
           main = paste("Wykres Morana - Mieszkania oddane do uzytkowania -", ROK),
           xlab = "Mieszkania oddane (standaryzowane)",
           ylab = "Przestrzenne opoznienie mieszkan oddanych")

# ============================================================
#  2. LUDNOSC W WIEKU 25-34 LAT
# ============================================================
moran.test(dane_rok$WSK25_34, lw, alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$WSK25_34, lw, labels = dane_rok$wojewodztwo, pch = 20, col = "#d6604d",
           main = paste("Wykres Morana - Ludnosc w wieku 25-34 lat -", ROK),
           xlab = "Ludnosc 25-34 (standaryzowana)",
           ylab = "Przestrzenne opoznienie ludnosci 25-34")

# ============================================================
#  3. WSKAZNIK URBANIZACJI
# ============================================================
moran.test(dane_rok$WSK_URB, lw, alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$WSK_URB, lw, labels = dane_rok$wojewodztwo, pch = 20, col = "#4dac26",
           main = paste("Wykres Morana - Wskaznik urbanizacji -", ROK),
           xlab = "Urbanizacja (standaryzowana)",
           ylab = "Przestrzenne opoznienie urbanizacji")

# ============================================================
#  4. NAKLADY INWESTYCYJNE
# ============================================================
moran.test(dane_rok$NAKL, lw, alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$NAKL, lw, labels = dane_rok$wojewodztwo, pch = 20, col = "#7b3294",
           main = paste("Wykres Morana - Naklady inwestycyjne w sektorze prywatnym -", ROK),
           xlab = "Naklady (standaryzowane)",
           ylab = "Przestrzenne opoznienie nakladow")

# ============================================================
#  5. SREDNIE WYNAGRODZENIE
# ============================================================
moran.test(dane_rok$WYNAGR, lw, alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$WYNAGR, lw, labels = dane_rok$wojewodztwo, pch = 20, col = "#e08214",
           main = paste("Wykres Morana - Srednie wynagrodzenie -", ROK),
           xlab = "Wynagrodzenie (standaryzowane)",
           ylab = "Przestrzenne opoznienie wynagrodzenia")

# ============================================================
#  6. SALDO MIGRACJI
# ============================================================
moran.test(dane_rok$SM, lw, alternative = "two.sided", zero.policy = TRUE)

moran.plot(dane_rok$SM, lw, labels = dane_rok$wojewodztwo, pch = 20, col = "#081d58",
           main = paste("Wykres Morana - Saldo migracji -", ROK),
           xlab = "Saldo migracji (standaryzowane)",
           ylab = "Przestrzenne opoznienie salda migracji")

# ============================================================
#  PETLA: Moran I dla wszystkich zmiennych i lat
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
  scale_shape_manual(values = c(1, 16), labels = c("p >= 0.05", "p < 0.05"), name = "Istotnosc") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_x_continuous(breaks = seq(min(lata), max(lata), 2)) +
  scale_color_discrete(labels = etykiety) +
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
  MO ~ WSK_URB + NAKL + WYNAGR + SM + WSK25_34,
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

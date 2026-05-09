# ============================================================
#  Wizualizacje - nowe dane - województwa 2004-2024
# ============================================================

library(ggplot2)
library(gganimate)
library(reshape2)
library(corrplot)
library(gifski)
library(readxl)
library(dplyr)

SCIEZKA_DANE <- "C:/Users/Patryk/Desktop/dane projekty/nowe dane/nowe_dane.xlsx"

dane <- read_excel(SCIEZKA_DANE, sheet = "Ludnosc_25_34")

# Paleta kolorów dla 16 województw
paleta_woj <- c(
  "#1f77b4","#ff7f0e","#2ca02c","#d62728","#9467bd",
  "#8c564b","#e377c2","#7f7f7f","#bcbd22","#17becf",
  "#aec7e8","#ffbb78","#98df8a","#ff9896","#c5b0d5","#c49c94"
)

# ============================================================
#  1. WYKRESY LINIOWE
# ============================================================

# --- Mieszkania oddane do użytkowania ---
ggplot(dane, aes(x = rok, y = MO, color = wojewodztwo)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2) +
  scale_color_manual(values = paleta_woj) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2)) +
  labs(
    title    = "Mieszkania oddane do użytkowania",
    subtitle = "Województwa, 2004–2024",
    x        = "Rok",
    y        = "Liczba mieszkań",
    color    = "Województwo"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "right",
    legend.text      = element_text(size = 8),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold")
  )

# --- Ludność w wieku 25-34 lat ---
ggplot(dane, aes(x = rok, y = `WSK25-34`, color = wojewodztwo)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2) +
  scale_color_manual(values = paleta_woj) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2)) +
  labs(
    title    = "Ludność w wieku 25-34 lat",
    subtitle = "Województwa, 2004–2024",
    x        = "Rok",
    y        = "Liczba osób",
    color    = "Województwo"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "right",
    legend.text      = element_text(size = 8),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold")
  )

# --- Wskaźnik urbanizacji ---
ggplot(dane, aes(x = rok, y = WSK_URB, color = wojewodztwo)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2) +
  scale_color_manual(values = paleta_woj) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2)) +
  labs(
    title    = "Wskaźnik urbanizacji",
    subtitle = "Województwa, 2004–2024",
    x        = "Rok",
    y        = "Udział ludności miejskiej [%]",
    color    = "Województwo"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "right",
    legend.text      = element_text(size = 8),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold")
  )

# --- Nakłady inwestycyjne w sektorze prywatnym ---
ggplot(dane, aes(x = rok, y = `NAKŁ`, color = wojewodztwo)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2) +
  scale_color_manual(values = paleta_woj) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2)) +
  labs(
    title    = "Nakłady inwestycyjne w sektorze prywatnym",
    subtitle = "Województwa, 2004–2024",
    x        = "Rok",
    y        = "Nakłady [zł]",
    color    = "Województwo"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "right",
    legend.text      = element_text(size = 8),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold")
  )

# --- Średnie wynagrodzenie ---
ggplot(dane, aes(x = rok, y = WYNAGR, color = wojewodztwo)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2) +
  scale_color_manual(values = paleta_woj) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2)) +
  labs(
    title    = "Średnie wynagrodzenie",
    subtitle = "Województwa, 2004–2024",
    x        = "Rok",
    y        = "Wynagrodzenie [zł]",
    color    = "Województwo"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "right",
    legend.text      = element_text(size = 8),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold")
  )

# --- Saldo migracji ---
ggplot(dane, aes(x = rok, y = SM, color = wojewodztwo)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2) +
  scale_color_manual(values = paleta_woj) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2)) +
  labs(
    title    = "Saldo migracji",
    subtitle = "Województwa, 2004–2024",
    x        = "Rok",
    y        = "Saldo migracji [os./1000 mieszkańców]",
    color    = "Województwo"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "right",
    legend.text      = element_text(size = 8),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold")
  )

# ============================================================
#  2. ANIMACJA - mieszkania oddane do użytkowania
# ============================================================

anim <- ggplot(dane, aes(x = rok, y = MO, color = wojewodztwo, group = wojewodztwo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = paleta_woj) +
  labs(
    title    = "Mieszkania oddane do użytkowania",
    subtitle = "Rok: {frame_along}",
    x        = "Rok",
    y        = "Liczba mieszkań",
    color    = "Województwo"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.text      = element_text(size = 8),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold")
  ) +
  transition_reveal(rok)

animate(anim, nframes = 80, fps = 10, width = 900, height = 550,
        renderer = gifski_renderer("animacja_mieszkania.gif"))

# ============================================================
#  3. HEATMAPY
# ============================================================

heatmapa <- function(zmienna, tytul, jednostka) {
  df_wide <- dcast(dane, rok ~ wojewodztwo, value.var = zmienna)
  df_long <- melt(df_wide, id.vars = "rok", variable.name = "wojewodztwo", value.name = "value")

  ggplot(df_long, aes(x = wojewodztwo, y = factor(rok), fill = value)) +
    geom_tile(color = "white", linewidth = 0.3) +
    scale_fill_gradient(low = "#ffffd4", high = "#bd0026") +
    labs(
      title = tytul,
      x     = "Województwo",
      y     = "Rok",
      fill  = jednostka
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x     = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y     = element_text(size = 8),
      plot.title      = element_text(face = "bold"),
      legend.position = "right"
    )
}

heatmapa("MO",       "Mieszkania oddane do użytkowania",              "Mieszkania")
heatmapa("WSK25-34", "Ludność w wieku 25-34 lat",                     "Osoby")
heatmapa("WSK_URB",  "Wskaźnik urbanizacji",                          "%")
heatmapa("NAKŁ",     "Nakłady inwestycyjne w sektorze prywatnym",     "zł")
heatmapa("WYNAGR",   "Średnie wynagrodzenie",                          "zł")
heatmapa("SM",       "Saldo migracji",                                 "os./1000")

# ============================================================
#  4. HISTOGRAM - rozkład zmiennych dla wybranego roku
# ============================================================

rok_wybrany <- 2015
dane_rok    <- subset(dane, rok == rok_wybrany)

# Mieszkania oddane
ggplot(dane_rok, aes(x = MO)) +
  geom_histogram(bins = 8, fill = "#2166ac", color = "white", alpha = 0.85) +
  geom_vline(aes(xintercept = mean(MO)),
             color = "#d73027", linewidth = 1, linetype = "dashed") +
  labs(
    title    = paste("Rozkład mieszkań oddanych do użytkowania w", rok_wybrany, "roku"),
    subtitle = "Linia przerywana = średnia",
    x        = "Liczba mieszkań",
    y        = "Liczba województw"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

# Średnie wynagrodzenie
ggplot(dane_rok, aes(x = WYNAGR)) +
  geom_histogram(bins = 8, fill = "#4dac26", color = "white", alpha = 0.85) +
  geom_vline(aes(xintercept = mean(WYNAGR)),
             color = "#d73027", linewidth = 1, linetype = "dashed") +
  labs(
    title    = paste("Rozkład średniego wynagrodzenia w", rok_wybrany, "roku"),
    subtitle = "Linia przerywana = średnia",
    x        = "Wynagrodzenie [zł]",
    y        = "Liczba województw"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

# ============================================================
#  5. BOXPLOTY - zróżnicowanie przestrzenne
# ============================================================

# Mieszkania oddane
ggplot(dane, aes(x = reorder(wojewodztwo, MO, median), y = MO, fill = wojewodztwo)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8) +
  scale_fill_manual(values = paleta_woj) +
  labs(
    title    = "Zróżnicowanie mieszkań oddanych do użytkowania",
    subtitle = "Województwa, 2004–2024 (posortowane wg mediany)",
    x        = NULL,
    y        = "Liczba mieszkań"
  ) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "none",
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Średnie wynagrodzenie
ggplot(dane, aes(x = reorder(wojewodztwo, WYNAGR, median), y = WYNAGR, fill = wojewodztwo)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8) +
  scale_fill_manual(values = paleta_woj) +
  labs(
    title    = "Zróżnicowanie średniego wynagrodzenia",
    subtitle = "Województwa, 2004–2024 (posortowane wg mediany)",
    x        = NULL,
    y        = "Wynagrodzenie [zł]"
  ) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "none",
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Saldo migracji
ggplot(dane, aes(x = reorder(wojewodztwo, SM, median), y = SM, fill = wojewodztwo)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8) +
  scale_fill_manual(values = paleta_woj) +
  labs(
    title    = "Zróżnicowanie salda migracji",
    subtitle = "Województwa, 2004–2024 (posortowane wg mediany)",
    x        = NULL,
    y        = "Saldo migracji [os./1000 mieszkańców]"
  ) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "none",
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# ============================================================
#  6. MACIERZ KORELACJI
# ============================================================

zmienne_num <- dane[, c("MO", "WSK25-34", "WSK_URB", "NAKŁ", "WYNAGR", "SM")]

colnames(zmienne_num) <- c(
  "Mieszkania oddane",
  "Ludność 25-34",
  "Urbanizacja %",
  "Nakłady inwest.",
  "Wynagrodzenie",
  "Saldo migracji"
)

corr_matrix <- cor(zmienne_num, use = "complete.obs")

corrplot(
  corr_matrix,
  method      = "color",
  type        = "upper",
  addCoef.col = "black",
  number.cex  = 0.8,
  tl.col      = "black",
  tl.srt      = 45,
  col         = colorRampPalette(c("#d73027", "white", "#1a9850"))(200),
  title       = "Macierz korelacji - nowe dane",
  mar         = c(0, 0, 2, 0)
)

# ============================================================
#  Wizualizacje danych panelowych - województwa 2004-2024
# ============================================================

setwd("C:/Users/damia/Desktop/studia/magisterka/sem2/ekonometria przestrzenna/projekt")

# Wczytanie danych
dane <- read.csv("panel_wojewodztwa_2004_2024.csv", header = TRUE, sep = ",", dec = ".", encoding = "UTF-8")

# Podgląd danych
fix(dane)

# ── Pakiety ──────────────────────────────────────────────────
library(ggplot2)
library(gganimate)
library(reshape2)
library(corrplot)
library(gifski)

# Paleta kolorów dla 16 województw
paleta_woj <- c(
  "#1f77b4","#ff7f0e","#2ca02c","#d62728","#9467bd",
  "#8c564b","#e377c2","#7f7f7f","#bcbd22","#17becf",
  "#aec7e8","#ffbb78","#98df8a","#ff9896","#c5b0d5","#c49c94"
)

# ============================================================
#  1. WYKRESY LINIOWE - każda zmienna osobno
# ============================================================

# --- Zużycie energii elektrycznej ---
ggplot(dane, aes(x = rok, y = zuzycie_energii_GWh, color = wojewodztwo)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2) +
  scale_color_manual(values = paleta_woj) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2)) +
  labs(
    title    = "Zużycie energii elektrycznej w gospodarstwach domowych",
    subtitle = "Województwa, 2004–2024",
    x        = "Rok",
    y        = "Zużycie [GWh]",
    color    = "Województwo"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "right",
    legend.text      = element_text(size = 8),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold")
  )

# --- PKB ---
ggplot(dane, aes(x = rok, y = pkb_mln_zl / 1000, color = wojewodztwo)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2) +
  scale_color_manual(values = paleta_woj) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2)) +
  labs(
    title    = "PKB województw",
    subtitle = "Województwa, 2004–2024",
    x        = "Rok",
    y        = "PKB [mld zł]",
    color    = "Województwo"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "right",
    legend.text      = element_text(size = 8),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold")
  )

# --- Cena energii elektrycznej ---
ggplot(dane, aes(x = rok, y = cena_energii_zl_kWh, color = wojewodztwo)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2) +
  scale_color_manual(values = paleta_woj) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2)) +
  labs(
    title    = "Przeciętna cena energii elektrycznej (taryfa G-11)",
    subtitle = "Województwa, 2004–2024",
    x        = "Rok",
    y        = "Cena [zł/kWh]",
    color    = "Województwo"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "right",
    legend.text      = element_text(size = 8),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold")
  )

# --- HDD ---
ggplot(dane, aes(x = rok, y = hdd, color = wojewodztwo)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2) +
  scale_color_manual(values = paleta_woj) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2)) +
  labs(
    title    = "Stopniodni grzewcze (HDD)",
    subtitle = "Województwa, 2004–2024",
    x        = "Rok",
    y        = "HDD",
    color    = "Województwo"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "right",
    legend.text      = element_text(size = 8),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold")
  )

# --- Urbanizacja ---
ggplot(dane, aes(x = rok, y = urbanizacja_pct, color = wojewodztwo)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2) +
  scale_color_manual(values = paleta_woj) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2)) +
  labs(
    title    = "Wskaźnik urbanizacji",
    subtitle = "Województwa, 2004–2024",
    x        = "Rok",
    y        = "Ludność miejska [%]",
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
#  2. ANIMACJA - zużycie energii
# ============================================================

anim <- ggplot(dane, aes(x = rok, y = zuzycie_energii_GWh, color = wojewodztwo, group = wojewodztwo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = paleta_woj) +
  labs(
    title    = "Zużycie energii elektrycznej w gospodarstwach domowych",
    subtitle = "Rok: {frame_along}",
    x        = "Rok",
    y        = "Zużycie [GWh]",
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
        renderer = gifski_renderer("animacja_zuzycie_energii.gif"))

# ============================================================
#  3. HEATMAPY
# ============================================================

# Funkcja pomocnicza do tworzenia heatmapy
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
      axis.text.x  = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y  = element_text(size = 8),
      plot.title   = element_text(face = "bold"),
      legend.position = "right"
    )
}

heatmapa("zuzycie_energii_GWh", "Zużycie energii elektrycznej w gospodarstwach domowych", "GWh")
heatmapa("pkb_mln_zl",          "PKB województw",                                         "mln zł")
heatmapa("hdd",                 "Stopniodni grzewcze (HDD)",                               "HDD")
heatmapa("urbanizacja_pct",     "Wskaźnik urbanizacji",                                    "%")
heatmapa("cena_energii_zl_kWh","Cena energii elektrycznej (taryfa G-11)",                 "zł/kWh")

# ============================================================
#  4. HISTOGRAM - rozkład zmiennych dla wybranego roku
# ============================================================

rok_wybrany <- 2015
dane_rok <- subset(dane, rok == rok_wybrany)

# Zużycie energii
ggplot(dane_rok, aes(x = zuzycie_energii_GWh)) +
  geom_histogram(bins = 8, fill = "#2166ac", color = "white", alpha = 0.85) +
  geom_vline(aes(xintercept = mean(zuzycie_energii_GWh)),
             color = "#d73027", linewidth = 1, linetype = "dashed") +
  labs(
    title    = paste("Rozkład zużycia energii elektrycznej w", rok_wybrany, "roku"),
    subtitle = "Linia przerywana = średnia",
    x        = "Zużycie [GWh]",
    y        = "Liczba województw"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

# PKB
ggplot(dane_rok, aes(x = pkb_mln_zl / 1000)) +
  geom_histogram(bins = 8, fill = "#4dac26", color = "white", alpha = 0.85) +
  geom_vline(aes(xintercept = mean(pkb_mln_zl / 1000)),
             color = "#d73027", linewidth = 1, linetype = "dashed") +
  labs(
    title    = paste("Rozkład PKB w", rok_wybrany, "roku"),
    subtitle = "Linia przerywana = średnia",
    x        = "PKB [mld zł]",
    y        = "Liczba województw"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

# ============================================================
#  5. BOXPLOTY - zróżnicowanie przestrzenne
# ============================================================

# Zużycie energii
ggplot(dane, aes(x = reorder(wojewodztwo, zuzycie_energii_GWh, median),
                 y = zuzycie_energii_GWh, fill = wojewodztwo)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8) +
  scale_fill_manual(values = paleta_woj) +
  labs(
    title    = "Zróżnicowanie zużycia energii elektrycznej",
    subtitle = "Województwa, 2004–2024 (posortowane wg mediany)",
    x        = NULL,
    y        = "Zużycie [GWh]"
  ) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title      = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# PKB
ggplot(dane, aes(x = reorder(wojewodztwo, pkb_mln_zl, median),
                 y = pkb_mln_zl / 1000, fill = wojewodztwo)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8) +
  scale_fill_manual(values = paleta_woj) +
  labs(
    title    = "Zróżnicowanie PKB województw",
    subtitle = "Województwa, 2004–2024 (posortowane wg mediany)",
    x        = NULL,
    y        = "PKB [mld zł]"
  ) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title      = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# HDD
ggplot(dane, aes(x = reorder(wojewodztwo, hdd, median),
                 y = hdd, fill = wojewodztwo)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8) +
  scale_fill_manual(values = paleta_woj) +
  labs(
    title    = "Zróżnicowanie stopniodni grzewczych (HDD)",
    subtitle = "Województwa, 2004–2024 (posortowane wg mediany)",
    x        = NULL,
    y        = "HDD"
  ) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title      = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# ============================================================
#  6. MACIERZ KORELACJI
# ============================================================

library(corrplot)

zmienne_num <- dane[, c("zuzycie_energii_GWh", "cena_energii_zl_kWh",
                         "ludnosc", "urbanizacja_pct",
                         "pkb_mln_zl", "hdd", "cdd")]

# Ładniejsze etykiety osi
colnames(zmienne_num) <- c("Zużycie energii", "Cena energii",
                            "Ludność", "Urbanizacja %",
                            "PKB", "HDD", "CDD")

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
  title       = "Macierz korelacji zmiennych panelowych",
  mar         = c(0, 0, 2, 0)
)

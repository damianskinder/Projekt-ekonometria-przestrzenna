# ============================================================
#  Wizualizacje — zużycie energii elektrycznej
#  Dane: panel_wojewodztwa_2004_2024.csv | N=16 T=2004-2024
#  Zawiera: wykresy liniowe, animacja GIF, heatmapy,
#           histogramy, boxploty, macierz korelacji
# ============================================================

library(ggplot2)
library(gganimate)
library(reshape2)
library(corrplot)
library(gifski)
library(dplyr)

dane <- read.csv("panel_wojewodztwa_2004_2024.csv", header = TRUE,
                 sep = ",", dec = ".", encoding = "UTF-8")
dane$pkb_mld_zl <- dane$pkb_mln_zl / 1000

paleta_woj <- c(
  "#1f77b4","#ff7f0e","#2ca02c","#d62728","#9467bd",
  "#8c564b","#e377c2","#7f7f7f","#bcbd22","#17becf",
  "#aec7e8","#ffbb78","#98df8a","#ff9896","#c5b0d5","#c49c94"
)

# ============================================================
#  1. WYKRESY LINIOWE
# ============================================================

ggplot(dane, aes(x = rok, y = zuzycie_energii_GWh, color = wojewodztwo)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.2) +
  scale_color_manual(values = paleta_woj) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2)) +
  labs(title = "Zuzycie energii elektrycznej w gospodarstwach domowych",
       subtitle = "Wojewodztwa, 2004-2024", x = "Rok", y = "Zuzycie [GWh]",
       color = "Wojewodztwo") +
  theme_minimal(base_size = 12) +
  theme(legend.text = element_text(size = 8), panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggplot(dane, aes(x = rok, y = pkb_mld_zl, color = wojewodztwo)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.2) +
  scale_color_manual(values = paleta_woj) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2)) +
  labs(title = "PKB wojewodztw", subtitle = "Wojewodztwa, 2004-2024",
       x = "Rok", y = "PKB [mld zl]", color = "Wojewodztwo") +
  theme_minimal(base_size = 12) +
  theme(legend.text = element_text(size = 8), panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggplot(dane, aes(x = rok, y = cena_energii_zl_kWh, color = wojewodztwo)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.2) +
  scale_color_manual(values = paleta_woj) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2)) +
  labs(title = "Przecietna cena energii elektrycznej (taryfa G-11)",
       subtitle = "Wojewodztwa, 2004-2024", x = "Rok", y = "Cena [zl/kWh]",
       color = "Wojewodztwo") +
  theme_minimal(base_size = 12) +
  theme(legend.text = element_text(size = 8), panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggplot(dane, aes(x = rok, y = hdd, color = wojewodztwo)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.2) +
  scale_color_manual(values = paleta_woj) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2)) +
  labs(title = "Stopniodni grzewcze (HDD)", subtitle = "Wojewodztwa, 2004-2024",
       x = "Rok", y = "HDD", color = "Wojewodztwo") +
  theme_minimal(base_size = 12) +
  theme(legend.text = element_text(size = 8), panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggplot(dane, aes(x = rok, y = urbanizacja_pct, color = wojewodztwo)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.2) +
  scale_color_manual(values = paleta_woj) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2)) +
  labs(title = "Wskaznik urbanizacji", subtitle = "Wojewodztwa, 2004-2024",
       x = "Rok", y = "Ludnosc miejska [%]", color = "Wojewodztwo") +
  theme_minimal(base_size = 12) +
  theme(legend.text = element_text(size = 8), panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

# ============================================================
#  2. ANIMACJA GIF
# ============================================================

anim <- ggplot(dane, aes(x = rok, y = zuzycie_energii_GWh,
                          color = wojewodztwo, group = wojewodztwo)) +
  geom_line(linewidth = 1) + geom_point(size = 2.5) +
  scale_color_manual(values = paleta_woj) +
  labs(title = "Zuzycie energii elektrycznej w gospodarstwach domowych",
       subtitle = "Rok: {frame_along}", x = "Rok", y = "Zuzycie [GWh]",
       color = "Wojewodztwo") +
  theme_minimal(base_size = 12) +
  theme(legend.text = element_text(size = 8), panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold")) +
  transition_reveal(rok)

animate(anim, nframes = 80, fps = 10, width = 900, height = 550,
        renderer = gifski_renderer("animacja_zuzycie_energii.gif"))

# ============================================================
#  3. HEATMAPY
# ============================================================

heatmapa <- function(zmienna, tytul, jednostka) {
  df_wide <- dcast(dane, rok ~ wojewodztwo, value.var = zmienna)
  df_long <- melt(df_wide, id.vars = "rok", variable.name = "wojewodztwo", value.name = "value")
  ggplot(df_long, aes(x = wojewodztwo, y = factor(rok), fill = value)) +
    geom_tile(color = "white", linewidth = 0.3) +
    scale_fill_gradient(low = "#ffffd4", high = "#bd0026") +
    labs(title = tytul, x = "Wojewodztwo", y = "Rok", fill = jednostka) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          plot.title = element_text(face = "bold"))
}

heatmapa("zuzycie_energii_GWh", "Zuzycie energii elektrycznej",    "GWh")
heatmapa("pkb_mln_zl",          "PKB wojewodztw",                  "mln zl")
heatmapa("hdd",                 "Stopniodni grzewcze (HDD)",        "HDD")
heatmapa("urbanizacja_pct",     "Wskaznik urbanizacji",             "%")
heatmapa("cena_energii_zl_kWh","Cena energii elektrycznej",        "zl/kWh")

# ============================================================
#  4. HISTOGRAMY
# ============================================================

rok_wybrany <- 2015
dane_rok    <- subset(dane, rok == rok_wybrany)

ggplot(dane_rok, aes(x = zuzycie_energii_GWh)) +
  geom_histogram(bins = 8, fill = "#2166ac", color = "white", alpha = 0.85) +
  geom_vline(aes(xintercept = mean(zuzycie_energii_GWh)),
             color = "#d73027", linewidth = 1, linetype = "dashed") +
  labs(title = paste("Rozklad zuzycia energii elektrycznej w", rok_wybrany, "roku"),
       subtitle = "Linia przerywana = srednia", x = "Zuzycie [GWh]", y = "Liczba wojewodztw") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

ggplot(dane_rok, aes(x = pkb_mld_zl)) +
  geom_histogram(bins = 8, fill = "#4dac26", color = "white", alpha = 0.85) +
  geom_vline(aes(xintercept = mean(pkb_mld_zl)),
             color = "#d73027", linewidth = 1, linetype = "dashed") +
  labs(title = paste("Rozklad PKB w", rok_wybrany, "roku"),
       subtitle = "Linia przerywana = srednia", x = "PKB [mld zl]", y = "Liczba wojewodztw") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

# ============================================================
#  5. BOXPLOTY
# ============================================================

ggplot(dane, aes(x = reorder(wojewodztwo, zuzycie_energii_GWh, median),
                 y = zuzycie_energii_GWh, fill = wojewodztwo)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8) +
  scale_fill_manual(values = paleta_woj) +
  labs(title = "Zroznicowanie zuzycia energii elektrycznej",
       subtitle = "Posortowane wg mediany", x = NULL, y = "Zuzycie [GWh]") +
  coord_flip() + theme_minimal(base_size = 12) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

ggplot(dane, aes(x = reorder(wojewodztwo, pkb_mld_zl, median),
                 y = pkb_mld_zl, fill = wojewodztwo)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8) +
  scale_fill_manual(values = paleta_woj) +
  labs(title = "Zroznicowanie PKB wojewodztw",
       subtitle = "Posortowane wg mediany", x = NULL, y = "PKB [mld zl]") +
  coord_flip() + theme_minimal(base_size = 12) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

ggplot(dane, aes(x = reorder(wojewodztwo, hdd, median), y = hdd, fill = wojewodztwo)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8) +
  scale_fill_manual(values = paleta_woj) +
  labs(title = "Zroznicowanie stopniodni grzewczych (HDD)",
       subtitle = "Posortowane wg mediany", x = NULL, y = "HDD") +
  coord_flip() + theme_minimal(base_size = 12) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

# ============================================================
#  6. MACIERZ KORELACJI
# ============================================================

zmienne_num <- dane[, c("zuzycie_energii_GWh", "cena_energii_zl_kWh",
                         "ludnosc", "urbanizacja_pct", "pkb_mln_zl", "hdd", "cdd")]
colnames(zmienne_num) <- c("Zuzycie energii", "Cena energii", "Ludnosc",
                            "Urbanizacja %", "PKB", "HDD", "CDD")

corrplot(cor(zmienne_num, use = "complete.obs"),
         method = "color", type = "upper", addCoef.col = "black",
         number.cex = 0.8, tl.col = "black", tl.srt = 45,
         col = colorRampPalette(c("#d73027", "white", "#1a9850"))(200),
         title = "Macierz korelacji — dane energii elektrycznej", mar = c(0, 0, 2, 0))

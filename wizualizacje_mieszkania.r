# ============================================================
#  Wizualizacje — mieszkania oddane do uzytkowania
#  Dane: nowe_dane.xlsx (sheet: Ludnosc_25_34) | N=16 T=2004-2024
#  Zawiera: wykresy liniowe, animacja GIF, heatmapy,
#           histogramy, boxploty, macierz korelacji
# ============================================================

library(ggplot2)
library(gganimate)
library(reshape2)
library(corrplot)
library(gifski)
library(readxl)
library(dplyr)

dane <- read_excel("nowe_dane.xlsx", sheet = "Ludnosc_25_34")
names(dane)[names(dane) == "WSK25-34"]          <- "WSK25_34"
names(dane)[grep("^NAK", names(dane))]          <- "NAKL"
dane <- as.data.frame(dane)

paleta_woj <- c(
  "#1f77b4","#ff7f0e","#2ca02c","#d62728","#9467bd",
  "#8c564b","#e377c2","#7f7f7f","#bcbd22","#17becf",
  "#aec7e8","#ffbb78","#98df8a","#ff9896","#c5b0d5","#c49c94"
)

# ============================================================
#  1. WYKRESY LINIOWE
# ============================================================

ggplot(dane, aes(x = rok, y = MO, color = wojewodztwo)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.2) +
  scale_color_manual(values = paleta_woj) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2)) +
  labs(title = "Mieszkania oddane do uzytkowania",
       subtitle = "Wojewodztwa, 2004-2024", x = "Rok", y = "Liczba mieszkan",
       color = "Wojewodztwo") +
  theme_minimal(base_size = 12) +
  theme(legend.text = element_text(size = 8), panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggplot(dane, aes(x = rok, y = WSK25_34, color = wojewodztwo)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.2) +
  scale_color_manual(values = paleta_woj) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2)) +
  labs(title = "Ludnosc w wieku 25-34 lat",
       subtitle = "Wojewodztwa, 2004-2024", x = "Rok", y = "Liczba osob",
       color = "Wojewodztwo") +
  theme_minimal(base_size = 12) +
  theme(legend.text = element_text(size = 8), panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggplot(dane, aes(x = rok, y = WSK_URB, color = wojewodztwo)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.2) +
  scale_color_manual(values = paleta_woj) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2)) +
  labs(title = "Wskaznik urbanizacji",
       subtitle = "Wojewodztwa, 2004-2024", x = "Rok", y = "Udzial ludnosci miejskiej [%]",
       color = "Wojewodztwo") +
  theme_minimal(base_size = 12) +
  theme(legend.text = element_text(size = 8), panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggplot(dane, aes(x = rok, y = NAKL, color = wojewodztwo)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.2) +
  scale_color_manual(values = paleta_woj) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2)) +
  labs(title = "Naklady inwestycyjne w sektorze prywatnym",
       subtitle = "Wojewodztwa, 2004-2024", x = "Rok", y = "Naklady [zl]",
       color = "Wojewodztwo") +
  theme_minimal(base_size = 12) +
  theme(legend.text = element_text(size = 8), panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggplot(dane, aes(x = rok, y = WYNAGR, color = wojewodztwo)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.2) +
  scale_color_manual(values = paleta_woj) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2)) +
  labs(title = "Srednie wynagrodzenie",
       subtitle = "Wojewodztwa, 2004-2024", x = "Rok", y = "Wynagrodzenie [zl]",
       color = "Wojewodztwo") +
  theme_minimal(base_size = 12) +
  theme(legend.text = element_text(size = 8), panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggplot(dane, aes(x = rok, y = SM, color = wojewodztwo)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.2) +
  scale_color_manual(values = paleta_woj) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2)) +
  labs(title = "Saldo migracji",
       subtitle = "Wojewodztwa, 2004-2024", x = "Rok",
       y = "Saldo migracji [os./1000 mieszkancow]", color = "Wojewodztwo") +
  theme_minimal(base_size = 12) +
  theme(legend.text = element_text(size = 8), panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

# ============================================================
#  2. ANIMACJA GIF
# ============================================================

anim <- ggplot(dane, aes(x = rok, y = MO, color = wojewodztwo, group = wojewodztwo)) +
  geom_line(linewidth = 1) + geom_point(size = 2.5) +
  scale_color_manual(values = paleta_woj) +
  labs(title = "Mieszkania oddane do uzytkowania",
       subtitle = "Rok: {frame_along}", x = "Rok", y = "Liczba mieszkan",
       color = "Wojewodztwo") +
  theme_minimal(base_size = 12) +
  theme(legend.text = element_text(size = 8), panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold")) +
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
    labs(title = tytul, x = "Wojewodztwo", y = "Rok", fill = jednostka) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          axis.text.y = element_text(size = 8),
          plot.title = element_text(face = "bold"))
}

heatmapa("MO",       "Mieszkania oddane do uzytkowania",           "Mieszkania")
heatmapa("WSK25_34", "Ludnosc w wieku 25-34 lat",                  "Osoby")
heatmapa("WSK_URB",  "Wskaznik urbanizacji",                       "%")
heatmapa("NAKL",     "Naklady inwestycyjne w sektorze prywatnym",  "zl")
heatmapa("WYNAGR",   "Srednie wynagrodzenie",                      "zl")
heatmapa("SM",       "Saldo migracji",                             "os./1000")

# ============================================================
#  4. HISTOGRAMY
# ============================================================

rok_wybrany <- 2015
dane_rok    <- subset(dane, rok == rok_wybrany)

ggplot(dane_rok, aes(x = MO)) +
  geom_histogram(bins = 8, fill = "#2166ac", color = "white", alpha = 0.85) +
  geom_vline(aes(xintercept = mean(MO)),
             color = "#d73027", linewidth = 1, linetype = "dashed") +
  labs(title = paste("Rozklad mieszkan oddanych do uzytkowania w", rok_wybrany, "roku"),
       subtitle = "Linia przerywana = srednia", x = "Liczba mieszkan", y = "Liczba wojewodztw") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

ggplot(dane_rok, aes(x = WYNAGR)) +
  geom_histogram(bins = 8, fill = "#4dac26", color = "white", alpha = 0.85) +
  geom_vline(aes(xintercept = mean(WYNAGR)),
             color = "#d73027", linewidth = 1, linetype = "dashed") +
  labs(title = paste("Rozklad sredniego wynagrodzenia w", rok_wybrany, "roku"),
       subtitle = "Linia przerywana = srednia", x = "Wynagrodzenie [zl]", y = "Liczba wojewodztw") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

# ============================================================
#  5. BOXPLOTY
# ============================================================

ggplot(dane, aes(x = reorder(wojewodztwo, MO, median), y = MO, fill = wojewodztwo)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8) +
  scale_fill_manual(values = paleta_woj) +
  labs(title = "Zroznicowanie mieszkan oddanych do uzytkowania",
       subtitle = "Posortowane wg mediany", x = NULL, y = "Liczba mieszkan") +
  coord_flip() + theme_minimal(base_size = 12) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

ggplot(dane, aes(x = reorder(wojewodztwo, WYNAGR, median), y = WYNAGR, fill = wojewodztwo)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8) +
  scale_fill_manual(values = paleta_woj) +
  labs(title = "Zroznicowanie sredniego wynagrodzenia",
       subtitle = "Posortowane wg mediany", x = NULL, y = "Wynagrodzenie [zl]") +
  coord_flip() + theme_minimal(base_size = 12) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

ggplot(dane, aes(x = reorder(wojewodztwo, SM, median), y = SM, fill = wojewodztwo)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8) +
  scale_fill_manual(values = paleta_woj) +
  labs(title = "Zroznicowanie salda migracji",
       subtitle = "Posortowane wg mediany", x = NULL,
       y = "Saldo migracji [os./1000 mieszkancow]") +
  coord_flip() + theme_minimal(base_size = 12) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

# ============================================================
#  6. MACIERZ KORELACJI
# ============================================================

zmienne_num <- dane[, c("MO", "WSK25_34", "WSK_URB", "NAKL", "WYNAGR", "SM")]
colnames(zmienne_num) <- c("Mieszkania oddane", "Ludnosc 25-34", "Urbanizacja %",
                            "Naklady inwest.", "Wynagrodzenie", "Saldo migracji")

corrplot(cor(zmienne_num, use = "complete.obs"),
         method = "color", type = "upper", addCoef.col = "black",
         number.cex = 0.8, tl.col = "black", tl.srt = 45,
         col = colorRampPalette(c("#d73027", "white", "#1a9850"))(200),
         title = "Macierz korelacji — dane o mieszkaniach", mar = c(0, 0, 2, 0))

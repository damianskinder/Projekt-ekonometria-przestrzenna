# ============================================================
#  Interaktywna mapa województw — zużycie energii elektrycznej
#  Dane: panel_wojewodztwa_2004_2024.csv | N=16 T=2004-2024
#  Uruchomienie: shiny::runApp("mapa_energia.r")
# ============================================================

library(shiny)
library(sf)
library(ggplot2)
library(dplyr)
library(scales)

# ── Wczytanie danych ─────────────────────────────────────────
dane <- read.csv("panel_wojewodztwa_2004_2024.csv", header = TRUE,
                 sep = ",", dec = ".", encoding = "UTF-8")
dane$pkb_mld_zl <- dane$pkb_mln_zl / 1000
dane$teryt      <- sprintf("%02d", as.numeric(dane$teryt))

mapa <- st_read("wojewodztwa.shp", quiet = TRUE)
mapa$JPT_KOD_JE <- sprintf("%02d", as.numeric(mapa$JPT_KOD_JE))
mapa <- mapa[order(mapa$JPT_KOD_JE), ]

# ── Konfiguracja zmiennych ────────────────────────────────────
zmienne_config <- list(
  "Zuzycie energii elektrycznej [GWh]" = list(col = "zuzycie_energii_GWh", low = "lightyellow",  high = "#08306b"),
  "Cena energii elektrycznej [zl/kWh]" = list(col = "cena_energii_zl_kWh", low = "#fff7ec",      high = "#7f0000"),
  "PKB [mld zl]"                        = list(col = "pkb_mld_zl",           low = "#f7fcf5",      high = "#00441b"),
  "Liczba ludnosci [osoby]"             = list(col = "ludnosc",              low = "#f7fbff",      high = "#08306b"),
  "Urbanizacja [%]"                     = list(col = "urbanizacja_pct",      low = "#fff5eb",      high = "#7f2704"),
  "Stopniodni grzewcze - HDD"          = list(col = "hdd",                  low = "#ffffd9",      high = "#081d58"),
  "Stopniodni chlodzenia - CDD"        = list(col = "cdd",                  low = "#fff7fb",      high = "#67001f")
)

# ── UI ───────────────────────────────────────────────────────
ui <- fluidPage(
  titlePanel("Mapa interaktywna — dane o zuzyciu energii elektrycznej"),
  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput("zmienna", "Zmienna:", choices = names(zmienne_config)),
      sliderInput("rok", "Rok:", min = 2004, max = 2024, value = 2015, step = 1, sep = "",
                  animate = animationOptions(interval = 1200, loop = FALSE)),
      hr(),
      h5("Statystyki opisowe (wybrany rok):"),
      tableOutput("statystyki"),
      hr(),
      h5("Obserwacje odstajace (IQR):"),
      tableOutput("outliery")
    ),
    mainPanel(width = 9,
      plotOutput("mapa", height = "580px"),
      hr(),
      h5("Wszystkie wojewodztwa — wybrany rok:"),
      tableOutput("tabela")
    )
  )
)

# ── SERVER ───────────────────────────────────────────────────
server <- function(input, output, session) {

  dane_filtered <- reactive({
    cfg    <- zmienne_config[[input$zmienna]]
    df_rok <- dane %>% filter(rok == input$rok) %>%
      select(teryt, wojewodztwo, all_of(cfg$col))
    merge(mapa, df_rok, by.x = "JPT_KOD_JE", by.y = "teryt")
  })

  output$mapa <- renderPlot({
    md  <- dane_filtered()
    cfg <- zmienne_config[[input$zmienna]]
    col <- cfg$col
    ggplot(md) +
      geom_sf(aes(fill = .data[[col]]), color = "white", linewidth = 0.5) +
      scale_fill_gradient(low = cfg$low, high = cfg$high,
                          name = input$zmienna, labels = scales::comma) +
      geom_sf_text(aes(label = wojewodztwo), size = 2.8, color = "grey20",
                   fontface = "bold", check_overlap = TRUE) +
      labs(title = input$zmienna, subtitle = paste("Rok:", input$rok),
           caption = "Zrodlo: GUS BDL, Eurostat") +
      theme_void(base_size = 13) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
            plot.subtitle = element_text(hjust = 0.5, color = "grey40", size = 12),
            legend.position = "right", legend.key.height = unit(1.2, "cm"))
  })

  output$statystyki <- renderTable({
    cfg <- zmienne_config[[input$zmienna]]
    col <- cfg$col
    df  <- dane %>% filter(rok == input$rok) %>% pull(!!sym(col))
    q1 <- quantile(df, 0.25, na.rm = TRUE); q3 <- quantile(df, 0.75, na.rm = TRUE)
    data.frame(
      Miara   = c("Minimum", "Q1 (25%)", "Mediana", "Srednia", "Q3 (75%)", "Maximum", "Odch. std."),
      Wartosc = round(c(min(df, na.rm=T), q1, median(df, na.rm=T),
                        mean(df, na.rm=T), q3, max(df, na.rm=T), sd(df, na.rm=T)), 2)
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$outliery <- renderTable({
    cfg <- zmienne_config[[input$zmienna]]
    col <- cfg$col
    df_rok <- dane %>% filter(rok == input$rok)
    x <- df_rok[[col]]
    q1 <- quantile(x, 0.25, na.rm=T); q3 <- quantile(x, 0.75, na.rm=T)
    iqr <- q3 - q1; lo <- q1 - 1.5*iqr; hi <- q3 + 1.5*iqr
    idx <- which(x < lo | x > hi)
    if (length(idx) == 0) return(data.frame(Info = "Brak obserwacji odstajacych"))
    data.frame(Wojewodztwo = df_rok$wojewodztwo[idx],
               Wartosc = round(x[idx], 2),
               Typ = ifelse(x[idx] < lo, "nizszy", "wyzszy"))
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$tabela <- renderTable({
    cfg <- zmienne_config[[input$zmienna]]
    col <- cfg$col
    dane %>% filter(rok == input$rok) %>%
      select(wojewodztwo, !!sym(col)) %>%
      arrange(desc(!!sym(col))) %>%
      rename(Wojewodztwo = wojewodztwo, Wartosc = !!sym(col)) %>%
      mutate(Wartosc = round(Wartosc, 3))
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

shinyApp(ui = ui, server = server)

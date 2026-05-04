# ============================================================
#  Interaktywna mapa województw - wybór roku i zmiennej
#  Uruchomienie: shiny::runApp("mapa_interaktywna.R")
# ============================================================

library(shiny)
library(sf)
library(ggplot2)
library(dplyr)

# ── Wczytanie danych (raz przy starcie aplikacji) ─────────────
dane <- read.csv("panel_wojewodztwa_2004_2024.csv", header = TRUE,
                 sep = ",", dec = ".", encoding = "UTF-8")
dane$pkb_mld_zl <- dane$pkb_mln_zl / 1000
dane$teryt      <- sprintf("%02d", as.numeric(dane$teryt))

mapa <- st_read("wojewodztwa.shp")
mapa$JPT_KOD_JE <- sprintf("%02d", as.numeric(mapa$JPT_KOD_JE))

# ── Konfiguracja zmiennych ────────────────────────────────────
zmienne_config <- list(
  "Zużycie energii elektrycznej [GWh]"      = list(col = "zuzycie_energii_GWh", low = "lightyellow",  high = "#08306b"),
  "Cena energii elektrycznej [zł/kWh]"      = list(col = "cena_energii_zl_kWh", low = "#fff7ec",      high = "#7f0000"),
  "PKB [mld zł]"                            = list(col = "pkb_mld_zl",           low = "#f7fcf5",      high = "#00441b"),
  "Liczba ludności [osoby]"                  = list(col = "ludnosc",              low = "#f7fbff",      high = "#08306b"),
  "Urbanizacja [%]"                          = list(col = "urbanizacja_pct",      low = "#fff5eb",      high = "#7f2704"),
  "Stopniodni grzewcze - HDD"               = list(col = "hdd",                  low = "#ffffd9",      high = "#081d58"),
  "Stopniodni chłodzenia - CDD"             = list(col = "cdd",                  low = "#fff7fb",      high = "#67001f")
)

# ── UI ───────────────────────────────────────────────────────
ui <- fluidPage(

  titlePanel("Wizualizacja przestrzenna województw Polski"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      selectInput(
        inputId  = "zmienna",
        label    = "Zmienna:",
        choices  = names(zmienne_config),
        selected = names(zmienne_config)[1]
      ),

      sliderInput(
        inputId = "rok",
        label   = "Rok:",
        min     = 2004,
        max     = 2024,
        value   = 2015,
        step    = 1,
        sep     = "",
        animate = animationOptions(interval = 1200, loop = FALSE)
      ),

      hr(),

      h5("Statystyki dla wybranego roku:"),
      tableOutput("statystyki")
    ),

    mainPanel(
      width = 9,
      plotOutput("mapa", height = "580px"),
      hr(),
      h5("Wszystkie województwa — wybrany rok:"),
      tableOutput("tabela")
    )
  )
)

# ── SERVER ───────────────────────────────────────────────────
server <- function(input, output, session) {

  # Dane dla wybranego roku i zmiennej
  dane_filtered <- reactive({
    cfg    <- zmienne_config[[input$zmienna]]
    df_rok <- dane %>% filter(rok == input$rok) %>%
      select(teryt, wojewodztwo, all_of(cfg$col))
    merge(mapa, df_rok, by.x = "JPT_KOD_JE", by.y = "teryt")
  })

  # Mapa
  output$mapa <- renderPlot({
    md  <- dane_filtered()
    cfg <- zmienne_config[[input$zmienna]]
    col <- cfg$col

    ggplot(md) +
      geom_sf(aes(fill = .data[[col]]), color = "white", linewidth = 0.5) +
      scale_fill_gradient(
        low    = cfg$low,
        high   = cfg$high,
        name   = input$zmienna,
        labels = scales::comma
      ) +
      geom_sf_text(aes(label = wojewodztwo), size = 2.8,
                   color = "grey20", fontface = "bold",
                   check_overlap = TRUE) +
      labs(
        title    = input$zmienna,
        subtitle = paste("Rok:", input$rok),
        caption  = "Źródło: GUS BDL, Eurostat"
      ) +
      theme_void(base_size = 13) +
      theme(
        plot.title    = element_text(face = "bold", hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, color = "grey40", size = 12),
        plot.caption  = element_text(hjust = 1, color = "grey60", size = 9),
        legend.position      = "right",
        legend.title         = element_text(size = 10),
        legend.key.height    = unit(1.2, "cm")
      )
  })

  # Tabela statystyk
  output$statystyki <- renderTable({
    cfg <- zmienne_config[[input$zmienna]]
    col <- cfg$col
    df  <- dane %>% filter(rok == input$rok) %>% pull(!!sym(col))
    data.frame(
      Miara   = c("Minimum", "Mediana", "Średnia", "Maximum"),
      Wartość = round(c(min(df, na.rm = TRUE), median(df, na.rm = TRUE),
                        mean(df, na.rm = TRUE), max(df, na.rm = TRUE)), 2)
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # Tabela wszystkich województw
  output$tabela <- renderTable({
    cfg <- zmienne_config[[input$zmienna]]
    col <- cfg$col
    dane %>%
      filter(rok == input$rok) %>%
      select(wojewodztwo, !!sym(col)) %>%
      arrange(desc(!!sym(col))) %>%
      rename(Województwo = wojewodztwo, Wartość = !!sym(col)) %>%
      mutate(Wartość = round(Wartość, 3))
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

# ── Uruchomienie ─────────────────────────────────────────────
shinyApp(ui = ui, server = server)

# ============================================================
#  PANEL GŁÓWNY - Projekt Ekonometria Przestrzenna
#  Uruchomienie: shiny::runApp("Panel główny.r")
# ============================================================

library(shiny)
library(sf)
library(ggplot2)
library(dplyr)
library(reshape2)
library(corrplot)
library(spdep)
library(plm)
library(readxl)
library(scales)

# ── Wczytanie danych ─────────────────────────────────────────
mapa <- st_read("wojewodztwa.shp", quiet = TRUE)
mapa$JPT_KOD_JE <- sprintf("%02d", as.numeric(mapa$JPT_KOD_JE))
mapa <- mapa[order(mapa$JPT_KOD_JE), ]

dane_s <- read.csv("panel_wojewodztwa_2004_2024.csv", header = TRUE,
                   sep = ",", dec = ".", encoding = "UTF-8")
dane_s$teryt      <- sprintf("%02d", as.numeric(dane_s$teryt))
dane_s$pkb_mld_zl <- dane_s$pkb_mln_zl / 1000

dane_n <- read_excel("nowe_dane.xlsx", sheet = "Ludnosc_25_34")
names(dane_n)[names(dane_n) == "WSK25-34"] <- "WSK25_34"
dane_n$teryt <- sprintf("%02d", as.numeric(dane_n$teryt))

addResourcePath("assets", normalizePath("."))

# ── Paleta ────────────────────────────────────────────────────
paleta_woj <- c(
  "#1f77b4","#ff7f0e","#2ca02c","#d62728","#9467bd",
  "#8c564b","#e377c2","#7f7f7f","#bcbd22","#17becf",
  "#aec7e8","#ffbb78","#98df8a","#ff9896","#c5b0d5","#c49c94"
)

lata_s <- 2004:2024
lata_n <- sort(unique(dane_n$rok))

# ── Konfiguracje zmiennych ────────────────────────────────────
mapa_zm_s <- list(
  "Zużycie energii elektrycznej [GWh]" = list(col = "zuzycie_energii_GWh", low = "lightyellow", high = "#08306b"),
  "Cena energii elektrycznej [zł/kWh]" = list(col = "cena_energii_zl_kWh", low = "#fff7ec",     high = "#7f0000"),
  "PKB [mld zł]"                       = list(col = "pkb_mld_zl",           low = "#f7fcf5",     high = "#00441b"),
  "Liczba ludności [osoby]"            = list(col = "ludnosc",              low = "#f7fbff",     high = "#08306b"),
  "Urbanizacja [%]"                    = list(col = "urbanizacja_pct",      low = "#fff5eb",     high = "#7f2704"),
  "Stopniodni grzewcze - HDD"          = list(col = "hdd",                  low = "#ffffd9",     high = "#081d58"),
  "Stopniodni chłodzenia - CDD"        = list(col = "cdd",                  low = "#fff7fb",     high = "#67001f")
)
mapa_zm_n <- list(
  "Mieszkania oddane do użytkowania"          = list(col = "MO",       low = "lightyellow", high = "#08306b"),
  "Ludność w wieku 25-34 lat"                 = list(col = "WSK25_34", low = "#fff7ec",     high = "#7f0000"),
  "Wskaźnik urbanizacji [%]"                  = list(col = "WSK_URB",  low = "#f7fcf5",     high = "#00441b"),
  "Nakłady inwestycyjne w sektorze prywatnym" = list(col = "NAKŁ",     low = "#f7fbff",     high = "#08306b"),
  "Średnie wynagrodzenie [zł]"                = list(col = "WYNAGR",   low = "#fff5eb",     high = "#7f2704"),
  "Saldo migracji"                            = list(col = "SM",       low = "#ffffd9",     high = "#081d58")
)
wiz_zm_s <- list(
  "Zużycie energii elektrycznej [GWh]" = list(col = "zuzycie_energii_GWh", ylab = "Zużycie [GWh]",  jed = "GWh"),
  "PKB [mld zł]"                       = list(col = "pkb_mld_zl",           ylab = "PKB [mld zł]",   jed = "mld zł"),
  "Cena energii elektrycznej [zł/kWh]" = list(col = "cena_energii_zl_kWh",  ylab = "Cena [zł/kWh]", jed = "zł/kWh"),
  "Stopniodni grzewcze HDD"            = list(col = "hdd",                  ylab = "HDD",            jed = "HDD"),
  "Urbanizacja [%]"                    = list(col = "urbanizacja_pct",      ylab = "Udział [%]",     jed = "%"),
  "Liczba ludności"                    = list(col = "ludnosc",              ylab = "Ludność [os.]",  jed = "os."),
  "Stopniodni chłodzenia CDD"          = list(col = "cdd",                  ylab = "CDD",            jed = "CDD")
)
wiz_zm_n <- list(
  "Mieszkania oddane do użytkowania"          = list(col = "MO",       ylab = "Liczba mieszkań",    jed = "Mieszkania"),
  "Ludność w wieku 25-34 lat"                 = list(col = "WSK25_34", ylab = "Liczba osób",        jed = "Osoby"),
  "Wskaźnik urbanizacji [%]"                  = list(col = "WSK_URB",  ylab = "Udział [%]",         jed = "%"),
  "Nakłady inwestycyjne w sektorze prywatnym" = list(col = "NAKŁ",     ylab = "Nakłady [zł]",       jed = "zł"),
  "Średnie wynagrodzenie [zł]"                = list(col = "WYNAGR",   ylab = "Wynagrodzenie [zł]", jed = "zł"),
  "Saldo migracji"                            = list(col = "SM",       ylab = "Saldo [os./1000]",   jed = "os./1000")
)
auto_zm_s <- list(
  "Zużycie energii elektrycznej [GWh]" = "zuzycie_energii_GWh",
  "Cena energii elektrycznej [zł/kWh]" = "cena_energii_zl_kWh",
  "PKB [mln zł]"                       = "pkb_mln_zl",
  "Liczba ludności"                    = "ludnosc",
  "Urbanizacja [%]"                    = "urbanizacja_pct",
  "Stopniodni grzewcze HDD"            = "hdd",
  "Stopniodni chłodzenia CDD"          = "cdd"
)
auto_zm_n <- list(
  "Mieszkania oddane do użytkowania"          = "MO",
  "Ludność w wieku 25-34 lat"                 = "WSK25_34",
  "Wskaźnik urbanizacji [%]"                  = "WSK_URB",
  "Nakłady inwestycyjne w sektorze prywatnym" = "NAKŁ",
  "Średnie wynagrodzenie [zł]"                = "WYNAGR",
  "Saldo migracji"                            = "SM"
)
typy_wiz <- c("Liniowy", "Heatmapa", "Histogram", "Boxplot", "Macierz korelacji", "Animacja")

# ── Macierze wag — sąsiedztwo Queen (wspólna granica) ────────
nb   <- poly2nb(mapa, queen = TRUE)
lw_s <- nb2listw(nb, style = "W", zero.policy = TRUE)
lw_n <- lw_s

# ── Pre-obliczenie I Morana ───────────────────────────────────
oblicz_morana <- function(dane, zm_cfg, lata, lw) {
  wyniki <- data.frame()
  for (zm_label in names(zm_cfg)) {
    zm_col <- zm_cfg[[zm_label]]
    for (yr in lata) {
      df_yr <- as.data.frame(dane) %>% filter(rok == yr) %>% arrange(teryt)
      test  <- suppressWarnings(moran.test(df_yr[[zm_col]], lw, alternative="two.sided", zero.policy=TRUE))
      wyniki <- rbind(wyniki, data.frame(zmienna=zm_label, rok=yr,
        moran_I=round(test$estimate["Moran I statistic"],4),
        p_value=round(test$p.value,4), istotna=test$p.value<0.05))
    }
  }
  wyniki
}
wyniki_s <- oblicz_morana(dane_s, auto_zm_s, lata_s, lw_s)
wyniki_n <- oblicz_morana(dane_n, auto_zm_n, lata_n, lw_n)

# ── Modele FE ─────────────────────────────────────────────────
pdata_s    <- pdata.frame(dane_s, index = c("teryt","rok"))
model_fe_s <- plm(zuzycie_energii_GWh ~ dochod_os + urbanizacja_pct + cena_energii_zl_kWh + hdd + cdd,
                  data=pdata_s, model="within")
pdata_n    <- pdata.frame(as.data.frame(dane_n), index = c("teryt","rok"))
model_fe_n <- plm(MO ~ WSK_URB + `NAKŁ` + WYNAGR + SM + WSK25_34, data=pdata_n, model="within")

# ── CSS ───────────────────────────────────────────────────────
css <- "
  body { background: #f0f4f8; font-family: 'Segoe UI', Arial, sans-serif; }
  .navbar { background: #1a3a5c !important; border: none; border-radius: 0; }
  .navbar-brand { color: #fff !important; font-weight: 700; font-size: 1.1em; }
  .navbar .nav > li > a { color: #cde !important; }
  .navbar .nav > li.active > a,
  .navbar .nav > li > a:hover { background: #2563a8 !important; color: #fff !important; }
  .sekcja { padding: 28px 24px 40px; }
  .sekcja-naglowek {
    font-size: 0.9em; font-weight: 600; color: #2563a8;
    border-left: 4px solid #2563a8; padding-left: 12px;
    margin-bottom: 22px; line-height: 1.4;
  }
  .karty-rzad { display: flex; gap: 20px; flex-wrap: wrap; margin-bottom: 10px; }
  .skrypt-karta {
    flex: 1 1 240px; background: #fff; border-radius: 10px;
    padding: 22px 20px 18px; box-shadow: 0 2px 10px rgba(0,0,0,.07);
    border-top: 4px solid #2563a8;
    display: flex; flex-direction: column; gap: 12px; transition: box-shadow .2s;
  }
  .skrypt-karta:hover { box-shadow: 0 4px 18px rgba(37,99,168,.18); }
  .gif-karta { border-top-color: #0891b2; }
  .gif-karta:hover { box-shadow: 0 4px 18px rgba(8,145,178,.18); }
  .karta-naglowek { display: flex; align-items: center; gap: 10px; }
  .karta-tytul { margin: 0; font-size: 1em; color: #1a3a5c; font-weight: 600; }
  .karta-ikona { font-size: 2em; line-height: 1; }
  .karta-opis { color: #556; font-size: 0.86em; flex-grow: 1; margin: 0; line-height: 1.5; }
  .karta-przyciski { display: flex; gap: 8px; }
  .btn-nav {
    flex: 1; border-radius: 6px; font-size: 0.84em;
    padding: 7px 4px; font-weight: 500;
  }
  .btn-przejdz {
    background: #2563a8; color: #fff; border: none;
  }
  .btn-przejdz:hover, .btn-przejdz:focus { background: #1a3a5c; color: #fff; }
  .btn-gif {
    background: #ecfeff; color: #0891b2; border: 1.5px solid #0891b2;
  }
  .btn-gif:hover, .btn-gif:focus { background: #0891b2; color: #fff; }
"

# ── Funkcje kart ──────────────────────────────────────────────
karta <- function(id, tytul, opis, ikona) {
  div(class = "skrypt-karta",
    div(class = "karta-naglowek",
      span(class = "karta-ikona", ikona),
      h4(class = "karta-tytul", tytul)
    ),
    p(class = "karta-opis", opis),
    div(class = "karta-przyciski",
      actionButton(paste0("go_", id), "Przejdź →",
                   class = "btn-nav btn-przejdz",
                   icon  = icon("arrow-right"))
    )
  )
}

karta_gif <- function(id, tytul, opis) {
  div(class = "skrypt-karta gif-karta",
    div(class = "karta-naglowek",
      span(class = "karta-ikona", "🎞️"),
      h4(class = "karta-tytul", tytul)
    ),
    p(class = "karta-opis", opis),
    div(class = "karta-przyciski",
      actionButton(paste0("gif_", id), "Otwórz animację",
                   class = "btn-nav btn-gif",
                   icon  = icon("film"))
    )
  )
}

# ── UI ───────────────────────────────────────────────────────
ui <- navbarPage(
  "Panel Główny", id = "nav",
  header = tags$head(tags$style(HTML(css))),

  # ── Strona główna ─────────────────────────────────────────
  tabPanel("Strona główna", value = "home",
    div(class = "sekcja",

      div(class = "sekcja-naglowek",
        "Stare dane — panel_wojewodztwa_2004_2024.csv",
        tags$br(),
        tags$span(style = "font-weight:400; color:#555;",
          "Zmienne: zużycie energii, cena energii, PKB, ludność, urbanizacja, HDD, CDD")
      ),
      div(class = "karty-rzad",
        karta("mapa_s",  "Mapa interaktywna",
              "Interaktywna mapa województw z suwakiem roku i wyborem zmiennej.", "🗺️"),
        karta("wiz_s",   "Wizualizacje",
              "Wykresy liniowe, heatmapy, histogramy, boxploty i macierz korelacji.", "📊"),
        karta("auto_s",  "Autokorelacja przestrzenna",
              "Test Morana I, wykresy Morana, I Morana w czasie i model panelowy FE.", "📐"),
        karta_gif("zuz", "Animacja — zużycie energii",
              "Zmiany zużycia energii elektrycznej w województwach w latach 2004–2024.")
      ),

      tags$hr(style = "margin: 30px 0;"),

      div(class = "sekcja-naglowek",
        "Nowe dane — nowe_dane.xlsx",
        tags$br(),
        tags$span(style = "font-weight:400; color:#555;",
          "Zmienne: mieszkania oddane, ludność 25–34, urbanizacja, nakłady, wynagrodzenie, migracja")
      ),
      div(class = "karty-rzad",
        karta("mapa_n",  "Mapa interaktywna",
              "Interaktywna mapa województw z suwakiem roku i wyborem zmiennej.", "🗺️"),
        karta("wiz_n",   "Wizualizacje",
              "Wykresy liniowe, heatmapy, histogramy, boxploty i macierz korelacji.", "📊"),
        karta("auto_n",  "Autokorelacja przestrzenna",
              "Test Morana I, wykresy Morana, I Morana w czasie i model panelowy FE.", "📐"),
        karta_gif("mies","Animacja — mieszkania oddane",
              "Zmiany liczby mieszkań oddanych do użytkowania w województwach w latach 2004–2024.")
      )
    )
  ),

  # ── Stare dane ────────────────────────────────────────────
  navbarMenu("Stare dane",

    tabPanel("Mapa interaktywna", value = "mapa_s",
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput("zm_s", "Zmienna:", choices = names(mapa_zm_s)),
          sliderInput("rok_s", "Rok:", min=2004, max=2024, value=2015, step=1, sep="",
                      animate=animationOptions(interval=1200)),
          hr(), h5("Statystyki:"), tableOutput("stat_s")
        ),
        mainPanel(width = 9,
          plotOutput("mapa_s", height="580px"), hr(),
          h5("Wszystkie województwa — wybrany rok:"), tableOutput("tab_s")
        )
      )
    ),

    tabPanel("Wizualizacje", value = "wiz_s",
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput("typ_s", "Typ wykresu:", choices = typy_wiz),
          conditionalPanel(
            condition = "input.typ_s != 'Macierz korelacji' && input.typ_s != 'Animacja'",
            selectInput("wiz_zm_s", "Zmienna:", choices = names(wiz_zm_s))
          ),
          conditionalPanel(
            condition = "input.typ_s == 'Histogram'",
            sliderInput("wiz_rok_s", "Rok:", min=2004, max=2024, value=2015, step=1, sep="")
          )
        ),
        mainPanel(width = 9,
          conditionalPanel(condition = "input.typ_s != 'Animacja'",
            plotOutput("wykres_s", height="560px")
          ),
          conditionalPanel(condition = "input.typ_s == 'Animacja'",
            div(style="text-align:center; padding:30px;",
              tags$img(src="assets/animacja_zuzycie_energii.gif", width="90%", style="max-width:860px;"),
              p(style="color:#888; font-size:.88em; margin-top:10px;",
                "Zużycie energii elektrycznej — animacja 2004–2024")
            )
          )
        )
      )
    ),

    tabPanel("Autokorelacja przestrzenna", value = "auto_s",
      fluidPage(br(),
        tabsetPanel(
          tabPanel("Test Morana", br(),
            sidebarLayout(
              sidebarPanel(width=3,
                selectInput("auto_zm_s", "Zmienna:", choices=names(auto_zm_s)),
                sliderInput("auto_rok_s", "Rok:", min=2004, max=2024, value=2015, step=1, sep="")
              ),
              mainPanel(width=9,
                plotOutput("mplot_s", height="500px"),
                hr(), h5("Wynik testu Morana I:"), verbatimTextOutput("test_s")
              )
            )
          ),
          tabPanel("I Morana w czasie", br(),
            plotOutput("mczas_s", height="460px"), hr(),
            h5("Tabela podsumowująca:"), tableOutput("tauto_s")
          ),
          tabPanel("Model panelowy FE", br(),
            h4("Model efektów stałych — Zużycie energii elektrycznej [GWh]"),
            verbatimTextOutput("modsum_s")
          )
        )
      )
    )
  ),

  # ── Nowe dane ─────────────────────────────────────────────
  navbarMenu("Nowe dane",

    tabPanel("Mapa interaktywna", value = "mapa_n",
      sidebarLayout(
        sidebarPanel(width=3,
          selectInput("zm_n", "Zmienna:", choices=names(mapa_zm_n)),
          sliderInput("rok_n", "Rok:", min=min(lata_n), max=max(lata_n), value=2015, step=1, sep="",
                      animate=animationOptions(interval=1200)),
          hr(), h5("Statystyki:"), tableOutput("stat_n")
        ),
        mainPanel(width=9,
          plotOutput("mapa_n", height="580px"), hr(),
          h5("Wszystkie województwa — wybrany rok:"), tableOutput("tab_n")
        )
      )
    ),

    tabPanel("Wizualizacje", value = "wiz_n",
      sidebarLayout(
        sidebarPanel(width=3,
          selectInput("typ_n", "Typ wykresu:", choices=typy_wiz),
          conditionalPanel(
            condition="input.typ_n != 'Macierz korelacji' && input.typ_n != 'Animacja'",
            selectInput("wiz_zm_n", "Zmienna:", choices=names(wiz_zm_n))
          ),
          conditionalPanel(
            condition="input.typ_n == 'Histogram'",
            sliderInput("wiz_rok_n", "Rok:", min=min(lata_n), max=max(lata_n), value=2015, step=1, sep="")
          )
        ),
        mainPanel(width=9,
          conditionalPanel(condition="input.typ_n != 'Animacja'",
            plotOutput("wykres_n", height="560px")
          ),
          conditionalPanel(condition="input.typ_n == 'Animacja'",
            div(style="text-align:center; padding:30px;",
              tags$img(src="assets/animacja_mieszkania.gif", width="90%", style="max-width:860px;"),
              p(style="color:#888; font-size:.88em; margin-top:10px;",
                "Mieszkania oddane do użytkowania — animacja 2004–2024")
            )
          )
        )
      )
    ),

    tabPanel("Autokorelacja przestrzenna", value = "auto_n",
      fluidPage(br(),
        tabsetPanel(
          tabPanel("Test Morana", br(),
            sidebarLayout(
              sidebarPanel(width=3,
                selectInput("auto_zm_n", "Zmienna:", choices=names(auto_zm_n)),
                sliderInput("auto_rok_n", "Rok:", min=min(lata_n), max=max(lata_n), value=2015, step=1, sep="")
              ),
              mainPanel(width=9,
                plotOutput("mplot_n", height="500px"),
                hr(), h5("Wynik testu Morana I:"), verbatimTextOutput("test_n")
              )
            )
          ),
          tabPanel("I Morana w czasie", br(),
            plotOutput("mczas_n", height="460px"), hr(),
            h5("Tabela podsumowująca:"), tableOutput("tauto_n")
          ),
          tabPanel("Model panelowy FE", br(),
            h4("Model efektów stałych — Mieszkania oddane do użytkowania"),
            verbatimTextOutput("modsum_n")
          )
        )
      )
    )
  )
)

# ── SERVER ───────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Nawigacja ze strony głównej ─────────────────────────────
  nawiguj <- list(
    go_mapa_s = "mapa_s", go_wiz_s = "wiz_s", go_auto_s = "auto_s",
    go_mapa_n = "mapa_n", go_wiz_n = "wiz_n", go_auto_n = "auto_n"
  )
  lapply(names(nawiguj), function(btn) {
    observeEvent(input[[btn]], ignoreInit = TRUE, {
      updateNavbarPage(session, "nav", selected = nawiguj[[btn]])
    })
  })

  gif_paths <- list(
    gif_zuz  = "animacja_zuzycie_energii.gif",
    gif_mies = "animacja_mieszkania.gif"
  )
  lapply(names(gif_paths), function(btn) {
    observeEvent(input[[btn]], ignoreInit = TRUE, {
      sciezka <- normalizePath(gif_paths[[btn]], mustWork = FALSE)
      browseURL(paste0("file:///", gsub("\\\\", "/", sciezka)))
    })
  })

  # ── Mapa — stare dane ──────────────────────────────────────
  filtered_s <- reactive({
    cfg    <- mapa_zm_s[[input$zm_s]]
    df_rok <- dane_s %>% filter(rok==input$rok_s) %>% select(teryt, wojewodztwo, all_of(cfg$col))
    merge(mapa, df_rok, by.x="JPT_KOD_JE", by.y="teryt")
  })
  output$mapa_s <- renderPlot({
    md <- filtered_s(); cfg <- mapa_zm_s[[input$zm_s]]; col <- cfg$col
    ggplot(md) +
      geom_sf(aes(fill=.data[[col]]), color="white", linewidth=0.5) +
      scale_fill_gradient(low=cfg$low, high=cfg$high, name=input$zm_s, labels=scales::comma) +
      geom_sf_text(aes(label=wojewodztwo), size=2.8, color="grey20", fontface="bold", check_overlap=TRUE) +
      labs(title=input$zm_s, subtitle=paste("Rok:",input$rok_s), caption="Źródło: GUS BDL, Eurostat") +
      theme_void(base_size=13) +
      theme(plot.title=element_text(face="bold",hjust=0.5,size=16),
            plot.subtitle=element_text(hjust=0.5,color="grey40",size=12), legend.position="right")
  })
  output$stat_s <- renderTable({
    cfg <- mapa_zm_s[[input$zm_s]]; col <- cfg$col
    df  <- dane_s %>% filter(rok==input$rok_s) %>% pull(!!sym(col))
    data.frame(Miara=c("Minimum","Mediana","Średnia","Maximum"),
               Wartość=round(c(min(df,na.rm=T),median(df,na.rm=T),mean(df,na.rm=T),max(df,na.rm=T)),2))
  }, striped=TRUE, hover=TRUE, bordered=TRUE)
  output$tab_s <- renderTable({
    cfg <- mapa_zm_s[[input$zm_s]]; col <- cfg$col
    dane_s %>% filter(rok==input$rok_s) %>% select(wojewodztwo,!!sym(col)) %>%
      arrange(desc(!!sym(col))) %>% rename(Województwo=wojewodztwo,Wartość=!!sym(col)) %>%
      mutate(Wartość=round(Wartość,3))
  }, striped=TRUE, hover=TRUE, bordered=TRUE)

  # ── Wizualizacje — stare dane ──────────────────────────────
  output$wykres_s <- renderPlot({
    if (input$typ_s=="Animacja") return(NULL)
    if (input$typ_s=="Macierz korelacji") {
      zm_num <- dane_s[,c("zuzycie_energii_GWh","cena_energii_zl_kWh","ludnosc","urbanizacja_pct","pkb_mln_zl","hdd","cdd")]
      colnames(zm_num) <- c("Zużycie energii","Cena energii","Ludność","Urbanizacja %","PKB","HDD","CDD")
      corrplot(cor(zm_num,use="complete.obs"), method="color", type="upper", addCoef.col="black",
               number.cex=0.8, tl.col="black", tl.srt=45,
               col=colorRampPalette(c("#d73027","white","#1a9850"))(200),
               title="Macierz korelacji — stare dane", mar=c(0,0,2,0))
      return(invisible(NULL))
    }
    cfg <- wiz_zm_s[[input$wiz_zm_s]]; col <- cfg$col
    switch(input$typ_s,
      "Liniowy" = ggplot(dane_s,aes(x=rok,y=.data[[col]],color=wojewodztwo)) +
        geom_line(linewidth=0.8)+geom_point(size=1.2)+scale_color_manual(values=paleta_woj)+
        scale_x_continuous(breaks=seq(2004,2024,2))+
        labs(title=input$wiz_zm_s,subtitle="Województwa, 2004–2024",x="Rok",y=cfg$ylab,color="Województwo")+
        theme_minimal(base_size=12)+theme(legend.text=element_text(size=8),panel.grid.minor=element_blank(),plot.title=element_text(face="bold")),
      "Heatmapa" = { df_wide<-dcast(dane_s,rok~wojewodztwo,value.var=col)
        df_long<-melt(df_wide,id.vars="rok",variable.name="wojewodztwo",value.name="value")
        ggplot(df_long,aes(x=wojewodztwo,y=factor(rok),fill=value))+geom_tile(color="white",linewidth=0.3)+
          scale_fill_gradient(low="#ffffd4",high="#bd0026",name=cfg$jed)+labs(title=input$wiz_zm_s,x="Województwo",y="Rok")+
          theme_minimal(base_size=11)+theme(axis.text.x=element_text(angle=45,hjust=1,size=8),plot.title=element_text(face="bold")) },
      "Histogram" = { d<-subset(dane_s,rok==input$wiz_rok_s)
        ggplot(d,aes(x=.data[[col]]))+geom_histogram(bins=8,fill="#2166ac",color="white",alpha=0.85)+
          geom_vline(aes(xintercept=mean(.data[[col]])),color="#d73027",linewidth=1,linetype="dashed")+
          labs(title=paste(input$wiz_zm_s,"—",input$wiz_rok_s),subtitle="Linia przerywana = średnia",x=cfg$ylab,y="Liczba województw")+
          theme_minimal(base_size=12)+theme(plot.title=element_text(face="bold"),panel.grid.minor=element_blank()) },
      "Boxplot" = ggplot(dane_s,aes(x=reorder(wojewodztwo,.data[[col]],median),y=.data[[col]],fill=wojewodztwo))+
        geom_boxplot(outlier.shape=21,outlier.size=2,alpha=0.8)+scale_fill_manual(values=paleta_woj)+
        labs(title=paste("Zróżnicowanie —",input$wiz_zm_s),subtitle="Posortowane wg mediany",x=NULL,y=cfg$ylab)+
        coord_flip()+theme_minimal(base_size=12)+theme(legend.position="none",plot.title=element_text(face="bold"))
    )
  })

  # ── Autokorelacja — stare dane ─────────────────────────────
  dr_s <- reactive({ dane_s %>% filter(rok==input$auto_rok_s) %>% arrange(teryt) })
  output$test_s  <- renderPrint({ suppressWarnings(moran.test(dr_s()[[auto_zm_s[[input$auto_zm_s]]]],lw_s,alternative="two.sided",zero.policy=TRUE)) })
  output$mplot_s <- renderPlot({ zm<-auto_zm_s[[input$auto_zm_s]]; df<-dr_s()
    moran.plot(df[[zm]],lw_s,labels=df$wojewodztwo,pch=20,col="#2166ac",
               main=paste("Wykres Morana —",input$auto_zm_s,"—",input$auto_rok_s),
               xlab=paste(input$auto_zm_s,"(standaryzowane)"),ylab="Przestrzenne opóźnienie") })
  output$mczas_s <- renderPlot({
    ggplot(wyniki_s,aes(x=rok,y=moran_I,color=zmienna))+geom_line(linewidth=0.9)+
      geom_point(aes(shape=istotna),size=2.5)+scale_shape_manual(values=c(1,16),labels=c("p ≥ 0.05","p < 0.05"),name="Istotność")+
      geom_hline(yintercept=0,linetype="dashed",color="grey50")+scale_x_continuous(breaks=seq(2004,2024,2))+
      labs(title="Statystyka I Morana w czasie (2004–2024)",subtitle="Wypełniony punkt = istotne (p < 0.05)",x="Rok",y="Moran's I",color="Zmienna")+
      theme_minimal(base_size=12)+theme(plot.title=element_text(face="bold"),panel.grid.minor=element_blank()) })
  output$tauto_s <- renderTable({ wyniki_s %>% group_by(zmienna) %>%
      summarise(I_srednie=round(mean(moran_I),4),I_min=round(min(moran_I),4),I_max=round(max(moran_I),4),
                pct=paste0(round(mean(istotna)*100),"%"),.groups="drop") %>%
      rename(Zmienna=zmienna,`I średnie`=I_srednie,`I min`=I_min,`I max`=I_max,`% istotnych`=pct)
  }, striped=TRUE, hover=TRUE, bordered=TRUE)
  output$modsum_s <- renderPrint({ summary(model_fe_s) })

  # ── Mapa — nowe dane ───────────────────────────────────────
  filtered_n <- reactive({
    cfg    <- mapa_zm_n[[input$zm_n]]
    df_rok <- as.data.frame(dane_n) %>% filter(rok==input$rok_n) %>% select(teryt,wojewodztwo,all_of(cfg$col))
    merge(mapa, df_rok, by.x="JPT_KOD_JE", by.y="teryt")
  })
  output$mapa_n <- renderPlot({
    md <- filtered_n(); cfg <- mapa_zm_n[[input$zm_n]]; col <- cfg$col
    ggplot(md)+geom_sf(aes(fill=.data[[col]]),color="white",linewidth=0.5)+
      scale_fill_gradient(low=cfg$low,high=cfg$high,name=input$zm_n,labels=scales::comma)+
      geom_sf_text(aes(label=wojewodztwo),size=2.8,color="grey20",fontface="bold",check_overlap=TRUE)+
      labs(title=input$zm_n,subtitle=paste("Rok:",input$rok_n),caption="Źródło: GUS BDL")+
      theme_void(base_size=13)+
      theme(plot.title=element_text(face="bold",hjust=0.5,size=16),
            plot.subtitle=element_text(hjust=0.5,color="grey40",size=12),legend.position="right")
  })
  output$stat_n <- renderTable({
    cfg <- mapa_zm_n[[input$zm_n]]; col <- cfg$col
    df  <- as.data.frame(dane_n) %>% filter(rok==input$rok_n) %>% pull(!!sym(col))
    data.frame(Miara=c("Minimum","Mediana","Średnia","Maximum"),
               Wartość=round(c(min(df,na.rm=T),median(df,na.rm=T),mean(df,na.rm=T),max(df,na.rm=T)),2))
  }, striped=TRUE, hover=TRUE, bordered=TRUE)
  output$tab_n <- renderTable({
    cfg <- mapa_zm_n[[input$zm_n]]; col <- cfg$col
    as.data.frame(dane_n) %>% filter(rok==input$rok_n) %>% select(wojewodztwo,!!sym(col)) %>%
      arrange(desc(!!sym(col))) %>% rename(Województwo=wojewodztwo,Wartość=!!sym(col)) %>%
      mutate(Wartość=round(Wartość,3))
  }, striped=TRUE, hover=TRUE, bordered=TRUE)

  # ── Wizualizacje — nowe dane ────────────────────────────────
  output$wykres_n <- renderPlot({
    if (input$typ_n=="Animacja") return(NULL)
    df <- as.data.frame(dane_n)
    if (input$typ_n=="Macierz korelacji") {
      zm_num <- df[,c("MO","WSK25_34","WSK_URB","NAKŁ","WYNAGR","SM")]
      colnames(zm_num) <- c("Mieszkania","Ludność 25-34","Urbanizacja %","Nakłady","Wynagrodzenie","Saldo migracji")
      corrplot(cor(zm_num,use="complete.obs"),method="color",type="upper",addCoef.col="black",
               number.cex=0.8,tl.col="black",tl.srt=45,
               col=colorRampPalette(c("#d73027","white","#1a9850"))(200),
               title="Macierz korelacji — nowe dane",mar=c(0,0,2,0))
      return(invisible(NULL))
    }
    cfg <- wiz_zm_n[[input$wiz_zm_n]]; col <- cfg$col
    switch(input$typ_n,
      "Liniowy" = ggplot(df,aes(x=rok,y=.data[[col]],color=wojewodztwo))+
        geom_line(linewidth=0.8)+geom_point(size=1.2)+scale_color_manual(values=paleta_woj)+
        scale_x_continuous(breaks=seq(min(lata_n),max(lata_n),2))+
        labs(title=input$wiz_zm_n,subtitle="Województwa, 2004–2024",x="Rok",y=cfg$ylab,color="Województwo")+
        theme_minimal(base_size=12)+theme(legend.text=element_text(size=8),panel.grid.minor=element_blank(),plot.title=element_text(face="bold")),
      "Heatmapa" = { df_wide<-dcast(df,rok~wojewodztwo,value.var=col)
        df_long<-melt(df_wide,id.vars="rok",variable.name="wojewodztwo",value.name="value")
        ggplot(df_long,aes(x=wojewodztwo,y=factor(rok),fill=value))+geom_tile(color="white",linewidth=0.3)+
          scale_fill_gradient(low="#ffffd4",high="#bd0026",name=cfg$jed)+labs(title=input$wiz_zm_n,x="Województwo",y="Rok")+
          theme_minimal(base_size=11)+theme(axis.text.x=element_text(angle=45,hjust=1,size=8),plot.title=element_text(face="bold")) },
      "Histogram" = { d<-subset(df,rok==input$wiz_rok_n)
        ggplot(d,aes(x=.data[[col]]))+geom_histogram(bins=8,fill="#2166ac",color="white",alpha=0.85)+
          geom_vline(aes(xintercept=mean(.data[[col]])),color="#d73027",linewidth=1,linetype="dashed")+
          labs(title=paste(input$wiz_zm_n,"—",input$wiz_rok_n),subtitle="Linia przerywana = średnia",x=cfg$ylab,y="Liczba województw")+
          theme_minimal(base_size=12)+theme(plot.title=element_text(face="bold"),panel.grid.minor=element_blank()) },
      "Boxplot" = ggplot(df,aes(x=reorder(wojewodztwo,.data[[col]],median),y=.data[[col]],fill=wojewodztwo))+
        geom_boxplot(outlier.shape=21,outlier.size=2,alpha=0.8)+scale_fill_manual(values=paleta_woj)+
        labs(title=paste("Zróżnicowanie —",input$wiz_zm_n),subtitle="Posortowane wg mediany",x=NULL,y=cfg$ylab)+
        coord_flip()+theme_minimal(base_size=12)+theme(legend.position="none",plot.title=element_text(face="bold"))
    )
  })

  # ── Autokorelacja — nowe dane ───────────────────────────────
  dr_n <- reactive({ as.data.frame(dane_n) %>% filter(rok==input$auto_rok_n) %>% arrange(teryt) })
  output$test_n  <- renderPrint({ suppressWarnings(moran.test(dr_n()[[auto_zm_n[[input$auto_zm_n]]]],lw_n,alternative="two.sided",zero.policy=TRUE)) })
  output$mplot_n <- renderPlot({ zm<-auto_zm_n[[input$auto_zm_n]]; df<-dr_n()
    moran.plot(df[[zm]],lw_n,labels=df$wojewodztwo,pch=20,col="#2166ac",
               main=paste("Wykres Morana —",input$auto_zm_n,"—",input$auto_rok_n),
               xlab=paste(input$auto_zm_n,"(standaryzowane)"),ylab="Przestrzenne opóźnienie") })
  output$mczas_n <- renderPlot({
    ggplot(wyniki_n,aes(x=rok,y=moran_I,color=zmienna))+geom_line(linewidth=0.9)+
      geom_point(aes(shape=istotna),size=2.5)+scale_shape_manual(values=c(1,16),labels=c("p ≥ 0.05","p < 0.05"),name="Istotność")+
      geom_hline(yintercept=0,linetype="dashed",color="grey50")+scale_x_continuous(breaks=seq(min(lata_n),max(lata_n),2))+
      labs(title="Statystyka I Morana w czasie (2004–2024)",subtitle="Wypełniony punkt = istotne (p < 0.05)",x="Rok",y="Moran's I",color="Zmienna")+
      theme_minimal(base_size=12)+theme(plot.title=element_text(face="bold"),panel.grid.minor=element_blank()) })
  output$tauto_n <- renderTable({ wyniki_n %>% group_by(zmienna) %>%
      summarise(I_srednie=round(mean(moran_I),4),I_min=round(min(moran_I),4),I_max=round(max(moran_I),4),
                pct=paste0(round(mean(istotna)*100),"%"),.groups="drop") %>%
      rename(Zmienna=zmienna,`I średnie`=I_srednie,`I min`=I_min,`I max`=I_max,`% istotnych`=pct)
  }, striped=TRUE, hover=TRUE, bordered=TRUE)
  output$modsum_n <- renderPrint({ summary(model_fe_n) })
}

shinyApp(ui = ui, server = server)

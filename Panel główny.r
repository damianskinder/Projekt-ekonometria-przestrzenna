# ============================================================
#  PANEL GŁÓWNY - Projekt Ekonometria Przestrzenna
#  Temat: Mieszkania oddane do użytkowania (województwa, 2004-2024)
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
library(splm)
library(spatialreg)

# ── Wczytanie danych ─────────────────────────────────────────
mapa <- st_read("wojewodztwa.shp", quiet = TRUE)
mapa$JPT_KOD_JE <- sprintf("%02d", as.numeric(mapa$JPT_KOD_JE))
mapa <- mapa[order(mapa$JPT_KOD_JE), ]

dane_n <- read_excel("nowe_dane.xlsx", sheet = "Ludnosc_25_34")
names(dane_n)[names(dane_n) == "WSK25-34"] <- "WSK25_34"
names(dane_n)[grep("^NAK", names(dane_n))] <- "NAKL"
dane_n$teryt <- sprintf("%02d", as.numeric(dane_n$teryt))

addResourcePath("assets", normalizePath("."))

# ── Paleta ────────────────────────────────────────────────────
paleta_woj <- c(
  "#1f77b4","#ff7f0e","#2ca02c","#d62728","#9467bd",
  "#8c564b","#e377c2","#7f7f7f","#bcbd22","#17becf",
  "#aec7e8","#ffbb78","#98df8a","#ff9896","#c5b0d5","#c49c94"
)

lata_n <- sort(unique(dane_n$rok))

# ── Konfiguracje zmiennych ────────────────────────────────────
mapa_zm_n <- list(
  "Mieszkania oddane do użytkowania"          = list(col = "MO",       low = "lightyellow", high = "#2166ac"),
  "Ludność w wieku 25-34 lat"                 = list(col = "WSK25_34", low = "#fff7ec",     high = "#7f0000"),
  "Wskaznik urbanizacji [%]"                  = list(col = "WSK_URB",  low = "#f7fcf5",     high = "#00441b"),
  "Naklady inwestycyjne w sektorze prywatnym" = list(col = "NAKL",     low = "#f7fbff",     high = "#2166ac"),
  "Srednie wynagrodzenie [zl]"                = list(col = "WYNAGR",   low = "#fff5eb",     high = "#7f2704"),
  "Saldo migracji"                            = list(col = "SM",       low = "#ffffd9",     high = "#081d58")
)
wiz_zm_n <- list(
  "Mieszkania oddane do użytkowania"          = list(col = "MO",       ylab = "Liczba mieszkan",    jed = "Mieszkania"),
  "Ludność w wieku 25-34 lat"                 = list(col = "WSK25_34", ylab = "Liczba osob",        jed = "Osoby"),
  "Wskaźnik urbanizacji [%]"                  = list(col = "WSK_URB",  ylab = "Udzial [%]",         jed = "%"),
  "Nakłady inwestycyjne w sektorze prywatnym" = list(col = "NAKL",     ylab = "Naklady [zl]",       jed = "zl"),
  "Średnie wynagrodzenie [zł]"                = list(col = "WYNAGR",   ylab = "Wynagrodzenie [zl]", jed = "zl"),
  "Saldo migracji"                            = list(col = "SM",       ylab = "Saldo [os./1000]",   jed = "os./1000")
)
auto_zm_n <- list(
  "Mieszkania oddane do uzytkowania"          = "MO",
  "Ludnosc w wieku 25-34 lat"                 = "WSK25_34",
  "Wskaznik urbanizacji [%]"                  = "WSK_URB",
  "Naklady inwestycyjne w sektorze prywatnym" = "NAKL",
  "Srednie wynagrodzenie [zl]"                = "WYNAGR",
  "Saldo migracji"                            = "SM"
)
typy_wiz <- c("Liniowy", "Heatmapa", "Histogram", "Gestosc", "Rozrzut", "Boxplot", "Macierz korelacji", "Animacja")

# ── Macierz wag — sasiedztwo Queen ──────────────────────────
nb   <- poly2nb(mapa, queen = TRUE)
lw_n <- nb2listw(nb, style = "W", zero.policy = TRUE)

# ── Pre-obliczenie I Morana ───────────────────────────────────
oblicz_morana <- function(dane, zm_cfg, lata, lw) {
  wyniki <- data.frame()
  for (zm_label in names(zm_cfg)) {
    zm_col <- zm_cfg[[zm_label]]
    for (yr in lata) {
      df_yr <- as.data.frame(dane) %>% filter(rok == yr) %>% arrange(teryt)
      test  <- suppressWarnings(moran.test(df_yr[[zm_col]], lw, alternative = "two.sided", zero.policy = TRUE))
      wyniki <- rbind(wyniki, data.frame(
        zmienna = zm_label, rok = yr,
        moran_I = round(test$estimate["Moran I statistic"], 4),
        p_value = round(test$p.value, 4),
        istotna = test$p.value < 0.05))
    }
  }
  wyniki
}
wyniki_n <- oblicz_morana(dane_n, auto_zm_n, lata_n, lw_n)

# ── Model FE (plm) ───────────────────────────────────────────
pdata_n    <- pdata.frame(as.data.frame(dane_n), index = c("teryt", "rok"))
model_fe_n <- plm(MO ~ WSK_URB + NAKL + WYNAGR + SM + WSK25_34,
                  data = pdata_n, model = "within")

# ── OLS benchmark ────────────────────────────────────────────
model_ols_n <- lm(MO ~ WSK_URB + NAKL + WYNAGR + SM + WSK25_34,
                  data = as.data.frame(dane_n))

# ── Testy LM (selekcja SAR vs SEM) ───────────────────────────
oblicz_lmtesty <- function(model_fe, lw) {
  testy  <- c("lml", "lme", "rlml", "rlme")
  nazwy  <- c("LM-Lag (wskazuje SAR)", "LM-Error (wskazuje SEM)",
               "Robust LM-Lag", "Robust LM-Error")
  wyniki <- lapply(testy, function(t) slmtest(model_fe, lw, t))
  data.frame(
    Test       = nazwy,
    Statystyka = round(sapply(wyniki, function(r) as.numeric(r$statistic)), 4),
    `p-value`  = round(sapply(wyniki, function(r) r$p.value), 4),
    Istotnosc  = ifelse(sapply(wyniki, function(r) r$p.value) < 0.05, "Tak (p<0.05)", "Nie"),
    check.names = FALSE
  )
}
lmtab_n <- oblicz_lmtesty(model_fe_n, lw_n)

# ── Przestrzenne modele panelowe (splm) ──────────────────────
model_sar_n <- spml(MO ~ WSK_URB + NAKL + WYNAGR + SM + WSK25_34,
                    data = as.data.frame(dane_n), index = c("teryt", "rok"), listw = lw_n,
                    model = "within", lag = TRUE, spatial.error = "none")
model_sem_n <- spml(MO ~ WSK_URB + NAKL + WYNAGR + SM + WSK25_34,
                    data = as.data.frame(dane_n), index = c("teryt", "rok"), listw = lw_n,
                    model = "within", lag = FALSE, spatial.error = "b")

# ── pFtest — efekty stale vs pooled ──────────────────────────
model_pool_n <- plm(MO ~ WSK_URB + NAKL + WYNAGR + SM + WSK25_34,
                    data = pdata_n, model = "pooling")
pf_n <- pFtest(model_fe_n, model_pool_n)

# ── Efekty bezposrednie/posrednie/calkowite (LeSage & Pace) ──
oblicz_impacts <- function(model_sar, lw) {
  rho   <- as.numeric(model_sar$arcoef)[1]
  betas <- coef(model_sar)
  W     <- listw2mat(lw)
  n     <- nrow(W)
  S_W   <- solve(diag(n) - rho * W)
  dir_m <- sum(diag(S_W)) / n
  tot_m <- sum(S_W) / n
  ind_m <- tot_m - dir_m
  data.frame(
    Zmienna     = names(betas),
    Bezposredni = round(betas * dir_m, 4),
    Posredni    = round(betas * ind_m, 4),
    Calkowity   = round(betas * tot_m, 4)
  )
}
impacts_n <- oblicz_impacts(model_sar_n, lw_n)

# ── Helper: asymetria ─────────────────────────────────────────
skewness_val <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) return(NA)
  m <- mean(x); s <- sd(x)
  (n / ((n - 1) * (n - 2))) * sum(((x - m) / s)^3)
}

# ── Helper: outlier (metoda IQR) ──────────────────────────────
znajdz_outliery <- function(dane_rok, col, woj_col = "wojewodztwo") {
  x  <- dane_rok[[col]]
  woj <- dane_rok[[woj_col]]
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lo <- q1 - 1.5 * iqr; hi <- q3 + 1.5 * iqr
  idx <- which(x < lo | x > hi)
  if (length(idx) == 0) return(data.frame(Wojewodztwo = character(), Wartosc = numeric(), Typ = character()))
  data.frame(
    Wojewodztwo = woj[idx],
    Wartosc     = round(x[idx], 2),
    Typ         = ifelse(x[idx] < lo, "nizszy", "wyzszy")
  )
}

# ── Tabela porownawcza modeli ─────────────────────────────────
buduj_porownanie <- function(ols, sar, sem, y_vec) {
  ll  <- c(as.numeric(logLik(ols)), sar$logLik, sem$logLik)
  k   <- c(
    attr(logLik(ols), "df"),
    tryCatch(nrow(summary(sar)$CoefTable), error = function(e) length(sar$coefficients) + 2),
    tryCatch(nrow(summary(sem)$CoefTable), error = function(e) length(sem$coefficients) + 2)
  )
  aic   <- round(-2 * ll + 2 * k, 2)
  param <- c(
    "—",
    tryCatch(paste0("rho = ", round(sar$arcoef, 4)), error = function(e) "—"),
    tryCatch(paste0("lambda = ", round(sem$errcoef, 4)), error = function(e) "—")
  )
  r2 <- c(
    round(summary(ols)$r.squared, 4),
    tryCatch(round(cor(y_vec - as.numeric(resid(sar)), y_vec)^2, 4), error = function(e) NA),
    tryCatch(round(cor(y_vec - as.numeric(resid(sem)), y_vec)^2, 4), error = function(e) NA)
  )
  data.frame(
    Model                 = c("OLS (pooled)", "SAR panel FE", "SEM panel FE"),
    `Log-likelihood`      = round(ll, 2),
    AIC                   = aic,
    `R2 / pseudo-R2`      = r2,
    `Param. przestrzenny` = param,
    check.names = FALSE
  )
}
comp_n <- buduj_porownanie(model_ols_n, model_sar_n, model_sem_n,
                            y_vec = as.data.frame(dane_n)$MO)

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
  .btn-przejdz { background: #2563a8; color: #fff; border: none; }
  .btn-przejdz:hover, .btn-przejdz:focus { background: #1a3a5c; color: #fff; }
  .btn-gif { background: #ecfeff; color: #0891b2; border: 1.5px solid #0891b2; }
  .btn-gif:hover, .btn-gif:focus { background: #0891b2; color: #fff; }
  .lm-hint {
    background: #f0f7ff; border-left: 4px solid #2563a8;
    padding: 10px 14px; border-radius: 4px; font-size: 0.88em; color: #1a3a5c;
    margin-bottom: 16px;
  }
   .shiny-html-output table td,
  .shiny-html-output table th {
    white-space: nowrap;
    padding: 4px 10px;
  }
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
      actionButton(paste0("go_", id), "Przejdz",
                   class = "btn-nav btn-przejdz", icon = icon("arrow-right"))
    )
  )
}
karta_gif <- function(id, tytul, opis) {
  div(class = "skrypt-karta gif-karta",
    div(class = "karta-naglowek",
      span(class = "karta-ikona", "GIF"),
      h4(class = "karta-tytul", tytul)
    ),
    p(class = "karta-opis", opis),
    div(class = "karta-przyciski",
      actionButton(paste0("gif_", id), "Otworz animacje",
                   class = "btn-nav btn-gif", icon = icon("film"))
    )
  )
}

# ── UI ───────────────────────────────────────────────────────
ui <- navbarPage(
  "Panel Glowny", id = "nav",
  header = tags$head(tags$style(HTML(css))),

  # ── Strona glowna ─────────────────────────────────────────
  tabPanel("Strona glowna", value = "home",
    div(class = "sekcja",

      div(class = "sekcja-naglowek",
        "Dane o mieszkaniach oddanych do użytkowania — nowe_dane.xlsx",
        tags$br(),
        tags$span(style = "font-weight:400; color:#555;",
          "Zmienne: mieszkania oddane, ludnosc 25-34, urbanizacja, naklady, wynagrodzenie, migracja")
      ),
      div(class = "karty-rzad",
        karta("mapa_n",  "Mapa interaktywna",
              "Interaktywna mapa województw z suwakiem roku i wyborem zmiennej.", "Map"),
        karta("wiz_n",   "Wizualizacje",
              "Wykresy liniowe, heatmapy, histogramy, boxploty i macierz korelacji.", "Chart"),
        karta("auto_n",  "Autokorelacja przestrzenna",
              "Test Morana I, LISA (hot/cold spots), I Morana w czasie.", "Corr"),
        karta("model_n", "Modelowanie przestrzenne",
              "OLS benchmark, testy LM, modele SAR i SEM panel FE, porownanie AIC.", "Model"),
        karta_gif("mies", "Animacja — mieszkania oddane",
              "Zmiany liczby mieszkan oddanych do uzytkowania w latach 2004-2024.")
      )
    )
  ),

  # ── Dane o mieszkaniach ───────────────────────────────────
  navbarMenu("Dane o mieszkaniach oddanych do użytkowania",

    tabPanel("Mapa interaktywna", value = "mapa_n",
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput("zm_n", "Zmienna:", choices = names(mapa_zm_n)),
          sliderInput("rok_n", "Rok:", min = min(lata_n), max = max(lata_n), value = 2015, step = 1, sep = "",
                      animate = animationOptions(interval = 1200)),
          hr(), h5("Statystyki opisowe:"), tableOutput("stat_n"),
          hr(), h5("Obserwacje odstające (IQR):"), tableOutput("outlier_n")
        ),
        mainPanel(width = 9,
          plotOutput("mapa_n", height = "580px"), hr(),
          h5("Wszystkie województwa — wybrany rok:"), tableOutput("tab_n")
        )
      )
    ),

    tabPanel("Wizualizacje", value = "wiz_n",
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput("typ_n", "Typ wykresu:", choices = typy_wiz),
          conditionalPanel(
            condition = "input.typ_n != 'Macierz korelacji' && input.typ_n != 'Animacja'",
            selectInput("wiz_zm_n", "Zmienna:", choices = names(wiz_zm_n))
          ),
          conditionalPanel(
            condition = "input.typ_n == 'Histogram'",
            sliderInput("wiz_rok_n", "Rok:", min = min(lata_n), max = max(lata_n), value = 2015, step = 1, sep = "")
          ),
          conditionalPanel(
            condition = "input.typ_n == 'Rozrzut'",
            selectInput("scatter_x_n", "Zmienna na osi X:", choices = names(wiz_zm_n))
          )
        ),
        mainPanel(width = 9,
          conditionalPanel(condition = "input.typ_n != 'Animacja'",
            plotOutput("wykres_n", height = "560px")
          ),
          conditionalPanel(condition = "input.typ_n == 'Animacja'",
            div(style = "text-align:center; padding:30px;",
              tags$img(src = "assets/animacja_mieszkania.gif", width = "90%", style = "max-width:860px;"),
              p(style = "color:#888; font-size:.88em; margin-top:10px;",
                "Mieszkania oddane do uzytkowania — animacja 2004-2024")
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
              sidebarPanel(width = 3,
                selectInput("auto_zm_n", "Zmienna:", choices = names(auto_zm_n)),
                sliderInput("auto_rok_n", "Rok:", min = min(lata_n), max = max(lata_n), value = 2015, step = 1, sep = "")
              ),
              mainPanel(width = 9,
                plotOutput("mplot_n", height = "500px"),
                hr(), h5("Wynik testu Morana I:"), verbatimTextOutput("test_n")
              )
            )
          ),
          tabPanel("LISA — skupiska lokalne", br(),
            sidebarLayout(
              sidebarPanel(width = 3,
                selectInput("lisa_zm_n", "Zmienna:", choices = names(auto_zm_n)),
                sliderInput("lisa_rok_n", "Rok:", min = min(lata_n), max = max(lata_n), value = 2015, step = 1, sep = ""),
                hr(),
                p(style = "font-size:0.82em; color:#555; line-height:1.5;",
                  tags$b("HH (hot spot):"), " wysokie otoczone wysokimi", tags$br(),
                  tags$b("LL (cold spot):"), " niskie otoczone niskimi", tags$br(),
                  tags$b("HL:"), " wysokie otoczone niskimi", tags$br(),
                  tags$b("LH:"), " niskie otoczone wysokimi", tags$br(),
                  tags$b("NS:"), " nieistotne (p >= 0.05)"
                )
              ),
              mainPanel(width = 9,
                plotOutput("lisa_mapa_n", height = "520px"),
                hr(), h5("Wartosci LISA dla wybranego roku:"),
                tableOutput("lisa_tbl_n")
              )
            )
          ),
          tabPanel("I Morana w czasie", br(),
            plotOutput("mczas_n", height = "460px"), hr(),
            h5("Tabela podsumowująca:"), tableOutput("tauto_n")
          ),
          tabPanel("Model panelowy FE (referencja)", br(),
            h4("Model efektów stalych — Mieszkania oddane do uzytkowania"),
            verbatimTextOutput("modsum_n"),
            hr(),
            h5("Test F dla efektów stalych (FE vs pooled OLS):"),
            p(style = "color:#555; font-size:0.88em;",
              "H0: efekty indywidualne sa nieistotne (OLS pooled wystarcza). Odrzucenie H0 uzasadnia model FE."),
            verbatimTextOutput("pf_n_out")
          )
        )
      )
    ),

    tabPanel("Modelowanie przestrzenne", value = "model_n",
      fluidPage(br(),
        tabsetPanel(
          tabPanel("OLS (benchmark)", br(),
            h4("Model OLS (pooled) — Mieszkania oddane do użytkowania"),
            p(style = "color:#555; font-size:0.88em;",
              "Model klasyczny bez uwzględnienia struktury przestrzennej ani efektów indywidualnych."),
            verbatimTextOutput("modsum_ols_n")
          ),
          tabPanel("Testy LM", br(),
            h4("Testy Lagrange Multiplier — selekcja SAR vs SEM"),
            div(class = "lm-hint",
              tags$b("reguła decyzyjna:"), tags$br(),
              "1. Jesli oba LM istotne — patrz na Robust: istotny Robust LM-Lag wskazuje SAR, istotny Robust LM-Error wskazuje SEM.", tags$br(),
              "2. Jesli tylko jeden LM istotny — wybierz odpowiadajacy model.", tags$br(),
              "3. Jesli zadne Robust nieistotne — roznice miedzy modelami moga byc minimalne."
            ),
            tableOutput("lmtab_n_out"),
            hr(),
            h5("Moran I dla reszt OLS (srednia po latach):"),
            verbatimTextOutput("moran_reszty_n")
          ),
          tabPanel("Model SAR panel FE", br(),
            h4("Spatial Autoregressive Model (SAR) — panel, efekty stale"),
            p(style = "color:#555; font-size:0.88em;",
              "Rownanie: Y = rho * W * Y + X * beta + mu + epsilon",
              tags$br(),
              "Parametr rho mierzy przestrzenne oddzialywanie liczby oddawanych mieszkan miedzy wojewodztwami."),
            verbatimTextOutput("modsum_sar_n"),
            hr(),
            h5("Efekty bezposrednie, posrednie i calkowite (LeSage & Pace, 2009):"),
            p(style = "color:#555; font-size:0.88em;",
              "W modelu SAR wspolczynniki beta NIE sa bezposrednimi efektami marginalnymi.",
              tags$br(),
              "Bezposredni = wplyw zmiany Xi na Yi. Posredni (spillover) = wplyw na sasiednie jednostki. Calkowity = suma obu."),
            tableOutput("impacts_n_out")
          ),
          tabPanel("Model SEM panel FE", br(),
            h4("Spatial Error Model (SEM) — panel, efekty stale"),
            p(style = "color:#555; font-size:0.88em;",
              "Rownanie: Y = X * beta + mu + u,  u = lambda * W * u + epsilon",
              tags$br(),
              "Parametr lambda wychwytuje przestrzenna autokorelacje nieobserwowanych czynnikow."),
            verbatimTextOutput("modsum_sem_n")
          ),
          tabPanel("Porownanie modeli", br(),
            h4("Porownanie dopasowania modeli"),
            p(style = "color:#555; font-size:0.88em;",
              "Nizsze AIC = lepsze dopasowanie. Param. przestrzenny istotny = efekty przestrzenne sa istotne."),
            tableOutput("comp_tbl_n"),
            hr(),
            h5("Moran I dla reszt modeli — wyniki testu:"),
            verbatimTextOutput("moran_comp_n")
          )
        )
      )
    )
  )
)

# ── SERVER ───────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Nawigacja ze strony glownej ────────────────────────────
  nawiguj <- list(
    go_mapa_n  = "mapa_n",  go_wiz_n  = "wiz_n",  go_auto_n  = "auto_n",  go_model_n = "model_n"
  )
  lapply(names(nawiguj), function(btn) {
    observeEvent(input[[btn]], ignoreInit = TRUE, {
      updateNavbarPage(session, "nav", selected = nawiguj[[btn]])
    })
  })

  gif_paths <- list(gif_mies = "animacja_mieszkania.gif")
  lapply(names(gif_paths), function(btn) {
    observeEvent(input[[btn]], ignoreInit = TRUE, {
      sciezka <- normalizePath(gif_paths[[btn]], mustWork = FALSE)
      browseURL(paste0("file:///", gsub("\\\\", "/", sciezka)))
    })
  })

  # ── Mapa — dane o mieszkaniach ─────────────────────────────
  filtered_n <- reactive({
    cfg    <- mapa_zm_n[[input$zm_n]]
    df_rok <- as.data.frame(dane_n) %>% filter(rok == input$rok_n) %>%
      select(teryt, wojewodztwo, all_of(cfg$col))
    merge(mapa, df_rok, by.x = "JPT_KOD_JE", by.y = "teryt")
  })
  output$mapa_n <- renderPlot({
    md <- filtered_n(); cfg <- mapa_zm_n[[input$zm_n]]; col <- cfg$col
    ggplot(md) +
      geom_sf(aes(fill = .data[[col]]), color = "white", linewidth = 0.5) +
      scale_fill_gradient(low = cfg$low, high = cfg$high, name = input$zm_n, labels = scales::comma) +
      geom_sf_text(aes(label = wojewodztwo), size = 2.8, color = "grey20",
                   fontface = "bold", check_overlap = TRUE) +
      labs(title = input$zm_n, subtitle = paste("Rok:", input$rok_n), caption = "Zrodlo: GUS BDL") +
      theme_void(base_size = 13) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
            plot.subtitle = element_text(hjust = 0.5, color = "grey40", size = 12),
            legend.position = "right")
  })
  output$stat_n <- renderTable({
    cfg <- mapa_zm_n[[input$zm_n]]; col <- cfg$col
    df  <- as.data.frame(dane_n) %>% filter(rok == input$rok_n) %>% pull(!!sym(col))
    data.frame(
      Miara   = c("Minimum", "Q1 (25%)", "Mediana", "Srednia", "Q3 (75%)", "Maximum",
                  "Odch. std.", "Asymetria"),
      Wartosc = round(c(min(df, na.rm=T), quantile(df, 0.25, na.rm=T),
                        median(df, na.rm=T), mean(df, na.rm=T),
                        quantile(df, 0.75, na.rm=T), max(df, na.rm=T),
                        sd(df, na.rm=T), skewness_val(df)), 3))
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  output$outlier_n <- renderTable({
    cfg <- mapa_zm_n[[input$zm_n]]; col <- cfg$col
    df_rok <- as.data.frame(dane_n) %>% filter(rok == input$rok_n)
    out <- znajdz_outliery(df_rok, col)
    if (nrow(out) == 0) data.frame(Info = "Brak obserwacji odstajacych") else out
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  output$tab_n <- renderTable({
    cfg <- mapa_zm_n[[input$zm_n]]; col <- cfg$col
    as.data.frame(dane_n) %>% filter(rok == input$rok_n) %>% select(wojewodztwo, !!sym(col)) %>%
      arrange(desc(!!sym(col))) %>% rename(Wojewodztwo = wojewodztwo, Wartosc = !!sym(col)) %>%
      mutate(Wartosc = round(Wartosc, 3))
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # ── Wizualizacje — dane o mieszkaniach ────────────────────
  output$wykres_n <- renderPlot({
    if (input$typ_n == "Animacja") return(NULL)
    df <- as.data.frame(dane_n)
    if (input$typ_n == "Macierz korelacji") {
      zm_num <- df[, c("MO","WSK25_34","WSK_URB","NAKL","WYNAGR","SM")]
      colnames(zm_num) <- c("Mieszkania","Ludnosc 25-34","Urbanizacja %","Naklady","Wynagrodzenie","Saldo migracji")
      corrplot(cor(zm_num, use = "complete.obs"), method = "color", type = "upper",
               addCoef.col = "black", number.cex = 0.8, tl.col = "black", tl.srt = 45,
               col = colorRampPalette(c("#d73027","white","#1a9850"))(200),
               title = "Macierz korelacji — dane mieszkaniowe", mar = c(0, 0, 2, 0))
      return(invisible(NULL))
    }
    cfg <- wiz_zm_n[[input$wiz_zm_n]]; col <- cfg$col
    switch(input$typ_n,
      "Liniowy" = ggplot(df, aes(x = rok, y = .data[[col]], color = wojewodztwo)) +
        geom_line(linewidth = 0.8) + geom_point(size = 1.2) +
        scale_color_manual(values = paleta_woj) +
        scale_x_continuous(breaks = seq(min(lata_n), max(lata_n), 2)) +
        labs(title = input$wiz_zm_n, subtitle = "Wojewodztwa, 2004-2024",
             x = "Rok", y = cfg$ylab, color = "Wojewodztwo") +
        theme_minimal(base_size = 12) +
        theme(legend.text = element_text(size = 8), panel.grid.minor = element_blank(),
              plot.title = element_text(face = "bold")),
      "Heatmapa" = {
        df_wide <- dcast(df, rok ~ wojewodztwo, value.var = col)
        df_long <- melt(df_wide, id.vars = "rok", variable.name = "wojewodztwo", value.name = "value")
        ggplot(df_long, aes(x = wojewodztwo, y = factor(rok), fill = value)) +
          geom_tile(color = "white", linewidth = 0.3) +
          scale_fill_gradient(low = "#ffffd4", high = "#bd0026", name = cfg$jed) +
          labs(title = input$wiz_zm_n, x = "Wojewodztwo", y = "Rok") +
          theme_minimal(base_size = 11) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
                plot.title = element_text(face = "bold"))
      },
      "Histogram" = {
        d <- subset(df, rok == input$wiz_rok_n)
        ggplot(d, aes(x = .data[[col]])) +
          geom_histogram(bins = 8, fill = "#2166ac", color = "white", alpha = 0.85) +
          geom_vline(aes(xintercept = mean(.data[[col]])), color = "#d73027", linewidth = 1, linetype = "dashed") +
          labs(title = paste(input$wiz_zm_n, "—", input$wiz_rok_n), subtitle = "Linia przerywana = srednia",
               x = cfg$ylab, y = "Liczba wojewodztw") +
          theme_minimal(base_size = 12) +
          theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())
      },
      "Gestosc" = ggplot(df, aes(x = .data[[col]])) +
        geom_density(fill = "#2166ac", alpha = 0.35, color = "#2166ac", linewidth = 1) +
        geom_vline(aes(xintercept = mean(.data[[col]], na.rm = TRUE)),
                   color = "#d73027", linewidth = 1, linetype = "dashed") +
        geom_vline(aes(xintercept = median(.data[[col]], na.rm = TRUE)),
                   color = "#1a9850", linewidth = 1, linetype = "dotted") +
        labs(title = paste("Rozklad —", input$wiz_zm_n),
             subtitle = "Czerwona linia przerywana = srednia | Zielona kropkowana = mediana",
             x = cfg$ylab, y = "Gestosc") +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank()),
      "Rozrzut" = {
        cfg_x <- wiz_zm_n[[input$scatter_x_n]]; col_x <- cfg_x$col
        ggplot(df, aes(x = .data[[col_x]], y = .data[[col]], color = wojewodztwo)) +
          geom_point(size = 2.5, alpha = 0.7) +
          geom_smooth(aes(group = 1), method = "lm", se = TRUE,
                      color = "#d73027", linewidth = 1, linetype = "dashed", alpha = 0.15) +
          scale_color_manual(values = paleta_woj) +
          labs(title = paste(input$wiz_zm_n, "vs", input$scatter_x_n),
               subtitle = "Kazdy punkt = jedno wojewodztwo w jednym roku | linia = trend OLS",
               x = cfg_x$ylab, y = cfg$ylab, color = "Wojewodztwo") +
          theme_minimal(base_size = 12) +
          theme(legend.text = element_text(size = 8), panel.grid.minor = element_blank(),
                plot.title = element_text(face = "bold"))
      },
      "Boxplot" = ggplot(df, aes(x = reorder(wojewodztwo, .data[[col]], median),
                                  y = .data[[col]], fill = wojewodztwo)) +
        geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8) +
        scale_fill_manual(values = paleta_woj) +
        labs(title = paste("Zroznicowanie —", input$wiz_zm_n), subtitle = "Posortowane wg mediany",
             x = NULL, y = cfg$ylab) +
        coord_flip() + theme_minimal(base_size = 12) +
        theme(legend.position = "none", plot.title = element_text(face = "bold"))
    )
  })

  # ── Autokorelacja — dane o mieszkaniach ───────────────────
  dr_n <- reactive({ as.data.frame(dane_n) %>% filter(rok == input$auto_rok_n) %>% arrange(teryt) })
  output$test_n  <- renderPrint({ suppressWarnings(moran.test(dr_n()[[auto_zm_n[[input$auto_zm_n]]]], lw_n, alternative = "two.sided", zero.policy = TRUE)) })
  output$mplot_n <- renderPlot({
    zm <- auto_zm_n[[input$auto_zm_n]]; df <- dr_n()
    moran.plot(df[[zm]], lw_n, labels = df$wojewodztwo, pch = 20, col = "#2166ac",
               main = paste("Wykres Morana —", input$auto_zm_n, "—", input$auto_rok_n),
               xlab = paste(input$auto_zm_n, "(standaryzowane)"), ylab = "Przestrzenne opoznienie")
  })
  output$mczas_n <- renderPlot({
    ggplot(wyniki_n, aes(x = rok, y = moran_I, color = zmienna)) +
      geom_line(linewidth = 0.9) +
      geom_point(aes(shape = istotna), size = 2.5) +
      scale_shape_manual(values = c(1, 16), labels = c("p >= 0.05", "p < 0.05"), name = "Istotnosc") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      scale_x_continuous(breaks = seq(min(lata_n), max(lata_n), 2)) +
      labs(title = "Statystyka I Morana w czasie",
           subtitle = "Wypelniony punkt = istotne (p < 0.05)",
           x = "Rok", y = "Moran's I", color = "Zmienna") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())
  })
  output$tauto_n <- renderTable({
    wyniki_n %>% group_by(zmienna) %>%
      summarise(I_srednie = round(mean(moran_I), 4), I_min = round(min(moran_I), 4),
                I_max = round(max(moran_I), 4), pct = paste0(round(mean(istotna) * 100), "%"),
                .groups = "drop") %>%
      rename(Zmienna = zmienna, `I srednie` = I_srednie, `I min` = I_min,
             `I max` = I_max, `% istotnych` = pct)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  output$modsum_n  <- renderPrint({ summary(model_fe_n) })
  output$pf_n_out  <- renderPrint({ pf_n })

  # ── LISA — dane o mieszkaniach ────────────────────────────
  lisa_data_n <- reactive({
    zm  <- auto_zm_n[[input$lisa_zm_n]]
    df  <- as.data.frame(dane_n) %>% filter(rok == input$lisa_rok_n) %>% arrange(teryt)
    x   <- df[[zm]]
    x_std   <- as.vector(scale(x))
    lag_std <- as.vector(lag.listw(lw_n, x_std))
    lisa    <- suppressWarnings(localmoran(x, lw_n, zero.policy = TRUE))
    p_val   <- lisa[, 5]
    kategoria <- dplyr::case_when(
      x_std > 0 & lag_std > 0 & p_val < 0.05 ~ "HH (hot spot)",
      x_std < 0 & lag_std < 0 & p_val < 0.05 ~ "LL (cold spot)",
      x_std > 0 & lag_std < 0 & p_val < 0.05 ~ "HL (high-low)",
      x_std < 0 & lag_std > 0 & p_val < 0.05 ~ "LH (low-high)",
      TRUE ~ "NS (nieistotna)"
    )
    data.frame(teryt = df$teryt, wojewodztwo = df$wojewodztwo,
               lisa_I = round(lisa[, 1], 4), p_val = round(p_val, 4), kategoria = kategoria)
  })
  paleta_lisa <- c("HH (hot spot)" = "#d73027", "LL (cold spot)" = "#4575b4",
                   "HL (high-low)" = "#fdae61", "LH (low-high)" = "#abd9e9",
                   "NS (nieistotna)" = "#eeeeee")
  output$lisa_mapa_n <- renderPlot({
    df_lisa <- lisa_data_n()
    md <- merge(mapa, df_lisa, by.x = "JPT_KOD_JE", by.y = "teryt")
    ggplot(md) +
      geom_sf(aes(fill = kategoria), color = "white", linewidth = 0.5) +
      scale_fill_manual(values = paleta_lisa, name = "Typ skupiska LISA", drop = FALSE) +
      geom_sf_text(aes(label = wojewodztwo), size = 2.6, color = "grey20",
                   fontface = "bold", check_overlap = TRUE) +
      labs(title = paste("LISA —", input$lisa_zm_n),
           subtitle = paste("Rok:", input$lisa_rok_n, "| Istotnosc p < 0.05"),
           caption = "Zrodlo: obliczenia wlasne, spdep::localmoran") +
      theme_void(base_size = 13) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
            plot.subtitle = element_text(hjust = 0.5, color = "grey40", size = 12),
            legend.position = "right")
  })
  output$lisa_tbl_n <- renderTable({
    lisa_data_n() %>%
      select(Wojewodztwo = wojewodztwo, `LISA I` = lisa_I, `p-value` = p_val, Kategoria = kategoria) %>%
      arrange(Kategoria, desc(`LISA I`))
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # ── Modelowanie przestrzenne — dane o mieszkaniach ─────────
  output$modsum_ols_n <- renderPrint({ summary(model_ols_n) })
  output$lmtab_n_out  <- renderTable({ lmtab_n }, striped = TRUE, hover = TRUE, bordered = TRUE)
  output$moran_reszty_n <- renderPrint({
    df_n <- as.data.frame(dane_n)
    res_sr <- df_n %>% mutate(reszta = resid(model_ols_n)) %>%
      group_by(teryt) %>% summarise(reszta = mean(reszta), .groups = "drop") %>% arrange(teryt)
    cat("Moran I dla reszt OLS (srednia reszty po latach, per wojewodztwo):\n\n")
    print(suppressWarnings(moran.test(res_sr$reszta, lw_n, alternative = "two.sided", zero.policy = TRUE)))
  })
  output$modsum_sar_n  <- renderPrint({ summary(model_sar_n) })
  output$impacts_n_out <- renderTable({ impacts_n }, striped = TRUE, hover = TRUE, bordered = TRUE)
  output$modsum_sem_n <- renderPrint({ summary(model_sem_n) })
  output$comp_tbl_n   <- renderTable({ comp_n }, striped = TRUE, hover = TRUE, bordered = TRUE)
  output$moran_comp_n <- renderPrint({
    df_n <- as.data.frame(dane_n)
    res_ols <- df_n %>% mutate(r = resid(model_ols_n)) %>%
      group_by(teryt) %>% summarise(r = mean(r), .groups = "drop") %>% arrange(teryt)
    cat("=== Moran I — reszty OLS (srednia po latach) ===\n")
    print(suppressWarnings(moran.test(res_ols$r, lw_n, alternative = "two.sided", zero.policy = TRUE)))
    cat("\n=== Moran I — reszty SAR panel FE (srednia po latach) ===\n")
    res_sar_df <- data.frame(teryt = df_n$teryt, r = as.numeric(resid(model_sar_n)))
    res_sar_sr <- res_sar_df %>% group_by(teryt) %>% summarise(r = mean(r), .groups = "drop") %>% arrange(teryt)
    print(suppressWarnings(moran.test(res_sar_sr$r, lw_n, alternative = "two.sided", zero.policy = TRUE)))
    cat("\n=== Moran I — reszty SEM panel FE (srednia po latach) ===\n")
    res_sem_df <- data.frame(teryt = df_n$teryt, r = as.numeric(resid(model_sem_n)))
    res_sem_sr <- res_sem_df %>% group_by(teryt) %>% summarise(r = mean(r), .groups = "drop") %>% arrange(teryt)
    print(suppressWarnings(moran.test(res_sem_sr$r, lw_n, alternative = "two.sided", zero.policy = TRUE)))
  })
}

shinyApp(ui = ui, server = server)

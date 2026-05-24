# ============================================================
#  Modelowanie przestrzenne — zuzycie energii elektrycznej
#  Dane: panel_wojewodztwa_2004_2024.csv | N=16 T=2004-2024
#  Pipeline: OLS → FE → pFtest → LM testy → SAR → SEM
#            → Efekty bezposrednie/posrednie/calkowite
#            → Tabela porownawcza modeli
# ============================================================

library(sf)
library(spdep)
library(plm)
library(splm)
library(dplyr)
library(ggplot2)

# ── Wczytanie danych ─────────────────────────────────────────
dane <- read.csv("panel_wojewodztwa_2004_2024.csv", header = TRUE,
                 sep = ",", dec = ".", encoding = "UTF-8")
dane$pkb_mld_zl <- dane$pkb_mln_zl / 1000
dane$teryt      <- sprintf("%02d", as.numeric(dane$teryt))

# ── Macierz wag przestrzennych ────────────────────────────────
mapa <- st_read("wojewodztwa.shp", quiet = TRUE)
mapa$JPT_KOD_JE <- sprintf("%02d", as.numeric(mapa$JPT_KOD_JE))
mapa <- mapa[order(mapa$JPT_KOD_JE), ]
nb   <- poly2nb(mapa, queen = TRUE)
lw   <- nb2listw(nb, style = "W", zero.policy = TRUE)

# ── Dane panelowe ─────────────────────────────────────────────
pdata <- pdata.frame(dane, index = c("teryt", "rok"))

FORMULA <- zuzycie_energii_GWh ~ dochod_os + urbanizacja_pct + cena_energii_zl_kWh + hdd + cdd

# ============================================================
#  1. MODEL OLS (pooling)
# ============================================================
cat("\n========== MODEL OLS (Pooling) ==========\n")
model_ols  <- plm(FORMULA, data = pdata, model = "pooling")
sum_ols    <- summary(model_ols)
print(sum_ols)

# ============================================================
#  2. MODEL EFEKTOW STALYCH (FE)
# ============================================================
cat("\n========== MODEL EFEKTOW STALYCH (FE) ==========\n")
model_fe  <- plm(FORMULA, data = pdata, model = "within")
sum_fe    <- summary(model_fe)
print(sum_fe)

# ── Test F: FE vs OLS ────────────────────────────────────────
cat("\n--- Test F: FE vs OLS (pFtest) ---\n")
pf <- pFtest(model_fe, model_ols)
print(pf)

# ============================================================
#  3. TESTY LM (Lagrange Multiplier) — wybor modelu
# ============================================================
cat("\n========== TESTY LAGRANGE MULTIPLIER ==========\n")

ROK_LM   <- 2015
dane_lm  <- dane %>% filter(rok == ROK_LM) %>% arrange(teryt)
model_lm_ols <- lm(zuzycie_energii_GWh ~ dochod_os + urbanizacja_pct +
                     cena_energii_zl_kWh + hdd + cdd, data = dane_lm)

cat("\nLM-lag (SAR):\n");   print(slmtest(model_lm_ols, lw, test = "lml"))
cat("\nLM-err (SEM):\n");   print(slmtest(model_lm_ols, lw, test = "lme"))
cat("\nRLM-lag (SAR):\n");  print(slmtest(model_lm_ols, lw, test = "rlml"))
cat("\nRLM-err (SEM):\n");  print(slmtest(model_lm_ols, lw, test = "rlme"))

# ============================================================
#  4. MODEL SAR — przestrzenny model autoregresyjny (panel FE)
# ============================================================
cat("\n========== MODEL SAR (panel FE) ==========\n")
model_sar <- spml(FORMULA, data = dane, index = c("teryt", "rok"),
                  listw = lw, lag = TRUE, spatial.error = "none",
                  model = "within", effect = "individual")
sum_sar   <- summary(model_sar)
print(sum_sar)

# ============================================================
#  5. MODEL SEM — przestrzenny model bledu (panel FE)
# ============================================================
cat("\n========== MODEL SEM (panel FE) ==========\n")
model_sem <- spml(FORMULA, data = dane, index = c("teryt", "rok"),
                  listw = lw, lag = FALSE, spatial.error = "b",
                  model = "within", effect = "individual")
sum_sem   <- summary(model_sem)
print(sum_sem)

# ============================================================
#  6. EFEKTY BEZPOSREDNIE, POSREDNIE I CALKOWITE (LeSage & Pace 2009)
# ============================================================
cat("\n========== EFEKTY PRZESTRZENNE SAR ==========\n")

rho_s  <- tryCatch(as.numeric(model_sar$arcoef)[1], error = function(e) NA)
betas  <- coef(model_sar)
W_mat  <- listw2mat(lw)
n_s    <- nrow(W_mat)
S_W    <- solve(diag(n_s) - rho_s * W_mat)
dir_m  <- sum(diag(S_W)) / n_s
tot_m  <- sum(S_W)      / n_s
ind_m  <- tot_m - dir_m

impacts_tab <- data.frame(
  Zmienna      = names(betas),
  Bezposredni  = round(betas * dir_m, 4),
  Posredni     = round(betas * ind_m, 4),
  Calkowity    = round(betas * tot_m, 4)
)
print(impacts_tab)

cat(sprintf("\nrho (SAR) = %.4f | mnoznik direct = %.4f | indirect = %.4f | total = %.4f\n",
            rho_s, dir_m, ind_m, tot_m))

# ============================================================
#  7. TABELA POROWNAWCZA MODELI
# ============================================================
cat("\n========== TABELA POROWNAWCZA ==========\n")

rho_val   <- tryCatch(round(as.numeric(model_sar$arcoef)[1], 4),  error = function(e) NA)
lambda_val <- tryCatch(round(as.numeric(model_sem$errcoef)[1], 4), error = function(e) NA)
ll_ols    <- tryCatch(round(as.numeric(logLik(model_ols))[1], 2),  error = function(e) NA)
ll_fe     <- tryCatch(round(as.numeric(logLik(model_fe))[1],  2),  error = function(e) NA)
ll_sar    <- tryCatch(round(as.numeric(model_sar$logLik)[1],  2),  error = function(e) NA)
ll_sem    <- tryCatch(round(as.numeric(model_sem$logLik)[1],  2),  error = function(e) NA)
aic_ols   <- tryCatch(round(AIC(model_ols), 2), error = function(e) NA)
aic_fe    <- tryCatch(round(AIC(model_fe),  2), error = function(e) NA)
r2_ols    <- tryCatch(round(sum_ols$r.squared["rsq"], 4), error = function(e) NA)
r2_fe     <- tryCatch(round(sum_fe$r.squared["rsq"],  4), error = function(e) NA)

tabela_por <- data.frame(
  Model        = c("OLS (pooling)", "FE (within)", "SAR (panel FE)", "SEM (panel FE)"),
  Log_Lik      = c(ll_ols,  ll_fe,  ll_sar, ll_sem),
  AIC          = c(aic_ols, aic_fe, NA,     NA),
  R2           = c(r2_ols,  r2_fe,  NA,     NA),
  Rho_Lambda   = c(NA, NA, rho_val, lambda_val)
)

print(tabela_por)

# ============================================================
#  8. WYKRES: wspolczynniki modelu FE vs SAR
# ============================================================
coef_fe  <- data.frame(Zmienna = names(coef(model_fe)),
                        Wspolczynnik = coef(model_fe), Model = "FE")
coef_sar_df <- data.frame(Zmienna = names(betas),
                           Wspolczynnik = betas, Model = "SAR")
coef_both <- rbind(coef_fe, coef_sar_df)

ggplot(coef_both, aes(x = Zmienna, y = Wspolczynnik, fill = Model)) +
  geom_col(position = "dodge", alpha = 0.85) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_fill_manual(values = c("FE" = "#2166ac", "SAR" = "#d6604d")) +
  labs(title = "Porownanie wspolczynnikow: FE vs SAR",
       subtitle = "Zuzycie energii elektrycznej",
       x = NULL, y = "Wspolczynnik") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 30, hjust = 1))

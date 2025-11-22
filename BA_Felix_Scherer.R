# ============================================================================
# BA L'isolement n'est pas une solution - oder doch?: Vollständige Analyse
# Felix Scherer, Dezember 2025
# ============================================================================

# ============================================================================
# 0. SETUP
# ============================================================================

library(tidyverse)
library(here)
library(readxl)
library(pxR)
library(pdftools)
library(stringi)
library(progress)
library(psych)
library(car)
library(lmtest)
library(sandwich)
library(stargazer)

# ============================================================================
# 1. GESELLSCHAFTLICHE PRÄFERENZEN (CSS-Daten)
# ============================================================================

df_css <- read.csv(here("data", "raw", "css_sicherheit.csv"))

# Variablen definieren
sozio_vars <- c("gndr", "reg", "canton", "edulvlcat3")
id_time_var <- "year"
age_var <- "age"

metric_vars <- df_css %>%
  select(-all_of(c(id_time_var, sozio_vars))) %>%
  names()
metric_vars <- unique(c(age_var, metric_vars))

# Sonderwellen entfernen
df_css <- df_css %>%
  filter(!(year %in% c(2020.6, 2022.5)))

# Jährliche Mittelwerte
yearly_means <- df_css %>%
  group_by(year) %>%
  summarise(
    across(all_of(metric_vars),
           ~ mean(suppressWarnings(as.numeric(.)), na.rm = TRUE),
           .names = "mean_{.col}"),
    .groups = "drop"
  )

# Soziodemographische Anteile berechnen
share_one <- function(data, varname) {
  data %>%
    mutate(!!varname := as.factor(.data[[varname]])) %>%
    count(year, !!sym(varname), name = "n") %>%
    group_by(year) %>%
    mutate(share = n / sum(n)) %>%
    ungroup() %>%
    rename(level = !!sym(varname)) %>%
    pivot_wider(
      names_from = level,
      values_from = share,
      names_prefix = paste0("share_", varname, "_")
    )
}

share_tables <- map(sozio_vars, ~ share_one(df_css, .x))
yearly_shares <- reduce(share_tables, full_join, by = "year") %>%
  arrange(year)

# Zusammenführen und filtern
yearly_summary <- yearly_means %>%
  left_join(yearly_shares, by = "year") %>%
  filter(year >= 2010 & year <= 2025)

write.csv(yearly_summary, here("data", "processed", "css_yearly_means.csv"), row.names = FALSE)

# ============================================================================
# 2. EU-VERFLECHTUNGSINDEX: DATENAUFBEREITUNG
# ============================================================================

# --- 2.1 Ausländische EU-Bevölkerung ---

df_foreign_raw <- read_excel(here("data", "raw", "zuwanderung.xlsx"), 
                             sheet = "T 01.05.01.01", skip = 2)
colnames(df_foreign_raw)[1] <- "staatsangehoerigkeit"

jahre <- 2010:2023
spalten_indices <- 31:44
eu_zeile <- which(df_foreign_raw$staatsangehoerigkeit == "EU-Staaten")

df_foreign <- data.frame(
  jahr = jahre,
  eu_bevoelkerung = as.numeric(df_foreign_raw[eu_zeile, spalten_indices])
)

write.csv(df_foreign, here("data", "processed", "eu_bevoelkerung.csv"), row.names = FALSE)

# --- 2.2 Handelsvolumen ---

df_trade_raw <- read_excel(here("data", "raw", "handelsvolumen.xlsx"))

df_trade <- df_trade_raw %>%
  filter(!is.na(.[[3]]), !grepl("^[0-9]|Quelle:|Auskunft:|Stand:|©", .[[1]])) %>%
  slice(4:n()) %>%
  setNames(c("category", "subcategory", as.character(1990:2024))) %>%
  pivot_longer(
    cols = `2010`:`2024`,
    names_to = "jahr", 
    values_to = "export_value"
  ) %>%
  mutate(jahr = as.numeric(jahr)) %>%
  filter(subcategory == "EU") %>%
  select(jahr, export_value) %>%
  arrange(jahr)

write.csv(df_trade, here("data", "processed", "handel_eu.csv"), row.names = FALSE)

# --- 2.3 Grenzgänger ---

grenz_raw <- read.px(here("data", "raw", "grenzgaenger.px"))
grenz_df <- as.data.frame(grenz_raw)

df_grenzgaenger <- grenz_df %>%
  rename(
    kanton = 1, 
    wohnsitzstaat = 2, 
    geschlecht = 3, 
    quartal = 4, 
    anzahl = value
  ) %>%
  filter(wohnsitzstaat %in% c("Deutschland", "Frankreich", "Italien", "Österreich")) %>%
  mutate(jahr = as.numeric(substr(quartal, 1, 4))) %>%
  group_by(jahr) %>%
  summarise(grenzgaenger = sum(anzahl, na.rm = TRUE)) %>%
  filter(jahr >= 2010 & jahr <= 2024)

write.csv(df_grenzgaenger, here("data", "processed", "grenzgaenger.csv"), row.names = FALSE)

# --- 2.4 Rechtliche Konvergenz (fedlex) ---

csv_in  <- here("data", "raw", "fedlex_AS_2010_2025.csv")
csv_out <- here("data", "processed", "fedlex_eu_labeled.csv")

eu_regex <- paste0(
  "(?is)",
  "(CELEX\\s*:?\\s*\\d{4}[A-Z]?\\d{4})|",
  "(Richtlinie\\s*\\(EU\\)|Verordnung\\s*\\(EU\\)|Beschluss\\s*\\(EU\\)|",
  "Weiterentwicklung\\s+des\\s+Schengen\\-?Besitzstands|Schengen|Dublin|",
  "Freizügigkeitsabkommen|FZA|EWR|EU[-\\s]?rechtskonform|Harmonisierung|",
  "Übernahme\\s+(des\\s+)?EU[-\\s]?Rechts)|",
  "(directive\\s*\\(UE\\)|règlement\\s*\\(UE\\)|décision\\s*\\(UE\\)|",
  "espace\\s+Schengen|accord\\s+sur\\s+la\\s+libre\\s+circulation)|",
  "(direttiva\\s*\\(UE\\)|regolamento\\s*\\(UE\\)|decisione\\s*\\(UE\\)|",
  "spazio\\s+Schengen|Dublino|accordo\\s+sulla\\s+libera\\s+circ)"
)

celex_id_rx <- "(?i)\\bCELEX\\s*:?\\s*([0-9]{4}[A-Z]?[0-9]{4})\\b"
context_chars <- 160

get_pdf_text <- function(url) {
  url <- URLdecode(url)
  tf <- tempfile(fileext = ".pdf")
  tryCatch({
    download.file(url, tf, mode = "wb", quiet = TRUE)
    txt <- pdftools::pdf_text(tf) %>% paste(collapse = "\n")
    stringi::stri_replace_all_regex(txt, "\\s+", " ") %>% stringi::stri_trim_both()
  }, error = function(e) NA_character_)
}

first_context <- function(text, pattern, chars = 160) {
  if (is.na(text)) return(NA_character_)
  m <- stringi::stri_locate_first_regex(text, pattern)
  if (all(is.na(m))) return(NA_character_)
  start <- max(1, m[1] - chars)
  end <- min(nchar(text), m[2] + chars)
  snip <- substr(text, start, end)
  stringi::stri_replace_all_regex(snip, "\\s+", " ") %>% 
    (\(z) paste0("…", z, "…"))()
}

process_block <- function(block_df) {
  pb <- progress::progress_bar$new(
    total = nrow(block_df), 
    format = "Block [:bar] :current/:total (:percent)"
  )
  texts <- map_chr(block_df$url_fixed, ~ { pb$tick(); get_pdf_text(.x) })
  
  block_df %>%
    mutate(
      text = texts,
      eu_flag = !is.na(text) & stringi::stri_detect_regex(text, eu_regex),
      celex_id = if_else(
        !is.na(text) & stringi::stri_detect_regex(text, celex_id_rx),
        stringi::stri_match_first_regex(text, celex_id_rx)[,2],
        NA_character_
      ),
      hit_count = ifelse(!is.na(text), 
                         lengths(stringi::stri_extract_all_regex(text, eu_regex)), 
                         0L),
      first_hit = map_chr(text, ~ first_context(.x, eu_regex, context_chars))
    ) %>%
    select(act, date, title, url = url_fixed, eu_flag, celex_id, hit_count, first_hit)
}

# Rohdaten laden
raw <- read_csv(csv_in, show_col_types = FALSE)
raw$url_fixed <- URLdecode(raw$url_fixed)
raw <- raw %>% distinct(act, .keep_all = TRUE)

# Bereits verarbeitete Einträge laden (falls vorhanden)
done_acts <- character(0)
if (file.exists(csv_out)) {
  done <- read_csv(csv_out, show_col_types = FALSE)
  done_acts <- done$act
}

todo <- raw %>% filter(!act %in% done_acts)

# Blockweise Verarbeitung
chunk_size <- 500
n <- nrow(todo)

if (n > 0) {
  for (i in seq(1, n, by = chunk_size)) {
    j <- min(i + chunk_size - 1, n)
    
    block <- todo[i:j, c("act", "date", "title", "url_fixed")]
    out_block <- process_block(block)
    
    if (!file.exists(csv_out)) {
      write_csv(out_block, csv_out)
    } else {
      write_csv(out_block, csv_out, append = TRUE)
    }
  }
}

# Jährliche Zusammenfassung erstellen
df_rechtskonvergenz <- read_csv(csv_out, show_col_types = FALSE) %>%
  mutate(jahr = as.integer(substr(as.character(date), 1, 4))) %>%
  count(jahr, eu_flag) %>%
  pivot_wider(names_from = eu_flag, values_from = n, values_fill = 0) %>%
  rename(eu_false = `FALSE`, eu_true = `TRUE`) %>%
  mutate(
    total = eu_true + eu_false,
    eu_share = eu_true / total
  ) %>%
  select(jahr, eu_share, eu_true, total)

write.csv(df_rechtskonvergenz, here("data", "processed", "rechtskonvergenz.csv"), row.names = FALSE)

# ============================================================================
# 3. PCA-INDEX KONSTRUKTION
# ============================================================================

# Daten laden
df_foreign <- read_csv(here("data", "processed", "eu_bevoelkerung.csv"), show_col_types = FALSE)
df_trade <- read_csv(here("data", "processed", "handel_eu.csv"), show_col_types = FALSE)
df_grenzgaenger <- read_csv(here("data", "processed", "grenzgaenger.csv"), show_col_types = FALSE)
df_rechtskonvergenz <- read_csv(here("data", "processed", "rechtskonvergenz.csv"), show_col_types = FALSE)

# Zusammenführen
df_index_raw <- df_rechtskonvergenz %>%
  inner_join(df_grenzgaenger, by = "jahr") %>%
  inner_join(df_foreign, by = "jahr") %>%
  inner_join(df_trade, by = "jahr") %>%
  arrange(jahr)

# Z-Standardisierung
vars <- c("eu_share", "grenzgaenger", "eu_bevoelkerung", "export_value")

stats_tbl <- df_index_raw %>%
  summarise(across(all_of(vars),
                   list(mean = ~mean(., na.rm = TRUE),
                        sd   = ~sd(., na.rm = TRUE)),
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(everything(),
               names_to = c("variable", ".value"),
               names_pattern = "(.*)_(mean|sd)")

mu <- setNames(stats_tbl$mean, stats_tbl$variable)
sigma <- setNames(stats_tbl$sd, stats_tbl$variable)

df_index_std <- df_index_raw %>%
  mutate(across(all_of(vars),
                ~ (. - mu[cur_column()]) / sigma[cur_column()],
                .names = "{.col}_z")) %>%
  select(jahr, ends_with("_z"))

write.csv(df_index_std, here("data", "processed", "index_standardisiert.csv"), row.names = FALSE)

# PCA Diagnostik
vars_z <- c("eu_share_z", "grenzgaenger_z", "eu_bevoelkerung_z", "export_value_z")
cor_mat <- cor(df_index_std[, vars_z])

print(KMO(cor_mat))
print(cortest.bartlett(cor_mat, n = nrow(df_index_std)))
print(alpha(df_index_std[, vars_z]))

# PCA durchführen
pca_model <- prcomp(df_index_std[vars_z], center = FALSE, scale. = FALSE)
print(summary(pca_model))
print(pca_model$rotation)

# Index erstellen
df_index <- df_index_std %>%
  mutate(
    PCA1_raw = pca_model$x[,1],
    PCA1_z = scale(PCA1_raw)[,1],
    PCA1_0_100 = (PCA1_z - min(PCA1_z)) / (max(PCA1_z) - min(PCA1_z)) * 100
  ) %>%
  select(jahr, PCA1_raw, PCA1_z, PCA1_0_100)

write.csv(df_index, here("data", "processed", "eu_index.csv"), row.names = FALSE)

# ============================================================================
# 4. DATEN ZUSAMMENFÜHREN
# ============================================================================

df_index <- df_index %>% rename(year = jahr)

df_merged <- yearly_means %>%
  inner_join(df_index, by = "year") %>%
  inner_join(df_index_std %>% rename(year = jahr), by = "year")

write.csv(df_merged, here("data", "processed", "analysis_data.csv"), row.names = FALSE)

# ============================================================================
# 5. REGRESSION
# ============================================================================

# Daten vorbereiten
df_analysis <- df_merged %>%
  arrange(year) %>%
  mutate(PCA1_z_lead = lead(PCA1_z)) %>%
  mutate(
    coopeco = mean_coopeco,
    coopnatojoin = mean_coopnatojoin,
    ntrclearpos = mean_ntrclearpos,
    coopeujoin_original = mean_coopeujoin,
    coopeujoin = -1 * mean_coopeujoin,
    savegnrl = mean_savegnrl,
    futch = mean_futch
  ) %>%
  filter(!is.na(PCA1_z_lead)) %>%
  select(year, PCA1_z_lead, coopeco, coopnatojoin, ntrclearpos, 
         coopeujoin, coopeujoin_original, savegnrl, futch)

# Deskriptive Statistik
print(summary(df_analysis %>% select(-year, -coopeujoin_original)))

# Korrelationsmatrix
cor_matrix <- df_analysis %>%
  select(PCA1_z_lead, coopeco, coopnatojoin, ntrclearpos, coopeujoin) %>%
  cor(use = "complete.obs")
print(round(cor_matrix, 3))

# Modelle schätzen
model1_koop <- lm(PCA1_z_lead ~ coopeco + coopnatojoin, data = df_analysis)
model2_auto <- lm(PCA1_z_lead ~ ntrclearpos + coopeujoin, data = df_analysis)
model3_full <- lm(PCA1_z_lead ~ coopeco + coopnatojoin + ntrclearpos + coopeujoin, data = df_analysis)
model4_reduced <- lm(PCA1_z_lead ~ coopnatojoin + ntrclearpos, data = df_analysis)

# Diagnostik: VIF
vif1 <- vif(model1_koop)
vif2 <- vif(model2_auto)
vif3 <- vif(model3_full)
vif4 <- vif(model4_reduced)

max_vif1 <- round(max(vif1), 2)
max_vif2 <- round(max(vif2), 2)
max_vif3 <- round(max(vif3), 2)
max_vif4 <- round(max(vif4), 2)

# Diagnostik: Durbin-Watson
dw_koop <- durbinWatsonTest(model1_koop)
dw_auto <- durbinWatsonTest(model2_auto)
dw_full <- durbinWatsonTest(model3_full)
dw_red <- durbinWatsonTest(model4_reduced)

# Diagnostik: Shapiro-Wilk & Breusch-Pagan
shapiro_full <- shapiro.test(residuals(model3_full))
shapiro_red <- shapiro.test(residuals(model4_reduced))
bp_full <- bptest(model3_full)
bp_red <- bptest(model4_reduced)

# Cook's Distance
cooks_d <- cooks.distance(model4_reduced)
influential <- which(cooks_d > 4/length(cooks_d))

# Residuenplots
par(mfrow = c(2, 2))
plot(model4_reduced)
par(mfrow = c(1, 1))

# Newey-West robuste SE
robust_koop <- coeftest(model1_koop, vcov = NeweyWest(model1_koop, lag = 2))
robust_auto <- coeftest(model2_auto, vcov = NeweyWest(model2_auto, lag = 2))
robust_full <- coeftest(model3_full, vcov = NeweyWest(model3_full, lag = 2))
robust_red <- coeftest(model4_reduced, vcov = NeweyWest(model4_reduced, lag = 2))

se_koop <- sqrt(diag(NeweyWest(model1_koop, lag = 2)))
se_auto <- sqrt(diag(NeweyWest(model2_auto, lag = 2)))
se_full <- sqrt(diag(NeweyWest(model3_full, lag = 2)))
se_red <- sqrt(diag(NeweyWest(model4_reduced, lag = 2)))

# Regressionstabelle exportieren
stargazer(model1_koop, model2_auto, model3_full, model4_reduced,
          type = "latex",
          out = here("output", "tables", "regression_results.tex"),
          title = "Determinanten der EU-Verflechtung (t+1)",
          column.labels = c("H1", "H2", "Vollmodell", "Reduziert"),
          dep.var.labels = "EU-Verflechtungsindex (z-std., t+1)",
          covariate.labels = c("H1a: Wirtschaftspragmatismus",
                               "H1b: NATO-Befürwortung",
                               "H2a: Diff. Neutralität",
                               "H2b: EU-Beitritts-Ablehnung"),
          se = list(se_koop, se_auto, se_full, se_red),
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Newey-West robuste SE (lag=2)",
          add.lines = list(
            c("Durbin-Watson", 
              round(dw_koop$dw, 2), 
              round(dw_auto$dw, 2),
              round(dw_full$dw, 2), 
              round(dw_red$dw, 2)),
            c("VIF (max)", max_vif1, max_vif2, max_vif3, max_vif4)
          ))

# Modellvergleich
comparison <- data.frame(
  Modell = c("H1", "H2", "Vollmodell", "Reduziert"),
  R2 = c(summary(model1_koop)$r.squared,
         summary(model2_auto)$r.squared,
         summary(model3_full)$r.squared, 
         summary(model4_reduced)$r.squared),
  Adj_R2 = c(summary(model1_koop)$adj.r.squared,
             summary(model2_auto)$adj.r.squared,
             summary(model3_full)$adj.r.squared,
             summary(model4_reduced)$adj.r.squared),
  AIC = c(AIC(model1_koop), AIC(model2_auto), AIC(model3_full), AIC(model4_reduced)),
  BIC = c(BIC(model1_koop), BIC(model2_auto), BIC(model3_full), BIC(model4_reduced))
)

comparison[, 2:5] <- round(comparison[, 2:5], 3)
print(comparison)

# ============================================================================
# ENDE
# ============================================================================
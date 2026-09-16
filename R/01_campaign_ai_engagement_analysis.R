# ============================================================
# ANALISI DESCRITTIVE - DATASET CAMPAGNE / AI DETECTION
# Focus: average_available_models
# ============================================================

library(tidyverse)
library(readxl)
library(ggplot2)
library(corrplot)
library(dplyr)
library(colorspace)
library(stringr)
library(scales)

# Crea le cartelle di output se non esistono
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

# 1. Importa dataset
dati <- read_excel("data/campaign_dataset.xlsx")

# 2. Pulizia variabili
dati <- dati %>%
  mutate(
    average_ai = as.numeric(average_available_models),
    likes = as.numeric(likes),
    comments = as.numeric(comments),
    sharing = as.numeric(sharing),
    fatturato = as.numeric(`Fatturato in euro`),
    numero_dipendenti = as.numeric(numero_dipendenti),
    brand = recode(
      as.character(brand),
      "CocaCola" = "Coca-Cola",
      "Ikea" = "IKEA",
      "Lego" = "LEGO"
    ),
    brand = as.factor(brand),
    campagna = as.factor(campagna),
    file_type = as.factor(file_type)
  )

# Controllo
str(dati)
summary(dati$average_ai)

# ============================================================
# 1. DESCRITTIVE GENERALI
# ============================================================

descrittive_generali <- dati %>%
  summarise(
    n_osservazioni = n(),
    n_brand = n_distinct(brand),
    n_campagne = n_distinct(campagna),
    media_average_ai = mean(average_ai, na.rm = TRUE),
    mediana_average_ai = median(average_ai, na.rm = TRUE),
    sd_average_ai = sd(average_ai, na.rm = TRUE),
    min_average_ai = min(average_ai, na.rm = TRUE),
    max_average_ai = max(average_ai, na.rm = TRUE)
  )

print(descrittive_generali)
pdf("output/figures/tutti_i_grafici.pdf", width = 10, height = 7)
# Istogramma average AI
ggplot(dati, aes(x = average_ai)) +
  geom_histogram(
    bins = 15,
    fill = "#D55E00",       # colore interno delle colonne
    color = "white",        # bordo delle colonne
    linewidth = 0.5,
    alpha = 0.85
  ) +
  labs(
    title = "Distribuzione dell'Average AI Score",
    x = "Average AI Score",
    y = "Frequenza"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14,
      hjust = 0.5
    ),
    axis.title = element_text(
      face = "bold"
    ),
    panel.grid.minor = element_blank()
  )

# ============================================================
# 2. CONTENUTI PER BRAND
# ============================================================

# ============================================================
# PALETTE DEI BRAND
# Colori della palette Okabe-Ito, accessibili ai daltonici
# ============================================================

colori_brand <- c(
  "LEGO"       = "#F0E442",
  "Zalando"    = "#E69F00",
  "YSL Beauty" = "#000000",
  "Heinz"      = "#009E73",
  "Coca-Cola"  = "#D55E00",
  "IKEA"       = "#0072B2",
  "Patagonia"  = "#56B4E9"
)

# Conteggio dei contenuti per brand
contenuti_brand <- dati %>%
  count(brand, sort = TRUE)

print(contenuti_brand)

# Grafico
ggplot(
  contenuti_brand,
  aes(
    x = reorder(brand, n),
    y = n,
    fill = brand
  )
) +
  geom_col(
    width = 0.75,
    color = "grey25",
    linewidth = 0.3
  ) +
  geom_text(
    aes(label = n),
    hjust = -0.35,
    size = 4
  ) +
  coord_flip() +
  scale_fill_manual(
    values = colori_brand
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.10))
  ) +
  labs(
    title = "Numero di contenuti per brand",
    subtitle = "Distribuzione dei contenuti inclusi nel campione",
    x = NULL,
    y = "Numero di contenuti"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 11,
      hjust = 0.5,
      color = "grey35"
    ),
    axis.title.x = element_text(
      size = 12,
      face = "bold"
    ),
    axis.text.y = element_text(
      size = 11
    ),
    axis.text.x = element_text(
      size = 10
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )
# ============================================================
# 3. AVERAGE AI SCORE PER BRAND
# ============================================================

ai_brand <- dati %>%
  group_by(brand) %>%
  summarise(
    n_contenuti = n(),
    n_campagne = n_distinct(campagna),
    media_average_ai = mean(average_ai, na.rm = TRUE),
    mediana_average_ai = median(average_ai, na.rm = TRUE),
    sd_average_ai = sd(average_ai, na.rm = TRUE),
    min_average_ai = min(average_ai, na.rm = TRUE),
    max_average_ai = max(average_ai, na.rm = TRUE)
  ) %>%
  arrange(desc(media_average_ai))

print(ai_brand)

ggplot(
  ai_brand,
  aes(
    x = reorder(brand, media_average_ai),
    y = media_average_ai,
    fill = brand
  )
) +
  geom_col(
    width = 0.75,
    color = "grey25",
    linewidth = 0.3
  ) +
  geom_text(
    aes(label = sprintf("%.3f", media_average_ai)),
    hjust = -0.25,
    size = 4
  ) +
  coord_flip() +
  scale_fill_manual(
    values = colori_brand
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Average AI Score medio per brand",
    subtitle = "Confronto descrittivo dell'Average AI Score medio tra i brand",
    x = NULL,
    y = "Average AI Score medio"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5
    ),
    
    plot.subtitle = element_text(
      size = 11,
      hjust = 0.5,
      color = "grey35"
    ),
    
    axis.title.x = element_text(
      size = 12,
      face = "bold"
    ),
    
    axis.text.y = element_text(
      size = 11
    ),
    
    axis.text.x = element_text(
      size = 10
    ),
    
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )
# ============================================================
# 4. AVERAGE AI SCORE PER CAMPAGNA
# ============================================================

ai_campagna <- dati %>%
  group_by(brand, campagna) %>%
  summarise(
    n_contenuti = n(),
    media_average_ai = mean(average_ai, na.rm = TRUE),
    mediana_average_ai = median(average_ai, na.rm = TRUE),
    sd_average_ai = sd(average_ai, na.rm = TRUE),
    max_average_ai = max(average_ai, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(media_average_ai))

print(ai_campagna)

# ============================================================
# ACRONIMI DELLE CAMPAGNE
# ============================================================

acronimi_campagne <- c(
  "Made Without PFAS_PFAS Free"          = "PFAS",
  "Human Powered_Worn Wear Snow Tour"    = "WWST",
  "Share a Coke"                         = "SAC",
  "MYSLF L’Absolu"                       = "MLA",
  "Feel It All_FIFA World Cup"           = "FIA",
  "STOCKHOLM 2025"                       = "STH25",
  "The Heinz Dipper"                     = "THD",
  "LEGO Star Wars_Rebuild the Galaxy"    = "LSW",
  "What Do I Wear_SS25"                  = "WDIW",
  "Breakfast Ketchup"                    = "BK",
  "Libre Berry Crush"                    = "LBC",
  "She Built That"                       = "SBT",
  "IKEA PS 2026"                         = "IPS26",
  "Lily Collins"                         = "LC"
)


# PREPARAZIONE DEI DATI DEL GRAFICO

ai_campagna_grafico <- ai_campagna %>%
  group_by(brand) %>%
  arrange(campagna, .by_group = TRUE) %>%
  mutate(
    numero_campagna = row_number(),
    chiave_colore = paste0(brand, "_", numero_campagna)
  ) %>%
  ungroup() %>%
  mutate(
    acronimo = unname(
      acronimi_campagne[as.character(campagna)]
    ),
    
    acronimo = if_else(
      is.na(acronimo),
      str_trunc(
        str_replace_all(campagna, "_", " "),
        width = 18
      ),
      acronimo
    ),
    
    etichetta_barra = paste0(
      acronimo,
      "  ",
      sprintf("%.3f", media_average_ai)
    )
  )

# DUE TONALITÀ PER CIASCUN BRAND

colori_campagne <- unlist(
  lapply(
    names(colori_brand),
    function(nome_brand) {
      
      colore_base <- colori_brand[[nome_brand]]
      if (nome_brand == "YSL Beauty") {
        
        colori <- c(
          "#737373",
          "#000000"
        )
        
      } else {
        
        colori <- c(
          colorspace::lighten(
            colore_base,
            amount = 0.28
          ),
          colorspace::darken(
            colore_base,
            amount = 0.12
          )
        )
      }
      
      names(colori) <- c(
        paste0(nome_brand, "_1"),
        paste0(nome_brand, "_2")
      )
      
      colori
    }
  )
)


# ORDINE DEI BRAND

ordine_brand <- ai_campagna_grafico %>%
  group_by(brand) %>%
  summarise(
    media_brand = mean(
      media_average_ai,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(media_brand) %>%
  pull(brand)


ai_campagna_grafico <- ai_campagna_grafico %>%
  mutate(
    brand = factor(
      brand,
      levels = ordine_brand
    )
  )

print(ai_campagna_grafico)

# GRAFICO A BARRE AFFIANCATE

ggplot(
  ai_campagna_grafico,
  aes(
    x = brand,
    y = media_average_ai,
    fill = chiave_colore
  )
) +
  
  geom_col(
    position = position_dodge(width = 0.78),
    width = 0.68,
    color = "grey25",
    linewidth = 0.3
  ) +
  
  geom_text(
    aes(
      label = etichetta_barra
    ),
    position = position_dodge(width = 0.78),
    hjust = -0.12,
    size = 3.4,
    fontface = "bold"
  ) +
  
  coord_flip() +
  
  scale_fill_manual(
    values = colori_campagne
  ) +
  
  scale_y_continuous(
    expand = expansion(
      mult = c(0, 0.28)
    )
  ) +
  
  labs(
    title = "Average AI Score medio per campagna",
    subtitle = "Per ogni brand sono confrontate le due campagne analizzate",
    x = "Brand",
    y = "Average AI Score medio",
    caption = "Le sigle accanto alle barre identificano le campagne; il numero indica il punteggio medio."
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    legend.position = "none",
    
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5
    ),
    
    plot.subtitle = element_text(
      size = 11,
      hjust = 0.5,
      color = "grey35"
    ),
    
    plot.caption = element_text(
      size = 9,
      color = "grey40",
      hjust = 0
    ),
    
    axis.title = element_text(
      face = "bold"
    ),
    
    axis.text.y = element_text(
      size = 11
    ),
    
    panel.grid.major.y = element_blank(),
    
    panel.grid.minor = element_blank()
  )
# ============================================================
# 5. IMAGE VS VIDEO
# ============================================================

formato_ai <- dati %>%
  group_by(file_type) %>%
  summarise(
    n = n(),
    media_average_ai = mean(average_ai, na.rm = TRUE),
    mediana_average_ai = median(average_ai, na.rm = TRUE),
    sd_average_ai = sd(average_ai, na.rm = TRUE)
  )

print(formato_ai)

ggplot(
  dati,
  aes(
    x = file_type,
    y = average_ai
  )
) +
  geom_boxplot(
    fill = "#56B4E9",
    color = "grey20",
    linewidth = 0.5,
    width = 0.65,
    outlier.color = "#D55E00",
    outlier.size = 2.2
  ) +
  scale_x_discrete(
    labels = c(
      "image" = "Immagine",
      "video" = "Video"
    )
  ) +
  labs(
    title = "Distribuzione dell’Average AI Score per formato di file",
    subtitle = "Confronto descrittivo dei punteggi in base al formato del contenuto",
    x = "Formato del contenuto",
    y = "Average AI Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      size = 15,
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 10.5,
      color = "grey35",
      hjust = 0.5
    ),
    axis.title = element_text(
      face = "bold"
    ),
    panel.grid.minor = element_blank()
  )


# ============================================================
# 5B. ACCORDO TRA I DETECTOR - SOLO IMMAGINI
# ============================================================

# Nomi delle tre variabili dei detector utilizzati sulle immagini
variabili_detector <- c(
  "openfake_p_fake",
  "omniaid_dino_v2_ai_probability",
  "dda_ai_probability"
)

# ------------------------------------------------------------
# 5B.1 Dataset contenente esclusivamente le immagini
# ------------------------------------------------------------

dati_immagini_detector <- dati %>%
  filter(file_type == "image") %>%
  mutate(
    across(
      all_of(variabili_detector),
      as.numeric
    )
  ) %>%
  filter(
    if_all(
      all_of(variabili_detector),
      ~ !is.na(.)
    )
  )


# ------------------------------------------------------------
# 5B.2 Statistiche descrittive dei tre detector
# ------------------------------------------------------------

descrittive_detector <- dati_immagini_detector %>%
  select(all_of(variabili_detector)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "detector",
    values_to = "score"
  ) %>%
  mutate(
    detector = recode(
      detector,
      "openfake_p_fake" = "OpenFake",
      "omniaid_dino_v2_ai_probability" = "OmniAID-DINO v2",
      "dda_ai_probability" = "Dual Data Alignment"
    )
  ) %>%
  group_by(detector) %>%
  summarise(
    n = n(),
    media = mean(score, na.rm = TRUE),
    mediana = median(score, na.rm = TRUE),
    deviazione_standard = sd(score, na.rm = TRUE),
    minimo = min(score, na.rm = TRUE),
    massimo = max(score, na.rm = TRUE),
    .groups = "drop"
  )

print(descrittive_detector)

# ------------------------------------------------------------
# 5B.3 Correlazione di Spearman tra i detector
# ------------------------------------------------------------

matrice_detector <- dati_immagini_detector %>%
  select(all_of(variabili_detector))

cor_detector <- cor(
  matrice_detector,
  method = "spearman",
  use = "complete.obs"
)

colnames(cor_detector) <- c(
  "OpenFake",
  "OmniAID-DINO v2",
  "Dual Data Alignment"
)

rownames(cor_detector) <- c(
  "OpenFake",
  "OmniAID-DINO v2",
  "Dual Data Alignment"
)

print(round(cor_detector, 3))



# ============================================================
# 6. ENGAGEMENT

# ------------------------------------------------------------
# 6.1 Engagement assoluto
# ------------------------------------------------------------

dati <- dati %>%
  mutate(
    engagement_assoluto = likes + comments + sharing
  )


# ------------------------------------------------------------
# 6.2 Trasformazione logaritmica
# ------------------------------------------------------------

dati <- dati %>%
  mutate(
    log_likes = log1p(likes),
    log_comments = log1p(comments),
    log_sharing = log1p(sharing)
  )


# ------------------------------------------------------------
# 6.3 Standardizzazione
# ------------------------------------------------------------

dati <- dati %>%
  mutate(
    z_likes = as.numeric(scale(log_likes)),
    z_comments = as.numeric(scale(log_comments)),
    z_sharing = as.numeric(scale(log_sharing))
  )


# ------------------------------------------------------------
# 6.4 Engagement Index
# ------------------------------------------------------------

dati <- dati %>%
  mutate(
    engagement_index_raw = (
      z_likes +
        z_comments +
        z_sharing
    ) / 3
  )

# ------------------------------------------------------------
# 6.5 Riscalamento dell'indice tra 0 e 1
# ------------------------------------------------------------

dati <- dati %>%
  mutate(
    engagement_index = scales::rescale(
      engagement_index_raw,
      to = c(0, 1)
    )
  )


##########################################################################
# ============================================================
# 6.5B TABELLA DATASET COMPLETO PER LA TESI
# ============================================================

tabella_dataset_completo <- dati %>%
  select(
    brand,
    campagna,
    file_type,
    average_ai,
    likes,
    comments,
    sharing,
    engagement_index
  ) %>%
  mutate(
    average_ai = round(average_ai, 3),
    engagement_index = round(engagement_index, 3)
  )

print(tabella_dataset_completo)
library(knitr)
tabella_latex <- tabella_dataset_completo %>%
  mutate(
    across(
      everything(),
      ~ ifelse(is.na(.), "--", as.character(.))
    )
  )

latex_code <- knitr::kable(
  tabella_latex,
  format = "latex",
  booktabs = TRUE,
  longtable = TRUE,
  escape = TRUE,
  caption = "Dataset completo utilizzato nell'analisi empirica",
  label = "tab:dataset_completo",
  align = c("l", "l", "l", "r", "r", "r", "r", "r")
)

writeLines(
  latex_code,
  "output/tables/tabella_dataset_completo.tex",
  useBytes = TRUE
)
# ------------------------------------------------------------
# 6.6 Statistiche descrittive
# ------------------------------------------------------------

engagement_descrittive <- dati %>%
  summarise(
    media_likes = mean(likes, na.rm = TRUE),
    mediana_likes = median(likes, na.rm = TRUE),
    
    media_comments = mean(comments, na.rm = TRUE),
    mediana_comments = median(comments, na.rm = TRUE),
    
    media_sharing = mean(sharing, na.rm = TRUE),
    mediana_sharing = median(sharing, na.rm = TRUE),
    
    media_engagement_assoluto =
      mean(engagement_assoluto, na.rm = TRUE),
    
    mediana_engagement_assoluto =
      median(engagement_assoluto, na.rm = TRUE),
    
    media_engagement_index =
      mean(engagement_index, na.rm = TRUE),
    
    mediana_engagement_index =
      median(engagement_index, na.rm = TRUE)
  )

print(engagement_descrittive)

# Average AI Score vs likes
ggplot(
  dati,
  aes(
    x = average_ai,
    y = likes
  )
) +
  geom_point(
    color = "#0072B2",
    size = 2.5,
    alpha = 0.7
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "grey20",
    linewidth = 1
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      big.mark = ".",
      decimal.mark = ",",
      accuracy = 1
    )
  ) +
  labs(
    title = "Relazione tra Average AI Score e like",
    subtitle = "Ogni punto rappresenta un contenuto",
    x = "Average AI Score",
    y = "Numero di like"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      size = 15,
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 10.5,
      color = "grey35",
      hjust = 0.5
    ),
    axis.title = element_text(
      face = "bold"
    ),
    panel.grid.minor = element_blank()
  )


# Average AI Score vs engagement assoluto
ggplot(
  dati,
  aes(
    x = average_ai,
    y = engagement_assoluto
  )
) +
  geom_point(
    color = "#009E73",
    size = 2.5,
    alpha = 0.7
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "grey20",
    linewidth = 1
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      big.mark = ".",
      decimal.mark = ",",
      accuracy = 1
    )
  ) +
  labs(
    title = "Relazione tra Average AI Score ed engagement assoluto",
    subtitle = "Engagement assoluto = like + commenti + condivisioni",
    x = "Average AI Score",
    y = "Engagement assoluto (numero di interazioni)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      size = 15,
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 10.5,
      color = "grey35",
      hjust = 0.5
    ),
    axis.title = element_text(
      face = "bold"
    ),
    panel.grid.minor = element_blank()
  )

# Average AI Score vs engagement totale (migliorato)

ggplot(
  dati,
  aes(
    x = average_ai,
    y = engagement_index,
    colour = brand
  )
) +
  geom_point(
    size = 3,
    alpha = 0.8
  ) +
  geom_smooth(
    aes(group = 1),
    method = "lm",
    se = FALSE,
    colour = "black",
    linewidth = 1
  ) +
  scale_colour_manual(
    values = colori_brand
  ) +
  scale_y_log10(
    labels = scales::label_number(big.mark = ".")
  ) +
  labs(
    title = "Relationship between Average AI Score and Engagement index",
    x = "Average AI Score",
    y = "Engagement index",
    colour = "Brand"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    
    plot.title = element_text(
      size = 15,
      face = "bold",
      hjust = 0.5
    ),
    
    plot.subtitle = element_text(
      size = 10.5,
      colour = "grey35",
      hjust = 0.5
    ),
    
    axis.title = element_text(
      face = "bold"
    ),
    
    panel.grid.minor = element_blank()
  )

# ============================================================
# 7. CORRELAZIONI SOLO CON AVERAGE AI
# ============================================================

vars_num <- dati %>%
  select(
    average_ai,
    likes,
    comments,
    sharing,
    engagement_index,
    fatturato,
    numero_dipendenti
  )

mat_cor <- round(cor(vars_num, use = "pairwise.complete.obs"), 2)

corrplot(
  mat_cor,
  method = "color",
  type = "upper",
  diag = FALSE,
  addCoef.col = "black",
  number.cex = 0.7,
  tl.col = "black",
  tl.srt = 45
)


#====================================================
# Correlazione AI Score - Engagement index per brand
#====================================================

cor_brand <- dati %>%
  group_by(brand) %>%
  summarise(
    correlazione = cor(
      average_ai,
      engagement_index,
      use = "complete.obs"
    ),
    n_contenuti = n(),
    .groups = "drop"
  ) %>%
  arrange(correlazione)

print(cor_brand)

#====================================================
# Grafico
#====================================================

ggplot(
  cor_brand,
  aes(
    x = reorder(brand, correlazione),
    y = correlazione,
    fill = brand
  )
) +
  
  geom_col(
    width = 0.68,
    color = "grey25",
    linewidth = 0.3
  ) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey40",
    linewidth = 0.7
  ) +
  
  geom_text(
    aes(label = sprintf("%.2f", correlazione)),
    hjust = ifelse(cor_brand$correlazione >= 0, -0.20, 1.20),
    size = 3.8,
    fontface = "bold"
  ) +
  
  coord_flip() +
  
  scale_fill_manual(
    values = colori_brand
  ) +
  
  scale_y_continuous(
    limits = c(-1, 1),
    breaks = seq(-1, 1, 0.25),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  
  labs(
    title = "Correlazione tra Average AI Score ed Engagement per brand",
    x = "Brand",
    y = "Coefficiente di correlazione (Pearson)",
    caption = "Valori vicini a +1 indicano una forte correlazione positiva; valori vicini a -1 una forte correlazione negativa."
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    legend.position = "none",
    
    plot.title = element_text(
      size = 15,
      face = "bold",
      hjust = 0.5
    ),
    
    plot.subtitle = element_text(
      size = 10.5,
      color = "grey35",
      hjust = 0.5
    ),
    
    plot.caption = element_text(
      size = 9,
      color = "grey40"
    ),
    
    axis.title = element_text(
      face = "bold"
    ),
    
    axis.text = element_text(
      size = 11
    ),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

cor.test(
  dati$average_ai,
  dati$engagement_index,
  method = "spearman",
  exact = FALSE
)
# ============================================================
# 8. TABELLA FINALE PER BRAND
# ============================================================

tabella_brand <- dati %>%
  group_by(brand) %>%
  summarise(
    n_contenuti = n(),
    n_campagne = n_distinct(campagna),
    quota_video = mean(file_type == "video", na.rm = TRUE),
    media_average_ai = mean(average_ai, na.rm = TRUE),
    mediana_average_ai = median(average_ai, na.rm = TRUE),
    media_likes = mean(likes, na.rm = TRUE),
    media_comments = mean(comments, na.rm = TRUE),
    media_engagement = mean(engagement_index, na.rm = TRUE),
    fatturato = max(fatturato, na.rm = TRUE),
    dipendenti = max(numero_dipendenti, na.rm = TRUE)
  ) %>%
  arrange(desc(media_average_ai))

print(tabella_brand)
dev.off()

# =============================================================================
# FONTI E DICHIARAZIONE SULL'USO DI STRUMENTI DI INTELLIGENZA ARTIFICIALE
# =============================================================================

# Fonti:
# Lo script è stato sviluppato facendo riferimento alla documentazione ufficiale
# di R e dei pacchetti utilizzati. Per la costruzione dell'Engagement Index e
# per le analisi statistiche sono stati inoltre utilizzati i riferimenti
# metodologici citati nel testo della tesi e nella relativa bibliografia,
# in particolare quelli relativi alla costruzione di indicatori compositi
# e alla correlazione di rango di Spearman.

# Dichiarazione sull'uso di strumenti di AI:
# Strumenti di intelligenza artificiale generativa sono stati utilizzati come
# supporto alla comprensione, revisione, debugging e riorganizzazione del codice,
# nonché per migliorarne la leggibilità, la documentazione e alcune
# visualizzazioni.
# Le scelte metodologiche, l'adattamento del codice ai dati, l'esecuzione delle
# analisi, il controllo degli output e l'interpretazione dei risultati sono stati
# verificati dall'autrice.
# Gli strumenti di AI non sono stati utilizzati per generare o alterare
# artificialmente i dati analizzati.

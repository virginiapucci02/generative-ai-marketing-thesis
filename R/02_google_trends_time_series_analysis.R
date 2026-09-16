# ============================================================
# ANALISI TIME SERIES PER BRAND
# Trend, stagionalità, ACF, PACF, ADF, pre/post campagna
# ============================================================



# ricorda che se qualche pacchetto necessita di essere installato
# usa la sequente riga di codice:
# install.packages("nome_pacchetto")

library(tidyverse)
library(lubridate)
library(zoo)
library(tseries)
library(forecast)
library(patchwork)

# Crea le cartelle di output se non esistono
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 1. FUNZIONE LETTURA FILE TS
# ============================================================

leggi_ts <- function(file_name) {
  
  dati <- read_csv(file_name, show_col_types = FALSE)
  
  names(dati) <- names(dati) %>%
    str_to_lower() %>%
    str_replace_all(" ", "_")
  
  data_col <- names(dati)[str_detect(names(dati), "date|data|month|mese|time")]
  
  if (length(data_col) == 0) {
    stop(paste("Nessuna colonna data trovata in", file_name))
  }
  
  data_col <- data_col[1]
  
  dati <- dati %>%
    rename(data = all_of(data_col)) %>%
    mutate(
      data = as.Date(data),
      across(where(is.character), ~ str_replace_all(.x, ",", ".")),
      across(where(is.character), ~ suppressWarnings(as.numeric(.x)))
    ) %>%
    arrange(data)
  
  y_col <- dati %>%
    select(-data) %>%
    select(where(is.numeric)) %>%
    names() %>%
    .[1]
  
  dati <- dati %>%
    rename(value = all_of(y_col)) %>%
    mutate(
      mese = month(data),
      anno = year(data),
      time_index = row_number(),
      growth = ifelse(lag(value) == 0 | is.na(lag(value)),NA,
        (value - lag(value)) / lag(value)),
      ma7 = rollmean(value, 7, fill = NA, align = "right"),
      ma30 = rollmean(value, 30, fill = NA, align = "right")
    )
  
  return(dati)
}

# ============================================================
# 2. BRAND, CAMPAGNE E DATE REALI DI LANCIO
# ============================================================

brand_list <- c(
  "COCACOLA",
  "HEINZ",
  "IKEA",
  "LEGO",
  "PATAGONIA",
  "YSL",
  "ZALANDO"
)

campagne_date <- tibble(
  
  brand = brand_list,
  
  # ----------------------------------------------------------
  # PRIMA CAMPAGNA IN ORDINE CRONOLOGICO
  # ----------------------------------------------------------
  
  nome_campagna_1 = c(
    "Share a Coke",
    "Breakfast Ketchup",
    "STOCKHOLM 2025",
    "LEGO Star Wars - Rebuild the Galaxy",
    "Made Without PFAS - PFAS Free",
    "MYSLF L’Absolu",
    "What Do I Wear - SS25"
  ),
  
  campagna_1 = as.Date(c(
    "2025-03-26",   # CocaCola - Share a Coke
    "2025-06-11",   # Heinz - Breakfast Ketchup
    "2025-04-07",   # IKEA - STOCKHOLM 2025
    "2024-05-06",   # LEGO - LEGO Star Wars
    "2025-03-15",   # Patagonia - Made Without PFAS
    "2025-07-31",   # YSL - MYSLF L'Absolu
    "2025-03-21"    # Zalando - What Do I Wear_SS25
  )),
  
  # ----------------------------------------------------------
  # SECONDA CAMPAGNA IN ORDINE CRONOLOGICO
  # ----------------------------------------------------------
  
  nome_campagna_2 = c(
    "Feel It All - FIFA World Cup",
    "The Heinz Dipper",
    "IKEA PS 2026",
    "She Built That",
    "Human Powered - Worn Wear Snow Tour",
    "Libre Berry Crush",
    "Lily Collins"
  ),
  
  campagna_2 = as.Date(c(
    "2026-01-27",   # CocaCola - Feel It All_FIFA World Cup
    "2026-01-13",   # Heinz - The Heinz Dipper
    "2026-05-13",   # IKEA - IKEA PS 2026
    "2025-06-04",   # LEGO - She Built That
    "2026-01-14",   # Patagonia - Human Powered_Worn Wear Snow Tour
    "2026-01-15",   # YSL - Libre Berry Crush
    "2026-03-09"    # Zalando - Lily Collins
  ))
)

print(campagne_date)


# ============================================================
# 3. FUNZIONI PER L'ANALISI DELLE CAMPAGNE
# ============================================================


# ------------------------------------------------------------
# 3.1 FUNZIONI DI SUPPORTO
# ------------------------------------------------------------

media_sicura <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

mediana_sicura <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  median(x, na.rm = TRUE)
}

sd_sicura <- function(x) {
  x <- x[!is.na(x)]
  
  if (length(x) < 2) {
    return(NA_real_)
  }
  
  sd(x)
}


# ============================================================
# 3.2 FUNZIONE ANALISI PRE / POST DI UNA CAMPAGNA
# ============================================================

analizza_finestra_campagna <- function(
    serie,
    brand_name,
    nome_campagna,
    data_lancio,
    finestra_mesi = 3
) {
  
  # ----------------------------------------------------------
  # Google Trends è mensile.
  # Manteniamo la data esatta della campagna,
  # ma per l'analisi usiamo il mese corrispondente.
  # ----------------------------------------------------------
  
  lancio_mese <- floor_date(
    data_lancio,
    unit = "month"
  )
  
  
  # ----------------------------------------------------------
  # Estremi della finestra
  # ----------------------------------------------------------
  
  inizio_pre <- lancio_mese %m-%
    months(finestra_mesi)
  
  fine_post <- lancio_mese %m+%
    months(finestra_mesi)
  
  
  # ----------------------------------------------------------
  # Tre mesi precedenti
  # Il mese del lancio NON è incluso
  # ----------------------------------------------------------
  
  dati_pre <- serie %>%
    filter(
      data >= inizio_pre,
      data < lancio_mese
    )
  
  
  # ----------------------------------------------------------
  # Mese del lancio
  # ----------------------------------------------------------
  
  dati_lancio <- serie %>%
    filter(
      data == lancio_mese
    )
  
  valore_lancio <- if (nrow(dati_lancio) > 0) {
    first(dati_lancio$value)
  } else {
    NA_real_
  }
  
  
  # ----------------------------------------------------------
  # Tre mesi successivi
  # Il mese del lancio NON è incluso
  # ----------------------------------------------------------
  
  dati_post <- serie %>%
    filter(
      data > lancio_mese,
      data <= fine_post
    )
  
  
  # ----------------------------------------------------------
  # Finestra completa:
  # 3 mesi prima + lancio + 3 mesi dopo
  # Serve per individuare il picco vicino alla campagna
  # ----------------------------------------------------------
  
  dati_finestra <- serie %>%
    filter(
      data >= inizio_pre,
      data <= fine_post
    )
  
  
  # ----------------------------------------------------------
  # Individuazione del picco
  # ----------------------------------------------------------
  
  if (
    nrow(dati_finestra) > 0 &&
    any(!is.na(dati_finestra$value))
  ) {
    
    indice_picco <- which.max(
      replace(
        dati_finestra$value,
        is.na(dati_finestra$value),
        -Inf
      )
    )
    
    valore_picco <- dati_finestra$value[indice_picco]
    data_picco <- dati_finestra$data[indice_picco]
    
    distanza_picco_mesi <-
      12 * (
        year(data_picco) -
          year(lancio_mese)
      ) +
      (
        month(data_picco) -
          month(lancio_mese)
      )
    
  } else {
    
    valore_picco <- NA_real_
    data_picco <- as.Date(NA)
    distanza_picco_mesi <- NA_real_
  }
  
  
  # ----------------------------------------------------------
  # Statistiche pre e post
  # ----------------------------------------------------------
  
  media_pre <- media_sicura(
    dati_pre$value
  )
  
  media_post <- media_sicura(
    dati_post$value
  )
  
  
  # ----------------------------------------------------------
  # Tabella finale della campagna
  # ----------------------------------------------------------
  
  tibble(
    
    brand = brand_name,
    
    campagna = nome_campagna,
    
    data_lancio = data_lancio,
    
    mese_lancio = lancio_mese,
    
    
    # Numero di osservazioni disponibili
    
    n_pre = nrow(dati_pre),
    
    n_post = nrow(dati_post),
    
    
    # Periodo precedente
    
    media_pre = media_pre,
    
    mediana_pre =
      mediana_sicura(
        dati_pre$value
      ),
    
    sd_pre =
      sd_sicura(
        dati_pre$value
      ),
    
    
    # Mese del lancio
    
    valore_mese_lancio =
      valore_lancio,
    
    
    # Periodo successivo
    
    media_post =
      media_post,
    
    mediana_post =
      mediana_sicura(
        dati_post$value
      ),
    
    sd_post =
      sd_sicura(
        dati_post$value
      ),
    
    
    # Differenza assoluta
    
    differenza_media =
      ifelse(
        !is.na(media_pre) &
          !is.na(media_post),
        media_post - media_pre,
        NA_real_
      ),
    
    
    # Variazione percentuale
    
    variazione_percentuale =
      ifelse(
        !is.na(media_pre) &
          !is.na(media_post) &
          media_pre > 0,
        
        (
          (media_post - media_pre) /
            media_pre
        ) * 100,
        
        NA_real_
      ),
    
    
    # Picco nella finestra
    
    valore_picco =
      valore_picco,
    
    data_picco =
      data_picco,
    
    distanza_picco_mesi =
      distanza_picco_mesi,
    
    
    # TRUE solamente se abbiamo realmente
    # tutti i 3 mesi precedenti e successivi
    
    finestra_completa =
      n_distinct(dati_pre$data) ==
      finestra_mesi &
      n_distinct(dati_post$data) ==
      finestra_mesi
  )
}


# ============================================================
# 3.3 GRAFICO DELLA SERIE SPECIFICA DI UNA CAMPAGNA
# ============================================================

grafico_campagna_specifica <- function(
    serie,
    brand_name,
    nome_campagna,
    data_lancio
) {
  
  lancio_mese <- floor_date(
    data_lancio,
    unit = "month"
  )
  
  
  ggplot(
    serie,
    aes(
      x = data,
      y = value
    )
  ) +
    
    geom_line(
      linewidth = 1,
      color = "#0072B2"
    ) +
    
    geom_point(
      size = 2,
      color = "#0072B2"
    ) +
    
    geom_vline(
      xintercept =
        as.numeric(lancio_mese),
      linetype = "dashed",
      linewidth = 0.9,
      color = "#D55E00"
    ) +
    
    annotate(
      "text",
      x = lancio_mese,
      y = Inf,
      label = paste0(
        "Lancio: ",
        format(
          data_lancio,
          "%d/%m/%Y"
        )
      ),
      angle = 90,
      vjust = 1.4,
      hjust = 1,
      size = 3.2
    ) +
    
    scale_x_date(
      date_breaks = "3 months",
      date_labels = "%b\n%Y"
    ) +
    
    coord_cartesian(
      ylim = c(0, 100)
    ) +
    
    labs(
      title = paste(
        brand_name,
        "-",
        nome_campagna
      ),
      
      subtitle =
        "Google Trends della ricerca specifica della campagna",
      
      x = "Data",
      
      y = "Indice Google Trends (0–100)"
    ) +
    
    theme_bw(
      base_size = 13
    ) +
    
    theme(
      
      plot.title =
        element_text(
          face = "bold"
        ),
      
      panel.grid.minor =
        element_blank(),
      
      axis.text.x =
        element_text(
          size = 9
        )
    )
}


# ============================================================
# 3.4 FUNZIONE ANALISI COMPLETA PER BRAND
# ============================================================

analisi_ts_brand <- function(brand_name) {
  
  
  # ----------------------------------------------------------
  # A. NOMI DEI TRE FILE
  # ----------------------------------------------------------
  file_totale <- paste0(
    "data/google_trends/ts_",
    brand_name,
    ".csv"
  )
  
  file_campagna_1 <- paste0(
    "data/google_trends/ts_",
    brand_name,
    "_c1.csv"
  )
  
  file_campagna_2 <- paste0(
    "data/google_trends/ts_",
    brand_name,
    "_c2.csv"
  )
  # ----------------------------------------------------------
  # Controllo esistenza file
  # ----------------------------------------------------------
  
  file_richiesti <- c(
    file_totale,
    file_campagna_1,
    file_campagna_2
  )
  
  file_mancanti <- file_richiesti[
    !file.exists(file_richiesti)
  ]
  
  if (length(file_mancanti) > 0) {
    
    stop(
      paste(
        "File mancanti per",
        brand_name,
        ":",
        paste(
          file_mancanti,
          collapse = ", "
        )
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # B. LETTURA DELLE TRE SERIE
  # ----------------------------------------------------------
  
  # Serie generale del brand
  
  ts_brand <- leggi_ts(
    file_totale
  )
  
  
  # Serie specifica della campagna 1
  
  ts_campagna_1 <- leggi_ts(
    file_campagna_1
  )
  
  
  # Serie specifica della campagna 2
  
  ts_campagna_2 <- leggi_ts(
    file_campagna_2
  )
  
  
  # ----------------------------------------------------------
  # C. MEDIA MOBILE TRIMESTRALE
  #
  # La funzione leggi_ts() sopra contiene ancora ma7 e ma30.
  # Qui costruiamo correttamente ma3 perché i dati sono mensili.
  # ----------------------------------------------------------
  
  ts_brand <- ts_brand %>%
    mutate(
      ma3 = rollmean(
        value,
        3,
        fill = NA,
        align = "right"
      )
    )
  
  
  # ----------------------------------------------------------
  # D. DATE DELLE DUE CAMPAGNE
  # ----------------------------------------------------------
  
  date_brand <- campagne_date %>%
    filter(
      brand == brand_name
    )
  
  if (nrow(date_brand) != 1) {
    
    stop(
      paste(
        "Errore nelle date delle campagne per",
        brand_name
      )
    )
  }
  
  
  # Conversione nel mese di lancio
  
  lancio_1 <- floor_date(
    date_brand$campagna_1,
    unit = "month"
  )
  
  lancio_2 <- floor_date(
    date_brand$campagna_2,
    unit = "month"
  )
  
  
  # ----------------------------------------------------------
  # E. CONTROLLO COPERTURA DELLA SERIE GENERALE
  # ----------------------------------------------------------
  
  if (
    lancio_1 < min(ts_brand$data, na.rm = TRUE) ||
    lancio_1 > max(ts_brand$data, na.rm = TRUE)
  ) {
    
    warning(
      paste(
        "Campagna 1 fuori dalla serie Google Trends:",
        brand_name
      )
    )
  }
  
  if (
    lancio_2 < min(ts_brand$data, na.rm = TRUE) ||
    lancio_2 > max(ts_brand$data, na.rm = TRUE)
  ) {
    
    warning(
      paste(
        "Campagna 2 fuori dalla serie Google Trends:",
        brand_name
      )
    )
  }
  
  
  # ==========================================================
  # F. STATISTICHE DESCRITTIVE GENERALI
  # ==========================================================
  
  date_attese <- seq(
    from = min(
      ts_brand$data,
      na.rm = TRUE
    ),
    to = max(
      ts_brand$data,
      na.rm = TRUE
    ),
    by = "month"
  )
  
  
  serie_mensile_regolare <-
    nrow(ts_brand) ==
    length(date_attese) &&
    all(
      ts_brand$data ==
        date_attese
    )
  
  
  descrittive <- ts_brand %>%
    summarise(
      
      brand =
        brand_name,
      
      n =
        n(),
      
      data_inizio =
        min(
          data,
          na.rm = TRUE
        ),
      
      data_fine =
        max(
          data,
          na.rm = TRUE
        ),
      
      media =
        mean(
          value,
          na.rm = TRUE
        ),
      
      mediana =
        median(
          value,
          na.rm = TRUE
        ),
      
      sd =
        sd(
          value,
          na.rm = TRUE
        ),
      
      min =
        min(
          value,
          na.rm = TRUE
        ),
      
      max =
        max(
          value,
          na.rm = TRUE
        ),
      
      coeff_variazione =
        sd(
          value,
          na.rm = TRUE
        ) /
        mean(
          value,
          na.rm = TRUE
        ),
      
      serie_mensile_regolare =
        serie_mensile_regolare
    )
  
  print(
    descrittive
  )
  
  
  # ==========================================================
  # G. GRAFICO TREND GENERALE + MEDIA MOBILE 3 MESI
  # ==========================================================
  
  plot_data <- ts_brand %>%
    select(
      data,
      value,
      ma3
    ) %>%
    pivot_longer(
      cols = c(
        value,
        ma3
      ),
      names_to = "serie",
      values_to = "valore"
    ) %>%
    mutate(
      serie = recode(
        serie,
        value =
          "Serie originale",
        ma3 =
          "Media mobile (3 mesi)"
      )
    )
  
  
  p_trend <- ggplot(
    plot_data,
    aes(
      x = data,
      y = valore,
      colour = serie
    )
  ) +
    
    geom_line(
      linewidth = 1,
      na.rm = TRUE
    ) +
    
    # Trend lineare complessivo
    
    geom_smooth(
      data = ts_brand,
      aes(
        x = data,
        y = value
      ),
      inherit.aes = FALSE,
      method = "lm",
      se = FALSE,
      colour = "black",
      linewidth = 1,
      linetype = "dashed"
    ) +
    
    # Prima campagna
    
    geom_vline(
      xintercept =
        as.numeric(lancio_1),
      linetype = 2,
      linewidth = 0.8
    ) +
    
    # Seconda campagna
    
    geom_vline(
      xintercept =
        as.numeric(lancio_2),
      linetype = 2,
      linewidth = 0.8
    ) +
    
    # Nome campagna 1
    
    annotate(
      "text",
      x = lancio_1,
      y = Inf,
      label =
        date_brand$nome_campagna_1,
      angle = 90,
      vjust = 1.3,
      hjust = 1,
      size = 3.1
    ) +
    
    # Nome campagna 2
    
    annotate(
      "text",
      x = lancio_2,
      y = Inf,
      label =
        date_brand$nome_campagna_2,
      angle = 90,
      vjust = 1.3,
      hjust = 1,
      size = 3.1
    ) +
    
    scale_colour_manual(
      values = c(
        "Serie originale" =
          "#2C7FB8",
        "Media mobile (3 mesi)" =
          "#D95F02"
      )
    ) +
    
    scale_x_date(
      date_breaks =
        "6 months",
      date_labels =
        "%b\n%Y"
    ) +
    
    coord_cartesian(
      ylim = c(
        0,
        100
      )
    ) +
    
    labs(
      title = paste(
        "Serie storica Google Trends -",
        brand_name
      ),
      
      subtitle =
        "Linee verticali = mesi di lancio delle campagne | Linea nera tratteggiata = trend lineare",
      
      x = "Data",
      
      y = "Indice Google Trends (0–100)",
      
      colour = NULL
    ) +
    
    theme_bw(
      base_size = 14
    ) +
    
    theme(
      
      legend.position =
        "bottom",
      
      legend.title =
        element_blank(),
      
      plot.title =
        element_text(
          face = "bold"
        ),
      
      panel.grid.minor =
        element_blank()
    )
  
  print(
    p_trend
  )
  
  
  # ==========================================================
  # H. REGRESSIONE DEL TREND GENERALE
  # ==========================================================
  
  mod_trend <- lm(
    value ~ time_index,
    data = ts_brand
  )
  
  print(
    summary(
      mod_trend
    )
  )
  
  
  # ==========================================================
  # I. ANALISI PRE / LANCIO / POST DELLE DUE CAMPAGNE
  # ==========================================================
  
  risultato_campagna_1 <-
    analizza_finestra_campagna(
      
      serie =
        ts_brand,
      
      brand_name =
        brand_name,
      
      nome_campagna =
        date_brand$nome_campagna_1,
      
      data_lancio =
        date_brand$campagna_1,
      
      finestra_mesi =
        3
    )
  
  
  risultato_campagna_2 <-
    analizza_finestra_campagna(
      
      serie =
        ts_brand,
      
      brand_name =
        brand_name,
      
      nome_campagna =
        date_brand$nome_campagna_2,
      
      data_lancio =
        date_brand$campagna_2,
      
      finestra_mesi =
        3
    )
  
  
  tab_periodi <- bind_rows(
    risultato_campagna_1,
    risultato_campagna_2
  )
  
  print(
    tab_periodi
  )
  
  
  # ----------------------------------------------------------
  # Segnalazione automatica delle finestre incomplete
  # ----------------------------------------------------------
  
  if (
    any(
      !tab_periodi$finestra_completa
    )
  ) {
    
    cat(
      "\nATTENZIONE:",
      brand_name,
      "presenta almeno una campagna senza 3 mesi completi prima e dopo.\n"
    )
    
    print(
      tab_periodi %>%
        filter(
          !finestra_completa
        ) %>%
        select(
          brand,
          campagna,
          data_lancio,
          n_pre,
          n_post,
          finestra_completa
        )
    )
  }
  
  
  # ==========================================================
  # L. GRAFICO PRE / MESE DEL LANCIO / POST
  # ==========================================================
  
  dati_periodi_plot <- tab_periodi %>%
    select(
      campagna,
      mediana_pre,
      valore_mese_lancio,
      mediana_post
    ) %>%
    pivot_longer(
      cols = c(
        mediana_pre,
        valore_mese_lancio,
        mediana_post
      ),
      names_to =
        "periodo",
      values_to =
        "valore"
    ) %>%
    mutate(
      
      periodo = recode(
        
        periodo,
        
        mediana_pre =
          "3 mesi precedenti",
        
        valore_mese_lancio =
          "Mese del lancio",
        
        mediana_post =
          "3 mesi successivi"
      ),
      
      periodo = factor(
        periodo,
        levels = c(
          "3 mesi precedenti",
          "Mese del lancio",
          "3 mesi successivi"
        )
      )
    )
  
  
  p_periodi <- ggplot(
    dati_periodi_plot,
    aes(
      x = campagna,
      y = valore,
      fill = periodo
    )
  ) +
    
    geom_col(
      position =
        position_dodge(
          width = 0.8
        ),
      width = 0.7
    ) +
    
    geom_text(
      aes(
        label = round(
          valore,
          1
        )
      ),
      position =
        position_dodge(
          width = 0.8
        ),
      vjust = -0.35,
      size = 3.4
    ) +
    
    scale_fill_manual(
      values = c(
        "3 mesi precedenti" =
          "#0072B2",
        "Mese del lancio" =
          "#E69F00",
        "3 mesi successivi" =
          "#009E73"
      )
    ) +
    
    coord_cartesian(
      ylim = c(
        0,
        105
      )
    ) +
    
    labs(
      
      title = paste(
        "Google Trends prima e dopo le campagne -",
        brand_name
      ),
      
      subtitle =
        "Mediana nei tre mesi precedenti e successivi; il mese del lancio è riportato separatamente",
      
      x = NULL,
      
      y = "Indice Google Trends (0–100)",
      
      fill = NULL,
      
      caption =
        "Le eventuali finestre temporali incomplete sono segnalate nella tabella dei risultati."
    ) +
    
    theme_bw(
      base_size = 13
    ) +
    
    theme(
      
      legend.position =
        "bottom",
      
      axis.text.x =
        element_text(
          angle = 15,
          hjust = 1
        ),
      
      plot.title =
        element_text(
          face = "bold"
        ),
      
      panel.grid.minor =
        element_blank()
    )
  
  print(
    p_periodi
  )
  
  
  # ==========================================================
  # M. STAGIONALITÀ MENSILE
  # ==========================================================
  
  p_mese <- ggplot(
    ts_brand,
    aes(
      x = factor(
        mese,
        levels = 1:12
      ),
      y = value
    )
  ) +
    
    geom_boxplot(
      fill = "#56B4E9",
      color = "grey20"
    ) +
    
    scale_x_discrete(
      labels = month.abb
    ) +
    
    labs(
      
      title = paste(
        "Stagionalità mensile -",
        brand_name
      ),
      
      subtitle =
        "Distribuzione dell'interesse Google Trends nei diversi mesi dell'anno",
      
      x = "Mese",
      
      y = "Indice Google Trends (0–100)"
    ) +
    
    theme_bw(
      base_size = 14
    ) +
    
    theme(
      
      plot.title =
        element_text(
          face = "bold"
        ),
      
      panel.grid.minor =
        element_blank()
    )
  
  print(
    p_mese
  )
  
  
  # ==========================================================
  # N. DECOMPOSIZIONE STL
  # Frequenza = 12 perché i dati sono mensili
  # ==========================================================
  
  p_stl <- NULL
  
  if (
    nrow(ts_brand) >= 24 &&
    serie_mensile_regolare &&
    !anyNA(ts_brand$value)
  ) {
    
    ts_object <- ts(
      
      ts_brand$value,
      
      start = c(
        year(
          min(ts_brand$data)
        ),
        month(
          min(ts_brand$data)
        )
      ),
      
      frequency = 12
    )
    
    
    decomp <- stl(
      ts_object,
      s.window = "periodic"
    )
    
    
    p_stl <- autoplot(
      decomp
    ) +
      
      labs(
        title = paste(
          "Decomposizione STL -",
          brand_name
        )
      )
    
    
    print(
      p_stl
    )
    
  } else {
    
    cat(
      "\nSTL non eseguita per",
      brand_name,
      ": serie troppo corta, irregolare o contenente valori mancanti.\n"
    )
  }
  
  
  # ==========================================================
  # O. ACF E PACF
  # Ogni lag corrisponde a UN MESE
  # ==========================================================
  
  value_clean <- na.omit(
    ts_brand$value
  )
  
  p_acf <- NULL
  p_pacf <- NULL
  
  
  if (
    length(
      value_clean
    ) > 10
  ) {
    
    lag_massimo <- min(
      12,
      length(
        value_clean
      ) - 1
    )
    
    
    p_acf <- ggAcf(
      value_clean,
      lag.max =
        lag_massimo
    ) +
      
      theme_bw(
        base_size = 14
      ) +
      
      labs(
        
        title = paste(
          "ACF mensile -",
          brand_name
        ),
        
        subtitle =
          "Autocorrelazione dell'interesse Google Trends",
        
        x =
          "Lag (mesi)",
        
        y =
          "Autocorrelazione"
      )
    
    
    print(
      p_acf
    )
    
    
    p_pacf <- ggPacf(
      value_clean,
      lag.max =
        lag_massimo
    ) +
      
      theme_bw(
        base_size = 14
      ) +
      
      labs(
        
        title = paste(
          "PACF mensile -",
          brand_name
        ),
        
        subtitle =
          "Autocorrelazione parziale dell'interesse Google Trends",
        
        x =
          "Lag (mesi)",
        
        y =
          "Autocorrelazione parziale"
      )
    
    
    print(
      p_pacf
    )
  }
  
  
  # ==========================================================
  # P. TEST DI STAZIONARIETÀ ADF
  # ==========================================================
  
  adf_result <- NULL
  
  if (
    length(
      value_clean
    ) > 10
  ) {
    
    adf_result <- adf.test(
      value_clean
    )
    
    print(
      adf_result
    )
  }
  
  
  # ==========================================================
  # Q. SERIE GOOGLE TRENDS SPECIFICHE DELLE DUE CAMPAGNE
  # ==========================================================
  
  p_campagna_1 <-
    grafico_campagna_specifica(
      
      serie =
        ts_campagna_1,
      
      brand_name =
        brand_name,
      
      nome_campagna =
        date_brand$nome_campagna_1,
      
      data_lancio =
        date_brand$campagna_1
    )
  
  
  p_campagna_2 <-
    grafico_campagna_specifica(
      
      serie =
        ts_campagna_2,
      
      brand_name =
        brand_name,
      
      nome_campagna =
        date_brand$nome_campagna_2,
      
      data_lancio =
        date_brand$campagna_2
    )
  
  
  print(
    p_campagna_1
  )
  
  print(
    p_campagna_2
  )
  
  
  # ----------------------------------------------------------
  # Grafico combinato delle due campagne
  #
  # ATTENZIONE:
  # i due file Google Trends sono normalizzati separatamente.
  # Non bisogna confrontare direttamente l'altezza dei valori
  # di Campagna 1 con quella di Campagna 2.
  # ----------------------------------------------------------
  
  p_campagne_specifiche <-
    p_campagna_1 +
    p_campagna_2 +
    
    plot_annotation(
      
      title = paste(
        "Ricerche specifiche delle campagne -",
        brand_name
      ),
      
      caption =
        "Nota: ciascuna serie Google Trends è normalizzata separatamente su scala 0–100. I valori assoluti delle due campagne non sono direttamente confrontabili."
    )
  
  
  print(
    p_campagne_specifiche
  )
  
  
  # ==========================================================
  # R. SALVATAGGIO DEI GRAFICI
  # ==========================================================
  
  ggsave(
    paste0(
      "output/figures/trend_ts_",
      brand_name,
      ".pdf"
    ),
    plot =
      p_trend,
    width =
      14,
    height =
      8
  )
  
  
  ggsave(
    paste0(
      "output/figures/periodi_ts_",
      brand_name,
      ".pdf"
    ),
    plot =
      p_periodi,
    width =
      12,
    height =
      7
  )
  
  
  ggsave(
    paste0(
      "output/figures/stagionalita_ts_",
      brand_name,
      ".pdf"
    ),
    plot =
      p_mese,
    width =
      10,
    height =
      6
  )
  
  
  ggsave(
    paste0(
      "output/figures/campagne_specifiche_",
      brand_name,
      ".pdf"
    ),
    plot =
      p_campagne_specifiche,
    width =
      16,
    height =
      7
  )
  
  
  if (
    !is.null(
      p_acf
    )
  ) {
    
    ggsave(
      paste0(
        "output/figures/acf_ts_",
        brand_name,
        ".pdf"
      ),
      plot =
        p_acf,
      width =
        9,
      height =
        6
    )
  }
  
  
  if (
    !is.null(
      p_pacf
    )
  ) {
    
    ggsave(
      paste0(
        "output/figures/pacf_ts_",
        brand_name,
        ".pdf"
      ),
      plot =
        p_pacf,
      width =
        9,
      height =
        6
    )
  }
  
  
  if (
    !is.null(
      p_stl
    )
  ) {
    
    ggsave(
      paste0(
        "output/figures/stl_ts_",
        brand_name,
        ".pdf"
      ),
      plot =
        p_stl,
      width =
        11,
      height =
        8
    )
  }
  
  
  # ==========================================================
  # S. OUTPUT DELLA FUNZIONE
  # ==========================================================
  
  return(
    
    list(
      
      descrittive =
        descrittive,
      
      periodi =
        tab_periodi,
      
      modello_trend =
        mod_trend,
      
      adf =
        adf_result
    )
  )
}


# ============================================================
# 4. ESEGUI ANALISI PER I BRAND
# ============================================================

# Per eseguire l'analisi su tutti i brand:
brand_da_analizzare <- brand_list

# Se vuoi prima fare una prova soltanto su Zalando,
# commenta la riga precedente e usa:
# brand_da_analizzare <- c("ZALANDO")


risultati_ts <- list()


for (
  b in brand_da_analizzare
) {
  
  cat(
    "\n\n====================================================\n"
  )
  
  cat(
    "ANALISI TIME SERIES:",
    b,
    "\n"
  )
  
  cat(
    "====================================================\n\n"
  )
  
  
  risultati_ts[[b]] <-
    analisi_ts_brand(
      b
    )
}


# ============================================================
# 5. TABELLA RIASSUNTIVA DELLE SERIE STORICHE
# ============================================================

tabella_descrittive_finale <- map_dfr(
  risultati_ts,
  ~ .x$descrittive
)

print(
  tabella_descrittive_finale
)


write_csv(
  tabella_descrittive_finale,
  "output/tables/tabella_descrittive_time_series_brand.csv"
)


# ============================================================
# 6. TABELLA PRE / LANCIO / POST
# TUTTE LE 14 CAMPAGNE
# ============================================================

tabella_periodi_finale <- map2_dfr(
  
  risultati_ts,
  
  names(
    risultati_ts
  ),
  
  ~ .x$periodi
)


# Ordine più leggibile

tabella_periodi_finale <-
  tabella_periodi_finale %>%
  
  arrange(
    brand,
    data_lancio
  )


print(
  tabella_periodi_finale,
  n = Inf
)


write_csv(
  tabella_periodi_finale,
  "output/tables/tabella_pre_post_campagne_brand.csv"
)


# ============================================================
# 7. TABELLA SINTETICA PER LA TESI
# ============================================================

tabella_campagne_sintetica <-
  tabella_periodi_finale %>%
  
  select(
    
    brand,
    
    campagna,
    
    data_lancio,
    
    n_pre,
    
    media_pre,
    
    mediana_pre,
    
    valore_mese_lancio,
    
    n_post,
    
    media_post,
    
    mediana_post,
    
    differenza_media,
    
    variazione_percentuale,
    
    valore_picco,
    
    data_picco,
    
    distanza_picco_mesi,
    
    finestra_completa
  )


print(
  tabella_campagne_sintetica,
  n = Inf
)


write_csv(
  tabella_campagne_sintetica,
  "output/tables/tabella_sintetica_campagne_google_trends.csv"
)


# ============================================================
# 8. CONTROLLO DELLE CAMPAGNE CON FINESTRA INCOMPLETA
# ============================================================

campagne_finestra_incompleta <-
  tabella_periodi_finale %>%
  
  filter(
    !finestra_completa
  ) %>%
  
  select(
    brand,
    campagna,
    data_lancio,
    n_pre,
    n_post
  )


print(
  campagne_finestra_incompleta,
  n = Inf
)

############
tabella_periodi_finale %>%
  select(
    brand,
    campagna,
    mediana_pre,
    valore_mese_lancio,
    mediana_post,
    differenza_media,
    variazione_percentuale,
    valore_picco,
    data_picco,
    distanza_picco_mesi,
    finestra_completa
  ) %>%
  print(n = Inf, width = Inf)

# ============================================================
# SALVA TUTTI I GRAFICI IN UN UNICO PDF
# ============================================================

pdf(
  file = "output/figures/tutti_i_grafici_time_series.pdf",
  width = 12,
  height = 8,
  onefile = TRUE
)
for (b in brand_list) {
  analisi_ts_brand(b)
}

dev.off()

# =============================================================================
# FONTI E DICHIARAZIONE SULL'USO DI STRUMENTI DI INTELLIGENZA ARTIFICIALE
# =============================================================================

# Fonti:
# Lo script è stato sviluppato facendo riferimento alla documentazione ufficiale
# di R e dei pacchetti utilizzati. Per le analisi delle serie temporali,
# incluse media mobile, decomposizione STL, ACF, PACF e test ADF, sono stati
# inoltre utilizzati i riferimenti metodologici citati nel testo della tesi
# e nella relativa bibliografia, in particolare Hyndman e Athanasopoulos
# per l'analisi e l'interpretazione delle serie temporali.

# Dichiarazione sull'uso di strumenti di AI:
# Strumenti di intelligenza artificiale generativa sono stati utilizzati come
# supporto alla comprensione, revisione, debugging e riorganizzazione del codice,
# nonché per migliorarne la leggibilità, la documentazione e alcune
# visualizzazioni.
# Le scelte metodologiche, la definizione delle finestre temporali,
# l'adattamento del codice ai dati Google Trends, l'esecuzione delle analisi,
# il controllo degli output e l'interpretazione dei risultati sono stati
# verificati dall'autrice.
# Gli strumenti di AI non sono stati utilizzati per generare o alterare
# artificialmente i dati analizzati.

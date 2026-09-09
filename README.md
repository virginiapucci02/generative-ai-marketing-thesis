# Generative AI nelle campagne di marketing

Repository associato alla tesi magistrale:

**“Generative AI nelle campagne di marketing: dalla produzione visuale alla risposta digitale del consumatore”**

Il repository contiene il codice R e i dati utilizzati per la riproduzione delle principali analisi empiriche sviluppate nella tesi.

## Contenuto del repository

La cartella `R/` contiene i due script utilizzati per le analisi:

- `01_campaign_ai_engagement_analysis.R`  
  Analizza il dataset dei contenuti delle campagne, la distribuzione dell'Average AI Score a livello di brand e campagna, la concordanza tra i detector applicati alle immagini e le metriche di engagement. Lo script costruisce inoltre l'Engagement Index e analizza la relazione tra quest'ultimo e l'Average AI Score.

- `02_google_trends_time_series_analysis.R`  
  Analizza le serie temporali Google Trends relative ai sette brand e alle quattordici campagne considerate nella ricerca. Le elaborazioni comprendono statistiche descrittive, trend temporali, media mobile, stagionalità, decomposizione STL, ACF, PACF, test Augmented Dickey-Fuller e confronto tra i periodi precedente, di lancio e successivo alle campagne.

La cartella `data/` contiene:

- `campaign_dataset.xlsx`: dataset utilizzato per le analisi dei contenuti, degli AI score e dell'engagement;
- `google_trends/`: serie Google Trends relative ai brand e alle singole campagne.

## Struttura

```text
generative-ai-marketing-thesis/
├── R/
│   ├── 01_campaign_ai_engagement_analysis.R
│   └── 02_google_trends_time_series_analysis.R
├── data/
│   ├── campaign_dataset.xlsx
│   └── google_trends/
│       ├── ts_COCACOLA.csv
│       ├── ts_COCACOLA_c1.csv
│       ├── ts_COCACOLA_c2.csv
│       └── ...
├── .gitignore
├── LICENSE
└── README.md

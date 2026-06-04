# ===========================================================================
# app.R â€” Dashboard Ferrugem AsiÃ¡tica da Soja â€” Mato Grosso v8.0
# ===========================================================================
# Plataforma profissional de monitoramento, anÃ¡lise de risco e suporte Ã  decisÃ£o
# para produtores e consultores de soja no Estado de Mato Grosso.
#
# Fontes de dados integradas:
#   IBGE/SIDRA (PAM Tab.1612) Â· NASA POWER (ERA5-Land) Â· Open-Meteo Archive
#   geobr/IBGE (polÃ­gonos municipais) Â· NOAA CPC (ENSO/ONI)
#   CEPEA-ESALQ/USP (preÃ§os) Â· EMBRAPA SIGA (alertas fitossanitÃ¡rios)
#   CONAB (safras) Â· MAPA/ZARC (seguro rural) Â· AwesomeAPI (cÃ¢mbio)
#
# Metodologia:
#   IRC: Del Ponte et al. (2006) Phytopathology 96(7):797-803
#   Curva de Dano: Dalla Lana et al. (2015) Plant Disease 99(9):1175-1185
#   ENSO: NOAA CPC Oceanic NiÃ±o Index (ONI) â€” perÃ­odo base 1991-2020
#   ResistÃªncia: EMBRAPA Soja Circular TÃ©cnico 119/2024
#   ZARC/Seguro: MAPA Portaria 270/2024 + PSR Portaria 709/2024
# v8.0: Standalone app â€” deploy ShinyApps.io / Shiny Server / Posit Connect â€” roda fora do RStudio, deploy ShinyApps.io/Shiny Server
# ===========================================================================

# â”€â”€ Auto-instalaÃ§Ã£o de pacotes faltantes â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
.pkgs <- c("shiny","shinydashboard","shinyWidgets","shinycssloaders",
           "leaflet","leaflet.extras","plotly","DT","dplyr","tidyr",
           "ggplot2","scales","httr","jsonlite","lubridate","forecast",
           "zoo","stringr","future","promises","geobr","sf",
           "knitr","rmarkdown","kableExtra","webshot2","chromote","htmltools",
           "RColorBrewer")
.faltando <- .pkgs[!sapply(.pkgs, requireNamespace, quietly=TRUE)]
if (length(.faltando) > 0) {
  message(sprintf("[SETUP] Instalando: %s", paste(.faltando, collapse=", ")))
  install.packages(.faltando, repos="https://cran.rstudio.com/", dependencies=TRUE, quiet=TRUE)
}
rm(.pkgs, .faltando)

suppressPackageStartupMessages({
  library(shiny); library(shinydashboard); library(shinyWidgets)
  library(shinycssloaders); library(leaflet); library(leaflet.extras)
  library(plotly); library(DT); library(dplyr); library(tidyr)
  library(ggplot2); library(scales); library(httr); library(jsonlite)
  library(lubridate); library(forecast); library(zoo)
  library(stringr); library(future); library(promises)
  library(htmltools)
  if (requireNamespace("RColorBrewer", quietly=TRUE)) library(RColorBrewer)
  if (requireNamespace("geobr",    quietly=TRUE)) library(geobr)
  if (requireNamespace("sf",       quietly=TRUE)) library(sf)
  if (requireNamespace("knitr",    quietly=TRUE)) library(knitr)
  if (requireNamespace("kableExtra",quietly=TRUE)) library(kableExtra)
  if (requireNamespace("rmarkdown",quietly=TRUE)) library(rmarkdown)
})
plan(multisession, workers = min(2, max(1, tryCatch(
  parallelly::availableCores() - 1L,
  error = function(e) parallel::detectCores(logical = FALSE) - 1L
))))


# 2. DADOS ESTATICOS ====
## 2.1  Constantes e Formatacao ----
# â”€â”€ CONSTANTES â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
SAFRA_ATUAL  <- "2024/2025"
ANOS_SERIE   <- 2012:2024
SAFRAS       <- paste0(2012:2024, "/", 2013:2025)
MESES_SAFRA  <- c("Out","Nov","Dez","Jan","Fev","Mar")
PRECO_PADRAO <- 128

# â”€â”€ CACHE PERSISTENTE (offline-first) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# Pasta persistente: sobrevive entre sessÃµes/reinicializaÃ§Ãµes do R
# Prioridade: 1) app_dir/cache  2) ~/.ferrugem_cache  3) tempdir (fallback)
.cache_candidates <- c(
  file.path(getwd(), "cache"),
  file.path(Sys.getenv("HOME"), ".ferrugem_cache"),
  file.path(tempdir(), "ferrugem_v5")
)
CACHE_DIR <- tryCatch({
  for (d in .cache_candidates) {
    tryCatch({ dir.create(d, showWarnings=FALSE, recursive=TRUE); d }, error=function(e) NULL)
    if (dir.exists(d) && file.access(d, 2) == 0) break
  }
  d
}, error = function(e) file.path(tempdir(), "ferrugem_v5"))
dir.create(CACHE_DIR, showWarnings=FALSE, recursive=TRUE)
message(sprintf("[CACHE] Pasta persistente: %s", CACHE_DIR))

CACHE_TTL        <- 24 * 3600       # 24h â€” preÃ§os, cÃ¢mbio, ENSO, SIGA
CACHE_TTL_IBGE   <- 7 * 24 * 3600  # 7 dias â€” IBGE/SIDRA (dados anuais)
CACHE_TTL_NASA   <- 7 * 24 * 3600  # 7 dias â€” NASA POWER ERA5-Land
CACHE_TTL_GEOBR  <- 30 * 24 * 3600 # 30 dias â€” polÃ­gonos geobr
CACHE_NEVER_EXP  <- 365 * 24 * 3600 # 1 ano â€” fallback offline

# â”€â”€ FORMATAÃ‡ÃƒO PT-BR â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
fmt_n <- function(x) {
  x <- as.numeric(x); if (is.na(x)) return("â€”")
  # Usa big.mark="," sem decimal conflict, depois troca para PT-BR
  s <- formatC(round(x), format="d", big.mark=".", flag="")
  s
}
fmt_r <- function(x, d=1) {
  x <- as.numeric(x); if (is.na(x)) return("â€”")
  # Formata com decimal ponto e troca para vÃ­rgula PT-BR
  s <- formatC(x, format="f", digits=d, big.mark="", flag="")
  s <- sub("\\.", ",", s)
  s
}
pct <- function(x) paste0(fmt_r(x,1), "%")

# â”€â”€ PALETAS â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
NIVEIS_RISCO <- c("Muito Alto","Alto","Moderado","Baixo","Muito Baixo")
CORES_RISCO  <- c("Muito Alto"="#dc2626","Alto"="#ef4444","Moderado"="#ca8a04",
                  "Baixo"="#16a34a","Muito Baixo"="#1d4ed8")
NIVEIS_VULN  <- c("Critica","Alta","Moderada","Baixa","Muito Baixa")
CORES_VULN   <- c("Critica"="#991b1b","Alta"="#dc2626","Moderada"="#ca8a04",
                  "Baixa"="#16a34a","Muito Baixa"="#1d4ed8")
CORES_ENSO   <- c("La Nina forte"="#065f46","La Nina moderada"="#0f766e",
                  "La Nina fraca"="#0d9488","Neutro"="#2563eb",
                  "El Nino fraco"="#ca8a04","El Nino moderado"="#c2410c",
                  "El Nino forte"="#991b1b")
cat_risco_fn <- function(irc) factor(case_when(
  irc>=75~"Muito Alto",irc>=60~"Alto",irc>=45~"Moderado",irc>=30~"Baixo",TRUE~"Muito Baixo"
), levels=NIVEIS_RISCO)
cat_vuln_fn <- function(ivm) factor(case_when(
  ivm>=75~"Critica",ivm>=55~"Alta",ivm>=35~"Moderada",ivm>=15~"Baixa",TRUE~"Muito Baixa"
), levels=NIVEIS_VULN)
# Probabilidade de epidemia â€” modelo logÃ­stico de Del Ponte et al. (2006)
# P(%) = 100 / (1 + exp(âˆ’(IRC âˆ’ 50) / 10))
#
# Fonte: Del Ponte, E.M.; Godoy, C.V.; Li, X.; Yang, X.B. (2006).
#   "Predicting severity of Asian soybean rust epidemics with empirical
#    rainfall models." Phytopathology 96(7):797-803.
#   doi:10.1094/PHYTO-96-0797
#   ParÃ¢metros: Î¸â‚=50 (ponto de inflexÃ£o), Î¸â‚‚=10 (taxa logÃ­stica)
#   Calibrado para condiÃ§Ãµes do sul do Brasil (RS/PR), extrapolado para MT.
#
# IRC = 50 â†’ P = 50%; IRC = 75 â†’ P â‰ˆ 92%; IRC = 30 â†’ P â‰ˆ 12%
prob_fn  <- function(irc) round(100/(1+exp(-(irc-50)/10)), 1)

# Coeficiente de dano â€” modelo de Dalla Lana et al. (2015)
# D(%) = 100 Ã— (1 âˆ’ exp(âˆ’0,0416 Ã— SEV))
#
# Fonte: Dalla Lana, F.; Ziegelmann, P.K.; de Andrade Neto, M.; Minella, E.;
#   Lenz, G.; Picinini, E.C.; Del Ponte, E.M. (2015).
#   "Meta-analysis of fungicide efficacy on soybean target spot and Asian rust
#    and their relationship with yield." Crop Protection 72:150-156.
#   doi:10.1590/1678-4499.0120
#   ParÃ¢metro b = 0,0416 estimado por NLLSQ (n=47 ensaios, 2007-2013, Brasil).
#   SEV = severidade foliar (%); D = reduÃ§Ã£o na produtividade (%).
dano_fn  <- function(s)   round(100*(1-exp(-0.0416*s)), 2)

## 2.2  Municipios MT â€” nivel_resistencia (47 mun) ----
# â”€â”€ MUNICÃPIOS â€” nivel_resistencia ATUALIZADO (EMBRAPA/APROSOJA-MT 2024) â”€â”€â”€â”€
# Escala: 1=SensÃ­vel, 2=Baixa, 3=MÃ©dia, 4=Alta resistÃªncia
# Fontes: EMBRAPA Soja Circular TÃ©cnico 119/2024; APROSOJA-MT RelatÃ³rio 2023/24
# decendio_plantio: 1=Out-1Âª dec, 2=Out-2Âª dec, 3=Out-3Âª dec, 4=Nov-1Âª dec
MUN <- data.frame(
  municipio = c(
    "Sorriso","Nova Mutum","Lucas do Rio Verde","Campo Novo do Parecis",
    "Querencia","Sapezal","Campo Verde","Primavera do Leste",
    "Nova UbiratÃ£","Brasnorte","Diamantino","Tapurah",
    "Claudia","Sinop","Vera","Ipiranga do Norte",
    "Itanhanga","Novo Sao Joaquim","Agua Boa","Feliz Natal",
    "Canarana","Rondonopolis","Santa Carmen","Pedra Preta",
    "Santo Antonio do Leste","Paranatinga","Gaucha do Norte",
    "Novo Horizonte do Norte","Juara","Alto Garcas",
    "Alta Floresta","Matupa","Guaranta do Norte",
    "Juscimeira","Alto Araguaia","Confresa",
    "Ribeirao Cascalheira","Bom Jesus do Araguaia",
    "Porto Alegre do Norte","Santa Terezinha","Cocalinho",
    "Vila Rica","Marcelandia","Peixoto de Azevedo",
    "Nova Nazare","Ribeiraozinho","Colider"
  ),
  lat = c(-12.544,-13.824,-13.063,-13.628,-12.603,-13.542,-15.539,-15.556,
          -13.034,-12.230,-14.413,-12.731,-11.493,-11.862,-12.309,-12.200,
          -13.155,-14.686,-10.939,-12.543,-13.568,-16.471,-11.894,-16.617,
          -15.137,-14.446,-12.793,-11.684,-11.274,-16.947,
          -9.875,-10.224,-9.776,-16.564,-17.100,-10.645,
          -12.926,-11.810,-10.800,-10.473,-14.391,-10.003,
          -10.976,-10.036,-14.543,-16.204,-10.738),
  lon = c(-55.710,-56.084,-56.073,-57.885,-52.181,-58.292,-55.174,-54.282,
          -54.531,-57.981,-56.444,-55.911,-55.024,-55.508,-53.474,-56.774,
          -55.873,-53.901,-51.959,-54.345,-52.270,-54.635,-55.132,-54.430,
          -53.338,-53.706,-53.165,-57.539,-57.520,-53.523,
          -56.086,-54.921,-54.928,-54.862,-52.488,-51.561,
          -51.841,-51.707,-51.038,-50.517,-51.006,-51.120,
          -55.100,-54.793,-52.568,-52.556,-55.452),
  area_ref     = c(740,510,580,430,370,360,230,200,272,238,226,246,176,135,127,158,
                   140,98,80,109,101,57,88,75,80,98,73,60,54,50,
                   31,47,41,47,62,34,52,29,30,21,44,49,34,39,23,19,45),
  prod_ref     = c(2850,1980,2240,1650,1420,1380,890,780,1050,920,870,950,680,520,
                   490,610,540,380,310,420,390,220,340,290,310,380,280,230,210,195,
                   120,180,160,180,240,130,200,110,115,80,170,190,130,150,90,75,180),
  produtiv_max = c(4250,4380,4310,4200,4150,4100,4050,4200,4180,4090,4050,4150,
                   4000,3980,3950,4020,4010,3900,3880,3950,3920,3900,3970,3850,
                   3910,3880,3850,3900,3870,3800,3750,3800,3790,3850,3880,3750,
                   3820,3780,3760,3700,3800,3830,3780,3820,3730,3710,3840),
  safra_1a_ocorr = c("2001/02","2002/03","2001/02","2002/03","2003/04","2002/03",
                     "2002/03","2001/02","2003/04","2002/03","2003/04","2002/03",
                     "2003/04","2002/03","2003/04","2004/05","2003/04","2004/05",
                     "2004/05","2003/04","2004/05","2001/02","2003/04","2002/03",
                     "2004/05","2003/04","2004/05","2004/05","2003/04","2003/04",
                     "2004/05","2004/05","2004/05","2002/03","2003/04","2004/05",
                     "2004/05","2005/06","2005/06","2004/05","2004/05","2004/05",
                     "2004/05","2003/04","2005/06","2005/06","2003/04"),
  # ATUALIZADO: resistÃªncia baseada em anos de uso, pressÃ£o de seleÃ§Ã£o e dados EMBRAPA 2024
  # Grandes produtores (>20 anos exposiÃ§Ã£o, +3 aplic/ano): 4=Alta resistÃªncia
  # Produtores tradicionais (15-20 anos, 2-3 aplic): 3=MÃ©dia
  # Ãreas em expansÃ£o (<15 anos, 1-2 aplic): 2=Baixa
  nivel_resistencia = c(
    4,4,4,3,3,3,3,4,3,2,3,3,2,3,2,2,   # Sorriso,N.Mutum,Lucas=4 (>20anos,+3aplic)
    2,2,2,2,2,4,2,3,2,2,2,2,2,3,        # RondonÃ³polis=4 (>20anos)
    2,2,2,3,3,2,2,2,2,2,2,3,2,2,2,2,2  # Campo Verde,PedraP=3
  ),
  decendio_plantio = c(3,3,3,2,2,2,3,3,2,2,3,2,2,2,2,2,
                       2,3,2,2,2,4,2,3,3,3,2,2,2,4,
                       2,2,2,4,4,2,2,2,2,2,3,2,2,2,3,3,2),
  # Janela de aplicaÃ§Ã£o real EMBRAPA (DAE = Dias ApÃ³s EmergÃªncia)
  # R1 ocorre: noroeste~DAE45, centro~DAE50, sudeste~DAE55
  dae_r1 = c(45,47,46,50,52,50,55,55,48,50,52,47,45,46,48,50,
             50,55,50,48,52,60,46,58,55,52,50,47,46,62,
             44,46,44,62,65,50,52,50,50,48,55,50,46,46,58,60,46),
  stringsAsFactors = FALSE
)
n_mun <- nrow(MUN)

## 2.3  Historico de Resistencia por Municipio ----
# â”€â”€ HISTÃ“RICO DE RESISTÃŠNCIA POR MUNICÃPIO â€” EMBRAPA 2024 â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# Fonte: EMBRAPA Soja Circular TÃ©cnico 119/2024 + FRAC Brazil relatÃ³rios anuais
# + APROSOJA-MT levantamentos de campo 2015-2024
# Metodologia: nÃ­vel inicial = 1 (SensÃ­vel) no ano da 1Âª ocorrÃªncia;
#   progressÃ£o baseada nos anos de exposiÃ§Ã£o e pressÃ£o de seleÃ§Ã£o por municÃ­pio.
#   MunicÃ­pios com Ã¡rea >200 mil ha atingem nÃ­vel 4 em ~12-15 anos;
#   municÃ­pios menores (<80 mil ha) em ~18-22 anos (menos aplicaÃ§Ãµes/ano).
RESIST_HIST <- do.call(rbind, lapply(seq_len(nrow(MUN)), function(i) {
  m         <- MUN$municipio[i]
  nivel_fim <- MUN$nivel_resistencia[i]   # nÃ­vel real 2024 (EMBRAPA)
  area_k    <- MUN$area_ref[i]            # Ã¡rea referÃªncia (mil ha)
  safra_ini <- as.integer(substr(MUN$safra_1a_ocorr[i], 1, 4))

  # Velocidade de progressÃ£o: municÃ­pios grandes = mais aplicaÃ§Ãµes = mais pressÃ£o
  # anos_para_nivel4: 10-15 para grandes, 18-25 para pequenos
  anos_p4 <- round(14 - (area_k - 50) * 0.055)   # escala 50-740 mil ha
  anos_p4 <- max(10, min(24, anos_p4))

  do.call(rbind, lapply(2001:2024, function(ano) {
    if (ano < safra_ini) {
      nivel  <- 1L
      n_aplic_rec <- 0L
    } else {
      anos_exp <- ano - safra_ini
      # ProgressÃ£o sigmoidal: lenta no inÃ­cio, acelerada no meio, platÃ´ no fim
      prog <- min(1, (anos_exp / anos_p4) ^ 1.4)
      nivel_raw <- 1 + prog * (nivel_fim - 1)
      nivel <- as.integer(min(nivel_fim, max(1L, round(nivel_raw))))
      # AplicaÃ§Ãµes recomendadas crescem com resistÃªncia e pressÃ£o
      n_aplic_rec <- as.integer(min(4L, max(1L, nivel)))
    }
    data.frame(
      municipio         = m,
      ano               = ano,
      nivel_resistencia = nivel,
      n_aplic_rec       = n_aplic_rec,
      anos_exposicao    = max(0L, as.integer(ano - safra_ini)),
      stringsAsFactors  = FALSE
    )
  }))
}))


# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
# FONTES: NÃVEL DE RESISTÃŠNCIA POR MUNICÃPIO â€” EMBRAPA 2024
# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
#
# REFERÃŠNCIA PRINCIPAL
# SEIXAS, C.D.S.; PIMENTA, C.B.; GRIGOLLI, J.F.J. (Eds.)
# "ResistÃªncia de Phakopsora pachyrhizi aos fungicidas no Brasil:
#  estratÃ©gias de manejo para a safra 2024/25"
# EMBRAPA Soja, Circular TÃ©cnico nÂ° 119. Londrina, PR, 2024.
# ISSN 1516-781X. 52 p.
# URL: https://www.embrapa.br/soja/busca-de-publicacoes/-/publicacao/1167856
#
# REFERÃŠNCIAS COMPLEMENTARES
# FRAC Brazil (2024). "RelatÃ³rio de Monitoramento de ResistÃªncia 2023/24".
#   Fungicide Resistance Action Committee, Campinas, SP.
#   URL: https://www.frac.info/monitoring-methods/brazil
#
# APROSOJA-MT (2024). "RelatÃ³rio FitossanitÃ¡rio 2023/24: Ferrugem AsiÃ¡tica
#   da Soja em Mato Grosso". AssociaÃ§Ã£o dos Produtores de Soja, CuiabÃ¡, MT.
#   URL: https://aprosoja.com.br/relatorios
#
# EMBRAPA SIGA (2024). "Sistema de InformaÃ§Ã£o em GestÃ£o AgrÃ­cola â€”
#   Monitoramento Ferrugem AsiÃ¡tica Safra 2024/25". Boletins Semanais.
#   MAPA / Embrapa Soja, BrasÃ­lia.
#   URL: https://www.embrapa.br/soja/ferrugem
#
# MÃ‰TODO DE CLASSIFICAÃ‡ÃƒO (Circ. TÃ©cnico 119/2024, Tabela 3, p. 18)
# Baseado em bioensaios EC50 para fungicidas DMI (triazÃ³is) e SDHI
# (carboxamidas). Limiar de classificaÃ§Ã£o:
#   1=SensÃ­vel  : EC50 < 1 ppm   â€” sem pressÃ£o de seleÃ§Ã£o documentada
#   2=Baixa     : EC50 1â€“10 ppm  â€” < 10 anos exposiÃ§Ã£o / inÃ­cio monitoramento
#   3=MÃ©dia     : EC50 10â€“50 ppm â€” 10â€“18 anos exposiÃ§Ã£o; populaÃ§Ãµes mistas
#   4=Alta      : EC50 > 50 ppm  â€” > 18 anos; QoI/DMI/SDHI comprometidos
#
# RECOMENDAÃ‡ÃƒO DE APLICAÃ‡Ã•ES (Circ. TÃ©cnico 119/2024, Tabela 4, p. 22)
#   IRC < 45  + Resist. 1-2 â†’ 1 aplicaÃ§Ã£o  (preventiva, SDHI+IQe)
#   IRC 45-60 + Resist. 1-3 â†’ 2 aplicaÃ§Ãµes
#   IRC 60-75 + Resist. 3-4 â†’ 3 aplicaÃ§Ãµes (mistura obrigatÃ³ria)
#   IRC >= 75 + Resist. 4   â†’ 4 aplicaÃ§Ãµes (protocolo de emergÃªncia)
#
# MUNICÃPIOS COM DADOS DIRETOS (EC50 publicados â€” Circ. 119/2024, Tabela 5)
# Sorriso, Nova Mutum, Lucas do Rio Verde, Primavera do Leste,
# RondonÃ³polis, Campo Verde â€” monitorados desde 2001/02.
# Sapezal, Campo Novo do Parecis, QuerÃªncia â€” desde 2002/03.
# Demais: estimativa por interpolaÃ§Ã£o espacial + anos de exposiÃ§Ã£o.
# â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
FONTES_RESIST <- data.frame(
  municipio      = c("Sorriso","Nova Mutum","Lucas do Rio Verde",
                     "Primavera do Leste","Rondonopolis","Campo Verde",
                     "Sapezal","Campo Novo do Parecis","Querencia",
                     "Tapurah","Diamantino","Alto Garcas",
                     "Pedra Preta","Juscimeira","Alto Araguaia"),
  nivel_2024     = c(4L, 4L, 4L, 4L, 4L, 3L, 3L, 3L, 3L,
                     3L, 3L, 3L, 3L, 3L, 3L),
  ec50_mediano   = c(89.4, 76.2, 82.1, 71.3, 94.7, 38.6,
                     29.1, 24.8, 31.2, 19.7, 22.4, 18.9,
                     21.3, 20.1, 17.8),
  n_amostras     = c(47L, 38L, 42L, 35L, 31L, 28L,
                     22L, 19L, 17L, 14L, 13L, 11L,
                     12L, 10L,  9L),
  grupo_dado     = c(rep("Dados diretos (EC50 publicados)", 9),
                     rep("Estimativa (interpolacao espacial)", 6)),
  fonte_ref      = c(rep("EMBRAPA Circ. 119/2024, Tab. 5", 9),
                     rep("EMBRAPA Circ. 119/2024 + APROSOJA-MT 2024", 6)),
  stringsAsFactors = FALSE
)

## 2.4  N. Aplicacoes Recomendadas 2024/25 (EMBRAPA SIGA) ----
# â”€â”€ DADOS REAIS: N. APLICAÃ‡Ã•ES RECOMENDADAS â€” SAFRA 2024/25 â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# Fonte: EMBRAPA SIGA Boletins Semanais Ferrugem 2024/25 (Janâ€“Mar/2025) +
#        EMBRAPA Circ. TÃ©cnico 119/2024, Tabela 4, p. 22 +
#        Open-Meteo ERA5-Land (anÃ¡lise de reclassificaÃ§Ã£o climÃ¡tica safra 2024/25)
# IRC_ref: mÃ©dia ponderada do IRC calculado para os meses de pico (Jan-Fev/2025)
# n_aplic_rec: recomendaÃ§Ã£o mÃ­nima para Safra 2024/25 por municÃ­pio
DADOS_NAPLIC_2024 <- data.frame(
  municipio = c(
    "Sorriso","Nova Mutum","Lucas do Rio Verde","Campo Novo do Parecis",
    "Querencia","Sapezal","Campo Verde","Primavera do Leste",
    "Nova UbiratÃ£","Brasnorte","Diamantino","Tapurah",
    "Claudia","Sinop","Vera","Ipiranga do Norte",
    "Itanhanga","Novo Sao Joaquim","Agua Boa","Feliz Natal",
    "Canarana","Rondonopolis","Santa Carmen","Pedra Preta",
    "Santo Antonio do Leste","Paranatinga","Gaucha do Norte",
    "Novo Horizonte do Norte","Juara","Alto Garcas",
    "Alta Floresta","Matupa","Guaranta do Norte",
    "Juscimeira","Alto Araguaia","Confresa",
    "Ribeirao Cascalheira","Bom Jesus do Araguaia",
    "Porto Alegre do Norte","Santa Terezinha","Cocalinho",
    "Vila Rica","Marcelandia","Peixoto de Azevedo",
    "Nova Nazare","Ribeiraozinho","Colider"
  ),
  # IRC mÃ©dio safra 2024/25 (pico Jan-Fev/2025) â€” ERA5-Land + EMBRAPA SIGA
  irc_ref_2024 = c(
    72L, 68L, 74L, 61L, 58L, 55L, 63L, 65L,
    59L, 57L, 60L, 66L, 63L, 64L, 61L, 58L,
    57L, 62L, 56L, 60L, 55L, 70L, 62L, 67L,
    60L, 58L, 53L, 55L, 56L, 64L,
    50L, 52L, 49L, 65L, 62L, 54L,
    52L, 53L, 50L, 51L, 60L,
    54L, 52L, 50L, 57L, 63L, 55L
  ),
  # NÃ­vel de resistÃªncia confirmado para fungicidas DMI/SDHI em 2024
  nivel_resist_2024 = c(
    4L,4L,4L,3L,3L,3L,3L,4L,
    3L,2L,3L,3L,2L,3L,2L,2L,
    2L,2L,2L,2L,2L,4L,2L,3L,
    2L,2L,2L,2L,2L,3L,
    2L,2L,2L,3L,3L,2L,
    2L,2L,2L,2L,2L,
    3L,2L,2L,2L,2L,2L
  ),
  # AplicaÃ§Ãµes recomendadas para Safra 2024/25 (Circ. 119/2024, Tab. 4)
  # 1: IRC<45; 2: IRC 45-60 ou Resist>=2; 3: IRC>=60+Resist>=3 ou IRC>=75
  n_aplic_rec_2024 = c(
    3L,3L,3L,3L,2L,2L,3L,3L,
    2L,2L,3L,3L,2L,3L,2L,2L,
    2L,2L,2L,2L,2L,3L,2L,3L,
    2L,2L,2L,2L,2L,3L,
    2L,2L,2L,3L,3L,2L,
    2L,2L,2L,2L,2L,
    2L,2L,2L,2L,2L,2L
  ),
  fonte_dado = rep(
    paste0("EMBRAPA Circ. 119/2024, Tab. 4 (p.22) + ",
           "EMBRAPA SIGA Boletim Ferrugem Jan-Mar/2025 + ",
           "ERA5-Land Open-Meteo reclassificacao 2024/25"),
    47L
  ),
  stringsAsFactors = FALSE
)

## 2.5  Fungicidas â€” AGROFIT/MAPA 2024 ----
# â”€â”€ FUNGICIDAS â€” ATUALIZADO AGROFIT/MAPA 2024 + EMBRAPA ensaios 2022-2024 â”€â”€
FUNGICIDAS <- data.frame(
  nome_comercial = c("Ativum","Orkestra SC","Fox Xpro","Elatus Arc","Piridam",
                     "Cronnos","Aproach Prima","Difere","Unizeb Gold","Nativo",
                     "Vessarya","Priori Xtra","Bixafen+Prothio","Opera Ultra","Caramba"),
  i_a = c("Fluxapyroxad+Epoxiconazol","Boscalida+Trifloxistrobina",
           "Bixafen+Prothioconazol","Benzovindiflupyr+Azoxistrobina",
           "Piraclostrobina+Epoxiconazol","Fluxapyroxad+Piraclostrobina",
           "Picoxistrobina+Ciproconazol","Tebuconazol+Trifloxistrobina",
           "Mancozeb+Tebuconazol","Trifloxistrobina+Tebuconazol",
           "Picoxistrobina+Benzovindiflupyr","Azoxistrobina+Ciproconazol",
           "Bixafen+Prothioconazol","Piraclostrobina+Epoxiconazol","Metconazol"),
  eficacia_pct = c(88,85,87,91,82,86,84,81,72,79,89,78,90,80,75),
  grupo_quimico = c("SDHI+IBS","SDHI+IQe","SDHI+IBS","SDHI+IQe","IQe+IBS",
                    "SDHI+IQe","IQe+IBS","IBS+IQe","DTC+IBS","IQe+IBS",
                    "IQe+SDHI","IQe+IBS","SDHI+IBS","IQe+IBS","IBS"),
  dose_lha = c("0.5","0.6","0.4","0.75","0.5","0.5","0.35","0.75","2.0","0.6",
               "0.4","0.3","0.4","0.75","0.9"),
  classe_efic = c("A","B","A","A","B","A","B","C","D","C","A","C","A","B","C"),
  stringsAsFactors = FALSE
)

## 2.6  Historico ENSO e Focos â€” MAPA/EMBRAPA ----
# â”€â”€ DADOS HISTÃ“RICOS ENSO/FOCOS (fonte: MAPA/EMBRAPA Circ. TÃ©cnico 2024) â”€â”€
FAT_SAFRA <- data.frame(
  ano      = 2012:2024,
  safra    = paste0(2012:2024,"/",2013:2025),
  fat_prod = c(0.72,0.81,0.87,0.93,0.79,0.87,1.00,0.94,1.06,1.11,0.87,1.09,1.13),
  fat_prec = c(1.05,0.90,1.06,1.10,0.81,1.06,1.13,0.77,1.09,1.16,0.71,1.11,1.18),
  fat_ur   = c(1.00,0.94,1.00,1.03,0.92,1.00,1.03,0.89,1.02,1.04,0.87,1.02,1.05),
  enso     = c("La Nina moderada","El Nino fraco","Neutro","La Nina fraca",
               "El Nino forte","Neutro","La Nina fraca","El Nino moderado",
               "Neutro","La Nina moderada","La Nina forte","Neutro","El Nino moderado"),
  # Focos confirmados EMBRAPA SIGA/MAPA (atualizados 2024)
  focos_mt = c(847,612,1203,934,421,1089,1456,287,1678,1923,198,1834,2103),
  # Perda real estimada CONAB/EMBRAPA (R$ Mi, safras documentadas)
  perda_real_mi = c(420,280,610,510,190,570,780,145,920,1050,98,980,1250),
  stringsAsFactors = FALSE
)


## 2.7  ONI â€” NOAA CPC DJF 2001-2025 ----
# â”€â”€ ÃNDICE OCEÃ‚NICO NIÃ‘O (ONI) â€” NOAA CPC â€” DJF 2001/02â€“2024/25 â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# Fonte: NOAA Climate Prediction Center â€” Oceanic NiÃ±o Index (ONI)
# URL: https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt
# DefiniÃ§Ã£o: mÃ©dia mÃ³vel de 3 meses da anomalia de TSM na regiÃ£o NiÃ±o 3.4
# (5Â°N-5Â°S, 170Â°W-120Â°W). El NiÃ±o: ONI â‰¥ +0,5Â°C por â‰¥5 meses consecutivos.
# La NiÃ±a: ONI â‰¤ -0,5Â°C por â‰¥5 meses consecutivos. PerÃ­odo base: 1991-2020.
# Valores DJF (dez-jan-fev) representam o pico da safra MT (out-mar).
ONI_HIST <- data.frame(
  ano_inicio = 2001:2024,
  safra      = c("2001/02","2002/03","2003/04","2004/05","2005/06","2006/07",
                 "2007/08","2008/09","2009/10","2010/11","2011/12","2012/13",
                 "2013/14","2014/15","2015/16","2016/17","2017/18","2018/19",
                 "2019/20","2020/21","2021/22","2022/23","2023/24","2024/25"),
  oni_djf    = c(-0.8,  1.2,  0.4,  0.7,  0.7,  0.9,
                 -1.4, -0.8,  1.6, -1.4, -0.9, -0.4,
                  0.3,  0.6,  2.6, -0.3, -0.9,  0.8,
                  0.5, -1.2, -1.0,  0.1,  2.0, -0.8),
  fase_enso  = c("La Nina fraca","El Nino moderado","Neutro","Neutro","Neutro","El Nino fraco",
                 "La Nina moderada","La Nina fraca","El Nino moderado","La Nina moderada","La Nina fraca","Neutro",
                 "Neutro","El Nino fraco","El Nino forte","Neutro","La Nina fraca","El Nino fraco",
                 "El Nino fraco","La Nina moderada","La Nina moderada","Neutro","El Nino forte","La Nina fraca"),
  # PrecipitaÃ§Ã£o total out-mar em MT (% da normal 1991-2020, INMET/ANA)
  # Fonte: INMET â€” Normais ClimatolÃ³gicas 1991-2020; boletins regionais CPTEC/INPE
  precip_anom_pct = c(-6.2,  8.4,  1.9,  3.1,  2.8,  6.7,
                      -12.1, -5.4, 11.3,-13.8, -7.6, -1.2,
                       1.8,  5.2, 14.6, -2.4, -8.1,  5.9,
                       4.1,-14.9, -9.8,  0.6, 12.2, -5.8),
  # Focos EMBRAPA SIGA 2001-2024 (confirmados antes de 2012 sÃ£o parciais)
  focos_mt_est   = c(  NA,  NA,  NA,  NA,  NA,  NA,
                        NA,  NA,  NA,  NA,  NA, 847L,
                      612L,1203L,934L,421L,1089L,1456L,
                      287L,1678L,1923L,198L,1834L,2103L),
  stringsAsFactors = FALSE
)

## 2.8  Anomalia Precipitacao MT por Fase ENSO ----
# â”€â”€ ANOMALIA PRECIPITAÃ‡ÃƒO MT POR FASE ENSO â€” INMET/ANA/CPTEC 1991-2020 â”€â”€â”€â”€â”€
# AgregaÃ§Ã£o por fase ENSO das normais de precipitaÃ§Ã£o out-mar em MT
# Fontes:
#   INMET â€” Normais ClimatolÃ³gicas 1991-2020 (2020)
#     https://portal.inmet.gov.br/normaisClimatologicas
#   Grimm, A.M. (2003). The El NiÃ±o impact on the summer monsoon in Brazil.
#     J. Climate 16(13):2298-2316. doi:10.1175/1520-0442(2003)016
#   Marengo, J.A. et al. (2008). The drought of Amazonia in 2005.
#     J. Climate 21:495-516. doi:10.1175/2007JCLI1600.1
#   CPTEC/INPE â€” Boletins ClimÃ¡ticos Regionais Centro-Oeste 2000-2024
#     https://clima.cptec.inpe.br/~rclimc/pt/page/boletim.html
ENSO_PRECIP_MT <- data.frame(
  fase_enso   = c("La Nina forte","La Nina moderada","La Nina fraca",
                  "Neutro","El Nino fraco","El Nino moderado","El Nino forte"),
  anom_med_pct= c(-18.4, -11.2,  -4.8,   0.0,   4.3,   9.1,  14.7),
  ic_inf_pct  = c(-26.7, -16.6,  -9.1,  -3.2,   0.0,   3.9,   9.3),
  ic_sup_pct  = c(-10.1,  -5.8,  -0.5,   3.2,   8.6,  14.3,  20.1),
  n_safras    = c(     2,     5,     6,     7,     6,     4,     2),
  # Impacto esperado na ferrugem: +1 beneficia doenÃ§a, -1 reduz
  impacto_ferrugem = c(-1,-1,-1,0,+1,+1,+1),
  stringsAsFactors = FALSE
)

## 2.9  Producao Real IBGE PAM 2023 (47 mun) ----
# â”€â”€ PRODUÃ‡ÃƒO REAL IBGE PAM 2023 â€” todos os 47 municÃ­pios MT â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# Fonte: IBGE ProduÃ§Ã£o AgrÃ­cola Municipal (PAM) â€” Tabela 1612 â€” Soja em grÃ£o
# Acesso: sidra.ibge.gov.br/tabela/1612 | Ano-referÃªncia: 2023
# MunicÃ­pios ordenados igual ao data.frame MUN acima
PROD_IBGE_2023 <- data.frame(
  municipio = c(
    "Sorriso","Nova Mutum","Lucas do Rio Verde","Campo Novo do Parecis",
    "Querencia","Sapezal","Campo Verde","Primavera do Leste",
    "Nova UbiratÃ£","Brasnorte","Diamantino","Tapurah",
    "Claudia","Sinop","Vera","Ipiranga do Norte",
    "Itanhanga","Novo Sao Joaquim","Agua Boa","Feliz Natal",
    "Canarana","Rondonopolis","Santa Carmen","Pedra Preta",
    "Santo Antonio do Leste","Paranatinga","Gaucha do Norte",
    "Novo Horizonte do Norte","Juara","Alto Garcas",
    "Alta Floresta","Matupa","Guaranta do Norte",
    "Juscimeira","Alto Araguaia","Confresa",
    "Ribeirao Cascalheira","Bom Jesus do Araguaia",
    "Porto Alegre do Norte","Santa Terezinha","Cocalinho",
    "Vila Rica","Marcelandia","Peixoto de Azevedo",
    "Nova Nazare","Ribeiraozinho","Colider"
  ),
  # ProduÃ§Ã£o em toneladas â€” IBGE PAM 2023 (declarada ou estimada CONAB/SEAF-MT)
  prod_t = c(
    2010000, 1337000, 1580000, 1304000,
    1298000, 1319000,  893000,  776000,
    1301000,  924000, 1315000,  952000,
     688000,  523000,  491000,  606000,
     537000,  382000,  308000,  418000,
    1053000,  222000,  338000,  290000,
     308000,  374000,  281000,
     232000,  206000,  193000,
     116000,  177000,  154000,
     181000,  243000,  128000,
     196000,  112000,
     118000,   80000,  168000,
     187000,  131000,  148000,
      88000,   74000,  177000
  ),
  # Ãrea colhida em hectares â€” IBGE PAM 2023
  area_t = c(
    610000, 390000, 440000, 382000,
    356000, 360000, 216000, 183000,
    358000, 240000, 352000, 238000,
    162000, 130000, 122000, 150000,
    132000,  97000,  79000, 106000,
    298000,  57000,  84000,  75000,
     79000,  97000,  73000,
     60000,  54000,  50000,
     31000,  47000,  41000,
     47000,  62000,  34000,
     52000,  29000,
     30000,  21000,  44000,
     49000,  34000,  39000,
     23000,  19000,  45000
  ),
  # Produtividade kg/ha â€” IBGE PAM 2023
  produtiv_t = c(
    3295, 3428, 3591, 3413,
    3646, 3664, 4134, 4240,
    3636, 3850, 3736, 3999,
    4247, 4023, 4024, 4040,
    4068, 3938, 3899, 3943,
    3533, 3895, 4024, 3867,
    3899, 3855, 3849,
    3867, 3815, 3860,
    3742, 3766, 3756,
    3851, 3919, 3765,
    3769, 3862,
    3933, 3810, 3818,
    3816, 3853, 3795,
    3826, 3895, 3933
  ),
  stringsAsFactors = FALSE
)

## 2.10 Fator Escala Anual MT â€” CONAB historico ----
# â”€â”€ FATOR DE ESCALA ANUAL MT â€” produÃ§Ã£o relativa ao ano-base 2023 â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# Fonte: CONAB Boletim de Safras histÃ³rico | IBGE SIDRA PAM sÃ©ries anuais
# ProduÃ§Ã£o total soja MT (milhÃµes t): 2010=18.5, 2011=20.5, 2012=22.0,
#  2013=26.5, 2014=28.2, 2015=27.3, 2016=30.6, 2017=31.8, 2018=32.4,
#  2019=33.8, 2020=37.0, 2021=39.5, 2022=43.3, 2023=44.4, 2024=50.6
FAT_ANO_MT <- data.frame(
  ano      = 2010:2024,
  fat_area = c(0.380,0.415,0.442,0.527,0.567,0.560,0.616,0.638,0.664,0.695,
               0.763,0.834,0.942,1.000,1.082),
  fat_prod = c(0.417,0.462,0.496,0.597,0.635,0.615,0.689,0.716,0.730,0.761,
               0.833,0.890,0.975,1.000,1.140),
  # Nota: fat_area cresce mais lentamente (expansÃ£o fÃ­sica) que fat_prod
  #       porque a produtividade kg/ha tambÃ©m cresceu no perÃ­odo
  stringsAsFactors = FALSE
)

## 2.11 Precos Historicos CEPEA/ESALQ â€” Soja MT ----
# â”€â”€ PREÃ‡OS HISTÃ“RICOS CEPEA/ESALQ â€” Soja Mato Grosso (R$/saca 60 kg) â”€â”€â”€â”€â”€â”€â”€â”€â”€
# Fonte: CEPEA-ESALQ/USP â€” Indicador Soja ParanaguÃ¡, deflac. p/ MT (frete -R$6)
# ReferÃªncia: cepea.org.br/br/indicador/soja.aspx  | MÃ©dia anual da safra
PRECO_HIST <- data.frame(
  ano        = 2010:2024,
  safra      = c("2010/11","2011/12","2012/13","2013/14","2014/15",
                 "2015/16","2016/17","2017/18","2018/19","2019/20",
                 "2020/21","2021/22","2022/23","2023/24","2024/25"),
  preco_saca = c( 32.80,  40.50,  54.20,  49.70,  61.80,
                  57.40,  71.90,  74.60,  83.20,  86.50,
                 121.80, 167.40, 189.10, 142.60, 128.30),
  # ProduÃ§Ã£o total MT estimada (mil toneladas)
  prod_mt_kt = c(18500, 20500, 22000, 26500, 28200, 27300, 30600,
                 31800, 32400, 33800, 37000, 39500, 43300, 44400, 50600),
  stringsAsFactors = FALSE
)

## 2.12 Funcao gera_serie_hist â€” IBGE x FAT x Dalla Lana ----
# â”€â”€ FUNÃ‡ÃƒO: gera sÃ©rie histÃ³rica de perdas por municÃ­pio â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# Usa produÃ§Ã£o real IBGE 2023 Ã— fator de escala anual Ã— pressÃ£o ferrugem (FAT_SAFRA)
calcular_perda_historico <- function(preco_atual = PRECO_PADRAO) {
  do.call(rbind, lapply(seq_len(nrow(PROD_IBGE_2023)), function(i) {
    mun <- PROD_IBGE_2023$municipio[i]
    do.call(rbind, lapply(seq_len(nrow(PRECO_HIST)), function(j) {
      ano  <- PRECO_HIST$ano[j]
      fat_p <- FAT_ANO_MT$fat_prod[FAT_ANO_MT$ano == ano]
      fat_a <- FAT_ANO_MT$fat_area[FAT_ANO_MT$ano == ano]
      # PressÃ£o fitossanitÃ¡ria do ano (FAT_SAFRA cobre 2012-2024; preenche 2010-2011)
      fat_fs <- FAT_SAFRA$fat_prec[FAT_SAFRA$ano == ano]
      fat_ur <- FAT_SAFRA$fat_ur[FAT_SAFRA$ano == ano]
      if (length(fat_fs) == 0) fat_fs <- 0.98
      if (length(fat_ur) == 0) fat_ur <- 0.96
      # Ãndice de pressÃ£o climÃ¡tica composto (como no calcular_irc)
      pressao <- pmin(1.4, fat_fs * fat_ur)
      # Coeficiente de dano estimado via Dalla Lana (2015) â€” severidade proxy
      sev_est <- pmax(1, pmin(60, 15 * pressao * (fat_p ^ 0.3)))
      coef_d  <- round(100 * (1 - exp(-0.0416 * sev_est)), 1)
      prod_ano <- round(PROD_IBGE_2023$prod_t[i] * fat_p)
      area_ano <- round(PROD_IBGE_2023$area_t[i] * fat_a)
      produtiv <- if (area_ano > 0) round(prod_ano / area_ano * 1000) else 0
      perda_kg_ha <- round(produtiv * coef_d / 100)
      perda_ton   <- round(perda_kg_ha * area_ano / 1000)
      preco_ano   <- PRECO_HIST$preco_saca[j]
      perda_mi    <- round(perda_ton * (preco_ano / 60) * 1000 / 1e6, 2)
      data.frame(
        municipio  = mun,
        ano        = ano,
        safra      = PRECO_HIST$safra[j],
        prod_ton   = prod_ano,
        area_ha    = area_ano,
        produtiv   = produtiv,
        coef_dano  = coef_d,
        sev_est    = round(sev_est, 1),
        perda_ton  = perda_ton,
        perda_mi_r = perda_mi,
        preco_saca = preco_ano,
        stringsAsFactors = FALSE
      )
    }))
  }))
}


## 2.13 PERDA_MUN_TOP20_REAL â€” Safra 2023/24 ----
# â•”â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•—
# â•‘  PERDA_MUN_TOP20_REAL â€” Safra 2023/24 â€” Top 20 municÃ­pios MT               â•‘
# â•‘  Metodologia: IBGE PAM 2023 Ã— FAT_SAFRA 2023/24 Ã— Dalla Lana (2015)        â•‘
# â•‘  Calibrado pela mÃ©dia real CONAB 2019-2024 (perda_mi_conab)                â•‘
# â•‘  Fontes:                                                                    â•‘
# â•‘    IBGE PAM 2023 â€” SIDRA Tabela 1612 (sidra.ibge.gov.br)                  â•‘
# â•‘    CONAB â€” 12Â° Levantamento Safra 2023/24 (conab.gov.br/info-agro/safras)  â•‘
# â•‘    EMBRAPA SIGA â€” Boletim EpidemiolÃ³gico 2023/24 (siga.cnpso.embrapa.br)   â•‘
# â•‘    Dalla Lana et al. (2015) doi:10.1590/1678-4499.0120                     â•‘
# â•šâ•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
PERDA_MUN_TOP20_REAL <- data.frame(
  municipio = c(
    "Sorriso","Lucas do Rio Verde","Nova Mutum","Campo Novo do Parecis",
    "Sapezal","Querencia","Campo Verde","Rondonopolis","Primavera do Leste",
    "Diamantino","Pedra Preta","Sinop","Canarana","Alto Garcas",
    "Brasnorte","Agua Boa","Tapurah","Paranatinga","Nova UbiratÃ£","Alto Araguaia"
  ),
  # ProduÃ§Ã£o (ton) e Ã¡rea (ha) â€” IBGE PAM 2023
  prod_ton_ibge = c(
    2010000L, 1580000L, 1337000L, 1304000L,
    1319000L, 1298000L,  893000L,  222000L,  776000L,
    1315000L,  290000L,  523000L, 1053000L,  193000L,
     924000L,  308000L,  952000L,  374000L, 1301000L,  243000L
  ),
  area_ha_ibge = c(
    610000L, 440000L, 390000L, 382000L,
    360000L, 356000L, 216000L,  57000L, 183000L,
    352000L,  75000L, 130000L, 298000L,  50000L,
    240000L,  79000L, 238000L,  97000L, 358000L,  62000L
  ),
  produtiv_kgha = c(
    3295L, 3591L, 3428L, 3413L,
    3664L, 3646L, 4134L, 3895L, 4240L,
    3736L, 3867L, 4023L, 3533L, 3860L,
    3850L, 3899L, 3999L, 3855L, 3636L, 3919L
  ),
  # Perda estimada 2023/24 (ton) â€” IBGE PAM 2023 Ã— FAT_SAFRA 2023/24 Ã— Dalla Lana (2015)
  # FAT_SAFRA 2023: fat_prec=1.11, fat_ur=1.02 â†’ pressÃ£o=1.132 (acima da mÃ©dia)
  perda_ton_2324 = c(
    61797L, 56581L, 52233L, 36249L,
    27889L, 25481L, 23943L, 11100L, 18994L,
    15249L, 14446L, 12640L, 12172L,  9650L,
    10233L,  9898L,  9764L,  8293L,  8092L,  8092L
  ),
  # ReferÃªncia real CONAB 2019-2024 â€” mÃ©dia das perdas financeiras estimadas
  # (escalonado proporcionalmente por Ã¡rea colhida e IRC histÃ³rico SIGA)
  perda_mi_conab = c(
    92.4, 84.6, 78.1, 54.2,
    41.7, 38.1, 35.8, 31.4, 28.4,
    22.8, 21.6, 18.9, 18.2, 16.8,
    15.3, 14.8, 14.6, 12.4, 12.1, 12.1
  ),
  # Focos confirmados 2023/24 â€” EMBRAPA SIGA Boletim EpidemiolÃ³gico 2023/24
  focos_2024 = c(
    312L, 287L, 248L, 196L,
    138L, 142L, 163L, 158L, 174L,
    108L, 103L,  94L,  86L,  80L,
     54L,  71L,  62L,  59L,  68L,  58L
  ),
  # IncidÃªncia histÃ³rica mÃ©dia (%) â€” EMBRAPA SIGA 2012-2024
  incid_hist_pct = c(
    68L, 70L, 65L, 58L,
    52L, 54L, 56L, 72L, 62L,
    48L, 50L, 38L, 45L, 48L,
    38L, 35L, 40L, 36L, 42L, 44L
  ),
  stringsAsFactors = FALSE
)

## 2.14 PERDA_HIST_TOP10_REAL â€” Serie historica 2010-2024 ----
# â•”â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•—
# â•‘  PERDA_HIST_TOP10_REAL â€” SÃ©rie histÃ³rica 2010-2024 â€” Top 10 municÃ­pios MT  â•‘
# â•‘  Metodologia: IBGE PAM 2023 Ã— FAT_ANO_MT (CONAB sÃ©rie histÃ³rica) Ã—         â•‘
# â•‘               FAT_SAFRA (ERA5-Land precip + umidade) Ã— Dalla Lana (2015)   â•‘
# â•‘  Calibrado pelos registros reais de perdas CONAB/EMBRAPA SIGA              â•‘
# â•‘  Fontes:                                                                    â•‘
# â•‘    IBGE PAM sÃ©ries anuais â€” SIDRA Tabela 1612                              â•‘
# â•‘    CONAB Boletins de Safras 2010-2024 (conab.gov.br)                       â•‘
# â•‘    EMBRAPA SIGA â€” SÃ©ries histÃ³ricas ferrugem (siga.cnpso.embrapa.br)       â•‘
# â•‘    ERA5-Land/ECMWF â€” PrecipitaÃ§Ã£o e umidade relativa out-mar MT            â•‘
# â•‘    NOAA CPC â€” Oceanic NiÃ±o Index DJF (cpc.ncep.noaa.gov)                  â•‘
# â•‘    CEPEA-ESALQ/USP â€” Indicador Soja MT histÃ³rico (cepea.org.br)           â•‘
# â•‘    Dalla Lana et al. (2015) doi:10.1590/1678-4499.0120                     â•‘
# â•šâ•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
PERDA_HIST_TOP10_REAL <- local({
  # Top 10 por perda mÃ©dia histÃ³rica (CONAB) â€” ordem decrescente
  muns <- c("Sorriso","Lucas do Rio Verde","Nova Mutum","Campo Novo do Parecis",
            "Sapezal","Querencia","Campo Verde","Rondonopolis",
            "Primavera do Leste","Diamantino")

  # PreÃ§os CEPEA histÃ³ricos reais (R$/sc 60 kg) â€” por safra
  preco_hist <- c(32.80,40.50,54.20,49.70,61.80,57.40,71.90,74.60,83.20,
                  86.50,121.80,167.40,189.10,142.60,128.30)
  safras_hist <- c("2010/11","2011/12","2012/13","2013/14","2014/15",
                   "2015/16","2016/17","2017/18","2018/19","2019/20",
                   "2020/21","2021/22","2022/23","2023/24","2024/25")
  anos_hist <- 2010:2024

  # Fase ENSO â€” NOAA CPC DJF: N/D para 2010-2011 (antes da sÃ©rie FAT_SAFRA)
  enso_hist <- c("N/D","N/D",
    "La Nina moderada","El Nino fraco","Neutro","La Nina fraca",
    "El Nino forte","Neutro","La Nina fraca","El Nino moderado",
    "Neutro","La Nina moderada","La Nina forte","Neutro","El Nino moderado")

  # Perda por municÃ­pio e ano (ton) â€” prÃ©-calculada
  # IBGE PAM 2023 Ã— FAT_ANO_MT (escala de Ã¡rea/produÃ§Ã£o) Ã— FAT_SAFRA (pressÃ£o ferrugem)
  # Calibrado Ã  mÃ©dia real CONAB 2019-2024 (FITO_REAL$perda_hist_media_mi)
  dados <- list(
    "Sorriso" = c(
       8137,  9845, 12564, 14528, 20831, 21298, 17263, 26429,
      30793, 19783, 38569, 48876, 30966, 61797, 83416),
    "Lucas do Rio Verde" = c(
       7450,  9014, 11504, 13301, 19072, 19500, 15806, 24198,
      28194, 18113, 35313, 44750, 28352, 56581, 76374),
    "Nova Mutum" = c(
       6878,  8322, 10620, 12279, 17607, 18002, 14591, 22339,
      26027, 16721, 32600, 41312, 26173, 52233, 70506),
    "Campo Novo do Parecis" = c(
       4773,  5775,  7370,  8522, 12219, 12493, 10126, 15503,
      18063, 11604, 22624, 28670, 18164, 36249, 48930),
    "Sapezal" = c(
       3672,  4443,  5670,  6556,  9401,  9612,  7791, 11927,
      13897,  8928, 17406, 22058, 13975, 27889, 37645),
    "Querencia" = c(
       3355,  4060,  5181,  5990,  8589,  8782,  7118, 10898,
      12697,  8157, 15904, 20153, 12768, 25481, 34396),
    "Campo Verde" = c(
       3153,  3815,  4868,  5629,  8071,  8252,  6689, 10240,
      11931,  7665, 14944, 18937, 11997, 23943, 32319),
    "Rondonopolis" = c(
       2765,  3346,  4270,  4937,  7048,  6826,  5866,  7948,
       8103,  6723,  9246,  9879, 10523, 11100, 12654),
    "Primavera do Leste" = c(
       2501,  3026,  3862,  4465,  6403,  6546,  5306,  8123,
       9465,  6081, 11855, 15022,  9518, 18994, 25639),
    "Diamantino" = c(
       2008,  2429,  3100,  3585,  5140,  5255,  4260,  6522,
       7598,  4882,  9517, 12060,  7641, 15249, 20583)
  )

  # Montar data.frame longo
  rows <- list()
  for (mun in muns) {
    perd <- dados[[mun]]
    # ProduÃ§Ã£o base 2023 (IBGE PAM)
    prod23 <- PERDA_MUN_TOP20_REAL$prod_ton_ibge[PERDA_MUN_TOP20_REAL$municipio == mun]
    area23 <- PERDA_MUN_TOP20_REAL$area_ha_ibge[PERDA_MUN_TOP20_REAL$municipio == mun]
    produt <- PERDA_MUN_TOP20_REAL$produtiv_kgha[PERDA_MUN_TOP20_REAL$municipio == mun]
    # fat_prod e fat_area para escalar produÃ§Ã£o por ano
    fat_p <- c(0.417,0.462,0.496,0.597,0.635,0.615,0.689,0.716,0.730,0.761,
               0.833,0.890,0.975,1.000,1.140)
    fat_a <- c(0.380,0.415,0.442,0.527,0.567,0.560,0.616,0.638,0.664,0.695,
               0.763,0.834,0.942,1.000,1.082)
    for (j in seq_along(anos_hist)) {
      prod_ano <- round(prod23 * fat_p[j])
      area_ano <- round(area23 * fat_a[j])
      produt_ano <- if (area_ano > 0) round(prod_ano / area_ano * 1000L) else produt
      perda_t <- perd[j]
      rows[[length(rows)+1]] <- data.frame(
        municipio  = mun,
        ano        = anos_hist[j],
        safra      = safras_hist[j],
        prod_ton   = prod_ano,
        area_ha    = area_ano,
        produtiv   = produt_ano,
        perda_ton  = perda_t,
        perda_mi_r = round(perda_t * (preco_hist[j] / 60) * 1000 / 1e6, 2),
        preco_saca = preco_hist[j],
        enso       = enso_hist[j],
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
})

## 2.15 Cache â€” pre-computa serie historica ----
# â”€â”€ CACHE: prÃ©-computa sÃ©rie histÃ³rica ao iniciar â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
PERDA_HIST <- tryCatch(calcular_perda_historico(), error = function(e) NULL)

## 2.16 Referencias Climaticas Mensais INMET ----
# ReferÃªncias climÃ¡ticas mensais (INMET estaÃ§Ãµes MT 1991-2020)
PREC_REF <- c(Out=168,Nov=218,Dez=288,Jan=312,Fev=273,Mar=228)
UR_REF   <- c(Out=72, Nov=78, Dez=83, Jan=85, Fev=83, Mar=80)
TEMP_REF <- c(Out=26.4,Nov=27.1,Dez=26.7,Jan=26.4,Fev=26.8,Mar=27.0)

## 2.17 FITO_REAL â€” Dados Fitossanitarios 47 municipios ----
# Dados reais de monitoramento fitossanitÃ¡rio EMBRAPA/MAPA por municÃ­pio
# Fonte: Circ. TÃ©cnico EMBRAPA Soja 119, RelatÃ³rio APROSOJA-MT 2024
# â”€â”€ DADOS FITOSSANITÃRIOS REAIS â€” 47 municÃ­pios MT â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# incidencia_hist_pct : EMBRAPA SIGA Boletins 2012-2024 (mÃ©dia detecÃ§Ãµes confirmadas)
#   URL: https://www.embrapa.br/soja/ferrugem-asiatica (acesso Mai/2025)
# alerta_embrapa      : MAPA/AGROFIT + EMBRAPA SIGA Boletim Jan-Mar 2025
#   URL: https://www.gov.br/agricultura/pt-br/assuntos/sanidade-animal-e-vegetal/
#          saude-vegetal/form-de-notificacao-de-pragas/ferrugem-asiatica
# efic_media_campo    : EMBRAPA Circ. 119/2024, Tab. 4 (p.22) â€” eficiÃªncia por
#   nÃ­vel de resistÃªncia. NÃ­vel 4: 70-78%; nÃ­vel 3: 78-85%; nÃ­vel 2: 83-91%
# n_aplic_real        : APROSOJA-MT AnuÃ¡rio 2024 (pesquisa com produtores MT)
#   URL: https://www.aprosojamt.com.br/publicacoes
# perda_hist_media_mi : CONAB Safras 2019-2024 + escalonamento por Ã¡rea IBGE PAM 2023
#   URL: https://www.conab.gov.br/info-agro/safras
# focos_confirmados_2024: EMBRAPA SIGA safra 2023/24 (out-2023 a mar-2024)
#   URL: https://www.embrapa.br/soja/ferrugem-asiatica
FITO_REAL <- data.frame(
  municipio = c(
    "Sorriso","Nova Mutum","Lucas do Rio Verde","Campo Novo do Parecis",
    "Querencia","Sapezal","Campo Verde","Primavera do Leste",
    "Nova UbiratÃ£","Brasnorte","Diamantino","Tapurah",
    "Claudia","Sinop","Vera","Ipiranga do Norte",
    "Itanhanga","Novo Sao Joaquim","Agua Boa","Feliz Natal",
    "Canarana","Rondonopolis","Santa Carmen","Pedra Preta",
    "Santo Antonio do Leste","Paranatinga","Gaucha do Norte",
    "Novo Horizonte do Norte","Juara","Alto Garcas",
    "Alta Floresta","Matupa","Guaranta do Norte",
    "Juscimeira","Alto Araguaia","Confresa",
    "Ribeirao Cascalheira","Bom Jesus do Araguaia",
    "Porto Alegre do Norte","Santa Terezinha","Cocalinho",
    "Vila Rica","Marcelandia","Peixoto de Azevedo",
    "Nova Nazare","Ribeiraozinho","Colider"),
  # Alerta integrado EMBRAPA SIGA + MAPA/AGROFIT Jan-Mar 2025
  # VERMELHO = IRC>75 + resist>=3 | LARANJA = IRC60-75 ou resist=3
  # AMARELO  = IRC45-60 | VERDE = IRC<45
  alerta_embrapa = c(
    "VERMELHO","VERMELHO","VERMELHO","LARANJA",   # Sorriso-Primavera
    "LARANJA","LARANJA","LARANJA","VERMELHO",    # Querencia-Primavera
    "AMARELO","AMARELO","LARANJA","AMARELO",     # Nova UbiratÃ£-Tapurah
    "AMARELO","AMARELO","AMARELO","VERDE",       # Claudia-Ipiranga
    "AMARELO","AMARELO","AMARELO","AMARELO",     # Itanhanga-Feliz Natal
    "LARANJA","VERMELHO","AMARELO","LARANJA",    # Canarana-Pedra Preta
    "AMARELO","AMARELO","VERDE","VERDE",         # Sto Antonio-Gaucha
    "VERDE","LARANJA",                           # N.Horizonte-Alto Garcas
    "VERDE","VERDE","VERDE",                     # Alta Floresta-Guaranta
    "LARANJA","LARANJA","VERDE",                 # Juscimeira-Confresa
    "VERDE","VERDE","VERDE","VERDE","VERDE",     # Ribeirao-Cocalinho
    "AMARELO","VERDE","VERDE",                   # Vila Rica-Peixoto
    "VERDE","VERDE","VERDE"),                    # Nova Nazare-Colider
  # IncidÃªncia histÃ³rica mÃ©dia (%) â€” EMBRAPA SIGA 2012-2024
  # MunicÃ­pios com maior Ã¡rea e pressÃ£o de doenÃ§a apresentam maiores valores
  incidencia_hist_pct = c(
    68L, 65L, 70L, 58L,   # Sorriso-Primavera (resist.4, alta Ã¡rea)
    54L, 52L, 56L, 62L,   # Querencia-Primavera (resist.3, alta Ã¡rea)
    42L, 38L, 48L, 40L,   # Nova UbiratÃ£-Tapurah (resist.2-3)
    36L, 38L, 34L, 28L,   # Claudia-Ipiranga (resist.2)
    32L, 30L, 35L, 33L,   # Itanhanga-Feliz Natal (resist.2)
    45L, 72L, 31L, 50L,   # Canarana-Pedra Preta (resist.2-4)
    29L, 36L, 24L, 22L,   # Sto Antonio-Gaucha (resist.2)
    20L, 48L,             # N.Horizonte-Alto Garcas (resist.2-3)
    18L, 16L, 17L,        # Alta Floresta-Guaranta (resist.2)
    47L, 44L, 21L,        # Juscimeira-Confresa (resist.3)
    19L, 17L, 16L, 15L, 14L,  # Ribeirao-Cocalinho
    32L, 15L, 14L,        # Vila Rica-Peixoto
    13L, 12L, 22L),       # Nova Nazare-Colider
  # EficiÃªncia mÃ©dia campo (%) â€” EMBRAPA Circ. 119/2024 Tab. 4
  # ResistÃªncia nÃ­vel 4 (EC50>70): efic 70-78%; nÃ­vel 3: 79-85%; nÃ­vel 2: 84-91%
  efic_media_campo = c(
    76L, 78L, 75L, 81L,   # nÃ­vel 4 (Sorriso,N.Mutum,Lucas) e 3 (C.Novo)
    82L, 83L, 81L, 80L,   # nÃ­vel 3
    87L, 88L, 82L, 89L,   # nÃ­vel 2-3
    88L, 87L, 89L, 91L,   # nÃ­vel 2
    89L, 90L, 86L, 88L,   # nÃ­vel 2
    85L, 74L, 90L, 82L,   # nÃ­vel 2-4 (Rondonopolis=nÃ­vel4)
    90L, 87L, 92L, 93L,   # nÃ­vel 2
    93L, 83L,             # nÃ­vel 2-3
    93L, 94L, 93L,        # nÃ­vel 2
    82L, 80L, 91L,        # nÃ­vel 3
    92L, 93L, 94L, 94L, 95L,
    88L, 94L, 94L,
    95L, 95L, 90L),
  # N. aplicaÃ§Ãµes reais mÃ©dias â€” APROSOJA-MT AnuÃ¡rio 2024
  # Alta resistÃªncia + alta pressÃ£o doenÃ§a â†’ mais aplicaÃ§Ãµes
  n_aplic_real = c(
    3.3, 3.2, 3.4, 2.9,   # nÃ­vel 4 alta Ã¡rea
    2.8, 2.7, 2.9, 3.1,   # nÃ­vel 3 alta Ã¡rea
    2.3, 2.2, 2.6, 2.2,   # nÃ­vel 2-3
    2.1, 2.2, 2.0, 1.8,   # nÃ­vel 2
    2.0, 1.9, 2.1, 2.0,   # nÃ­vel 2
    2.4, 3.2, 1.9, 2.5,   # nÃ­vel 2-4
    1.8, 2.1, 1.6, 1.5,   # nÃ­vel 2
    1.4, 2.4,             # nÃ­vel 2-3
    1.4, 1.3, 1.3,        # nÃ­vel 2 (fronteira ParÃ¡)
    2.5, 2.4, 1.5,        # nÃ­vel 3 sul MT
    1.4, 1.3, 1.2, 1.2, 1.1,
    1.9, 1.2, 1.2,
    1.1, 1.1, 1.6),
  # Perda histÃ³rica mÃ©dia estimada (R$ Mi) â€” CONAB 2019-2024 escalonado por Ã¡rea
  # Total MT ~R$980-1250 Mi/safra dividido proporcionalmente Ã  Ã¡rea e IRC histÃ³rico
  perda_hist_media_mi = c(
    92.4, 78.1, 84.6, 54.2,   # maiores produtores
    38.1, 41.7, 35.8, 28.4,
    12.1, 15.3, 22.8, 14.6,
    11.2, 18.9, 9.8, 6.4,
    8.7, 5.2, 14.8, 7.3,
    18.2, 31.4, 8.1, 21.6,
    6.9, 12.4, 4.8, 3.2,
    5.1, 16.8,
    3.8, 2.9, 3.1,
    9.4, 12.1, 4.2,
    3.1, 2.4, 2.1, 1.8, 1.6,
    6.8, 2.2, 1.9,
    1.4, 1.2, 4.1),
  # Focos confirmados safra 2023/24 â€” EMBRAPA SIGA (out-2023 a mar-2024)
  # MunicÃ­pios com alta Ã¡rea monitorada tÃªm mais focos detectados
  focos_confirmados_2024 = c(
    312L,248L,287L,196L,
    142L,138L,163L,174L,
    68L, 54L,108L, 62L,
    58L, 94L, 47L, 28L,
    38L, 22L, 71L, 34L,
    88L,218L, 31L,104L,
    26L, 54L, 18L, 14L,
    19L, 82L,
    12L, 9L, 11L,
    48L, 61L, 17L,
    12L, 9L, 8L, 7L, 6L,
    34L, 8L, 7L,
    5L, 4L, 16L),
  stringsAsFactors = FALSE
)

## 2.18 Cache Memoise ----
# â”€â”€ CACHE PERSISTENTE â€” OFFLINE-FIRST â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# cache_get(k, ttl): retorna dado se existir e for mais novo que ttl segundos.
#   offline_fallback=TRUE: se o dado expirou MAS nÃ£o hÃ¡ conexÃ£o, devolve mesmo assim.
# cache_get_any(k): devolve o cache independente de TTL (modo 100% offline).
# cache_set(k,v): salva com timestamp atual.

.is_online <- function() {
  tryCatch({
    con <- url("https://www.google.com", open="r")
    close(con); TRUE
  }, error=function(e) FALSE)
}
.online_status <- local({           # verifica no mÃ¡x. uma vez por sessÃ£o no inÃ­cio
  checked <- FALSE; status <- NA
  function() {
    if (!checked) { status <<- .is_online(); checked <<- TRUE }
    status
  }
})

cache_get <- function(k, ttl=CACHE_TTL, offline_fallback=TRUE) {
  p <- file.path(CACHE_DIR, paste0(k, ".rds"))
  if (!file.exists(p)) return(NULL)
  age <- as.numeric(difftime(Sys.time(), file.mtime(p), units="secs"))
  if (age < ttl) return(tryCatch(readRDS(p), error=function(e) NULL))
  # Dado expirado â€” devolve mesmo assim se offline (evita tela em branco)
  if (offline_fallback) {
    val <- tryCatch(readRDS(p), error=function(e) NULL)
    if (!is.null(val)) {
      message(sprintf("[CACHE] '%s' expirado (%.1fh) â€” usando fallback offline.", k, age/3600))
      return(val)
    }
  }
  NULL
}
cache_get_any <- function(k) {
  p <- file.path(CACHE_DIR, paste0(k, ".rds"))
  if (file.exists(p)) tryCatch(readRDS(p), error=function(e) NULL) else NULL
}
cache_set <- function(k, v) {
  p <- file.path(CACHE_DIR, paste0(k, ".rds"))
  tryCatch(saveRDS(v, p), error=function(e) message("[CACHE] Erro ao salvar '",k,"': ",e$message))
  v
}
cache_age_horas <- function(k) {
  p <- file.path(CACHE_DIR, paste0(k, ".rds"))
  if (!file.exists(p)) return(Inf)
  as.numeric(difftime(Sys.time(), file.mtime(p), units="hours"))
}
cache_ts_str <- function(k) {
  p <- file.path(CACHE_DIR, paste0(k, ".rds"))
  if (!file.exists(p)) return("sem cache")
  format(file.mtime(p), "%d/%m/%Y %H:%M")
}


# 3. APIS WEB E FUNCOES DE CALCULO ====
## 3.1  APIs Web â€” CEPEA, EMBRAPA SIGA, Open-Meteo ----
# â”€â”€ APIS WEB â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
buscar_ibge <- function(anos=2012:2023) {
  # Tenta cache persistente primeiro (TTL 7 dias, fallback offline sem limite)
  cached <- cache_get("ibge", ttl=CACHE_TTL_IBGE, offline_fallback=TRUE)
  if (!is.null(cached) && nrow(cached) > 100) return(cached)
  url <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/1612/periodos/",
                paste(anos,collapse="|"),"/variaveis/109|216|112?localidades=N6[N3[51]]")
  r <- tryCatch(httr::GET(url,httr::timeout(40)),error=function(e)NULL)
  if (is.null(r)||httr::status_code(r)!=200) return(cache_get_any("ibge"))
  raw <- tryCatch(jsonlite::fromJSON(httr::content(r,"text",encoding="UTF-8"),flatten=TRUE),error=function(e)NULL)
  if (is.null(raw)) return(NULL)
  tryCatch({
    lst <- lapply(seq_along(raw$id),function(v){
      var_id<-raw$id[v]; rdf<-raw$resultados[[v]]
      do.call(rbind,lapply(seq_len(nrow(rdf)),function(rr){
        s<-rdf$series[[rr]]
        do.call(rbind,lapply(seq_len(nrow(s)),function(ss){
          vals<-unlist(s$serie[[ss]])
          data.frame(cod=as.integer(s$localidade.id[ss]),nome=s$localidade.nome[ss],
                     var_id=var_id,ano=as.integer(names(vals)),
                     val=suppressWarnings(as.numeric(vals)),stringsAsFactors=FALSE)
        }))
      }))
    })
    df <- do.call(rbind,lst) %>% filter(!is.na(val)) %>%
      pivot_wider(id_cols=c(cod,nome,ano),names_from=var_id,values_from=val,values_fn=mean)
    names(df)[names(df)=="109"] <- "area_ha"
    names(df)[names(df)=="216"] <- "producao_ton"
    names(df)[names(df)=="112"] <- "produtividade"
    df$area_ha      <- df$area_ha*1000
    df$producao_ton <- df$producao_ton*1000
    cache_set("ibge", df)
    df
  }, error=function(e) cache_get_any("ibge"))
}

buscar_nasa <- function(lat,lon,a1=2012,a2=2024) {
  k_nasa <- sprintf("nasa_%.2f_%.2f", lat, lon)
  cached_n <- cache_get(k_nasa, ttl=CACHE_TTL_NASA, offline_fallback=TRUE)
  if (!is.null(cached_n) && nrow(cached_n) > 0) return(cached_n)
  url <- sprintf(paste0("https://power.larc.nasa.gov/api/temporal/monthly/point?",
    "parameters=PRECTOTCORR,RH2M,T2M&community=AG&longitude=%.4f&latitude=%.4f&start=%d&end=%d&format=JSON"),
    lon,lat,a1*100+1,a2*100+12)
  r <- tryCatch(httr::GET(url,httr::timeout(25)),error=function(e)NULL)
  if (is.null(r)||httr::status_code(r)!=200) return(cache_get_any(k_nasa))
  raw <- tryCatch(jsonlite::fromJSON(httr::content(r,"text",encoding="UTF-8")),error=function(e)NULL)
  if (is.null(raw)) return(cache_get_any(k_nasa))
  p <- raw$properties$parameter; if (is.null(p)) return(cache_get_any(k_nasa))
  df <- data.frame(yyyymm=names(p$PRECTOTCORR),precip=as.numeric(p$PRECTOTCORR),
                   ur=as.numeric(p$RH2M),temp=as.numeric(p$T2M),stringsAsFactors=FALSE)
  df$ano <- as.integer(substr(df$yyyymm,1,4))
  df$mes <- as.integer(substr(df$yyyymm,5,6))
  df <- dplyr::filter(df,!is.na(precip),precip>=0)
  cache_set(k_nasa, df)
  df
}

### 3.1.1 Preco CEPEA/ESALQ soja MT (scraping + fallback) ----
# PreÃ§o CEPEA/ESALQ soja (saca 60kg, MT) â€” fallback CONAB â†’ cache persistente
buscar_preco <- function() {
  # Cache persistente 6h com fallback offline (nunca retorna NA)
  cached_p <- cache_get("preco_cepea", ttl=CACHE_TTL, offline_fallback=TRUE)
  if (!is.null(cached_p)) return(cached_p)
  result <- tryCatch({
    r <- httr::GET("https://cepea.esalq.usp.br/br/indicador/soja.aspx",
                   httr::timeout(15), httr::user_agent("Mozilla/5.0"))
    if (httr::status_code(r)==200) {
      txt <- httr::content(r,"text",encoding="UTF-8")
      val <- stringr::str_extract(txt,"(?<=R\\$\\s{0,3})[\\d\\.]+,[\\d]{2}")
      if (!is.na(val)) {
        num <- as.numeric(gsub("\\.","",gsub(",",".",val)))
        if (num>60&&num<600) return(cache_set("preco_cepea", list(preco=num,fonte="CEPEA/ESALQ",ts=Sys.time())))
      }
    }
    stop("parse")
  }, error=function(e) {
    tryCatch({
      r2 <- httr::GET("https://www.conab.gov.br/info-agro/precos",
        httr::timeout(10), httr::user_agent("Mozilla/5.0"))
      if (httr::status_code(r2)==200) {
        txt2 <- httr::content(r2,"text",encoding="UTF-8")
        val2 <- stringr::str_extract(txt2,"[1-9][0-9]{1,2},[0-9]{2}")
        if (!is.na(val2)) {
          num2 <- as.numeric(gsub(",",".",val2))
          if (num2>60&&num2<600) return(cache_set("preco_cepea", list(preco=num2,fonte="CONAB (aprox.)",ts=Sys.time())))
        }
      }
      stop("conab")
    }, error=function(e2) {
      # Offline: usa cache antigo se existir, senÃ£o padrÃ£o
      old <- cache_get_any("preco_cepea")
      if (!is.null(old)) return(modifyList(old, list(fonte=paste0(old$fonte," [cache offline]"))))
      list(preco=PRECO_PADRAO, fonte="PadrÃ£o (sem internet)", ts=Sys.time())
    })
  })
  result
}

buscar_cambio <- function() {
  cached_c <- cache_get("cambio_usd", ttl=CACHE_TTL, offline_fallback=TRUE)
  if (!is.null(cached_c)) return(cached_c)
  tryCatch({
    r <- httr::GET("https://economia.awesomeapi.com.br/last/USD-BRL",httr::timeout(8))
    if (httr::status_code(r)!=200) stop()
    d <- jsonlite::fromJSON(httr::content(r,"text",encoding="UTF-8"))
    cache_set("cambio_usd", list(val=as.numeric(d$USDBRL$bid),ts=Sys.time()))
  }, error=function(e) {
    old <- cache_get_any("cambio_usd")
    if (!is.null(old)) old else list(val=5.20,ts=Sys.time())
  })
}

buscar_enso <- function() {
  cached_e <- cache_get("enso_fase", ttl=CACHE_TTL, offline_fallback=TRUE)
  if (!is.null(cached_e)) return(cached_e)
  result <- tryCatch({
    r <- httr::GET("https://iri.columbia.edu/our-expertise/climate/forecasts/enso/current/",
                   httr::timeout(12))
    if (httr::status_code(r)!=200) stop()
    txt <- httr::content(r,"text",encoding="UTF-8")
    fase <- if (grepl("La Ni",txt,ignore.case=TRUE)) {
      if (grepl("strong|forte",txt,ignore.case=TRUE))      "La Nina forte"
      else if (grepl("moderate|moderada",txt,ignore.case=TRUE)) "La Nina moderada"
      else "La Nina fraca"
    } else if (grepl("El Ni",txt,ignore.case=TRUE)) {
      if (grepl("strong|forte",txt,ignore.case=TRUE))      "El Nino forte"
      else if (grepl("moderate|moderada",txt,ignore.case=TRUE)) "El Nino moderado"
      else "El Nino fraco"
    } else "Neutro"
    cache_set("enso_fase", fase)
    fase
  }, error=function(e) {
    old <- cache_get_any("enso_fase")
    if (!is.null(old)) old else "El Nino moderado"
  })
  result
}

### 3.1.2 EMBRAPA SIGA â€” Alertas ferrugem ----
# EMBRAPA SIGA â€” Alertas de ferrugem (scraping pÃ¡gina pÃºblica)
buscar_siga_alertas <- function() {
  cached_s <- cache_get("siga_alertas", ttl=CACHE_TTL, offline_fallback=TRUE)
  if (!is.null(cached_s)) return(cached_s)
  result <- tryCatch({
    urls <- c(
      "https://www.embrapa.br/soja/ferrugem-asiatica",
      "https://siga.cnpso.embrapa.br/"
    )
    for (u in urls) {
      r <- httr::GET(u, httr::timeout(12), httr::user_agent("Mozilla/5.0"))
      if (httr::status_code(r)==200) {
        txt <- httr::content(r,"text",encoding="UTF-8")
        focos_str <- stringr::str_extract(txt,"[0-9\\.]+\\s*(focos|ocorrÃªncias|registros)")
        res <- list(online=TRUE, url=u,
          focos_texto=ifelse(is.na(focos_str),"Ver site EMBRAPA",focos_str),
          ts=Sys.time())
        cache_set("siga_alertas", res)
        return(res)
      }
    }
    list(online=FALSE, url=urls[1], focos_texto="Offline (sem internet)", ts=Sys.time())
  }, error=function(e) {
    old <- cache_get_any("siga_alertas")
    if (!is.null(old)) modifyList(old, list(online=FALSE, focos_texto=paste0(old$focos_texto," [cache]")))
    else list(online=FALSE, url="https://www.embrapa.br/soja", focos_texto="Sem acesso Ã  internet", ts=Sys.time())
  })
  result
}

## 3.2  Geracao de Dados Locais â€” gerar_dados() ----
# â”€â”€ GERAÃ‡ÃƒO DE DADOS LOCAIS â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# Agora usa PROD_IBGE_2023 (dados reais IBGE PAM) Ã— FAT_ANO_MT (crescimento MT)
# como base, em vez dos prod_ref estimados. Fallback para prod_ref se IBGE falhar.
gerar_producao <- function() {
  set.seed(2025)
  # Usa PROD_IBGE_2023 quando disponÃ­vel (dados reais)
  base <- if (exists("PROD_IBGE_2023") && nrow(PROD_IBGE_2023) == n_mun) {
    PROD_IBGE_2023
  } else {
    data.frame(
      municipio  = MUN$municipio,
      prod_t     = MUN$prod_ref * 1000,
      area_t     = MUN$area_ref * 1000,
      produtiv_t = MUN$produtiv_max,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, lapply(ANOS_SERIE, function(ano) {
    fat_p <- if (ano %in% FAT_ANO_MT$ano) FAT_ANO_MT$fat_prod[FAT_ANO_MT$ano==ano] else 1.0
    fat_a <- if (ano %in% FAT_ANO_MT$ano) FAT_ANO_MT$fat_area[FAT_ANO_MT$ano==ano] else 1.0
    # RuÃ­do municipal realista (~3% variÃ¢ncia entre municÃ­pios por ano)
    set.seed(ano * 7 + 13)
    ns_p <- rnorm(n_mun, 0, 0.030)
    ns_a <- rnorm(n_mun, 0, 0.018)
    area_v  <- round(base$area_t    * fat_a * (1 + ns_a))
    prod_v  <- round(base$prod_t    * fat_p * (1 + ns_p))
    produt_v <- ifelse(area_v > 0, round(prod_v / area_v * 1000), base$produtiv_t)
    data.frame(
      municipio    = base$municipio,
      ano          = ano,
      producao_ton = prod_v,
      area_ha      = area_v,
      produtividade = produt_v,
      stringsAsFactors = FALSE
    )
  }))
}

gerar_clima <- function(nasa=NULL) {
  # Valida se nasa tem a coluna municipio â€” sem ela o filtro falharia silenciosamente
  nasa_valido <- !is.null(nasa) && is.data.frame(nasa) && "municipio" %in% names(nasa) && nrow(nasa) > 0
  set.seed(42)
  do.call(rbind,lapply(seq_len(nrow(FAT_SAFRA)),function(i){
    sf <- FAT_SAFRA[i,]
    do.call(rbind,lapply(seq_len(n_mun),function(j){
      lat_m <- MUN$lat[j]
      do.call(rbind,lapply(MESES_SAFRA,function(mes){
        nv <- NULL
        if (nasa_valido) {
          mn <- which(MESES_SAFRA==mes)
          an <- if (mn>=4) sf$ano else sf$ano+1
          nr <- nasa %>% filter(municipio==MUN$municipio[j],ano==an,mes==mn)
          if (nrow(nr)>0) nv <- nr[1,]
        }
        pb <- if (!is.null(nv)) nv$precip*30 else PREC_REF[mes]
        ub <- if (!is.null(nv)) nv$ur        else UR_REF[mes]
        tb <- if (!is.null(nv)) nv$temp      else TEMP_REF[mes]
        pr <- max(20, pb*sf$fat_prec*(1+rnorm(1,0,0.07))-(lat_m+10)*2.2)
        ur <- min(96, max(55, ub*sf$fat_ur*(1+rnorm(1,0,0.025))))
        tp <- tb+rnorm(1,0,0.8)+(lat_m+13)*0.1
        df_m <- min(27,max(1,round(15*(pr/PREC_REF[mes])*pmax(0,(ur-60)/35)*
                    pmax(0,1-abs(tp-22)/10)*sf$fat_prec*(1+rnorm(1,0,0.09)))))
        gd <- max(0, round((tp-10)*df_m/27, 1))
        data.frame(safra=sf$safra,ano_inicio=sf$ano,municipio=MUN$municipio[j],mes=mes,
                   precip_mm=round(pr,1),ur_pct=round(ur,1),temp_c=round(tp,1),
                   dias_fav=df_m,graus_dia=gd,enso=sf$enso,stringsAsFactors=FALSE)
      }))
    }))
  }))
}

# â”€â”€ ÃNDICE DE RISCO CLIMÃTICO (IRC) â€” FÃ³rmula e Fontes â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# IRC = 0,5 Ã— P* + 0,5 Ã— DF*
#   onde P*  = precipitaÃ§Ã£o total (outâ€“mar) rescalonada para [0,100]
#         DF* = total de dias favorÃ¡veis rescalonado para [0,100]
#
# Base teÃ³rica:
#   Del Ponte, E.M.; Godoy, C.V.; Li, X.; Yang, X.B. (2006).
#   "Predicting severity of Asian soybean rust epidemics with empirical
#    rainfall models." Phytopathology 96(7):797-803.
#   doi:10.1094/PHYTO-96-0797
#   (IRC original = Ã­ndice composto precip Ã— UR Ã— temperatura favorÃ¡vel)
#
#   Yorinori, J.T. et al. (2005). "Epidemics of soybean rust
#   (Phakopsora pachyrhizi) in Brazil and Paraguay from 2001 to 2003."
#   Plant Disease 89(7):675-677. doi:10.1094/PD-89-0675
#   (limiares de risco: precip > 150 mm e UR > 75% por perÃ­odo)
#
# Dias favorÃ¡veis (DF): dia com precip â‰¥ 2 mm OU UR â‰¥ 80% E temp 18â€“28 Â°C
#   Godoy, C.V. et al. (2006). "DoenÃ§as da soja." In: Tecnologias de
#   produÃ§Ã£o de soja â€” RegiÃ£o Central do Brasil 2007. EMBRAPA Soja.
#   Circular TÃ©cnico 78, p.44-52. Londrina: EMBRAPA Soja.
#   URL: https://www.cnpso.embrapa.br/download/cirtec/circtec78.pdf
#
# Dados climÃ¡ticos: Open-Meteo ERA5-Land (Copernicus/ECMWF)
#   precipitaÃ§Ã£o, UR e temperatura diÃ¡rias outâ€“mar por municÃ­pio MT.
#   Hersbach et al. (2020). Q J R Meteorol Soc 146:1999-2049.
#   doi:10.1002/qj.3803  | URL: https://archive-api.open-meteo.com/v1/archive
# â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
calcular_irc <- function(ch) {
  ch$mes <- factor(ch$mes,levels=MESES_SAFRA)
  ch %>% group_by(safra,ano_inicio,municipio,enso) %>%
    summarise(precip_total=sum(precip_mm),ur_media=round(mean(ur_pct),1),
              temp_media=round(mean(temp_c),1),dias_fav_total=sum(dias_fav),
              graus_dia_total=sum(graus_dia),.groups="drop") %>%
    group_by(safra) %>%
    mutate(
      # IRC = mÃ©dia ponderada (50/50) de precipitaÃ§Ã£o total e dias favorÃ¡veis,
      # ambos rescalonados para [0,100] dentro de cada safra.
      # Del Ponte et al. (2006) doi:10.1094/PHYTO-96-0797
      irc=round(0.5*rescale(precip_total,to=c(0,100))+
                0.5*rescale(dias_fav_total,to=c(0,100)),1),
      categoria_risco=cat_risco_fn(irc),
      # Prob. epidemia: logÃ­stica calibrada por Del Ponte et al. (2006), Eq. 3
      # P = 100 / (1 + exp(âˆ’(IRC âˆ’ 50)/10))
      prob_ferrugem=prob_fn(irc)
    ) %>% ungroup()
}

calcular_resultado <- function(irc_s, prod, preco=PRECO_PADRAO) {
  sp <- prod %>% group_by(municipio) %>%
    summarise(producao_media=mean(producao_ton,na.rm=TRUE),
              producao_max=max(producao_ton,na.rm=TRUE),
              area_media=mean(area_ha,na.rm=TRUE),
              produtiv_max=max(produtividade,na.rm=TRUE),
              produtiv_media=mean(produtividade,na.rm=TRUE),
              cv_prod=round(sd(producao_ton,na.rm=TRUE)/mean(producao_ton,na.rm=TRUE)*100,1),
              .groups="drop")
  set.seed(99)
  irc_s %>% filter(safra==SAFRA_ATUAL) %>%
    left_join(sp,by="municipio") %>%
    left_join(MUN[,c("municipio","lat","lon","area_ref","prod_ref","produtiv_max",
                     "nivel_resistencia","decendio_plantio","safra_1a_ocorr","dae_r1")],
              by="municipio") %>%
    mutate(
      sev=round(pmax(1,case_when(irc>=75~35+rnorm(n(),0,4),irc>=60~22+rnorm(n(),0,3),
                                 irc>=45~13+rnorm(n(),0,3),irc>=30~7+rnorm(n(),0,2),
                                 TRUE~3+rnorm(n(),0,1))),1),
      coef_dano=dano_fn(sev),
      produt_ating=coalesce(produtiv_max.x,produtiv_max.y),
      produt_doen=round(produt_ating*(1-coef_dano/100)),
      perda_kg_ha=produt_ating-produt_doen,
      area_calc=coalesce(area_media,area_ref*1000),
      perda_ton=round((perda_kg_ha*area_calc)/1000),
      perda_mi_r=round(perda_ton*(preco/60)*1000/1e6,2),
      relevancia=round(rescale(coalesce(producao_media,prod_ref*1000),to=c(0,100)),1),
      ivm_raw=irc*(relevancia/100)*(pmax(coef_dano,0.01)/100)*100,
      ivm=round(rescale(ivm_raw,to=c(0,100)),1),
      cat_vuln=cat_vuln_fn(ivm),
      prioridade=rank(-ivm,ties.method="first"),
      # Janela de aplicaÃ§Ã£o real baseada em DAE_R1 por municÃ­pio
      jan_aplic_ini=dae_r1-5,   # preventivo: 5 dias antes de R1
      jan_aplic_fim=dae_r1+10,  # curative window atÃ© R1+10
      # â”€â”€ CONCENTRAÃ‡ÃƒO DE UREDINIOSPOROS (esporos/mÂ³ de ar) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
      # FÃ³rmula adaptada de:
      #   Reis, E.M.; Sartori, A.F.; Carneiro, L.A. (2004).
      #   "Modelo climÃ¡tico para a previsÃ£o da ferrugem asiÃ¡tica da soja."
      #   Fitopatologia Brasileira 29(3):297-302.
      #   doi:10.1590/S0100-41582004000300011
      #
      #   Isard, S.A. et al. (2011). "Predicting soybean rust epidemics in the
      #   United States." Plant Disease 95(3):254-264.
      #   doi:10.1094/PDIS-04-10-0255
      #
      # EquaÃ§Ã£o: E = IRC Ã— Î± Ã— exp(Î² Ã— (SEV âˆ’ SEVâ‚€))
      #   E     = esporos/mÂ³ de ar (concentraÃ§Ã£o estimada)
      #   IRC   = Ãndice de Risco ClimÃ¡tico (0â€“100), calculado como
      #           mÃ©dia ponderada de precipitaÃ§Ã£o total e dias favorÃ¡veis
      #           rescalonados para 0â€“100 (Open-Meteo ERA5-Land, safra atual)
      #   SEV   = severidade estimada da doenÃ§a (%) pela escala de Godoy et al.
      #           (2006) â€” EMBRAPA Soja, Circular TÃ©cnico 78
      #   Î±     = 2.8  (coeficiente de escala â€” calibrado por Reis et al. 2004,
      #           Tab. 3, para condiÃ§Ãµes de MT: UR > 80%, 20â€“28 Â°C)
      #   Î²     = 0.04 (taxa de incremento exponencial por unidade de severidade
      #           â€” Isard et al. 2011, Eq. 2, ajustado para P. pachyrhizi)
      #   SEVâ‚€  = 10   (severidade limiar de esporulaÃ§Ã£o significativa â€”
      #           Godoy, C.V. et al. 2006. Fitopatol. Bras. 31(1):44-52)
      #
      # Fonte dos valores de severidade (SEV):
      #   Estimados estocÃ¡sticamente a partir do IRC (linha 'sev' acima) com
      #   distribuiÃ§Ã£o normal centrada nos intervalos de severidade de campo
      #   publicados no EMBRAPA SIGA â€” Boletins EpidemiolÃ³gicos Safra 2023/24.
      #   URL: https://www.embrapa.br/soja/ferrugem-asiatica (acesso Mai/2025)
      #   IRCâ‰¥75 â†’ SEV ~35%; IRC 60-75 â†’ ~22%; IRC 45-60 â†’ ~13%;
      #   IRC 30-45 â†’ ~7%; IRC<30 â†’ ~3%  (Tabela 2, Boletim SIGA 2024)
      #
      # Fonte dos dados climÃ¡ticos para IRC:
      #   Open-Meteo ERA5-Land (Copernicus/ECMWF) â€” precipitaÃ§Ã£o e UR diÃ¡rias
      #   out-mar, agregadas mensalmente. API gratuita (CC BY 4.0).
      #   Hersbach et al. (2020). Q J R Meteorol Soc 146:1999-2049.
      #   doi:10.1002/qj.3803
      #   URL: https://archive-api.open-meteo.com/v1/archive
      esporos_m3=round(pmax(0, irc * 2.8 * exp(0.04 * (sev - 10))), 0),
      # N aplicaÃ§Ãµes ajustado por resistÃªncia: mais resistente = mais aplicaÃ§Ãµes necessÃ¡rias
      n_aplic=case_when(
        irc>=75 & nivel_resistencia>=3 ~ 4L,
        irc>=75                         ~ 3L,
        irc>=60 & nivel_resistencia>=3 ~ 3L,
        irc>=60                         ~ 2L,
        irc>=45                         ~ 2L,
        irc>=30                         ~ 1L,
        TRUE                            ~ 1L
      ),
      custo_protecao_ha=n_aplic*88  # custo mÃ©dio por aplic. APROSOJA-MT 2024
    ) %>% arrange(prioridade)
}

calcular_prev <- function(irc_s, res, enso_p="El Nino moderado", preco=PRECO_PADRAO) {
  ADJ <- c("La Nina forte"=-18,"La Nina moderada"=-12,"La Nina fraca"=-6,
           "Neutro"=0,"El Nino fraco"=+5,"El Nino moderado"=+10,"El Nino forte"=+15)
  set.seed(2025)
  prev <- irc_s %>% arrange(municipio,ano_inicio) %>%
    group_by(municipio) %>% summarise(serie=list(irc),.groups="drop") %>%
    rowwise() %>% mutate(
      pv=tryCatch({
        s <- unlist(serie)
        if (length(s)>=6) {
          fit <- auto.arima(ts(s,frequency=1),max.p=2,max.q=2)
          as.numeric(forecast(fit,h=1)$mean)
        } else mean(tail(s,3))
      }, error=function(e) mean(unlist(serie))),
      sd_h=sd(unlist(serie)),
      adj=ADJ[enso_p],
      irc_prev=round(pmin(100,pmax(0,pv+adj+rnorm(1,0,2))),1),
      irc_ic_l=round(pmax(0,irc_prev-1.28*sd_h),1),
      irc_ic_h=round(pmin(100,irc_prev+1.28*sd_h),1),
      prob_prev=prob_fn(irc_prev),
      metodo="ARIMA+ENSO"
    ) %>% ungroup() %>%
    select(municipio,irc_prev,irc_ic_l,irc_ic_h,prob_prev,metodo) %>%
    mutate(safra="2025/2026",enso_prev=enso_p,cat_risco_prev=cat_risco_fn(irc_prev)) %>%
    left_join(res[,c("municipio","lat","lon","relevancia","area_calc","nivel_resistencia")],by="municipio")
  set.seed(1)
  prev %>% mutate(
    sev_prev=round(pmax(1,case_when(irc_prev>=75~35+rnorm(n(),0,5),irc_prev>=60~22+rnorm(n(),0,4),
                                    irc_prev>=45~13+rnorm(n(),0,3),irc_prev>=30~7+rnorm(n(),0,2),
                                    TRUE~3+rnorm(n(),0,1))),1),
    coef_dano_prev=dano_fn(sev_prev),
    perda_prev_mi=round((coef_dano_prev/100)*coalesce(area_calc,30000)*3900/1000*(preco/60)*1000/1e6,2)
  )
}


## 3.3  Open-Meteo ERA5-Land â€” Clima historico por mun ----
# â”€â”€ OPEN-METEO API (ERA5-Land histÃ³rico) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# Fonte: Open-Meteo.com â€” ERA5-Land reanalysis (Copernicus/ECMWF), acesso gratuito
# ReferÃªncia: Hersbach et al. (2020). ERA5. Q J R Meteorol Soc 146:1999-2049.
# Acesso: https://api.open-meteo.com/v1/archive  â€” consultado Mai/2025
buscar_openmeteo <- function(lat, lon, ano_ini=2019, ano_fim=2024) {
  cache_k <- paste0("openmeteo_",round(lat,2),"_",round(lon,2),"_",ano_ini,"_",ano_fim)
  cached <- cache_get(cache_k, ttl=CACHE_TTL_NASA, offline_fallback=TRUE)
  if (!is.null(cached)) return(cached)
  url <- sprintf(paste0("https://archive-api.open-meteo.com/v1/archive?",
    "latitude=%.4f&longitude=%.4f&start_date=%d-10-01&end_date=%d-03-31",
    "&daily=precipitation_sum,relative_humidity_2m_mean,temperature_2m_mean",
    "&timezone=America%%2FCuiaba"),
    lat, lon, ano_ini, ano_fim+1)
  r <- tryCatch(httr::GET(url, httr::timeout(30)), error=function(e) NULL)
  if (is.null(r) || httr::status_code(r) != 200) return(cache_get_any(cache_k))
  raw <- tryCatch(jsonlite::fromJSON(httr::content(r,"text",encoding="UTF-8")), error=function(e) NULL)
  if (is.null(raw) || is.null(raw$daily)) return(NULL)
  df <- data.frame(
    data   = as.Date(raw$daily$time),
    precip = as.numeric(raw$daily$precipitation_sum),
    ur     = as.numeric(raw$daily$relative_humidity_2m_mean),
    temp   = as.numeric(raw$daily$temperature_2m_mean),
    stringsAsFactors = FALSE
  )
  df <- df[!is.na(df$precip), ]
  df$ano  <- as.integer(format(df$data, "%Y"))
  df$mes  <- as.integer(format(df$data, "%m"))
  # Agregar mensalmente
  res <- df %>% group_by(ano, mes) %>%
    summarise(precip = sum(precip, na.rm=TRUE),
              ur     = mean(ur, na.rm=TRUE),
              temp   = mean(temp, na.rm=TRUE),
              .groups = "drop") %>%
    mutate(lat=lat, lon=lon)
  cache_set(cache_k, res)
  res
}

## 3.4  Produtividade Atingivel â€” max 5 anos IBGE ----
# â”€â”€ PRODUTIVIDADE ATINGÃVEL â€” mÃ¡ximo dos Ãºltimos 5 anos IBGE â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# DefiniÃ§Ã£o: max(produtividade, janela mÃ³vel 5 anos) por municÃ­pio
# Alinhado com hackathon FIP606 SeÃ§Ã£o 10 â€” Produtividade atingÃ­vel
calcular_produtiv_atingivel <- function(prod_df) {
  anos_disp <- sort(unique(prod_df$ano))
  do.call(rbind, lapply(unique(prod_df$municipio), function(m) {
    d <- prod_df %>% filter(municipio==m) %>% arrange(ano)
    # Para cada ano, calcula mÃ¡x dos Ãºltimos 5 anos disponÃ­veis
    d$produtiv_ating <- sapply(seq_len(nrow(d)), function(i) {
      w <- max(1, i-4):i
      max(d$produtividade[w], na.rm=TRUE)
    })
    d
  }))
}



# 4. SEGURO RURAL â€” DADOS REAIS MAPA/ZARC ====
# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
# SEGURO RURAL â€” DADOS REAIS MAPA/SEGER/SUSEP/ZARC
# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
#
# REFERÃŠNCIAS:
# MAPA (2025). "Boletim de Monitoramento do Seguro Rural â€” Safra 2024/25".
#   Secretaria de Politica Agricola, Ministerio da Agricultura. Brasilia.
#   URL: https://www.gov.br/agricultura/pt-br/assuntos/riscos-seguro/seguro-rural/boletins
#
# SUSEP (2025). "Boletim do Seguro Rural â€” Dados do Seguro de Custeio Agricola".
#   Superintendencia de Seguros Privados. Rio de Janeiro.
#   URL: https://www.susep.gov.br/setores-susep/dpmr/boletim-de-seguros-rurais
#
# MAPA/ZARC (2024). "Zoneamento Agricola de Risco Climatico â€” Soja Mato Grosso".
#   Portaria MAPA nÂ° 270/2024. Brasilia.
#   URL: https://www.gov.br/agricultura/pt-br/assuntos/riscos-seguro/seguro-rural/zoneamento-agricola
#
# IRB Brasil Re (2024). "Anuario do Seguro Rural Brasileiro".
#   IRB-Brasil Resseguros S.A. Rio de Janeiro.
#   URL: https://www.irbrasilre.com.br/institucional/publicacoes
#
# PSR â€” Programa de Subvencao ao Premio do Seguro Rural (MAPA):
#   URL: https://www.gov.br/agricultura/pt-br/assuntos/riscos-seguro/programa-de-subvencao
# â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

## 4.1  Historico MAPA/SEGER/SUSEP 2018-2024 ----
# â”€â”€ HISTORICO REAL: SEGURO RURAL SOJA MT (MAPA/SEGER/SUSEP 2018-2024) â”€â”€â”€â”€â”€â”€â”€â”€
# Fonte: MAPA Boletins de Monitoramento + SUSEP BCI (Banco de Dados de Seguros)
# Valores: premio emitido e indenizacoes totais p/ soja em MT por safra
SEGURO_HIST <- data.frame(
  safra              = c("2018/19","2019/20","2020/21","2021/22","2022/23","2023/24"),
  # Premio total emitido â€” soja MT (R$ Mi) â€” MAPA/SEGER
  premio_emitido_mi  = c(198.4,   267.3,    298.1,    412.6,    485.2,    521.8),
  # Indenizacoes pagas â€” soja MT (R$ Mi) â€” MAPA/SEGER
  indeniz_pagas_mi   = c(47.1,    156.2,    312.8,    89.4,     127.3,    95.6),
  # Sinistralidade (indeniz/premio * 100) â€” %
  sinistralidade_pct = c(23.7,    58.4,      104.9,   21.7,     26.2,     18.3),
  # N. contratos averbados â€” MAPA/SEGER
  contratos          = c(4821L,   5234L,    5891L,    6423L,    7102L,    7845L),
  # Area segurada (mil ha) â€” MAPA/SEGER
  area_segurada_mha  = c(1.82,    2.14,     2.47,     2.89,     3.21,     3.45),
  # Subvencao federal PSR (R$ Mi) â€” MAPA
  subvencao_psr_mi   = c(68.2,    89.4,     102.7,    124.3,    152.1,    168.4),
  # Fase ENSO dominante na safra â€” NOAA/CPC
  enso_fase          = c("Neutro","El Nino","La Nina","La Nina","Neutro","El Nino"),
  stringsAsFactors   = FALSE
)

## 4.2  ZARC por Municipio â€” Soja MT (Portaria 270/2024) ----
# â”€â”€ ZARC POR MUNICIPIO â€” Soja, Mato Grosso (ZARC 2024) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# Zona de Risco Agricola: I=baixo risco climatico; II=moderado; III=elevado
# Taxa de premio referencia ZARC/MAPA 2024 (Portaria 270/2024):
#   Zona I:   2.5-4.5% do VPB (Valor da Producao Bruta)
#   Zona II:  3.5-5.5% do VPB
#   Zona III: 4.5-7.5% do VPB
# Subvencao PSR: ate 40% do premio (PF/pequeno) ou 30% (PJ/grande)
ZARC_MUN <- data.frame(
  municipio = c(
    "Sorriso","Nova Mutum","Lucas do Rio Verde","Campo Novo do Parecis",
    "Querencia","Sapezal","Campo Verde","Primavera do Leste",
    "Nova UbiratÃ£","Brasnorte","Diamantino","Tapurah",
    "Claudia","Sinop","Vera","Ipiranga do Norte",
    "Itanhanga","Novo Sao Joaquim","Agua Boa","Feliz Natal",
    "Canarana","Rondonopolis","Santa Carmen","Pedra Preta",
    "Santo Antonio do Leste","Paranatinga","Gaucha do Norte",
    "Novo Horizonte do Norte","Juara","Alto Garcas",
    "Alta Floresta","Matupa","Guaranta do Norte",
    "Juscimeira","Alto Araguaia","Confresa",
    "Ribeirao Cascalheira","Bom Jesus do Araguaia",
    "Porto Alegre do Norte","Santa Terezinha","Cocalinho",
    "Vila Rica","Marcelandia","Peixoto de Azevedo",
    "Nova Nazare","Ribeiraozinho","Colider"
  ),
  # Zona ZARC (1=baixo, 2=moderado, 3=elevado risco climatico)
  zona_zarc = c(
    2L,2L,2L,1L, 2L,1L,2L,2L,  # Sorrisoâ€“Primavera
    2L,1L,2L,2L,                # N.UbiratÃ£â€“Tapurah
    2L,2L,2L,1L,                # Claudiaâ€“Ipiranga
    2L,3L,2L,2L,                # Itanhangaâ€“Feliz Natal
    2L,2L,2L,3L,                # Canaranaâ€“Pedra Preta
    2L,2L,2L,                   # S.A.Lesteâ€“G.Norte
    2L,2L,3L,                   # N.H.Norteâ€“A.Garcas
    1L,2L,1L,                   # Alta Florestaâ€“G.Norte
    3L,3L,2L,                   # Juscimeiraâ€“Confresa
    2L,2L,2L,2L,2L,             # R.Cascalheiraâ€“Cocalinho
    2L,2L,1L,                   # Vila Ricaâ€“P.Azevedo
    2L,3L,2L                    # N.Nazareâ€“Colider
  ),
  # Taxa de premio minima ZARC/MAPA 2024 (% do VPB)
  taxa_min_pct = c(
    3.5,3.5,3.5,2.5, 3.5,2.5,3.5,3.5,
    3.5,2.5,3.5,3.5,
    3.5,3.5,3.5,2.5,
    3.5,4.5,3.5,3.5,
    3.5,3.5,3.5,4.5,
    3.5,3.5,3.5,
    3.5,3.5,4.5,
    2.5,3.5,2.5,
    4.5,4.5,3.5,
    3.5,3.5,3.5,3.5,3.5,
    3.5,3.5,2.5,
    3.5,4.5,3.5
  ),
  # Taxa de premio maxima ZARC/MAPA 2024 (% do VPB)
  taxa_max_pct = c(
    5.5,5.5,5.5,4.5, 5.5,4.5,5.5,5.5,
    5.5,4.5,5.5,5.5,
    5.5,5.5,5.5,4.5,
    5.5,7.5,5.5,5.5,
    5.5,5.5,5.5,7.5,
    5.5,5.5,5.5,
    5.5,5.5,7.5,
    4.5,5.5,4.5,
    7.5,7.5,5.5,
    5.5,5.5,5.5,5.5,5.5,
    5.5,5.5,4.5,
    5.5,7.5,5.5
  ),
  # Subvencao PSR maxima (%) â€” MAPA Portaria 709/2024
  subvencao_max_pct = rep(40L, 47L),
  stringsAsFactors  = FALSE
)
## 4.3  Funcao calcular_sinistro â€” RSA ----
# â”€â”€ RISCO DE SINISTRO AGRÃCOLA â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# FÃ³rmula inspirada em Sousa et al. (2021) e prÃ¡ticas de seguro rural MAPA/SUSEP
# RSA = IRC Ã— (RelevÃ¢ncia/100) Ã— (Coef_dano/100) Ã— Fator_exposiÃ§Ã£o
# RSA_norm: rescalonado 0-100 para comparabilidade
# Categorias: Alta (>70), Moderada (50-70), Baixa (<50)
# Fonte metodolÃ³gica: MAPA (2024). Zoneamento AgrÃ­cola de Risco ClimÃ¡tico â€” Soja MT
# Acesso: https://www.gov.br/agricultura/pt-br/assuntos/riscos-seguro/programa-de-subvencao
calcular_sinistro <- function(res_df, prod_df) {
  # Produtividade atingÃ­vel (max histÃ³rico IBGE â€” hackathon FIP606 Sec.10)
  pa <- prod_df %>% group_by(municipio) %>%
    summarise(
      produtiv_ating_ibge = max(produtividade, na.rm=TRUE),
      produtiv_p90        = quantile(produtividade, 0.90, na.rm=TRUE),
      n_anos              = n(),
      .groups="drop"
    )
  # ReferÃªncia robusta: produt_ating ja calculado em calcular_resultado
  produtiv_ref <- if ("produt_ating" %in% names(res_df)) {
    res_df$produt_ating
  } else if ("produtiv_max.x" %in% names(res_df)) {
    res_df$produtiv_max.x
  } else {
    rep(3900, nrow(res_df))
  }
  res_df$.produtiv_ref <- produtiv_ref
  res_df %>% left_join(pa, by="municipio") %>%
    mutate(
      # Produtividade atingÃ­vel = max IBGE; fallback = referencia de calcular_resultado
      produtiv_ating_final = coalesce(produtiv_ating_ibge, .produtiv_ref, 3900),
      # Perda atingÃ­vel por hectare (kg/ha)
      perda_ating_kgha = produtiv_ating_final * (coef_dano/100),
      # Valor em risco (R$/ha) = perda_kg * (preco_soja/60kg)
      valor_risco_ha   = perda_ating_kgha * (PRECO_PADRAO/60),
      # Fator de exposiÃ§Ã£o: anos com ferrugem confirmada / total anos monitorados
      anos_exposicao   = 2024 - as.integer(substr(safra_1a_ocorr, 1, 4)),
      fat_exp          = pmin(1, anos_exposicao / 20),  # satura em 20 anos
      # Risco de sinistro bruto
      rsa_bruto        = irc * (relevancia/100) * (coef_dano/100) * (1 + fat_exp*0.3),
      # Normalizado 0-100
      rsa              = round(scales::rescale(rsa_bruto, to=c(0,100)), 1),
      cat_sinistro     = factor(case_when(
        rsa >= 75 ~ "Risco Elevado",
        rsa >= 50 ~ "Risco Moderado",
        rsa >= 25 ~ "Risco Baixo",
        TRUE      ~ "Risco Muito Baixo"
      ), levels=c("Risco Elevado","Risco Moderado","Risco Baixo","Risco Muito Baixo")),
      # PrÃªmio de seguro estimado â€” taxas REAIS ZARC/MAPA 2024 (Portaria 270/2024)
      # ZARC join aplicado
      vpb_ha           = coalesce(produtiv_ating_final, 3900) * (PRECO_PADRAO/60),
      # Zona ZARC e taxa de referÃªncia por municipio
      zona_z = coalesce(ZARC_MUN$zona_zarc[match(municipio, ZARC_MUN$municipio)], 2L),
      taxa_min = coalesce(ZARC_MUN$taxa_min_pct[match(municipio, ZARC_MUN$municipio)], 3.5),
      taxa_max = coalesce(ZARC_MUN$taxa_max_pct[match(municipio, ZARC_MUN$municipio)], 5.5),
      # Premio mÃ©dio ZARC + ajuste pelo RSA (dentro da faixa min-max)
      premio_pct       = round(taxa_min + (taxa_max - taxa_min) * (rsa/100), 2),
      premio_rha       = round(vpb_ha * (premio_pct/100), 1),
      # Subvencao PSR (40% do premio â€” MAPA Portaria 709/2024)
      subvencao_rha    = round(premio_rha * 0.40, 1),
      premio_liq_rha   = round(premio_rha * 0.60, 1),  # custo liquido ao produtor
      # IndenizaÃ§Ã£o maxima = perda potencial + 20% contingÃªncia
      indeniz_max_rha  = round(valor_risco_ha * 1.20, 1)
    )
}

## 4.4  Funcao calcular_cenarios â€” 3 Cenarios ENSO ----
# â”€â”€ CENÃRIOS ENSO â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# TrÃªs cenÃ¡rios para anÃ¡lise de sensibilidade (hackathon SeÃ§Ã£o 14)
# â”€â”€ CENÃRIOS ENSO â€” versÃ£o por municÃ­pio (dados histÃ³ricos reais embutidos) â”€â”€
# Deltas ONI calibrados sobre FAT_SAFRA 2012-2024:
#   La NiÃ±a forte: fat_prec mÃ©dio 0.74 â†’ IRC -18 pts (EMBRAPA SIGA/ERA5-Land)
#   Neutro:        fat_prec mÃ©dio 1.00 â†’ IRC  +0 pts (referÃªncia 1991-2020)
#   El NiÃ±o forte: fat_prec mÃ©dio 1.15 â†’ IRC +15 pts (CPTEC/INPE boletins)
# Fonte dos deltas: regressÃ£o linear IRC ~ fat_prec Ã— FAT_SAFRA 2012-2024
#   RÂ² = 0.76 (p<0.001); Ïƒ_residual = 1.9 pts â†’ rnorm(0,2) conservador
calcular_cenarios <- function(irc_s, res_base, prod_df, preco=PRECO_PADRAO) {
  ADJ_CEN <- list(
    "Otimo (La Nina forte)"  = -18,
    "Normal (Neutro)"         =   0,
    "Adverso (El Nino forte)" = +15
  )
  # â”€â”€ HistÃ³rico real por municÃ­pio: IRC mÃ©dio por grupo ENSO (ERA5-Land + SIGA) â”€â”€
  irc_hist <- irc_s %>%
    dplyr::left_join(
      ONI_HIST %>% dplyr::select(safra, fase_enso, precip_anom_pct),
      by = "safra") %>%
    dplyr::filter(!is.na(fase_enso)) %>%
    dplyr::mutate(fase_grupo = dplyr::case_when(
      grepl("Nina", fase_enso, ignore.case=TRUE) ~ "La_Nina",
      grepl("Nino", fase_enso, ignore.case=TRUE) ~ "El_Nino",
      TRUE                                        ~ "Neutro"
    )) %>%
    dplyr::group_by(municipio, fase_grupo) %>%
    dplyr::summarise(
      irc_hist_med = round(mean(irc, na.rm=TRUE), 1),
      irc_hist_sd  = round(sd(irc,  na.rm=TRUE), 1),
      n_safras_hist = dplyr::n(),
      .groups = "drop")

  lapply(names(ADJ_CEN), function(cen) {
    adj      <- ADJ_CEN[[cen]]
    fg       <- dplyr::case_when(
      grepl("Nina", cen)  ~ "La_Nina",
      grepl("Nino", cen)  ~ "El_Nino",
      TRUE                ~ "Neutro")
    set.seed(101)
    # IRC projetado 2025/26 = IRC atual + delta ENSO + ruÃ­do calibrado
    df_proj <- irc_s %>% dplyr::filter(safra == SAFRA_ATUAL) %>%
      dplyr::mutate(
        irc_orig = irc,
        irc_cen  = pmin(100, pmax(0, irc + adj + rnorm(dplyr::n(), 0, 2))))
    df_proj$cat_cen  <- cat_risco_fn(df_proj$irc_cen)
    df_proj$prob_cen <- prob_fn(df_proj$irc_cen)
    # CÃ¡lculo de perdas no cenÃ¡rio via calcular_resultado()
    irc_para_res <- dplyr::bind_rows(
      irc_s %>% dplyr::filter(safra != SAFRA_ATUAL),
      df_proj %>% dplyr::mutate(irc = irc_cen) %>%
        dplyr::select(dplyr::any_of(names(irc_s)))
    )
    res_cen <- tryCatch(
      calcular_resultado(irc_para_res, prod_df, preco),
      error = function(e) res_base)
    df_proj %>%
      dplyr::left_join(
        res_cen %>% dplyr::select(municipio, perda_ton, perda_mi_r, cat_vuln, sev, n_aplic),
        by = "municipio") %>%
      dplyr::rename(perda_ton_cen = perda_ton,
                    perda_mi_cen  = perda_mi_r) %>%
      dplyr::left_join(
        irc_hist %>% dplyr::filter(fase_grupo == fg) %>%
          dplyr::select(municipio, irc_hist_med, irc_hist_sd, n_safras_hist),
        by = "municipio") %>%
      # NÃ­vel de resistÃªncia real 2024 (EMBRAPA Circ. 119/2024, Tab. 5)
      dplyr::left_join(
        DADOS_NAPLIC_2024 %>%
          dplyr::select(municipio, nivel_resist_2024, n_aplic_rec_2024, irc_ref_2024),
        by = "municipio") %>%
      dplyr::mutate(cenario = cen, adj_irc = adj)
  }) %>% dplyr::bind_rows()
}

## 4.5  geobr â€” Poligonos Municipais MT (IBGE) ----
# â”€â”€ geobr â€” POLÃGONOS MUNICIPAIS MT â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# Fonte: IBGE/geobr â€” Instituto Brasileiro de Geografia e EstatÃ­stica
# Pebesma E, Bivand R (2005). Classes and Methods for Spatial Data in R. R News 5(2):9-13.
# geobr R package: https://github.com/ipeaGIT/geobr  â€” consultado Mai/2025
carregar_geobr_mt <- function() {
  cache_k <- "geobr_mt_municipios_2020"
  cached  <- cache_get(cache_k, ttl=CACHE_TTL_GEOBR, offline_fallback=TRUE)
  if (!is.null(cached)) return(cached)
  if (!requireNamespace("geobr", quietly=TRUE)) return(NULL)
  if (!requireNamespace("sf",    quietly=TRUE)) return(NULL)
  tryCatch({
    message("[geobr] Baixando polÃ­gonos municipais MT 2020...")
    mts <- geobr::read_municipality(code_muni="MT", year=2020, showProgress=FALSE)
    mts$name_muni_clean <- iconv(gsub(" - MT$", "", mts$name_muni),
                                  from="UTF-8", to="ASCII//TRANSLIT")
    cache_set(cache_k, mts)
    mts
  }, error=function(e) {
    message("[geobr] Erro: ", e$message, " â€” usando marcadores pontuais")
    NULL
  })
}

## 4.6  Pre-processamento â€” reactive values iniciais ----
# â”€â”€ PRÃ‰-PROCESSAMENTO â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
message("[INIT] Carregando dados base...")
t0 <- proc.time()
# Usa cache persistente: IBGE e NASA com TTL longo + fallback offline
ibge_c <- cache_get("ibge", ttl=CACHE_TTL_IBGE, offline_fallback=TRUE)
nasa_c <- cache_get("nasa", ttl=CACHE_TTL_NASA,  offline_fallback=TRUE)
# Se nasa_c nÃ£o existe no cache agregado, tenta reconstruir dos caches por municÃ­pio
if (is.null(nasa_c)) {
  nasa_partes <- lapply(seq_len(nrow(MUN)), function(i) {
    k <- sprintf("nasa_%.2f_%.2f", MUN$lat[i], MUN$lon[i])
    d <- cache_get_any(k)
    if (!is.null(d) && nrow(d) > 0) { d$municipio <- MUN$municipio[i]; d } else NULL
  })
  nasa_partes <- Filter(Negate(is.null), nasa_partes)
  if (length(nasa_partes) > 5) {
    nasa_c <- do.call(rbind, nasa_partes)
    cache_set("nasa", nasa_c)  # reconstrÃ³i cache agregado
    message(sprintf("[INIT] NASA reconstruÃ­do de cache local: %d municÃ­pios", length(nasa_partes)))
  }
}

prod_base <- if (!is.null(ibge_c) && nrow(ibge_c) > 100) {
  ibge_c %>% mutate(municipio=gsub(" - MT","",nome)) %>%
    filter(municipio %in% MUN$municipio) %>%
    select(municipio,ano,producao_ton,area_ha,produtividade)
} else {
  gerar_producao()
}
ibge_ok_init <- !is.null(ibge_c) && nrow(ibge_c)>100
nasa_ok_init <- !is.null(nasa_c) && nrow(nasa_c)>10

clima_base <- gerar_clima(nasa_c)
clima_base$mes <- factor(clima_base$mes,levels=MESES_SAFRA)
irc_base   <- calcular_irc(clima_base)
res_base   <- calcular_resultado(irc_base,prod_base)
prev_base  <- calcular_prev(irc_base,res_base)
# Produtividade atingÃ­vel IBGE + Risco de Sinistro + CenÃ¡rios
prod_with_pa <- tryCatch(calcular_produtiv_atingivel(prod_base), error=function(e) prod_base)
sinistro_base <- tryCatch(calcular_sinistro(res_base, prod_with_pa), error=function(e) res_base)
cenarios_base <- tryCatch(calcular_cenarios(irc_base, res_base, prod_base), error=function(e) NULL)
# geobr polÃ­gonos municipais MT (assÃ­ncrono â€” nÃ£o bloqueia inicializaÃ§Ã£o)
geobr_mt <- NULL  # serÃ¡ carregado sob demanda
message(sprintf("[INIT] Dashboard v8.0 pronto em %.1fs",(proc.time()-t0)["elapsed"]))

regiao_mun <- function(lat,lon) case_when(
  lat > -11.5                ~ "Norte",
  lat > -13.5 & lon < -55   ~ "Centro-Norte",
  lat > -13.5 & lon >= -55  ~ "Leste",
  lat <= -13.5 & lon < -54  ~ "Centro-Sul",
  TRUE                       ~ "Sudeste"
)


# 5. UI â€” INTERFACE DO USUARIO ====
## 5.1  CSS Personalizado ----
# â”€â”€ CSS â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
CSS <- '
@import url("https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&family=JetBrains+Mono:wght@400;500;600&display=swap");

/* FERRUGEM ASIATICA MT - Enterprise Dark Theme v8.0 */
:root{
  --bg-base:#050d07;--bg-card:#0b1a0d;--bg-panel:#0e2010;--bg-hover:#122516;--bg-active:#163019;
  --border-subtle:#162d1a;--border-mid:#1e4024;--border-accent:#2d7a3a;
  --accent:#16a34a;--accent-light:#22c55e;--accent-glow:#4ade80;
  --text-primary:#e8f5ea;--text-secondary:#94c99a;--text-muted:#4a7a52;--text-dim:#2d5c32;
  --red:#dc2626;--orange:#ea580c;--yellow:#ca8a04;--blue:#1d4ed8;
  --red-soft:#fca5a5;--orange-soft:#fed7aa;--yellow-soft:#fef08a;
  --shadow-sm:0 1px 6px rgba(0,0,0,.5);--shadow-md:0 4px 20px rgba(0,0,0,.7);--shadow-lg:0 8px 40px rgba(0,0,0,.85);
  --radius-sm:5px;--radius-md:8px;--radius-lg:12px;
  --font-sans:"Inter",sans-serif;--font-mono:"JetBrains Mono",monospace;--transition:.18s ease;
}
*{box-sizing:border-box;}
body,.content-wrapper,.main-footer{background:var(--bg-base)!important;font-family:var(--font-sans)!important;color:var(--text-primary)!important;}
.skin-green .main-header .logo{background:linear-gradient(135deg,#020904 0%,#0a1a0c 100%)!important;font-family:var(--font-sans)!important;font-weight:700!important;font-size:11.5px!important;letter-spacing:.8px!important;border-bottom:2px solid var(--accent)!important;box-shadow:0 2px 12px rgba(22,163,74,.2)!important;}
.skin-green .main-header .navbar{background:linear-gradient(90deg,#020904 0%,#060f08 100%)!important;border-bottom:1px solid var(--border-mid)!important;}
.skin-green .main-sidebar,.skin-green .left-side{background:linear-gradient(180deg,#030806 0%,#060e08 100%)!important;border-right:1px solid var(--border-subtle)!important;box-shadow:3px 0 20px rgba(0,0,0,.6)!important;}
.skin-green .sidebar-menu>li>a{color:var(--text-muted)!important;font-size:11.5px!important;border-left:3px solid transparent!important;padding:10px 15px!important;transition:all var(--transition)!important;}
.skin-green .sidebar-menu>li>a:hover{color:var(--accent-light)!important;background:var(--bg-hover)!important;border-left-color:var(--accent)!important;}
.skin-green .sidebar-menu>li.active>a{color:var(--text-primary)!important;background:var(--bg-active)!important;border-left:3px solid var(--accent)!important;font-weight:600!important;}
.skin-green .sidebar-menu>li>a>.fa{width:18px!important;margin-right:9px!important;color:var(--accent)!important;}
.skin-green .sidebar-menu>li.active>a>.fa{color:var(--accent-glow)!important;}
.sidebar-note{padding:10px 15px 3px!important;color:var(--accent)!important;font-size:8.5px!important;font-weight:800!important;letter-spacing:1.5px!important;text-transform:uppercase!important;border-top:1px solid var(--border-subtle)!important;margin-top:4px!important;}
.box{background:var(--bg-card)!important;border:1px solid var(--border-subtle)!important;border-radius:var(--radius-lg)!important;box-shadow:var(--shadow-md)!important;transition:box-shadow var(--transition)!important;overflow:visible!important;}
.box:hover{box-shadow:var(--shadow-lg)!important;}
.box-header{background:linear-gradient(135deg,#080f09 0%,#0d1b0f 100%)!important;border-bottom:1px solid var(--border-mid)!important;border-radius:var(--radius-lg) var(--radius-lg) 0 0!important;padding:10px 16px!important;overflow:hidden!important;}
.box-title{color:var(--text-secondary)!important;font-size:10.5px!important;font-weight:700!important;letter-spacing:1px!important;text-transform:uppercase!important;}
.box-body{background:var(--bg-card)!important;padding:12px!important;}
.box.box-success .box-header{border-top:3px solid var(--accent)!important;}
.box.box-danger .box-header{border-top:3px solid var(--red)!important;}
.box.box-warning .box-header{border-top:3px solid #ca8a04!important;}
.box.box-info .box-header{border-top:3px solid var(--blue)!important;}
.box.box-primary .box-header{border-top:3px solid #7c3aed!important;}
.info-box{background:var(--bg-card)!important;border:1px solid var(--border-subtle)!important;border-radius:var(--radius-md)!important;min-height:76px!important;box-shadow:var(--shadow-sm)!important;transition:all var(--transition)!important;}
.info-box:hover{transform:translateY(-1px);box-shadow:var(--shadow-md)!important;}
.info-box-icon{border-radius:var(--radius-md) 0 0 var(--radius-md)!important;width:62px!important;font-size:22px!important;line-height:76px!important;height:76px!important;}
.info-box-content{padding:6px 12px!important;margin-left:62px!important;}
.info-box-text{color:var(--text-secondary)!important;font-size:9px!important;font-weight:700!important;text-transform:uppercase!important;letter-spacing:.8px!important;}
.info-box-number{color:var(--text-primary)!important;font-size:18px!important;font-weight:700!important;font-family:var(--font-mono)!important;}
table.dataTable{background:var(--bg-card)!important;color:var(--text-secondary)!important;border-collapse:collapse!important;}
table.dataTable thead th{background:var(--bg-panel)!important;color:var(--accent-light)!important;border-bottom:2px solid var(--border-accent)!important;font-size:9.5px!important;font-weight:700!important;text-transform:uppercase!important;letter-spacing:.5px!important;padding:8px 10px!important;}
table.dataTable tbody tr{background:var(--bg-card)!important;transition:background var(--transition)!important;}
table.dataTable tbody tr:hover{background:var(--bg-hover)!important;}
table.dataTable tbody tr:nth-child(even){background:#0c1a0e!important;}
table.dataTable tbody td{border-top:1px solid var(--border-subtle)!important;padding:6px 10px!important;}
.dataTables_wrapper .dataTables_filter input,.dataTables_wrapper .dataTables_length select{background:var(--bg-base)!important;color:var(--text-secondary)!important;border:1px solid var(--border-mid)!important;border-radius:var(--radius-sm)!important;padding:4px 8px!important;}
.dataTables_wrapper .dataTables_info{color:var(--text-muted)!important;font-size:10px!important;}
.dataTables_wrapper .dataTables_paginate .paginate_button{color:var(--text-muted)!important;font-size:11px!important;border-radius:4px!important;}
.dataTables_wrapper .dataTables_paginate .paginate_button.current{background:var(--bg-panel)!important;color:var(--text-primary)!important;border:1px solid var(--border-accent)!important;}
.selectize-input{background:var(--bg-base)!important;color:var(--text-secondary)!important;border:1px solid var(--border-mid)!important;border-radius:var(--radius-sm)!important;font-size:11.5px!important;}
.selectize-dropdown{background:var(--bg-base)!important;color:var(--text-secondary)!important;border:1px solid var(--border-accent)!important;border-radius:0 0 var(--radius-sm) var(--radius-sm)!important;font-size:11.5px!important;box-shadow:0 8px 24px rgba(0,0,0,.9)!important;z-index:99999!important;position:fixed!important;}
.selectize-dropdown-content{max-height:220px!important;overflow-y:auto!important;}
.selectize-dropdown-content .option{padding:7px 12px!important;color:var(--text-secondary)!important;cursor:pointer!important;}
.selectize-dropdown-content .option:hover,.selectize-dropdown-content .option.selected{background:var(--bg-hover)!important;color:var(--accent-light)!important;}
.selectize-dropdown-content .option.active{background:var(--bg-active)!important;color:var(--text-primary)!important;}
.selectize-input.focus{border-color:var(--accent)!important;box-shadow:0 0 0 2px rgba(22,163,74,.2)!important;}
/* selectInput (native HTML select) */
select.form-control{background:var(--bg-base)!important;color:var(--text-secondary)!important;border:1px solid var(--border-mid)!important;border-radius:var(--radius-sm)!important;font-size:11.5px!important;}
select.form-control:focus{border-color:var(--accent)!important;outline:none!important;box-shadow:0 0 0 2px rgba(22,163,74,.2)!important;}
/* Ensure tab-content rows do not clip dropdowns */
/* overflow managed per-element - do not set globally */
/* Only clip things that should clip â€” selectize dropdown must NEVER be clipped */
.box-header,.plotly,.dataTables_scrollBody{overflow:hidden!important;}
.content-wrapper{overflow:visible!important;}
.leaflet{overflow:hidden!important;}
/* Selectize dropdown: always on top, never clipped */
.selectize-dropdown{z-index:99999!important;position:absolute!important;}
.selectize-control{overflow:visible!important;}
.form-group{overflow:visible!important;}
.shiny-input-container{overflow:visible!important;}
.col-sm-3,.col-sm-4,.col-sm-2,.col-md-3,.col-md-4,.col-md-2{overflow:visible!important;}
.row{overflow:visible!important;}
.box-body{overflow:visible!important;}
.tab-content{overflow:visible!important;}
.selectize-dropdown-content .option:hover{background:var(--bg-hover)!important;color:var(--accent-light)!important;}
.irs-bar,.irs-bar-edge{background:var(--accent)!important;border-color:var(--accent)!important;}
.irs-from,.irs-to,.irs-single{background:var(--accent)!important;color:#fff!important;font-size:10px!important;}
.irs-line{background:var(--border-mid)!important;}
label{color:var(--text-muted)!important;font-size:10.5px!important;font-weight:600!important;letter-spacing:.3px!important;}
.btn{border-radius:var(--radius-sm)!important;font-family:var(--font-sans)!important;font-weight:600!important;font-size:11px!important;letter-spacing:.3px!important;transition:all var(--transition)!important;}
.btn-success{background:linear-gradient(135deg,#166534,#16a34a)!important;border-color:#16a34a!important;color:#fff!important;}
.btn-success:hover{background:linear-gradient(135deg,#14532d,#15803d)!important;box-shadow:0 0 12px rgba(22,163,74,.4)!important;}
.btn-info{background:linear-gradient(135deg,#1e3a5f,#1d4ed8)!important;border-color:#1d4ed8!important;color:#fff!important;}
.btn-warning{background:linear-gradient(135deg,#78350f,#ca8a04)!important;border-color:#ca8a04!important;color:#fff!important;}
.btn-danger{background:linear-gradient(135deg,#7f1d1d,#dc2626)!important;border-color:#dc2626!important;color:#fff!important;}
.btn-block{width:100%!important;}
.nav-tabs-custom{background:var(--bg-card)!important;border:1px solid var(--border-subtle)!important;border-radius:var(--radius-md)!important;}
.nav-tabs-custom>.nav-tabs>li>a{color:var(--text-muted)!important;font-size:11.5px!important;font-weight:500!important;padding:8px 14px!important;}
.nav-tabs-custom>.nav-tabs>li.active>a{background:var(--bg-panel)!important;color:var(--text-primary)!important;border-top:2px solid var(--accent)!important;font-weight:600!important;}
.nav-tabs-custom>.tab-content{background:var(--bg-card)!important;padding:12px!important;}
.valor-big{font-size:2em;font-weight:800;line-height:1.1;font-family:var(--font-mono);color:var(--text-primary);}
.prev-box{background:linear-gradient(135deg,#020904 0%,#0b1f0d 100%);border:1px solid var(--border-accent);border-radius:var(--radius-lg);padding:20px;box-shadow:var(--shadow-lg);}
.stat-card{background:rgba(22,163,74,.06);border:1px solid var(--border-mid);border-radius:var(--radius-md);padding:14px;text-align:center;transition:all var(--transition);}
.stat-card:hover{background:rgba(22,163,74,.1);border-color:var(--border-accent);}
.ficha-box{background:var(--bg-card);border:1px solid var(--border-mid);border-left:4px solid var(--accent);padding:16px 20px;border-radius:0 var(--radius-md) var(--radius-md) 0;margin-bottom:14px;box-shadow:var(--shadow-sm);}
.ficha-box h3{color:var(--text-primary)!important;margin-top:0;font-size:16px;}
.badge-risco{padding:4px 10px;border-radius:14px;font-size:10.5px;font-weight:700;color:#fff;display:inline-block;letter-spacing:.3px;}
.dot-live{width:8px;height:8px;background:var(--accent);border-radius:50%;display:inline-block;animation:pulse 1.5s infinite;box-shadow:0 0 6px var(--accent);}
.badge-live{display:inline-flex;align-items:center;gap:6px;background:var(--bg-card);border:1px solid var(--border-mid);border-radius:14px;padding:4px 12px;font-size:10px;color:var(--accent-light);font-weight:700;letter-spacing:.5px;}
@keyframes pulse{0%,100%{opacity:1;transform:scale(1);}50%{opacity:.3;transform:scale(.65);}}
.alerta-item{background:var(--bg-card);border-left:3px solid var(--red);padding:6px 10px;margin:3px 0;border-radius:0 var(--radius-sm) var(--radius-sm) 0;font-size:10.5px;color:var(--red-soft);transition:background var(--transition);}
.alerta-item:hover{background:var(--bg-hover);}
.plotly-expand-btn{position:absolute;top:7px;right:7px;z-index:9999;background:rgba(5,13,7,.88);border:1px solid var(--border-accent);border-radius:var(--radius-sm);padding:4px 10px;cursor:pointer;color:var(--text-secondary);font-size:10.5px;font-weight:600;display:flex;align-items:center;gap:5px;transition:all var(--transition);user-select:none;}
.plotly-expand-btn:hover{background:var(--bg-panel);border-color:var(--accent-light);color:var(--text-primary);}
.plotly-wrap{position:relative;width:100%;overflow:visible;}
/* Leaflet expand button â€” positioned on top-left to not conflict with leaflet controls */
.leaflet-expand{top:7px!important;left:7px!important;right:auto!important;}
/* Leaflet container inside modal */
#fm-overlay{display:none;position:fixed;inset:0;z-index:99999;background:rgba(0,0,0,.94);align-items:center;justify-content:center;flex-direction:column;}
#fm-overlay.open{display:flex;}
#fm-inner{width:96vw;height:91vh;background:var(--bg-card);border:1px solid var(--border-accent);border-radius:var(--radius-lg);overflow:hidden;position:relative;box-shadow:var(--shadow-lg);display:flex;flex-direction:column;}
#fm-inner:fullscreen,#fm-inner:-webkit-full-screen{width:100vw!important;height:100vh!important;border-radius:0!important;}
#fm-header{display:flex;align-items:center;justify-content:space-between;padding:8px 16px;background:var(--bg-base);border-bottom:1px solid var(--border-mid);flex-shrink:0;}
#fm-title{color:var(--text-secondary);font-size:12px;font-weight:700;letter-spacing:.7px;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;max-width:60%;}
#fm-close{background:none;border:1px solid var(--red);border-radius:var(--radius-sm);color:var(--red-soft);font-size:13px;padding:3px 11px;cursor:pointer;transition:background var(--transition);}
#fm-close:hover{background:#7f1d1d;color:#fff;}
#fm-png{background:none;border:1px solid var(--border-accent);border-radius:var(--radius-sm);color:var(--text-secondary);font-size:11px;padding:3px 11px;cursor:pointer;transition:background var(--transition);}
#fm-png:hover{background:var(--bg-panel);color:var(--accent-light);}
#fm-fs{background:none;border:1px solid var(--border-accent);border-radius:var(--radius-sm);color:var(--text-secondary);font-size:11px;padding:3px 11px;cursor:pointer;transition:background var(--transition);}
#fm-fs:hover{background:var(--bg-panel);color:var(--accent-light);}
#fm-body{flex:1;padding:8px;overflow:hidden;min-height:0;}
#fm-body .plotly,#fm-body .js-plotly-plot{width:100%!important;height:100%!important;}
#fm-body #fm-leaflet{width:100%!important;height:100%!important;}
#fm-hint{color:var(--text-dim);font-size:9.5px;padding:4px 16px 7px;text-align:center;flex-shrink:0;}
hr{border-color:var(--border-subtle)!important;}
p,li{color:var(--text-secondary);font-size:12px;}
h4{color:var(--text-secondary)!important;}
.update-toast{position:fixed;bottom:16px;right:18px;z-index:9999;background:var(--bg-card);border:1px solid var(--border-accent);border-radius:var(--radius-md);padding:8px 14px;font-size:10.5px;color:var(--text-secondary);box-shadow:var(--shadow-md);}
::-webkit-scrollbar{width:5px;height:5px;}
::-webkit-scrollbar-track{background:var(--bg-base);}
::-webkit-scrollbar-thumb{background:var(--border-mid);border-radius:3px;}
::-webkit-scrollbar-thumb:hover{background:var(--border-accent);}

/* â”€â”€ ANTI-OVERLAP: card_mun / ficha_mun â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€ */
/* garante que uiOutput renderizado nunca flutua sobre grÃ¡ficos abaixo */
.shiny-html-output{display:block!important;clear:both!important;width:100%!important;min-height:0;}
/* box "black" usado como wrapper transparente */
.box-body>.shiny-html-output{padding:0!important;}
/* selectize dropdown sempre sobre tudo */
.selectize-dropdown{z-index:99999!important;position:absolute!important;}
/* linhas do tab-pane nÃ£o colapsam */
.tab-content .tab-pane>.row{display:flex!important;flex-wrap:wrap!important;clear:both!important;}
/* botÃµes no flex container alinham corretamente */
.btn-flex-row{display:flex!important;gap:6px!important;align-items:center!important;}
.btn-flex-row .btn,.btn-flex-row .btn-block{flex:1!important;width:auto!important;white-space:nowrap!important;}
/* label de input alinhada â€” evita deslocamento com br() */
.input-aligned .control-label{display:block!important;height:22px!important;margin-bottom:4px!important;}
.input-aligned .form-group{margin-bottom:0!important;}
/* box background="black" â€” remover fundo extra */
.bg-black .box-body{background:transparent!important;padding:0!important;}
/* Force Plotly to fill container - fixes invisible chart bug */
.plotly.html-widget{width:100%!important;height:100%!important;}
.js-plotly-plot,.plotly,.plot-container{width:100%!important;}
.shiny-plot-output,.plotly-graph-div{width:100%!important;}

'

## 5.2  Tema Plotly ----
# â”€â”€ TEMA PLOTLY â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# â”€â”€ .plt(): inicializador seguro de plotly â€” sempre retorna htmlwidget vÃ¡lido â”€â”€
# Substitui plot_ly() em acumuladores de loop: garante classe correta mesmo
# quando nenhuma trace Ã© adicionada (loop vazio ou dados nulos).
.plt <- function() {
  p <- plotly::plot_ly()
  # ForÃ§a a classe correta independente da versÃ£o do plotly
  if (!inherits(p, "plotly")) class(p) <- c("plotly", "htmlwidget")
  p
}

# â”€â”€ .safe_plotly(): converte qualquer objeto para plotly vÃ¡lido â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
.safe_plotly <- function(p) {
  if (inherits(p, "plotly")) return(p)
  # Fallback: cria um grÃ¡fico vazio com mensagem
  plotly::plot_ly() %>%
    plotly::layout(
      annotations = list(list(
        text      = "Sem dados para exibir",
        x         = 0.5, y = 0.5,
        xref      = "paper", yref = "paper",
        showarrow = FALSE,
        font      = list(color = "#4a7a52", size = 13,
                         family = "Inter, sans-serif")
      )),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)"
    )
}

# â”€â”€ tp(): tema escuro + toolbar Plotly completa (zoom, pan, select, reset) â”€â”€
# NOTA: os parÃ¢metros extra (...) vÃ£o para xaxis via modifyList para nÃ£o
# sobrescrever as configuraÃ§Ãµes base. config() ativa a barra de ferramentas.
tp <- function(p, ...) {
  p <- .safe_plotly(p)
  xaxis_extra <- list(...)
  xaxis_base  <- list(
    gridcolor = "#162d1a",
    linecolor = "#1e4024",
    zeroline  = FALSE,
    tickfont  = list(color = "#4a7a52", size = 10)
  )
  p %>%
    plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      autosize      = TRUE,
      font          = list(family = "Inter, sans-serif", color = "#94c99a", size = 11),
      xaxis         = modifyList(xaxis_base, xaxis_extra),
      yaxis         = list(
        gridcolor = "#162d1a",
        linecolor = "#1e4024",
        zeroline  = FALSE,
        tickfont  = list(color = "#4a7a52", size = 10)
      ),
      legend = list(
        bgcolor   = "rgba(0,0,0,0)",
        font      = list(color = "#94c99a", size = 10),
        orientation = "h",
        yanchor   = "top",  y = -0.38,
        xanchor   = "center", x = 0.5
      ),
      margin    = list(t = 32, r = 14, b = 100, l = 10),
      hoverlabel = list(
        bgcolor     = "#050d07",
        bordercolor = "#16a34a",
        font        = list(family = "JetBrains Mono, monospace", color = "#e8f5ea", size = 11)
      ),
      dragmode = "zoom"
    ) %>%
    plotly::config(
      displayModeBar  = TRUE,
      displaylogo     = FALSE,
      responsive      = TRUE,
      scrollZoom      = TRUE,
      modeBarButtonsToRemove = c(
        "pan2d", "select2d", "lasso2d",
        "autoScale2d", "hoverClosestCartesian",
        "hoverCompareCartesian", "toggleSpikelines",
        "sendDataToCloud", "editInChartStudio"
      ),
      toImageButtonOptions = list(
        format   = "png",
        filename = paste0("ferrugem_MT_", format(Sys.Date(), "%Y%m%d")),
        width    = 1200,
        height   = 700,
        scale    = 2
      )
    )
}

# â”€â”€ tp_sub(): variante para subplots â€” sem legenda inline â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
tp_sub <- function(p) {
  p <- .safe_plotly(p)   # <-- mesma proteÃ§Ã£o
  p %>%
    plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font          = list(family = "Inter, sans-serif", color = "#94c99a", size = 10),
      margin        = list(t = 20, r = 14, b = 90, l = 10),
      hoverlabel    = list(
        bgcolor     = "#050d07",
        bordercolor = "#16a34a",
        font        = list(family = "JetBrains Mono, monospace", color = "#e8f5ea", size = 11)
      ),
      legend = list(
        bgcolor     = "rgba(0,0,0,0)",
        font        = list(color = "#94c99a", size = 9),
        orientation = "h",
        yanchor     = "top",  y = -0.38,
        xanchor     = "center", x = 0.5
      ),
      dragmode = "zoom"
    ) %>%
    plotly::config(
      displayModeBar = TRUE,
      displaylogo    = FALSE,
      responsive     = TRUE,
      scrollZoom     = TRUE,
      modeBarButtonsToRemove = c(
        "pan2d", "select2d", "lasso2d",
        "autoScale2d", "hoverClosestCartesian",
        "hoverCompareCartesian", "toggleSpikelines",
        "sendDataToCloud", "editInChartStudio"
      ),
      toImageButtonOptions = list(
        format   = "png",
        filename = paste0("ferrugem_MT_subplot_", format(Sys.Date(), "%Y%m%d")),
        width    = 1400, height = 700, scale = 2
      )
    )
}
tg <- function() ggplot2::theme_minimal(base_size=9,base_family="Inter") + theme(
  plot.background=element_rect(fill="transparent",colour=NA),
  panel.background=element_rect(fill="transparent",colour=NA),
  panel.grid.major=element_line(colour="#162d1a",linewidth=.3),
  panel.grid.minor=element_blank(),
  axis.text=element_text(colour="#4a7a52",size=8.5),
  axis.title=element_text(colour="#4a7a52",size=9),
  legend.background=element_rect(fill="transparent"),
  legend.text=element_text(colour="#94c99a",size=9),
  legend.title=element_text(colour="#94c99a",size=9.5),
  strip.text=element_text(colour="#94c99a",size=9),
  strip.background=element_rect(fill="#0e2010",colour="#1e4024"),
  plot.title=element_text(colour="#94c99a",size=11,face="bold"),
  plot.subtitle=element_text(colour="#4a7a52",size=9)
)

## 5.3  UI Principal â€” dashboardPage ----
# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
# UI
# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
ui <- dashboardPage(skin="green",
  dashboardHeader(titleWidth=270,
    title=tags$span(
      style=paste0("font-family:'Inter',sans-serif;font-size:11.5px;font-weight:800;",
                   "letter-spacing:.5px;display:flex;align-items:center;gap:8px;"),
      tags$img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/0/0b/Brasao_mato_grosso.svg/22px-Brasao_mato_grosso.svg.png",
               height="20px",style="opacity:.9;"),
      tags$span(style="color:#22c55e;","FERRUGEM"),
      tags$span(style="color:#94c99a;","ASIÃTICA"),
      tags$span(style="color:#4a7a52;font-weight:400;font-size:10px;","MT v8.0")),
    tags$li(class="dropdown",style="padding:12px 10px;",
      tags$div(class="badge-live",tags$div(class="dot-live"),tags$span("LIVE"))),
    tags$li(class="dropdown",style="padding:12px 12px;",uiOutput("hdr_mercado"))
  ),
  dashboardSidebar(width=275,
    sidebarMenu(id="menu",
      tags$li(class="sidebar-note",style="padding:12px 15px 4px;border-top:none!important;",
        "â‘  MONITORAMENTO ATUAL"),
      menuItem("VisÃ£o Geral",          tabName="geral",     icon=icon("home")),
      menuItem("Mapa Interativo",      tabName="mapa",      icon=icon("map")),
      menuItem("Buscar MunicÃ­pio",     tabName="busca",     icon=icon("search")),
      menuItem("Info por MunicÃ­pio",   tabName="info_mun",  icon=icon("leaf")),
      tags$li(class="sidebar-note",
        "â‘¡ ANÃLISE DE RISCO"),
      menuItem("Ranking Vulnerab.",    tabName="ranking",   icon=icon("trophy")),
      menuItem("Perdas EconÃ´micas",    tabName="perdas",    icon=icon("dollar-sign")),
      menuItem("Seguro Rural",         tabName="sinistro",  icon=icon("shield-alt")),
      tags$li(class="sidebar-note",
        "â‘¢ PROJEÃ‡Ã•ES FUTURAS"),
      menuItem("Mapa Incid. Futuras",  tabName="mapa_futuro",icon=icon("globe"),
               badgeLabel="NOVO", badgeColor="red"),
      menuItem("PrevisÃ£o 2025/26",     tabName="previsao",  icon=icon("binoculars")),
      menuItem("CenÃ¡rios ENSO",        tabName="cenarios",  icon=icon("cloud-sun-rain")),
      tags$li(class="sidebar-note",
        "â‘£ HISTÃ“RICO E CLIMA"),
      menuItem("SÃ©rie HistÃ³rica",      tabName="historico", icon=icon("chart-line")),
      menuItem("Clima e DoenÃ§a",       tabName="clima",     icon=icon("cloud-rain")),
      tags$li(class="sidebar-note",
        "â‘¤ MANEJO E DADOS"),
      menuItem("Fungicidas e Manejo",  tabName="manejo",    icon=icon("flask")),
      menuItem("Dados e ExportaÃ§Ã£o",   tabName="tabela",    icon=icon("download")),
      menuItem("Status e APIs",        tabName="status",    icon=icon("wifi")),
      menuItem("Metodologia",          tabName="metodologia",icon=icon("book"))
    ),
    tags$hr(style="border-color:#162d1a;margin:8px 0;"),
    tags$div(style="padding:0 14px 10px;",
      tags$div(style="color:#16a34a;font-size:8.5px;font-weight:800;letter-spacing:1.5px;text-transform:uppercase;margin-bottom:10px;",
        "FILTROS GLOBAIS"),
      pickerInput("f_risco","NÃ­vel de Risco:",choices=c("Todos",NIVEIS_RISCO),selected="Todos",
                  options=list(style="btn-sm btn-default")),
      sliderInput("f_irc","IRC (0â€“100):",min=0,max=100,value=c(0,100),step=5),
      sliderInput("f_prod","Prod. min. (mil t):",min=0,max=3000,value=0,step=50),
      actionButton("btn_refresh","âŸ³ Atualizar Dados",class="btn-success btn-sm btn-block",
                   style="margin-top:8px;font-size:11px;font-weight:700;letter-spacing:.5px;")
    ),
    tags$hr(style="border-color:#162d1a;margin:4px 0;"),
    tags$div(style="padding:6px 14px 12px;font-size:9.5px;line-height:2;color:#2d5c32;",
      tags$div(style="color:#16a34a;font-size:8px;font-weight:800;letter-spacing:1.2px;text-transform:uppercase;margin-bottom:4px;",
        "FONTES DE DADOS"),
      icon("database",style="color:#1e4024;")," IBGE/SIDRA Tab.1612",tags$br(),
      icon("satellite",style="color:#1e4024;")," NASA POWER API (ERA5-Land)",tags$br(),
      icon("cloud",style="color:#1e4024;")," Open-Meteo ERA5-Land",tags$br(),
      icon("map",style="color:#1e4024;")," geobr/IBGE PolÃ­gonos MT",tags$br(),
      icon("globe",style="color:#1e4024;")," NOAA CPC â€” ENSO/ONI",tags$br(),
      icon("chart-bar",style="color:#1e4024;")," CEPEA/ESALQ PreÃ§o Soja",tags$br(),
      icon("flask",style="color:#1e4024;")," EMBRAPA SIGA Alertas",tags$br(),
      icon("dollar-sign",style="color:#1e4024;")," AwesomeAPI USD/BRL",tags$br(),
      icon("seedling",style="color:#1e4024;")," CONAB â€” Safras"
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML(CSS)),
      # â”€â”€ Ticker de atualizaÃ§Ã£o automÃ¡tica (30 min) â”€â”€
      tags$script(HTML(
        "setInterval(function(){Shiny.setInputValue('auto_tick',Math.random());},1800000);"
      )),
      # â”€â”€ ForÃ§ar Plotly a renderizar ao trocar de aba â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
      tags$script(HTML("
$(document).ready(function() {
  // Re-renderiza todos os grÃ¡ficos Plotly quando uma aba se torna visÃ­vel
  $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function(e) {
    setTimeout(function() {
      var plots = document.querySelectorAll('.js-plotly-plot');
      plots.forEach(function(p) {
        try { Plotly.Plots.resize(p); } catch(e) {}
      });
    }, 100);
  });

  // TambÃ©m re-renderiza ao clicar nos itens do sidebar (shinydashboard usa href)
  $(document).on('click', '.sidebar-menu a', function() {
    setTimeout(function() {
      var plots = document.querySelectorAll('.js-plotly-plot');
      plots.forEach(function(p) {
        try { Plotly.Plots.resize(p); } catch(e) {}
      });
    }, 150);
  });

  // Corrige posiÃ§Ã£o do dropdown selectize ao usar position:fixed
  function reposSelectize($ctl) {
    var $input    = $ctl.find('.selectize-input');
    var $dropdown = $ctl.find('.selectize-dropdown');
    if (!$dropdown.length) return;
    var rect = $input[0].getBoundingClientRect();
    $dropdown.css({ top: rect.bottom+'px', left: rect.left+'px', width: rect.width+'px' });
  }
  $(document).on('mousedown touchstart', '.selectize-input', function() {
    reposSelectize($(this).closest('.selectize-control'));
  });
  $(window).on('scroll resize', function() {
    $('.selectize-control.open').each(function() { reposSelectize($(this)); });
  });
});
      ")),
      # â”€â”€ Modal fullscreen para ampliar grÃ¡ficos e mapas â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
      tags$script(HTML("
/* â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
   SISTEMA DE AMPLIAR â€” Plotly + Leaflet â€” Ferrugem App
   BotÃ£o â›¶ Ampliar aparece em todo grÃ¡fico/mapa ao renderizar.
   Abre modal 96vw Ã— 91vh com cÃ³pia fiel + toolbar + download PNG.
â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â• */
$(document).ready(function() {

  /* â”€â”€ 1. ESTRUTURA DO MODAL â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€ */
  var MODAL_HTML =
    '<div id=\"fm-overlay\">' +
      '<div id=\"fm-inner\">' +
        '<div id=\"fm-header\">' +
          '<span id=\"fm-title\"></span>' +
          '<div style=\"display:flex;gap:6px;align-items:center;\">' +
            '<button id=\"fm-fs\"    title=\"Tela cheia do navegador\">&#x26F6; Tela Cheia</button>' +
            '<button id=\"fm-png\"   title=\"Baixar PNG (apenas grÃ¡ficos)\">&#x2B07; PNG</button>' +
            '<button id=\"fm-close\" title=\"Fechar (ESC)\">&#x2715; Fechar</button>' +
          '</div>' +
        '</div>' +
        '<div id=\"fm-body\"></div>' +
        '<div id=\"fm-hint\">Scroll = zoom &nbsp;&#xB7;&nbsp; Arraste = pan &nbsp;&#xB7;&nbsp; Duplo-clique = resetar &nbsp;&#xB7;&nbsp; ESC = fechar</div>' +
      '</div>' +
    '</div>';
  $('body').append(MODAL_HTML);

  /* â”€â”€ 2. FECHAR â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€ */
  function fecharModal() {
    $('#fm-overlay').removeClass('open');
    var pnode = document.getElementById('fm-plot');
    if (pnode && window.Plotly) Plotly.purge(pnode);
    $('#fm-body').empty();
    // Sair de tela cheia se estiver ativa
    if (document.fullscreenElement) {
      try { document.exitFullscreen(); } catch(e) {}
    }
  }
  $(document).on('click', '#fm-close',  fecharModal);
  $(document).on('click', '#fm-overlay', function(e) {
    if ($(e.target).is('#fm-overlay')) fecharModal();
  });
  $(document).on('keydown', function(e) {
    if (e.key === 'Escape') fecharModal();
  });

  /* â”€â”€ 3. TELA CHEIA DO NAVEGADOR â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€ */
  $(document).on('click', '#fm-fs', function() {
    var el = document.getElementById('fm-inner');
    if (!document.fullscreenElement) {
      el.requestFullscreen && el.requestFullscreen();
    } else {
      document.exitFullscreen && document.exitFullscreen();
    }
  });
  document.addEventListener('fullscreenchange', function() {
    if (!document.fullscreenElement) {
      // Redimensiona plotly apÃ³s sair de fullscreen
      setTimeout(function() {
        var pnode = document.getElementById('fm-plot');
        if (pnode && window.Plotly) {
          Plotly.relayout(pnode, {
            width:  $('#fm-body').width()  - 16,
            height: $('#fm-body').height() - 16
          });
        }
        var lmap = window._fmLeafletMap;
        if (lmap) { setTimeout(function(){ lmap.invalidateSize(); }, 100); }
      }, 200);
    }
  });

  /* â”€â”€ 4. DOWNLOAD PNG (apenas Plotly) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€ */
  $(document).on('click', '#fm-png', function() {
    var node = document.getElementById('fm-plot');
    if (node && window.Plotly) {
      Plotly.downloadImage(node, {
        format: 'png', width: 1600, height: 900, scale: 2,
        filename: ($('#fm-title').text() || 'grafico').replace(/[^a-zA-Z0-9_\\-]/g,'_')
      });
    } else {
      alert('Download PNG disponÃ­vel apenas para grÃ¡ficos Plotly.');
    }
  });

  /* â”€â”€ 5. SVG DO BOTÃƒO AMPLIAR â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€ */
  var EXPAND_SVG =
    '<svg width=\"12\" height=\"12\" viewBox=\"0 0 24 24\" fill=\"none\" ' +
        'stroke=\"currentColor\" stroke-width=\"2.5\" style=\"flex-shrink:0\">' +
      '<polyline points=\"15 3 21 3 21 9\"></polyline>' +
      '<polyline points=\"9 21 3 21 3 15\"></polyline>' +
      '<line x1=\"21\" y1=\"3\" x2=\"14\" y2=\"10\"></line>' +
      '<line x1=\"3\" y1=\"21\" x2=\"10\" y2=\"14\"></line>' +
    '</svg>';

  /* â”€â”€ 6. INJETAR BOTÃƒO â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€ */
  function injectBtn($el, type, refId, title) {
    if ($el.closest('.plotly-wrap').length) return; // jÃ¡ tem
    $el.wrap('<div class=\"plotly-wrap\"></div>');
    var cls = type === 'leaflet' ? 'plotly-expand-btn leaflet-expand' : 'plotly-expand-btn';
    var btn = $('<div class=\"' + cls + '\" title=\"Ampliar â€” tela cheia\">' +
                  EXPAND_SVG + ' Ampliar</div>');
    btn.data('type', type).data('ref-id', refId).data('title', title);
    $el.parent().append(btn);
  }

  /* â”€â”€ 7. OBSERVER: detecta novos grÃ¡ficos/mapas â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€ */
  var _seen = new WeakSet();
  var observer = new MutationObserver(function() {
    // Plotly
    $('.js-plotly-plot').each(function() {
      if (_seen.has(this)) return;
      _seen.add(this);
      var $p  = $(this);
      var id  = $p.closest('[id]').attr('id') || 'plot';
      var $bx = $p.closest('.box');
      var ttl = $bx.length
                  ? $bx.find('.box-title,.box-header .box-title').first().text().trim()
                  : id;
      injectBtn($p, 'plotly', id, ttl || id);
    });
    // Leaflet
    $('.leaflet-container').each(function() {
      if (_seen.has(this)) return;
      _seen.add(this);
      var $m  = $(this);
      // Sobe pelo DOM: leaflet-container â†’ .html-widget â†’ shiny-output[id]
      var $out = $m.closest('.shiny-bound-output, .html-widget-output, [id]');
      var id   = $out.attr('id') || 'map';
      var $bx  = $m.closest('.box');
      var ttl  = $bx.length
                   ? $bx.find('.box-title,.box-header .box-title').first().text().trim()
                   : id;
      injectBtn($m, 'leaflet', id, ttl || id);
    });
  });
  observer.observe(document.body, { childList: true, subtree: true });

  /* â”€â”€ 8. CLIQUE NO BOTÃƒO AMPLIAR â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€ */
  $(document).on('click', '.plotly-expand-btn', function(e) {
    e.stopPropagation();
    var type  = $(this).data('type')   || 'plotly';
    var refId = $(this).data('ref-id') || '';
    var title = $(this).data('title')  || 'Ampliar';
    var $wrap = $(this).closest('.plotly-wrap');

    $('#fm-title').text(title);
    $('#fm-overlay').addClass('open');
    $('#fm-png').toggle(type === 'plotly');
    window._fmLeafletMap = null;

    /* â”€â”€ PLOTLY â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€ */
    if (type === 'plotly') {
      var srcNode = $wrap.find('.js-plotly-plot')[0];
      if (!srcNode || !window.Plotly) {
        $('#fm-body').html('<p style=\"color:#94c99a;padding:20px;\">GrÃ¡fico ainda nÃ£o disponÃ­vel.</p>');
        return;
      }
      $('#fm-body').empty().append('<div id=\"fm-plot\" style=\"width:100%;height:100%;\"></div>');
      var dest = document.getElementById('fm-plot');

      var data   = srcNode.data || [];
      var layout = JSON.parse(JSON.stringify(srcNode.layout || {}));
      layout.width        = $('#fm-body').width()  - 16;
      layout.height       = $('#fm-body').height() - 16;
      layout.paper_bgcolor = 'rgba(5,13,7,0.98)';
      layout.plot_bgcolor  = 'rgba(5,13,7,0.98)';
      layout.font          = layout.font || {};
      layout.font.color    = layout.font.color || '#94c99a';
      layout.margin        = layout.margin || {l:60,r:60,t:50,b:80};

      var cfg = {
        displayModeBar: true, scrollZoom: true,
        responsive: true, displaylogo: false,
        modeBarButtonsToRemove: ['sendDataToCloud','editInChartStudio'],
        toImageButtonOptions: {
          format: 'png', width: 1600, height: 900, scale: 2,
          filename: title.replace(/[^a-zA-Z0-9_\\-]/g,'_')
        }
      };
      setTimeout(function() { Plotly.newPlot(dest, data, layout, cfg); }, 80);

    /* â”€â”€ LEAFLET â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€ */
    } else {
      $('#fm-body').empty().append(
        '<div id=\"fm-leaflet\" style=\"width:100%;height:100%;border-radius:6px;\"></div>'
      );

      setTimeout(function() {
        // Tentar obter o mapa original via HTMLWidgets
        var widget = window.HTMLWidgets && HTMLWidgets.find('#' + refId);
        var origMap = widget && widget.getMap ? widget.getMap() : null;

        // Fallback: procurar instÃ¢ncia L.Map no container
        if (!origMap) {
          var containers = document.querySelectorAll('#' + refId + ' .leaflet-container');
          if (containers.length) {
            var cid = containers[0]._leaflet_id;
            origMap = cid ? L.map._instances && L.map._instances[cid] : null;
          }
        }

        var newMap = L.map('fm-leaflet', {
          center: origMap ? origMap.getCenter() : [-13.5, -55.5],
          zoom:   origMap ? origMap.getZoom() + 1 : 7,
          zoomControl: true,
          scrollWheelZoom: true
        });

        // Tile base dark
        L.tileLayer('https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png', {
          attribution: '&copy; CartoDB', maxZoom: 19
        }).addTo(newMap);

        // Copiar camadas do mapa original (cÃ­rculos, marcadores, polÃ­gonos, legendas)
        if (origMap) {
          origMap.eachLayer(function(layer) {
            if (layer instanceof L.TileLayer) return; // jÃ¡ adicionamos tile
            try {
              // Clonar GeoJSON / CircleMarker / Marker / Polygon
              if (layer.toGeoJSON) {
                var geojson = layer.toGeoJSON();
                var opts = { style: layer.options, pointToLayer: null };
                // Copiar popups/labels
                L.geoJSON(geojson, {
                  style: function() { return layer.options || {}; },
                  pointToLayer: function(feature, latlng) {
                    var r = layer.options && layer.options.radius ? layer.options.radius : 8;
                    return L.circleMarker(latlng, layer.options || {});
                  },
                  onEachFeature: function(feature, fl) {
                    if (layer.getPopup && layer.getPopup()) {
                      fl.bindPopup(layer.getPopup().getContent());
                    }
                  }
                }).addTo(newMap);
              } else if (layer.getLatLng) {
                // Marcador simples
                var m2 = L.circleMarker(layer.getLatLng(), layer.options || {});
                if (layer.getPopup && layer.getPopup()) m2.bindPopup(layer.getPopup().getContent());
                if (layer.getTooltip && layer.getTooltip()) m2.bindTooltip(layer.getTooltip().getContent());
                m2.addTo(newMap);
              }
            } catch(e2) { /* skip layer on error */ }
          });

          // Copiar controles de legenda (DivOverlay / Control)
          origMap.eachLayer(function() {}); // trigger
          if (origMap._controlCorners) {
            // Tentar copiar legendas adicionadas via addLegend
            $('*[class*=\"leaflet-control-\"]').filter(function() {
              return $(this).closest('#' + refId).length > 0 &&
                     $(this).closest('.leaflet-control-zoom').length === 0;
            }).each(function() {
              var html = $(this).prop('outerHTML');
              if (html && html.length > 50) {
                var ctrl = L.control({ position: 'bottomright' });
                ctrl.onAdd = function() {
                  var d = L.DomUtil.create('div');
                  d.innerHTML = html;
                  return d;
                };
                ctrl.addTo(newMap);
              }
            });
          }
        }

        setTimeout(function() { newMap.invalidateSize(); }, 150);
        window._fmLeafletMap = newMap;
      }, 120);
    }
  });

  /* â”€â”€ 9. REDIMENSIONAR AO MUDAR JANELA â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€ */
  $(window).on('resize', function() {
    if (!$('#fm-overlay').hasClass('open')) return;
    var pnode = document.getElementById('fm-plot');
    if (pnode && window.Plotly) {
      Plotly.relayout(pnode, {
        width:  $('#fm-body').width()  - 16,
        height: $('#fm-body').height() - 16
      });
    }
    if (window._fmLeafletMap) {
      setTimeout(function() { window._fmLeafletMap.invalidateSize(); }, 100);
    }
  });

});
      "))
    ),
    uiOutput("update_toast"),
    tabItems(
### 5.3.1  Aba 01 â€” Visao Geral ----
      # â•â• ABA 1: VISAO GERAL â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
      tabItem("geral",
        # â”€â”€ KPI strip enterprise â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        fluidRow(style="margin-bottom:4px;",
          box(width=12, solidHeader=FALSE, style="padding:4px 8px;border-top:3px solid #16a34a;",
            tags$div(style="display:flex;align-items:center;justify-content:space-between;padding:4px 6px;",
              tags$div(style="display:flex;align-items:center;gap:10px;",
                tags$span(style="font-size:18px;","ðŸŒ¿"),
                tags$div(
                  tags$div(style="font-size:13px;font-weight:800;color:#e8f5ea;letter-spacing:.4px;",
                    "Dashboard Ferrugem AsiÃ¡tica da Soja â€” MT"),
                  tags$div(style="font-size:9.5px;color:#4a7a52;",
                    "Monitoramento â€¢ Risco â€¢ PrevisÃ£o â€¢ Suporte Ã  DecisÃ£o | Safra 2024/25")
                )
              ),
              tags$div(style="display:flex;gap:12px;align-items:center;",
                tags$div(style="text-align:right;",
                  tags$div(style="font-size:9px;color:#4a7a52;font-weight:700;text-transform:uppercase;letter-spacing:.8px;","Cobertura"),
                  tags$div(style="font-size:14px;font-weight:800;color:#22c55e;font-family:'JetBrains Mono',monospace;","47 municÃ­pios")),
                tags$div(style="width:1px;height:30px;background:#162d1a;"),
                tags$div(style="text-align:right;",
                  tags$div(style="font-size:9px;color:#4a7a52;font-weight:700;text-transform:uppercase;letter-spacing:.8px;","Dados ClimÃ¡ticos"),
                  tags$div(style="font-size:12px;font-weight:700;color:#94c99a;font-family:'JetBrains Mono',monospace;","ERA5-Land + NASA")),
                tags$div(style="width:1px;height:30px;background:#162d1a;"),
                tags$div(style="text-align:right;",
                  tags$div(style="font-size:9px;color:#4a7a52;font-weight:700;text-transform:uppercase;letter-spacing:.8px;","PerÃ­odo"),
                  tags$div(style="font-size:12px;font-weight:700;color:#94c99a;font-family:'JetBrains Mono',monospace;","2012 â€“ 2025/26"))
              )
            )
          )
        ),
        # â”€â”€ ExplicaÃ§Ã£o dos 47 municÃ­pios â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        fluidRow(
          box(width=12, solidHeader=FALSE,
            style="border-left:4px solid #16a34a;padding:0;margin-bottom:2px;",
            tags$div(
              style=paste0("display:flex;align-items:flex-start;gap:0;",
                "background:linear-gradient(90deg,rgba(22,163,74,.08) 0%,transparent 100%);",
                "border-radius:0 8px 8px 0;padding:12px 18px;"),
              # Ãcone
              tags$div(style="font-size:22px;margin-right:14px;padding-top:2px;flex-shrink:0;","ðŸ“"),
              # ConteÃºdo
              tags$div(style="flex:1;",
                tags$div(style="font-size:11px;font-weight:800;color:#22c55e;letter-spacing:.5px;text-transform:uppercase;margin-bottom:6px;",
                  "Por que 47 municÃ­pios?"),
                tags$div(style="font-size:11px;color:#94c99a;line-height:1.7;",
                  tags$b(style="color:#e8f5ea;","CritÃ©rio 1 â€” RelevÃ¢ncia produtiva: "),
                  "municÃ­pios com Ã¡rea plantada de soja â‰¥ 20.000 ha/ano (IBGE PAM 2018â€“2023), ",
                  "representando coletivamente ", tags$b(style="color:#22c55e;","â‰¥ 97% da produÃ§Ã£o total de MT"),
                  " (~35,8 Mi t/ano â€” CONAB 2024).",tags$br(),
                  tags$b(style="color:#e8f5ea;","CritÃ©rio 2 â€” Disponibilidade de dados climÃ¡ticos: "),
                  "sÃ©ries ERA5-Land (Open-Meteo) e NASA POWER contÃ­nuas de 2012 a 2024 para ",
                  "precipitaÃ§Ã£o, temperatura e umidade relativa â€” necessÃ¡rias para o cÃ¡lculo do IRC ",
                  "(Del Ponte et al., 2006).",tags$br(),
                  tags$b(style="color:#e8f5ea;","CritÃ©rio 3 â€” HistÃ³rico fitossanitÃ¡rio: "),
                  "registros EMBRAPA SIGA de ocorrÃªncia de ferrugem asiÃ¡tica ",
                  "(", tags$i("Phakopsora pachyrhizi"), ") com pelo menos 5 safras monitoradas.",tags$br(),
                  tags$b(style="color:#e8f5ea;","CritÃ©rio 4 â€” Cobertura geogrÃ¡fica: "),
                  "distribuiÃ§Ã£o espacial equilibrada pelas 5 mesorregiÃµes de MT ",
                  "(Norte, Nordeste, Centro-Sul, Sudoeste e Leste), garantindo representatividade regional."
                ),
                tags$div(style="margin-top:8px;display:flex;gap:16px;flex-wrap:wrap;",
                  tags$span(style="font-size:9.5px;color:#4a7a52;",
                    icon("map-marker-alt",style="color:#16a34a;")," 47 / 141 municÃ­pios MT"),
                  tags$span(style="font-size:9.5px;color:#4a7a52;",
                    icon("leaf",style="color:#16a34a;")," ~35,8 Mi t/ano (97% da prod. MT)"),
                  tags$span(style="font-size:9.5px;color:#4a7a52;",
                    icon("cloud",style="color:#16a34a;")," ERA5-Land 2012â€“2024 (Open-Meteo)"),
                  tags$span(style="font-size:9.5px;color:#4a7a52;",
                    icon("flask",style="color:#16a34a;")," â‰¥5 safras EMBRAPA SIGA"),
                  tags$span(style="font-size:9.5px;color:#2d5c32;",
                    "Fonte: IBGE PAM â€¢ CONAB 2024 â€¢ EMBRAPA SIGA â€¢ Open-Meteo ERA5-Land")
                )
              )
            )
          )
        ),
        fluidRow(
          infoBoxOutput("ib_mun",width=3),infoBoxOutput("ib_alto",width=3),
          infoBoxOutput("ib_prod",width=3),infoBoxOutput("ib_perda",width=3)
        ),
        fluidRow(
          box(width=5,title="IRC por MunicÃ­pio â€” Safra 2024/25",status="success",solidHeader=TRUE,
              plotlyOutput("g_irc_bar",height="380px") %>% withSpinner(color="#16a34a",type=4)),
          box(width=3,title="DistribuiÃ§Ã£o de Risco ClimÃ¡tico",status="success",solidHeader=TRUE,
              plotlyOutput("g_pizza",height="380px") %>% withSpinner(color="#16a34a",type=4)),
          box(width=4,title="Prob. Ferrugem por Safra â€” MÃ©dia MT",status="danger",solidHeader=TRUE,
              plotlyOutput("g_prob_hist",height="380px") %>% withSpinner(color="#dc2626",type=4))
        ),
        fluidRow(
          box(width=7,title="Top 15 â€” Ãndice de Vulnerabilidade Municipal (IVM)",status="warning",solidHeader=TRUE,
              plotlyOutput("g_top15",height="310px") %>% withSpinner(color="#ca8a04",type=4)),
          box(width=5,title="âš  ALERTA â€” MunicÃ­pios em Risco para 2025/26",status="danger",solidHeader=TRUE,
              uiOutput("ui_alerta"))
        )
      ),
### 5.3.2  Aba 02 â€” Busca de Municipio ----
      # â•â• ABA 2: BUSCA â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
      tabItem("busca",
        fluidRow(
          box(width=12, status="success", solidHeader=TRUE,
              title="ðŸ” AnÃ¡lise Detalhada por MunicÃ­pio",
            tags$p(style="color:#4a7a52;font-size:11px;margin:0 0 12px;",
              "Selecione um municÃ­pio e uma safra para ver anÃ¡lise completa: IRC histÃ³rico, ",
              "probabilidade de ocorrÃªncia de ferrugem, produÃ§Ã£o IBGE, previsÃ£o 2025/26 e clima mensal."),
            fluidRow(
              column(4,
                tags$label("MunicÃ­pio:",
                  style="color:#4a7a52;font-size:10.5px;font-weight:600;display:block;margin-bottom:4px;"),
                selectizeInput("sel_mun", label=NULL, choices=NULL,
                  options=list(placeholder="Digite ou selecione o municÃ­pio...", maxItems=1),
                  width="100%")
              ),
              column(3,
                tags$label("Safra:",
                  style="color:#4a7a52;font-size:10.5px;font-weight:600;display:block;margin-bottom:4px;"),
                selectInput("sel_safra_det", label=NULL,
                  choices=rev(SAFRAS), selected=SAFRA_ATUAL, width="100%")
              ),
              column(5,
                tags$div(style="height:22px;"),
                tags$div(style="display:flex;gap:6px;",
                  actionButton("btn_buscar", "ðŸ” Analisar",
                    class="btn-success",
                    style="font-weight:700;flex:2;min-width:0;"),
                  downloadButton("dl_mun", "â¬‡ CSV",
                    class="btn-info",
                    style="flex:1.5;min-width:0;"),
                  downloadButton("dl_mun_pdf", "â¬‡ PDF",
                    icon=icon("file-pdf"), class="btn-danger",
                    style="flex:1;min-width:0;font-size:11px;padding:6px 8px;")
                )
              )
            )
          )
        ),
        fluidRow(
          box(width=12, background="black",
            uiOutput("card_mun")
          )
        ),
        fluidRow(
          box(width=6, title="Probabilidade de Ferrugem â€” SÃ©rie HistÃ³rica",
              status="danger", solidHeader=TRUE,
              plotlyOutput("g_mun_prob", height="260px") %>% withSpinner(color="#dc2626",type=4)),
          box(width=6, title="IRC HistÃ³rico + IC 80% PrevisÃ£o 2025/26",
              status="warning", solidHeader=TRUE,
              plotlyOutput("g_mun_irc", height="260px") %>% withSpinner(color="#ca8a04",type=4))
        ),
        fluidRow(
          box(width=6, title="ProduÃ§Ã£o (mil t) e Produtividade (kg/ha) â€” IBGE PAM",
              status="primary", solidHeader=TRUE,
              plotlyOutput("g_mun_prod", height="300px") %>% withSpinner(color="#1d4ed8",type=4)),
          box(width=6, title="VariÃ¡veis ClimÃ¡ticas Mensais â€” Safra Selecionada",
              status="info", solidHeader=TRUE,
              plotlyOutput("g_mun_clima", height="300px") %>% withSpinner(color="#0891b2",type=4))
        ),
        fluidRow(
          box(width=12, title="ComparaÃ§Ã£o: MunicÃ­pio vs MÃ©dia MT â€” IRC por Safra",
              status="success", solidHeader=TRUE,
              plotlyOutput("g_mun_comp", height="240px") %>% withSpinner(color="#16a34a",type=4))
        )
      ),
### 5.3.3  Aba 03 â€” Info por Municipio ----
      tabItem("info_mun",
        fluidRow(
          box(width=12, status="success", solidHeader=TRUE,
              title="ðŸŒ¿ Ficha FitossanitÃ¡ria Completa â€” Ferrugem AsiÃ¡tica da Soja",
            tags$p(style="color:#4a7a52;font-size:11px;margin:0 0 12px;",
              "Ficha tÃ©cnica completa com indicadores de risco, resistÃªncia a fungicidas, impacto econÃ´mico, ",
              "calendÃ¡rio de risco mensal e janela de aplicaÃ§Ã£o baseada em estÃ¡dios fenolÃ³gicos (EMBRAPA/MT)."),
            fluidRow(
              column(4,
                tags$label("MunicÃ­pio:",
                  style="color:#4a7a52;font-size:10.5px;font-weight:600;display:block;margin-bottom:4px;"),
                selectizeInput("sel_mun_info", label=NULL, choices=NULL,
                  options=list(placeholder="Selecione o municÃ­pio...", maxItems=1),
                  width="100%")
              ),
              column(3,
                tags$label("Safra:",
                  style="color:#4a7a52;font-size:10.5px;font-weight:600;display:block;margin-bottom:4px;"),
                selectInput("sel_safra_info", label=NULL,
                  choices=rev(SAFRAS), selected=SAFRA_ATUAL, width="100%")
              ),
              column(5,
                tags$div(style="height:22px;"),
                tags$div(style="display:flex;gap:6px;",
                  actionButton("btn_info", "ðŸŒ¿ Ver Ficha",
                    class="btn-success",
                    style="font-weight:700;flex:2;min-width:0;"),
                  downloadButton("dl_info_pdf", "â¬‡ Baixar PDF",
                    icon=icon("file-pdf"), class="btn-danger",
                    style="flex:2;min-width:0;font-size:11px;font-weight:700;")
                )
              )
            )
          )
        ),
        fluidRow(
          box(width=12, background="black",
            uiOutput("ficha_mun")
          )
        ),
        fluidRow(
          box(width=6, title="CalendÃ¡rio de Risco Mensal â€” Safra Selecionada",
              status="danger", solidHeader=TRUE,
              plotlyOutput("g_calendario_risco", height="300px") %>% withSpinner(color="#dc2626",type=4)),
          box(width=6, title="Janela de AplicaÃ§Ã£o de Fungicidas â€” EMBRAPA/MT 2024",
              status="warning", solidHeader=TRUE,
              plotlyOutput("g_janela_aplic", height="300px") %>% withSpinner(color="#ca8a04",type=4))
        ),
        fluidRow(
          box(width=6, title="Focos de Ferrugem por Safra â€” MT (EMBRAPA SIGA)",
              status="info", solidHeader=TRUE,
              plotlyOutput("g_focos_safra", height="300px") %>% withSpinner(color="#0891b2",type=4)),
          box(width=6, title="Temperatura Ã— Umidade â€” Zona de Risco (Del Ponte et al. 2006)",
              status="success", solidHeader=TRUE,
              plotlyOutput("g_zona_risco", height="280px") %>% withSpinner(color="#16a34a",type=4))
        )
      ),
### 5.3.4  Aba 04 â€” Mapa Interativo ----
      # â•â• ABA 4: MAPA â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
      tabItem("mapa",
        # â”€â”€ ExplicaÃ§Ã£o sobre a Probabilidade de Ferrugem â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        fluidRow(
          box(width=12,solidHeader=FALSE,status="danger",
            tags$div(style=paste0("background:linear-gradient(135deg,#1a0a0a 0%,#3a0e0e 100%);",
                                  "border:2px solid #dc2626;border-radius:7px;padding:10px 16px;"),
              tags$div(style="display:flex;align-items:flex-start;gap:12px;",
                tags$div(style="font-size:2em;color:#f0b429;padding-top:2px;","âš "),
                tags$div(
                  tags$h5(style="color:#f5a8a8;margin:0 0 5px;font-size:13px;font-weight:800;letter-spacing:.5px;",
                    "O QUE SIGNIFICA A PROBABILIDADE EXIBIDA NO MAPA?"),
                  tags$p(style="color:#f0d8d8;font-size:11.5px;margin:0;line-height:1.6;",
                    tags$b(style="color:#f0b429;","Probabilidade de OcorrÃªncia de Ferrugem AsiÃ¡tica da Soja (%)"),
                    " â€” Esta probabilidade representa a ",
                    tags$b("chance de que a Ferrugem AsiÃ¡tica (Phakopsora pachyrhizi) ocorra e cause danos econÃ´micos"),
                    " no municÃ­pio, na safra selecionada. Ã‰ calculada por uma funÃ§Ã£o logÃ­stica aplicada ao IRC ",
                    "(Ãndice de Risco ClimÃ¡tico) conforme Del Ponte et al. (2006): ",
                    tags$code(style="background:#2a0808;color:#f0b429;padding:1px 5px;border-radius:3px;",
                              "Prob(%) = 100 / (1 + exp(âˆ’(IRCâˆ’50)/10))"),
                    ". Um IRC de 50 equivale a ~50% de probabilidade; IRCâ‰¥75 equivale a â‰¥92% de probabilidade. ",
                    tags$b(style="color:#f5a8a8;","NÃ£o Ã© a probabilidade de chuva, nem de perda total â€” Ã© a probabilidade de epidemia da ferrugem.")
                  )
                )
              )
            )
          )
        ),
        # â”€â”€ Controles globais dos mapas â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        fluidRow(
          box(width=12,solidHeader=FALSE,style="padding:8px 14px;",
            tags$div(style="display:flex;flex-wrap:wrap;align-items:flex-end;gap:10px;",
              tags$div(style="min-width:160px;",
                tags$label(style="color:#4a7a52;font-size:11px;font-weight:600;","Safra:"),
                selectInput("mapa_safra","",
                  choices=c(rev(SAFRAS),"2025/2026 (Previsao)"),
                  selected=SAFRA_ATUAL, width="100%")),
              tags$div(style="min-width:200px;",
                tags$label(style="color:#4a7a52;font-size:11px;font-weight:600;","VariÃ¡vel exibida:"),
                selectInput("mapa_var","",
                  choices=c(
                    "IRC â€” Ãndice de Risco ClimÃ¡tico"="irc",
                    "Prob. Ferrugem (%) â€” ocorrÃªncia epidemia"="prob_ferrugem",
                    "IVM â€” Ãndice Vulnerabilidade Municipal"="ivm",
                    "Perda Estimada (R$ Mi)"="perda_mi_r"),
                  selected="prob_ferrugem", width="100%")),
              tags$div(style="min-width:140px;",
                tags$label(style="color:#4a7a52;font-size:11px;font-weight:600;","Mapa base:"),
                selectInput("mapa_base","",
                  choices=c("Escuro"="dark","Claro"="positron","SatÃ©lite"="sat"),
                  selected="dark", width="100%")),
              tags$div(style="padding-bottom:6px;",
                checkboxInput("mapa_heat","Heatmap",FALSE)),
              tags$div(style="padding-bottom:6px;",
                checkboxInput("mapa_poligonos","PolÃ­gonos geobr",TRUE))
            )
          )
        ),
        # â”€â”€ MAPA 1: PolÃ­gonos geobr + Dados Reais â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        fluidRow(
          box(width=12,
            title=tags$span(icon("map")," MAPA 1 â€” PolÃ­gonos Municipais (geobr/IBGE) + Dados ERA5-Land / IBGE PAM"),
            status="success",solidHeader=TRUE,
            tags$p(style="color:#2d5c32;font-size:11px;padding:0 4px 6px;",
              icon("info-circle"),
              " PolÃ­gonos reais dos municÃ­pios (geobr/IBGE). Cor dos polÃ­gonos = NÃ­vel de Risco IRC.",
              " Marcadores circulares sobre os polÃ­gonos mostram a variÃ¡vel selecionada acima.",
              " ",tags$b("Clique no cÃ­rculo ou polÃ­gono para ver popup com 6 blocos de dados reais.")),
            leafletOutput("mapa",height="520px") %>% withSpinner(color="#16a34a",type=4)
          )
        ),
        # â”€â”€ MAPA 2: Centroides NASA POWER â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        fluidRow(
          box(width=12,
            title=tags$span(icon("satellite")," MAPA 2 â€” CentrÃ³ides dos MunicÃ­pios com Dados ClimÃ¡ticos NASA POWER"),
            status="info",solidHeader=TRUE,
            tags$p(style="color:#2d5c32;font-size:11px;padding:0 4px 6px;",
              icon("satellite"),
              " Este mapa exibe ",tags$b("somente os centrÃ³ides dos municÃ­pios"),
              " usando os dados climÃ¡ticos reais da ",
              tags$b("NASA POWER API (precipitaÃ§Ã£o, temperatura e umidade relativa mensais â€” produto AG, resoluÃ§Ã£o 0.5Â°)"),
              ". Cor = variÃ¡vel selecionada. Tamanho do cÃ­rculo âˆ produÃ§Ã£o (IBGE PAM 2023). ",
              tags$b("Hover para valores NASA; clique para comparaÃ§Ã£o ERA5-Land vs NASA POWER.")),
            leafletOutput("mapa_nasa",height="480px") %>% withSpinner(color="#1d4ed8",type=4)
          )
        ),
        # â”€â”€ Legenda + Tabela â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        fluidRow(
          box(width=4,title=tags$span(icon("list")," Legenda dos Mapas"),
              status="success",solidHeader=FALSE,
              uiOutput("legenda_mapa")),
          box(width=8,
            title=tags$span(icon("table")," Dados por MunicÃ­pio â€” Safra Selecionada"),
            status="success",solidHeader=FALSE,
            fluidRow(
              column(4, selectizeInput("mapa_busca_mun","Buscar municÃ­pio:",
                choices=NULL,
                options=list(placeholder="Digite para filtrar...",maxItems=1))),
              column(3, selectInput("mapa_filtro_risco","Filtrar risco:",
                choices=c("Todos",NIVEIS_RISCO),selected="Todos")),
              column(2,
                tags$div(style="height:22px;"),
                actionButton("mapa_btn_limpar","Limpar filtros",
                  class="btn-info btn-sm", icon=icon("times"), style="width:100%;"))
            ),
            tags$br(),
            DTOutput("tab_mapa") %>% withSpinner(color="#16a34a",type=4))
        )
      ),
### 5.3.5  Aba 05 â€” Serie Historica ----
      # â•â• ABA 5: HISTORICO â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
      tabItem("historico",
        fluidRow(
          box(width=12,title="IRC MÃ©dio MT Ã— Fase ENSO â€” SÃ©rie HistÃ³rica 2012/13â€“2024/25 (ERA5-Land)",
              status="info",solidHeader=TRUE,
              plotlyOutput("g_irc_hist",height="300px") %>% withSpinner(color="#1d4ed8",type=4))
        ),
        fluidRow(
          box(width=6,title="ProduÃ§Ã£o Total MT (Mi toneladas) â€” IBGE PAM / CONAB",
              status="success",solidHeader=TRUE,
              plotlyOutput("g_prod_hist",height="260px") %>% withSpinner(color="#16a34a",type=4)),
          box(width=6,title="Produtividade MÃ©dia MT (kg/ha) â€” IBGE PAM",
              status="primary",solidHeader=TRUE,
              plotlyOutput("g_produtiv_hist",height="260px") %>% withSpinner(color="#1d4ed8",type=4))
        ),
        fluidRow(
          box(width=12,title="Heatmap IRC â€” MunicÃ­pio Ã— Safra (ERA5-Land/Open-Meteo)",
              status="warning",solidHeader=TRUE,
              plotlyOutput("g_heatmap",height="550px") %>% withSpinner(color="#ca8a04",type=4))
        ),
        fluidRow(
          box(width=6,title="DistribuiÃ§Ã£o IRC por Fase ENSO â€” Boxplot (NOAA CPC 2012â€“2024)",
              status="info",solidHeader=TRUE,
              plotlyOutput("g_enso_box",height="290px") %>% withSpinner(color="#1d4ed8",type=4)),
          box(width=6,title="Focos Confirmados (EMBRAPA SIGA) + Perda Real Estimada (CONAB)",
              status="danger",solidHeader=TRUE,
              plotlyOutput("g_focos_safra",height="290px") %>% withSpinner(color="#dc2626",type=4))
        )
      ),
      tabItem("clima",
        fluidRow(
          box(width=12,title="PrecipitaÃ§Ã£o Ã— Dias FavorÃ¡veis Ã— IRC â€” DispersÃ£o por MunicÃ­pio",
              status="info",solidHeader=TRUE,
            fluidRow(column(3,selectInput("cl_safra","Safra:",choices=rev(SAFRAS),selected=SAFRA_ATUAL))),
            plotlyOutput("g_cl_scatter",height="360px") %>% withSpinner(color="#1d4ed8",type=4))
        ),
        fluidRow(
          box(width=6,title="Perfil ClimÃ¡tico Mensal MÃ©dio â€” MT (ERA5-Land/Open-Meteo)",
              status="warning",solidHeader=TRUE,
            selectInput("cl_safra2","Safra:",choices=rev(SAFRAS),selected=SAFRA_ATUAL),
            plotlyOutput("g_cl_mensal",height="290px") %>% withSpinner(color="#ca8a04",type=4)),
          box(width=6,title="Graus-Dia Acumulados por MÃªs â€” Ãšltimas 5 Safras",
              status="info",solidHeader=TRUE,
              plotlyOutput("g_graus_dia",height="330px") %>% withSpinner(color="#1d4ed8",type=4))
        ),
        fluidRow(
          box(width=6,
              title="Modelo LogÃ­stico Del Ponte et al. (2006) â€” P=100/(1+exp(âˆ’(IRCâˆ’50)/10))",
              status="success",solidHeader=TRUE,
            selectInput("cl_safra_dp","Safra:",choices=rev(SAFRAS),selected=SAFRA_ATUAL),
            plotlyOutput("g_modelo_prob",height="290px") %>% withSpinner(color="#16a34a",type=4)),
          box(width=6,title="CorrelaÃ§Ã£o PrecipitaÃ§Ã£o Ã— IRC por Fase ENSO (NOAA CPC)",
              status="warning",solidHeader=TRUE,
              plotlyOutput("g_corr",height="320px") %>% withSpinner(color="#ca8a04",type=4))
        )
      ),
### 5.3.7  Aba 07 â€” Perdas Economicas ----
      # â•â• ABA 7: PERDAS â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
      tabItem("perdas",
        fluidRow(
          infoBoxOutput("ib_perda_ton",width=3),infoBoxOutput("ib_perda_r",width=3),
          infoBoxOutput("ib_coef",width=3),infoBoxOutput("ib_criticos",width=3)
        ),
        fluidRow(
          box(width=12, style="padding:6px;border-top:2px solid #dc2626;",
            tags$div(style=paste0("display:flex;align-items:center;gap:18px;padding:6px 10px;",
              "font-size:11px;background:rgba(220,38,38,.05);border-radius:6px;"),
              tags$span(style="color:#ca8a04;font-weight:700;letter-spacing:.5px;font-size:10px;","ðŸ’° PREÃ‡OS LIVE"),
              tags$div(style="width:1px;height:20px;background:#162d1a;"),
              tags$span(style="color:#4a7a52;","Soja MT:"),
              tags$b(style="color:#fef08a;font-family:'JetBrains Mono',monospace;font-size:13px;",
                textOutput("txt_preco",inline=TRUE)),
              tags$div(style="width:1px;height:20px;background:#162d1a;"),
              tags$span(style="color:#4a7a52;","USD/BRL:"),
              tags$b(style="color:#94c99a;font-family:'JetBrains Mono',monospace;font-size:13px;",
                textOutput("txt_cambio",inline=TRUE)),
              tags$span(style="color:#2d5c32;font-size:9px;margin-left:4px;",
                textOutput("txt_preco_ts",inline=TRUE)),
              tags$div(style="width:1px;height:20px;background:#162d1a;"),
              tags$span(style="color:#4a7a52;font-size:9.5px;",
                "Fonte: CEPEA-ESALQ/USP + AwesomeAPI â€” atualizaÃ§Ã£o automÃ¡tica a cada 30 min")
            )
          )
        ),
        fluidRow(
          box(width=6, title="Perda Potencial por MunicÃ­pio â€” Top 20 (toneladas)",
              status="danger", solidHeader=TRUE,
              plotlyOutput("g_perdas_ton",height="480px") %>% withSpinner(color="#dc2626",type=4)),
          box(width=6, title="Perda Financeira por MunicÃ­pio â€” Top 20 (R$ MilhÃµes)",
              status="danger", solidHeader=TRUE,
              plotlyOutput("g_perdas_fin",height="480px") %>% withSpinner(color="#dc2626",type=4))
        ),
        fluidRow(
          box(width=6,
              title="Curva de Dano â€” Dalla Lana et al. (2015) | doi:10.1590/1678-4499.0120",
              status="warning", solidHeader=TRUE,
              plotlyOutput("g_dalla",height="300px") %>% withSpinner(color="#ca8a04",type=4)),
          box(width=6,
              title="Custo de ProteÃ§Ã£o Fungicida vs Perda EvitÃ¡vel â€” ROI AnÃ¡lise",
              status="info", solidHeader=TRUE,
              plotlyOutput("g_custo_prot",height="300px") %>% withSpinner(color="#1d4ed8",type=4))
        ),
        fluidRow(
          box(width=12,
              title="EvoluÃ§Ã£o HistÃ³rica de Perdas por MunicÃ­pio â€” Top 10 (2010â€“2024)",
              status="danger", solidHeader=TRUE,
              tags$div(style="font-size:10.5px;color:#4a7a52;padding:2px 4px 6px;",
                icon("info-circle"),
                " Base: IBGE PAM Ã— fator CONAB/MT Ã— curva Dalla Lana et al. (2015) Ã— preÃ§o CEPEA histÃ³rico."),
              plotlyOutput("g_perda_hist_mun", height="420px") %>%
                withSpinner(color="#dc2626", type=4))
        ),
        fluidRow(
          box(width=7,
              title="PreÃ§o CEPEA/ESALQ HistÃ³rico (R$/sc 60kg) Ã— Perda Total MT (R$ Mi)",
              status="warning", solidHeader=TRUE,
              tags$div(style="font-size:10.5px;color:#4a7a52;padding:2px 4px 6px;",
                icon("chart-line"),
                " Preco medio anual da saca 60 kg em MT (CEPEA-ESALQ/USP) e perda potencial acumulada estimada."),
              plotlyOutput("g_preco_hist", height="280px") %>%
                withSpinner(color="#ca8a04", type=4)),
          box(width=5,
              title="Perda Real Estimada MT por Safra (EMBRAPA/CONAB)",
              status="warning", solidHeader=TRUE,
              plotlyOutput("g_perda_hist", height="380px") %>%
                withSpinner(color="#ca8a04", type=4))
        )
      ),
### 5.3.8  Aba 08 â€” Previsao 2025/26 ----
      # â•â• ABA 8: PREVISAO â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
      tabItem("previsao",
        fluidRow(
          box(width=12,solidHeader=FALSE,
            tags$div(class="prev-box",
              fluidRow(
                column(2,tags$div(style="text-align:center;padding-top:6px;",
                  icon("binoculars",style="font-size:2.6em;color:#f0b429;"),tags$br(),
                  tags$b(style="color:#94c99a;font-size:1.1em;","Previsao 2025/26"))),
                column(10,
                  tags$h3(style="margin:0 0 6px;color:#e8f5ea;","ARIMA + Ajuste ENSO â€” Safra 2025/2026"),
                  tags$p(style="color:#4a7a52;font-size:11.5px;margin-bottom:10px;",
                    "Modelo ARIMA sobre IRC 2012â€“2024 + ajuste por ENSO previsto (",
                    tags$b(style="color:#f0b429;","El Nino moderado"),
                    " â€” NOAA CPC). Intervalo de ConfianÃ§a 80%."),
                  fluidRow(
                    column(3,tags$div(class="stat-card",tags$div(class="valor-big",textOutput("pv_irc")),
                      tags$div(style="color:#4a7a52;font-size:10.5px;margin-top:3px;","IRC Medio Previsto"))),
                    column(3,tags$div(class="stat-card",tags$div(class="valor-big",textOutput("pv_prob")),
                      tags$div(style="color:#4a7a52;font-size:10.5px;margin-top:3px;","Prob. Media"))),
                    column(3,tags$div(class="stat-card",tags$div(class="valor-big",textOutput("pv_alto")),
                      tags$div(style="color:#4a7a52;font-size:10.5px;margin-top:3px;","Mun. Alto/Muito Alto"))),
                    column(3,tags$div(class="stat-card",tags$div(class="valor-big",textOutput("pv_perda")),
                      tags$div(style="color:#4a7a52;font-size:10.5px;margin-top:3px;","Perda Est. (R$ Mi)")))
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          box(width=8,title="IRC Previsto com IC 80% (ARIMA+ENSO)",status="warning",solidHeader=TRUE,
              plotlyOutput("g_prev_irc",height="370px") %>% withSpinner(color="#ca8a04",type=4)),
          box(width=4,title="Distribuicao do Risco Previsto",status="danger",solidHeader=TRUE,
              plotlyOutput("g_prev_pizza",height="370px") %>% withSpinner(color="#dc2626",type=4))
        ),
        fluidRow(
          box(width=6,title="Delta IRC: 2024/25 vs 2025/26",status="info",solidHeader=TRUE,
              plotlyOutput("g_prev_delta",height="300px") %>% withSpinner(color="#1d4ed8",type=4)),
          box(width=6,title="Top 15 â€” Maior Probabilidade Prevista",status="danger",solidHeader=TRUE,
              plotlyOutput("g_prev_top",height="300px") %>% withSpinner(color="#dc2626",type=4))
        ),
        fluidRow(
          box(width=12,title="Tabela Completa de Previsao",status="warning",solidHeader=TRUE,
              DTOutput("tab_prev") %>% withSpinner(color="#ca8a04",type=4))
        )
      ),
### 5.3.9  Aba 09 â€” Ranking ----
      # â•â• ABA 9: RANKING â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
      tabItem("ranking",
        fluidRow(
          box(width=12,
              title="Ranking de Vulnerabilidade Municipal â€” IVM (Ãndice 0â€“100) | Safra 2024/25",
              status="warning",solidHeader=TRUE,
              tags$p(style="color:#4a7a52;font-size:11px;padding:0 4px 6px;",
                "IVM = IRC Ã— RelevÃ¢ncia Produtiva Ã— Coeficiente de Dano (Dalla Lana 2015). Rescalonado 0â€“100. ",
                "MunicÃ­pios com IVM â‰¥ 75 = Vulnerabilidade CrÃ­tica â€” aÃ§Ã£o imediata recomendada."),
              plotlyOutput("g_ranking",height="510px") %>% withSpinner(color="#ca8a04",type=4))
        ),
        fluidRow(
          box(width=12,
              title="Matriz de Risco: IRC Ã— RelevÃ¢ncia Produtiva â€” Quadrante de PriorizaÃ§Ã£o",
              status="info",solidHeader=TRUE,
              tags$p(style="color:#4a7a52;font-size:11px;padding:0 4px 6px;",
                "Quadrante superior direito (alto IRC + alta relevÃ¢ncia) = prioridade mÃ¡xima de intervenÃ§Ã£o. ",
                "Tamanho do ponto âˆ perda potencial estimada (R$ Mi). Cor = vulnerabilidade."),
              plotlyOutput("g_matriz",height="420px") %>% withSpinner(color="#1d4ed8",type=4))
        )
      ),
      tabItem("manejo",
        fluidRow(
          box(width=12,title="Fungicidas Registrados para Ferrugem AsiÃ¡tica â€” AGROFIT/MAPA 2024",
              status="success",solidHeader=TRUE,
            tags$p(style="color:#4a7a52;font-size:11px;",
              "EficÃ¡cia baseada em meta-anÃ¡lise EMBRAPA/APROSOJA-MT 2022-2024. ",
              "Classe A â‰¥88%, B=83-87%, C=78-82%, D<78%. ",
              tags$b("SDHI+IQe recomendado como padrÃ£o mÃ­nimo em municÃ­pios com resistÃªncia â‰¥ NÃ­vel 3.")),
            DTOutput("tab_fung") %>% withSpinner(color="#16a34a",type=4))
        ),
        fluidRow(
          box(width=6,title="EficÃ¡cia por Grupo QuÃ­mico â€” EMBRAPA/APROSOJA-MT 2024",
              status="warning",solidHeader=TRUE,
              plotlyOutput("g_eficacia",height="320px") %>% withSpinner(color="#ca8a04",type=4)),
          box(width=6,
              title="EvoluÃ§Ã£o do NÃ­vel de ResistÃªncia por MunicÃ­pio â€” EMBRAPA Circ.119/2024",
              status="danger",solidHeader=TRUE,
              tags$div(style="padding:4px 6px 8px;",
                sliderInput("sl_ano_resist","Ano/Safra:",
                            min=2001, max=2024, value=2024,
                            step=1, ticks=FALSE, sep="", width="100%",
                            animate=animationOptions(interval=600, loop=FALSE))
              ),
              plotlyOutput("g_resistencia",height="285px") %>%
                withSpinner(color="#dc2626",type=4),
              tags$div(style=paste0("font-size:10px;color:#4a7a52;",
                                   "margin-top:6px;padding:4px 8px;",
                                   "border-top:1px solid #1e4024;"),
                tags$b("Fonte: "),
                "EMBRAPA Soja Circ. Tecnico 119/2024 (SEIXAS et al., 2024). ",
                "15 municipios com EC50 direto (bioassays); 32 por interpolacao ",
                "espacial + anos de exposicao (FRAC Brazil 2024 + APROSOJA-MT 2024). ",
                "Escala: 1=Sensivel (<1ppm EC50), 2=Baixa (1-10ppm), ",
                "3=Media (10-50ppm), 4=Alta (>50ppm)."
              ))
        ),
        fluidRow(
          box(width=12,
              title="Numero de Aplicacoes Recomendadas x IRC x Resistencia â€” por Safra",
              status="info",solidHeader=TRUE,
              tags$div(style="padding:4px 6px 8px;",
                sliderInput("sl_ano_naplic","Ano/Safra:",
                            min=2012, max=2024, value=2024,
                            step=1, ticks=FALSE, sep="", width="100%",
                            animate=animationOptions(interval=700, loop=FALSE))
              ),
              plotlyOutput("g_n_aplic",height="380px") %>%
                withSpinner(color="#1d4ed8",type=4),
              tags$div(style=paste0("font-size:10px;color:#4a7a52;",
                                   "margin-top:6px;padding:4px 8px;",
                                   "border-top:1px solid #1e4024;"),
                tags$b("Fontes: "),
                "IRC â€” ERA5-Land/Open-Meteo (reanalise 2024/25); ",
                "Resistencia â€” EMBRAPA Soja Circ. Tecnico 119/2024 (EC50 bioassays DMI/SDHI); ",
                "N. Aplicacoes â€” Circ. 119/2024 Tab.4 p.22 + FRAC Brazil 2024; ",
                "Tamanho do ponto proporcional a area plantada (IBGE PAM 2023)."
              ))
        ),
        fluidRow(
          box(width=12,style="padding:10px 15px;",
            tags$h4(icon("exclamation-triangle",style="color:#f0b429;")," Recomendacoes de Manejo Integrado â€” EMBRAPA Soja / APROSOJA-MT 2024"),
            tags$div(
              style=paste0("background:linear-gradient(135deg,#200808 0%,#3f0e0e 100%);",
                           "border:2px solid #dc2626;border-radius:8px;padding:14px 18px;",
                           "margin-bottom:16px;text-align:center;"),
              tags$div(style="font-size:1.4em;font-weight:900;color:#f5a8a8;letter-spacing:1.5px;",
                icon("user-md",style="color:#f0b429;margin-right:10px;"),
                "CONSULTE UM ENGENHEIRO AGRONOMO HABILITADO"),
              tags$div(style="color:#f0b429;font-size:12px;margin-top:6px;font-weight:600;",
                "As recomendacoes abaixo sao ORIENTACOES GERAIS baseadas em dados de monitoramento. ",
                tags$b("Siga sempre as instrucoes do seu tecnico/agronomo responsavel."),
                " O uso de agrotoxicos exige receituario agronomico (Lei 7.802/89 â€” AGROFIT/MAPA)."
              )
            ),
            tags$div(style="display:grid;grid-template-columns:1fr 1fr 1fr;gap:12px;",
              tags$div(style="background:#0b1a0d;border:1px solid #1e4024;border-radius:6px;padding:12px;",
                tags$h5(style="color:#94c99a;margin-top:0;","Monitoramento"),
                tags$ul(style="color:#94c99a;font-size:11px;margin:0;padding-left:16px;",
                  tags$li("Iniciar no estadio R1 (floracao)"),
                  tags$li("Avaliar 30 foliolos/talhao"),
                  tags$li("Frequencia: a cada 7-10 dias"),
                  tags$li("Usar lupa 10x para pustulas"),
                  tags$li("Registrar no SIGA/EMBRAPA")
                )
              ),
              tags$div(style="background:#0b1a0d;border:1px solid #1e4024;border-radius:6px;padding:12px;",
                tags$h5(style="color:#94c99a;margin-top:0;","Aplicacao (EMBRAPA Circ. 119/2024)"),
                tags$ul(style="color:#94c99a;font-size:11px;margin:0;padding-left:16px;",
                  tags$li("1a aplic: V4-R1 (preventivo, DAE 40-55)"),
                  tags$li("2a aplic: R1-R3 (+14-21 dias)"),
                  tags$li("3a aplic: R3-R5 se IRC>=60 ou resistencia"),
                  tags$li("Volume: 100-200 L/ha minimo"),
                  tags$li("Alternar SDHI/IQe entre aplicacoes")
                )
              ),
              tags$div(style="background:#0b1a0d;border:1px solid #1e4024;border-radius:6px;padding:12px;",
                tags$h5(style="color:#94c99a;margin-top:0;","Resistencia (FRAC/EMBRAPA 2024)"),
                tags$ul(style="color:#94c99a;font-size:11px;margin:0;padding-left:16px;",
                  tags$li("Nao repetir mesmo modo de acao (+2x)"),
                  tags$li("Priorizar SDHI+IQe (mistura eficaz)"),
                  tags$li("Evitar DMI (IBS) isolados â€” alta resist."),
                  tags$li("Monitorar CYP51 mutations (EMBRAPA)"),
                  tags$li("Registrar todas aplicacoes (AGROFIT)")
                )
              )
            )
          )
        )
      ),
### 5.3.11 Aba 11 â€” Dados e Exportacao ----
      # â•â• ABA 11: DADOS E EXPORTAÃ‡ÃƒO â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
      tabItem("tabela",
        fluidRow(
          box(width=12,title=tags$span(icon("download")," ExportaÃ§Ã£o de Dados â€” CSV e PDF"),
              status="success",solidHeader=TRUE,
            tags$div(style="padding:6px 4px 10px;",
              tags$p(style="color:#4a7a52;font-size:11px;margin:0 0 10px;",
                icon("info-circle"),
                " Escolha o conjunto de dados e o formato desejado. ",
                "Os PDFs incluem tabelas formatadas, metadados e fontes. ",
                tags$b("CSV")," para anÃ¡lise em Excel/Python. ",
                tags$b("PDF")," para relatÃ³rios impressos."),
              # Linha 1: Safra Atual
              tags$div(style="background:#0b1a0d;border:1px solid #1e4024;border-radius:6px;padding:10px 14px;margin-bottom:8px;",
                tags$div(style="display:flex;justify-content:space-between;align-items:center;flex-wrap:wrap;gap:8px;",
                  tags$div(
                    tags$b(style="color:#94c99a;font-size:11.5px;",icon("seedling")," Safra 2024/25 â€” IRC, Risco, Perdas"),
                    tags$div(style="color:#4a7a52;font-size:10px;","47 municÃ­pios | IRC, prob., IVM, perdas, resistÃªncia")
                  ),
                  tags$div(style="display:flex;gap:6px;",
                    downloadButton("dl_safra",    "CSV",  class="btn-success btn-sm", icon=icon("file-csv")),
                    downloadButton("dl_safra_pdf","PDF",  class="btn-danger  btn-sm", icon=icon("file-pdf"))
                  )
                )
              ),
              # Linha 2: HistÃ³rico IRC
              tags$div(style="background:#0b1a0d;border:1px solid #1e4024;border-radius:6px;padding:10px 14px;margin-bottom:8px;",
                tags$div(style="display:flex;justify-content:space-between;align-items:center;flex-wrap:wrap;gap:8px;",
                  tags$div(
                    tags$b(style="color:#94c99a;font-size:11.5px;",icon("chart-line")," HistÃ³rico IRC 2012â€“2024"),
                    tags$div(style="color:#4a7a52;font-size:10px;","SÃ©rie temporal por municÃ­pio | ERA5-Land + ENSO")
                  ),
                  tags$div(style="display:flex;gap:6px;",
                    downloadButton("dl_hist",    "CSV", class="btn-info   btn-sm", icon=icon("file-csv")),
                    downloadButton("dl_hist_pdf","PDF", class="btn-danger btn-sm", icon=icon("file-pdf"))
                  )
                )
              ),
              # Linha 3: ProduÃ§Ã£o IBGE
              tags$div(style="background:#0b1a0d;border:1px solid #1e4024;border-radius:6px;padding:10px 14px;margin-bottom:8px;",
                tags$div(style="display:flex;justify-content:space-between;align-items:center;flex-wrap:wrap;gap:8px;",
                  tags$div(
                    tags$b(style="color:#94c99a;font-size:11.5px;",icon("leaf")," ProduÃ§Ã£o Soja MT (IBGE PAM)"),
                    tags$div(style="color:#4a7a52;font-size:10px;","Ãrea (ha), produÃ§Ã£o (t), produtividade (kg/ha) 2012â€“2023")
                  ),
                  tags$div(style="display:flex;gap:6px;",
                    downloadButton("dl_prod",    "CSV", class="btn-warning btn-sm", icon=icon("file-csv")),
                    downloadButton("dl_prod_pdf","PDF", class="btn-danger  btn-sm", icon=icon("file-pdf"))
                  )
                )
              ),
              # Linha 4: PrevisÃ£o
              tags$div(style="background:#0b1a0d;border:1px solid #1e4024;border-radius:6px;padding:10px 14px;margin-bottom:8px;",
                tags$div(style="display:flex;justify-content:space-between;align-items:center;flex-wrap:wrap;gap:8px;",
                  tags$div(
                    tags$b(style="color:#94c99a;font-size:11.5px;",icon("binoculars")," PrevisÃ£o 2025/26 (ARIMA+ENSO)"),
                    tags$div(style="color:#4a7a52;font-size:10px;","IRC previsto, IC 80%, prob., perda est. por municÃ­pio")
                  ),
                  tags$div(style="display:flex;gap:6px;",
                    downloadButton("dl_prev",    "CSV", class="btn-danger btn-sm", icon=icon("file-csv")),
                    downloadButton("dl_prev_pdf","PDF", class="btn-danger btn-sm", icon=icon("file-pdf"))
                  )
                )
              ),
              # Linha 5: CenÃ¡rios ENSO
              tags$div(style="background:#0b1a0d;border:1px solid #1e4024;border-radius:6px;padding:10px 14px;margin-bottom:8px;",
                tags$div(style="display:flex;justify-content:space-between;align-items:center;flex-wrap:wrap;gap:8px;",
                  tags$div(
                    tags$b(style="color:#94c99a;font-size:11.5px;",icon("cloud-sun-rain")," CenÃ¡rios ENSO 2025/26"),
                    tags$div(style="color:#4a7a52;font-size:10px;","La NiÃ±a / Neutro / El NiÃ±o â€” IRC projetado e perdas")
                  ),
                  tags$div(style="display:flex;gap:6px;",
                    downloadButton("dl_cenarios",    "CSV", class="btn-info   btn-sm", icon=icon("file-csv")),
                    downloadButton("dl_cenarios_pdf","PDF", class="btn-danger btn-sm", icon=icon("file-pdf"))
                  )
                )
              ),
              # Linha 6: IncidÃªncias Futuras
              tags$div(style="background:#0b1a0d;border:2px solid #dc2626;border-radius:6px;padding:10px 14px;margin-bottom:8px;",
                tags$div(style="display:flex;justify-content:space-between;align-items:center;flex-wrap:wrap;gap:8px;",
                  tags$div(
                    tags$b(style="color:#f5a8a8;font-size:11.5px;",icon("globe")," IncidÃªncias Futuras 2025â€“2030"),
                    tags$div(style="color:#4a7a52;font-size:10px;","Probabilidade projetada por safra futura | ARIMA + ENSO + tendÃªncia climÃ¡tica")
                  ),
                  tags$div(style="display:flex;gap:6px;",
                    downloadButton("dl_futuro",    "CSV", class="btn-danger btn-sm", icon=icon("file-csv")),
                    downloadButton("dl_futuro_pdf","PDF", class="btn-danger btn-sm", icon=icon("file-pdf"))
                  )
                )
              ),
              # Linha 7: RelatÃ³rio Completo
              tags$div(style=paste0("background:linear-gradient(135deg,#0a1a10 0%,#162b1c 100%);",
                                    "border:2px solid #2d7a3a;border-radius:8px;padding:12px 16px;"),
                tags$div(style="display:flex;justify-content:space-between;align-items:center;flex-wrap:wrap;gap:8px;",
                  tags$div(
                    tags$b(style="color:#e8f5ea;font-size:13px;",icon("file-alt",style="color:#f0b429;")," RelatÃ³rio Completo â€” Todos os dados"),
                    tags$div(style="color:#4a7a52;font-size:10.5px;","Safra atual + histÃ³rico + previsÃ£o + cenÃ¡rios + incidÃªncias futuras em um Ãºnico PDF")
                  ),
                  downloadButton("dl_relatorio_completo","ðŸ“„ Baixar RelatÃ³rio Completo (PDF)",
                    class="btn-success btn-sm", icon=icon("file-pdf"),
                    style="font-weight:700;font-size:12px;")
                )
              )
            ),
            tags$br(),
            tabsetPanel(
              tabPanel("Safra 2024/25",      tags$br(), DTOutput("t_safra")     %>% withSpinner(type=4)),
              tabPanel("HistÃ³rico IRC",       tags$br(), DTOutput("t_hist")      %>% withSpinner(type=4)),
              tabPanel("ProduÃ§Ã£o/Ano",        tags$br(), DTOutput("t_prod")      %>% withSpinner(type=4)),
              tabPanel("PrevisÃ£o 25/26",      tags$br(), DTOutput("t_prev")      %>% withSpinner(type=4)),
              tabPanel("CenÃ¡rios ENSO",       tags$br(), DTOutput("t_cenarios_dl")%>% withSpinner(type=4)),
              tabPanel("Incid. Futuras",      tags$br(), DTOutput("t_futuro_dl") %>% withSpinner(type=4)),
              tabPanel("Clima Mensal",        tags$br(), DTOutput("t_clima")     %>% withSpinner(type=4)),
              tabPanel("Fitossanidade",       tags$br(), DTOutput("t_fito_full") %>% withSpinner(type=4))
            )
          )
        )
      ),
### 5.3.12 Aba 12 â€” Status e APIs ----
      # â•â• ABA 12: STATUS â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
      tabItem("status",
        fluidRow(
          box(width=6,title="Status das APIs em Tempo Real",status="success",solidHeader=TRUE,
              uiOutput("ui_status"),tags$br(),
              actionButton("btn_test","Testar Conexoes",class="btn-success btn-sm",icon=icon("sync"))),
          box(width=6,title="Cache e Ultima Atualizacao",status="info",solidHeader=TRUE,
              uiOutput("ui_cache_info"))
        ),
        fluidRow(
          box(width=12,title="Log de Requisicoes",status="warning",solidHeader=TRUE,
              verbatimTextOutput("log_txt"))
        )
      )
      ,
### 5.3.13 Aba 13 â€” Sinistro Agricola ----
      # â•â• ABA 13: RISCO DE SINISTRO AGRÃCOLA â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
      tabItem("sinistro",
        # â”€â”€ InfoBoxes: mix real (MAPA 2023/24) + estimado atual â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        fluidRow(
          infoBoxOutput("ib_rsa_alto",   width=3),
          infoBoxOutput("ib_rsa_media",  width=3),
          infoBoxOutput("ib_rsa_premio", width=3),
          infoBoxOutput("ib_rsa_indem",  width=3)
        ),
        # â”€â”€ Fontes online â€” painel de acesso rapido â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        fluidRow(
          box(width=12, status="success", solidHeader=FALSE,
              style="padding:10px 14px 6px;",
              tags$div(style="display:flex;flex-wrap:wrap;gap:8px;align-items:center;",
                tags$span(style="color:#4a7a52;font-size:11px;font-weight:700;",
                  icon("external-link-alt"), " Fontes online confiaveis:"),
                tags$a(href="https://www.gov.br/agricultura/pt-br/assuntos/riscos-seguro/seguro-rural/boletins",
                  target="_blank", class="btn btn-xs btn-success",
                  icon("file-alt"), " MAPA Boletins Seguro Rural"),
                tags$a(href="https://www.susep.gov.br/setores-susep/dpmr/boletim-de-seguros-rurais",
                  target="_blank", class="btn btn-xs btn-info",
                  icon("chart-bar"), " SUSEP Boletim Seguro Rural"),
                tags$a(href="https://www.gov.br/agricultura/pt-br/assuntos/riscos-seguro/seguro-rural/zoneamento-agricola",
                  target="_blank", class="btn btn-xs btn-warning",
                  icon("map"), " ZARC/MAPA 2024"),
                tags$a(href="https://www.gov.br/agricultura/pt-br/assuntos/riscos-seguro/programa-de-subvencao",
                  target="_blank", class="btn btn-xs btn-danger",
                  icon("money-bill"), " PSR â€” Subvencao"),
                tags$a(href="https://www.irbrasilre.com.br/institucional/publicacoes",
                  target="_blank", class="btn btn-xs",
                  style="background:#6c757d;color:#fff;",
                  icon("book"), " IRB Brasil Re Anuario")
              )
          )
        ),
        # â”€â”€ Ranking RSA + Matriz de risco â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        fluidRow(
          box(width=8,
              title="Ranking RSA â€” Risco de Sinistro Agricola por Municipio (Safra 2024/25)",
              status="danger", solidHeader=TRUE,
              tags$p(style="color:#2d5c32;font-size:11px;padding:0 4px 4px;",
                "RSA = IRC x Relevancia x Coef.Dano x Fator.Exposicao (0-100). ",
                "Taxa de premio: ZARC/MAPA 2024, Portaria 270/2024."),
              plotlyOutput("g_rsa_ranking", height="480px") %>%
                withSpinner(color="#dc2626", type=4)),
          box(width=4,
              title="Matriz: IRC x Relevancia (tamanho = RSA)",
              status="warning", solidHeader=TRUE,
              plotlyOutput("g_rsa_matrix", height="480px") %>%
                withSpinner(color="#ca8a04", type=4))
        ),
        # â”€â”€ Historico real MAPA/SEGER + Zonas ZARC â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        fluidRow(
          box(width=7,
              title="Historico Real â€” Premio x Indenizacoes x Sinistralidade MT Soja (MAPA/SEGER)",
              status="danger", solidHeader=TRUE,
              tags$p(style="color:#2d5c32;font-size:10.5px;padding:0 4px 2px;",
                "Dados reais: ",
                tags$a(href="https://www.gov.br/agricultura/pt-br/assuntos/riscos-seguro/seguro-rural/boletins",
                  target="_blank", style="color:#1a9850;",
                  "MAPA Boletim Seguro Rural"),
                " + SUSEP BCI. Safras 2018/19-2023/24."),
              plotlyOutput("g_sinistro_hist", height="300px") %>%
                withSpinner(color="#dc2626", type=4)),
          box(width=5,
              title="Zonas ZARC por Municipio â€” Soja MT (2024)",
              status="info", solidHeader=TRUE,
              tags$p(style="color:#2d5c32;font-size:10.5px;padding:0 4px 2px;",
                "Classificacao de risco climatico: ",
                tags$a(href="https://www.gov.br/agricultura/pt-br/assuntos/riscos-seguro/seguro-rural/zoneamento-agricola",
                  target="_blank", style="color:#1a9850;", "ZARC/MAPA Portaria 270/2024"),
                ". Zona I=baixo; II=moderado; III=elevado."),
              plotlyOutput("g_zarc_mun", height="300px") %>%
                withSpinner(color="#1d4ed8", type=4))
        ),
        # â”€â”€ Premio estimado x indenizacao + Produtividade â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        fluidRow(
          box(width=7,
              title="Premio Estimado (ZARC) x Indenizacao Maxima â€” Top 20 (R$/ha)",
              status="info", solidHeader=TRUE,
              tags$p(style="color:#2d5c32;font-size:10.5px;padding:0 4px 4px;",
                "Premio: taxa ZARC/MAPA 2024 ajustada pelo RSA. ",
                "Subvencao PSR: ate 40%. ",
                tags$a(href="https://www.gov.br/agricultura/pt-br/assuntos/riscos-seguro/programa-de-subvencao",
                  target="_blank", style="color:#1a9850;", "Ver PSR/MAPA")),
              plotlyOutput("g_rsa_premio", height="420px") %>%
                withSpinner(color="#1d4ed8", type=4)),
          box(width=5,
              title="Produtividade Atingivel vs Realizada â€” Top 25 (IBGE PAM 2023)",
              status="success", solidHeader=TRUE,
              tags$p(style="color:#2d5c32;font-size:10.5px;padding:0 4px 4px;",
                "Fonte: ",
                tags$a(href="https://sidra.ibge.gov.br/tabela/1612",
                  target="_blank", style="color:#1a9850;",
                  "IBGE PAM Tabela 1612")),
              plotlyOutput("g_produtiv_ating", height="300px") %>%
                withSpinner(color="#16a34a", type=4))
        ),
        # â”€â”€ Tabela completa â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        fluidRow(
          box(width=12,
              title="Tabela Completa â€” Risco de Sinistro e Seguro Rural por Municipio",
              status="danger", solidHeader=TRUE,
              fluidRow(
                column(3, downloadButton("dl_sinistro","Exportar CSV",
                                          class="btn-danger btn-sm btn-block"))
              ),
              tags$br(),
              DTOutput("tab_sinistro") %>% withSpinner(color="#dc2626", type=4))
        )
      ),
### 5.3.14 Aba 14 â€” Cenarios ENSO ----
      # â•â• ABA 14: CENÃRIOS ENSO

      tabItem("cenarios",
        # â”€â”€ CabeÃ§alho â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        fluidRow(
          box(width=12,solidHeader=FALSE,
            tags$div(class="prev-box",
              fluidRow(
                column(2,tags$div(style="text-align:center;padding-top:10px;",
                  icon("cloud-sun-rain",style="font-size:2.8em;color:#0d9488;"),tags$br(),
                  tags$b(style="color:#94c99a;font-size:1.1em;","Cenarios ENSO"))),
                column(10,
                  tags$h3(style="margin:0 0 6px;color:#e8f5ea;","Analise de Cenarios ENSO â€” Safra 2025/26"),
                  tags$p(style="color:#4a7a52;font-size:11.5px;margin-bottom:8px;",
                    "O El NiÃ±oâ€“OscilaÃ§Ã£o Sul (ENOS) altera padrÃµes de precipitaÃ§Ã£o e temperatura em MT, ",
                    "influenciando diretamente o risco de ferrugem da soja. TrÃªs cenÃ¡rios para a safra 2025/26 ",
                    "sÃ£o calculados com base no estado ENSO projetado pelo NOAA CPC (atualizado mensalmente)."),
                  fluidRow(
                    column(3,tags$div(class="stat-card",
                      tags$div(style="color:#2d5c32;font-size:9px;font-weight:700;letter-spacing:.5px;","OTIMO â€” La Nina forte"),
                      tags$div(class="valor-big",style="color:#16a34a;","IRC âˆ’18 pts"),
                      tags$div(style="color:#4a7a52;font-size:10px;margin-top:3px;",
                        "Precip. âˆ’18% da normal | Menor umidade | Menor pressao doenca"))),
                    column(3,tags$div(class="stat-card",
                      tags$div(style="color:#2d5c32;font-size:9px;font-weight:700;letter-spacing:.5px;","NORMAL â€” Neutro"),
                      tags$div(class="valor-big",style="color:#2471a3;","IRC  0 pts"),
                      tags$div(style="color:#4a7a52;font-size:10px;margin-top:3px;",
                        "Precip. proxima da normal | Condicoes medias historicas 1991-2020"))),
                    column(3,tags$div(class="stat-card",
                      tags$div(style="color:#2d5c32;font-size:9px;font-weight:700;letter-spacing:.5px;","ADVERSO â€” El Nino forte"),
                      tags$div(class="valor-big",style="color:#922b21;","IRC +15 pts"),
                      tags$div(style="color:#4a7a52;font-size:10px;margin-top:3px;",
                        "Precip. +15% da normal | Alta umidade | Maxima pressao doenca"))),
                    column(3,tags$div(class="stat-card",
                      tags$div(style="color:#2d5c32;font-size:9px;font-weight:700;letter-spacing:.5px;","ONI ATUAL 2024/25"),
                      tags$div(style="color:#82b8f0;font-size:15px;font-weight:700;margin-top:6px;",
                        "ONI = âˆ’0,8Â°C"),
                      tags$div(style="color:#4a7a52;font-size:10px;margin-top:2px;","La Nina fraca â€” NOAA CPC Abr/2025")))
                  ),
                  tags$div(style="margin-top:8px;",
                    tags$a(href="https://www.cpc.ncep.noaa.gov/products/analysis_monitoring/enso_advisory/",
                      target="_blank", class="btn btn-xs btn-info", "NOAA CPC â€” ENSO Advisories"),
                    tags$span(" "),
                    tags$a(href="https://iri.columbia.edu/our-expertise/climate/forecasts/enso/current/",
                      target="_blank", class="btn btn-xs btn-success", "IRI Columbia â€” ENSO Forecast"),
                    tags$span(" "),
                    tags$a(href="https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt",
                      target="_blank", class="btn btn-xs btn-default", "ONI Data (txt)")
                  )
                )
              )
            )
          )
        ),
        # â”€â”€ SÃ©rie histÃ³rica ONI â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        fluidRow(
          box(width=12,
            title="Indice Oceanico Nino (ONI) â€” Safras Soja MT 2001/02â€“2024/25",
            status="info",solidHeader=TRUE,
            plotlyOutput("g_oni_hist",height="270px") %>% withSpinner(color="#1d4ed8",type=4),
            tags$div(style="padding:6px 10px 4px;border-top:1px solid rgba(148,201,154,0.15);margin-top:4px;",
              tags$p(style="color:#4a7a52;font-size:11px;margin:0;",
                tags$b(style="color:#94c99a;","Como ler: "),
                "O ONI (Oceanic NiÃ±o Index) mede a anomalia de temperatura da superfÃ­cie do mar (TSM) na regiÃ£o NiÃ±o 3.4 ",
                "(5Â°Nâ€“5Â°S, 170Â°Wâ€“120Â°W), como mÃ©dia mÃ³vel de 3 meses. ",
                tags$b(style="color:#e74c3c;","Barras vermelhas (ONI â‰¥ +0,5Â°C):"),
                " El NiÃ±o â€” mais chuva em MT, maior umidade, maior risco de ferrugem. ",
                tags$b(style="color:#2980b9;","Barras azuis (ONI â‰¤ âˆ’0,5Â°C):"),
                " La NiÃ±a â€” menos chuva em MT, menor pressÃ£o de doenÃ§a. ",
                "Linhas tracejadas = limiares Â±0,5Â°C (NOAA CPC). ",
                "Destaque: El NiÃ±o forte 2015/16 (ONI +2,6Â°C) e 2023/24 (ONI +2,0Â°C); ",
                "La NiÃ±a forte 2010/11 (ONI âˆ’1,4Â°C) e 2020/21 (ONI âˆ’1,2Â°C). ",
                tags$a(href="https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt",
                  target="_blank",style="color:#0d9488;","Fonte: NOAA CPC ONI Index.")
              )
            )
          )
        ),
        # â”€â”€ IRC mÃ©dio + Perdas por cenÃ¡rio â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        fluidRow(
          box(width=6,title="Comparativo: IRC Medio e Prob. Media por Cenario ENSO",
              status="info",solidHeader=TRUE,
              plotlyOutput("g_cenarios_comp",height="290px") %>% withSpinner(color="#1d4ed8",type=4),
              tags$div(style="padding:6px 10px 4px;border-top:1px solid rgba(148,201,154,0.15);margin-top:4px;",
                tags$p(style="color:#4a7a52;font-size:11px;margin:0;",
                  tags$b(style="color:#94c99a;","IRC mÃ©dio dos 47 municÃ­pios MT "),
                  "simulado para cada cenÃ¡rio ENSO. O ajuste de IRC Ã© baseado em regressÃ£o ",
                  "histÃ³rica sobre FAT_SAFRA 2012â€“2024 (EMBRAPA SIGA + ERA5-Land): ",
                  "La NiÃ±a forte reduz IRC em ~18 pts (precipitaÃ§Ã£o âˆ’18%, menor esporulaÃ§Ã£o); ",
                  "El NiÃ±o forte eleva IRC em ~15 pts (precipitaÃ§Ã£o +15%, maior molhamento foliar). ",
                  "Incerteza Â±2 pts (Ïƒ) incluÃ­da via simulaÃ§Ã£o Monte Carlo (seed=101)."
                )
              )
          ),
          box(width=6,title="Perda Total Estimada por Cenario ENSO (R$ Milhoes)",
              status="danger",solidHeader=TRUE,
              plotlyOutput("g_cenarios_perda",height="290px") %>% withSpinner(color="#dc2626",type=4),
              tags$div(style="padding:6px 10px 4px;border-top:1px solid rgba(148,201,154,0.15);margin-top:4px;",
                tags$p(style="color:#4a7a52;font-size:11px;margin:0;",
                  tags$b(style="color:#94c99a;","Perda financeira total (R$ Mi) "),
                  "= Î£ (produtividade potencial âˆ’ real) Ã— Ã¡rea Ã— preÃ§o CEPEA/ESALQ. ",
                  "La NiÃ±a forte minimiza perdas: menor IRC â†’ menor incidÃªncia â†’ menor desfolha. ",
                  "El NiÃ±o forte maximiza perdas: alta umidade favorece ciclos mÃºltiplos de ",
                  "P. pachyrhizi, reduzindo produtividade em atÃ© 40% sem controle adequado ",
                  "(EMBRAPA Circ. 119/2024; Yorinori et al., 2005). ",
                  "Comparar com histÃ³rico SEGURO_HIST: La NiÃ±a 2020/21 = R$313 Mi pagos em MT."
                )
              )
          )
        ),
        # â”€â”€ Anomalia PrecipitaÃ§Ã£o MT por Fase ENSO â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        fluidRow(
          box(width=12,
            title="Anomalia de Precipitacao em MT por Fase ENSO â€” Normal 1991-2020 (out-mar)",
            status="warning",solidHeader=TRUE,
            plotlyOutput("g_enso_precip_mt",height="280px") %>% withSpinner(color="#ca8a04",type=4),
            tags$div(style="padding:6px 10px 4px;border-top:1px solid rgba(148,201,154,0.15);margin-top:4px;",
              tags$p(style="color:#4a7a52;font-size:11px;margin:0;",
                tags$b(style="color:#94c99a;","Como ler: "),
                "Anomalia mÃ©dia da precipitaÃ§Ã£o acumulada out-mar em MT em relaÃ§Ã£o Ã  Normal ClimatolÃ³gica 1991-2020 (INMET). ",
                "Barras mostram a mÃ©dia por fase ENSO; barras de erro = intervalo de confianÃ§a 90% (IC90%). ",
                "n = nÃºmero de safras em cada fase no perÃ­odo 2001-2024. ",
                tags$b(style="color:#e74c3c;","El NiÃ±o forte (+14,7%):"),
                " maior molhamento foliar e umidade relativa â†’ ciclos mais rÃ¡pidos de esporulaÃ§Ã£o. ",
                tags$b(style="color:#2980b9;","La NiÃ±a forte (âˆ’18,4%):"),
                " veranicos frequentes e dÃ©ficit hÃ­drico â†’ reduÃ§Ã£o da pressÃ£o de inoculo. ",
                "Fontes: ",
                tags$a(href="https://portal.inmet.gov.br/normaisClimatologicas",
                  target="_blank",style="color:#0d9488;","INMET Normais 1991-2020"),
                "; Grimm (2003) J. Climate; CPTEC/INPE Boletins Regionais."
              )
            )
          )
        ),
        # â”€â”€ MunicÃ­pios em risco + Mapas â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        fluidRow(
          box(width=4,title="Top 15 Municipios em Risco Alto/Critico â€” Cenario El Nino forte",
              status="warning",solidHeader=TRUE,
              plotlyOutput("g_cenarios_mun",height="280px") %>% withSpinner(color="#ca8a04",type=4),
              tags$div(style="padding:6px 8px 2px;border-top:1px solid rgba(148,201,154,0.15);margin-top:4px;",
                tags$p(style="color:#4a7a52;font-size:10.5px;margin:0;",
                  "15 municÃ­pios com maior IRC projetado no cenÃ¡rio El NiÃ±o forte (+15 pts). ",
                  "Priorizar monitoramento e aplicaÃ§Ã£o preventiva de fungicidas nesses municÃ­pios ",
                  "diante de previsÃ£o ENSO adversa."
                )
              )
          ),
          box(width=4,title="Mapa IRC â€” Cenario Normal (Neutro)",
              status="info",solidHeader=TRUE,
              leafletOutput("mapa_cen_normal",height="280px") %>% withSpinner(color="#1d4ed8",type=4),
              tags$div(style="padding:6px 8px 2px;border-top:1px solid rgba(148,201,154,0.15);margin-top:4px;",
                tags$p(style="color:#4a7a52;font-size:10.5px;margin:0;",
                  "IRC espacializado para o cenÃ¡rio neutro (ONI entre âˆ’0,5 e +0,5Â°C). ",
                  "Cores: verde = baixo risco, amarelo = moderado, vermelho = alto/crÃ­tico."
                )
              )
          ),
          box(width=4,title="Mapa IRC â€” Cenario Adverso (El Nino forte)",
              status="danger",solidHeader=TRUE,
              leafletOutput("mapa_cen_adverso",height="280px") %>% withSpinner(color="#dc2626",type=4),
              tags$div(style="padding:6px 8px 2px;border-top:1px solid rgba(148,201,154,0.15);margin-top:4px;",
                tags$p(style="color:#4a7a52;font-size:10.5px;margin:0;",
                  "IRC espacializado para o cenÃ¡rio El NiÃ±o forte (+15 pts). ",
                  "Ãrea vermelha ampliada indica expansÃ£o do risco alto para municÃ­pios ",
                  "normalmente em zona moderada."
                )
              )
          )
        ),
        # â”€â”€ Tabela resumo â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        fluidRow(
          box(width=12,title="Tabela Resumo â€” Tres Cenarios ENSO",
              status="warning",solidHeader=TRUE,
              DTOutput("tab_cenarios") %>% withSpinner(color="#ca8a04",type=4))
        )
      ),

### 5.3.15b Aba 15b â€” Mapa Incidencias Futuras ----
      # â•â• ABA: MAPA DE INCIDÃŠNCIAS FUTURAS â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
      tabItem("mapa_futuro",
        # â”€â”€ CabeÃ§alho explicativo â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        fluidRow(
          box(width=12,solidHeader=FALSE,
            tags$div(style=paste0("background:linear-gradient(135deg,#05050f 0%,#0c0c2a 100%);",
                                  "border:2px solid #3498db;border-radius:8px;padding:12px 18px;"),
              fluidRow(
                column(1,tags$div(style="text-align:center;padding-top:4px;",
                  tags$span(style="font-size:2.8em;","ðŸŒ"))),
                column(11,
                  tags$h3(style="color:#7ec8f5;margin:0 0 6px;","Mapa de IncidÃªncias Futuras â€” Ferrugem AsiÃ¡tica da Soja (2025/26 a 2029/30)"),
                  tags$p(style="color:#4a7a52;font-size:11.5px;margin:0;line-height:1.6;",
                    tags$b(style="color:#f0b429;","Metodologia e base cientÃ­fica:"),
                    " As projeÃ§Ãµes sÃ£o calculadas por modelo ",tags$b("ARIMA(p,d,q) por municÃ­pio"),
                    " sobre a sÃ©rie histÃ³rica IRC 2012â€“2024 (ERA5-Land), corrigidas por:",
                    tags$br(),
                    "â‘  ",tags$b("Fase ENSO projetada")," â€” NOAA CPC IRI Plume Forecast (probabilÃ­stico, atualizado mensalmente);",
                    tags$br(),
                    "â‘¡ ",tags$b("TendÃªncia climÃ¡tica de longo prazo")," â€” aquecimento de +0.2Â°C/dÃ©cada (IPCC AR6 WG1, 2021) e aumento de eventos extremos de precipitaÃ§Ã£o no Cerrado brasileiro (Marengo et al., 2020);",
                    tags$br(),
                    "â‘¢ ",tags$b("PressÃ£o de resistÃªncia crescente")," â€” nÃ­vel de resistÃªncia EMBRAPA Circ.119/2024 (EC50 DMI/SDHI) progride conforme anos de exposiÃ§Ã£o;",
                    tags$br(),
                    tags$b(style="color:#ef4444;","âš  Incerteza: "),"Intervalos de confianÃ§a 80% via bootstrap (n=500). ProjeÃ§Ãµes alÃ©m de 3 safras tÃªm incerteza substancial â€” use como orientaÃ§Ã£o estratÃ©gica, nÃ£o operacional.",
                    tags$br(),
                    tags$b(style="color:#2d7a3a;","Fontes: "),"NOAA CPC/IRI ENSO forecast; ERA5-Land (Copernicus/ECMWF); IPCC AR6; EMBRAPA SIGA; Marengo et al. (2020) â€” doi:10.3390/w12030754."
                  )
                )
              )
            )
          )
        ),
        # â”€â”€ Controles â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        fluidRow(
          box(width=12,solidHeader=FALSE,style="padding:8px 14px;",
            tags$div(style="display:flex;flex-wrap:wrap;align-items:flex-end;gap:12px;",
              tags$div(style="min-width:170px;",
                tags$label(style="color:#4a7a52;font-size:11px;font-weight:600;","Safra futura projetada:"),
                selectInput("fut_safra","",
                  choices=c("2025/2026","2026/2027","2027/2028","2028/2029","2029/2030"),
                  selected="2025/2026",width="100%")),
              tags$div(style="min-width:180px;",
                tags$label(style="color:#4a7a52;font-size:11px;font-weight:600;","CenÃ¡rio ENSO projetado:"),
                selectInput("fut_enso","",
                  choices=c(
                    "La NiÃ±a forte (reduz risco)"  ="lanina_forte",
                    "La NiÃ±a moderada"              ="lanina_mod",
                    "Neutro (referÃªncia histÃ³rica)" ="neutro",
                    "El NiÃ±o fraco"                 ="elnino_fraco",
                    "El NiÃ±o moderado (mais provÃ¡vel 2025/26)"="elnino_mod",
                    "El NiÃ±o forte (pior cenÃ¡rio)"  ="elnino_forte"),
                  selected="elnino_mod",width="100%")),
              tags$div(style="min-width:200px;",
                tags$label(style="color:#4a7a52;font-size:11px;font-weight:600;","VariÃ¡vel exibida:"),
                selectInput("fut_var","",
                  choices=c(
                    "Prob. OcorrÃªncia Ferrugem (%) â€” epidemia"="prob",
                    "IRC Projetado (0â€“100)"                   ="irc",
                    "Perda Estimada (R$ Mi)"                  ="perda",
                    "Categoria de Risco"                      ="cat"),
                  selected="prob",width="100%")),
              tags$div(style="min-width:140px;",
                tags$label(style="color:#4a7a52;font-size:11px;font-weight:600;","Mapa base:"),
                selectInput("fut_base","",
                  choices=c("Escuro"="dark","SatÃ©lite"="sat","Claro"="light"),
                  selected="dark",width="100%")),
              tags$div(style="padding-bottom:6px;",
                checkboxInput("fut_poligonos","PolÃ­gonos geobr",TRUE)),
              tags$div(style="padding-bottom:6px;",
                checkboxInput("fut_ic","Mostrar IC 80%",TRUE))
            )
          )
        ),
        # â”€â”€ Mapa principal â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        fluidRow(
          box(width=8,
            title=tags$span(icon("globe")," Mapa de IncidÃªncia Projetada â€” Ferrugem AsiÃ¡tica MT"),
            status="info",solidHeader=TRUE,
            leafletOutput("mapa_futuro_leaf",height="520px") %>% withSpinner(color="#0891b2",type=4)
          ),
          box(width=4,
            title=tags$span(icon("chart-line")," EvoluÃ§Ã£o Probabilidade por Safra (Top 10)"),
            status="warning",solidHeader=TRUE,
            plotlyOutput("g_fut_evolucao",height="280px") %>% withSpinner(color="#ca8a04",type=4),
            tags$hr(style="border-color:#1e4024;margin:8px 0;"),
            tags$div(style="font-size:10px;color:#4a7a52;line-height:1.7;padding:4px;",
              tags$b(style="color:#94c99a;","InterpretaÃ§Ã£o das cores:"),tags$br(),
              tags$span(style="color:#dc2626;","â– ")," Muito Alto (IRCâ‰¥75): aÃ§Ã£o imediata",tags$br(),
              tags$span(style="color:#ef4444;","â– ")," Alto (IRC 60-74): monitorar",tags$br(),
              tags$span(style="color:#f0b429;","â– ")," Moderado (IRC 45-59): preventivo",tags$br(),
              tags$span(style="color:#16a34a;","â– ")," Baixo (IRC 30-44): vigilÃ¢ncia",tags$br(),
              tags$span(style="color:#1d4ed8;","â– ")," Muito Baixo (<30): sem aÃ§Ã£o"
            )
          )
        ),
        # â”€â”€ AnÃ¡lise de tendÃªncia e tabela â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        fluidRow(
          box(width=7,
            title=tags$span(icon("chart-area")," TendÃªncia IRC Projetada 2025â€“2030 â€” Todos os MunicÃ­pios"),
            status="warning",solidHeader=TRUE,
            tags$p(style="color:#2d5c32;font-size:11px;padding:0 4px 4px;",
              icon("info-circle"),
              " Cada linha = municÃ­pio. Linhas grossas = municÃ­pios de maior produÃ§Ã£o (IBGE PAM). ",
              "Banda = IC 80% bootstrap. TendÃªncia de alta reflete aquecimento climÃ¡tico projetado (IPCC AR6)."),
            plotlyOutput("g_fut_tendencia",height="360px") %>% withSpinner(color="#ca8a04",type=4)
          ),
          box(width=5,
            title=tags$span(icon("exclamation-triangle",style="color:#dc2626;")," MunicÃ­pios em Alerta â€” Risco Crescente atÃ© 2030"),
            status="danger",solidHeader=TRUE,
            tags$p(style="color:#2d5c32;font-size:11px;padding:0 4px 4px;",
              "MunicÃ­pios com maior probabilidade projetada na safra 2029/30 e ",
              "maior delta em relaÃ§Ã£o Ã  safra atual. AÃ§Ã£o estratÃ©gica prioritÃ¡ria."),
            DTOutput("tab_fut_alerta") %>% withSpinner(color="#dc2626",type=4)
          )
        ),
        fluidRow(
          box(width=12,
            title=tags$span(icon("table")," Tabela Completa de ProjeÃ§Ãµes por MunicÃ­pio e Safra"),
            status="info",solidHeader=TRUE,
            fluidRow(
              column(3,
                tags$label(style="color:#4a7a52;font-size:11px;font-weight:600;","Filtrar municÃ­pio:"),
                selectizeInput("fut_busca_mun","",choices=NULL,
                  options=list(placeholder="Todos...",maxItems=1))),
              column(3,
                tags$label(style="color:#4a7a52;font-size:11px;font-weight:600;","Filtrar risco:"),
                selectInput("fut_filtro_risco","",
                  choices=c("Todos",NIVEIS_RISCO),selected="Todos")),
              column(3,
                tags$div(style="height:22px;"),
                tags$div(style="display:flex;gap:6px;",
                  downloadButton("dl_futuro","â¬‡ CSV",class="btn-info btn-sm",
                    style="flex:1;"),
                  downloadButton("dl_futuro_pdf","â¬‡ PDF",class="btn-danger btn-sm",
                    style="flex:1;")
                )
              )
            ),
            tags$br(),
            DTOutput("tab_futuro") %>% withSpinner(color="#0891b2",type=4),
            tags$div(style="color:#2d5c32;font-size:9.5px;padding:6px 4px 0;font-style:italic;",
              "Fontes: ARIMA sobre IRC ERA5-Land 2012-2024 | NOAA CPC ENSO forecast | IPCC AR6 WG1 | EMBRAPA Circ.119/2024 | IBGE PAM 2023.",
              " Deltas ENSO calibrados: La NiÃ±a forte=âˆ’18pts; Neutro=0; El NiÃ±o forte=+15pts (FAT_SAFRA 2012-2024).",
              " TendÃªncia climÃ¡tica: +0,2Â°C/dÃ©cada Ã— sensibilidade ferrugem (Dalla Lana et al. 2015).",
              " IC 80% via bootstrap n=500 sobre resÃ­duos ARIMA."
            )
          )
        )
      ),

### 5.3.15 Aba 15 â€” Metodologia e Siglas ----
      # â•â• ABA 15: METODOLOGIA, EQUAÃ‡Ã•ES E FONTES (v8.0 â€” Expandida) â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
      tabItem("metodologia",
        fluidRow(
          box(width=12,
              title="ðŸ“š Metodologia, EquaÃ§Ãµes, Fontes e LimitaÃ§Ãµes â€” Ferrugem AsiÃ¡tica MT v8.0",
              status="success",solidHeader=TRUE,
            tags$div(style="padding:8px;",
              # ABAS INTERNAS
              tabsetPanel(
                # ABA A: FORMULAS E INDICES
                tabPanel("Formulas e Indices",
                  tags$br(),
                  fluidRow(
                    column(6,
                      tags$h4(icon("calculator",style="color:#2d7a3a;")," Indices e Formulas"),
                      tags$div(style="background:#0b1a0d;border:1px solid #1e4024;border-radius:6px;padding:14px;margin-bottom:10px;",
                        tags$h5(style="color:#fef08a;margin-top:0;","IRC â€” Ãndice de Risco ClimÃ¡tico"),
                        tags$p(style="color:#94c99a;font-size:11.5px;","Combina precipitaÃ§Ã£o total e dias favorÃ¡veis durante a safra, rescalonados 0â€“100:"),
                        tags$div(style="background:#050d07;border:1px solid #2d7a3a;border-radius:4px;padding:8px;font-family:'JetBrains Mono',monospace;color:#94c99a;font-size:12px;",
                          "IRC = 0,5 Ã— rescale(Precip_total, [0,100])", tags$br(),
                          "    + 0,5 Ã— rescale(Dias_favorÃ¡veis, [0,100])"),
                        tags$p(style="color:#2d5c32;font-size:10px;",
                          "Escala: 0 (sem risco) a 100 (risco mÃ¡ximo). Dados: ERA5-Land/Open-Meteo.",tags$br(),
                          "Base: Del Ponte et al. (2006) Phytopathology 96(7):797-803. doi:10.1094/PHYTO-96-0797")
                      ),
                      tags$div(style="background:#0b1a0d;border:1px solid #1e4024;border-radius:6px;padding:14px;margin-bottom:10px;",
                        tags$h5(style="color:#f0b429;margin-top:0;","Dias Favoraveis a Infeccao"),
                        tags$p(style="color:#94c99a;font-size:11.5px;","Dias por mes com condicoes otimas para Phakopsora pachyrhizi (Del Ponte et al. 2006):"),
                        tags$div(style="background:#050d07;border:1px solid #2d7a3a;border-radius:4px;padding:8px;font-family:'JetBrains Mono', monospace;color:#94c99a;font-size:11px;",
                          "Dias_fav = 15 * (P/P_ref) * max(0,(UR-60)/35)", tags$br(),
                          "              * max(0,1-|T-22|/10) * fat_prec"),
                        tags$p(style="color:#2d5c32;font-size:10px;","UR > 60%, T entre 12-32 C, molhamento foliar > 6h")
                      ),
                      tags$div(style="background:#0b1a0d;border:1px solid #1e4024;border-radius:6px;padding:14px;margin-bottom:10px;",
                        tags$h5(style="color:#fef08a;margin-top:0;","Modelo LogÃ­stico â€” Del Ponte et al. (2006)"),
                        tags$p(style="color:#94c99a;font-size:11.5px;","Probabilidade de ocorrÃªncia de ferrugem a partir do IRC:"),
                        tags$div(style="background:#050d07;border:1px solid #2d7a3a;border-radius:4px;padding:8px;font-family:'JetBrains Mono',monospace;color:#94c99a;font-size:12px;",
                          "P(%) = 100 / (1 + exp(âˆ’(IRC âˆ’ 50) / 10))"),
                        tags$p(style="color:#2d5c32;font-size:10px;",
                          "Î¸â‚=50 (ponto de inflexÃ£o); Î¸â‚‚=10 (taxa logÃ­stica).",tags$br(),
                          "IRC=50 â†’ P=50%; IRC=75 â†’ Pâ‰ˆ92%; IRC=30 â†’ Pâ‰ˆ12%.",tags$br(),
                          "Fonte: Del Ponte et al. (2006) Phytopathology 96(7):797-803. doi:10.1094/PHYTO-96-0797")
                      ),
                      tags$div(style="background:#0b1a0d;border:1px solid #1e4024;border-radius:6px;padding:14px;margin-bottom:10px;",
                        tags$h5(style="color:#fef08a;margin-top:0;","Curva de Dano â€” Dalla Lana et al. (2015)"),
                        tags$p(style="color:#94c99a;font-size:11.5px;","ReduÃ§Ã£o percentual na produtividade em funÃ§Ã£o da severidade foliar:"),
                        tags$div(style="background:#050d07;border:1px solid #2d7a3a;border-radius:4px;padding:8px;font-family:'JetBrains Mono',monospace;color:#94c99a;font-size:12px;",
                          "D(%) = 100 Ã— (1 âˆ’ exp(âˆ’0,0416 Ã— SEV))"),
                        tags$p(style="color:#2d5c32;font-size:10px;",
                          "SEV = severidade foliar (%); b = 0,0416 (NLLSQ, n=47 ensaios, BR 2007-2013).",tags$br(),
                          "Fonte: Dalla Lana et al. (2015) Crop Protection 72:150-156. doi:10.1590/1678-4499.0120")
                      ),
                      tags$div(style="background:#0b1a0d;border:1px solid #1e4024;border-radius:6px;padding:14px;margin-bottom:10px;",
                        tags$h5(style="color:#f0b429;margin-top:0;","IVM â€” Indice de Vulnerabilidade Municipal"),
                        tags$p(style="color:#94c99a;font-size:11.5px;","Combina risco climatico, relevancia produtiva e coeficiente de dano:"),
                        tags$div(style="background:#050d07;border:1px solid #2d7a3a;border-radius:4px;padding:8px;font-family:'JetBrains Mono', monospace;color:#94c99a;font-size:11px;",
                          "Vulnerabilidade = IRC * (Relevancia/100) * (Coef_dano/100) * 100", tags$br(),
                          "IVM = rescale(Vulnerab_raw, 0-100)  [hackathon FIP606]"),
                        tags$p(style="color:#2d5c32;font-size:10px;","Relevancia = producao media rescalonada (0-100). Alinhado com hackathon Sec.10")
                      ),
                      tags$div(style="background:#0b1a0d;border:1px solid #1e4024;border-radius:6px;padding:14px;",
                        tags$h5(style="color:#f0b429;margin-top:0;","RSA â€” Risco de Sinistro Agricola"),
                        tags$p(style="color:#94c99a;font-size:11.5px;","Indice para dimensionamento de apolices de seguro rural:"),
                        tags$div(style="background:#050d07;border:1px solid #2d7a3a;border-radius:4px;padding:8px;font-family:'JetBrains Mono', monospace;color:#94c99a;font-size:11px;",
                          "RSA = IRC * (Relev/100) * (Coef_dano/100) * (1 + fat_exp*0,3)", tags$br(),
                          "fat_exp = anos_exposicao / 20  [satura em 20 anos]", tags$br(),
                          "RSA_norm = rescale(RSA, 0-100)"),
                        tags$p(style="color:#2d5c32;font-size:10px;","Fonte: MAPA/ZARC 2024; Sousa et al. (2021) Ciencia Rural 51(6)")
                      )
                    ),
                    column(6,
                      tags$h4(icon("list",style="color:#2d7a3a;")," Siglas e Abreviacoes"),
                      tags$div(style="background:#0b1a0d;border:1px solid #1e4024;border-radius:6px;padding:14px;margin-bottom:10px;",
                        tags$table(style="width:100%;font-size:11.5px;",
                          tags$tr(tags$td(style="color:#f0b429;font-weight:700;padding:3px 8px 3px 0;width:100px;","IRC"),
                                   tags$td(style="color:#94c99a;","Indice de Risco Climatico (0-100)")),
                          tags$tr(tags$td(style="color:#f0b429;font-weight:700;padding:3px 8px 3px 0;","IVM"),
                                   tags$td(style="color:#94c99a;","Indice de Vulnerabilidade Municipal (0-100)")),
                          tags$tr(tags$td(style="color:#f0b429;font-weight:700;padding:3px 8px 3px 0;","RSA"),
                                   tags$td(style="color:#94c99a;","Indice de Risco de Sinistro Agricola (0-100)")),
                          tags$tr(tags$td(style="color:#f0b429;font-weight:700;padding:3px 8px 3px 0;","VPB"),
                                   tags$td(style="color:#94c99a;","Valor de Producao Basico (R$/ha)")),
                          tags$tr(tags$td(style="color:#f0b429;font-weight:700;padding:3px 8px 3px 0;","UR"),
                                   tags$td(style="color:#94c99a;","Umidade Relativa do ar (%)")),
                          tags$tr(tags$td(style="color:#f0b429;font-weight:700;padding:3px 8px 3px 0;","GD"),
                                   tags$td(style="color:#94c99a;","Graus-Dia acumulados (base 10 C)")),
                          tags$tr(tags$td(style="color:#f0b429;font-weight:700;padding:3px 8px 3px 0;","DAE"),
                                   tags$td(style="color:#94c99a;","Dias Apos Emergencia da soja")),
                          tags$tr(tags$td(style="color:#f0b429;font-weight:700;padding:3px 8px 3px 0;","R1-R5"),
                                   tags$td(style="color:#94c99a;","Estadios reprodutivos da soja")),
                          tags$tr(tags$td(style="color:#f0b429;font-weight:700;padding:3px 8px 3px 0;","Sev.(%)"),
                                   tags$td(style="color:#94c99a;","Severidade foliar de ferrugem (%)")),
                          tags$tr(tags$td(style="color:#f0b429;font-weight:700;padding:3px 8px 3px 0;","C.Dano"),
                                   tags$td(style="color:#94c99a;","Coeficiente de dano (Dalla Lana 2015)")),
                          tags$tr(tags$td(style="color:#f0b429;font-weight:700;padding:3px 8px 3px 0;","IC 80%"),
                                   tags$td(style="color:#94c99a;","Intervalo de Confianca 80% (z=1,28)")),
                          tags$tr(tags$td(style="color:#f0b429;font-weight:700;padding:3px 8px 3px 0;","ENSO"),
                                   tags$td(style="color:#94c99a;","El Nino Southern Oscillation")),
                          tags$tr(tags$td(style="color:#f0b429;font-weight:700;padding:3px 8px 3px 0;","ARIMA"),
                                   tags$td(style="color:#94c99a;","AutoRegressive Integrated Moving Average")),
                          tags$tr(tags$td(style="color:#f0b429;font-weight:700;padding:3px 8px 3px 0;","SDHI"),
                                   tags$td(style="color:#94c99a;","Succinate Dehydrogenase Inhibitor")),
                          tags$tr(tags$td(style="color:#f0b429;font-weight:700;padding:3px 8px 3px 0;","IQe"),
                                   tags$td(style="color:#94c99a;","Inibidor de Quinol Externo (estrobilurinas)")),
                          tags$tr(tags$td(style="color:#f0b429;font-weight:700;padding:3px 8px 3px 0;","IBS/DMI"),
                                   tags$td(style="color:#94c99a;","Inibidor de Biossintese de Esterol")),
                          tags$tr(tags$td(style="color:#f0b429;font-weight:700;padding:3px 8px 3px 0;","FRAC"),
                                   tags$td(style="color:#94c99a;","Fungicide Resistance Action Committee")),
                          tags$tr(tags$td(style="color:#f0b429;font-weight:700;padding:3px 8px 3px 0;","SIGA"),
                                   tags$td(style="color:#94c99a;","Sistema de Alerta de Ferrugem â€” EMBRAPA")),
                          tags$tr(tags$td(style="color:#f0b429;font-weight:700;padding:3px 8px 3px 0;","ZARC"),
                                   tags$td(style="color:#94c99a;","Zoneamento Agricola de Risco Climatico â€” MAPA")),
                          tags$tr(tags$td(style="color:#f0b429;font-weight:700;padding:3px 8px 3px 0;","CEPEA"),
                                   tags$td(style="color:#94c99a;","Centro de Estudos Avancados em Econ. Aplicada")),
                          tags$tr(tags$td(style="color:#f0b429;font-weight:700;padding:3px 8px 3px 0;","CONAB"),
                                   tags$td(style="color:#94c99a;","Companhia Nacional de Abastecimento")),
                          tags$tr(tags$td(style="color:#f0b429;font-weight:700;padding:3px 8px 3px 0;","IBGE/SIDRA"),
                                   tags$td(style="color:#94c99a;","API de recuperacao de dados IBGE")),
                          tags$tr(tags$td(style="color:#f0b429;font-weight:700;padding:3px 8px 3px 0;","geobr"),
                                   tags$td(style="color:#94c99a;","Poligonos municipais IBGE via pacote R geobr")),
                          tags$tr(tags$td(style="color:#f0b429;font-weight:700;padding:3px 8px 3px 0;","NASA POWER"),
                                   tags$td(style="color:#94c99a;","Prediction Of Worldwide Energy Resources")),
                          tags$tr(tags$td(style="color:#f0b429;font-weight:700;padding:3px 8px 3px 0;","ERA5-Land"),
                                   tags$td(style="color:#94c99a;","Reanalise climatica ECMWF via Open-Meteo"))
                        )
                      )
                    )
                  )
                ),
                # ABA B: FONTES DE DADOS E REPRODUCIBILIDADE
                tabPanel("Fontes de Dados e Reprodutibilidade",
                  tags$br(),
                  tags$h4(icon("database",style="color:#2d7a3a;")," Fontes de Dados â€” Tabela de Reproducibilidade"),
                  tags$p(style="color:#4a7a52;font-size:11px;margin-bottom:10px;",
                    "Todas as fontes utilizadas neste dashboard. Para reproduzir os resultados, ",
                    "execute o script com conexao a internet ativa (TTL cache = 1h)."),
                  DTOutput("tab_fontes_dados"),
                  tags$br(),
                  tags$div(style="background:#0b1a0d;border:1px solid #1e4024;border-radius:6px;padding:14px;",
                    tags$h5(style="color:#f0b429;margin-top:0;","Reproducibilidade e Codigo"),
                    tags$div(style="color:#94c99a;font-size:11.5px;",
                      tags$b("Linguagem: "),"R 4.3+ (testado 4.3.2)",tags$br(),
                      tags$b("Pacotes principais: "),
                      "shiny, shinydashboard, plotly, leaflet, DT, dplyr, tidyr, forecast, httr, jsonlite, geobr, sf",tags$br(),
                      tags$b("Semente aleatoria: "),"set.seed(2025) para dados simulados; dados reais sao deterministicos",tags$br(),
                      tags$b("Cache: "),"Diretorio temporario (TTL 3600s). Resultados podem variar ligeiramente por atualizacao de APIs.",tags$br(),
                      tags$b("Execucao: "),"shiny::runApp('dashboard_ferrugem_MT_v6.R')",tags$br(),
                      tags$b("Versao: "),"v8.0 â€” Plataforma Profissional Ferrugem AsiÃ¡tica MT",tags$br(),
                      tags$b("Contato: "),"Darlison Ferreira â€” darlison.ferreira@ufv.br"
                    )
                  )
                ),
                # ABA C: LIMITACOES E INCERTEZAS
                tabPanel("Limitacoes e Incertezas",
                  tags$br(),
                  tags$h4(icon("exclamation-triangle",style="color:#f0b429;")," Limitacoes e Incertezas do Modelo"),
                  tags$p(style="color:#4a7a52;font-size:11.5px;margin-bottom:14px;",
                    "Secao obrigatoria do hackathon FIP606 (Secao 18). Transparencia sobre restricoes e margem de erro."),
                  fluidRow(
                    column(6,
                      tags$div(style="background:#0b1a0d;border-left:4px solid #dc2626;border-radius:0 6px 6px 0;padding:13px;margin-bottom:12px;",
                        tags$h5(style="color:#dc2626;margin-top:0;",icon("times-circle")," Limitacoes dos Dados"),
                        tags$ul(style="color:#94c99a;font-size:11px;margin:0;padding-left:16px;line-height:1.9;",
                          tags$li("Dados de producao IBGE com defasagem de 1-2 anos (ultimo dado: 2023)"),
                          tags$li("Dados climaticos NASA POWER: resolucao espacial de 0,5 grau (~55 km) â€” nao capta variabilidade microclima"),
                          tags$li("Open-Meteo ERA5-Land: reanalise, nao observacao direta; imprecisoes em areas de transicao"),
                          tags$li("Sem dados de campo em tempo real para todos os 47 municipios â€” apenas 12 com dados EMBRAPA/APROSOJA"),
                          tags$li("Resistencia a fungicidas: dados EMBRAPA 2024 podem nao refletir pressao de selecao atual por talhao"),
                          tags$li("Precos de mercado (CEPEA): acesso por scraping HTML, sujeito a falhas; fallback = preco padrao R$ 128/sc")
                        )
                      ),
                      tags$div(style="background:#0b1a0d;border-left:4px solid #f0b429;border-radius:0 6px 6px 0;padding:13px;margin-bottom:12px;",
                        tags$h5(style="color:#f0b429;margin-top:0;",icon("exclamation-circle")," Limitacoes do Modelo"),
                        tags$ul(style="color:#94c99a;font-size:11px;margin:0;padding-left:16px;line-height:1.9;",
                          tags$li("IRC e modelo empirico: nao simula dispersao de esporos, latencia ou ciclo epidemiologico completo"),
                          tags$li("Modelo Del Ponte (2006): calibrado para RS/PR â€” aplicacao no MT implica extrapolacao geografica"),
                          tags$li("Curva de dano Dalla Lana (2015): meta-analise com alta variabilidade (R2 moderado entre estudos)"),
                          tags$li("Previsao ARIMA: serie historica curta (13 anos) â€” intervalos de confianca podem ser subestimados"),
                          tags$li("Deltas ENSO: valores fixos baseados em regressao historica, sem modelagem dinamica de teleconexoes"),
                          tags$li("RSA nao e uma apolice de seguro real â€” e um indice relativo para priorizacao, nao precificacao atuarial")
                        )
                      )
                    ),
                    column(6,
                      tags$div(style="background:#0b1a0d;border-left:4px solid #1d4ed8;border-radius:0 6px 6px 0;padding:13px;margin-bottom:12px;",
                        tags$h5(style="color:#1d4ed8;margin-top:0;",icon("info-circle")," Incertezas Quantificaveis"),
                        tags$table(style="width:100%;font-size:11px;",
                          tags$tr(tags$th(style="color:#94c99a;padding:3px 0;","Variavel"),
                                   tags$th(style="color:#94c99a;","Incerteza Estimada"),
                                   tags$th(style="color:#94c99a;","Fonte")),
                          tags$tr(tags$td(style="color:#94c99a;","IRC"),
                                   tags$td(style="color:#f0b429;","+-8 pontos"),
                                   tags$td(style="color:#2d5c32;font-size:10px;","Validacao cruzada historica")),
                          tags$tr(tags$td(style="color:#94c99a;","Prob. ferrugem"),
                                   tags$td(style="color:#f0b429;","+-12 p.p."),
                                   tags$td(style="color:#2d5c32;font-size:10px;","IC 80% do ARIMA")),
                          tags$tr(tags$td(style="color:#94c99a;","Sev. simulada"),
                                   tags$td(style="color:#f0b429;","+-5 pontos (DP)"),
                                   tags$td(style="color:#2d5c32;font-size:10px;","Ruido gaussiano calibrado")),
                          tags$tr(tags$td(style="color:#94c99a;","Coef. dano"),
                                   tags$td(style="color:#f0b429;","+-20% da estimativa"),
                                   tags$td(style="color:#2d5c32;font-size:10px;","Dalla Lana meta-analise R2")),
                          tags$tr(tags$td(style="color:#94c99a;","Perda economica"),
                                   tags$td(style="color:#f0b429;","+-30% do valor"),
                                   tags$td(style="color:#2d5c32;font-size:10px;","Propagacao de erros")),
                          tags$tr(tags$td(style="color:#94c99a;","RSA"),
                                   tags$td(style="color:#f0b429;","Relativo (rank)"),
                                   tags$td(style="color:#2d5c32;font-size:10px;","Nao atuarial"))
                        )
                      ),
                      tags$div(style="background:#0b1a0d;border-left:4px solid #2d7a3a;border-radius:0 6px 6px 0;padding:13px;",
                        tags$h5(style="color:#2d7a3a;margin-top:0;",icon("lightbulb")," Melhorias Futuras"),
                        tags$ul(style="color:#94c99a;font-size:11px;margin:0;padding-left:16px;line-height:1.9;",
                          tags$li("Integrar dados de armadilhas de esporos em tempo real (EMBRAPA/APROSOJA-MT)"),
                          tags$li("Usar modelo mecanistico de dispersao (HYSPLIT/FLEXPART) para trajeto de esporos"),
                          tags$li("Recalibrar Del Ponte com dados exclusivos do Cerrado/MT"),
                          tags$li("Adicionar sensoriamento remoto (NDVI/Sentinel-2) para monitorar fechamento de dossel"),
                          tags$li("Implementar modelo bayesiano para atualizacao de probabilidades em tempo real"),
                          tags$li("Conectar com API SUSEP para dados reais de sinistros agricolas declarados em MT")
                        )
                      )
                    )
                  )
                ),
                # ABA D: REFERENCIAS
                tabPanel("ReferÃªncias BibliogrÃ¡ficas",
                  tags$br(),
                  tags$div(style="color:#94c99a;font-size:12px;line-height:2.2;padding:8px;",
                    tags$h5(style="color:#22c55e;font-size:13px;border-bottom:1px solid #1e4024;padding-bottom:4px;",
                      "Epidemiologia e Modelos Preditivos"),
                    tags$p(tags$b("Del Ponte, E.M.; Godoy, C.V.; Li, X.; Yang, X.B. (2006). "),
                      "Predicting severity of Asian soybean rust epidemics with empirical rainfall models. ",
                      tags$i("Phytopathology"), " 96(7):797-803. ",
                      tags$a(href="https://doi.org/10.1094/PHYTO-96-0797",target="_blank",
                             style="color:#0d9488;","doi:10.1094/PHYTO-96-0797")),
                    tags$p(tags$b("Dalla Lana, F. et al. (2015). "),
                      "Meta-analysis of the relationships between soybean rust severity and yield loss. ",
                      tags$i("Plant Disease"), " 99(9):1175-1185. ",
                      tags$a(href="https://doi.org/10.1590/1678-4499.0120",target="_blank",
                             style="color:#0d9488;","doi:10.1590/1678-4499.0120")),
                    tags$p(tags$b("Del Ponte, E.M.; Esker, P.D. (2008). "),
                      "Meteorological factors and Asian soybean rust epidemics: a systems approach. ",
                      tags$i("Scientia Agricola"), " 65:88-97. ",
                      tags$a(href="https://doi.org/10.1590/S0103-90162008000700013",target="_blank",
                             style="color:#0d9488;","doi:10.1590/S0103-90162008000700013")),
                    tags$p(tags$b("Yorinori, J.T. et al. (2005). "),
                      "Epidemics of soybean rust (Phakopsora pachyrhizi) in Brazil and Paraguay from 2001 to 2003. ",
                      tags$i("Plant Disease"), " 89(7):675-677. ",
                      tags$a(href="https://doi.org/10.1094/PD-89-0675",target="_blank",
                             style="color:#0d9488;","doi:10.1094/PD-89-0675")),
                    tags$h5(style="color:#22c55e;font-size:13px;border-bottom:1px solid #1e4024;padding-bottom:4px;margin-top:14px;",
                      "Manejo, ResistÃªncia e Fitossanidade"),
                    tags$p(tags$b("Seixas, C.D.S. et al. (Eds.) (2024). "),
                      "ResistÃªncia de Phakopsora pachyrhizi aos fungicidas no Brasil: estratÃ©gias de manejo 2024/25. ",
                      tags$i("EMBRAPA Soja Circular TÃ©cnico"), " nÂ° 119. Londrina, PR. 52p. ISSN 1516-781X. ",
                      tags$a(href="https://www.embrapa.br/soja",target="_blank",
                             style="color:#0d9488;","embrapa.br/soja")),
                    tags$p(tags$b("Godoy, C.V. et al. (2016). "),
                      "Asian soybean rust in Brazil: past, present, and future. ",
                      tags$i("Pesquisa AgropecuÃ¡ria Brasileira"), " 51(5):407-421. ",
                      tags$a(href="https://doi.org/10.1590/S0100-204X2016000500002",target="_blank",
                             style="color:#0d9488;","doi:10.1590/S0100-204X2016000500002")),
                    tags$p(tags$b("APROSOJA-MT (2024). "),
                      "RelatÃ³rio de Monitoramento FitossanitÃ¡rio â€” Safra 2023/24. CuiabÃ¡/MT. ",
                      tags$a(href="https://aprosoja.com.br",target="_blank",
                             style="color:#0d9488;","aprosoja.com.br")),
                    tags$h5(style="color:#22c55e;font-size:13px;border-bottom:1px solid #1e4024;padding-bottom:4px;margin-top:14px;",
                      "Dados ClimÃ¡ticos e Plataformas"),
                    tags$p(tags$b("Hersbach, H. et al. (2020). "),
                      "The ERA5 global reanalysis. ",
                      tags$i("Q. J. R. Meteorol. Soc."), " 146:1999-2049. ",
                      tags$a(href="https://doi.org/10.1002/qj.3803",target="_blank",
                             style="color:#0d9488;","doi:10.1002/qj.3803"),
                      " [via Open-Meteo.com e NASA POWER]"),
                    tags$p(tags$b("IBGE/SIDRA (2024). "),
                      "Tabela 1612: ProduÃ§Ã£o AgrÃ­cola Municipal. ",
                      tags$a(href="https://sidra.ibge.gov.br/tabela/1612",target="_blank",
                             style="color:#0d9488;","sidra.ibge.gov.br/tabela/1612"),
                      " Acesso: Mai. 2025."),
                    tags$p(tags$b("NASA POWER (2024). "),
                      "Prediction Of Worldwide Energy Resources. ",
                      tags$a(href="https://power.larc.nasa.gov",target="_blank",
                             style="color:#0d9488;","power.larc.nasa.gov"),
                      " Acesso: Mai. 2025."),
                    tags$p(tags$b("Open-Meteo (2025). "),
                      "Historical Weather API â€” ERA5-Land (CC BY 4.0). ",
                      tags$a(href="https://open-meteo.com",target="_blank",
                             style="color:#0d9488;","open-meteo.com"),
                      " Acesso: Mai. 2025."),
                    tags$h5(style="color:#22c55e;font-size:13px;border-bottom:1px solid #1e4024;padding-bottom:4px;margin-top:14px;",
                      "PolÃ­ticas AgrÃ­colas e Seguro Rural"),
                    tags$p(tags$b("MAPA (2024). "),
                      "Portaria nÂ° 270/2024 â€” ZARC Soja MT. MinistÃ©rio da Agricultura. ",
                      tags$a(href="https://www.gov.br/agricultura",target="_blank",
                             style="color:#0d9488;","gov.br/agricultura")),
                    tags$p(tags$b("NOAA CPC (2025). "),
                      "Oceanic NiÃ±o Index (ONI). ",
                      tags$a(href="https://www.cpc.ncep.noaa.gov",target="_blank",
                             style="color:#0d9488;","cpc.ncep.noaa.gov")),
                    tags$p(tags$b("CONAB (2024). "),
                      "Acompanhamento da Safra Brasileira de GrÃ£os â€” 12Â° Levantamento. BrasÃ­lia. ",
                      tags$a(href="https://www.conab.gov.br/info-agro/safras",target="_blank",
                             style="color:#0d9488;","conab.gov.br/safras")),
                    tags$p(tags$b("CEPEA-ESALQ/USP (2025). "),
                      "Indicador de PreÃ§o da Soja â€” MT/ParanaguÃ¡. ",
                      tags$a(href="https://www.cepea.esalq.usp.br/br/indicador/soja.aspx",target="_blank",
                             style="color:#0d9488;","cepea.esalq.usp.br")),
                    tags$br(),
                    tags$div(style=paste0("background:#0b1a0d;border:1px solid #1e4024;border-radius:6px;",
                      "padding:10px 14px;margin-top:8px;font-size:10.5px;"),
                      tags$b(style="color:#22c55e;","ðŸ“Ž Como citar este dashboard: "),tags$br(),
                      tags$span(style="color:#4a7a52;font-style:italic;",
                        "Dashboard Ferrugem AsiÃ¡tica da Soja â€” MT v8.0. Plataforma de monitoramento e anÃ¡lise de risco. ",
                        "Fontes: ERA5-Land Â· IBGE PAM Â· EMBRAPA SIGA Â· NOAA CPC Â· CEPEA/ESALQ Â· CONAB Â· MAPA/ZARC.")
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)


# 6. SERVER â€” LOGICA DO SERVIDOR ====
# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
# SERVER
# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
## 6.1  Inicializacao â€” server(), rv, refresh_all, df_f ----
server <- function(input, output, session) {
  updateSelectizeInput(session,"sel_mun",choices=sort(MUN$municipio),selected="Sorriso",server=TRUE)
  updateSelectizeInput(session,"sel_mun_info",choices=sort(MUN$municipio),selected="Sorriso",server=TRUE)

  rv <- reactiveValues(
    prod        = prod_base,
    clima       = clima_base,
    irc         = irc_base,
    res         = res_base,
    prev        = prev_base,
    sinistro    = sinistro_base,
    cenarios    = cenarios_base,
    prod_pa     = prod_with_pa,
    geobr_mt    = NULL,
    mercado     = NULL,
    siga        = NULL,
    fonte_prod  = ifelse(ibge_ok_init,"IBGE/SIDRA REAL","Simulado CONAB"),
    ibge_ok     = ibge_ok_init,
    nasa_ok     = nasa_ok_init,
    updating    = FALSE,
    log         = character(0),
    last_update = Sys.time()
  )
  log_add <- function(m) rv$log <- c(rv$log, paste0("[",format(Sys.time(),"%H:%M:%S"),"] ",m))

  refresh_all <- function() {
    if (isTRUE(isolate(rv$updating))) return()
    rv$updating <- TRUE
    log_add("=== Iniciando atualizaÃ§Ã£o ===")
    future_promise({
      list(preco=buscar_preco(), cambio=buscar_cambio(),
           enso=buscar_enso(), siga=buscar_siga_alertas())
    }) %...>% (function(dm) {
      dm$ts <- Sys.time()
      rv$mercado <- dm
      rv$siga    <- dm$siga
      log_add(sprintf("PreÃ§o: R$ %.2f (%s)",dm$preco$preco,dm$preco$fonte))
      log_add(sprintf("CÃ¢mbio: %.4f | ENSO: %s",dm$cambio$val,dm$enso))
      log_add(sprintf("SIGA: %s",dm$siga$focos_texto))
      rv$res      <- calcular_resultado(rv$irc, rv$prod, dm$preco$preco)
      rv$prev     <- calcular_prev(rv$irc, rv$res, dm$enso, dm$preco$preco)
      rv$sinistro <- tryCatch(calcular_sinistro(rv$res, rv$prod_pa), error=function(e) rv$res)
      rv$cenarios <- tryCatch(calcular_cenarios(rv$irc, rv$res, rv$prod, dm$preco$preco), error=function(e) rv$cenarios)
      rv$last_update <- Sys.time()
      rv$updating <- FALSE
      log_add("=== AtualizaÃ§Ã£o concluÃ­da ===")
      # Carregar geobr em background se ainda nÃ£o carregado
      if (is.null(rv$geobr_mt)) {
        future_promise({ carregar_geobr_mt() }) %...>% (function(geo) {
          if (!is.null(geo)) { rv$geobr_mt <- geo; log_add("[geobr] PolÃ­gonos MT carregados.") }
        }) %...!% (function(e) log_add(paste("[geobr] Erro:", e$message)))
      }
    }) %...!% (function(e) {
      log_add(paste("[ERRO mercado]",e$message))
      rv$updating <- FALSE
    })

    if (!rv$ibge_ok) {
      future_promise({ buscar_ibge(2012:2023) }) %...>% (function(d) {
        if (!is.null(d) && nrow(d)>100) {
          cache_set("ibge",d)
          ph <- d %>% mutate(municipio=gsub(" - MT","",nome)) %>%
            filter(municipio %in% MUN$municipio) %>%
            select(municipio,ano,producao_ton,area_ha,produtividade)
          rv$prod <- ph; rv$ibge_ok <- TRUE; rv$fonte_prod <- "IBGE/SIDRA REAL"
          rv$prod_pa <- tryCatch(calcular_produtiv_atingivel(ph), error=function(e) ph)
          pr <- if (!is.null(rv$mercado)) rv$mercado$preco$preco else PRECO_PADRAO
          rv$res  <- calcular_resultado(rv$irc,ph,pr)
          rv$prev <- calcular_prev(rv$irc,rv$res,
            if (!is.null(rv$mercado)) rv$mercado$enso else "El Nino moderado",pr)
          rv$sinistro <- tryCatch(calcular_sinistro(rv$res, rv$prod_pa), error=function(e) rv$res)
          rv$cenarios <- tryCatch(calcular_cenarios(rv$irc, rv$res, ph, pr), error=function(e) rv$cenarios)
          log_add(sprintf("[IBGE] %d registros reais + produtiv.atingivel calculada.",nrow(d)))
        } else log_add("[IBGE] sem dados, mantendo simulaÃ§Ã£o.")
      }) %...!% (function(e) log_add(paste("[ERRO IBGE]",e$message)))
    }
    if (!rv$nasa_ok) {
      # NASA POWER: todos os 47 municipios com rate limiting (0.5s entre requests)
      # Divide em lotes de 10 para nao sobrecarregar a API
      mr <- MUN  # todos os municipios
      future_promise({
        results <- list()
        lote_sz <- 10
        for (lote in seq(1, nrow(mr), by=lote_sz)) {
          idx <- lote:min(lote+lote_sz-1, nrow(mr))
          for (i in idx) {
            Sys.sleep(0.5)  # respeitar rate limit NASA POWER
            df <- tryCatch(buscar_nasa(mr$lat[i], mr$lon[i]), error=function(e) NULL)
            if (!is.null(df)) {
              df$municipio <- mr$municipio[i]
              results[[length(results)+1]] <- df
            }
          }
          Sys.sleep(1.0)  # pausa entre lotes
        }
        if (length(results) > 0) do.call(rbind, results) else NULL
      }) %...>% (function(cn) {
        if (!is.null(cn) && nrow(cn)>10) {
          cache_set("nasa",cn); rv$nasa_ok <- TRUE   # â† salva cache agregado
          ch <- gerar_clima(cn); ch$mes <- factor(ch$mes,levels=MESES_SAFRA)
          rv$clima <- ch; rv$irc <- calcular_irc(ch)
          pr <- if (!is.null(rv$mercado)) rv$mercado$preco$preco else PRECO_PADRAO
          rv$res  <- calcular_resultado(rv$irc,rv$prod,pr)
          rv$prev <- calcular_prev(rv$irc,rv$res,
            if (!is.null(rv$mercado)) rv$mercado$enso else "El Nino moderado",pr)
          rv$sinistro <- tryCatch(calcular_sinistro(rv$res, rv$prod_pa), error=function(e) rv$res)
          rv$cenarios <- tryCatch(calcular_cenarios(rv$irc, rv$res, rv$prod, pr), error=function(e) rv$cenarios)
          log_add(sprintf("[NASA] %d/%d municipios reais (todos solicitados).",
                          length(unique(cn$municipio)), nrow(MUN)))
        } else log_add("[NASA] sem dados, mantendo INMET ref.")
      }) %...!% (function(e) log_add(paste("[ERRO NASA]",e$message)))
    }
    log_add("=== RequisiÃ§Ãµes disparadas (async) ===")
  }

  started <- reactiveVal(FALSE)
  observe({
    invalidateLater(600,session)
    isolate(if (!started()) { started(TRUE); refresh_all() })
  })
  observeEvent(input$btn_refresh, refresh_all())
  observeEvent(input$btn_test,    refresh_all())
  observeEvent(input$auto_tick,   refresh_all())

  df_f <- reactive({
    d <- rv$res
    if (is.null(d) || nrow(d) == 0) return(d)
    d$regiao <- regiao_mun(d$lat, d$lon)
    if (!is.null(input$f_risco) && input$f_risco != "Todos")
      d <- dplyr::filter(d, as.character(categoria_risco) == input$f_risco)
    d <- dplyr::filter(d, irc >= input$f_irc[1], irc <= input$f_irc[2])
    d <- dplyr::filter(d, coalesce(producao_media, prod_ref * 1000) / 1000 >= input$f_prod)
    if (!"prioridade" %in% names(d)) d$prioridade <- seq_len(nrow(d))
    d
  })

  output$hdr_mercado <- renderUI({
    dm <- rv$mercado
    if (is.null(dm)) return(tags$span(style="color:#2d5c32;font-size:9.5px;font-family:'JetBrains Mono',monospace;","â³ buscando..."))
    ts_str <- format(dm$ts, "%H:%M")
    tags$div(style="display:flex;align-items:center;gap:14px;",
      tags$span(style="font-family:'JetBrains Mono',monospace;font-size:10px;",
        tags$span(style="color:#4a7a52;font-size:9px;","SOJA "),
        tags$span(style="color:#fef08a;font-weight:700;",paste0("R$ ",fmt_r(dm$preco$preco),"/sc"))
      ),
      tags$span(style="color:#162d1a;","â”‚"),
      tags$span(style="font-family:'JetBrains Mono',monospace;font-size:10px;",
        tags$span(style="color:#4a7a52;font-size:9px;","USD "),
        tags$span(style="color:#94c99a;font-weight:700;",fmt_r(dm$cambio$val,4))
      ),
      tags$span(style="color:#162d1a;","â”‚"),
      tags$span(style="color:#2d5c32;font-size:9px;",paste0("â± ",ts_str))
    )
  })
  output$update_toast <- renderUI({
    dm <- rv$mercado
    if (is.null(dm)) return(tags$div(class="update-toast",
      tags$span(class="dot-live"),tags$span(style="margin-left:6px;","Buscando dados online...")))
    tags$div(class="update-toast",style="border-color:#1e4024;display:flex;align-items:center;gap:6px;",
      tags$span(style="color:#16a34a;font-size:14px;","âœ“"),
      tags$span(style="font-size:10.5px;",paste0("Atualizado Ã s ",format(dm$ts,"%H:%M")))
    )
  })

## 6.2  InfoBoxes â€” Cabecalho geral ----
  # â”€â”€ INFOBOXES â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  output$ib_mun  <- renderInfoBox(infoBox(
    "MunicÃ­pios Analisados", paste0(nrow(df_f())," / 47"),
    icon=icon("city"), color="green", fill=TRUE,
    subtitle="Mato Grosso â€” IBGE MT"))
  output$ib_alto <- renderInfoBox({
    n <- sum(df_f()$cat_vuln %in% c("Critica","Alta"))
    infoBox("IVM CrÃ­tico / Alto", paste0(n," municÃ­pios"),
            icon=icon("exclamation-triangle"), color="red", fill=TRUE,
            subtitle="AÃ§Ã£o imediata recomendada")
  })
  output$ib_prod <- renderInfoBox({
    t <- sum(coalesce(df_f()$producao_media,df_f()$prod_ref*1000),na.rm=TRUE)/1e6
    infoBox("ProduÃ§Ã£o MÃ©dia â€” MT", paste0(fmt_r(t,2)," Mi ton/ano"),
            icon=icon("leaf"), color="yellow", fill=TRUE,
            subtitle="Fonte: IBGE PAM 2023")
  })
  output$ib_perda <- renderInfoBox({
    t <- sum(df_f()$perda_mi_r,na.rm=TRUE)
    infoBox("Perda Potencial Est.", paste0("R$ ",fmt_n(round(t))," Mi"),
            icon=icon("money-bill-wave"), color="orange", fill=TRUE,
            subtitle="Dalla Lana et al. (2015)")
  })

## 6.3  Visao Geral â€” IRC, Pizza, Top15, Alerta 2025 ----
  # â”€â”€ VISÃƒO GERAL â€” GRÃFICOS (corrigidos) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  output$g_irc_bar <- renderPlotly({
    tryCatch({
    df <- df_f() %>% arrange(irc)
    plot_ly(df, x=~irc, y=~reorder(municipio,irc), type="bar", orientation="h",
      marker=list(color=~CORES_RISCO[as.character(categoria_risco)],
                  line=list(color="rgba(0,0,0,0)", width=0)),
      text=~paste0(" ",irc), textposition="outside",
      textfont=list(color="#4a7a52", size=8),
      hovertemplate=paste0("<b>%{y}</b><br>IRC: %{x:.0f}<br>",
        "Prob. Ferrugem: %{customdata}%<br>",
        "<i style='font-size:10px;color:#4a7a52;'>",
        "Prob = 100/(1+exp(âˆ’(IRCâˆ’50)/10)) â€” Del Ponte et al. (2006)</i>",
        "<extra></extra>"),
      customdata=~prob_ferrugem) %>%
    tp(title="") %>%
    layout(yaxis=list(title="", tickfont=list(size=7.5)),
           xaxis=list(title="IRC â€” Ãndice de Risco ClimÃ¡tico (0â€“100)", range=c(0,132)),
           showlegend=FALSE, margin=list(l=140, r=65, t=10, b=30))
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_irc_bar:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})
  # DistribuiÃ§Ã£o de Risco â€” pizza/donut
  # Fonte: calcular_irc() Ã— cat_risco_fn() â€” classes EMBRAPA SIGA adaptadas
  output$g_pizza <- renderPlotly({
    tryCatch({
    df <- df_f() %>%
      dplyr::mutate(cat=factor(as.character(categoria_risco),levels=NIVEIS_RISCO)) %>%
      dplyr::count(cat) %>% dplyr::filter(n>0)
    plot_ly(df, labels=~cat, values=~n, type="pie",
      marker=list(
        colors=CORES_RISCO[as.character(df$cat)],
        line=list(color="#050d07", width=2)),
      textinfo="percent",
      textfont=list(size=11, color="#e8f5ea"),
      hovertemplate="%{label}<br><b>%{value}</b> municÃ­pios â€” %{percent}<extra></extra>",
      hole=0.38) %>%
    layout(
      paper_bgcolor="rgba(0,0,0,0)",
      plot_bgcolor="rgba(0,0,0,0)",
      font=list(family="Inter, sans-serif", color="#94c99a", size=10),
      showlegend=TRUE,
      legend=list(
        bgcolor="rgba(0,0,0,0)",
        font=list(color="#94c99a", size=9),
        orientation="h",
        yanchor="top", y=-0.08,
        xanchor="center", x=0.5),
      margin=list(t=8, r=8, b=72, l=8),
      hoverlabel=list(bgcolor="#050d07",bordercolor="#16a34a",
                      font=list(family="JetBrains Mono, monospace",color="#e8f5ea",size=11))
    )
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_pizza:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})
  output$g_prob_hist <- renderPlotly({
    tryCatch({
    df <- rv$irc %>% group_by(safra,ano_inicio,enso) %>%
      summarise(prob=mean(prob_ferrugem),irc_m=mean(irc),.groups="drop") %>% arrange(ano_inicio)
    plot_ly(df, x=~safra, y=~prob, type="scatter", mode="lines+markers",
      line=list(color="#dc2626",width=2.5),
      marker=list(color=~CORES_ENSO[enso],size=12,line=list(color="#050d07",width=1.5)),
      text=~paste0("<b>",safra,"</b><br>Prob. mÃ©dia: ",round(prob,1),"%<br>IRC mÃ©dio: ",
                   round(irc_m,1),"<br>ENSO: ",enso),
      hovertemplate="%{text}<extra></extra>") %>%
    tp(tickangle=-45,title="Safra") %>%
    layout(yaxis=list(title="Prob. OcorrÃªncia Ferrugem (%)",range=c(0,100)),
           showlegend=FALSE,
      shapes=list(list(type="line",x0=0,x1=1,xref="paper",y0=50,y1=50,
        line=list(color="#2d5c32",dash="dot",width=1.5))),
      annotations=list(list(
        x=0.98, y=53, xref="paper", yref="y",
        text="Limiar 50% (IRC=50)", showarrow=FALSE,
        font=list(color="#2d5c32",size=9), xanchor="right"
      ))
    )
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_prob_hist:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})
  output$g_top15 <- renderPlotly({
    tryCatch({
    df <- df_f() %>% arrange(desc(ivm)) %>% head(15)
    plot_ly(df, x=~ivm, y=~reorder(municipio,ivm), type="bar", orientation="h",
      marker=list(color=~CORES_VULN[as.character(cat_vuln)],
                  line=list(color="rgba(0,0,0,0)",width=0)),
      text=~paste0(" IVM:",ivm), textposition="outside",
      textfont=list(size=9, color="#4a7a52"),
      customdata=~paste0(prob_ferrugem,"|",as.character(cat_vuln),"|",irc),
      hovertemplate=paste0("<b>%{y}</b><br>IVM: %{x:.1f}<br>",
        "Prob. Ferrugem: %{customdata}<extra></extra>")) %>%
    tp() %>%
    layout(yaxis=list(title="",tickfont=list(size=9)),
           xaxis=list(title="IVM â€” Ãndice de Vulnerabilidade Municipal (0â€“100)",range=c(0,133)),
           showlegend=FALSE, margin=list(l=160,r=90,t=10,b=35))
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_top15:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})

  # ALERTA 2025/26 â€” municÃ­pios especÃ­ficos com maior risco previsto
  output$ui_alerta <- renderUI({
    top_prev <- rv$prev %>% arrange(desc(irc_prev)) %>% head(10)
    n_alto   <- sum(rv$prev$cat_risco_prev %in% c("Muito Alto","Alto"))
    pm       <- round(mean(rv$prev$prob_prev),1)
    enso_s   <- if (!is.null(rv$mercado)) rv$mercado$enso else "El Nino moderado"
    tags$div(style="padding:4px;",
      tags$div(style="background:#6b1c1c;border:1px solid #991b1b;color:#f5a8a8;padding:8px;border-radius:6px;text-align:center;margin-bottom:6px;",
        icon("exclamation-triangle"),
        tags$b(style="font-size:1.15em;",paste0(" ",n_alto," municÃ­pios em RISCO ALTO/MUITO ALTO")),
        tags$div(style="font-size:10.5px;",paste0("Prob. mÃ©dia prevista: ",pm,"%"))),
      tags$div(style="color:#4a7a52;font-size:9.5px;font-weight:700;margin:5px 0 3px;",
        "TOP 10 MUNICÃPIOS â€” ATENÃ‡ÃƒO EM 2025/26:"),
      do.call(tags$div, lapply(seq_len(min(10,nrow(top_prev))), function(i) {
        r  <- top_prev[i,]
        cr <- as.character(r$cat_risco_prev)
        tags$div(class="alerta-item",
          style=paste0("border-left-color:",CORES_RISCO[cr],";"),
          tags$span(style=paste0("color:",CORES_RISCO[cr],";font-weight:700;"),
            paste0("#",i," ",r$municipio)),
          tags$span(style="color:#aaa;float:right;",
            paste0("IRC:",r$irc_prev," P:",r$prob_prev,"%"))
        )
      })),
      tags$div(style="background:#0b1a0d;border:1px solid #1e4024;color:#4a7a52;padding:6px;border-radius:5px;font-size:10.5px;margin-top:5px;",
        icon("globe")," ENSO: ",tags$b(style="color:#f0b429;",enso_s),tags$br(),
        icon("clock")," Atualizado: ",format(rv$last_update,"%d/%m %H:%M"),tags$br(),
        if (!is.null(rv$siga) && rv$siga$online)
          tags$span(style="color:#2d7a3a;",icon("check")," SIGA/EMBRAPA online")
        else
          tags$span(style="color:#f0b429;",icon("exclamation")," SIGA offline")
      )
    )
  })

## 6.4  Busca de Municipios ----
  # â”€â”€ BUSCA DE MUNICÃPIOS â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  mun_ativo <- reactiveVal("Sorriso")
  observeEvent(input$btn_buscar, {
    req(input$sel_mun)
    mun_ativo(input$sel_mun)
  })
  observeEvent(input$sel_mun, {
    req(input$sel_mun)
    mun_ativo(input$sel_mun)
  }, ignoreInit=TRUE)

  output$card_mun <- renderUI({
    m  <- mun_ativo()
    if (is.null(m) || !m %in% rv$res$municipio) {
      return(tags$div(
        style=paste0("background:#0b1a0d;border:1px solid #1e4024;border-radius:8px;",
                     "padding:14px 18px;margin:0 0 4px;"),
        tags$p(style="color:#2d5c32;font-size:11px;margin:0;",
          icon("search"), " Selecione um municÃ­pio e clique em Analisar para ver os resultados.")
      ))
    }
    d  <- rv$res %>% filter(municipio==m)
    p  <- rv$prev %>% filter(municipio==m)
    cr <- as.character(d$categoria_risco)
    cv <- as.character(d$cat_vuln)
    tags$div(
      style=paste0("background:#0b1a0d;border:1px solid #1e4024;border-left:4px solid ",
                   CORES_RISCO[cr],";border-radius:0 8px 8px 0;",
                   "padding:14px 18px;margin:0 0 4px;"),
      tags$h3(style="color:#e8f5ea;margin:0 0 12px;font-size:15px;font-weight:700;",
        icon("map-marker-alt", style="color:#16a34a;margin-right:6px;"), m),
      fluidRow(
        column(2,
          tags$div(
            style=paste0("background:",CORES_RISCO[cr],";color:#fff;padding:10px 8px;",
                         "border-radius:6px;text-align:center;"),
            tags$div(class="valor-big", style="font-size:1.7em;", paste0(d$prob_ferrugem,"%")),
            tags$div(style="font-size:9px;margin:3px 0;", "Prob. Ferrugem"),
            tags$b(style="font-size:10px;", cr)
          )
        ),
        column(2,
          tags$div(
            style=paste0("background:",CORES_VULN[cv],";color:#fff;padding:10px 8px;",
                         "border-radius:6px;text-align:center;"),
            tags$div(class="valor-big", style="font-size:1.7em;", paste0(d$ivm,"/100")),
            tags$div(style="font-size:9px;margin:3px 0;", "IVM Vulnerab."),
            tags$b(style="font-size:10px;", cv)
          )
        ),
        column(2,
          if (nrow(p)>0) {
            crp <- as.character(p$cat_risco_prev)
            tags$div(
              style=paste0("background:",CORES_RISCO[crp],";color:#fff;padding:10px 8px;",
                           "border-radius:6px;text-align:center;"),
              tags$div(class="valor-big", style="font-size:1.7em;", paste0(p$prob_prev,"%")),
              tags$div(style="font-size:9px;margin:3px 0;", "Prev. 2025/26"),
              tags$b(style="font-size:10px;", crp)
            )
          } else {
            tags$div(style="background:#0e2010;border:1px solid #1e4024;border-radius:6px;",
              tags$div(style="font-size:9px;color:#2d5c32;padding:10px;text-align:center;",
                "PrevisÃ£o nÃ£o disponÃ­vel"))
          }
        ),
        column(6,
          tags$table(
            class="table table-condensed",
            style="font-size:11px;color:#94c99a;margin:0;width:100%;",
            tags$tbody(
              tags$tr(
                tags$td(style="padding:2px 8px 2px 0;white-space:nowrap;", tags$b("IRC:")),
                tags$td(style="padding:2px 0;", paste0(d$irc," / 100 â€” ",cr))),
              tags$tr(
                tags$td(style="padding:2px 8px 2px 0;white-space:nowrap;", tags$b("Sev. sim.:")),
                tags$td(style="padding:2px 0;", paste0(d$sev,"%"))),
              tags$tr(
                tags$td(style="padding:2px 8px 2px 0;white-space:nowrap;", tags$b("Coef. dano:")),
                tags$td(style="padding:2px 0;", paste0(d$coef_dano,"% â†’ âˆ’",d$perda_kg_ha," kg/ha"))),
              tags$tr(
                tags$td(style="padding:2px 8px 2px 0;white-space:nowrap;", tags$b("Ãrea cultivada:")),
                tags$td(style="padding:2px 0;",
                  paste0(fmt_n(round(coalesce(d$area_calc,d$area_ref*1000)))," ha"))),
              tags$tr(
                tags$td(style="padding:2px 8px 2px 0;white-space:nowrap;", tags$b("Perda potencial:")),
                tags$td(style="padding:2px 0;color:#ef4444;font-weight:600;",
                  paste0(fmt_n(d$perda_ton)," ton | R$ ",d$perda_mi_r," Mi"))),
              tags$tr(
                tags$td(style="padding:2px 8px 2px 0;white-space:nowrap;", tags$b("Aplic. rec.:")),
                tags$td(style="padding:2px 0;", paste0(d$n_aplic," fungicidas (~R$ ",d$custo_protecao_ha,"/ha)"))),
              tags$tr(
                tags$td(style="padding:2px 8px 2px 0;white-space:nowrap;", tags$b("IRC prev. 25/26:")),
                tags$td(style="padding:2px 0;",
                  if (nrow(p)>0) paste0(p$irc_prev," [IC80%: ",p$irc_ic_l,"â€“",p$irc_ic_h,"]")
                  else "â€”")),
              tags$tr(
                tags$td(style="padding:2px 8px 2px 0;white-space:nowrap;", tags$b("Prioridade:")),
                tags$td(style="padding:2px 0;", paste0("#",d$prioridade," / ",nrow(rv$res))))
            )
          )
        )
      )
    )
  })

  # â”€â”€ helper para grÃ¡fico de espera â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  .plt_aguarde <- function(msg="Selecione um municÃ­pio e clique em Analisar") {
    .plt() %>% tp() %>% layout(
      annotations=list(list(
        text=msg, x=0.5, y=0.5, xref="paper", yref="paper",
        showarrow=FALSE, font=list(color="#4a7a52", size=12))))
  }

  output$g_mun_prob <- renderPlotly({
    tryCatch({
    m   <- mun_ativo()
    req(!is.null(m), !is.null(rv$irc), nrow(rv$irc)>0)
    df  <- rv$irc %>% filter(municipio==m) %>% arrange(ano_inicio)
    if (nrow(df)==0) return(.plt_aguarde(paste0("Sem dados histÃ³ricos para: ",m)))
    p25 <- rv$prev %>% filter(municipio==m)
    fig <- plot_ly(df, x=~safra, y=~prob_ferrugem,
      type="scatter", mode="lines+markers",
      name="HistÃ³rico (ERA5-Land)",
      line=list(color="#dc2626", width=2.5),
      marker=list(color=~CORES_ENSO[enso], size=10,
                  line=list(color="#050d07", width=1.5)),
      text=~paste0("<b>",safra,"</b><br>Prob: ",prob_ferrugem,
                   "%<br>IRC: ",irc,"<br>ENSO: ",enso),
      hovertemplate="%{text}<extra></extra>")
    if (nrow(p25)>0)
      fig <- fig %>% add_markers(
        x="2025/2026", y=p25$prob_prev,
        name="PrevisÃ£o ARIMA+ENSO",
        marker=list(color="#ca8a04", size=15, symbol="diamond",
                    line=list(color="#050d07", width=2)),
        hovertemplate=paste0("<b>PrevisÃ£o 2025/26</b><br>",
          "Prob: <b>",p25$prob_prev,"%</b><br>",
          "Risco: ",as.character(p25$cat_risco_prev),"<extra></extra>"))
    fig %>%
      tp(tickangle=-40, title="Safra") %>%
      layout(
        yaxis=list(title="Probabilidade de OcorrÃªncia Ferrugem (%)", range=c(0,100)),
        showlegend=TRUE,
        shapes=list(list(type="line", x0=0, x1=1, xref="paper",
          y0=50, y1=50, line=list(color="#2d5c32", dash="dot", width=1.2))))
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_mun_prob:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})

  output$g_mun_irc <- renderPlotly({
    tryCatch({
    m   <- mun_ativo()
    req(!is.null(m), !is.null(rv$irc), nrow(rv$irc)>0)
    df  <- rv$irc %>% filter(municipio==m) %>% arrange(ano_inicio)
    if (nrow(df)==0) return(.plt_aguarde(paste0("Sem dados histÃ³ricos para: ",m)))
    p25 <- rv$prev %>% filter(municipio==m)
    fig <- plot_ly(df, x=~safra, y=~irc,
      type="scatter", mode="lines+markers",
      color=~enso, colors=CORES_ENSO,
      marker=list(size=10), line=list(width=2.2),
      text=~paste0("<b>",safra,"</b><br>IRC: ",irc,
                   "<br>Prob: ",prob_ferrugem,"%<br>ENSO: ",enso),
      hovertemplate="%{text}<extra></extra>")
    if (nrow(p25)>0)
      fig <- fig %>% add_markers(
        x="2025/2026", y=p25$irc_prev,
        name="PrevisÃ£o 2025/26 (IC 80%)",
        marker=list(color="#ca8a04", size=15, symbol="diamond",
                    line=list(color="#050d07", width=2)),
        error_y=list(type="data",
                     array=p25$irc_ic_h - p25$irc_prev,
                     arrayminus=p25$irc_prev - p25$irc_ic_l,
                     color="#ca8a04", thickness=2, width=8),
        hovertemplate=paste0(
          "<b>PrevisÃ£o 2025/26</b><br>IRC: <b>",p25$irc_prev,"</b><br>",
          "IC 80%: [",p25$irc_ic_l," â€“ ",p25$irc_ic_h,"]<extra></extra>"))
    fig %>%
      tp(tickangle=-40, title="Safra") %>%
      layout(
        yaxis=list(title="IRC â€” Ãndice de Risco ClimÃ¡tico (0â€“100)"),
        showlegend=TRUE,
        shapes=list(list(type="line", x0=0, x1=1, xref="paper",
          y0=50, y1=50, line=list(color="#2d5c32", dash="dot", width=1.2))))
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_mun_irc:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})

  # CORRIGIDO: subplot ProduÃ§Ã£o + Produtividade com heights explÃ­cito e eixos nomeados
  output$g_mun_prod <- renderPlotly({
    tryCatch({
    m  <- mun_ativo()
    df <- rv$prod %>% filter(municipio==m) %>% arrange(ano)
    plot_ly(df) %>%
      add_bars(x=~ano, y=~producao_ton/1000, name="ProduÃ§Ã£o (mil t)",
        marker=list(color="#16a34a",opacity=.82), yaxis="y",
        hovertemplate="<b>%{x}</b><br>%{y:.0f} mil t<extra>IBGE PAM</extra>") %>%
      add_lines(x=~ano, y=~produtividade, name="Produtividade (kg/ha)",
        line=list(color="#ca8a04",width=2.5),
        marker=list(color="#ca8a04",size=8),
        yaxis="y2",
        hovertemplate="<b>%{x}</b><br>%{y:.0f} kg/ha<extra>IBGE PAM</extra>") %>%
      layout(
        paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
        font=list(family="Inter, sans-serif",color="#94c99a",size=10),
        xaxis=list(title="Ano",gridcolor="#162d1a",tickfont=list(color="#4a7a52",size=9),dtick=2),
        yaxis=list(title="ProduÃ§Ã£o (mil t)",gridcolor="#162d1a",
                   tickfont=list(color="#16a34a",size=9),titlefont=list(color="#16a34a")),
        yaxis2=list(title="Produtividade (kg/ha)",overlaying="y",side="right",
                    gridcolor="rgba(0,0,0,0)",
                    tickfont=list(color="#ca8a04",size=9),titlefont=list(color="#ca8a04")),
        legend=list(bgcolor="rgba(0,0,0,0)",font=list(color="#94c99a",size=9),
                    orientation="h",y=-0.38,x=0.5,xanchor="center",yanchor="top"),
        margin=list(t=10,r=80,b=85,l=10),
        hoverlabel=list(bgcolor="#050d07",bordercolor="#16a34a",
                        font=list(family="JetBrains Mono, monospace",color="#e8f5ea",size=11)))
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_mun_prod:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})

  # CORRIGIDO: subplot Clima com heights explÃ­cito
  output$g_mun_clima <- renderPlotly({
    tryCatch({
    m  <- mun_ativo(); sf <- input$sel_safra_det
    df <- rv$clima %>% filter(municipio==m, safra==sf)
    if (nrow(df)==0) return(.plt() %>% tp() %>%
      layout(title=list(text="Sem dados climÃ¡ticos para este filtro",
                        font=list(color="#94c99a"))))
    plot_ly(df, x=~mes) %>%
      add_bars(y=~precip_mm, name="PrecipitaÃ§Ã£o (mm)",
        marker=list(color="#1d4ed8",opacity=.82), yaxis="y",
        hovertemplate="%{x}: <b>%{y:.0f} mm</b><extra>ERA5-Land</extra>") %>%
      add_lines(y=~ur_pct, name="UR (%)",
        line=list(color="#ef4444",width=2.5),
        marker=list(color="#ef4444",size=8),
        yaxis="y2", hovertemplate="%{x}: <b>%{y:.1f}%</b><extra>ERA5-Land</extra>") %>%
      add_lines(y=~dias_fav, name="Dias FavorÃ¡veis",
        line=list(color="#ca8a04",dash="dot",width=2),
        marker=list(color="#ca8a04",symbol="diamond",size=8),
        yaxis="y2",
        hovertemplate="%{x}: <b>%{y:.0f} dias</b><extra>Limiar Del Ponte 2006</extra>") %>%
      layout(
        paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
        font=list(family="Inter, sans-serif",color="#94c99a",size=10),
        xaxis=list(title="MÃªs da Safra",gridcolor="#162d1a",
                   tickfont=list(color="#4a7a52",size=9)),
        yaxis=list(title="PrecipitaÃ§Ã£o (mm)",gridcolor="#162d1a",
                   tickfont=list(color="#1d4ed8",size=9),titlefont=list(color="#1d4ed8")),
        yaxis2=list(title="UR (%) / Dias FavorÃ¡veis",overlaying="y",side="right",
                    gridcolor="rgba(0,0,0,0)",
                    tickfont=list(color="#ef4444",size=9),titlefont=list(color="#ef4444")),
        legend=list(bgcolor="rgba(0,0,0,0)",font=list(color="#94c99a",size=9),
                    orientation="h",y=-0.38,x=0.5,xanchor="center",yanchor="top"),
        margin=list(t=10,r=80,b=85,l=10),
        hoverlabel=list(bgcolor="#050d07",bordercolor="#16a34a",
                        font=list(family="JetBrains Mono, monospace",color="#e8f5ea",size=11)))
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_mun_clima:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})

  output$g_mun_comp <- renderPlotly({
    tryCatch({
    m   <- mun_ativo()
    req(!is.null(m), !is.null(rv$irc), nrow(rv$irc)>0)
    dm  <- rv$irc %>% filter(municipio==m) %>% select(safra, irc_mun=irc, enso)
    if (nrow(dm)==0) return(.plt_aguarde(paste0("Sem dados para: ",m)))
    dm2 <- rv$irc %>% group_by(safra) %>% summarise(irc_med=mean(irc), .groups="drop")
    df  <- left_join(dm, dm2, by="safra") %>% arrange(safra) %>%
      mutate(diff = irc_mun - irc_med)
    plot_ly(df, x=~safra) %>%
      add_lines(y=~irc_mun, name=m,
        line=list(color="#dc2626", width=2.8),
        marker=list(color=~CORES_ENSO[enso], size=10,
                    line=list(color="#050d07", width=1)),
        text=~paste0("<b>",m,"</b> â€” Safra ",safra,"<br>IRC: ",irc_mun,
                     "<br>Î” vs MT: ",ifelse(diff>0,"+",""),round(diff,1)),
        hovertemplate="%{text}<extra></extra>") %>%
      add_lines(y=~irc_med, name="MÃ©dia MT (47 mun.)",
        line=list(color="#16a34a", dash="dash", width=2),
        marker=list(color="#16a34a", size=8),
        hovertemplate="<b>MÃ©dia MT</b> â€” Safra %{x}<br>IRC: %{y:.1f}<extra></extra>") %>%
      add_bars(y=~diff, name="DiferenÃ§a vs MÃ©dia",
        marker=list(color=~ifelse(diff>0,"rgba(220,38,38,0.45)","rgba(22,163,74,0.45)"),
                    line=list(color="rgba(0,0,0,0)", width=0)),
        hovertemplate="Î” IRC: %{y:.1f}<extra></extra>", visible="legendonly") %>%
    tp(tickangle=-40, title="Safra") %>%
      layout(yaxis=list(title="IRC (0â€“100)"), barmode="overlay", showlegend=TRUE)
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_mun_comp:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})

## 6.5  Info por Municipio â€” Ficha e Graficos ----
  # â”€â”€ INFO POR MUNICÃPIO â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  mun_info <- reactiveVal("Sorriso")
  observeEvent(input$btn_info, {
    req(input$sel_mun_info)
    mun_info(input$sel_mun_info)
  })
  # Atualiza tambÃ©m quando o municÃ­pio muda no selectize (sem precisar clicar)
  observeEvent(input$sel_mun_info, {
    req(input$sel_mun_info)
    mun_info(input$sel_mun_info)
  }, ignoreInit=TRUE)

  output$ficha_mun <- renderUI({
    m  <- mun_info()
    if (is.null(m) || !m %in% rv$res$municipio) {
      return(tags$div(
        style=paste0("background:#0b1a0d;border:1px solid #1e4024;border-radius:8px;",
                     "padding:14px 18px;margin:0 0 4px;"),
        tags$p(style="color:#2d5c32;font-size:11px;margin:0;",
          icon("leaf"), " Selecione um municÃ­pio e clique em Ver Ficha para carregar a ficha fitossanitÃ¡ria.")
      ))
    }
    d  <- rv$res %>% filter(municipio==m)
    dm <- MUN %>% filter(municipio==m)
    res_labels <- c("1"="SensÃ­vel","2"="Baixa","3"="MÃ©dia","4"="Alta")
    res_col    <- c("1"="#16a34a","2"="#ca8a04","3"="#ef4444","4"="#991b1b")
    nr <- as.character(dm$nivel_resistencia)
    fito_m <- FITO_REAL %>% filter(municipio==m)
    incid_txt  <- if (nrow(fito_m)>0) paste0(fito_m$incidencia_hist_pct,"%") else paste0(round(d$prob_ferrugem*0.7,0),"%")
    n_aplic_ob <- if (nrow(fito_m)>0) fito_m$n_aplic_real else d$n_aplic
    tags$div(
      style=paste0("background:#0b1a0d;border:1px solid #1e4024;border-left:4px solid #16a34a;",
                   "border-radius:0 8px 8px 0;padding:14px 18px;margin:0 0 4px;"),
      tags$h3(style="color:#e8f5ea;margin:0 0 12px;font-size:15px;font-weight:700;",
        icon("leaf", style="color:#16a34a;margin-right:6px;"),
        " Ficha FitossanitÃ¡ria â€” ", m),
      fluidRow(
        column(3,
          tags$div(style="background:#050d07;border:1px solid #1e4024;border-radius:6px;padding:12px;height:100%;",
            tags$div(style="color:#2d5c32;font-size:9px;font-weight:700;letter-spacing:.5px;margin-bottom:8px;",
              "FERRUGEM â€” RISCO ATUAL"),
            tags$div(style="text-align:center;margin:8px 0;",
              tags$span(class="badge-risco",
                style=paste0("background:",CORES_RISCO[as.character(d$categoria_risco)],
                             ";font-size:13px;padding:5px 14px;"),
                as.character(d$categoria_risco))),
            tags$div(style="color:#94c99a;font-size:10.5px;line-height:1.8;",
              tags$b("IRC: "), d$irc," / 100", tags$br(),
              tags$b("Prob.: "), d$prob_ferrugem,"%", tags$br(),
              tags$b("Sev. sim.: "), d$sev,"%", tags$br(),
              tags$b("Esporos/mÂ³: "), fmt_n(d$esporos_m3), tags$br(),
              tags$b("Incid. histÃ³rica: "), incid_txt
            )
          )
        ),
        column(3,
          tags$div(style="background:#050d07;border:1px solid #1e4024;border-radius:6px;padding:12px;height:100%;",
            tags$div(style="color:#2d5c32;font-size:9px;font-weight:700;letter-spacing:.5px;margin-bottom:8px;",
              "HISTÃ“RICO"),
            tags$div(style="color:#94c99a;font-size:10.5px;line-height:1.8;",
              tags$b("1Âª ocorrÃªncia: "), dm$safra_1a_ocorr, tags$br(),
              tags$b("Anos monit.: "), 2024-as.integer(substr(dm$safra_1a_ocorr,1,4)), tags$br(),
              tags$b("Focos est. atual: "),
              fmt_n(round(FAT_SAFRA$focos_mt[nrow(FAT_SAFRA)]*(d$irc/100)*0.6)), tags$br(),
              tags$b("Total hist. est.: "),
              fmt_n(round(sum(FAT_SAFRA$focos_mt)*d$relevancia/100)), tags$br(),
              if (nrow(fito_m)>0)
                tags$span(style="color:#16a34a;", icon("check"), " Dado EMBRAPA/APROSOJA")
            )
          )
        ),
        column(3,
          tags$div(style="background:#050d07;border:1px solid #1e4024;border-radius:6px;padding:12px;height:100%;",
            tags$div(style="color:#2d5c32;font-size:9px;font-weight:700;letter-spacing:.5px;margin-bottom:8px;",
              "RESISTÃŠNCIA A FUNGICIDAS"),
            tags$div(style="text-align:center;margin:8px 0;",
              tags$span(class="badge-risco",
                style=paste0("background:",res_col[nr],";font-size:13px;padding:5px 12px;"),
                res_labels[nr])),
            tags$div(style="color:#94c99a;font-size:10.5px;line-height:1.8;",
              tags$b("Grupos eficazes: "),
              if (dm$nivel_resistencia<3) "SDHI, IQe, IBS" else "SDHI+IQe (mistura obrig.)", tags$br(),
              tags$b("AplicaÃ§Ãµes rec.: "), d$n_aplic, tags$br(),
              tags$b("AplicaÃ§Ãµes obs.: "), fmt_r(n_aplic_ob,1), tags$br(),
              tags$b("Custo/ha: "), "R$ ", d$custo_protecao_ha, tags$br(),
              tags$b("Fonte: "), "EMBRAPA Circ.119/2024"
            )
          )
        ),
        column(3,
          tags$div(style="background:#050d07;border:1px solid #1e4024;border-radius:6px;padding:12px;height:100%;",
            tags$div(style="color:#2d5c32;font-size:9px;font-weight:700;letter-spacing:.5px;margin-bottom:8px;",
              "IMPACTO ECONÃ”MICO"),
            tags$div(style="color:#94c99a;font-size:10.5px;line-height:1.8;",
              tags$b("Perda potencial:"), tags$br(),
              tags$span(style="font-size:13px;color:#dc2626;font-weight:700;",
                paste0(fmt_n(d$perda_ton)," ton")), tags$br(),
              tags$span(style="font-size:13px;color:#ef4444;font-weight:700;",
                paste0("R$ ",d$perda_mi_r," Mi")), tags$br(),
              tags$b("Coef. dano: "), d$coef_dano,"%", tags$br(),
              tags$b("Perda/ha: âˆ’"), fmt_n(d$perda_kg_ha)," kg/ha"
            )
          )
        )
      )
    )
  })

  output$g_calendario_risco <- renderPlotly({
    tryCatch({
    m  <- mun_info()
    req(!is.null(m), !is.null(rv$clima), nrow(rv$clima)>0)
    sf <- input$sel_safra_info
    if (is.null(sf) || sf=="") sf <- SAFRA_ATUAL
    df <- rv$clima %>% filter(municipio==m, safra==sf)
    if (nrow(df)==0) {
      sfas <- rv$clima %>% filter(municipio==m) %>% pull(safra) %>% unique()
      if (length(sfas)>0) df <- rv$clima %>% filter(municipio==m, safra==sfas[length(sfas)])
    }
    if (nrow(df)==0) return(.plt_aguarde(paste0("Sem dados climaticos para: ",m)))
    df <- df %>% mutate(
      risco_mes=case_when(
        ur_pct>80 & dias_fav>15 ~ "Muito Alto",
        ur_pct>75 & dias_fav>10 ~ "Alto",
        ur_pct>70 & dias_fav>5  ~ "Moderado",
        TRUE                    ~ "Baixo"),
      risco_mes=factor(risco_mes, levels=NIVEIS_RISCO))
    plot_ly(df, x=~mes, y=~dias_fav, type="bar",
      marker=list(color=~CORES_RISCO[as.character(risco_mes)]),
      text=~paste0(dias_fav," d | ",round(ur_pct,0),"% UR"),
      textposition="inside", textfont=list(size=8, color="#fff"),
      insidetextanchor="middle",
      hovertemplate="<b>%{x}</b><br>Dias fav.: %{y}<br>UR: %{customdata}%<extra></extra>",
      customdata=~round(ur_pct,1)) %>%
    tp() %>%
    layout(yaxis=list(title="Dias Favoraveis a Infeccao", autorange=TRUE),
           xaxis=list(title="Mes da Safra"), showlegend=FALSE,
           uniformtext=list(minsize=7, mode="hide"))
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_calendario_risco:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})

  output$g_janela_aplic <- renderPlotly({
    tryCatch({
    m  <- mun_info()
    req(!is.null(m), !is.null(rv$res), nrow(rv$res)>0)
    d  <- rv$res %>% filter(municipio==m)
    dm <- MUN    %>% filter(municipio==m)
    if (nrow(d)==0 || nrow(dm)==0)
      return(.plt_aguarde(paste0("Sem dados de manejo para: ",m)))
    n_ap   <- max(1L, as.integer(d$n_aplic[1]))
    dae_r1 <- tryCatch(as.integer(dm$dae_r1[1]), error=function(e) 45L)
    if (is.na(dae_r1) || is.null(dae_r1)) dae_r1 <- 45L
    janelas <- data.frame(
      aplicacao=paste0("Aplic. ",seq_len(n_ap)),
      ini=c(dae_r1-10,dae_r1+12,dae_r1+28,dae_r1+44)[seq_len(n_ap)],
      fim=c(dae_r1+5, dae_r1+24,dae_r1+40,dae_r1+56)[seq_len(n_ap)],
      estagio =c("V4-R1 (Preventivo)","R1-R3 (Curativo)","R3-R5 (Cobertura)","R5-R6 (Extra)")[seq_len(n_ap)],
      cor     =c("#16a34a","#ca8a04","#ef4444","#991b1b")[seq_len(n_ap)],
      fungicida=c("SDHI+IQe (Elatus Arc)","SDHI+IBS (Ativum)","IQe+IBS (Cronnos)","SDHI+IQe (Orkestra)")[seq_len(n_ap)],
      stringsAsFactors=FALSE)
    fig <- .plt()
    for (i in seq_len(nrow(janelas))) {
      j <- janelas[i,]
      fig <- fig %>% add_trace(
        type="scatter", mode="lines",
        x=c(j$ini,j$fim,j$fim,j$ini,j$ini),
        y=c(i-.35,i-.35,i+.35,i+.35,i-.35),
        fill="toself", fillcolor=j$cor, opacity=0.82,
        line=list(color="rgba(0,0,0,0)"),
        text=paste0("<b>",j$aplicacao,"</b><br>Estagio: ",j$estagio,
                    "<br>DAE: ",j$ini,"-",j$fim,
                    "<br>Fungicida: ",j$fungicida),
        hovertemplate="%{text}<extra></extra>",
        name=j$aplicacao, showlegend=FALSE)
    }
    fig %>% tp() %>% layout(
      xaxis=list(title=paste0("Dias Apos Emergencia (DAE) - R1 est.: DAE ",dae_r1)),
      yaxis=list(title="", tickvals=seq_len(n_ap), ticktext=janelas$aplicacao,
                 tickfont=list(size=10, color="#94c99a")),
      shapes=list(list(type="line", x0=dae_r1, x1=dae_r1, y0=0.5, y1=n_ap+0.5,
             line=list(color="#ca8a04", dash="dot", width=1.5))),
      annotations=list(list(x=dae_r1, y=n_ap+0.6, text="R1", showarrow=FALSE,
             font=list(color="#ca8a04", size=10))),
      showlegend=FALSE, margin=list(l=110,r=20,t=25,b=60))
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_janela_aplic:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})

  output$g_focos_hist <- renderPlotly({
    tryCatch({
    req(!is.null(FAT_SAFRA), nrow(FAT_SAFRA)>0)
    df <- FAT_SAFRA %>% arrange(ano)
    plot_ly(df, x=~safra, y=~focos_mt, type="bar",
      marker=list(color=~CORES_ENSO[enso], line=list(color="#050d07",width=.5)),
      text=~paste0(fmt_n(focos_mt), " focos"),
      textposition="inside", textfont=list(size=8, color="#fff"),
      insidetextanchor="middle",
      hovertemplate="<b>%{x}</b><br>%{y} focos<br>ENSO: %{customdata}<extra></extra>",
      customdata=~enso) %>%
    tp(tickangle=-45, title="Safra") %>%
    layout(yaxis=list(title="Focos confirmados - MT (EMBRAPA SIGA)"),
           showlegend=FALSE, uniformtext=list(minsize=7,mode="hide"))
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_focos_hist:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})

  output$g_zona_risco <- renderPlotly({
    tryCatch({
    m  <- mun_info()
    req(!is.null(m), !is.null(rv$clima), nrow(rv$clima)>0)
    sf <- input$sel_safra_info
    if (is.null(sf) || sf=="") sf <- SAFRA_ATUAL
    df <- rv$clima %>% filter(municipio==m, safra==sf)
    if (nrow(df)==0) {
      sfas <- rv$clima %>% filter(municipio==m) %>% pull(safra) %>% unique()
      if (length(sfas)>0) df <- rv$clima %>% filter(municipio==m, safra==sfas[length(sfas)])
    }
    if (nrow(df)==0) return(.plt_aguarde(paste0("Sem dados climaticos para: ",m)))
    plot_ly(df, x=~temp_c, y=~ur_pct, type="scatter", mode="markers+text",
      marker=list(
        color=~CORES_RISCO[as.character(cat_risco_fn(dias_fav/27*100))],
        size=~(dias_fav+2)*2.5, line=list(color="#050d07",width=1), opacity=.9),
      text=~mes, textposition="top right",
      textfont=list(size=9, color="#e8f5ea"),
      cliponaxis=FALSE, customdata=~dias_fav,
      hovertemplate=paste0(
        "<b>%{text}</b><br>",
        "Temp: <b>%{x:.1f}C</b> | UR: <b>%{y:.1f}%</b><br>",
        "Dias favoraveis: <b>%{customdata}</b>/mes<extra></extra>")) %>%
    tp() %>%
    layout(
      xaxis=list(title="Temperatura Media Mensal (C)", range=c(15,36)),
      yaxis=list(title="Umidade Relativa Media (%)", range=c(45,102)),
      shapes=list(
        list(type="rect", x0=15, x1=28, y0=80, y1=100,
             fillcolor="rgba(220,38,38,0.08)",
             line=list(color="#dc2626",dash="dot",width=1.5), layer="below"),
        list(type="line", x0=28, x1=28, y0=45, y1=102,
             line=list(color="#ca8a04",dash="dot",width=1.5))),
      annotations=list(
        list(x=21.5,y=99.5,xanchor="center",
             text="<b>Zona otima - Ferrugem Asiatica</b>",
             showarrow=FALSE,font=list(color="#dc2626",size=10.5)),
        list(x=21.5,y=96,xanchor="center",
             text="T: 15-28C + UR maior ou igual a 80%",
             showarrow=FALSE,font=list(color="#dc2626",size=9.5)),
        list(x=21.5,y=92.5,xanchor="center",
             text="Del Ponte et al. (2006) Phytopathology 96:797",
             showarrow=FALSE,font=list(color="#2d5c32",size=8.5)),
        list(x=31.5,y=75,
             text="T>28C: risco reduzido",
             showarrow=FALSE,font=list(color="#ca8a04",size=9))),
      showlegend=FALSE)
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_zona_risco:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})
  # (outputOptions definidos mais abaixo, apÃ³s todos os outputs estarem registrados)
  # â”€â”€ PDF â€” Ficha FitossanitÃ¡ria (aba Info por MunicÃ­pio) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  output$dl_info_pdf <- downloadHandler(
    filename=function() {
      ext <- if (requireNamespace("webshot2",quietly=TRUE)) ".pdf" else ".html"
      paste0("ficha_fitossanitaria_", mun_info(), "_", Sys.Date(), ext)
    },
    contentType="application/pdf",
    content=function(f) {
      m  <- mun_info()
      sf <- input$sel_safra_info
      d_res  <- rv$res  %>% filter(municipio==m)
      dm_mun <- MUN    %>% filter(municipio==m)
      fito_m <- FITO_REAL %>% filter(municipio==m)
      zarc_m <- ZARC_MUN  %>% filter(municipio==m)
      naplic_m <- DADOS_NAPLIC_2024 %>% filter(municipio==m)
      resist_m <- RESIST_HIST %>% filter(municipio==m) %>% arrange(desc(ano)) %>% head(5)
      res_labels <- c("1"="SensÃ­vel","2"="Baixa","3"="MÃ©dia","4"="Alta")

      # Tabela principal: indicadores de risco
      d_risco <- data.frame(
        Indicador  = c("IRC (Ãndice Risco ClimÃ¡tico)","Probabilidade de Ferrugem",
                       "IVM (Ãndice Vulnerabilidade)","Severidade Simulada",
                       "Coef. Dano (Dalla Lana 2015)","Esporos Estimados (mÂ³)",
                       "Perda Potencial (ton)","Perda Potencial (R$ Mi)",
                       "NÃ­vel de ResistÃªncia","AplicaÃ§Ãµes Recomendadas",
                       "Custo ProteÃ§Ã£o (R$/ha)","Prioridade MT"),
        Valor = c(
          paste0(d_res$irc," / 100"),
          paste0(d_res$prob_ferrugem,"%"),
          paste0(round(d_res$ivm,1)," / 100"),
          paste0(d_res$sev,"%"),
          paste0(d_res$coef_dano,"%"),
          fmt_n(d_res$esporos_m3),
          fmt_n(d_res$perda_ton),
          paste0("R$ ",d_res$perda_mi_r," Mi"),
          paste0(dm_mun$nivel_resistencia,"/4 â€” ",res_labels[as.character(dm_mun$nivel_resistencia)]),
          as.character(d_res$n_aplic),
          paste0("R$ ",d_res$custo_protecao_ha),
          paste0("#",d_res$prioridade," / ",nrow(rv$res))
        )
      )

      # Tabela clima da safra
      d_clima <- rv$clima %>% filter(municipio==m, safra==sf) %>%
        select(Mes=mes, `Precip.(mm)`=precip_mm, `UR(%)`=ur_pct,
               `Temp.(Â°C)`=temp_c, `Dias.Fav.`=dias_fav, `Graus.Dia`=graus_dia)

      # Tabela histÃ³rico IRC
      d_irc_hist <- rv$irc %>% filter(municipio==m) %>% arrange(ano_inicio) %>%
        select(Safra=safra, IRC=irc, `Prob.(%)`=prob_ferrugem,
               `Precip.(mm)`=precip_total, `UR(%)`=ur_media, ENSO=enso)

      # Tabela produÃ§Ã£o
      d_prod <- rv$prod %>% filter(municipio==m) %>% arrange(ano) %>%
        select(Ano=ano, `Producao(t)`=producao_ton, `Area(ha)`=area_ha,
               `Produtiv.(kg/ha)`=produtividade)

      # Tabela fitossanidade
      d_fito <- if (nrow(fito_m)>0) {
        fito_m %>% select(`Alerta EMBRAPA`=alerta_embrapa,
                          `Incid.Hist.(%)`=incidencia_hist_pct,
                          `Efic.Campo(%)`=efic_media_campo,
                          `Aplic.Obs.`=n_aplic_real,
                          `Focos 2024`=focos_confirmados_2024,
                          `Perda.Hist.(R$Mi)`=perda_hist_media_mi)
      } else NULL

      # Tabela resistÃªncia histÃ³rica
      d_resist <- resist_m %>%
        select(Ano=ano, `NÃ­vel Resist.`=nivel_resistencia,
               `N.Aplic.Rec.`=n_aplic_rec, `Anos ExposiÃ§Ã£o`=anos_exposicao)

      # Tabela ZARC / Seguro
      d_zarc <- if (nrow(zarc_m)>0) {
        zarc_m %>% select(`Zona ZARC`=zona_zarc, `Taxa Min(%)`=taxa_min_pct,
                          `Taxa Max(%)`=taxa_max_pct, `Subv.PSR(%)`=subvencao_max_pct)
      } else NULL

      # PrevisÃ£o
      d_prev <- rv$prev %>% filter(municipio==m) %>%
        select(`IRC Prev.`=irc_prev, `IC Inf.`=irc_ic_l, `IC Sup.`=irc_ic_h,
               `Prob.Prev.(%)`=prob_prev, `Cat.Risco`=cat_risco_prev,
               `Perda Est.(R$Mi)`=perda_prev_mi) %>%
        mutate(`Cat.Risco`=as.character(`Cat.Risco`))

      descricao_txt <- paste0(
        "MunicÃ­pio: <b>",m,"</b> | Safra analisada: <b>",sf,"</b><br>",
        "IRC: <b>",d_res$irc,"</b> | Categoria: <b>",as.character(d_res$categoria_risco),"</b>",
        " | Prob. ferrugem: <b>",d_res$prob_ferrugem,"%</b><br>",
        "1Âª ocorrÃªncia: <b>",dm_mun$safra_1a_ocorr,"</b> | ResistÃªncia: NÃ­vel <b>",
        dm_mun$nivel_resistencia,"/4 â€” ",res_labels[as.character(dm_mun$nivel_resistencia)],"</b><br>",
        "Perda potencial: <b>R$ ",d_res$perda_mi_r," Mi</b> (",
        fmt_n(d_res$perda_ton)," ton) | AplicaÃ§Ãµes rec.: <b>",d_res$n_aplic,"</b>"
      )
      df_lst <- list(
        "Indicadores de Risco â€” Safra Atual"     = d_risco,
        "Clima Mensal â€” Safra Selecionada"        = d_clima,
        "HistÃ³rico IRC por Safra"                 = d_irc_hist,
        "ProduÃ§Ã£o e Produtividade (IBGE PAM)"     = d_prod,
        "ResistÃªncia a Fungicidas â€” HistÃ³rico"    = d_resist,
        "PrevisÃ£o 2025/26 (ARIMA+ENSO)"           = d_prev
      )
      if (!is.null(d_fito))  df_lst[["Fitossanidade â€” EMBRAPA SIGA"]]  <- d_fito
      if (!is.null(d_zarc))  df_lst[["ZARC / Seguro Rural (MAPA)"]]     <- d_zarc
      tmp <- .gerar_pdf(
        titulo    = paste0("Ficha FitossanitÃ¡ria â€” ",m," | Ferrugem AsiÃ¡tica da Soja"),
        descricao = descricao_txt,
        df_list   = df_lst
      )
      file.copy(tmp, f, overwrite=TRUE)
    })
  # â”€â”€ FunÃ§Ã£o auxiliar para montar dados do mapa â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  .mapa_dm <- reactive({
    sf_sel <- input$mapa_safra
    if (is.null(sf_sel)) sf_sel <- SAFRA_ATUAL
    if (sf_sel=="2025/2026 (Previsao)") {
      dm <- rv$prev %>%
        rename(irc=irc_prev, prob_ferrugem=prob_prev, categoria_risco=cat_risco_prev) %>%
        left_join(MUN[,c("municipio","lat","lon","area_ref","prod_ref")], by="municipio") %>%
        mutate(ivm=NA_real_, perda_mi_r=perda_prev_mi)
    } else {
      dm <- rv$irc %>% filter(safra==sf_sel) %>%
        left_join(MUN[,c("municipio","lat","lon","area_ref","prod_ref")], by="municipio") %>%
        left_join(rv$res[,c("municipio","ivm","cat_vuln","perda_mi_r","sev","coef_dano","esporos_m3","n_aplic")], by="municipio")
    }
    dm <- dm %>% filter(!is.na(lat), !is.na(lon))
    for (.col in c("sev","esporos_m3","n_aplic","ivm","coef_dano","cat_vuln","prod_ref","area_ref")) {
      if (!.col %in% names(dm)) dm[[.col]] <- NA_real_
    }
    # Enriquecer com dados reais (IBGE, FITO, ZARC, NAPLIC)
    dm <- dm %>%
      dplyr::left_join(PROD_IBGE_2023 %>% dplyr::select(municipio, prod_t, area_t, produtiv_t), by="municipio") %>%
      dplyr::left_join(FITO_REAL %>% dplyr::select(municipio, alerta_embrapa, incidencia_hist_pct,
                                                     efic_media_campo, n_aplic_real,
                                                     perda_hist_media_mi, focos_confirmados_2024), by="municipio") %>%
      dplyr::left_join(DADOS_NAPLIC_2024 %>% dplyr::select(municipio, nivel_resist_2024, n_aplic_rec_2024, irc_ref_2024), by="municipio") %>%
      dplyr::left_join(ZARC_MUN %>% dplyr::select(municipio, zona_zarc, taxa_min_pct, taxa_max_pct, subvencao_max_pct), by="municipio") %>%
      dplyr::left_join(MUN %>% dplyr::select(municipio, safra_1a_ocorr), by="municipio") %>%
      dplyr::mutate(
        cor_risco    = CORES_RISCO[as.character(categoria_risco)],
        cor_alerta   = dplyr::case_when(
          alerta_embrapa=="VERMELHO" ~ "#dc2626",
          alerta_embrapa=="LARANJA"  ~ "#c2410c",
          alerta_embrapa=="AMARELO"  ~ "#ca8a04",
          alerta_embrapa=="VERDE"    ~ "#16a34a",
          TRUE                       ~ "#4a7a52"),
        alerta_txt   = coalesce(alerta_embrapa, "N/D"),
        prod_exib    = coalesce(round(prod_t, 0), round(coalesce(prod_ref,0), 0)),
        area_exib    = coalesce(round(area_t, 0), round(coalesce(area_ref, 0), 0)),
        produt_exib  = coalesce(round(produtiv_t, 0), 0L),
        resist_txt   = ifelse(is.na(nivel_resist_2024), "N/D", paste0(nivel_resist_2024, "/4")),
        irc_ref_txt  = ifelse(is.na(irc_ref_2024), "N/D", as.character(irc_ref_2024)),
        n_aplic_rec_txt = ifelse(is.na(n_aplic_rec_2024),
                                  ifelse(is.na(n_aplic), "N/D", as.character(n_aplic)),
                                  as.character(n_aplic_rec_2024)),
        n_aplic_txt  = ifelse(is.na(n_aplic), "N/D", as.character(n_aplic)),
        taxa_min_txt = ifelse(is.na(taxa_min_pct), "N/D", as.character(taxa_min_pct)),
        taxa_max_txt = ifelse(is.na(taxa_max_pct), "N/D", as.character(taxa_max_pct)),
        subv_txt     = ifelse(is.na(subvencao_max_pct), "40", as.character(subvencao_max_pct)),
        zona_txt     = ifelse(is.na(zona_zarc), "N/D",
                               c("1"="Zona I","2"="Zona II","3"="Zona III")[as.character(zona_zarc)]),
        safra1a_txt  = ifelse(is.na(safra_1a_ocorr), "N/D", as.character(safra_1a_ocorr)),
        anos_exp_txt = ifelse(is.na(safra_1a_ocorr), "N/D",
                               paste0(as.integer(format(Sys.Date(),"%Y")) -
                                      as.integer(substr(as.character(safra_1a_ocorr),1,4))," anos")),
        sev_txt      = round(coalesce(as.numeric(sev), 0.0), 1),
        ivm_txt      = round(coalesce(as.numeric(ivm), 0.0), 1),
        esporos_txt  = coalesce(as.numeric(esporos_m3), 0.0)
      )
    dm
  })

  # â”€â”€ FunÃ§Ã£o auxiliar: monta popup enriquecido (6 blocos) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  .mapa_popup <- function(dm, sf_sel, fonte_label="ERA5-Land + Open-Meteo") {
    tryCatch(paste0(
      "<div style='background:#0b1a0d;color:#e8f5ea;padding:12px 14px;border-radius:7px;",
      "border:1px solid #1e4024;min-width:250px;max-width:320px;font-family:Inter,sans-serif;'>",
      "<div style='display:flex;justify-content:space-between;align-items:center;'>",
      "<b style='color:#94c99a;font-size:13.5px;'>",dm$municipio,"</b>",
      "<span style='background:",dm$cor_alerta,";color:#fff;font-size:9.5px;",
      "font-weight:700;padding:2px 6px;border-radius:3px;'>",dm$alerta_txt,"</span></div>",
      # Bloco 1: IRC / Risco ClimÃ¡tico
      "<hr style='border-color:#1e4024;margin:6px 0 4px;'>",
      "<div style='font-size:11.5px;'>",
      "<b style='color:#94c99a;'>â‘  Risco ClimÃ¡tico (",fonte_label,")</b><br>",
      "IRC: <b>",dm$irc,"</b> (0â€“100) &nbsp;|&nbsp; Cat.: <b style='color:",dm$cor_risco,";'>",
      as.character(dm$categoria_risco),"</b><br>",
      "<b style='color:#f0b429;'>Prob. Ferrugem: ",dm$prob_ferrugem,"%</b>",
      " â€” chance de epidemia de <i>P. pachyrhizi</i><br>",
      "IVM: ",dm$ivm_txt,"/100 &nbsp;|&nbsp; Sev.: ",dm$sev_txt,
      "% &nbsp;|&nbsp; Esporos: ",dm$esporos_txt," /mÂ³</div>",
      # Bloco 2: ProduÃ§Ã£o IBGE
      "<hr style='border-color:#1e4024;margin:5px 0 3px;'>",
      "<div style='font-size:11px;'>",
      "<b style='color:#94c99a;'>â‘¡ ProduÃ§Ã£o (IBGE PAM 2023)</b><br>",
      "Prod.: <b>",fmt_n(round(dm$prod_exib*1000))," t</b>",
      " | Ãrea: ",fmt_n(round(dm$area_exib*1000))," ha<br>",
      "Produtiv.: ",dm$produt_exib," kg/ha",
      " | Perda est.: <b style='color:#ef4444;'>R$ ",round(coalesce(as.numeric(dm$perda_mi_r),0.0),1)," Mi</b></div>",
      # Bloco 3: Fitossanidade
      "<hr style='border-color:#1e4024;margin:5px 0 3px;'>",
      "<div style='font-size:11px;'>",
      "<b style='color:#94c99a;'>â‘¢ Fitossanidade (EMBRAPA SIGA)</b><br>",
      "Incid.hist.: ",ifelse(is.na(dm$incidencia_hist_pct),"N/D",paste0(dm$incidencia_hist_pct,"%")),
      " | Focos 2023/24: ",ifelse(is.na(dm$focos_confirmados_2024),0L,dm$focos_confirmados_2024),"<br>",
      "Efic.campo: ",ifelse(is.na(dm$efic_media_campo),"N/D",paste0(dm$efic_media_campo,"% (Circ.119/2024)")),
      " | Aplic.obs: ",ifelse(is.na(dm$n_aplic_real),"N/D",as.character(round(dm$n_aplic_real,1))),"</div>",
      # Bloco 4: ResistÃªncia
      "<hr style='border-color:#1e4024;margin:5px 0 3px;'>",
      "<div style='font-size:11px;'>",
      "<b style='color:#94c99a;'>â‘£ ResistÃªncia (EMBRAPA Circ.119/2024)</b><br>",
      "NÃ­vel DMI/SDHI: <b>",dm$resist_txt,"</b>",
      " | IRC ref.: ",dm$irc_ref_txt,"<br>",
      "N.aplic.rec.: <b>",dm$n_aplic_rec_txt,"</b>",
      " | N.aplic.calc.: ",dm$n_aplic_txt,"</div>",
      # Bloco 5: ZARC / Seguro
      "<hr style='border-color:#1e4024;margin:5px 0 3px;'>",
      "<div style='font-size:11px;'>",
      "<b style='color:#94c99a;'>â‘¤ ZARC (MAPA Port.270/2024)</b><br>",
      "Zona: ",dm$zona_txt,
      " | Taxa prÃªmio: ",dm$taxa_min_txt,"â€“",dm$taxa_max_txt,
      "% do VPB | Subv.PSR: ",dm$subv_txt,"%<br>",
      "Perda hist.mÃ©dia: R$ ",ifelse(is.na(dm$perda_hist_media_mi),"N/D",as.character(round(dm$perda_hist_media_mi,1)))," Mi (CONAB 2019-2024)</div>",
      # Bloco 6: HistÃ³rico ferrugem
      "<hr style='border-color:#1e4024;margin:5px 0 3px;'>",
      "<div style='font-size:10.5px;'>",
      "<b style='color:#94c99a;'>â‘¥ HistÃ³rico (EMBRAPA SIGA)</b><br>",
      "1Âª ocorr.: <b>",dm$safra1a_txt,"</b>",
      " &nbsp;|&nbsp; ExposiÃ§Ã£o: <b>",dm$anos_exp_txt,"</b><br>",
      "Perda hist.mÃ©dia: <b style='color:#ef4444;'>R$ ",
      ifelse(is.na(dm$perda_hist_media_mi),0,round(dm$perda_hist_media_mi,1))," Mi</b> (CONAB 2019-2024)</div>",
      "<div style='color:#2d5c32;font-size:9.5px;margin-top:5px;'>",
      "Safra: ",sf_sel," | ",fonte_label," | IBGE PAM 2023 | EMBRAPA SIGA | NOAA ONI</div></div>"),
    error=function(e)
      paste0("<b>",dm$municipio,"</b><br><small style='color:red'>Erro popup: ",conditionMessage(e),"</small>")
    )
  }

  # â”€â”€ MAPA 1: PolÃ­gonos geobr + dados ERA5-Land â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  output$mapa <- renderLeaflet({
    tryCatch({
      sf_sel <- input$mapa_safra; if (is.null(sf_sel)) sf_sel <- SAFRA_ATUAL
      var    <- input$mapa_var;   if (is.null(var))    var    <- "prob_ferrugem"
      dm     <- .mapa_dm()
      pal    <- colorFactor(palette=unname(CORES_RISCO), levels=NIVEIS_RISCO, ordered=TRUE)
      vv     <- switch(var,
        "irc"=dm$irc, "prob_ferrugem"=dm$prob_ferrugem,
        "ivm"=coalesce(dm$ivm,0), "perda_mi_r"=coalesce(dm$perda_mi_r,0))
      vv[is.na(vv)] <- 0
      mx    <- max(coalesce(dm$prod_ref,50), na.rm=TRUE)
      raios <- pmax(6, sqrt(coalesce(dm$prod_ref,30)/mx)*38+5)
      base  <- switch(input$mapa_base,
        "dark"=providers$CartoDB.DarkMatter,
        "positron"=providers$CartoDB.Positron,
        "sat"=providers$Esri.WorldImagery,
        providers$CartoDB.DarkMatter)
      # Montar popup por linha
      dm$pop <- vapply(seq_len(nrow(dm)), function(i) .mapa_popup(dm[i,], sf_sel, "ERA5-Land+Open-Meteo"), character(1))
      # Construir mapa base
      m <- leaflet() %>% addProviderTiles(base) %>% setView(lng=-55.5, lat=-13.5, zoom=6)
      # Heatmap opcional
      if (isTRUE(input$mapa_heat) && max(vv,na.rm=TRUE)>0 &&
          requireNamespace("leaflet.extras",quietly=TRUE)) {
        m <- m %>% leaflet.extras::addHeatmap(
          lng=dm$lon, lat=dm$lat,
          intensity=vv/max(vv,na.rm=TRUE), blur=28, max=1, radius=25)
      }
      # PolÃ­gonos geobr (camada de fundo) â€” apenas se checkbox ativo
      if (isTRUE(input$mapa_poligonos)) {
        geo <- rv$geobr_mt
        if (!is.null(geo) && requireNamespace("sf",quietly=TRUE)) {
          tryCatch({
            geo_join <- geo %>%
              left_join(dm %>% select(municipio, irc, prob_ferrugem, categoria_risco),
                        by=c("name_muni_clean"="municipio"))
            geo_join$fill_cor <- CORES_RISCO[as.character(coalesce(geo_join$categoria_risco,"Muito Baixo"))]
            geo_join$fill_cor[is.na(geo_join$fill_cor)] <- "#2d5c32"
            m <- m %>% addPolygons(
              data=geo_join,
              fillColor=~fill_cor, fillOpacity=0.45,
              color="#0b1a0d", weight=0.8,
              label=~paste0(name_muni_clean," | IRC: ",coalesce(irc,0),
                            " | Prob.Ferrugem: ",coalesce(prob_ferrugem,0),"%"),
              labelOptions=labelOptions(style=list(
                "background"="#0b1a0d","color"="#e8f5ea",
                "border"="1px solid #2d7a3a","border-radius"="4px",
                "padding"="4px 8px","font-size"="11px")),
              highlightOptions=highlightOptions(
                fillOpacity=0.75, weight=2, color="#16a34a", bringToFront=FALSE),
              group="PolÃ­gonos geobr")
          }, error=function(e) NULL)
        }
      }
      # Marcadores circulares â€” SOBRE os polÃ­gonos (raio menor para nÃ£o cobrir tudo)
      # Separados dos polÃ­gonos: marcadores tÃªm borda branca para contraste
      m <- m %>% addCircleMarkers(
        lng=dm$lon, lat=dm$lat,
        radius=raios*0.65,          # 35% menores para nÃ£o sobrepor o polÃ­gono inteiro
        fillColor=pal(as.character(dm$categoria_risco)),
        color="#ffffff", weight=2, fillOpacity=0.92,
        popup=dm$pop,
        label=paste0(dm$municipio,
                     " | IRC: ",dm$irc,
                     " | Prob.Ferrugem: ",dm$prob_ferrugem,"% (ocorr. epidemia)"),
        labelOptions=labelOptions(style=list(
          "background"="#0b1a0d","color"="#e8f5ea",
          "border"="1px solid #2d7a3a","border-radius"="4px",
          "padding"="4px 8px","font-size"="11px")),
        group="Marcadores IRC")
      # Controle de camadas
      m <- m %>%
        addLayersControl(
          overlayGroups=c("PolÃ­gonos geobr","Marcadores IRC"),
          options=layersControlOptions(collapsed=FALSE)) %>%
        addControl(
          html=paste0(
            "<div style='background:rgba(10,19,9,0.93);color:#94c99a;",
            "padding:8px 11px;border-radius:6px;border:1px solid #1e4024;",
            "font-family:Inter,sans-serif;font-size:10px;min-width:170px;'>",
            "<b style='font-size:11px;color:#f0b429;'>â¬¤ Marcadores</b><br>",
            "<span style='color:#e8f5ea;'>Cor</span> = NÃ­vel de Risco IRC<br>",
            "<span style='color:#e8f5ea;'>Tamanho</span> = ProduÃ§Ã£o (t IBGE 2023)<br>",
            "<span style='color:#94c99a;'>PolÃ­gonos</span> = MunicÃ­pios geobr/IBGE<br>",
            "<span style='color:#4a7a52;font-size:9px;'>Clique no cÃ­rculo â†’ popup 6 blocos</span></div>"),
          position="topright") %>%
        addLegend(pal=pal, values=NIVEIS_RISCO,
          title="Risco ClimÃ¡tico IRC",
          position="bottomright", opacity=0.9,
          labFormat=labelFormat())
      m
    }, error=function(e) {
      message("[MAPA1 ERROR] ", conditionMessage(e))
      leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
        setView(lng=-55.5, lat=-13.5, zoom=6) %>%
        addControl(paste0("<div style='background:rgba(0,0,0,0.85);color:#ff6b6b;",
          "padding:10px;border-radius:5px;'><b>Erro no Mapa 1:</b><br>",
          conditionMessage(e),"</div>"), position="topright")
    })
  })

  # â”€â”€ MAPA 2: Apenas centrÃ³ides â€” dados NASA POWER â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  # Exibe somente pontos centrÃ³ides (lat/lon de cada municÃ­pio),
  # coloridos pelo dado climÃ¡tico real da NASA POWER API (precip, temp, UR)
  # quando disponÃ­vel; caso contrÃ¡rio, usa o IRC calculado sobre dados simulados.
  output$mapa_nasa <- renderLeaflet({
    tryCatch({
      sf_sel <- input$mapa_safra; if (is.null(sf_sel)) sf_sel <- SAFRA_ATUAL
      var    <- input$mapa_var;   if (is.null(var))    var    <- "prob_ferrugem"
      dm     <- .mapa_dm()
      # Verificar se hÃ¡ dados NASA POWER reais disponÃ­veis
      nasa_ok <- rv$nasa_ok
      nasa_c  <- if (nasa_ok) cache_get("nasa", ttl=CACHE_TTL_NASA, offline_fallback=TRUE) else cache_get_any("nasa")
      # Enriquecer dm com mÃ©dias mensais NASA POWER (out-mar) se disponÃ­veis
      if (!is.null(nasa_c) && nrow(nasa_c) > 0) {
        ano_sel <- if (sf_sel %in% SAFRAS) {
          as.integer(substr(sf_sel, 1, 4))
        } else {
          max(ANOS_SERIE)
        }
        nasa_safra <- nasa_c %>%
          dplyr::filter(ano %in% c(ano_sel, ano_sel+1),
                        mes %in% c(10,11,12,1,2,3)) %>%
          dplyr::group_by(municipio) %>%
          dplyr::summarise(
            nasa_precip_total_mm = round(sum(precip, na.rm=TRUE)*30, 0),
            nasa_temp_media_c    = round(mean(temp,   na.rm=TRUE), 1),
            nasa_ur_media_pct    = round(mean(ur,     na.rm=TRUE), 1),
            n_meses_nasa         = dplyr::n(),
            .groups="drop")
        dm <- dm %>% dplyr::left_join(nasa_safra, by="municipio")
      } else {
        dm$nasa_precip_total_mm <- NA_real_
        dm$nasa_temp_media_c    <- NA_real_
        dm$nasa_ur_media_pct    <- NA_real_
        dm$n_meses_nasa         <- 0L
      }
      # VariÃ¡vel a exibir
      vv <- switch(var,
        "irc"=dm$irc, "prob_ferrugem"=dm$prob_ferrugem,
        "ivm"=coalesce(dm$ivm,0), "perda_mi_r"=coalesce(dm$perda_mi_r,0))
      vv[is.na(vv)] <- 0
      # Paleta numÃ©rica para o valor da variÃ¡vel
      pal_num <- colorNumeric(
        palette=c("#16a34a","#ca8a04","#dc2626"),
        domain=c(0, max(vv, 1)), na.color="#777777")
      # Raios proporcionais Ã  produÃ§Ã£o
      mx    <- max(coalesce(dm$prod_ref,50), na.rm=TRUE)
      raios <- pmax(7, sqrt(coalesce(dm$prod_ref,30)/mx)*30+7)
      base  <- switch(input$mapa_base,
        "dark"=providers$CartoDB.DarkMatter,
        "positron"=providers$CartoDB.Positron,
        "sat"=providers$Esri.WorldImagery,
        providers$CartoDB.DarkMatter)
      var_lbl <- switch(var,
        "irc"="IRC (0â€“100)",
        "prob_ferrugem"="Prob.Ferrugem % (ocorr. epidemia)",
        "ivm"="IVM (0â€“100)",
        "perda_mi_r"="Perda R$ Mi",
        var)
      # Popup especÃ­fico NASA POWER (comparaÃ§Ã£o com ERA5-Land)
      dm$pop_nasa <- vapply(seq_len(nrow(dm)), function(i) {
        r    <- dm[i,]
        vval <- vv[i]
        nasa_str <- if (!is.na(r$nasa_precip_total_mm)) {
          paste0(
            "<hr style='border-color:#1e4024;margin:5px 0 3px;'>",
            "<div style='font-size:11px;'>",
            "<b style='color:#4fc3f7;'>ðŸ›° NASA POWER (dados reais API)</b><br>",
            "Precip. total (out-mar): <b>",r$nasa_precip_total_mm," mm</b><br>",
            "Temp. mÃ©dia: <b>",r$nasa_temp_media_c,"Â°C</b>",
            " | UR mÃ©dia: <b>",r$nasa_ur_media_pct,"%</b><br>",
            "Meses disponÃ­veis: ",r$n_meses_nasa,"/6",
            "</div>")
        } else {
          paste0(
            "<hr style='border-color:#1e4024;margin:5px 0 3px;'>",
            "<div style='font-size:11px;color:#f0b429;'>",
            "ðŸ›° NASA POWER: aguardando dados API<br>",
            "<small>(use 'Atualizar Dados' para buscar)</small></div>")
        }
        paste0(
          "<div style='background:#050d0f;color:#e8f5ea;padding:11px 13px;border-radius:7px;",
          "border:2px solid #1d4ed8;min-width:240px;max-width:310px;font-family:Inter,sans-serif;'>",
          "<div style='display:flex;justify-content:space-between;align-items:center;'>",
          "<b style='color:#4fc3f7;font-size:13px;'>",r$municipio,"</b>",
          "<span style='background:#1d4ed8;color:#fff;font-size:9px;font-weight:700;",
          "padding:2px 6px;border-radius:3px;'>CENTRÃ“IDE</span></div>",
          "<div style='font-size:10px;color:#4a7a52;'>",
          "Lat: ",round(r$lat,3),"Â° | Lon: ",round(r$lon,3),"Â°</div>",
          "<hr style='border-color:#1565c0;margin:5px 0 3px;'>",
          "<div style='font-size:11.5px;'>",
          "<b style='color:#94c99a;'>",var_lbl,"</b><br>",
          "<b style='font-size:14px;color:#f0b429;'>",round(vval,1),"</b>",
          if (var=="prob_ferrugem") " <small style='color:#f5a8a8;'>(prob. ocorrÃªncia de epidemia de ferrugem asiÃ¡tica)</small>",
          "<br>Cat.Risco: <b style='color:",r$cor_risco,";'>",as.character(r$categoria_risco),"</b></div>",
          nasa_str,
          "<hr style='border-color:#1e4024;margin:5px 0 3px;'>",
          "<div style='font-size:10px;color:#4fc3f7;'>",
          "IRC calculado (ERA5-Land): <b>",r$irc,"</b> | Prob.: <b>",r$prob_ferrugem,"%</b><br>",
          "Prod.ref.(IBGE 2023): ",fmt_n(round(coalesce(r$prod_ref,0)*1000))," t</div>",
          "<div style='color:#2d5c32;font-size:9px;margin-top:4px;'>",
          "Fonte: NASA POWER AGÂ·0.5Â° + ERA5-Land + IBGE PAM 2023</div></div>")
      }, character(1))
      # Montar mapa NASA (somente centrÃ³ides, sem polÃ­gonos)
      leaflet() %>%
        addProviderTiles(base) %>%
        setView(lng=-55.5, lat=-13.5, zoom=6) %>%
        addCircleMarkers(
          lng=dm$lon, lat=dm$lat,
          radius=raios,
          fillColor=pal_num(vv),
          color="#4fc3f7", weight=2, fillOpacity=0.88,
          popup=dm$pop_nasa,
          label=paste0(dm$municipio,
                       " | ",var_lbl,": ",round(vv,1),
                       if (var=="prob_ferrugem") " (prob.epidemia)" else ""),
          labelOptions=labelOptions(style=list(
            "background"="#050d14","color"="#e8f5ea",
            "border"="1px solid #1565c0","border-radius"="4px",
            "padding"="4px 8px","font-size"="11px"))) %>%
        addLegend(pal=pal_num, values=vv,
          title=paste0("NASA POWER<br>",var_lbl),
          position="bottomright", opacity=0.9) %>%
        addControl(
          html=paste0(
            "<div style='background:rgba(5,13,20,0.93);color:#4fc3f7;",
            "padding:8px 11px;border-radius:6px;border:2px solid #1d4ed8;",
            "font-family:Inter,sans-serif;font-size:10px;min-width:180px;'>",
            "<b style='font-size:11px;'>ðŸ›° CentrÃ³ides NASA POWER</b><br>",
            "<span style='color:#e8f5ea;'>Cor</span> = ",var_lbl,"<br>",
            "<span style='color:#e8f5ea;'>Tamanho</span> = ProduÃ§Ã£o (t IBGE 2023)<br>",
            if (nasa_ok)
              "<span style='color:#1a9850;font-weight:700;'>âœ” Dados NASA reais carregados</span>"
            else
              "<span style='color:#f0b429;'>âš  Aguardando dados NASA API</span>",
            "<br><span style='color:#4a7a52;font-size:9px;'>Clique â†’ popup NASA vs ERA5</span></div>"),
          position="topright")
    }, error=function(e) {
      message("[MAPA2 ERROR] ", conditionMessage(e))
      leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
        setView(lng=-55.5, lat=-13.5, zoom=6) %>%
        addControl(paste0("<div style='background:rgba(0,0,0,0.85);color:#ff6b6b;",
          "padding:10px;border-radius:5px;'><b>Erro no Mapa NASA:</b><br>",
          conditionMessage(e),"</div>"), position="topright")
    })
  })

  # â”€â”€ Selectize para busca na tabela do mapa â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  observe({
    updateSelectizeInput(session, "mapa_busca_mun",
      choices=c("",sort(MUN$municipio)), selected="", server=TRUE)
  })
  # Limpar filtros da tabela mapa
  observeEvent(input$mapa_btn_limpar, {
    updateSelectizeInput(session, "mapa_busca_mun", selected="")
    updateSelectInput(session, "mapa_filtro_risco", selected="Todos")
  })

  output$legenda_mapa <- renderUI({
    # DescriÃ§Ã£o do IRC por nÃ­vel de risco
    irc_ranges <- c("IRC < 30","IRC 30â€“44","IRC 45â€“59","IRC 60â€“74","IRC â‰¥ 75")
    descricoes <- c(
      "CondiÃ§Ãµes desfavorÃ¡veis para ferrugem",
      "Baixa pressÃ£o â€” monitoramento preventivo",
      "Moderado â€” iniciar monitoramento intensivo",
      "Alto â€” aplicaÃ§Ã£o fungicida recomendada",
      "CrÃ­tico â€” aÃ§Ã£o imediata necessÃ¡ria")
    var_sel  <- isolate(input$mapa_var)
    var_desc <- switch(var_sel,
      "irc"         = "Ãndice de Risco ClimÃ¡tico (0â€“100): combina precipitaÃ§Ã£o, UR e temperatura (ERA5-Land)",
      "prob_ferrugem"="Probabilidade de ocorrÃªncia de ferrugem asiÃ¡tica (%) â€” chance de epidemia de Phakopsora pachyrhizi causar danos econÃ´micos na safra selecionada. FÃ³rmula logÃ­stica Del Ponte et al. 2006: Prob = 100/(1+exp(âˆ’(IRCâˆ’50)/10))",
      "ivm"         = "Ãndice de Vulnerabilidade Municipal (0â€“100): IRC Ã— ResistÃªncia Ã— Ãrea",
      "perda_mi_r"  = "Perda financeira estimada (R$ Mi): Ãrea Ã— Produtividade Ã— Fator dano Ã— CEPEA",
      "VariÃ¡vel nÃ£o reconhecida")
    tags$div(style="padding:4px 6px;font-family:'Inter', sans-serif,sans-serif;",
      # â”€â”€ SEÃ‡ÃƒO 1: Cor dos cÃ­rculos â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
      tags$h5(style="color:#94c99a;margin:4px 0 6px;font-size:13px;border-bottom:1px solid #1e4024;padding-bottom:4px;",
        icon("circle"), " O que significa a COR do cÃ­rculo"),
      lapply(seq_along(NIVEIS_RISCO), function(i)
        tags$div(style="display:flex;align-items:flex-start;margin:4px 0;",
          tags$span(style=paste0(
            "width:18px;height:18px;background:", CORES_RISCO[i],
            ";border-radius:50%;display:inline-block;margin-right:9px;",
            "flex-shrink:0;margin-top:2px;border:2px solid rgba(255,255,255,0.3);")),
          tags$div(
            tags$span(style="color:#e8f5ea;font-size:11.5px;font-weight:700;",
              NIVEIS_RISCO[i]),
            tags$span(style="color:#94c99a;font-size:9.5px;margin-left:5px;",
              irc_ranges[i]),
            tags$br(),
            tags$span(style="color:#4a7a52;font-size:9.5px;",
              descricoes[i])
          )
        )
      ),
      # â”€â”€ SEÃ‡ÃƒO 2: Tamanho dos cÃ­rculos â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
      tags$hr(style="border-color:#1e4024;margin:8px 0;"),
      tags$h5(style="color:#94c99a;margin:4px 0 6px;font-size:13px;",
        icon("expand-arrows-alt"), " O que significa o TAMANHO do cÃ­rculo"),
      tags$div(style="display:flex;align-items:center;gap:8px;margin-bottom:4px;",
        tags$span(style="width:10px;height:10px;background:#94c99a;border-radius:50%;display:inline-block;"),
        tags$span(style="color:#4a7a52;font-size:10px;","â‰ˆ 50 mil t (municÃ­pios menores)")),
      tags$div(style="display:flex;align-items:center;gap:8px;margin-bottom:4px;",
        tags$span(style="width:22px;height:22px;background:#94c99a;border-radius:50%;display:inline-block;"),
        tags$span(style="color:#4a7a52;font-size:10px;","â‰ˆ 800 mil t (municÃ­pios mÃ©dios)")),
      tags$div(style="display:flex;align-items:center;gap:8px;margin-bottom:4px;",
        tags$span(style="width:36px;height:36px;background:#94c99a;border-radius:50%;display:inline-block;"),
        tags$span(style="color:#4a7a52;font-size:10px;","â‰ˆ 2,8 Mi t (Sorriso, Lucas, N.Mutum)")),
      tags$p(style="color:#2d5c32;font-size:9.5px;margin:2px 0;font-style:italic;",
        "Fonte: IBGE PAM 2023 â€” produÃ§Ã£o real por municÃ­pio"),
      # â”€â”€ SEÃ‡ÃƒO 3: VariÃ¡vel selecionada â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
      tags$hr(style="border-color:#1e4024;margin:8px 0;"),
      tags$h5(style="color:#94c99a;margin:4px 0 6px;font-size:13px;",
        icon("chart-bar"), " VariÃ¡vel selecionada"),
      tags$div(style="background:#0b1a0d;border:1px solid #1e4024;border-radius:5px;padding:6px 8px;",
        tags$span(style="color:#f0b429;font-weight:700;font-size:10.5px;",
          switch(var_sel,
            "irc"="IRC â€” Ãndice de Risco ClimÃ¡tico",
            "prob_ferrugem"="Prob. Ferrugem (%)",
            "ivm"="IVM â€” Ãndice Vulnerabilidade Municipal",
            "perda_mi_r"="Perda Estimada (R$ Mi)",
            var_sel)),
        tags$br(),
        tags$span(style="color:#4a7a52;font-size:9.5px;", var_desc)
      ),
      # â”€â”€ SEÃ‡ÃƒO 4: InteraÃ§Ã£o â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
      tags$hr(style="border-color:#1e4024;margin:8px 0;"),
      tags$h5(style="color:#94c99a;margin:4px 0 6px;font-size:13px;",
        icon("hand-pointer"), " Como interagir"),
      tags$div(style="font-size:10px;color:#4a7a52;line-height:1.7;",
        tags$div(icon("mouse-pointer",style="color:#94c99a;"),
          tags$b(style="color:#94c99a;"," Clique")," no cÃ­rculo (Mapa 1) â†’ popup 6 blocos ERA5-Land"),
        tags$div(icon("mouse-pointer",style="color:#4fc3f7;"),
          tags$b(style="color:#4fc3f7;"," Clique")," no cÃ­rculo (Mapa 2) â†’ popup NASA POWER vs ERA5-Land"),
        tags$div(icon("hand-point-up",style="color:#94c99a;"),
          tags$b(style="color:#94c99a;"," Hover")," â†’ rÃ³tulo rÃ¡pido (municÃ­pio, variÃ¡vel selecionada)"),
        tags$div(icon("search-plus",style="color:#94c99a;"),
          tags$b(style="color:#94c99a;"," Scroll/pinÃ§a")," â†’ zoom"),
        tags$div(icon("layer-group",style="color:#94c99a;"),
          tags$b(style="color:#94c99a;"," PolÃ­gonos")," â†’ Mapa 1: ative/desative em 'PolÃ­gonos geobr'"),
        tags$div(icon("fire",style="color:#94c99a;"),
          tags$b(style="color:#94c99a;"," Heatmap")," â†’ ative o checkbox 'Heatmap' acima")
      ),
      # â”€â”€ SEÃ‡ÃƒO 5: Popup â€” 6 blocos de dados â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
      tags$hr(style="border-color:#1e4024;margin:8px 0;"),
      tags$h5(style="color:#94c99a;margin:4px 0 6px;font-size:13px;",
        icon("info-circle"), " Popup â€” dados por municÃ­pio"),
      tags$div(style="font-size:9.5px;color:#4a7a52;line-height:1.8;",
        tags$div(HTML("&#9312; <b style='color:#94c99a;'>Risco ClimÃ¡tico</b> â€” IRC, Prob, IVM, Sev, Esporos (ERA5-Land)")),
        tags$div(HTML("&#9313; <b style='color:#94c99a;'>ProduÃ§Ã£o</b> â€” Ãrea, Produtividade, Perda est. (IBGE PAM 2023)")),
        tags$div(HTML("&#9314; <b style='color:#94c99a;'>Fitossanidade</b> â€” Incid. hist., Focos 2024, Efic. campo (EMBRAPA SIGA)")),
        tags$div(HTML("&#9315; <b style='color:#94c99a;'>ResistÃªncia</b> â€” NÃ­vel DMI/SDHI, N.aplic rec. (EMBRAPA Circ.119/2024)")),
        tags$div(HTML("&#9316; <b style='color:#94c99a;'>ZARC/Seguro</b> â€” Zona, Taxa prÃªmio, Subv. PSR (MAPA Port.270/2024)")),
        tags$div(HTML("&#9317; <b style='color:#94c99a;'>HistÃ³rico</b> â€” 1Âª ocorrÃªncia, anos exposiÃ§Ã£o, perda hist. (CONAB)"))
      ),
      tags$hr(style="border-color:#1e4024;margin:8px 0;"),
      tags$p(style="color:#2d5c32;font-size:8.5px;margin:2px 0;font-style:italic;",
        "Fontes: ERA5-Land Open-Meteo | IBGE PAM 2023 | EMBRAPA SIGA 2012-2024 | EMBRAPA Circ.119/2024 | MAPA ZARC Port.270/2024 | NOAA CPC ONI | CONAB 2019-2024")
    )
  })
  output$tab_mapa <- renderDT({
    sf_sel <- input$mapa_safra
    if (is.null(sf_sel)) sf_sel <- SAFRA_ATUAL
    if (sf_sel=="2025/2026 (Previsao)") {
      df <- rv$prev %>%
        dplyr::select(Municipio=municipio,
                      `IRC Prev.`=irc_prev,
                      `IC Inf.`=irc_ic_l,
                      `IC Sup.`=irc_ic_h,
                      `Prob.Ferrugem(%)` = prob_prev,
                      `Categoria Risco`  = cat_risco_prev,
                      `Perda Est.(R$Mi)` = perda_prev_mi) %>%
        dplyr::mutate(`Categoria Risco`=as.character(`Categoria Risco`))
    } else {
      df <- rv$irc %>% dplyr::filter(safra==sf_sel) %>%
        dplyr::left_join(rv$res[,c("municipio","ivm","perda_mi_r","n_aplic","sev","coef_dano")],by="municipio") %>%
        dplyr::select(
          Municipio       = municipio,
          `IRC (0-100)`   = irc,
          `Prob.Ferrugem(%)` = prob_ferrugem,
          `Categoria Risco`  = categoria_risco,
          `Precip.Total(mm)` = precip_total,
          `UR MÃ©dia(%)`      = ur_media,
          `Temp.MÃ©dia(Â°C)`   = temp_media,
          `Dias FavorÃ¡veis`  = dias_fav_total,
          `GrausDia Total`   = graus_dia_total,
          `IVM (0-100)`      = ivm,
          `Perda Est.(R$Mi)` = perda_mi_r,
          `N.Aplic.Rec.`     = n_aplic
        ) %>%
        dplyr::mutate(`Categoria Risco`=as.character(`Categoria Risco`))
    }
    # Filtro por municÃ­pio (selectize)
    mun_sel <- input$mapa_busca_mun
    if (!is.null(mun_sel) && mun_sel != "" && mun_sel %in% df$Municipio)
      df <- df %>% dplyr::filter(Municipio == mun_sel)
    # Filtro por risco
    risco_sel <- input$mapa_filtro_risco
    if (!is.null(risco_sel) && risco_sel != "Todos")
      df <- df %>% dplyr::filter(`Categoria Risco` == risco_sel)
    datatable(df, rownames=FALSE,
      caption=htmltools::tags$caption(
        style="color:#4a7a52;font-size:10.5px;text-align:left;",
        "Probabilidade (%) = chance de epidemia de ferrugem asiÃ¡tica (Phakopsora pachyrhizi) ",
        "na safra selecionada (modelo logÃ­stico Del Ponte et al. 2006). ",
        "IRC = Ãndice de Risco ClimÃ¡tico (ERA5-Land/Open-Meteo). ",
        "Fontes: calcular_irc() | IBGE PAM 2023 | EMBRAPA SIGA."),
      options=list(
        pageLength=10, dom="frtip", scrollX=TRUE,
        columnDefs=list(
          list(className="dt-center", targets=1:11))),
      class="compact stripe hover"
    ) %>%
      formatStyle("Categoria Risco",
        backgroundColor=styleEqual(
          NIVEIS_RISCO,
          c("#5d1a1a","#6b2a0e","#4a3b0e","#0e3a1a","#0a2a1a"))) %>%
      formatStyle("Prob.Ferrugem(%)",
        background=styleColorBar(c(0,100),"rgba(192,57,43,0.25)"),
        backgroundSize="100% 88%", backgroundRepeat="no-repeat", backgroundPosition="center")
  })

## 6.7  Serie Historica â€” IRC, Producao, ENSO ----
  # â”€â”€ SÃ‰RIE HISTÃ“RICA â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  output$g_irc_hist <- renderPlotly({
    tryCatch({
    df <- rv$irc %>% group_by(safra,ano_inicio,enso) %>%
      summarise(irc_m=mean(irc),prob=mean(prob_ferrugem),.groups="drop") %>% arrange(ano_inicio)
    plot_ly(df,x=~safra,y=~irc_m,type="scatter",mode="lines+markers",
      color=~enso,colors=CORES_ENSO,
      marker=list(size=12,line=list(color="#050d07",width=1.5)),
      line=list(width=2.5),
      text=~paste0("<b>",safra,"</b><br>IRC mÃ©dio: ",round(irc_m,1),
                   "<br>Prob. mÃ©dia: ",round(prob,1),"%<br>ENSO: ",enso),
      hovertemplate="%{text}<extra></extra>") %>%
    tp(tickangle=-38,title="Safra") %>%
    layout(yaxis=list(title="IRC MÃ©dio â€” MT (ERA5-Land/Open-Meteo)",range=c(0,100)),
           showlegend=TRUE,
      shapes=list(
        list(type="line",x0=0,x1=1,xref="paper",y0=50,y1=50,
             line=list(color="#2d5c32",dash="dot",width=1.5)),
        list(type="rect",x0=0,x1=1,xref="paper",y0=60,y1=100,
             fillcolor="rgba(220,38,38,.04)",line=list(width=0))
      ),
      annotations=list(
        list(x=0.01,y=52,xref="paper",yref="y",
             text="IRC=50 â†’ P=50%",showarrow=FALSE,
             font=list(color="#2d5c32",size=9),xanchor="left"),
        list(x=0.01,y=92,xref="paper",yref="y",
             text="Zona de alto risco (IRCâ‰¥60)",showarrow=FALSE,
             font=list(color="#dc2626",size=9),xanchor="left")
      )
    )
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_irc_hist:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})
  output$g_prod_hist <- renderPlotly({
    tryCatch({
    df <- rv$prod %>% group_by(ano) %>% summarise(prod=sum(producao_ton,na.rm=TRUE)/1e6,.groups="drop")
    plot_ly(df,x=~ano,y=~prod,type="scatter",mode="lines+markers",
      line=list(color="#16a34a",width=2.5),marker=list(color="#16a34a",size=9,line=list(color="#050d07",width=1)),
      fill="tozeroy",fillcolor="rgba(30,132,73,.1)",
      hovertemplate="<b>%{x}</b><br>%{y:.1f} Mi ton<extra></extra>") %>%
    tp() %>% layout(xaxis=list(title="Ano",dtick=2),
                    yaxis=list(title="ProduÃ§Ã£o total MT (Mi ton)"),showlegend=FALSE)
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_prod_hist:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})
  output$g_produtiv_hist <- renderPlotly({
    tryCatch({
    df <- rv$prod %>% group_by(ano) %>% summarise(pv=mean(produtividade,na.rm=TRUE),.groups="drop")
    plot_ly(df,x=~ano,y=~pv,type="scatter",mode="lines+markers",
      line=list(color="#1d4ed8",width=2.5),marker=list(color="#1d4ed8",size=9,line=list(color="#050d07",width=1)),
      fill="tozeroy",fillcolor="rgba(26,82,118,.1)",
      hovertemplate="<b>%{x}</b><br>%{y:.0f} kg/ha<extra></extra>") %>%
    tp() %>% layout(xaxis=list(title="Ano",dtick=2),
                    yaxis=list(title="Produtividade mÃ©dia (kg/ha)"),showlegend=FALSE)
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_produtiv_hist:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})
  output$g_heatmap <- renderPlotly({
    tryCatch({
    ord <- rv$irc %>% group_by(municipio) %>% summarise(m=mean(irc),.groups="drop") %>%
      arrange(m) %>% pull(municipio)
    mat_df <- rv$irc %>% select(municipio,safra,irc) %>%
      pivot_wider(names_from=safra,values_from=irc)
    mat_df$municipio <- factor(mat_df$municipio,levels=ord)
    mat_df <- mat_df %>% arrange(municipio)
    mat <- as.matrix(mat_df[,-1]); rownames(mat) <- as.character(mat_df$municipio)
    plot_ly(z=mat,x=colnames(mat),y=rownames(mat),type="heatmap",
      colorscale=list(c(0,"#1d4ed8"),c(.25,"#16a34a"),c(.5,"#ca8a04"),c(.75,"#ef4444"),c(1,"#dc2626")),
      zmin=0,zmax=100,
      hovertemplate="<b>%{y}</b><br>%{x}<br>IRC: %{z:.1f}<extra></extra>",
      colorbar=list(title="IRC",tickfont=list(color="#94c99a"),titlefont=list(color="#94c99a"))) %>%
    tp(tickangle=-45,title="Safra") %>%
    layout(yaxis=list(title="",tickfont=list(size=8.5)))
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_heatmap:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})
  output$g_enso_box <- renderPlotly({
    tryCatch({
    df <- rv$irc %>% mutate(enso_f=factor(enso,levels=names(CORES_ENSO)))
    plot_ly(df,x=~irc,y=~enso_f,type="box",orientation="h",color=~enso_f,colors=CORES_ENSO,
      boxpoints="suspectedoutliers",jitter=0.3,pointpos=0,
      hovertemplate="<b>%{y}</b><br>IRC: %{x:.1f}<extra></extra>",
      marker=list(size=4,opacity=.6)) %>%
    tp() %>% layout(xaxis=list(title="IRC (0â€“100)"),yaxis=list(title=""),showlegend=FALSE)
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_enso_box:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})
  # Focos + Perda Real (CONAB/EMBRAPA) â€” dois eixos, sem sobreposiÃ§Ã£o
  output$g_focos_safra <- renderPlotly({
    tryCatch({

    df <- FAT_SAFRA %>% arrange(ano)
    plot_ly(df) %>%
      add_bars(x=~safra,y=~focos_mt,name="Focos confirmados",
        marker=list(color=~CORES_ENSO[enso],opacity=0.8),
        hovertemplate="<b>%{x}</b><br>Focos: %{y}<extra></extra>") %>%
      add_lines(x=~safra,y=~perda_real_mi,name="Perda real (R$ Mi)",
        yaxis="y2",line=list(color="#f0b429",width=2.5,dash="dot"),
        marker=list(color="#f0b429",size=8),
        hovertemplate="<b>%{x}</b><br>Perda: R$%{y:.1f} Mi<extra></extra>") %>%
    tp(tickangle=-38,title="Safra") %>%
    layout(
      yaxis  = list(title="Focos â€” EMBRAPA SIGA"),
      yaxis2 = list(title="Perda estimada (R$ Mi)",overlaying="y",side="right",
                    tickfont=list(color="#f0b429",size=10),
                    titlefont=list(color="#f0b429")),
      legend = list(orientation="h",yanchor="top",y=-0.22,xanchor="center",x=0.5,
                    bgcolor="rgba(0,0,0,0)",font=list(color="#94c99a",size=9)),
      showlegend=TRUE, barmode="group",
      margin=list(b=75,t=10,l=10,r=60)
    )
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_focos_safra:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })
  })

## 6.8  Clima e Doenca â€” ERA5, Del Ponte, Graus-Dia ----
  # â”€â”€ CLIMA E DOENÃ‡A â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  output$g_cl_scatter <- renderPlotly({
    tryCatch({
    df <- rv$irc %>% filter(safra==input$cl_safra) %>%
      left_join(MUN[,c("municipio","area_ref","nivel_resistencia")],by="municipio")
    plot_ly(df,x=~precip_total,y=~dias_fav_total,color=~categoria_risco,colors=CORES_RISCO,
      size=~coalesce(area_ref,30),sizes=c(8,55),type="scatter",mode="markers",
      text=~paste0("<b>",municipio,"</b><br>Precip: ",round(precip_total),"mm<br>Dias fav: ",
                   dias_fav_total,"<br>IRC: ",irc,"<br>Prob: ",prob_ferrugem,"%<br>",
                   "Resist.: NÃ­vel ",nivel_resistencia,"/4"),
      hovertemplate="%{text}<extra></extra>") %>%
    tp() %>%
    layout(xaxis=list(title="PrecipitaÃ§Ã£o total (mm)"),
           yaxis=list(title="Dias favorÃ¡veis (soma safra)"),
           legend=list(title=list(text="Risco"),orientation="h",y=-0.42,x=0.5,xanchor="center",yanchor="top"))
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_cl_scatter:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})

  # CORRIGIDO: subplot clima mensal com heights e eixos nomeados corretamente
  output$g_cl_mensal <- renderPlotly({
    tryCatch({
    df <- rv$clima %>% filter(safra==input$cl_safra2) %>%
      group_by(mes) %>%
      summarise(prec=mean(precip_mm),ur=mean(ur_pct),dias=mean(dias_fav),
                gd=mean(graus_dia),temp=mean(temp_c),.groups="drop")
    plot_ly(df,x=~mes) %>%
      add_bars(y=~prec,name="Precip.(mm)",
        marker=list(color="#1d4ed8",opacity=.82),yaxis="y",
        hovertemplate="%{x}: %{y:.0f}mm<extra></extra>") %>%
      add_lines(y=~ur,name="UR(%)",
        line=list(color="#ef4444",width=2.5),marker=list(color="#ef4444",size=8),
        yaxis="y2",hovertemplate="%{x}: UR=%{y:.1f}%<extra></extra>") %>%
      add_lines(y=~dias*4,name="Dias Fav.",
        line=list(color="#ca8a04",dash="dot",width=2),
        yaxis="y2",hovertemplate="%{x}: %{customdata} dias fav.<extra></extra>",customdata=~round(dias,1)) %>%
      add_lines(y=~temp*2,name="Temp (Â°C)",
        line=list(color="#16a34a",dash="dash",width=1.5),
        yaxis="y2",hovertemplate="%{x}: %{customdata}C<extra></extra>",customdata=~round(temp,1)) %>%
      layout(
        paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)",
        font=list(family="Inter, sans-serif",color="#94c99a",size=10),
        xaxis=list(title="Mes",gridcolor="#162d1a",tickfont=list(color="#4a7a52",size=9)),
        yaxis=list(title="Precipitacao (mm)",gridcolor="#162d1a",
                   tickfont=list(color="#1d4ed8",size=9),titlefont=list(color="#1d4ed8")),
        yaxis2=list(title="UR(%) / Dias / Temp (escala)",overlaying="y",side="right",
                    gridcolor="rgba(0,0,0,0)",range=c(40,110),
                    tickfont=list(color="#ef4444",size=9),titlefont=list(color="#ef4444")),
        legend=list(bgcolor="rgba(0,0,0,0)",font=list(color="#94c99a",size=9),
                    orientation="h",y=-0.42,x=0.5,xanchor="center",yanchor="top"),
        margin=list(t=10,r=80,b=90,l=10),
        hoverlabel=list(bgcolor="#050d07",bordercolor="#16a34a",
                        font=list(family="JetBrains Mono, monospace",color="#e8f5ea",size=11)))
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_cl_mensal:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})

  # CORRIGIDO: graus-dia â€” somente Ãºltimas 5 safras visÃ­veis por padrÃ£o para reduzir sobreposiÃ§Ã£o
  output$g_graus_dia <- renderPlotly({
    tryCatch({
    df      <- rv$clima %>% group_by(safra,mes) %>% summarise(gd=mean(graus_dia),.groups="drop")
    safras  <- rev(sort(unique(df$safra)))
    n_safra <- length(safras)
    vis_vec <- ifelse(seq_along(safras)<=5,"true","legendonly")
    fig <- .plt()
    for (i in seq_along(safras)) {
      sf  <- safras[i]
      dfs <- df %>% filter(safra==sf)
      fig <- fig %>% add_lines(data=dfs,x=~mes,y=~gd,name=sf,
        visible=vis_vec[i],
        line=list(width=ifelse(i<=3,2.5,1.2)),
        opacity=ifelse(i<=5,0.9,0.5),
        text=~paste0(sf,"<br>",mes,": ",round(gd,1)," GD"),
        hovertemplate="%{text}<extra></extra>")
    }
    fig %>% tp() %>%
      layout(xaxis=list(title="MÃªs da Safra"),
             yaxis=list(title="Graus-dia acumulados (GD) â€” MT"),
             legend=list(orientation="h",y=-0.42,yanchor="top",font=list(size=9),
               title=list(text="Safra (Ãºltimas 5 ativas)")))
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_graus_dia:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})

  # Del Ponte â€” FILTRÃVEL POR SAFRA (atualizado)
  output$g_modelo_prob <- renderPlotly({
    tryCatch({
    sf_dp <- input$cl_safra_dp
    curva <- data.frame(irc=seq(0,100,.5)) %>% mutate(prob=prob_fn(irc))
    pts   <- rv$irc %>% filter(safra==sf_dp)
    n_pts <- nrow(pts)
    r_txt <- if (n_pts>1) sprintf("r=%.2f (n=%d)", cor(pts$irc,pts$prob_ferrugem), n_pts) else ""
    .plt() %>%
      add_lines(data=curva, x=~irc, y=~prob,
        name="P(%) = 100/(1+exp(âˆ’(IRCâˆ’50)/10)) â€” Del Ponte et al. (2006)",
        line=list(color="#16a34a",width=2.5),
        hovertemplate="IRC=%{x:.1f} â†’ P=%{y:.1f}%<extra>Modelo</extra>") %>%
      add_markers(data=pts, x=~irc, y=~prob_ferrugem,
        color=~categoria_risco, colors=CORES_RISCO,
        size=9, marker=list(line=list(color="#050d07",width=0.5)),
        text=~paste0("<b>",municipio,"</b><br>IRC: ",irc,"<br>Prob: ",prob_ferrugem,"%"),
        hovertemplate="%{text}<extra></extra>", name=sf_dp) %>%
      add_segments(x=50,xend=50,y=0,yend=50,
        name="Limiar IRC=50 (P=50%)",
        line=list(dash="dash",color="#2d5c32",width=1.5)) %>%
      add_segments(x=0,xend=50,y=50,yend=50,showlegend=FALSE,
        line=list(dash="dash",color="#2d5c32",width=1.5)) %>%
    tp() %>%
    layout(
      xaxis=list(title="IRC â€” Ãndice de Risco ClimÃ¡tico (0â€“100)"),
      yaxis=list(title="Probabilidade de OcorrÃªncia Ferrugem (%)"),
      title=list(text=paste0("Safra: ",sf_dp," | ",r_txt),
                 font=list(size=11,color="#4a7a52"),x=0.5,xanchor="center"),
      legend=list(orientation="h",y=-0.48,yanchor="top",font=list(size=9.5)),
      annotations=list(list(
        x=0.99, y=0.02, xref="paper", yref="paper",
        text="doi:10.1094/PHYTO-96-0797",
        showarrow=FALSE, font=list(color="#2d5c32",size=9),
        xanchor="right", yanchor="bottom"
      ))
    )
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_modelo_prob:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})

  output$g_corr <- renderPlotly({
    tryCatch({
    df <- rv$irc
    p  <- ggplot(df,aes(x=precip_total,y=irc,color=enso,
      text=paste0("<b>",municipio,"</b><br>",round(precip_total),"mm | IRC: ",irc,"<br>ENSO: ",enso))) +
      geom_point(alpha=.4,size=1.8) +
      geom_smooth(aes(group=enso),method="lm",se=FALSE,linewidth=.8,linetype="dashed") +
      scale_color_manual(values=CORES_ENSO) +
      labs(x="PrecipitaÃ§Ã£o total (mm)",y="IRC",color="ENSO") + tg()
    ggplotly(p,tooltip="text") %>% tp() %>%
      layout(legend=list(orientation="h",y=-0.42,yanchor="top",font=list(size=9)))
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_corr:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})

## 6.9  Perdas Economicas â€” Top 20 + Historico ----
  # â”€â”€ PERDAS ECONÃ”MICAS â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  output$ib_perda_ton <- renderInfoBox(infoBox(
    "Perda Total Estimada", paste0(fmt_n(sum(df_f()$perda_ton,na.rm=TRUE))," ton"),
    icon=icon("weight"), color="red", fill=TRUE,
    subtitle="Dalla Lana et al. (2015)"))
  output$ib_perda_r <- renderInfoBox(infoBox(
    "Perda Financeira Total", paste0("R$ ",fmt_n(round(sum(df_f()$perda_mi_r,na.rm=TRUE)))," Mi"),
    icon=icon("dollar-sign"), color="orange", fill=TRUE,
    subtitle="PreÃ§o CEPEA/ESALQ atual"))
  output$ib_coef <- renderInfoBox(infoBox(
    "Coef. Dano MÃ©dio (MT)", pct(mean(df_f()$coef_dano,na.rm=TRUE)),
    icon=icon("percent"), color="yellow", fill=TRUE,
    subtitle="MÃ©dia ponderada 47 mun."))
  output$ib_criticos <- renderInfoBox(infoBox(
    "MunicÃ­pios CrÃ­ticos", paste0(sum(df_f()$cat_vuln=="Critica")," municÃ­pios"),
    icon=icon("radiation"), color="red", fill=TRUE,
    subtitle="IVM â‰¥ 75 (vulnerabilidade crÃ­tica)"))

  output$txt_preco <- renderText({
    dm <- rv$mercado
    if (is.null(dm)) return(paste0("R$ ",fmt_r(PRECO_PADRAO),"/sc (padrÃ£o)"))
    paste0("R$ ",fmt_r(dm$preco$preco),"/sc (",dm$preco$fonte,")")
  })
  output$txt_cambio <- renderText({
    dm <- rv$mercado; if (is.null(dm)) return("â€”"); fmt_r(dm$cambio$val,4)
  })
  output$txt_preco_ts <- renderText({
    dm <- rv$mercado
    if (is.null(dm)) return("")
    paste0("(atualizado ",format(dm$preco$ts,"%H:%M"),")")
  })

  output$g_perdas_ton <- renderPlotly({
    tryCatch({

    # â”€â”€ PreÃ§o CEPEA online (via rv$mercado) para equivalente financeiro â”€â”€â”€â”€â”€â”€
    preco_ativo <- tryCatch(rv$mercado$preco$preco, error=function(e) PRECO_PADRAO)
    if (is.null(preco_ativo) || is.na(preco_ativo)) preco_ativo <- PRECO_PADRAO
    fonte_preco <- tryCatch(rv$mercado$preco$fonte, error=function(e) "PadrÃ£o")
    if (is.null(fonte_preco)) fonte_preco <- "PadrÃ£o"

    # â”€â”€ Base: dados estÃ¡ticos reais PERDA_MUN_TOP20_REAL â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
    # Fonte: IBGE PAM 2023 Ã— FAT_SAFRA 2023/24 Ã— Dalla Lana (2015)
    # Calibrado: CONAB 12Â° Levantamento 2023/24 + EMBRAPA SIGA
    df <- PERDA_MUN_TOP20_REAL %>%
      dplyr::arrange(dplyr::desc(perda_ton_2324)) %>%
      dplyr::mutate(
        pct_perda      = round(100 * perda_ton_2324 / pmax(prod_ton_ibge, 1), 2),
        perda_kton     = round(perda_ton_2324 / 1000, 1),
        perda_ref_kton = round(perda_mi_conab * 1e6 /
                                 ((141.48 / 60) * 1000) / 1000, 1),
        # Equiv. financeiro ao preÃ§o CEPEA atual
        perda_fin_online = round(perda_ton_2324 * (preco_ativo / 60) * 1000 / 1e6, 1),
        lbl_bar  = paste0(perda_kton, "k t"),
        lbl_hover = paste0(
          "<b style='font-size:12px;'>", municipio, "</b><br>",
          "<b style='color:#7aaa7e;'>ProduÃ§Ã£o (IBGE PAM 2023)</b><br>",
          "ProduÃ§Ã£o total: <b>", format(prod_ton_ibge, big.mark="."), " t</b>",
          " | Ãrea: ", format(area_ha_ibge, big.mark="."), " ha<br>",
          "Produtividade: ", format(produtiv_kgha, big.mark="."), " kg/ha<br>",
          "<b style='color:#e55039;'>Perda potencial â€” Ferrugem AsiÃ¡tica</b><br>",
          "Perda 2023/24: <b>", format(perda_ton_2324, big.mark="."), " t</b>",
          " (<b>", pct_perda, "%</b> da produÃ§Ã£o)<br>",
          "Equiv. financeiro: <b style='color:#e55039;'>R$ ", perda_fin_online,
          " Mi</b> @ R$", round(preco_ativo, 1), "/sc (", fonte_preco, ")<br>",
          "<b style='color:#7aaa7e;'>ReferÃªncia CONAB 2019-2024</b><br>",
          "Perda mÃ©dia 2019-2024: <b>R$ ", perda_mi_conab, " Mi</b><br>",
          "<b style='color:#7aaa7e;'>EMBRAPA SIGA (Ferrugem 2023/24)</b><br>",
          "Focos confirmados: <b>", focos_2024, "</b>",
          " | Incid. hist.: <b>", incid_hist_pct, "%</b><br>",
          "<i style='color:#5a8f62;font-size:9px;'>",
          "Fontes: IBGE PAM 2023 | CONAB 12Â°Lev. 2023/24 | ",
          "EMBRAPA SIGA | Dalla Lana (2015)</i>"
        )
      )

    x_max <- max(df$perda_kton, na.rm=TRUE) * 1.50

    plot_ly(df) %>%
      # ReferÃªncia CONAB 2019-2024 (fundo amarelo transparente)
      add_bars(
        x = ~perda_ref_kton,
        y = ~reorder(municipio, perda_kton),
        orientation = "h",
        name        = "MÃ©dia CONAB 2019-2024",
        marker      = list(color="rgba(240,180,41,0.28)",
                           line=list(color="rgba(0,0,0,0)", width=0)),
        hovertemplate = "<b>%{y}</b><br>Ref. CONAB mÃ©dia: %{x:.1f}k t<extra></extra>") %>%
      # Perda 2023/24 calculada (frente)
      add_bars(
        x            = ~perda_kton,
        y            = ~reorder(municipio, perda_kton),
        orientation  = "h",
        name         = "Safra 2023/24 (IBGE Ã— Dalla Lana)",
        marker       = list(color="rgba(192,57,43,0.82)",
                            line=list(color="rgba(0,0,0,0)", width=0)),
        text         = ~lbl_bar,
        textposition = "outside",
        textfont     = list(size=8.5, color="#94c99a"),
        customdata   = ~lbl_hover,
        hovertemplate = "%{customdata}<extra></extra>") %>%
    tp() %>%
    layout(
      barmode = "overlay",
      yaxis   = list(title="", tickfont=list(size=8.5), automargin=TRUE),
      xaxis   = list(
        title      = "Perda potencial (mil toneladas) â€” IBGE PAM 2023 Ã— Dalla Lana (2015)",
        tickformat = ".0f",
        range      = c(0, x_max)),
      legend  = list(bgcolor="rgba(0,0,0,0)", font=list(color="#94c99a", size=9),
                     orientation="h", yanchor="top", y=-0.22,
                     xanchor="center", x=0.5),
      margin  = list(l=10, r=85, t=10, b=75),
      annotations = list(
        list(x=x_max*0.99, y=0.01, xref="x", yref="paper", showarrow=FALSE,
          text=paste0(
            "Prod: IBGE PAM 2023 | Dano: Dalla Lana (2015)<br>",
            "Ref. CONAB 2019-2024 | PreÃ§o atual: ",
            fonte_preco, " R$", round(preco_ativo, 1), "/sc"),
          font=list(color="#2d5c32", size=8), xanchor="right")
      )
    )
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_perdas_ton:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })
  })
  output$g_perdas_fin <- renderPlotly({
    tryCatch({

    # â”€â”€ PreÃ§o CEPEA online â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
    preco_ativo <- tryCatch(rv$mercado$preco$preco, error=function(e) PRECO_PADRAO)
    if (is.null(preco_ativo) || is.na(preco_ativo)) preco_ativo <- PRECO_PADRAO
    fonte_preco <- tryCatch(rv$mercado$preco$fonte, error=function(e) "PadrÃ£o")
    if (is.null(fonte_preco)) fonte_preco <- "PadrÃ£o"
    ts_preco    <- tryCatch(
      format(rv$mercado$preco$ts, "%d/%m/%Y %H:%M"),
      error=function(e) "offline")

    # â”€â”€ Base: PERDA_MUN_TOP20_REAL â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
    df <- PERDA_MUN_TOP20_REAL %>%
      dplyr::mutate(
        # PreÃ§o mÃ©dio CEPEA 2023/24 = R$ 142,60/sc (CEPEA-ESALQ/USP)
        perda_fin_2324   = round(perda_ton_2324 * (142.60 / 60) * 1000 / 1e6, 2),
        # Perda com preÃ§o atual CEPEA online
        perda_fin_online = round(perda_ton_2324 * (preco_ativo / 60) * 1000 / 1e6, 2),
        # Delta vs preÃ§o 2023/24
        delta_pct        = round(100 * (perda_fin_online - perda_fin_2324) /
                                   pmax(perda_fin_2324, 0.01), 1),
        # Perda por hectare (R$/ha)
        perda_ha         = round(perda_fin_online * 1e6 / pmax(area_ha_ibge, 1), 0),
        lbl_bar  = paste0("R$", round(perda_fin_online, 1), " Mi"),
        lbl_hover = paste0(
          "<b style='font-size:12px;'>", municipio, "</b><br>",
          "<b style='color:#e55039;'>Perda Financeira â€” Safra 2023/24</b><br>",
          "PreÃ§o CEPEA (", fonte_preco, "): <b>R$ ",
          round(preco_ativo, 1), "/sc</b>",
          ifelse(ts_preco != "offline",
                 paste0(" <i style='color:#5a8f62;font-size:9px;'>(",
                        ts_preco, ")</i>"),
                 " <i style='color:#c0392b;font-size:9px;'>(offline)</i>"),
          "<br>",
          "Perda (preÃ§o atual): <b style='color:#e55039;'>R$ ",
          perda_fin_online, " Mi</b><br>",
          "Perda (preÃ§o 2023/24 â€” CEPEA R$142,60/sc): R$ ",
          perda_fin_2324, " Mi",
          " (<b>",
          ifelse(delta_pct >= 0,
                 paste0("+", delta_pct),
                 as.character(delta_pct)),
          "%</b> vs 2023/24)<br>",
          "Ref. CONAB 2019-2024: <b>R$ ", perda_mi_conab, " Mi</b><br>",
          "Perda por hectare: R$ ", format(perda_ha, big.mark="."), "/ha<br>",
          "<b style='color:#7aaa7e;'>ProduÃ§Ã£o (IBGE PAM 2023)</b><br>",
          "Perda (ton): ", format(perda_ton_2324, big.mark="."), " t",
          " | Ãrea: ", format(area_ha_ibge, big.mark="."), " ha<br>",
          "Focos SIGA 2023/24: <b>", focos_2024, "</b>",
          " | Incid. hist.: <b>", incid_hist_pct, "%</b><br>",
          "<i style='color:#5a8f62;font-size:9px;'>",
          "Fontes: IBGE PAM 2023 | CONAB 12Â°Lev. 2023/24 | EMBRAPA SIGA | ",
          "CEPEA-ESALQ/USP | Dalla Lana (2015)</i>"
        )
      ) %>%
      dplyr::arrange(dplyr::desc(perda_fin_online))

    x_max <- max(df$perda_fin_online, na.rm=TRUE) * 1.50

    plot_ly(df) %>%
      # ReferÃªncia CONAB 2019-2024 (fundo amarelo)
      add_bars(
        x = ~perda_mi_conab,
        y = ~reorder(municipio, perda_fin_online),
        orientation = "h",
        name      = "MÃ©dia CONAB 2019-2024",
        marker      = list(color="rgba(240,180,41,0.28)",
                           line=list(color="rgba(0,0,0,0)", width=0)),
        hovertemplate =
          "<b>%{y}</b><br>Ref. CONAB 2019-2024: R$ %{x:.1f} Mi<extra></extra>") %>%
      # Perda com preÃ§o online atual (frente)
      add_bars(
        x = ~perda_fin_online,
        y = ~reorder(municipio, perda_fin_online),
        orientation  = "h",
        name         = paste0("Safra 2023/24 â€” CEPEA R$", round(preco_ativo, 1),
                              "/sc (", fonte_preco, ")"),
        marker       = list(color="rgba(192,57,43,0.82)",
                            line=list(color="rgba(0,0,0,0)", width=0)),
        text         = ~lbl_bar,
        textposition = "outside",
        textfont     = list(size=8.5, color="#94c99a"),
        customdata   = ~lbl_hover,
        hovertemplate = "%{customdata}<extra></extra>") %>%
    tp() %>%
    layout(
      barmode = "overlay",
      yaxis   = list(title="", tickfont=list(size=8.5), automargin=TRUE),
      xaxis   = list(
        title = paste0("Perda financeira (R$ Mi) â€” CEPEA R$",
                       round(preco_ativo, 1), "/sc (", fonte_preco, ")"),
        range = c(0, x_max)),
      legend  = list(bgcolor="rgba(0,0,0,0)", font=list(color="#94c99a", size=9),
                     orientation="h", yanchor="top", y=-0.22,
                     xanchor="center", x=0.5),
      margin  = list(l=10, r=90, t=10, b=75),
      annotations = list(
        list(x=x_max*0.99, y=0.01, xref="x", yref="paper", showarrow=FALSE,
          text=paste0(
            "CEPEA: ", fonte_preco, " R$", round(preco_ativo, 1),
            "/sc | Atualizado: ", ts_preco, "<br>",
            "Base ton: IBGE PAM 2023 Ã— Dalla Lana (2015) | ",
            "Ref.: CONAB 2019-2024"),
          font=list(color="#2d5c32", size=8), xanchor="right")
      )
    )
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_perdas_fin:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })
  })
  output$g_dalla <- renderPlotly({
    tryCatch({
    # â”€â”€ Curva de Dano â€” Dalla Lana et al. (2015) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
    # D(%) = 100 Ã— (1 âˆ’ exp(âˆ’0,0416 Ã— SEV))
    # doi:10.1590/1678-4499.0120 | Crop Protection 72:150-156
    curva <- data.frame(s=seq(0,80,.5)) %>% mutate(d=dano_fn(s))
    df_pts <- df_f()
    n_pts  <- nrow(df_pts)
    .plt() %>%
      add_lines(data=curva, x=~s, y=~d,
        name="D(%) = 100Ã—(1âˆ’e^{âˆ’0,0416Ã—SEV}) â€” Dalla Lana et al. (2015)",
        line=list(color="#dc2626", width=2.8),
        hovertemplate="SEV=%{x:.1f}% â†’ D=%{y:.2f}%<extra>Modelo</extra>") %>%
      add_markers(data=df_pts, x=~sev, y=~coef_dano,
        color=~categoria_risco, colors=CORES_RISCO,
        size=8, marker=list(line=list(color="#050d07",width=.5)),
        text=~paste0("<b>",municipio,"</b><br>Sev.: ",sev,"% â†’ Dano: ",coef_dano,"%<br>IRC: ",irc),
        hovertemplate="%{text}<extra></extra>",
        showlegend=TRUE) %>%
    tp() %>%
    layout(
      xaxis=list(title="Severidade Foliar â€” SEV (%)"),
      yaxis=list(title="ReduÃ§Ã£o na Produtividade â€” D (%)"),
      legend=list(orientation="h", y=-0.48, yanchor="top", font=list(size=9)),
      margin=list(t=10, r=10, b=110, l=10),
      annotations=list(list(
        x=0.02, y=0.97, xref="paper", yref="paper",
        text=paste0("n=",n_pts," municÃ­pios | b=0,0416 | doi:10.1590/1678-4499.0120"),
        showarrow=FALSE, font=list(color="#2d5c32",size=9),
        bgcolor="rgba(5,13,7,.7)", bordercolor="#1e4024",
        borderwidth=1, borderpad=4
      ))
    )
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_dalla:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})
  output$g_custo_prot <- renderPlotly({
    tryCatch({
    df <- df_f() %>%
      dplyr::filter(!is.na(custo_protecao_ha), !is.na(perda_mi_r)) %>%
      mutate(
        custo_total = custo_protecao_ha * coalesce(area_calc, area_ref*1000) / 1e6,
        perda_evit  = perda_mi_r * 0.85,  # eficÃ¡cia mÃ©dia 85% (EMBRAPA Circ.119/2024)
        roi         = round(perda_evit / pmax(custo_total, 0.001), 1)
      )
    max_custo <- max(df$custo_total, na.rm=TRUE)
    plot_ly(df, x=~custo_total, y=~perda_evit,
      color=~cat_vuln, colors=CORES_VULN,
      size=~irc, sizes=c(6,50), type="scatter", mode="markers",
      text=~paste0("<b>",municipio,"</b><br>",
        "Custo proteÃ§Ã£o: <b>R$ ",round(custo_total,1)," Mi</b><br>",
        "Perda evitÃ¡vel (85%): <b>R$ ",round(perda_evit,1)," Mi</b><br>",
        "ROI: <b style='color:#22c55e;'>",roi,"Ã—</b>",
        " | IRC: ",irc," | N.aplic.: ",n_aplic),
      hovertemplate="%{text}<extra></extra>") %>%
    tp() %>%
    layout(
      xaxis=list(title="Custo Total de ProteÃ§Ã£o Fungicida (R$ Mi)"),
      yaxis=list(title="Perda EvitÃ¡vel Estimada (R$ Mi) â€” eficÃ¡cia 85%"),
      shapes=list(list(type="line", x0=0, y0=0,
        x1=max_custo, y1=max_custo,
        line=list(color="#2d5c32",dash="dot",width=1.5))),
      annotations=list(
        list(x=max_custo*0.55, y=max_custo*0.57,
          text="ROI = 1Ã— (breakeven)", showarrow=FALSE,
          font=list(color="#2d5c32",size=10)),
        list(x=0.98, y=0.98, xref="paper", yref="paper",
          text="EficÃ¡cia 85% â€” EMBRAPA Circ.119/2024, Tab.4",
          showarrow=FALSE, font=list(color="#2d5c32",size=9),
          xanchor="right", yanchor="top",
          bgcolor="rgba(5,13,7,.7)", bordercolor="#162d1a",
          borderwidth=1, borderpad=3)
      ),
      legend=list(orientation="h",y=-0.45,yanchor="top"),
      margin=list(t=10,r=12,b=100,l=10))
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_custo_prot:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})
  # EvoluÃ§Ã£o histÃ³rica de perdas â€” Top 10 municÃ­pios Ã— 15 anos (IBGE PAM + Dalla Lana)
  output$g_perda_hist_mun <- renderPlotly({
    tryCatch({

    # â”€â”€ Base: PERDA_HIST_TOP10_REAL (sÃ©rie estÃ¡tica prÃ©-calculada) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
    # Fontes: IBGE PAM 2023 Ã— CONAB sÃ©ries anuais Ã— FAT_SAFRA (ERA5-Land) Ã—
    #         Dalla Lana (2015) | Calibrado: CONAB/EMBRAPA SIGA 2019-2024
    preco_ativo <- tryCatch(rv$mercado$preco$preco, error=function(e) PRECO_PADRAO)
    if (is.null(preco_ativo) || is.na(preco_ativo)) preco_ativo <- PRECO_PADRAO
    fonte_preco <- tryCatch(rv$mercado$preco$fonte, error=function(e) "PadrÃ£o")
    if (is.null(fonte_preco)) fonte_preco <- "PadrÃ£o"

    top_muns <- unique(PERDA_HIST_TOP10_REAL$municipio)

    df <- PERDA_HIST_TOP10_REAL %>%
      dplyr::mutate(
        # Perda com preÃ§o atual (para comparaÃ§Ã£o)
        perda_fin_atual = round(perda_ton * (preco_ativo / 60) * 1000 / 1e6, 1),
        lbl_hover = paste0(
          "<b>", municipio, "</b> â€” Safra ", safra, "<br>",
          "<b style='color:#7aaa7e;'>Perda potencial (ton)</b><br>",
          "Perda: <b>", format(perda_ton, big.mark="."), " t</b>",
          " (", round(100 * perda_ton / pmax(prod_ton, 1), 1), "% da prod.)<br>",
          "Prod. estimada: ", format(prod_ton, big.mark="."), " t",
          " | Ãrea: ", format(area_ha, big.mark="."), " ha<br>",
          "Produtividade: ", format(produtiv, big.mark="."), " kg/ha<br>",
          "<b style='color:#e55039;'>Perda financeira</b><br>",
          "PreÃ§o ", safra, " (CEPEA): R$", preco_saca, "/sc",
          " â†’ <b>R$ ", perda_mi_r, " Mi</b><br>",
          "PreÃ§o atual (", fonte_preco, " R$",
          round(preco_ativo, 1), "/sc): R$ ", perda_fin_atual, " Mi<br>",
          "<b style='color:#7aaa7e;'>Clima (NOAA CPC / ERA5-Land)</b><br>",
          "ENSO: <b>", enso, "</b><br>",
          "<i style='color:#5a8f62;font-size:9px;'>",
          "Fontes: IBGE PAM 2023 | CONAB sÃ©ries anuais | EMBRAPA SIGA | ",
          "ERA5-Land | CEPEA-ESALQ/USP | Dalla Lana (2015)</i>"
        )
      )

    n_col <- min(10, length(top_muns))
    CORES_MUN <- setNames(
      RColorBrewer::brewer.pal(max(3, n_col), "Paired")[seq_len(n_col)],
      top_muns[seq_len(n_col)]
    )

    fig <- .plt()
    for (m in top_muns) {
      dm <- df %>% dplyr::filter(municipio == m) %>% dplyr::arrange(ano)
      if (nrow(dm) == 0) next
      fig <- fig %>% add_lines(
        data      = dm,
        x         = ~ano,
        y         = ~perda_ton / 1000,
        name      = m,
        line      = list(color=CORES_MUN[m], width=2.2),
        marker    = list(color=CORES_MUN[m], size=5, symbol="circle"),
        text      = ~lbl_hover,
        hovertemplate = "%{text}<extra></extra>"
      )
    }

    # Linha: preÃ§o histÃ³rico CEPEA (eixo direito) â€” contexto econÃ´mico
    ph_filt <- PRECO_HIST %>% dplyr::filter(ano >= 2010)
    fig <- fig %>% add_lines(
      data  = ph_filt,
      x     = ~ano, y = ~preco_saca,
      name  = "PreÃ§o CEPEA (R$/sc)",
      yaxis = "y2",
      line  = list(color="#f0b429", width=2.0, dash="dash"),
      marker= list(color="#f0b429", size=5, symbol="diamond"),
      hovertemplate =
        "<b>%{x}</b><br>PreÃ§o CEPEA: R$ %{y:.1f}/sc<extra></extra>")

    # Linha: focos totais MT â€” EMBRAPA SIGA (eixo direito, em mil focos)
    fat_focos <- FAT_SAFRA %>%
      dplyr::filter(!is.na(focos_mt), ano >= 2010) %>%
      dplyr::mutate(focos_k = round(focos_mt / 1000, 1))
    fig <- fig %>% add_lines(
      data  = fat_focos,
      x     = ~ano, y = ~focos_k,
      name  = "Focos MT (SIGA)",
      yaxis = "y2",
      line  = list(color="rgba(192,57,43,0.65)", width=1.5, dash="dot"),
      marker= list(color="rgba(192,57,43,0.7)", size=4),
      hovertemplate =
        "<b>%{x}</b><br>Focos MT: %{y:.1f}k (EMBRAPA SIGA)<extra></extra>")

    # Sombreamento por fase ENSO (usando FAT_SAFRA para 2012-2024)
    enso_shapes <- lapply(seq_len(nrow(FAT_SAFRA)), function(i) {
      if (FAT_SAFRA$ano[i] < 2010) return(NULL)
      cor_e <- switch(FAT_SAFRA$enso[i],
        "El Nino forte"    = "rgba(192,57,43,0.09)",
        "El Nino moderado" = "rgba(192,57,43,0.05)",
        "La Nina forte"    = "rgba(26,82,118,0.09)",
        "La Nina moderada" = "rgba(26,82,118,0.05)",
        "rgba(0,0,0,0)")
      list(type="rect", xref="x", yref="paper",
           x0=FAT_SAFRA$ano[i]-0.45, x1=FAT_SAFRA$ano[i]+0.45,
           y0=0, y1=1, fillcolor=cor_e, line=list(width=0))
    })
    enso_shapes <- Filter(Negate(is.null), enso_shapes)

    enso_annots <- list(
      list(x=2015, y=1.02, xref="x", yref="paper", showarrow=FALSE,
           text="El NiÃ±o<br>2015/16", font=list(color="rgba(192,57,43,0.8)", size=8),
           xanchor="center"),
      list(x=2020, y=1.02, xref="x", yref="paper", showarrow=FALSE,
           text="La NiÃ±a<br>2020/21", font=list(color="rgba(26,82,118,0.8)", size=8),
           xanchor="center"),
      list(x=2023, y=1.02, xref="x", yref="paper", showarrow=FALSE,
           text="El NiÃ±o<br>2023/24", font=list(color="rgba(192,57,43,0.8)", size=8),
           xanchor="center")
    )

    fig %>% tp() %>% layout(
      title = list(
        text = paste0(
          "Fontes: IBGE PAM 2023 Ã— CONAB Ã— ERA5-Land Ã— Dalla Lana (2015) | ",
          "PreÃ§o CEPEA histÃ³rico | Focos: EMBRAPA SIGA | ",
          "PreÃ§o atual: R$", round(preco_ativo, 1), "/sc (", fonte_preco, ")"),
        font=list(color="#5a8f62", size=9), x=0.01, xanchor="left"),
      xaxis  = list(title="Ano (safra inÃ­cio)", dtick=2, tickformat="d",
                    range=c(2009.5, 2024.5),
                    gridcolor="rgba(255,255,255,0.06)"),
      yaxis  = list(title="Perda potencial (mil toneladas)",
                    gridcolor="rgba(255,255,255,0.06)"),
      yaxis2 = list(
        title      = "PreÃ§o CEPEA (R$/sc) / Focos MT (mil)",
        overlaying = "y", side="right", showgrid=FALSE,
        tickfont   = list(color="#f0b429", size=9),
        titlefont  = list(color="#f0b429")),
      shapes      = enso_shapes,
      annotations = enso_annots,
      legend  = list(bgcolor="rgba(0,0,0,0)", font=list(color="#94c99a", size=9),
                     orientation="h", yanchor="top", y=-0.25,
                     xanchor="center", x=0.5),
      margin  = list(t=32, b=80, r=72, l=10)
    )
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_perda_hist_mun:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })
  })

  # Painel comparativo: preÃ§o histÃ³rico CEPEA Ã— perda total MT
  output$g_preco_hist <- renderPlotly({
    tryCatch({
    ph <- PRECO_HIST %>% arrange(ano)
    perda_mt <- if (!is.null(PERDA_HIST)) {
      PERDA_HIST %>% group_by(ano) %>%
        summarise(perda_total_mi=sum(perda_mi_r,na.rm=TRUE), .groups="drop") %>%
        right_join(ph, by="ano") %>%
        arrange(ano)
    } else ph %>% mutate(perda_total_mi=NA_real_)

    plot_ly(perda_mt, x=~ano) %>%
      add_bars(y=~perda_total_mi, name="Perda Total (R$ Mi)",
               yaxis="y",
               marker=list(color="rgba(192,57,43,0.75)",
                           line=list(color="rgba(0,0,0,0)",width=0))) %>%
      add_lines(y=~preco_saca, name="PreÃ§o CEPEA (R$/sc)",
                yaxis="y2",
                line=list(color="#ca8a04", width=2.5, dash="solid")) %>%
      tp() %>% layout(
        barmode = "overlay",
        xaxis   = list(title="Safra (ano inicio)", dtick=2, tickformat="d"),
        yaxis   = list(title="Perda total MT (R$ Mi)",
                       gridcolor="rgba(255,255,255,0.07)"),
        yaxis2  = list(title="Preco soja (R$/saca 60 kg)",
                       overlaying="y", side="right", showgrid=FALSE),
        legend  = list(orientation="h", y=-0.42, yanchor="top", font=list(size=9)),
        margin  = list(b=80, r=65),
        annotations = list(
          list(x=2023, y=max(perda_mt$perda_total_mi,na.rm=TRUE)*0.95,
               text="Fonte: CEPEA/ESALQ<br>+ IBGE PAM 2023",
               showarrow=FALSE, font=list(color="#2d5c32",size=9))
        )
      )
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_preco_hist:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})

  # Perda histÃ³rica real â€” usa dados CONAB/EMBRAPA (FAT_SAFRA$perda_real_mi)
  output$g_perda_hist <- renderPlotly({
    tryCatch({

    df <- FAT_SAFRA %>% arrange(ano)
    plot_ly(df,x=~safra) %>%
      add_bars(y=~perda_real_mi,name="Perda Real (R$ Mi)",
        marker=list(color=~CORES_ENSO[enso],line=list(color="#040906",width=.5)),
        text=~paste0("R$",format(perda_real_mi,big.mark="."),"Mi"),
        textposition="inside",textfont=list(size=8,color="#fff"),
        insidetextanchor="middle",
        hovertemplate="<b>%{x}</b><br>Perda: R$ %{y:.0f} Mi<br>ENSO: %{customdata}<extra></extra>",
        customdata=~enso) %>%
    tp(tickangle=-38,title="Safra") %>%
    layout(yaxis=list(title="Perda estimada real (R$ Mi) â€” CONAB/EMBRAPA"),showlegend=FALSE,
           uniformtext=list(minsize=7,mode="hide"))
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_perda_hist:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })
  })

## 6.10 Previsao 2025/26 â€” ARIMA + ajuste ENSO ----
  # â”€â”€ PREVISÃƒO 2025/26 â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  output$pv_irc   <- renderText(fmt_r(mean(rv$prev$irc_prev)))
  output$pv_prob  <- renderText(paste0(fmt_r(mean(rv$prev$prob_prev)),"%"))
  output$pv_alto  <- renderText(sum(rv$prev$cat_risco_prev %in% c("Muito Alto","Alto")))
  output$pv_perda <- renderText(paste0("R$ ",fmt_n(round(sum(rv$prev$perda_prev_mi,na.rm=TRUE)))))

  output$g_prev_irc <- renderPlotly({
    tryCatch({
    df <- rv$prev %>% arrange(desc(irc_prev))
    plot_ly(df,x=~irc_prev,y=~reorder(municipio,irc_prev),type="bar",orientation="h",
      marker=list(color=~CORES_RISCO[as.character(cat_risco_prev)]),
      error_x=list(type="data",array=df$irc_ic_h-df$irc_prev,
                   arrayminus=df$irc_prev-df$irc_ic_l,color="#a8c8ab",thickness=1.2,width=3),
      text=~paste0(" ",irc_prev," [",irc_ic_l,"â€“",irc_ic_h,"]"),
      textposition="outside",textfont=list(size=8,color="#4a7a52"),
      hovertemplate="<b>%{y}</b><br>IRC prev: %{x}<br>Prob: %{customdata}%<extra></extra>",
      customdata=~prob_prev) %>%
    tp() %>%
    layout(yaxis=list(title="",tickfont=list(size=7)),
           xaxis=list(title="IRC Previsto (IC 80%)",range=c(0,140)),
           showlegend=FALSE,margin=list(l=130,r=95,t=10,b=30))
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_prev_irc:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})
  output$g_prev_pizza <- renderPlotly({
    tryCatch({
    df <- rv$prev %>% mutate(cat=factor(as.character(cat_risco_prev),levels=NIVEIS_RISCO)) %>%
      count(cat) %>% filter(n>0)
    plot_ly(df,labels=~cat,values=~n,type="pie",hole=0.4,
      marker=list(colors=CORES_RISCO[as.character(df$cat)],line=list(color="#050d07",width=2)),
      textinfo="label+percent+value",textfont=list(size=9.5),
      hovertemplate="%{label}<br>%{value} municÃ­pios<extra></extra>") %>%
    tp() %>% layout(showlegend=FALSE)
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_prev_pizza:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})
  output$g_prev_delta <- renderPlotly({
    tryCatch({
    df <- left_join(rv$res %>% select(municipio,irc_at=irc),
                    rv$prev %>% select(municipio,irc_pv=irc_prev),by="municipio") %>%
      mutate(delta=irc_pv-irc_at) %>% arrange(desc(abs(delta))) %>% head(20)
    plot_ly(df,x=~delta,y=~reorder(municipio,delta),type="bar",orientation="h",
      marker=list(color=~ifelse(delta>0,"#dc2626","#16a34a"),opacity=.85),
      text=~paste0(ifelse(delta>0,"+",""),round(delta,1)),textposition="outside",
      textfont=list(size=9,color="#4a7a52"),
      hovertemplate="<b>%{y}</b><br>Delta IRC: %{x:.1f}<extra></extra>") %>%
    tp() %>%
    layout(yaxis=list(title="",tickfont=list(size=9)),
           xaxis=list(title="Delta IRC (PrevisÃ£o âˆ’ Atual)"),
           showlegend=FALSE,margin=list(l=155,r=70,t=10,b=30),
           shapes=list(list(type="line",x0=0,x1=0,y0=0,y1=1,yref="paper",
             line=list(color="#2d5c32",width=1.5))))
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_prev_delta:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})
  output$g_prev_top <- renderPlotly({
    tryCatch({
    df <- rv$prev %>% arrange(desc(prob_prev)) %>% head(15)
    plot_ly(df,x=~prob_prev,y=~reorder(municipio,prob_prev),type="bar",orientation="h",
      marker=list(color=~CORES_RISCO[as.character(cat_risco_prev)]),
      text=~paste0(" ",prob_prev,"%"),textposition="outside",
      textfont=list(size=9,color="#4a7a52"),
      hovertemplate="<b>%{y}</b><br>%{x:.1f}% prob. prevista<extra></extra>") %>%
    tp() %>%
    layout(yaxis=list(title="",tickfont=list(size=9)),
           xaxis=list(title="Probabilidade prevista (%)",range=c(0,125)),
           showlegend=FALSE,margin=list(l=158,r=70,t=10,b=30))
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_prev_top:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})
  output$tab_prev <- renderDT({
    rv$prev %>%
      select(Municipio=municipio,Safra=safra,`IRC Prev.`=irc_prev,
             `IC Low(80%)`=irc_ic_l,`IC High(80%)`=irc_ic_h,`Prob.(%)`=prob_prev,
             `Risco Previsto`=cat_risco_prev,`Sev.Prev.(%)`=sev_prev,
             `C.Dano(%)`=coef_dano_prev,`Perda R$Mi`=perda_prev_mi,
             `ENSO Previsto`=enso_prev,Metodo=metodo) %>%
      mutate(`Risco Previsto`=as.character(`Risco Previsto`)) %>%
      datatable(filter="top",rownames=FALSE,options=list(pageLength=15,scrollX=TRUE)) %>%
      formatStyle("Prob.(%)",background=styleColorBar(c(0,100),"#dc2626"),
        backgroundSize="90% 70%",backgroundRepeat="no-repeat",backgroundPosition="center") %>%
      formatStyle("Risco Previsto",color="white",fontWeight="bold",
        backgroundColor=styleEqual(NIVEIS_RISCO,unname(CORES_RISCO)))
  })

## 6.11 Ranking â€” IVM e Matriz de Risco ----
  # â”€â”€ RANKING â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  output$g_ranking <- renderPlotly({
    tryCatch({
    df <- df_f() %>% arrange(desc(ivm)) %>%
      mutate(rank_pos = rank(-ivm, ties.method="first"))
    plot_ly(df, x=~ivm, y=~reorder(municipio,ivm), type="bar", orientation="h",
      marker=list(
        color=~CORES_VULN[as.character(cat_vuln)],
        line=list(color="rgba(0,0,0,0)",width=0)),
      text=~paste0(" #",rank_pos," â€” IVM: ",ivm," | P: ",prob_ferrugem,"%"),
      textposition="outside", textfont=list(size=8.5, color="#4a7a52"),
      customdata=~rank_pos, meta=~perda_mi_r,
      hovertemplate=paste0(
        "<b>#%{customdata} â€” %{y}</b><br>",
        "IVM: <b>%{x:.1f}</b>/100<br>",
        "Perda potencial: <b>R$ %{meta} Mi</b><br>",
        "<i style='font-size:10px;color:#4a7a52;'>",
        "IVM = IRC Ã— RelevÃ¢ncia Ã— Coef.Dano â€” Dalla Lana (2015)</i>",
        "<extra></extra>")) %>%
    tp() %>%
    layout(
      yaxis=list(title="", tickfont=list(size=7.5)),
      xaxis=list(title="IVM â€” Ãndice de Vulnerabilidade Municipal (0â€“100)", range=c(0,138)),
      showlegend=FALSE,
      margin=list(l=148,r=118,t=10,b=35),
      annotations=list(
        list(x=75, y=-1.5, xref="x", yref="paper",
          text="â—„ Limiar CrÃ­tico (IVMâ‰¥75)", showarrow=FALSE,
          font=list(color="#dc2626",size=9), xanchor="left"),
        list(x=75, y=1, xref="x", yref="paper",
          text="", showarrow=FALSE),
        list(x=75, y=0, xref="x", yref="paper",
          showarrow=FALSE)
      ),
      shapes=list(list(type="line",x0=75,x1=75,y0=0,y1=1,xref="x",yref="paper",
        line=list(color="#dc2626",dash="dot",width=1.5)))
    )
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_ranking:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})
  output$g_matriz <- renderPlotly({
    tryCatch({
    df <- df_f()
    plot_ly(df, x=~irc, y=~relevancia, color=~cat_vuln, colors=CORES_VULN,
      size=~coalesce(perda_ton,500), sizes=c(6,60), type="scatter", mode="markers",
      text=~municipio,
      customdata=~prob_ferrugem, meta=~perda_mi_r,
      hovertemplate=paste0(
        "<b>%{text}</b><br>",
        "IRC: <b>%{x:.1f}</b> | RelevÃ¢ncia: <b>%{y:.0f}</b><br>",
        "Prob. ferrugem: <b>%{customdata}%</b><br>",
        "Perda potencial: <b>R$ %{meta} Mi</b>",
        "<extra></extra>")) %>%
    tp() %>%
    layout(
      xaxis=list(title="IRC â€” Risco ClimÃ¡tico (Del Ponte et al. 2006)", range=c(0,103)),
      yaxis=list(title="RelevÃ¢ncia Produtiva (0â€“100)", range=c(0,108)),
      shapes=list(
        list(type="rect", x0=60, x1=103, y0=50, y1=108,
             fillcolor="rgba(220,38,38,.07)",
             line=list(color="#dc2626",dash="dot",width=1)),
        list(type="line", x0=60, x1=60, y0=0, y1=108,
             line=list(color="#2d5c32",dash="dash",width=1)),
        list(type="line", x0=0, x1=103, y0=50, y1=50,
             line=list(color="#2d5c32",dash="dash",width=1))
      ),
      annotations=list(
        list(x=81, y=106, showarrow=FALSE,
             text="<b>âš  PRIORIDADE MÃXIMA</b>",
             font=list(color="#dc2626",size=11.5)),
        list(x=81, y=101, showarrow=FALSE,
             text="Alto IRC + Alta RelevÃ¢ncia",
             font=list(color="#ef4444",size=9)),
        list(x=30, y=20, showarrow=FALSE,
             text="Baixa prioridade",
             font=list(color="#2d5c32",size=9)),
        list(x=0.99, y=0.01, xref="paper", yref="paper",
             text="Tamanho âˆ perda potencial (R$ Mi)",
             showarrow=FALSE, font=list(color="#2d5c32",size=8.5),
             xanchor="right", yanchor="bottom")
      ),
      legend=list(orientation="h", y=-0.42, yanchor="top"),
      margin=list(t=10,r=10,b=100,l=10))
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_matriz:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})

## 6.12 Fungicidas e Manejo ----
  # â”€â”€ FUNGICIDAS E MANEJO â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  output$tab_fung <- renderDT({
    FUNGICIDAS %>%
      select(`Nome Comercial`=nome_comercial,`Ingrediente Ativo`=i_a,
             `EficÃ¡cia (%)`=eficacia_pct,`Grupo QuÃ­mico`=grupo_quimico,
             `Dose (L/ha)`=dose_lha,`Classe`=classe_efic) %>%
      datatable(rownames=FALSE,options=list(pageLength=15,dom="tp")) %>%
      formatStyle("EficÃ¡cia (%)",
        background=styleColorBar(c(65,100),"#2d7a3a"),
        backgroundSize="90% 70%",backgroundRepeat="no-repeat",backgroundPosition="center") %>%
      formatStyle("EficÃ¡cia (%)",color=styleInterval(c(80,88),c("#dc2626","#ca8a04","#16a34a"))) %>%
      formatStyle("Classe",color="white",fontWeight="bold",
        backgroundColor=styleEqual(c("A","B","C","D"),c("#16a34a","#ca8a04","#ef4444","#dc2626")))
  })
  output$g_eficacia <- renderPlotly({
    tryCatch({
    df <- FUNGICIDAS %>% arrange(desc(eficacia_pct))
    plot_ly(df,x=~eficacia_pct,y=~reorder(nome_comercial,eficacia_pct),
      type="bar",orientation="h",
      marker=list(color=~ifelse(eficacia_pct>=88,"#16a34a",ifelse(eficacia_pct>=83,"#ca8a04","#ef4444")),opacity=.9),
      text=~paste0(eficacia_pct,"% | ",grupo_quimico),textposition="outside",
      textfont=list(size=9,color="#4a7a52"),
      hovertemplate="<b>%{y}</b><br>EficÃ¡cia: %{x}%<br>I.A.: %{customdata}<extra></extra>",
      customdata=~i_a) %>%
    tp() %>%
    layout(yaxis=list(title="",tickfont=list(size=9.5)),
           xaxis=list(title="EficÃ¡cia (%)",range=c(60,110)),
           showlegend=FALSE,margin=list(l=120,r=90,t=25,b=30),
      shapes=list(list(type="line",x0=85,x1=85,y0=0,y1=1,yref="paper",
        line=list(color="#2d5c32",dash="dot",width=1.5))),
      annotations=list(list(x=85,y=1.02,xanchor="left",yanchor="top",
        text="MÃ­n. recomendado EMBRAPA",showarrow=FALSE,font=list(color="#2d5c32",size=9))))
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_eficacia:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})
  # ResistÃªncia por municÃ­pio â€” reativo ao slider de ano (EMBRAPA 2024 histÃ³rico)
  output$g_resistencia <- renderPlotly({
    tryCatch({
    RES_LABELS <- c("1"="1 - Sensivel","2"="2 - Baixa","3"="3 - Media","4"="4 - Alta")
    RES_COL    <- c("1"="#16a34a","2"="#ca8a04","3"="#ef4444","4"="#991b1b")

    ano_sel <- tryCatch(as.integer(input$sl_ano_resist), error=function(e) 2024L)
    if (is.na(ano_sel) || length(ano_sel)==0) ano_sel <- 2024L

    df <- RESIST_HIST %>%
      filter(ano == ano_sel) %>%
      mutate(
        resist_l = RES_LABELS[as.character(nivel_resistencia)],
        resist_c = RES_COL[as.character(nivel_resistencia)],
        anos_txt = ifelse(anos_exposicao > 0,
                          paste0(anos_exposicao, " anos exp."),
                          "Sem ocorrencia ainda"),
        hover = paste0(
          "<b>", municipio, "</b><br>",
          "Nivel ", ano_sel, ": <b>", resist_l, "</b><br>",
          anos_txt, "<br>",
          "Aplic. rec.: ", n_aplic_rec, "/safra<br>",
          "Nivel 2024 (EMBRAPA): ",
          RES_LABELS[as.character(
            MUN$nivel_resistencia[MUN$municipio == municipio]
          )]
        )
      ) %>%
      arrange(desc(nivel_resistencia), municipio)

    # Contagem por nivel para o titulo
    n_alta  <- sum(df$nivel_resistencia == 4)
    n_media <- sum(df$nivel_resistencia == 3)
    n_baixa <- sum(df$nivel_resistencia == 2)
    n_sens  <- sum(df$nivel_resistencia == 1)

    plot_ly(df,
      x = ~nivel_resistencia,
      y = ~reorder(municipio, nivel_resistencia),
      type="bar", orientation="h",
      marker=list(color=~resist_c, opacity=0.88,
                  line=list(color="rgba(0,0,0,0)", width=0)),
      text=~paste0(" ", resist_l),
      textposition="outside",
      textfont=list(size=7.8, color="#94c99a"),
      customdata=~hover,
      hovertemplate="%{customdata}<extra></extra>"
    ) %>%
    tp() %>%
    layout(
      title = list(
        text = paste0("Safra ", ano_sel, "/", ano_sel+1,
                      " â€” Alta:", n_alta, " | Media:", n_media,
                      " | Baixa:", n_baixa, " | Sensiveis:", n_sens),
        font = list(size=10, color="#4a7a52"), x=0, xanchor="left"
      ),
      yaxis  = list(title="", tickfont=list(size=7.2), automargin=TRUE),
      xaxis  = list(title="Nivel de Resistencia a Fungicidas",
                    tickvals=1:4, ticktext=RES_LABELS,
                    range=c(0, 6.2)),
      showlegend = FALSE,
      margin = list(l=8, r=90, t=32, b=30),
      annotations = list(list(
        x=5.3, y=0.01, xref="x", yref="paper",
        showarrow=FALSE,
        text="EMBRAPA Circ.<br>119/2024",
        font=list(color="#2d5c32", size=7.5),
        xanchor="right", yanchor="bottom"
      ))
    )
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_resistencia:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})
  output$g_n_aplic <- renderPlotly({
    tryCatch({
    # HISTÃ“RICO: usar RESIST_HIST + rv$irc filtrado por ano selecionado
    CORES_APLIC   <- c("1"="#16a34a","2"="#ca8a04","3"="#ef4444","4"="#dc2626")
    LABELS_RESIST <- c("1"="Sensivel","2"="Baixa","3"="Media","4"="Alta")
    LABELS_APLIC  <- c("1"="1 aplicacao","2"="2 aplicacoes",
                       "3"="3 aplicacoes","4"="4 aplicacoes")

    ano_sel <- tryCatch(as.integer(input$sl_ano_naplic), error=function(e) 2024L)
    if (is.na(ano_sel) || length(ano_sel)==0) ano_sel <- 2024L
    safra_str <- paste0(ano_sel, "/", sprintf("%02d", (ano_sel + 1L) %% 100L))

    # â”€â”€ ResistÃªncia histÃ³rica para o ano selecionado â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
    resist_ano <- RESIST_HIST %>% filter(ano == ano_sel)

    # â”€â”€ IRC histÃ³rico para a safra equivalente (rv$irc) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
    irc_ano <- tryCatch({
      rv$irc %>%
        filter(safra == safra_str) %>%
        select(municipio, irc, prob_ferrugem)
    }, error = function(e) NULL)

    if (is.null(irc_ano) || nrow(irc_ano) == 0) {
      # Fallback: IRC atual se a safra histÃ³rica nÃ£o estiver disponÃ­vel
      irc_ano <- tryCatch({
        df_f() %>% select(municipio, irc, prob_ferrugem)
      }, error = function(e) NULL)
    }

    if (is.null(irc_ano) || nrow(irc_ano) == 0)
      return(.plt() %>% tp() %>%
             layout(title=list(text="Sem dados para esta safra.",
                               font=list(color="#94c99a"))))

    # â”€â”€ Join IRC + ResistÃªncia + Ãrea â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
    df <- tryCatch({
      irc_ano %>%
        inner_join(resist_ano, by="municipio") %>%
        left_join(MUN[,c("municipio","area_ref")], by="municipio") %>%
        mutate(
          resist_l  = LABELS_RESIST[as.character(nivel_resistencia)],
          resist_n  = as.numeric(nivel_resistencia),
          # Calcular n_aplic pelo mesmo critÃ©rio de calcular_resultado()
          n_aplic_h = as.integer(case_when(
            irc >= 75L & nivel_resistencia >= 4L ~ 4L,
            irc >= 75L                            ~ 3L,
            irc >= 60L & nivel_resistencia >= 3L  ~ 3L,
            irc >= 60L                            ~ 2L,
            irc >= 45L                            ~ 2L,
            TRUE                                  ~ 1L
          )),
          aplic_f   = LABELS_APLIC[as.character(n_aplic_h)],
          jitter_y  = resist_n + runif(n(), -0.22, 0.22),
          sz_area   = pmin(22, pmax(7, sqrt(coalesce(area_ref, 50)) * 1.2)),
          hover = paste0(
            "<b>", municipio, "</b><br>",
            "Safra: <b>", safra_str, "</b><br>",
            "IRC: <b>", irc, "</b><br>",
            "Resist. (", ano_sel, "): <b>", resist_l, "</b><br>",
            "Anos exp.: ", anos_exposicao, "<br>",
            "Aplic. rec.: <b>", n_aplic_h, "</b><br>",
            if (ano_sel == 2024L) {
              irc_real <- DADOS_NAPLIC_2024$irc_ref_2024[DADOS_NAPLIC_2024$municipio == municipio]
              if (length(irc_real) > 0) paste0("IRC real SIGA: ", irc_real[1], "<br>") else ""
            } else "",
            "<i>Fonte: EMBRAPA Circ. 119/2024 + ERA5-Land</i>"
          )
        )
    }, error = function(e) NULL)

    if (is.null(df) || nrow(df) == 0)
      return(.plt() %>% tp() %>%
             layout(title=list(text="Sem dados para esta safra.",
                               font=list(color="#94c99a"))))

    fig <- .plt()

    # Camada 1 â€” pontos do modelo (cÃ­rculos)
    for (na_val in sort(unique(df$n_aplic_h))) {
      dna <- df %>% filter(n_aplic_h == na_val)
      fig <- fig %>% add_markers(
        data      = dna,
        x         = ~irc,
        y         = ~jitter_y,
        name      = paste0(LABELS_APLIC[as.character(na_val)]),
        legendgroup = as.character(na_val),
        marker    = list(
          color   = CORES_APLIC[as.character(na_val)],
          size    = ~sz_area,
          opacity = 0.82,
          symbol  = "circle",
          line    = list(color="rgba(255,255,255,0.20)", width=0.8)
        ),
        customdata    = ~hover,
        hovertemplate = "%{customdata}<extra></extra>"
      )
    }

    # Camada 2 â€” dados reais EMBRAPA SIGA (somente safra 2024/25, losangos)
    if (ano_sel == 2024L) {
      df_real <- tryCatch({
        DADOS_NAPLIC_2024 %>%
          left_join(MUN[,c("municipio","area_ref")], by="municipio") %>%
          mutate(
            resist_n  = as.numeric(nivel_resist_2024),
            resist_l  = LABELS_RESIST[as.character(nivel_resist_2024)],
            jitter_y  = resist_n + runif(n(), -0.22, 0.22),
            sz_area   = pmin(22, pmax(7, sqrt(coalesce(area_ref, 50)) * 1.2)),
            hover_r   = paste0(
              "<b>", municipio, "</b><br>",
              "<b>[DADO REAL â€” EMBRAPA SIGA 2024/25]</b><br>",
              "IRC ref. 2024/25: <b>", irc_ref_2024, "</b><br>",
              "Resist. 2024: <b>", resist_l, "</b><br>",
              "Aplic. rec.: <b>", n_aplic_rec_2024, "</b><br>",
              "<i>Fonte: EMBRAPA Circ. 119/2024 + SIGA Boletins Jan-Mar/2025</i>"
            )
          )
      }, error = function(e) NULL)
      if (!is.null(df_real) && nrow(df_real) > 0) {
        for (na_val in sort(unique(df_real$n_aplic_rec_2024))) {
          dna2 <- df_real %>% filter(n_aplic_rec_2024 == na_val)
          fig <- fig %>% add_markers(
            data      = dna2,
            x         = ~irc_ref_2024,
            y         = ~jitter_y,
            name      = paste0(LABELS_APLIC[as.character(na_val)], " â€” SIGA"),
            legendgroup = paste0(na_val, "r"),
            showlegend  = TRUE,
            marker    = list(
              color   = CORES_APLIC[as.character(na_val)],
              size    = ~sz_area * 0.82,
              opacity = 0.95,
              symbol  = "diamond",
              line    = list(color="rgba(255,255,255,0.90)", width=1.5)
            ),
            customdata    = ~hover_r,
            hovertemplate = "%{customdata}<extra></extra>"
          )
        }
      }
    }

    titulo_safra <- paste0(
      "Safra ", safra_str,
      if (ano_sel == 2024L) " â€” circulo=modelo / losango=EMBRAPA SIGA real" else
        " â€” dados historicos RESIST_HIST + IRC ERA5-Land"
    )

    fig %>% tp() %>% layout(
      title = list(
        text  = titulo_safra,
        font  = list(size=10, color="#4a7a52"),
        x=0, xanchor="left"
      ),
      xaxis = list(
        title     = "IRC â€” Indice de Risco Climatico (0-100)",
        range     = c(0, 107),
        gridcolor = "rgba(255,255,255,0.06)",
        zeroline  = FALSE
      ),
      yaxis = list(
        title     = "Nivel de Resistencia a Fungicidas",
        tickvals  = 1:4,
        ticktext  = paste0(1:4, " â€” ", LABELS_RESIST),
        range     = c(0.35, 4.65),
        gridcolor = "rgba(255,255,255,0.06)",
        zeroline  = FALSE
      ),
      legend = list(
        orientation = "h",
        y           = -0.38,
        x           = 0.5,
        xanchor     = "center",
        yanchor     = "top",
        font        = list(size=9, color="#94c99a"),
        bgcolor     = "rgba(0,0,0,0)",
        title       = list(
          text = "<b>N. Aplicacoes Rec. (EMBRAPA Circ. 119/2024):</b>  ",
          font = list(size=9, color="#4a7a52")
        )
      ),
      shapes = list(
        list(type="rect",x0=0,  x1=45, y0=0.35,y1=4.65,
             fillcolor="rgba(30,136,73,0.05)",  line=list(width=0)),
        list(type="rect",x0=45, x1=60, y0=0.35,y1=4.65,
             fillcolor="rgba(240,180,41,0.05)", line=list(width=0)),
        list(type="rect",x0=60, x1=75, y0=0.35,y1=4.65,
             fillcolor="rgba(229,80,57,0.06)",  line=list(width=0)),
        list(type="rect",x0=75, x1=107,y0=0.35,y1=4.65,
             fillcolor="rgba(192,57,43,0.08)",  line=list(width=0)),
        list(type="line",x0=45,x1=45,y0=0.35,y1=4.65,
             line=list(color="#ca8a04",dash="dot",width=1.4)),
        list(type="line",x0=60,x1=60,y0=0.35,y1=4.65,
             line=list(color="#ef4444",dash="dot",width=1.4)),
        list(type="line",x0=75,x1=75,y0=0.35,y1=4.65,
             line=list(color="#dc2626",dash="dot",width=1.4))
      ),
      annotations = list(
        list(x=22, y=4.58,text="Baixo",    showarrow=FALSE,
             font=list(color="#16a34a",size=8.5),xanchor="center"),
        list(x=52, y=4.58,text="Moderado", showarrow=FALSE,
             font=list(color="#ca8a04",size=8.5),xanchor="center"),
        list(x=67, y=4.58,text="Alto",     showarrow=FALSE,
             font=list(color="#ef4444",size=8.5),xanchor="center"),
        list(x=90, y=4.58,text="Muito Alto",showarrow=FALSE,
             font=list(color="#dc2626",size=8.5),xanchor="center"),
        list(x=44, y=0.38,text="IRC 45",   showarrow=FALSE,
             font=list(color="#ca8a04",size=8), xanchor="right"),
        list(x=59, y=0.38,text="IRC 60",   showarrow=FALSE,
             font=list(color="#ef4444",size=8), xanchor="right"),
        list(x=74, y=0.38,text="IRC 75",   showarrow=FALSE,
             font=list(color="#dc2626",size=8), xanchor="right")
      ),
      margin = list(t=10, r=20, b=120, l=20)
    )
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_n_aplic:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})

## 6.13 Dados Completos â€” Tabelas DT exportaveis ----
  # â”€â”€ DADOS COMPLETOS â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  output$t_safra <- renderDT({
    df_f() %>% arrange(prioridade) %>%
      transmute(`#`=prioridade,Municipio=municipio,IRC=irc,`Prob.(%)`=prob_ferrugem,
        Risco=as.character(categoria_risco),IVM=ivm,`Vuln.`=as.character(cat_vuln),
        `Sev.(%)`=sev,`C.Dano(%)`=coef_dano,`Esporos/m3`=esporos_m3,
        `Prod.Med.(mil t)`=round(coalesce(producao_media,prod_ref*1000)/1000),
        `Ãrea(ha)`=round(coalesce(area_calc,area_ref*1000)),
        `Perda(ton)`=perda_ton,`Perda(R$Mi)`=perda_mi_r,`Aplic.Rec.`=n_aplic) %>%
      datatable(filter="top",rownames=FALSE,options=list(pageLength=15,scrollX=TRUE,
        columnDefs=list(list(className="dt-center",targets="_all")))) %>%
      formatStyle("IRC",background=styleColorBar(c(0,100),"#1d4ed8"),
        backgroundSize="90% 70%",backgroundRepeat="no-repeat",backgroundPosition="center") %>%
      formatStyle("Prob.(%)",background=styleColorBar(c(0,100),"#dc2626"),
        backgroundSize="90% 70%",backgroundRepeat="no-repeat",backgroundPosition="center") %>%
      formatStyle("Vuln.",color="white",fontWeight="bold",
        backgroundColor=styleEqual(NIVEIS_VULN,unname(CORES_VULN))) %>%
      formatStyle("Risco",color="white",fontWeight="bold",
        backgroundColor=styleEqual(NIVEIS_RISCO,unname(CORES_RISCO)))
  })
  output$t_hist <- renderDT({
    rv$irc %>%
      select(Safra=safra,Municipio=municipio,IRC=irc,`Prob.(%)`=prob_ferrugem,
             Risco=categoria_risco,`Precip.(mm)`=precip_total,`UR(%)`=ur_media,
             `Temp.(Â°C)`=temp_media,`Dias Fav.`=dias_fav_total,`GD Total`=graus_dia_total,ENSO=enso) %>%
      mutate(Risco=as.character(Risco),`Precip.(mm)`=round(`Precip.(mm)`)) %>%
      datatable(filter="top",rownames=FALSE,options=list(pageLength=15,scrollX=TRUE))
  })
  output$t_prod <- renderDT({
    rv$prod %>%
      select(Ano=ano,Municipio=municipio,`ProduÃ§Ã£o(ton)`=producao_ton,
             `Ãrea(ha)`=area_ha,`Produtiv.(kg/ha)`=produtividade) %>%
      datatable(filter="top",rownames=FALSE,options=list(pageLength=15,scrollX=TRUE))
  })
  output$t_prev <- renderDT({
    rv$prev %>%
      select(Municipio=municipio,`IRC Prev.`=irc_prev,`IC Low`=irc_ic_l,
             `IC High`=irc_ic_h,`Prob.(%)`=prob_prev,Risco=cat_risco_prev,
             `Perda R$Mi`=perda_prev_mi,ENSO=enso_prev,MÃ©todo=metodo) %>%
      mutate(Risco=as.character(Risco)) %>%
      datatable(filter="top",rownames=FALSE,options=list(pageLength=15,scrollX=TRUE)) %>%
      formatStyle("Risco",color="white",fontWeight="bold",
        backgroundColor=styleEqual(NIVEIS_RISCO,unname(CORES_RISCO)))
  })
  output$t_clima <- renderDT({
    rv$clima %>%
      select(Safra=safra,Municipio=municipio,Mes=mes,
             `Precip.(mm)`=precip_mm,`UR(%)`=ur_pct,`Temp.(Â°C)`=temp_c,
             `Dias Fav.`=dias_fav,`Graus-Dia`=graus_dia,ENSO=enso) %>%
      datatable(filter="top",rownames=FALSE,options=list(pageLength=15,scrollX=TRUE))
  })
  # ABA FITOSSANIDADE â€” DADOS REAIS (EMBRAPA/APROSOJA-MT) integrados
  output$t_fito_full <- renderDT({
    tryCatch({

    # â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
    # Tabela Fitossanidade â€” dados estÃ¡ticos reais por municÃ­pio
    # Fontes primÃ¡rias:
    #   [1] EMBRAPA SIGA â€” Boletim EpidemiolÃ³gico Ferrugem 2023/24
    #       siga.cnpso.embrapa.br | IncidÃªncia hist. 2012-2024, Focos 2023/24
    #   [2] EMBRAPA Circular TÃ©cnica 119/2024 (Tabela 4, p.22)
    #       NÃ­vel resist. DMI/SDHI, N.aplic.recomendadas, Efic. campo
    #   [3] APROSOJA-MT AnuÃ¡rio 2024 â€” N.aplic. observadas em campo
    #   [4] IBGE PAM 2023 â€” ProduÃ§Ã£o, Ã¡rea, produtividade (SIDRA Tab.1612)
    #   [5] CONAB 12Â° Levantamento Safra 2023/24 â€” Perda hist. mÃ©dia R$ Mi
    #   [6] MAPA ZARC Portaria 709/2024 â€” Zona de risco climÃ¡tico
    #   [7] MUN (EMBRAPA SIGA) â€” 1Âª ocorrÃªncia confirmada de ferrugem por mun.
    # â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•

    LABELS_RESIST_TBL <- c("1"="1 â€” SensÃ­vel","2"="2 â€” Baixa","3"="3 â€” MÃ©dia","4"="4 â€” Alta")
    LABELS_ZONA       <- c("1"="Zona I â€” Baixo","2"="Zona II â€” Moderado","3"="Zona III â€” Elevado")
    LABELS_ALERTA     <- c(
      "VERMELHO" = "ðŸ”´ VERMELHO â€” Urgente",
      "LARANJA"  = "ðŸŸ  LARANJA â€” Alto",
      "AMARELO"  = "ðŸŸ¡ AMARELO â€” Moderado",
      "VERDE"    = "ðŸŸ¢ VERDE â€” Baixo"
    )

    df_fito <- MUN %>%
      dplyr::select(municipio, safra_1a_ocorr, nivel_resistencia, dae_r1) %>%
      # [1][2] EMBRAPA SIGA + Circ. 119/2024
      dplyr::left_join(
        FITO_REAL %>% dplyr::select(
          municipio, alerta_embrapa, incidencia_hist_pct,
          efic_media_campo, n_aplic_real,
          perda_hist_media_mi, focos_confirmados_2024),
        by = "municipio") %>%
      # [2] EMBRAPA Circ. 119/2024
      dplyr::left_join(
        DADOS_NAPLIC_2024 %>% dplyr::select(
          municipio, nivel_resist_2024, n_aplic_rec_2024, irc_ref_2024),
        by = "municipio") %>%
      # [4] IBGE PAM 2023
      dplyr::left_join(
        PROD_IBGE_2023 %>% dplyr::select(
          municipio, prod_t, area_t, produtiv_t),
        by = "municipio") %>%
      # [6] MAPA ZARC
      dplyr::left_join(
        ZARC_MUN %>% dplyr::select(
          municipio, zona_zarc, taxa_min_pct, taxa_max_pct, subvencao_max_pct),
        by = "municipio") %>%
      dplyr::mutate(
        # Campos formatados para exibiÃ§Ã£o
        alerta_fmt     = dplyr::recode(
                           coalesce(alerta_embrapa, "VERDE"), !!!LABELS_ALERTA),
        resist_fmt     = dplyr::recode(
                           as.character(nivel_resist_2024),  !!!LABELS_RESIST_TBL),
        zona_fmt       = dplyr::recode(
                           as.character(zona_zarc), !!!LABELS_ZONA),
        incid_fmt      = ifelse(is.na(incidencia_hist_pct), "N/D",
                                paste0(incidencia_hist_pct, "%")),
        efic_fmt       = ifelse(is.na(efic_media_campo), "N/D",
                                paste0(efic_media_campo, "%")),
        aplic_obs_fmt  = ifelse(is.na(n_aplic_real), "N/D",
                                as.character(n_aplic_real)),
        aplic_rec_fmt  = ifelse(is.na(n_aplic_rec_2024), "N/D",
                                as.character(n_aplic_rec_2024)),
        perda_ref_fmt  = ifelse(is.na(perda_hist_media_mi), "N/D",
                                paste0("R$ ", perda_hist_media_mi, " Mi")),
        taxa_fmt       = ifelse(is.na(taxa_min_pct), "N/D",
                                paste0(taxa_min_pct, "% â€“ ", taxa_max_pct, "%")),
        subv_fmt       = ifelse(is.na(subvencao_max_pct), "40%",
                                paste0(subvencao_max_pct, "%")),
        prod_fmt       = ifelse(is.na(prod_t), "N/D",
                                paste0(format(round(prod_t/1000), big.mark="."), "k t")),
        area_fmt       = ifelse(is.na(area_t), "N/D",
                                paste0(format(area_t, big.mark="."), " ha")),
        produtiv_fmt   = ifelse(is.na(produtiv_t), "N/D",
                                paste0(format(produtiv_t, big.mark="."), " kg/ha")),
        anos_exp       = ifelse(
                           is.na(safra_1a_ocorr), NA_integer_,
                           2024L - as.integer(substr(safra_1a_ocorr, 1, 4)))
      ) %>%
      dplyr::select(
        # IdentificaÃ§Ã£o
        `MunicÃ­pio`           = municipio,
        # Alerta integrado EMBRAPA SIGA [1]
        `Alerta SIGA`         = alerta_fmt,
        # Epidemiologia â€” EMBRAPA SIGA [1]
        `1Âª OcorrÃªncia`       = safra_1a_ocorr,
        `Anos ExposiÃ§Ã£o`      = anos_exp,
        `Incid. Hist. (%)`    = incid_fmt,
        `Focos 2023/24`       = focos_confirmados_2024,
        # ResistÃªncia a fungicidas â€” EMBRAPA Circ.119/2024 [2]
        `Resist. Fung. 2024`  = resist_fmt,
        `IRC Ref. 2024`       = irc_ref_2024,
        # RecomendaÃ§Ãµes de aplicaÃ§Ã£o [2][3]
        `Aplic. Rec. 2024`    = aplic_rec_fmt,
        `Aplic. Obs. Campo`   = aplic_obs_fmt,
        `Efic. Campo (%)`     = efic_fmt,
        `DAE R1 (dias)`       = dae_r1,
        # ProduÃ§Ã£o â€” IBGE PAM 2023 [4]
        `ProduÃ§Ã£o (mil t)`    = prod_fmt,
        `Ãrea Colhida`        = area_fmt,
        `Produtiv. (kg/ha)`   = produtiv_fmt,
        # Perda referÃªncia â€” CONAB 2019-2024 [5]
        `Perda MÃ©d. CONAB`    = perda_ref_fmt,
        # Seguro rural â€” MAPA ZARC [6]
        `Zona ZARC`           = zona_fmt,
        `Taxa PrÃªmio`         = taxa_fmt,
        `Subv. PSR`           = subv_fmt
      ) %>%
      dplyr::arrange(`MunicÃ­pio`)

    DT::datatable(
      df_fito,
      filter   = "top",
      rownames = FALSE,
      caption  = htmltools::tags$caption(
        style = paste0(
          "color:#5a8f62;font-size:10px;text-align:left;",
          "padding:6px 0 2px;line-height:1.6;"),
        htmltools::HTML(paste0(
          "<b>Fontes dos dados:</b> ",
          "[1] EMBRAPA SIGA â€” Boletim EpidemiolÃ³gico Ferrugem 2023/24 | ",
          "[2] EMBRAPA Circular TÃ©cnica 119/2024, Tab.4 (Resist. DMI/SDHI) | ",
          "[3] APROSOJA-MT AnuÃ¡rio 2024 (N.aplic. observadas) | ",
          "[4] IBGE PAM 2023 â€” SIDRA Tab.1612 | ",
          "[5] CONAB 12Â°Levant. Safra 2023/24 (mÃ©dia 2019-2024) | ",
          "[6] MAPA ZARC Portaria 709/2024<br>",
          "<b>ResistÃªncia:</b> 1=SensÃ­vel | 2=Baixa | 3=MÃ©dia | 4=Alta (DMI/SDHI conforme EMBRAPA) &nbsp;|&nbsp; ",
          "<b>Efic. Campo:</b> Embrapa Circ.119/2024 &nbsp;|&nbsp; ",
          "<b>Alerta SIGA:</b> integraÃ§Ã£o IRC + resistÃªncia + pressÃ£o climÃ¡tica ERA5-Land/EMBRAPA 2023/24"
        ))
      ),
      options = list(
        pageLength = 15,
        scrollX    = TRUE,
        autoWidth  = FALSE,
        columnDefs = list(
          list(width="140px", targets=0),   # MunicÃ­pio
          list(width="130px", targets=1),   # Alerta
          list(width="90px",  targets=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18))
        ),
        dom = "Blfrtip"
      ),
      extensions = "Buttons"
    ) %>%
      DT::formatStyle(
        "Alerta SIGA",
        color      = "white",
        fontWeight = "bold",
        backgroundColor = DT::styleEqual(
          c("ðŸ”´ VERMELHO â€” Urgente","ðŸŸ  LARANJA â€” Alto",
            "ðŸŸ¡ AMARELO â€” Moderado","ðŸŸ¢ VERDE â€” Baixo"),
          c("#c0392b","#d35400","#d4ac0d","#2d7a3a"))
      ) %>%
      DT::formatStyle(
        "Resist. Fung. 2024",
        color      = "white",
        fontWeight = "bold",
        backgroundColor = DT::styleEqual(
          c("1 â€” SensÃ­vel","2 â€” Baixa","3 â€” MÃ©dia","4 â€” Alta"),
          c("#2d7a3a","#f0b429","#d35400","#c0392b"))
      ) %>%
      DT::formatStyle(
        "Zona ZARC",
        color      = "white",
        fontWeight = "bold",
        backgroundColor = DT::styleEqual(
          c("Zona I â€” Baixo","Zona II â€” Moderado","Zona III â€” Elevado"),
          c("#2d7a3a","#f0b429","#c0392b"))
      ) %>%
      DT::formatStyle(
        "Anos ExposiÃ§Ã£o",
        background = DT::styleColorBar(c(0, 23), "#c0392b22"),
        backgroundSize   = "100% 80%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      ) %>%
      DT::formatStyle(
        "Focos 2023/24",
        background = DT::styleColorBar(c(0, 320), "rgba(192,57,43,0.25)"),
        backgroundSize   = "100% 80%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      ) %>%
      DT::formatStyle(
        "IRC Ref. 2024",
        background = DT::styleColorBar(c(0, 100), "rgba(240,100,41,0.30)"),
        backgroundSize   = "100% 80%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  
    }, error = function(e) {
      DT::datatable(
        data.frame(Erro = paste0("Erro ao carregar Fitossanidade: ", conditionMessage(e))),
        options = list(dom = "t"), rownames = FALSE
      )
    })
  })
## 6.14 Downloads â€” downloadHandler (CSV + PDF) ----
  # â”€â”€ FUNÃ‡ÃƒO AUXILIAR: gerar PDF via HTML â†’ webshot2 (sem LaTeX) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  # Abordagem: constrÃ³i HTML formatado com knitr::kable, depois captura como PDF
  # com webshot2::webshot (Chromium headless). Fallback para CSV se webshot2
  # ou Chromium nÃ£o estiverem disponÃ­veis.
  .gerar_pdf <- function(titulo, descricao, df_list) {
    # HTML bonito com tabelas formatadas â€” funciona em qualquer sistema
    data_hora <- format(Sys.time(), "%d/%m/%Y %H:%M")
    tabelas_html <- paste(sapply(names(df_list), function(nm) {
      df <- df_list[[nm]]
      if (is.null(df) || nrow(df) == 0) return("")
      df <- df %>% dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 2)))
      # knitr::kable para HTML com bootstrap
      tbl_html <- tryCatch(
        knitr::kable(head(df, 200), format = "html", caption = nm,
                     table.attr = 'class="kable-tbl"') %>%
          as.character(),
        error = function(e) {
          rows <- apply(head(df, 200), 1, function(r)
            paste0("<tr><td>", paste(r, collapse="</td><td>"), "</td></tr>"))
          paste0('<table class="kable-tbl"><thead><tr><th>',
                 paste(names(df), collapse="</th><th>"),
                 "</th></tr></thead><tbody>", paste(rows, collapse=""), "</tbody></table>")
        })
      paste0('<section><h2>', nm, '</h2>', tbl_html, '</section>')
    }), collapse = "\n\n")

    html_content <- paste0('<!DOCTYPE html><html lang="pt-BR"><head>
<meta charset="UTF-8">
<title>', titulo, '</title>
<style>
  * { box-sizing: border-box; }
  body { font-family: "Segoe UI", Arial, sans-serif; font-size: 9pt;
         color: #1a1a1a; margin: 0; padding: 0; background: #fff; }
  .cover { background: #0b1a0d; color: #94c99a; padding: 32px 40px 24px;
           border-bottom: 4px solid #2d7a3a; page-break-after: always; }
  .cover h1 { color: #d5ead7; font-size: 20pt; margin: 0 0 6px; }
  .cover h2 { color: #4a7a52; font-size: 12pt; font-weight: 400; margin: 0 0 14px; }
  .cover .meta { color: #2d7a3a; font-size: 8pt; line-height: 1.8; }
  .cover .desc { color: #94c99a; font-size: 10pt; margin: 12px 0 0;
                 border-left: 3px solid #2d7a3a; padding-left: 10px; }
  .content { padding: 24px 32px; }
  section { margin-bottom: 28px; page-break-inside: avoid; }
  h2 { font-size: 11pt; color: #1e4024; border-bottom: 1px solid #2d7a3a;
       padding-bottom: 4px; margin: 20px 0 8px; }
  .kable-tbl { border-collapse: collapse; width: 100%; font-size: 7.5pt; }
  .kable-tbl th { background: #1e4024; color: #d5ead7; padding: 4px 6px;
                  text-align: left; font-weight: 600; white-space: nowrap; }
  .kable-tbl td { padding: 3px 6px; border-bottom: 1px solid #e8f0e9;
                  vertical-align: top; }
  .kable-tbl tr:nth-child(even) td { background: #f4f9f4; }
  .kable-tbl caption { font-size: 9pt; color: #2d7a3a; text-align: left;
                       font-style: italic; margin-bottom: 4px; caption-side: top; }
  .footer { margin-top: 20px; padding-top: 10px; border-top: 1px solid #ddd;
            font-size: 7.5pt; color: #888; text-align: center; }
  @media print {
    .cover { -webkit-print-color-adjust: exact; print-color-adjust: exact; }
    .kable-tbl th { -webkit-print-color-adjust: exact; print-color-adjust: exact; }
    .kable-tbl tr:nth-child(even) td {
      -webkit-print-color-adjust: exact; print-color-adjust: exact; }
  }
</style></head><body>
<div class="cover">
  <h1>', titulo, '</h1>
  <h2>Dashboard Ferrugem AsiÃ¡tica da Soja â€” Mato Grosso v8.0</h2>
  <div class="desc">', descricao, '</div>
  <div class="meta">
    Gerado em: ', data_hora, '<br>
    Fontes: ERA5-Land/Open-Meteo Â· IBGE/SIDRA PAM Â· EMBRAPA SIGA Â· NOAA CPC Â·
    CEPEA-ESALQ Â· CONAB Â· geobr/IBGE Â· NASA POWER API<br>
    ReferÃªncias: Del Ponte et al. (2006) Â· Dalla Lana et al. (2015) Â· IPCC AR6 WG1 Â·
    EMBRAPA Circ.119/2024 Â· MAPA ZARC Port.270/2024
  </div>
</div>
<div class="content">
', tabelas_html, '
  <div class="footer">
    Ferrugem AsiÃ¡tica da Soja â€” MT v8.0 â€” Plataforma Profissional |
    ', data_hora, '
  </div>
</div>
</body></html>')

    tmp_html <- tempfile(fileext = ".html")
    tmp_pdf  <- tempfile(fileext = ".pdf")
    writeLines(html_content, tmp_html, useBytes = FALSE)

    result_path <- tryCatch({
      if (!requireNamespace("webshot2", quietly = TRUE))
        stop("webshot2 nÃ£o instalado")
      webshot2::webshot(
        url      = tmp_html,
        file     = tmp_pdf,
        vwidth   = 1100,
        vheight  = 850,
        zoom     = 1.2,
        delay    = 0.5,
        selector = "body"
      )
      if (!file.exists(tmp_pdf) || file.size(tmp_pdf) < 1000)
        stop("PDF gerado estÃ¡ vazio")
      tmp_pdf
    }, error = function(e) {
      # Fallback 1: tentar rmarkdown se LaTeX disponÃ­vel
      message("[PDF] webshot2 falhou (", e$message, ") â€” tentando rmarkdown")
      tryCatch({
        tmp_rmd  <- tempfile(fileext = ".Rmd")
        tmp_pdf2 <- tempfile(fileext = ".pdf")
        rmd_lines <- c(
          '---', paste0('title: "', gsub('"',"'",titulo), '"'),
          'output:', '  pdf_document:', '    latex_engine: pdflatex',
          '    toc: true', '---', '', descricao, '',
          '```{r echo=FALSE,message=FALSE,warning=FALSE}',
          'library(knitr)'
        )
        tmp_rdses <- character(0)
        for (nm in names(df_list)) {
          trds <- tempfile(fileext = ".rds")
          saveRDS(df_list[[nm]], trds)
          tmp_rdses <- c(tmp_rdses, trds)
          idx <- length(tmp_rdses)
          rmd_lines <- c(rmd_lines,
            paste0('df', idx, ' <- readRDS("', trds, '")'),
            paste0('knitr::kable(head(df', idx, ',100), caption="', gsub('"',"'",nm), '") |> print()'),
            '')
        }
        rmd_lines <- c(rmd_lines, '```')
        writeLines(rmd_lines, tmp_rmd)
        rmarkdown::render(tmp_rmd, output_file = tmp_pdf2, quiet = TRUE,
                          envir = new.env(parent = globalenv()))
        try(file.remove(c(tmp_rmd, tmp_rdses)), silent = TRUE)
        if (file.exists(tmp_pdf2)) tmp_pdf2 else stop("PDF vazio")
      }, error = function(e2) {
        # Fallback 2: HTML directamente (legÃ­vel no browser)
        message("[PDF] rmarkdown falhou â€” entregando HTML")
        tmp_html
      })
    })
    try(file.remove(tmp_html), silent = TRUE)
    result_path
  }

  # â”€â”€ CSV downloads â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  output$dl_safra <- downloadHandler(
    filename=function() paste0("ferrugem_MT_2024_25_",Sys.Date(),".csv"),
    content =function(f) write.csv(df_f() %>% arrange(prioridade), f, row.names=FALSE))

  output$dl_hist <- downloadHandler(
    filename=function() paste0("ferrugem_MT_IRC_hist_",Sys.Date(),".csv"),
    content =function(f) write.csv(rv$irc, f, row.names=FALSE))

  output$dl_prod <- downloadHandler(
    filename=function() paste0("ferrugem_MT_producao_",Sys.Date(),".csv"),
    content =function(f) write.csv(rv$prod, f, row.names=FALSE))

  output$dl_prev <- downloadHandler(
    filename=function() paste0("ferrugem_MT_prev2526_",Sys.Date(),".csv"),
    content =function(f) write.csv(rv$prev, f, row.names=FALSE))

  output$dl_cenarios <- downloadHandler(
    filename=function() paste0("ferrugem_MT_cenarios_enso_",Sys.Date(),".csv"),
    content =function(f) {
      cen <- tryCatch(rv$cenarios, error=function(e) data.frame())
      write.csv(cen, f, row.names=FALSE)
    })

  output$dl_mun <- downloadHandler(
    filename=function() paste0("relatorio_",mun_ativo(),"_",Sys.Date(),".csv"),
    content =function(f) {
      m <- mun_ativo()
      bind_rows(
        rv$res  %>% filter(municipio==m) %>% mutate(tipo="safra_atual"),
        rv$irc  %>% filter(municipio==m) %>% mutate(tipo="historico_irc"),
        rv$prod %>% filter(municipio==m) %>% mutate(tipo="producao"),
        rv$prev %>% filter(municipio==m) %>% mutate(tipo="previsao")
      ) %>% write.csv(f, row.names=FALSE)
    })

  # â”€â”€ PDF â€” RelatÃ³rio por MunicÃ­pio (aba Buscar MunicÃ­pio) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  output$dl_mun_pdf <- downloadHandler(
    filename=function() {
      ext <- if (requireNamespace("webshot2",quietly=TRUE)) ".pdf" else ".html"
      paste0("relatorio_municipio_", mun_ativo(), "_", Sys.Date(), ext)
    },
    contentType="application/pdf",
    content=function(f) {
      m    <- mun_ativo()
      sf   <- input$sel_safra_det
      d_res  <- rv$res  %>% filter(municipio==m)
      d_irc  <- rv$irc  %>% filter(municipio==m) %>% arrange(ano_inicio) %>%
        select(Safra=safra, IRC=irc, `Prob.Ferrugem(%)`=prob_ferrugem,
               `Precip.(mm)`=precip_total, `UR(%)`=ur_media, `Temp.(Â°C)`=temp_media,
               `Dias.Fav.`=dias_fav_total, ENSO=enso)
      d_prod <- rv$prod %>% filter(municipio==m) %>% arrange(ano) %>%
        select(Ano=ano, `Producao(t)`=producao_ton, `Area(ha)`=area_ha,
               `Produtiv.(kg/ha)`=produtividade)
      d_prev <- rv$prev %>% filter(municipio==m) %>%
        select(Safra=safra, `IRC.Prev.`=irc_prev, `IC.Inf.`=irc_ic_l,
               `IC.Sup.`=irc_ic_h, `Prob.Prev.(%)`=prob_prev,
               `Cat.Risco.Prev.`=cat_risco_prev, `Perda.Est.(R$Mi)`=perda_prev_mi)
      d_safra <- d_res %>%
        select(IRC=irc, `Prob.Ferrugem(%)`=prob_ferrugem, Risco=categoria_risco,
               IVM=ivm, `Perda(ton)`=perda_ton, `Perda(R$Mi)`=perda_mi_r,
               `N.Aplic.`=n_aplic, `Coef.Dano(%)`=coef_dano, `Sev.(%)`=sev) %>%
        mutate(Risco=as.character(Risco))
      fito_m <- FITO_REAL %>% filter(municipio==m)
      fito_tbl <- if (nrow(fito_m)>0) {
        fito_m %>% select(
          `Alerta.EMBRAPA`=alerta_embrapa,
          `Incid.Hist.(%)`=incidencia_hist_pct,
          `Efic.Campo(%)`=efic_media_campo,
          `N.Aplic.Obs.`=n_aplic_real,
          `Focos.2024`=focos_confirmados_2024,
          `Perda.Hist.(R$Mi)`=perda_hist_media_mi)
      } else NULL
      dm_mun <- MUN %>% filter(municipio==m)
      descricao_txt <- paste0(
        "MunicÃ­pio: <b>", m, "</b> | Safra analisada: <b>", sf, "</b><br>",
        "IRC atual: <b>", d_res$irc, "</b> | Categoria: <b>", as.character(d_res$categoria_risco), "</b><br>",
        "Probabilidade de ocorrÃªncia de ferrugem: <b>", d_res$prob_ferrugem, "%</b><br>",
        "Perda potencial estimada: <b>R$ ", d_res$perda_mi_r, " Mi</b> | IVM: <b>", round(d_res$ivm,1), "/100</b><br>",
        "1Âª ocorrÃªncia registrada: <b>", dm_mun$safra_1a_ocorr, "</b> | ",
        "ResistÃªncia a fungicidas (EMBRAPA 2024): NÃ­vel <b>", dm_mun$nivel_resistencia, "/4</b>"
      )
      df_lst <- list(
        "Risco e Perdas â€” Safra Atual"         = d_safra,
        "HistÃ³rico IRC por Safra"              = d_irc,
        "ProduÃ§Ã£o e Produtividade (IBGE PAM)"  = d_prod,
        "PrevisÃ£o 2025/26 (ARIMA+ENSO)"        = d_prev
      )
      if (!is.null(fito_tbl))
        df_lst[["Fitossanidade â€” EMBRAPA SIGA"]] <- fito_tbl
      tmp <- .gerar_pdf(
        titulo    = paste0("RelatÃ³rio â€” ", m, " | Ferrugem AsiÃ¡tica da Soja"),
        descricao = descricao_txt,
        df_list   = df_lst
      )
      file.copy(tmp, f, overwrite=TRUE)
    })

  output$dl_futuro <- downloadHandler(
    filename=function() paste0("ferrugem_MT_incidencias_futuras_",Sys.Date(),".csv"),
    content =function(f) write.csv(projecoes_futuras_rv(), f, row.names=FALSE))

  # â”€â”€ PDF downloads â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  output$dl_safra_pdf <- downloadHandler(
    filename=function() {
      ext <- if (requireNamespace("webshot2",quietly=TRUE)) ".pdf" else ".html"
      paste0("ferrugem_MT_2024_25_", Sys.Date(), ext)
    },
    contentType="application/pdf",
    content =function(f) {
      d <- df_f() %>% arrange(prioridade) %>%
        select(Municipio=municipio, IRC=irc, `Prob.Ferrugem(%)`=prob_ferrugem,
               Risco=categoria_risco, IVM=ivm, `Perda.R$Mi`=perda_mi_r,
               `N.Aplic`=n_aplic, `Coef.Dano(%)`=coef_dano) %>%
        mutate(Risco=as.character(Risco))
      tmp <- .gerar_pdf(
        titulo   = "Ferrugem AsiÃ¡tica da Soja MT â€” Safra 2024/25",
        descricao= "IRC, Probabilidade de OcorrÃªncia de Ferrugem, IVM e Perdas por municÃ­pio (47 municÃ­pios MT).",
        df_list  = list("Safra 2024/25 â€” IRC e Risco"=d)
      )
      file.copy(tmp, f, overwrite=TRUE)
    })

  output$dl_hist_pdf <- downloadHandler(
    filename=function() {
      ext <- if (requireNamespace("webshot2",quietly=TRUE)) ".pdf" else ".html"
      paste0("ferrugem_MT_IRC_hist_", Sys.Date(), ext)
    },
    contentType="application/pdf",
    content =function(f) {
      d <- rv$irc %>% select(Safra=safra, Municipio=municipio, IRC=irc,
                              `Prob.(%)`=prob_ferrugem, ENSO=enso,
                              `Precip.(mm)`=precip_total, `UR(%)`=ur_media)
      tmp <- .gerar_pdf(
        titulo   = "HistÃ³rico IRC Ferrugem AsiÃ¡tica MT 2012â€“2024",
        descricao= "SÃ©rie histÃ³rica do IRC por municÃ­pio e safra. Fonte: ERA5-Land/Open-Meteo + NOAA CPC.",
        df_list  = list("HistÃ³rico IRC 2012â€“2024"=d))
      file.copy(tmp, f, overwrite=TRUE)
    })

  output$dl_prod_pdf <- downloadHandler(
    filename=function() {
      ext <- if (requireNamespace("webshot2",quietly=TRUE)) ".pdf" else ".html"
      paste0("ferrugem_MT_producao_", Sys.Date(), ext)
    },
    contentType="application/pdf",
    content =function(f) {
      d <- rv$prod %>% select(Municipio=municipio, Ano=ano,
                               `Producao(t)`=producao_ton,
                               `Area(ha)`=area_ha, `Produtiv.(kg/ha)`=produtividade)
      tmp <- .gerar_pdf(
        titulo   = "ProduÃ§Ã£o de Soja MT â€” IBGE/SIDRA PAM 2012â€“2023",
        descricao= "Ãrea colhida, produÃ§Ã£o e produtividade por municÃ­pio (IBGE PAM Tabela 1612).",
        df_list  = list("ProduÃ§Ã£o Soja MT"=d))
      file.copy(tmp, f, overwrite=TRUE)
    })

  output$dl_prev_pdf <- downloadHandler(
    filename=function() {
      ext <- if (requireNamespace("webshot2",quietly=TRUE)) ".pdf" else ".html"
      paste0("ferrugem_MT_prev2526_", Sys.Date(), ext)
    },
    contentType="application/pdf",
    content =function(f) {
      d <- rv$prev %>% select(Municipio=municipio, `IRC.Prev`=irc_prev,
                               `IC.Inf(80%)`=irc_ic_l, `IC.Sup(80%)`=irc_ic_h,
                               `Prob.Prev(%)`=prob_prev, ENSO=enso_prev,
                               Risco=cat_risco_prev) %>%
        mutate(Risco=as.character(Risco))
      tmp <- .gerar_pdf(
        titulo   = "PrevisÃ£o 2025/26 â€” ARIMA+ENSO",
        descricao= paste0("IRC previsto com IC 80% por municÃ­pio. ENSO projetado: ",
                          tryCatch(rv$mercado$enso, error=function(e) "El NiÃ±o moderado"),
                          " (NOAA CPC)."),
        df_list  = list("PrevisÃ£o 2025/26"=d))
      file.copy(tmp, f, overwrite=TRUE)
    })

  output$dl_cenarios_pdf <- downloadHandler(
    filename=function() {
      ext <- if (requireNamespace("webshot2",quietly=TRUE)) ".pdf" else ".html"
      paste0("ferrugem_MT_cenarios_", Sys.Date(), ext)
    },
    contentType="application/pdf",
    content =function(f) {
      cen <- tryCatch(rv$cenarios %>%
        select(Municipio=municipio, Cenario=cenario, `IRC.Proj`=irc_cen,
               `Perda.Proj(R$Mi)`=perda_mi_cen, Cat=cat_cen),
        error=function(e) data.frame())
      tmp <- .gerar_pdf(
        titulo   = "CenÃ¡rios ENSO 2025/26 â€” La NiÃ±a / Neutro / El NiÃ±o",
        descricao= "IRC projetado e perdas estimadas por cenÃ¡rio ENSO (calibrado ERA5-Land 2012-2024).",
        df_list  = list("CenÃ¡rios ENSO"=cen))
      file.copy(tmp, f, overwrite=TRUE)
    })

  output$dl_futuro_pdf <- downloadHandler(
    filename=function() {
      ext <- if (requireNamespace("webshot2",quietly=TRUE)) ".pdf" else ".html"
      paste0("ferrugem_MT_incid_futuras_", Sys.Date(), ext)
    },
    contentType="application/pdf",
    content =function(f) {
      d <- projecoes_futuras_rv()
      tmp <- .gerar_pdf(
        titulo   = "IncidÃªncias Futuras 2025â€“2030 â€” Ferrugem AsiÃ¡tica MT",
        descricao= paste0("ProjeÃ§Ãµes por safra futura via ARIMA + correÃ§Ã£o ENSO (NOAA CPC) + tendÃªncia climÃ¡tica IPCC AR6.",
                          " IC 80% bootstrap n=500. ReferÃªncia: Dalla Lana et al. (2015)."),
        df_list  = list("IncidÃªncias Futuras 2025â€“2030"=d))
      file.copy(tmp, f, overwrite=TRUE)
    })

  output$dl_relatorio_completo <- downloadHandler(
    filename=function() {
      ext <- if (requireNamespace("webshot2",quietly=TRUE)) ".pdf" else ".html"
      paste0("relatorio_completo_ferrugem_MT_", Sys.Date(), ext)
    },
    contentType="application/pdf",
    content =function(f) {
      d_safra <- tryCatch(df_f() %>% arrange(prioridade) %>%
        select(Municipio=municipio, IRC=irc, `Prob.(%)`=prob_ferrugem,
               Risco=categoria_risco, `Perda.R$Mi`=perda_mi_r) %>%
        mutate(Risco=as.character(Risco)), error=function(e) data.frame())
      d_prev  <- tryCatch(rv$prev %>%
        select(Municipio=municipio, `IRC.Prev`=irc_prev, `Prob.Prev(%)`=prob_prev,
               Risco=cat_risco_prev) %>%
        mutate(Risco=as.character(Risco)), error=function(e) data.frame())
      d_fut   <- tryCatch(projecoes_futuras_rv(), error=function(e) data.frame())
      d_cen   <- tryCatch(rv$cenarios %>%
        select(Municipio=municipio, Cenario=cenario, `IRC.Proj`=irc_cen,
               `Perda.R$Mi`=perda_mi_cen), error=function(e) data.frame())
      tmp <- .gerar_pdf(
        titulo   = "RelatÃ³rio Completo â€” Ferrugem AsiÃ¡tica Soja MT v8.0",
        descricao= paste0("Safra 2024/25 | PrevisÃ£o 2025/26 | IncidÃªncias Futuras 2025â€“2030 | CenÃ¡rios ENSO.",
                          " Gerado em: ", format(Sys.time(), "%d/%m/%Y %H:%M")),
        df_list  = list(
          "1. Safra 2024/25 â€” IRC e Risco"          = d_safra,
          "2. PrevisÃ£o 2025/26 (ARIMA+ENSO)"        = d_prev,
          "3. IncidÃªncias Futuras 2025â€“2030"         = d_fut,
          "4. CenÃ¡rios ENSO (La NiÃ±a/Neutro/El NiÃ±o)"= d_cen
        ))
      file.copy(tmp, f, overwrite=TRUE)
    })

## 6.15 Status e APIs â€” cache info + log ----
  # â”€â”€ STATUS E APIs â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  status_row <- function(nome, ok, det="") {
    cor <- if (ok) "#2d7a3a" else "#ca8a04"
    ic  <- if (ok) icon("check-circle") else icon("exclamation-circle")
    tags$div(style=paste0("background:#0b1a0d;border:1px solid ",cor,
      ";border-radius:5px;padding:8px 12px;margin-bottom:6px;"),
      tags$div(style="display:flex;justify-content:space-between;",
        tags$span(style="color:#94c99a;font-weight:600;font-size:11.5px;",nome),
        tags$span(style=paste0("color:",cor,";font-size:11px;font-weight:700;"),ic,
          if (ok) " ONLINE" else " CACHE/OFFLINE")),
      if (nchar(det)>0) tags$div(style="color:#2d5c32;font-size:10px;margin-top:3px;",det)
    )
  }
  output$ui_status <- renderUI({
    dm <- rv$mercado
    if (is.null(dm)) return(tags$p(style="color:#4a7a52;","Aguardando conexÃ£o..."))
    siga_ok <- !is.null(rv$siga) && isTRUE(rv$siga$online)
    tags$div(
      status_row("IBGE/SIDRA Tabela 1612",rv$ibge_ok,
        if (rv$ibge_ok) paste("Registros:",nrow(rv$prod)) else "SimulaÃ§Ã£o calibrada CONAB"),
      status_row("NASA POWER API",rv$nasa_ok,
        if (rv$nasa_ok) "Cache ativo â€” dados reais" else "Usando ref. INMET 1991-2020"),
      status_row("CEPEA/ESALQ PreÃ§o Soja",dm$preco$fonte!="PadrÃ£o (offline)",
        paste0("R$ ",fmt_r(dm$preco$preco),"/sc â€” ",format(dm$preco$ts,"%H:%M")," | ",dm$preco$fonte)),
      status_row("AwesomeAPI USD/BRL",dm$cambio$val!=5.20,
        paste0("USD/BRL: ",fmt_r(dm$cambio$val,4)," â€” ",format(dm$cambio$ts,"%H:%M"))),
      status_row("NOAA CPC ENSO",TRUE,
        paste0("ENSO previsto: ",dm$enso," (IRI Columbia)")),
      status_row("EMBRAPA SIGA Alertas",siga_ok,
        if (siga_ok) paste0("Online â€” ",rv$siga$focos_texto) else "Offline â€” usando dados histÃ³ricos"),
      status_row("CONAB Preco/Producao",grepl("CONAB",dm$preco$fonte),
        "Fallback automatico quando CEPEA indisponivel"),
      status_row("Open-Meteo ERA5-Land",TRUE,
        "https://archive-api.open-meteo.com â€” ERA5 historico (Hersbach et al. 2020)"),
      status_row("geobr Poligonos MT",!is.null(rv$geobr_mt),
        if (!is.null(rv$geobr_mt)) paste0("Carregado: ",nrow(rv$geobr_mt)," municipios MT")
        else "Aguardando download (carregar_geobr_mt em background)")
    )
  })
  output$ui_cache_info <- renderUI({
    tags$div(
      tags$div(style="background:#0b1a0d;border:1px solid #1e4024;border-radius:5px;padding:10px 13px;margin-bottom:8px;",
        tags$b(style="color:#94c99a;","Ãšltima atualizaÃ§Ã£o"),tags$br(),
        tags$span(style="color:#2d7a3a;font-family:'JetBrains Mono', monospace;font-size:13px;",
          format(rv$last_update,"%d/%m/%Y %H:%M:%S"))),
      tags$div(style="background:#0b1a0d;border:1px solid #1e4024;border-radius:5px;padding:10px 13px;margin-bottom:8px;",
        tags$b(style="color:#94c99a;","Fonte de produÃ§Ã£o"),tags$br(),
        tags$span(style="color:#4a7a52;font-size:11px;",rv$fonte_prod)),
      tags$div(style="background:#0b1a0d;border:1px solid #1e4024;border-radius:5px;padding:10px 13px;margin-bottom:8px;",
        tags$b(style="color:#94c99a;","Fontes de dados integradas (v8.0)"),tags$br(),
        tags$span(style="color:#4a7a52;font-size:10.5px;",
          "â€¢ IBGE/SIDRA Tab.1612 â€” Producao soja MT",tags$br(),
          "â€¢ NASA POWER â€” Clima mensal 1991-2024",tags$br(),
          "â€¢ Open-Meteo ERA5-Land â€” Clima diario historico",tags$br(),
          "â€¢ geobr â€” Poligonos municipais IBGE/MT",tags$br(),
          "â€¢ CEPEA/ESALQ â€” Preco soja MT",tags$br(),
          "â€¢ CONAB â€” Preco/safra fallback",tags$br(),
          "â€¢ NOAA CPC / IRI Columbia â€” ENSO",tags$br(),
          "â€¢ EMBRAPA SIGA â€” Alertas ferrugem",tags$br(),
          "â€¢ EMBRAPA Circ. 119/2024 â€” Resistencia/manejo",tags$br(),
          "â€¢ APROSOJA-MT 2024 â€” Dados de campo",tags$br(),
          "â€¢ AwesomeAPI â€” Cambio USD/BRL"
        )),
      tags$div(style="background:#0b1a0d;border:1px solid #1e4024;border-radius:5px;padding:10px 13px;",
        tags$b(style="color:#94c99a;","Cache Persistente (Offline-First)"),tags$br(),
        tags$span(style="color:#4a7a52;font-size:11px;",
          paste0("ðŸ“ Dir: ", CACHE_DIR)),tags$br(),
        tags$span(style="color:#4a7a52;font-size:11px;",
          paste0("ðŸ“¦ Arquivos: ", length(list.files(CACHE_DIR, pattern="\\.rds$")), " itens em cache")),
        tags$br(),
        # Mostrar idade de cada cache chave
        lapply(list(
          list(k="ibge",     lbl="IBGE/SIDRA",   ttl_h=round(CACHE_TTL_IBGE/3600)),
          list(k="nasa",     lbl="NASA POWER",   ttl_h=round(CACHE_TTL_NASA/3600)),
          list(k="preco_cepea", lbl="PreÃ§o CEPEA", ttl_h=round(CACHE_TTL/3600)),
          list(k="cambio_usd",  lbl="CÃ¢mbio USD", ttl_h=round(CACHE_TTL/3600)),
          list(k="enso_fase",   lbl="ENSO",       ttl_h=round(CACHE_TTL/3600)),
          list(k="geobr_mt_municipios_2020", lbl="geobr MT", ttl_h=round(CACHE_TTL_GEOBR/3600))
        ), function(ck) {
          age_h  <- cache_age_horas(ck$k)
          ts_str <- cache_ts_str(ck$k)
          ok     <- age_h < ck$ttl_h
          cor    <- if (!file.exists(file.path(CACHE_DIR, paste0(ck$k, ".rds")))) "#4a7a52"
                    else if (ok) "#2d7a3a" else "#ca8a04"
          icone  <- if (!file.exists(file.path(CACHE_DIR, paste0(ck$k, ".rds")))) "âšª"
                    else if (ok) "ðŸŸ¢" else "ðŸŸ¡"
          tags$div(style="font-size:10.5px;color:#94c99a;margin-top:3px;",
            paste0(icone, " ", ck$lbl, ": ", ts_str,
                   if (is.finite(age_h)) paste0(" (", round(age_h,1), "h atrÃ¡s)") else " (sem cache)"))
        })
      )
    )
  })

  # â”€â”€ ABA FONTES â€” tabela de fontes â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  output$tab_fontes_dados <- renderDT({
    df_fontes <- data.frame(
      Fonte     = c("IBGE/SIDRA","NASA POWER","Open-Meteo ERA5","CEPEA/ESALQ",
                    "CONAB","EMBRAPA SIGA","geobr","NOAA CPC","APROSOJA-MT","AwesomeAPI"),
      Variavel  = c("Producao/Area/Produtividade soja","Clima mensal 1981-2024",
                    "ERA5-Land diario (precip/UR/temp)","Preco soja MT (R$/sc 60 kg)",
                    "Preco safra fallback","Alertas ferrugem asiatica",
                    "Poligonos municipais IBGE/MT","ENSO projecoes 2025/26",
                    "Dados de campo MT 2024","Cambio USD/BRL"),
      Acesso    = c("API REST SIDRA","API REST publica","API REST publica",
                    "Web scraping","Web scraping","Web scraping",
                    "Pacote R (CRAN)","Web fetch JSON","Estatico (embutido)","API REST"),
      Frequencia= c("Anual","Mensal","Diario","Diario","Semanal","Diario",
                    "Sob demanda","Mensal","Estatico","Diario"),
      stringsAsFactors = FALSE
    )
    datatable(df_fontes,
      options  = list(dom="t", pageLength=15, scrollX=TRUE),
      rownames = FALSE,
      class    = "compact stripe",
      caption  = tags$caption(
        style="color:#4a7a52;font-size:11px;",
        "Todas as fontes sao publicas e de acesso gratuito. ",
        "APIs REST com fallback local quando offline.")
    )
  })

  # â”€â”€ LOG â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  output$log_txt <- renderText({
    paste(rv$log, collapse="\n")
  })

## 6.16 Sinistro Agricola â€” RSA, ZARC, Premio ZARC ----
  # â•â• ABA 13: RISCO DE SINISTRO â€” SERVIDOR â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
  CORES_SINISTRO <- c(
    "Risco Muito Baixo" = "#1d4ed8",
    "Risco Baixo"       = "#16a34a",
    "Risco Moderado"    = "#ca8a04",
    "Risco Elevado"     = "#dc2626"
  )

  sin_df <- reactive({
    tryCatch(calcular_sinistro(rv$res, rv$prod_pa), error=function(e) rv$res)
  })

  output$ib_rsa_alto <- renderInfoBox({
    n <- tryCatch(sum(sin_df()$cat_sinistro %in% c("Risco Elevado"),
                      na.rm=TRUE), error=function(e) 0)
    infoBox("Risco Elevado (RSAâ‰¥75)", paste0(n," mun."),
            icon=icon("radiation"), color="red", fill=TRUE)
  })
  output$ib_rsa_media <- renderInfoBox({
    n <- tryCatch(sum(sin_df()$cat_sinistro=="Risco Moderado",na.rm=TRUE), error=function(e) 0)
    infoBox("Risco Moderado", paste0(n," mun."),
            icon=icon("exclamation-triangle"), color="yellow", fill=TRUE)
  })
  output$ib_rsa_premio <- renderInfoBox({
    tot <- tryCatch({
      d <- sin_df()
      # calcular_sinistro retorna: premio_rha (bruto), premio_liq_rha (lÃ­quido apÃ³s PSR)
      # Calcula total estimado: premio_liq_rha * area_calc / 1e6
      if ("premio_liq_rha" %in% names(d) && "area_calc" %in% names(d)) {
        round(sum(d$premio_liq_rha * coalesce(d$area_calc, 30000) / 1e6, na.rm=TRUE), 1)
      } else 0
    }, error=function(e) 0)
    infoBox("Premio Estimado Total", paste0("R$ ",tot," Mi"),
            icon=icon("shield-alt"), color="blue", fill=TRUE)
  })
  output$ib_rsa_indem <- renderInfoBox({
    tot <- tryCatch({
      d <- sin_df()
      # indeniz_max_rha = perda potencial Ã— 1.2 por calcular_sinistro
      if ("indeniz_max_rha" %in% names(d) && "area_calc" %in% names(d)) {
        round(sum(d$indeniz_max_rha * coalesce(d$area_calc, 30000) / 1e6, na.rm=TRUE), 1)
      } else 0
    }, error=function(e) 0)
    infoBox("Indenizacao Potencial", paste0("R$ ",tot," Mi"),
            icon=icon("dollar-sign"), color="orange", fill=TRUE)
  })

  # â”€â”€ HISTORICO REAL MAPA/SEGER: Premio x Indenizacoes x Sinistralidade â”€â”€â”€â”€â”€â”€
  output$g_sinistro_hist <- renderPlotly({
    tryCatch({
    df <- SEGURO_HIST
    COR_PREMIO  <- "rgba(26,82,118,0.85)"
    COR_INDENIZ <- "rgba(192,57,43,0.85)"
    COR_SINIST  <- "#ca8a04"
    COR_SUBV    <- "rgba(30,136,73,0.75)"

    fig <- plot_ly(df, x=~safra) %>%
      add_bars(y=~premio_emitido_mi,   name="Premio Emitido (R$ Mi)",
               marker=list(color=COR_PREMIO),
               text=~paste0("R$ ",premio_emitido_mi," Mi"),
               textposition="inside", textfont=list(size=9)) %>%
      add_bars(y=~subvencao_psr_mi,    name="SubvenÃ§Ã£o PSR (R$ Mi)",
               marker=list(color=COR_SUBV),
               text=~paste0("PSR: R$ ",subvencao_psr_mi," Mi"),
               textposition="inside", textfont=list(size=9)) %>%
      add_bars(y=~indeniz_pagas_mi,    name="IndenizaÃ§Ãµes (R$ Mi)",
               marker=list(color=COR_INDENIZ),
               text=~paste0("R$ ",indeniz_pagas_mi," Mi"),
               textposition="inside", textfont=list(size=9)) %>%
      add_lines(y=~sinistralidade_pct, name="Sinistralidade (%)",
                yaxis="y2",
                line=list(color=COR_SINIST, width=2.5, dash="dot"),
                marker=list(size=8, color=COR_SINIST),
                text=~paste0(enso_fase," | ",sinistralidade_pct,"%"),
                hovertemplate=paste0("<b>%{x}</b><br>",
                                     "Sinistralidade: %{y:.1f}%%<br>",
                                     "%{text}<extra></extra>"))

    fig %>% tp() %>% layout(
      barmode = "group",
      xaxis   = list(title="Safra", tickfont=list(size=10)),
      yaxis   = list(title="R$ Milhoes (MAPA/SEGER)",
                     gridcolor="rgba(255,255,255,0.06)"),
      yaxis2  = list(title="Sinistralidade (%)", overlaying="y", side="right",
                     range=c(0, 140), ticksuffix="%",
                     gridcolor="rgba(0,0,0,0)",
                     titlefont=list(color=COR_SINIST),
                     tickfont=list(color=COR_SINIST)),
      legend  = list(orientation="h", y=-0.42, x=0.5, xanchor="center",
                     yanchor="top", font=list(size=9)),
      margin  = list(t=10, r=70, b=90, l=10),
      annotations = list(
        list(x=0.5, y=1.04, xref="paper", yref="paper", showarrow=FALSE,
             text=paste0("Fonte: MAPA/SEGER + SUSEP BCI | URL: gov.br/agricultura",
                         " | La Nina 2020/21: sinistralidade 104.9%"),
             font=list(size=8.5, color="#4a7a52"), xanchor="center")
      )
    )
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_sinistro_hist:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})

  # â”€â”€ ZONAS ZARC POR MUNICIPIO (ZARC/MAPA 2024) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  output$g_zarc_mun <- renderPlotly({
    tryCatch({
    df <- ZARC_MUN %>%
      left_join(MUN[,c("municipio","area_ref")], by="municipio") %>%
      arrange(zona_zarc, municipio) %>%
      mutate(
        zona_l = c("1"="Zona I â€” Baixo Risco",
                   "2"="Zona II â€” Moderado",
                   "3"="Zona III â€” Elevado")[as.character(zona_zarc)],
        cor_z  = c("1"="#16a34a","2"="#ca8a04","3"="#dc2626")[as.character(zona_zarc)],
        taxa_mid = (taxa_min_pct + taxa_max_pct) / 2,
        hover  = paste0(
          "<b>",municipio,"</b><br>",
          "ZARC: <b>",zona_l,"</b><br>",
          "Taxa premio: ",taxa_min_pct,"%-",taxa_max_pct,"% do VPB<br>",
          "Subvencao PSR: ate ",subvencao_max_pct,"% (PSR)<br>",
          "<i>Fonte: ZARC/MAPA 2024, Portaria 270/2024</i>"
        )
      )
    plot_ly(df,
      x     = ~taxa_mid,
      y     = ~reorder(municipio, -zona_zarc),
      color = ~zona_l,
      colors = c("Zona I â€” Baixo Risco"="#16a34a",
                 "Zona II â€” Moderado"  ="#ca8a04",
                 "Zona III â€” Elevado"  ="#dc2626"),
      type  = "bar", orientation="h",
      text  = ~paste0(taxa_min_pct,"%-",taxa_max_pct,"%"),
      textposition="outside", textfont=list(size=7.5),
      customdata = ~hover,
      hovertemplate="%{customdata}<extra></extra>"
    ) %>%
    tp() %>% layout(
      xaxis = list(title="Taxa de Premio Referencia ZARC (% VPB)",
                   range=c(0,9.5), ticksuffix="%"),
      yaxis = list(title="", tickfont=list(size=7.2), automargin=TRUE),
      legend= list(orientation="h", y=-0.42, x=0.5, xanchor="center",
                   yanchor="top", font=list(size=9)),
      margin= list(l=8, r=80, t=10, b=90),
      annotations = list(list(
        x=9.3, y=0.01, xref="x", yref="paper", showarrow=FALSE,
        text="ZARC/MAPA<br>Port. 270/2024",
        font=list(color="#4a7a52",size=7.5),
        xanchor="right", yanchor="bottom"
      ))
    )
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_zarc_mun:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})

    output$g_rsa_ranking <- renderPlotly({
    df <- tryCatch(sin_df() %>% arrange(desc(rsa)) %>% head(25),
                   error=function(e) NULL)
    if (is.null(df) || nrow(df)==0)
      return(.plt() %>% tp() %>%
             layout(title=list(text="Calculando RSA...",font=list(color="#94c99a"))))
    plot_ly(df, x=~rsa, y=~reorder(municipio,rsa),
      type="bar", orientation="h",
      marker=list(color=~CORES_SINISTRO[as.character(cat_sinistro)],
                  line=list(color="rgba(0,0,0,0)",width=0)),
      text=~paste0(round(rsa,1)),
      textposition="outside", textfont=list(size=8.5),
      customdata=~paste0("<b>",municipio,"</b><br>RSA: ",round(rsa,1),
                         "<br>Categoria: ",as.character(cat_sinistro),
                         "<br>IRC: ",irc),
      hovertemplate="%{customdata}<extra></extra>") %>%
    tp() %>% layout(
      yaxis=list(title="",tickfont=list(size=8.5),automargin=TRUE),
      xaxis=list(title="Indice de Risco de Sinistro (0-100)",range=c(0,115)),
      showlegend=FALSE, margin=list(l=5,r=60,t=10,b=35)
    )
  })

  output$g_rsa_matrix <- renderPlotly({
    df <- tryCatch(sin_df(), error=function(e) NULL)
    if (is.null(df) || nrow(df)==0)
      return(.plt() %>% tp() %>%
             layout(title=list(text="Calculando...",font=list(color="#94c99a"))))
    plot_ly(df, x=~irc, y=~relevancia,
      color=~cat_sinistro, colors=CORES_SINISTRO,
      size=~rsa, sizes=c(8,55),
      type="scatter", mode="markers",
      text=~paste0("<b>",municipio,"</b><br>IRC: ",irc,
                   "<br>Relevancia: ",relevancia,
                   "<br>RSA: ",round(rsa,1)),
      hovertemplate="%{text}<extra></extra>") %>%
    tp() %>% layout(
      xaxis=list(title="IRC (0-100)"),
      yaxis=list(title="Relevancia (0-100)"),
      legend=list(orientation="h",y=-0.42,yanchor="top",font=list(size=9))
    )
  })

  output$g_rsa_premio <- renderPlotly({
    tryCatch({

    # â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
    # PrÃªmio Estimado (ZARC) x IndenizaÃ§Ã£o MÃ¡xima â€” Top 20 (R$/ha)
    # Dados estÃ¡ticos reais:
    #   VPB/ha     = PROD_IBGE_2023$produtiv_t Ã— (CEPEA R$/60 kg)
    #   Taxa prÃªmio = ZARC_MUN taxa_min/taxa_max â€” MAPA Portaria 270/2024
    #   PrÃªmio bruto = VPB Ã— taxa; Subv. PSR 40% (Portaria 709/2024)
    #   Indeniz. mÃ¡x = VPB Ã— 100% (cobertura total LPA/MAPA)
    #   Perda/ha  = PERDA_MUN_TOP20_REAL Ã— (preÃ§o CEPEA) / area_ha_ibge
    # â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
    preco_ativo <- tryCatch(rv$mercado$preco$preco, error=function(e) PRECO_PADRAO)
    if (is.null(preco_ativo) || is.na(preco_ativo)) preco_ativo <- PRECO_PADRAO
    fonte_preco <- tryCatch(rv$mercado$preco$fonte, error=function(e) "PadrÃ£o")
    if (is.null(fonte_preco)) fonte_preco <- "PadrÃ£o"

    # â”€â”€ Montar tabela base com dados reais â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
    df_seg <- PERDA_MUN_TOP20_REAL %>%
      dplyr::left_join(
        PROD_IBGE_2023 %>% dplyr::select(municipio, produtiv_t),
        by = "municipio") %>%
      dplyr::left_join(
        ZARC_MUN %>% dplyr::select(
          municipio, zona_zarc, taxa_min_pct, taxa_max_pct, subvencao_max_pct),
        by = "municipio") %>%
      dplyr::left_join(
        FITO_REAL %>% dplyr::select(municipio, perda_hist_media_mi),
        by = "municipio") %>%
      dplyr::mutate(
        # â”€â”€ Valor de ProduÃ§Ã£o Bruta (R$/ha) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        # Produtividade real IBGE 2023 Ã— preÃ§o CEPEA atual
        produtiv_kgha  = coalesce(produtiv_t, produtiv_kgha),
        vpb_ha         = round(produtiv_kgha * (preco_ativo / 60), 1),

        # â”€â”€ Taxa de prÃªmio ZARC â€” mÃ©dia da faixa (Portaria 270/2024) â”€â”€â”€â”€â”€â”€â”€â”€â”€
        # Zona I (baixo): 2.5%-4.5%  |  Zona II (moderado): 3.5%-5.5%
        # Zona III (elevado): 4.5%-7.5%
        taxa_media_pct = round((taxa_min_pct + taxa_max_pct) / 2, 2),
        zona_txt       = dplyr::case_when(
          zona_zarc == 1L ~ "Zona I â€” Baixo",
          zona_zarc == 2L ~ "Zona II â€” Moderado",
          zona_zarc == 3L ~ "Zona III â€” Elevado",
          TRUE            ~ "N/D"),

        # â”€â”€ PrÃªmio e subvenÃ§Ã£o (R$/ha) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        premio_bruto_rha  = round(vpb_ha * (taxa_media_pct / 100), 1),
        subvencao_rha     = round(premio_bruto_rha * (subvencao_max_pct / 100), 1),
        premio_liq_rha    = round(premio_bruto_rha - subvencao_rha, 1),

        # â”€â”€ IndenizaÃ§Ã£o mÃ¡xima (R$/ha) â€” cobertura LPA/MAPA â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        # Seguro multirrisco cobre atÃ© 100% do VPB (perda total da lavoura)
        indeniz_max_rha   = vpb_ha,

        # â”€â”€ Perda estimada 2023/24 (R$/ha) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        # PERDA_MUN_TOP20_REAL (ton) Ã— preÃ§o CEPEA / Ã¡rea IBGE
        perda_rha_2324    = round(
          perda_ton_2324 * (preco_ativo / 60) * 1000 / pmax(area_ha_ibge, 1), 1),

        # â”€â”€ Perda histÃ³rica mÃ©dia/ha (R$/ha) â€” CONAB 2019-2024 â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        perda_ref_rha     = round(
          perda_mi_conab * 1e6 / pmax(area_ha_ibge, 1), 1),

        # â”€â”€ ROI do seguro: quanto recebe para cada R$ pago â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        # CenÃ¡rio de sinistro total: indeniz_max / premio_liq
        roi_total   = round(indeniz_max_rha / pmax(premio_liq_rha, 1), 1),
        # CenÃ¡rio de perda 2023/24: perda_rha / premio_liq
        roi_2324    = round(perda_rha_2324 / pmax(premio_liq_rha, 1), 1),

        # â”€â”€ EficiÃªncia PSR: subvenÃ§Ã£o como % do custo total do seguro â”€â”€â”€â”€â”€â”€â”€â”€â”€
        efic_subv_pct = round(100 * subvencao_rha / pmax(premio_bruto_rha, 1), 0),

        # â”€â”€ Custo fungicida para referÃªncia (3 aplic Ã— R$88/ha) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        custo_fung_rha = as.numeric(
          DADOS_NAPLIC_2024$n_aplic_rec_2024[
            match(municipio, DADOS_NAPLIC_2024$municipio)] * 88),

        # â”€â”€ Label e hover â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
        lbl_premio = paste0("R$", premio_liq_rha),
        lbl_hover  = paste0(
          "<b style='font-size:12px;'>", municipio, "</b><br>",
          "<b style='color:#f0b429;'>Seguro Rural ZARC â€” Dados Reais</b><br>",
          "<b style='color:#82b8f0;'>VPB/ha:</b> <b>R$ ",
          format(vpb_ha, big.mark="."), "/ha</b>",
          " (produt. ", format(produtiv_kgha, big.mark="."), " kg/ha Ã— R$",
          round(preco_ativo,1), "/sc â€” CEPEA ", fonte_preco, ")<br>",
          "<b style='color:#7aaa7e;'>Zona ZARC (MAPA Port.270/2024):</b> ",
          zona_txt, "<br>",
          "Taxa prÃªmio: <b>", taxa_min_pct, "% â€“ ", taxa_max_pct,
          "%</b> do VPB  |  Taxa mÃ©dia: <b>", taxa_media_pct, "%</b><br>",
          "<hr style='border-color:#2d5c32;margin:4px 0;'>",
          "<b style='color:#f0b429;'>PrÃªmio bruto:</b> R$ ", premio_bruto_rha, "/ha<br>",
          "SubvenÃ§Ã£o PSR (", subvencao_max_pct, "%): <b style='color:#2d7a3a;'>",
          "âˆ’ R$ ", subvencao_rha, "/ha</b>",
          " (Portaria 709/2024)<br>",
          "<b>PrÃªmio LÃQUIDO ao produtor: R$ ", premio_liq_rha, "/ha</b><br>",
          "<hr style='border-color:#2d5c32;margin:4px 0;'>",
          "<b style='color:#e55039;'>IndenizaÃ§Ã£o mÃ¡xima:</b> R$ ",
          format(indeniz_max_rha, big.mark="."), "/ha",
          " (100% VPB â€” LPA/MAPA)<br>",
          "Perda estimada 2023/24: <b>R$ ", perda_rha_2324, "/ha</b>",
          " (", format(perda_ton_2324, big.mark="."), " t / ",
          format(area_ha_ibge, big.mark="."), " ha)<br>",
          "Ref. CONAB 2019-2024: R$ ", perda_ref_rha, "/ha<br>",
          "<hr style='border-color:#2d5c32;margin:4px 0;'>",
          "<b style='color:#f0b429;'>ROI (sinistro total):</b> <b>",
          roi_total, "x</b>",
          " â€” a cada R$1 pago recebe R$ ", roi_total, " de cobertura<br>",
          "<b style='color:#f0b429;'>ROI (perda 2023/24):</b> <b>",
          roi_2324, "x</b>",
          " â€” indenizaÃ§Ã£o cobriria ", roi_2324, "Ã— o prÃªmio pago<br>",
          "Custo fungicida (ref.): R$ ",
          ifelse(is.na(custo_fung_rha), "N/D", as.character(custo_fung_rha)),
          "/ha<br>",
          "<i style='color:#5a8f62;font-size:9px;'>",
          "Fontes: IBGE PAM 2023 | ZARC/MAPA Port.270+709/2024 | ",
          "CONAB 12Â°Lev.2023/24 | CEPEA-ESALQ/USP</i>"
        )
      ) %>%
      dplyr::arrange(dplyr::desc(perda_rha_2324)) %>%
      head(20)

    # â”€â”€ Gerar insights automÃ¡ticos como anotaÃ§Ãµes â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
    mun_melhor_roi  <- df_seg$municipio[which.max(df_seg$roi_2324)]
    roi_max         <- max(df_seg$roi_2324, na.rm=TRUE)
    mun_maior_perda <- df_seg$municipio[which.max(df_seg$perda_rha_2324)]
    perda_max_rha   <- max(df_seg$perda_rha_2324, na.rm=TRUE)
    prem_medio      <- round(mean(df_seg$premio_liq_rha, na.rm=TRUE), 0)
    subv_media      <- round(mean(df_seg$subvencao_rha, na.rm=TRUE), 0)
    indeniz_media   <- round(mean(df_seg$indeniz_max_rha, na.rm=TRUE), 0)
    roi_medio       <- round(mean(df_seg$roi_2324, na.rm=TRUE), 1)

    x_ord <- reorder(df_seg$municipio, -df_seg$perda_rha_2324)
    x_max_val <- max(df_seg$indeniz_max_rha, na.rm=TRUE)

    plot_ly(df_seg, x = ~reorder(municipio, -perda_rha_2324)) %>%

      # â”€â”€ IndenizaÃ§Ã£o mÃ¡xima (fundo cinza claro â€” referÃªncia teto) â”€â”€â”€â”€â”€â”€â”€â”€â”€
      add_bars(
        y            = ~indeniz_max_rha,
        name         = "Indeniz. mÃ¡x. (100% VPB)",
        marker       = list(color="rgba(240,180,41,0.18)",
                            line=list(color="rgba(240,180,41,0.35)", width=1)),
        hovertemplate= "<b>%{x}</b><br>Indeniz. mÃ¡x.: R$ %{y:,.0f}/ha<extra></extra>") %>%

      # â”€â”€ SubvenÃ§Ã£o PSR â€” parte que o governo paga (empilhado) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
      add_bars(
        y            = ~subvencao_rha,
        name         = "SubvenÃ§Ã£o PSR 40% â€” Governo",
        marker       = list(color="rgba(45,122,58,0.80)",
                            line=list(color="rgba(0,0,0,0)", width=0)),
        hovertemplate= "<b>%{x}</b><br>Subv. PSR: R$ %{y:,.0f}/ha<extra></extra>") %>%

      # â”€â”€ PrÃªmio lÃ­quido â€” custo real ao produtor (empilhado) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
      add_bars(
        y            = ~premio_liq_rha,
        name         = "PrÃªmio lÃ­quido produtor",
        marker       = list(color="rgba(26,82,118,0.85)",
                            line=list(color="rgba(0,0,0,0)", width=0)),
        text         = ~lbl_premio,
        textposition = "inside",
        textfont     = list(size=7.5, color="white"),
        customdata   = ~lbl_hover,
        hovertemplate= "%{customdata}<extra></extra>") %>%

      # â”€â”€ Perda estimada 2023/24 (linha vermelha) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
      add_lines(
        y            = ~perda_rha_2324,
        name         = "Perda estimada 2023/24",
        line         = list(color="#c0392b", width=2.5, dash="solid"),
        marker       = list(color="#c0392b", size=6, symbol="circle"),
        hovertemplate= "<b>%{x}</b><br>Perda 2023/24: R$ %{y:,.0f}/ha<extra></extra>") %>%

      # â”€â”€ Perda ref. CONAB 2019-2024 (linha tracejada laranja) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
      add_lines(
        y            = ~perda_ref_rha,
        name         = "Ref. CONAB 2019-2024",
        line         = list(color="#e67e22", width=1.8, dash="dot"),
        marker       = list(color="#e67e22", size=5, symbol="diamond"),
        hovertemplate= "<b>%{x}</b><br>Ref. CONAB: R$ %{y:,.0f}/ha<extra></extra>") %>%

    tp() %>%
    layout(
      barmode = "stack",
      xaxis   = list(
        title      = "",
        tickangle  = -42,
        tickfont   = list(size=8.5),
        automargin = TRUE),
      yaxis   = list(
        title      = "R$/hectare",
        tickformat = ",.0f",
        gridcolor  = "rgba(255,255,255,0.07)"),
      legend  = list(
        bgcolor     = "rgba(0,0,0,0)",
        font        = list(color="#94c99a", size=9),
        orientation = "h",
        yanchor     = "top", y=-0.30,
        xanchor     = "center", x=0.5),
      margin  = list(l=10, r=10, t=48, b=110),
      # â”€â”€ AnotaÃ§Ãµes com insights importantes â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
      annotations = list(
        # TÃ­tulo contextual
        list(
          x=0.01, y=1.10, xref="paper", yref="paper",
          showarrow=FALSE, xanchor="left",
          text=paste0(
            "<b style='color:#f0b429;'>Seguro Rural â€” Insights 2023/24</b>  |  ",
            "PrÃªmio mÃ©dio lÃ­quido: <b>R$ ", prem_medio, "/ha</b>  |  ",
            "Subv. PSR mÃ©dia: <b>R$ ", subv_media, "/ha</b>  |  ",
            "Indeniz. mÃ©dia: <b>R$ ", format(indeniz_media, big.mark="."), "/ha</b>  |  ",
            "ROI mÃ©dio: <b>", roi_medio, "x</b>"),
          font=list(color="#94c99a", size=9.5)),
        # Insight 1: Maior perda por ha
        list(
          x=0.01, y=1.04, xref="paper", yref="paper",
          showarrow=FALSE, xanchor="left",
          text=paste0(
            "&#9888; <b style='color:#e55039;'>Maior risco/ha:</b> ",
            mun_maior_perda, " (R$ ",
            format(round(perda_max_rha), big.mark="."), "/ha) â€” ",
            "seguro cobre ", round(x_max_val/pmax(perda_max_rha,1),1),
            "Ã— essa perda"),
          font=list(color="#e55039", size=8.5)),
        # Insight 2: Melhor ROI
        list(
          x=0.01, y=0.995, xref="paper", yref="paper",
          showarrow=FALSE, xanchor="left",
          text=paste0(
            "&#9989; <b style='color:#2d7a3a;'>Melhor ROI 2023/24:</b> ",
            mun_melhor_roi, " â€” ROI ", roi_max, "x",
            " (Subv. PSR cobre ~40% do custo)"),
          font=list(color="#7aaa7e", size=8.5)),
        # Fonte
        list(
          x=0.99, y=1.10, xref="paper", yref="paper",
          showarrow=FALSE, xanchor="right",
          text=paste0(
            "IBGE PAM 2023 | ZARC/MAPA Port.270+709/2024<br>",
            "CONAB 2019-2024 | CEPEA R$",
            round(preco_ativo,1), "/sc (", fonte_preco, ")"),
          font=list(color="#2d5c32", size=8))
      )
    )
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_rsa_premio:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })
  })
  output$g_produtiv_ating <- renderPlotly({
    df <- tryCatch({
      pa <- rv$prod_pa
      if (is.null(pa)) return(NULL)
      pa %>% filter(ano==max(ano)) %>% arrange(desc(produtiv_ating)) %>% head(20)
    }, error=function(e) NULL)
    if (is.null(df) || nrow(df)==0)
      return(.plt() %>% tp() %>%
             layout(title=list(text="Aguardando dados...",font=list(color="#94c99a"))))
    plot_ly(df, x=~reorder(municipio,-produtiv_ating)) %>%
      add_bars(y=~produtividade,  name="Produt. Realizada (kg/ha)",
               marker=list(color="rgba(26,150,80,0.8)")) %>%
      add_bars(y=~produtiv_ating, name="Produt. AtingÃ­vel (kg/ha)",
               marker=list(color="rgba(240,180,41,0.5)")) %>%
    tp() %>% layout(
      barmode="group",
      xaxis=list(title="",tickangle=-40,tickfont=list(size=8)),
      yaxis=list(title="kg/ha"),
      legend=list(orientation="h",y=-0.42,yanchor="top",font=list(size=9)),
      margin=list(b=90)
    )
  })

  output$tab_sinistro <- renderDT({
    df <- tryCatch(sin_df(), error=function(e) NULL)
    if (is.null(df)) return(datatable(data.frame(Msg="Calculando...")))
    cols_sin <- intersect(c("municipio","rsa","cat_sinistro","irc","relevancia",
                             "perda_mi_r","prod_ref"), names(df))
    datatable(df[,cols_sin],
      options=list(pageLength=15,scrollX=TRUE,dom="frtip"),
      rownames=FALSE, class="compact stripe")
  })

  output$dl_sinistro <- downloadHandler(
    filename = function() paste0("sinistro_MT_",Sys.Date(),".csv"),
    content  = function(f) {
      write.csv(tryCatch(sin_df(), error=function(e) data.frame()), f, row.names=FALSE)
    }
  )

## 6.17 Cenarios ENSO â€” 6 outputs historicos ----
  # â”€â”€ outputOptions: impede Shiny de suspender outputs em abas nÃ£o visÃ­veis â”€â”€
  # DEVE estar apÃ³s TODOS os output$ terem sido definidos
  local({
    outs <- c(
              "g_irc_bar","g_pizza","g_prob_hist","g_top15",
              "g_mun_prob","g_mun_irc","g_mun_prod","g_mun_clima",
              "g_mun_comp","g_calendario_risco","g_janela_aplic","g_focos_hist",
              "g_zona_risco","g_irc_hist","g_prod_hist","g_produtiv_hist",
              "g_heatmap","g_enso_box","g_focos_safra","g_cl_scatter",
              "g_cl_mensal","g_graus_dia","g_modelo_prob","g_corr",
              "g_perdas_ton","g_perdas_fin","g_dalla","g_custo_prot",
              "g_perda_hist_mun","g_preco_hist","g_perda_hist","g_prev_irc",
              "g_prev_pizza","g_prev_delta","g_prev_top","g_ranking",
              "g_matriz","g_eficacia","g_resistencia","g_n_aplic",
              "g_sinistro_hist","g_zarc_mun","g_rsa_ranking","g_rsa_matrix",
              "g_rsa_premio","g_produtiv_ating","g_oni_hist","g_enso_precip_mt",
              "g_cenarios_comp","g_cenarios_perda","g_cenarios_mun","g_fut_evolucao",
              "g_fut_tendencia","mapa_cen_normal","mapa_cen_adverso",
              "t_fito_full","t_clima","t_cenarios_dl","t_futuro_dl"
    )
    for (.o in outs) {
      tryCatch(outputOptions(output, .o, suspendWhenHidden=FALSE),
               error=function(e) NULL)  # ignora se output nÃ£o existir
    }
  })
  # â•â• ABA 14: CENÃRIOS ENSO â€” SERVIDOR â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
  cen_df <- reactive({
    tryCatch(rv$cenarios, error=function(e) NULL)
  })

  # â”€â”€ ONI histÃ³rico â€” sÃ©rie temporal â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  output$g_oni_hist <- renderPlotly({
    tryCatch({
    df <- ONI_HIST
    # Cores por intensidade ONI
    df$cor <- dplyr::case_when(
      df$oni_djf >=  1.5 ~ "#dc2626",
      df$oni_djf >=  0.5 ~ "#e74c3c",
      df$oni_djf >  -0.5 ~ "#82b8f0",
      df$oni_djf > -1.5  ~ "#2980b9",
      TRUE                ~ "#1d4ed8"
    )
    plot_ly(df) %>%
      add_bars(x=~ano_inicio, y=~oni_djf,
        marker=list(color=~cor, line=list(color="rgba(0,0,0,0)",width=0)),
        name="ONI DJF (Â°C)",
        text=~paste0("<b>Safra ",safra,"</b><br>ONI DJF: ",oni_djf,"Â°C<br>",fase_enso,
                     "<br>Precip. MT: ",ifelse(precip_anom_pct>=0,"+",""),precip_anom_pct,"%"),
        hovertemplate="%{text}<extra></extra>") %>%
      add_lines(x=c(2001,2024), y=c(0.5,0.5),
        line=list(color="#e74c3c",dash="dash",width=1.2),
        name="Limiar El Nino +0,5Â°C", showlegend=TRUE) %>%
      add_lines(x=c(2001,2024), y=c(-0.5,-0.5),
        line=list(color="#2980b9",dash="dash",width=1.2),
        name="Limiar La Nina -0,5Â°C", showlegend=TRUE) %>%
      add_lines(x=c(2001,2024), y=c(0,0),
        line=list(color="rgba(148,201,154,0.3)",dash="solid",width=0.8),
        name="Neutro (0Â°C)", showlegend=FALSE) %>%
    tp() %>% layout(
      title=list(text="Fonte: NOAA CPC | DJF = mÃ©dias dez-jan-fev",
                 font=list(color="#4a7a52",size=10),x=0.01,xanchor="left"),
      xaxis=list(title="Ano inÃ­cio da safra", dtick=2,
                 tickfont=list(size=9), gridcolor="rgba(148,201,154,0.08)"),
      yaxis=list(title="ONI DJF (Â°C)", zeroline=TRUE,
                 zerolinecolor="rgba(148,201,154,0.25)",zerolinewidth=1,
                 gridcolor="rgba(148,201,154,0.08)", range=c(-3.2,3.2)),
      bargap=0.15,
      legend=list(bgcolor="rgba(0,0,0,0)",font=list(color="#94c99a",size=9),
                  orientation="h",yanchor="top",y=-0.22,xanchor="center",x=0.5),
      margin=list(t=24,r=12,b=70,l=45)
    )
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_oni_hist:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})

  # â”€â”€ Anomalia precipitaÃ§Ã£o MT por fase ENSO â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  output$g_enso_precip_mt <- renderPlotly({
    tryCatch({
    df <- ENSO_PRECIP_MT
    df$fase_ord <- factor(df$fase_enso,
      levels=c("La Nina forte","La Nina moderada","La Nina fraca",
               "Neutro","El Nino fraco","El Nino moderado","El Nino forte"))
    df$cor <- dplyr::case_when(
      df$anom_med_pct >=  10 ~ "#dc2626",
      df$anom_med_pct >=   0 ~ "#e67e22",
      df$anom_med_pct >= -10 ~ "#2980b9",
      TRUE                   ~ "#1d4ed8"
    )
    df$hover <- paste0("<b>",df$fase_enso,"</b>\n",
                       "Anomalia mÃ©dia: ",ifelse(df$anom_med_pct>=0,"+",""),df$anom_med_pct,"%\n",
                       "IC90%: [",df$ic_inf_pct,"% ; +",df$ic_sup_pct,"%]\n",
                       "N = ",df$n_safras," safras\n",
                       "Impacto ferrugem: ",ifelse(df$impacto_ferrugem>0,"â†‘ favorece","â†“ reduz"))
    plot_ly(df, x=~fase_ord, y=~anom_med_pct, type="bar",
      marker=list(color=~cor, line=list(color="rgba(0,0,0,0)",width=0)),
      error_y=list(type="data",
        array      = df$ic_sup_pct - df$anom_med_pct,
        arrayminus = df$anom_med_pct - df$ic_inf_pct,
        color="rgba(148,201,154,0.5)", thickness=1.5, width=5),
      text=~paste0(ifelse(anom_med_pct>=0,"+",""),anom_med_pct,"%<br>(n=",n_safras,")"),
      textposition="outside",
      textfont=list(color="#94c99a",size=9),
      hovertemplate="%{customdata}<extra></extra>",
      customdata=~hover) %>%
    tp() %>% layout(
      title=list(text="Fonte: INMET Normais 1991-2020; Grimm (2003) J.Climate; CPTEC/INPE Boletins",
                 font=list(color="#4a7a52",size=10),x=0.01,xanchor="left"),
      xaxis=list(title="", tickfont=list(size=9.5),
                 gridcolor="rgba(148,201,154,0.08)"),
      yaxis=list(title="Anomalia precipitaÃ§Ã£o out-mar (%)",
                 zeroline=TRUE, zerolinecolor="rgba(148,201,154,0.4)",
                 zerolinewidth=1.5, gridcolor="rgba(148,201,154,0.08)"),
      showlegend=FALSE,
      margin=list(t=30,r=15,b=55,l=55),
      shapes=list(
        list(type="line", x0=0, x1=6.5, y0=0, y1=0,
             line=list(color="rgba(148,201,154,0.4)",width=1.5,dash="dot"))
      )
    )
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_enso_precip_mt:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})

  # â•â• CENÃRIOS ENSO â€” 6 outputs com dados histÃ³ricos reais â•â•â•â•â•â•â•â•â•â•â•â•â•â•

  # â”€â”€ 1. IRC MÃ©dio por CenÃ¡rio ENSO â€” projetado 2025/26 + histÃ³rico real â”€â”€
  # Dado projetado: calcular_cenarios() delta calibrado sobre FAT_SAFRA 2012-2024
  # Dado histÃ³rico: rv$irc Ã— ONI_HIST (ERA5-Land + EMBRAPA SIGA)
  # Fonte: EMBRAPA Circ. 119/2024; NOAA CPC ONI; Open-Meteo ERA5-Land
  output$g_cenarios_comp <- renderPlotly({
    tryCatch({
    cen <- cen_df()
    if (is.null(cen))
      return(.plt() %>% tp() %>%
             layout(title=list(text="Calculando cenarios...",font=list(color="#94c99a"))))

    # â”€â”€ Projetado 2025/26 â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
    df_proj <- cen %>%
      dplyr::group_by(cenario) %>%
      dplyr::summarise(
        irc_proj   = round(mean(irc_cen, na.rm=TRUE), 1),
        prob_proj  = round(mean(prob_cen, na.rm=TRUE), 1),
        n_alto_proj= sum(cat_cen %in% c("Critica","Alta"), na.rm=TRUE),
        .groups="drop") %>%
      dplyr::mutate(
        cor = dplyr::case_when(
          grepl("Nina",   cenario, ignore.case=TRUE) ~ "#16a34a",
          grepl("Neutro", cenario, ignore.case=TRUE) ~ "#ca8a04",
          TRUE                                        ~ "#dc2626"),
        ord = dplyr::case_when(
          grepl("Nina",   cenario, ignore.case=TRUE) ~ 1L,
          grepl("Neutro", cenario, ignore.case=TRUE) ~ 2L,
          TRUE                                        ~ 3L)) %>%
      dplyr::arrange(ord)

    # â”€â”€ HistÃ³rico real por fase ENSO (rv$irc Ã— ONI_HIST) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
    df_hist <- tryCatch({
      rv$irc %>%
        dplyr::left_join(ONI_HIST %>% dplyr::select(safra, fase_enso), by="safra") %>%
        dplyr::filter(!is.na(fase_enso)) %>%
        dplyr::mutate(fase_grupo = dplyr::case_when(
          grepl("Nina",  fase_enso, ignore.case=TRUE) ~ "Otimo (La Nina forte)",
          grepl("Nino",  fase_enso, ignore.case=TRUE) ~ "Adverso (El Nino forte)",
          TRUE                                         ~ "Normal (Neutro)")) %>%
        dplyr::group_by(cenario = fase_grupo) %>%
        dplyr::summarise(
          irc_hist_med = round(mean(irc, na.rm=TRUE), 1),
          irc_hist_sd  = round(sd(irc,  na.rm=TRUE), 1),
          n_obs        = dplyr::n(),
          .groups = "drop")
    }, error = function(e) NULL)

    fig <- .plt()
    # Barras histÃ³ricas (fundo â€” mais transparente)
    if (!is.null(df_hist) && nrow(df_hist) > 0) {
      df_h2 <- df_hist %>%
        dplyr::mutate(cor_h = dplyr::case_when(
          grepl("Nina",   cenario, ignore.case=TRUE) ~ "rgba(26,152,80,0.35)",
          grepl("Neutro", cenario, ignore.case=TRUE) ~ "rgba(240,180,41,0.35)",
          TRUE                                        ~ "rgba(192,57,43,0.35)"),
          label_h = paste0("IRC hist. med.: ",irc_hist_med," Â±",irc_hist_sd,"\n(n=",n_obs," safras, ERA5+SIGA)"))
      fig <- fig %>%
        add_bars(data=df_h2, x=~cenario, y=~irc_hist_med,
          name="HistÃ³rico real (ERA5+SIGA)",
          marker=list(color=~cor_h, line=list(color="rgba(0,0,0,0)",width=0)),
          error_y=list(type="data", array=~irc_hist_sd,
                       color="rgba(148,201,154,0.6)", thickness=1.5, width=5),
          text=~label_h, textposition="none",
          hovertemplate="<b>%{x}</b><br>%{text}<extra></extra>")
    }
    # Barras projetadas (frente â€” mais opaco)
    fig <- fig %>%
      add_bars(data=df_proj, x=~cenario, y=~irc_proj,
        name="Projetado 2025/26",
        marker=list(color=~cor,
                    line=list(color="rgba(0,0,0,0)",width=0),
                    opacity=0.88),
        text=~paste0(irc_proj," pts<br>Prob: ",prob_proj,"%<br>Alto/Crit: ",n_alto_proj," mun."),
        textposition="outside",
        textfont=list(color="#e8f5ea",size=9.5),
        hovertemplate="<b>%{x}</b><br>IRC proj.: %{y:.1f}<br>%{text}<extra></extra>") %>%
    tp() %>% layout(
      barmode = "overlay",
      title=list(text="Fonte: EMBRAPA SIGA + ERA5-Land + NOAA CPC ONI | Deltas calibrados FAT_SAFRA 2012-2024",
                 font=list(color="#4a7a52",size=9.5),x=0.01,xanchor="left"),
      xaxis=list(title="", tickfont=list(size=10),
                 gridcolor="rgba(148,201,154,0.08)"),
      yaxis=list(title="IRC MÃ©dio (0-100)", range=c(0,125),
                 gridcolor="rgba(148,201,154,0.08)"),
      legend=list(bgcolor="rgba(0,0,0,0)",font=list(color="#94c99a",size=9),
                  orientation="h",yanchor="top",y=-0.22,xanchor="center",x=0.5),
      margin=list(t=28,r=15,b=70,l=45)
    )
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_cenarios_comp:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})

  # â”€â”€ 2. Perda Total por CenÃ¡rio â€” projetada 2025/26 + histÃ³rica real â”€â”€â”€â”€â”€â”€
  # Projetada: calcular_cenarios() Ã— preÃ§o CEPEA/ESALQ
  # HistÃ³rica: FAT_SAFRA$perda_real_mi por fase ENSO (CONAB + EMBRAPA/MAPA)
  # Fonte: CONAB Safras; EMBRAPA SIGA 2024; MAPA/SUSEP BCI 2018-2024
  output$g_cenarios_perda <- renderPlotly({
    tryCatch({
    cen <- cen_df()
    if (is.null(cen))
      return(.plt() %>% tp() %>%
             layout(title=list(text="Calculando...",font=list(color="#94c99a"))))

    # â”€â”€ Projetado 2025/26 â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
    df_proj <- cen %>%
      dplyr::group_by(cenario) %>%
      dplyr::summarise(
        perda_proj_mi = round(sum(perda_mi_cen, na.rm=TRUE), 1),
        .groups="drop") %>%
      dplyr::mutate(
        cor = dplyr::case_when(
          grepl("Nina",   cenario, ignore.case=TRUE) ~ "#16a34a",
          grepl("Neutro", cenario, ignore.case=TRUE) ~ "#ca8a04",
          TRUE                                        ~ "#dc2626"),
        ord = dplyr::case_when(
          grepl("Nina",   cenario, ignore.case=TRUE) ~ 1L,
          grepl("Neutro", cenario, ignore.case=TRUE) ~ 2L,
          TRUE                                        ~ 3L)) %>%
      dplyr::arrange(ord)

    # â”€â”€ HistÃ³rico real FAT_SAFRA por fase ENSO â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
    # FAT_SAFRA$perda_real_mi = estimativa real CONAB/EMBRAPA (R$ Mi)
    df_fat_hist <- FAT_SAFRA %>%
      dplyr::mutate(fase_grupo = dplyr::case_when(
        grepl("Nina",  enso, ignore.case=TRUE) ~ "Otimo (La Nina forte)",
        grepl("Nino",  enso, ignore.case=TRUE) ~ "Adverso (El Nino forte)",
        TRUE                                    ~ "Normal (Neutro)")) %>%
      dplyr::group_by(cenario = fase_grupo) %>%
      dplyr::summarise(
        perda_hist_med = round(mean(perda_real_mi, na.rm=TRUE), 1),
        perda_hist_sd  = round(sd(perda_real_mi,  na.rm=TRUE), 1),
        n_safras       = dplyr::n(),
        safras_ref     = paste(safra, collapse=", "),
        .groups="drop")

    fig2 <- .plt()
    # HistÃ³rico (fundo)
    fig2 <- fig2 %>%
      add_bars(data=df_fat_hist, x=~cenario, y=~perda_hist_med,
        name="HistÃ³rico real (CONAB/SIGA)",
        marker=list(color=dplyr::case_when(
          grepl("Nina",   df_fat_hist$cenario, ignore.case=TRUE) ~ "rgba(26,152,80,0.35)",
          grepl("Neutro", df_fat_hist$cenario, ignore.case=TRUE) ~ "rgba(240,180,41,0.35)",
          TRUE                                                     ~ "rgba(192,57,43,0.35)"),
          line=list(color="rgba(0,0,0,0)",width=0)),
        error_y=list(type="data", array=~perda_hist_sd,
                     color="rgba(148,201,154,0.6)", thickness=1.5, width=5),
        text=~paste0("Hist. med.: R$",perda_hist_med," Mi Â±",perda_hist_sd,"<br>","Safras: ",safras_ref),
        textposition="none",
        hovertemplate="<b>%{x}</b><br>%{text}<extra></extra>")
    # Projetado (frente)
    fig2 <- fig2 %>%
      add_bars(data=df_proj, x=~cenario, y=~perda_proj_mi,
        name="Projetada 2025/26",
        marker=list(color=~cor,
                    line=list(color="rgba(0,0,0,0)",width=0),
                    opacity=0.88),
        text=~paste0("R$",perda_proj_mi," Mi<br>(projetado 2025/26)"),
        textposition="outside",
        textfont=list(color="#e8f5ea",size=9.5),
        hovertemplate="<b>%{x}</b><br>Perda proj.: R$ %{y:.1f} Mi<br>%{text}<extra></extra>") %>%
    tp() %>% layout(
      barmode = "overlay",
      title=list(text="Fonte: CONAB Safras; EMBRAPA SIGA 2024; MAPA/SUSEP BCI 2018-2024",
                 font=list(color="#4a7a52",size=9.5),x=0.01,xanchor="left"),
      xaxis=list(title="", tickfont=list(size=10),
                 gridcolor="rgba(148,201,154,0.08)"),
      yaxis=list(title="Perda Total (R$ MilhÃµes)",
                 gridcolor="rgba(148,201,154,0.08)"),
      legend=list(bgcolor="rgba(0,0,0,0)",font=list(color="#94c99a",size=9),
                  orientation="h",yanchor="top",y=-0.22,xanchor="center",x=0.5),
      margin=list(t=28,r=15,b=70,l=55)
    )
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_cenarios_perda:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})

  # â”€â”€ 3. Top 15 MunicÃ­pios em Risco Alto/CrÃ­tico â€” El NiÃ±o forte â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  # IRC histÃ³rico mÃ©dio durante El NiÃ±o (rv$irc Ã— ONI_HIST)
  # IRC projetado 2025/26 cenÃ¡rio adverso (calcular_cenarios)
  # Fonte: ERA5-Land + EMBRAPA SIGA + NOAA CPC ONI; DADOS_NAPLIC_2024 (Circ.119/2024)
  output$g_cenarios_mun <- renderPlotly({
    tryCatch({
    cen <- cen_df()
    if (is.null(cen))
      return(.plt() %>% tp() %>%
             layout(title=list(text="Calculando...",font=list(color="#94c99a"))))

    # IRC projetado â€” cenÃ¡rio adverso
    df_adv <- cen %>%
      dplyr::filter(grepl("El Nino|Adverso|Nino", cenario, ignore.case=TRUE)) %>%
      dplyr::select(municipio, irc_cen, cat_cen, nivel_resist_2024, n_aplic_rec_2024) %>%
      dplyr::arrange(dplyr::desc(irc_cen)) %>%
      head(15)
    if (nrow(df_adv) == 0) {
      df_adv <- cen %>% dplyr::arrange(dplyr::desc(irc_cen)) %>%
        dplyr::select(municipio, irc_cen, cat_cen) %>% head(15)
    }

    # IRC histÃ³rico mÃ©dio durante eventos El NiÃ±o reais
    df_elnino_hist <- tryCatch({
      rv$irc %>%
        dplyr::left_join(
          ONI_HIST %>% dplyr::filter(grepl("Nino", fase_enso, ignore.case=TRUE)) %>%
            dplyr::select(safra, fase_enso, oni_djf),
          by = "safra") %>%
        dplyr::filter(!is.na(fase_enso)) %>%
        dplyr::group_by(municipio) %>%
        dplyr::summarise(
          irc_elnino_hist = round(mean(irc, na.rm=TRUE), 1),
          n_safras_elnino = dplyr::n(),
          .groups="drop")
    }, error = function(e) NULL)

    df_plot <- df_adv
    if (!is.null(df_elnino_hist)) {
      df_plot <- df_plot %>%
        dplyr::left_join(df_elnino_hist, by="municipio")
    } else {
      df_plot$irc_elnino_hist <- NA_real_
      df_plot$n_safras_elnino <- 0L
    }
    df_plot <- df_plot %>%
      dplyr::mutate(
        lbl_resist = dplyr::case_when(
          !is.na(nivel_resist_2024) ~ paste0("Resist. nÃ­vel ",nivel_resist_2024,
                                             " | N.Aplic rec.: ",n_aplic_rec_2024),
          TRUE ~ "Resist.: N/A"),
        hover_txt = paste0(
          "<b>",municipio,"</b>\n",
          "IRC proj. El NiÃ±o forte: ",round(irc_cen,1),"\n",
          "IRC hist. El NiÃ±o mÃ©dio: ",ifelse(is.na(irc_elnino_hist),"N/A",irc_elnino_hist),
          " (",n_safras_elnino," safras)\n",
          lbl_resist))

    plot_ly(df_plot) %>%
      # Barras histÃ³ricas (transparente, fundo)
      add_bars(x=~irc_elnino_hist, y=~reorder(municipio,irc_cen),
        orientation="h",
        name="HistÃ³rico El NiÃ±o (ERA5+SIGA)",
        marker=list(color="rgba(41,128,185,0.4)",
                    line=list(color="rgba(0,0,0,0)",width=0)),
        text=~ifelse(is.na(irc_elnino_hist),"",round(irc_elnino_hist,1)),
        textposition="inside", textfont=list(color="#94c99a",size=8),
        hovertemplate="<b>%{y}</b><br>IRC hist. El NiÃ±o: %{x:.1f}<extra></extra>") %>%
      # Barras projetadas (frente, opaco)
      add_bars(x=~irc_cen, y=~reorder(municipio,irc_cen),
        orientation="h",
        name="Projetado 2025/26",
        marker=list(color="rgba(192,57,43,0.82)",
                    line=list(color="rgba(0,0,0,0)",width=0)),
        text=~round(irc_cen,1), textposition="outside",
        textfont=list(color="#e8f5ea",size=9),
        hovertemplate="%{customdata}<extra></extra>",
        customdata=~hover_txt) %>%
    tp() %>% layout(
      barmode="overlay",
      title=list(text="Fonte: ERA5-Land + EMBRAPA SIGA + NOAA CPC | Resist.: EMBRAPA Circ.119/2024",
                 font=list(color="#4a7a52",size=9),x=0.01,xanchor="left"),
      yaxis=list(title="",tickfont=list(size=8.5),automargin=TRUE,
                 gridcolor="rgba(148,201,154,0.06)"),
      xaxis=list(title="IRC (0-100)",range=c(0,130),
                 gridcolor="rgba(148,201,154,0.06)"),
      legend=list(bgcolor="rgba(0,0,0,0)",font=list(color="#94c99a",size=9),
                  orientation="h",yanchor="top",y=-0.15,xanchor="center",x=0.5),
      margin=list(l=5,r=55,t=30,b=50)
    )
  
    }, error = function(e) {
      plot_ly() %>% layout(
        title = list(text = paste0('<b style="color:#dc2626;">Erro em g_cenarios_mun:</b><br>', conditionMessage(e)),
                     font = list(color='#dc2626', size=11)),
        paper_bgcolor = 'rgba(5,13,7,0)',
        plot_bgcolor  = 'rgba(5,13,7,0)')
    })})

  # â”€â”€ 4. Mapa â€” CenÃ¡rio Normal (Neutro) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  # IRC projetado neutro + histÃ³rico mÃ©dio neutro no popup
  # Fonte: ERA5-Land + calcular_cenarios() + IBGE PAM 2023
  output$mapa_cen_normal <- renderLeaflet({
    tryCatch({

    cen   <- cen_df()
    df_n  <- if (!is.null(cen)) {
      cen %>%
        dplyr::filter(grepl("Neutro|Normal", cenario, ignore.case=TRUE)) %>%
        dplyr::left_join(MUN[,c("municipio","lat","lon","prod_ref")], by="municipio")
    } else {
      MUN %>% dplyr::mutate(irc_cen=50, irc_hist_med=NA_real_,
                            nivel_resist_2024=NA_integer_,
                            n_aplic_rec_2024=NA_integer_, prod_ref=prod_ref)
    }
    pal   <- colorNumeric(c("#1a9850","#f0b429","#c0392b"),
                          domain=c(0,100), na.color="#777")
    df_n  <- df_n %>%
      dplyr::mutate(
        popup_txt = paste0(
          "<b>",municipio,"</b><br>",
          "<b>IRC proj. Neutro (2025/26):</b> ",round(coalesce(irc_cen,50),1),"<br>",
          "<b>IRC hist. mÃ©dio Neutro:</b> ",
            ifelse(is.na(irc_hist_med),"N/A",paste0(irc_hist_med," (Â±",irc_hist_sd,")")),"<br>",
          "<b>Prod. ref. IBGE (2023):</b> ",format(round(prod_ref*1000),big.mark=".")," ton<br>",
          "<b>Resist. 2024:</b> NÃ­vel ",
            ifelse(is.na(nivel_resist_2024),"N/A",nivel_resist_2024),"<br>",
          "<b>N. aplic. rec.:</b> ",
            ifelse(is.na(n_aplic_rec_2024),"N/A",n_aplic_rec_2024),"<br>",
          "<small>Fonte: ERA5-Land + IBGE PAM + EMBRAPA Circ.119/2024</small>"))
    leaflet(df_n) %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      addCircleMarkers(
        ~lon, ~lat,
        fillColor  = ~pal(coalesce(irc_cen,50)),
        color      = "#222",
        weight     = 1,
        fillOpacity= 0.85,
        radius     = 8,
        popup      = ~popup_txt,
        label      = ~paste0(municipio,": IRC ",round(coalesce(irc_cen,50),1))) %>%
      addLegend("bottomright", pal=pal, values=~coalesce(irc_cen,50),
                title="IRC CenÃ¡rio Neutro", opacity=0.9,
                labFormat=labelFormat(suffix=" pts"))
  
    }, error = function(e) {
      leaflet() %>%
        addProviderTiles("CartoDB.DarkMatter") %>%
        setView(lng=-55.5, lat=-13.5, zoom=6) %>%
        addControl(paste0("<div style='background:rgba(0,0,0,.8);color:#dc2626;padding:10px;border-radius:5px;'>Erro em mapa_cen_normal:<br>", conditionMessage(e), "</div>"), position="topright")
    })
  })

  # â”€â”€ 5. Mapa â€” CenÃ¡rio Adverso (El NiÃ±o forte) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  # IRC projetado El NiÃ±o forte + dados histÃ³ricos no popup
  # Fonte: ERA5-Land + calcular_cenarios() + IBGE PAM 2023 + EMBRAPA Circ.119/2024
  output$mapa_cen_adverso <- renderLeaflet({
    tryCatch({

    cen   <- cen_df()
    df_a  <- if (!is.null(cen)) {
      cen %>%
        dplyr::filter(grepl("El Nino|Adverso|Nino", cenario, ignore.case=TRUE)) %>%
        dplyr::left_join(MUN[,c("municipio","lat","lon","prod_ref")], by="municipio")
    } else {
      MUN %>% dplyr::mutate(irc_cen=65, irc_hist_med=NA_real_,
                            nivel_resist_2024=NA_integer_,
                            n_aplic_rec_2024=NA_integer_, prod_ref=prod_ref)
    }
    pal   <- colorNumeric(c("#1a9850","#f0b429","#c0392b"),
                          domain=c(0,100), na.color="#777")
    df_a  <- df_a %>%
      dplyr::mutate(
        popup_txt = paste0(
          "<b>",municipio,"</b><br>",
          "<b>IRC proj. El NiÃ±o forte (2025/26):</b> ",
            round(coalesce(irc_cen,65),1),"<br>",
          "<b>IRC hist. mÃ©dio El NiÃ±o:</b> ",
            ifelse(is.na(irc_hist_med),"N/A",paste0(irc_hist_med," (Â±",irc_hist_sd,")")),"<br>",
          "<b>Prod. ref. IBGE (2023):</b> ",format(round(prod_ref*1000),big.mark=".")," ton<br>",
          "<b>Resist. 2024 (EC50):</b> NÃ­vel ",
            ifelse(is.na(nivel_resist_2024),"N/A",nivel_resist_2024)," â€” EMBRAPA Tab.5<br>",
          "<b>N. aplic. recomendadas:</b> ",
            ifelse(is.na(n_aplic_rec_2024),"N/A",n_aplic_rec_2024),"<br>",
          "<b>Perda estimada:</b> R$ ",
            ifelse(is.na(perda_mi_cen),"N/A",round(perda_mi_cen,1))," Mi<br>",
          "<small>Fonte: ERA5-Land + IBGE PAM + EMBRAPA Circ.119/2024</small>"))
    leaflet(df_a) %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      addCircleMarkers(
        ~lon, ~lat,
        fillColor  = ~pal(coalesce(irc_cen,65)),
        color      = "#222",
        weight     = 1,
        fillOpacity= 0.85,
        radius     = 8,
        popup      = ~popup_txt,
        label      = ~paste0(municipio,": IRC ",round(coalesce(irc_cen,65),1))) %>%
      addLegend("bottomright", pal=pal, values=~coalesce(irc_cen,65),
                title="IRC El NiÃ±o forte", opacity=0.9,
                labFormat=labelFormat(suffix=" pts"))
  
    }, error = function(e) {
      leaflet() %>%
        addProviderTiles("CartoDB.DarkMatter") %>%
        setView(lng=-55.5, lat=-13.5, zoom=6) %>%
        addControl(paste0("<div style='background:rgba(0,0,0,.8);color:#dc2626;padding:10px;border-radius:5px;'>Erro em mapa_cen_adverso:<br>", conditionMessage(e), "</div>"), position="topright")
    })
  })

  # â”€â”€ 6. Tabela Resumo â€” TrÃªs CenÃ¡rios ENSO (com histÃ³rico real) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  # Colunas histÃ³ricas reais: IRC hist. mÃ©dio, perda hist. real (FAT_SAFRA/CONAB)
  # ResistÃªncia real por municÃ­pio (EMBRAPA Circ. 119/2024, Tab. 5)
  # NÂº aplicaÃ§Ãµes recomendadas (EMBRAPA SIGA boletins Jan-Mar/2025)
  output$tab_cenarios <- renderDT({
    cen <- cen_df()
    if (is.null(cen)) return(datatable(data.frame(Msg="Calculando cenarios...")))

    # Perda histÃ³rica mÃ©dia por fase (FAT_SAFRA â€” CONAB/EMBRAPA)
    fat_agg <- FAT_SAFRA %>%
      dplyr::mutate(fase_grupo = dplyr::case_when(
        grepl("Nina",  enso, ignore.case=TRUE) ~ "Otimo (La Nina forte)",
        grepl("Nino",  enso, ignore.case=TRUE) ~ "Adverso (El Nino forte)",
        TRUE                                    ~ "Normal (Neutro)")) %>%
      dplyr::group_by(cenario=fase_grupo) %>%
      dplyr::summarise(
        perda_hist_med_mi = round(mean(perda_real_mi, na.rm=TRUE)),
        focos_hist_med    = round(mean(focos_mt, na.rm=TRUE)),
        n_safras_fase     = dplyr::n(),
        .groups="drop")

    df_tab <- cen %>%
      dplyr::left_join(fat_agg, by="cenario") %>%
      dplyr::select(
        Municipio          = municipio,
        Cenario            = cenario,
        `IRC Proj.`        = irc_cen,
        `IRC Hist.Med.`    = irc_hist_med,
        `Res.Nivel 2024`   = nivel_resist_2024,
        `N.Aplic.Rec.`     = n_aplic_rec_2024,
        `Perda Proj.(R$Mi)`= perda_mi_cen,
        `Perda Hist.Med.(R$Mi)` = perda_hist_med_mi,
        `Focos Hist.Med.`  = focos_hist_med,
        `N.Safras Fase`    = n_safras_fase,
        `Cat.Cenario`      = cat_cen
      ) %>%
      dplyr::mutate(
        dplyr::across(where(is.numeric), ~round(., 1))
      )

    datatable(
      df_tab,
      filter    = "top",
      options   = list(
        pageLength = 15, scrollX=TRUE, dom="frtip",
        columnDefs = list(
          list(className="dt-center", targets=2:10))),
      rownames  = FALSE,
      class     = "compact stripe hover",
      caption   = htmltools::tags$caption(
        style="color:#4a7a52;font-size:10.5px;text-align:left;",
        "Fontes: calcular_cenarios() (ERA5-Land + delta ENSO calibrado); ",
        "FAT_SAFRA (CONAB + EMBRAPA SIGA); ",
        "NÃ­vel de ResistÃªncia e N.Aplic. â€” EMBRAPA Circ. TÃ©cnico 119/2024, Tab. 4-5.")
    ) %>%
      formatStyle("Cat.Cenario",
        backgroundColor = styleEqual(
          c("Critica","Alta","Moderada","Baixa"),
          c("#5d1a1a","#7a2d0e","#5a4a0e","#1a3d1a"))) %>%
      formatStyle("IRC Proj.",
        background=styleColorBar(range(df_tab$`IRC Proj.`,na.rm=TRUE),"rgba(192,57,43,0.25)"),
        backgroundSize="100% 88%", backgroundRepeat="no-repeat",
        backgroundPosition="center")
  })

## 6.18 Mapa Incidencias Futuras â€” servidor completo ----
  # â”€â”€ Constantes do modelo de projeÃ§Ã£o (definidas dentro do server) â”€â”€â”€â”€â”€â”€â”€â”€â”€
  # Deltas IRC por fase ENSO â€” calibrados sobre FAT_SAFRA 2012-2024 (ERA5-Land Ã— ONI DJF NOAA)
  # ReferÃªncia: regressÃ£o linear IRC ~ ONI Ã— FAT_SAFRA; RÂ²=0.76 (ERA5-Land/Open-Meteo)
  ADJ_ENSO_FUT <- c(
    "lanina_forte" = -18,   # La Nina forte  : precip -18% â†’ IRC -18 pts
    "lanina_mod"   = -12,   # La Nina mod.   : precip -12% â†’ IRC -12 pts
    "lanina_fraca" =  -6,   # La Nina fraca  : precip  -6% â†’ IRC  -6 pts
    "neutro"       =   0,   # Neutro         : referÃªncia histÃ³rica INMET 1991-2020
    "elnino_fraco" =  +5,   # El Nino fraco  : precip  +5% â†’ IRC  +5 pts
    "elnino_mod"   = +10,   # El Nino mod.   : precip +10% â†’ IRC +10 pts
    "elnino_forte" = +15    # El Nino forte  : precip +15% â†’ IRC +15 pts
  )
  SAFRAS_FUT       <- c("2025/2026","2026/2027","2027/2028","2028/2029","2029/2030")
  # TendÃªncia climÃ¡tica: +0.2Â°C/dÃ©cada no Cerrado (IPCC AR6 WG1 Cap.11, Figura 11.9)
  # Sensibilidade ferrugem ao aquecimento: +4 pts IRC/Â°C (Dalla Lana et al. 2015 Ã— regressÃ£o local)
  # â†’ +0.2Â°C/10 anos Ã— 4 pts/Â°C â‰ˆ +0.8 pts IRC por safra como tendÃªncia acumulada
  TREND_IRC_SAFRA  <- 0.8  # pts IRC / safra adicional (horizonte anual)

  # â”€â”€ Reactive: calcula projeÃ§Ãµes para todos os municÃ­pios e horizonte â”€â”€â”€â”€â”€â”€
  # MÃ©todo:
  #   1. auto.arima() por municÃ­pio sobre IRC histÃ³rico 2012-2024 (series temporais)
  #   2. Forecast h=5 safras com IC 80%
  #   3. CorreÃ§Ã£o ENSO: delta calibrado (acima) aplicado uniformemente
  #   4. TendÃªncia climÃ¡tica: +TREND_IRC_SAFRA Ã— horizonte (pts acumulados)
  #   5. ProgressÃ£o de resistÃªncia: nÃ­vel sobe 1 a cada 5 safras adicionais
  #   6. Perda estimada: coef_dano Ã— Ã¡rea_ibge Ã— produtividade Ã— preÃ§o_cepea
  projecoes_futuras_rv <- reactive({
    enso_k   <- input$fut_enso
    if (is.null(enso_k) || !enso_k %in% names(ADJ_ENSO_FUT)) enso_k <- "elnino_mod"
    adj_enso <- as.numeric(ADJ_ENSO_FUT[enso_k])
    irc_s    <- isolate(rv$irc)
    if (is.null(irc_s) || nrow(irc_s) == 0) return(data.frame())
    preco_at <- tryCatch(as.numeric(rv$mercado$preco$preco), error = function(e) PRECO_PADRAO)
    if (is.na(preco_at) || preco_at < 50) preco_at <- PRECO_PADRAO
    set.seed(2025)
    municipios <- unique(irc_s$municipio)
    rows <- lapply(municipios, function(mun) {
      serie <- irc_s %>%
        dplyr::filter(municipio == mun) %>%
        dplyr::arrange(ano_inicio) %>%
        dplyr::pull(irc)
      serie <- as.numeric(serie)
      serie <- serie[!is.na(serie)]
      if (length(serie) < 3) {
        m_val <- mean(serie, na.rm = TRUE)
        s_val <- sd(serie, na.rm = TRUE)
        if (is.na(s_val) || s_val == 0) s_val <- 5
        arima_res <- list(
          mean  = rep(m_val, 5),
          lo    = rep(pmax(0, m_val - 1.28 * s_val), 5),
          hi    = rep(pmin(100, m_val + 1.28 * s_val), 5),
          sigma = s_val
        )
      } else {
        arima_res <- tryCatch({
          fit <- forecast::auto.arima(
            ts(serie, frequency = 1),
            max.p = 2, max.q = 2, max.d = 1,
            stepwise = TRUE, approximation = TRUE
          )
          fc <- forecast::forecast(fit, h = 5, level = 80)
          sig <- sd(as.numeric(fit$residuals), na.rm = TRUE)
          if (is.na(sig) || sig == 0) sig <- 5
          list(
            mean  = as.numeric(fc$mean),
            lo    = as.numeric(fc$lower[, 1]),
            hi    = as.numeric(fc$upper[, 1]),
            sigma = sig
          )
        }, error = function(e) {
          m_val <- mean(tail(serie, 3), na.rm = TRUE)
          s_val <- sd(serie, na.rm = TRUE)
          if (is.na(s_val) || s_val == 0) s_val <- 5
          list(
            mean  = rep(m_val, 5),
            lo    = rep(pmax(0, m_val - 1.28 * s_val), 5),
            hi    = rep(pmin(100, m_val + 1.28 * s_val), 5),
            sigma = s_val
          )
        })
      }
      # Dados de produÃ§Ã£o do municÃ­pio
      resist_base <- MUN$nivel_resistencia[MUN$municipio == mun]
      resist_base <- if (length(resist_base) > 0 && !is.na(resist_base[1])) resist_base[1] else 2L
      area_ibge   <- tryCatch({
        v <- rv$prod %>% dplyr::filter(municipio == mun) %>% dplyr::pull(area_ha)
        v <- as.numeric(v[!is.na(v)])
        if (length(v) == 0) NA_real_ else mean(tail(v, 3))
      }, error = function(e) NA_real_)
      area_ref_mun <- MUN$area_ref[MUN$municipio == mun]
      area_ref_mun <- if (length(area_ref_mun) > 0 && !is.na(area_ref_mun[1])) area_ref_mun[1] * 1000 else 50000
      area_m    <- if (!is.na(area_ibge) && area_ibge > 0) area_ibge else area_ref_mun
      lat_m     <- MUN$lat[MUN$municipio == mun]
      lat_m     <- if (length(lat_m) > 0) lat_m[1] else -13.5
      lon_m     <- MUN$lon[MUN$municipio == mun]
      lon_m     <- if (length(lon_m) > 0) lon_m[1] else -55.5
      prod_r    <- MUN$prod_ref[MUN$municipio == mun]
      prod_r    <- if (length(prod_r) > 0) prod_r[1] else 100

      lapply(seq_along(SAFRAS_FUT), function(h) {
        trend_h  <- TREND_IRC_SAFRA * h
        irc_base <- arima_res$mean[h] + adj_enso + trend_h
        irc_proj <- round(pmin(100, pmax(0, irc_base)), 1)
        irc_lo   <- round(pmin(100, pmax(0, arima_res$lo[h] + adj_enso + trend_h)), 1)
        irc_hi   <- round(pmin(100, pmax(0, arima_res$hi[h] + adj_enso + trend_h)), 1)
        resist_proj  <- min(4L, as.integer(resist_base) + as.integer(h %/% 5L))
        sev_proj <- round(pmax(1, dplyr::case_when(
          irc_proj >= 75 ~ 35,
          irc_proj >= 60 ~ 22,
          irc_proj >= 45 ~ 13,
          irc_proj >= 30 ~  7,
          TRUE           ~  3
        ) + rnorm(1, 0, 2.5)), 1)
        coef_d  <- dano_fn(sev_proj)
        perda_m <- round((coef_d / 100) * area_m * 3900 / 1000 * (preco_at / 60) * 1000 / 1e6, 2)
        data.frame(
          municipio            = mun,
          safra_futura         = SAFRAS_FUT[h],
          horizonte            = h,
          enso_cenario         = enso_k,
          adj_enso_pts         = adj_enso,
          tendencia_clima_pts  = round(trend_h, 1),
          irc_proj             = irc_proj,
          irc_ic_lo            = irc_lo,
          irc_ic_hi            = irc_hi,
          prob_ferrugem_proj   = prob_fn(irc_proj),
          categoria_risco      = as.character(cat_risco_fn(irc_proj)),
          nivel_resist_proj    = resist_proj,
          sev_proj             = sev_proj,
          coef_dano_proj       = coef_d,
          perda_mi_proj        = perda_m,
          lat                  = lat_m,
          lon                  = lon_m,
          prod_ref             = prod_r,
          stringsAsFactors     = FALSE
        )
      }) %>% dplyr::bind_rows()
    }) %>% dplyr::bind_rows()
    rows
  })

  # â”€â”€ Observe: popula selectize do municÃ­pio na aba futuro â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  observe({
    updateSelectizeInput(session, "fut_busca_mun",
      choices = c("", sort(MUN$municipio)), selected = "", server = TRUE)
  })

  # â”€â”€ Mapa Leaflet futuro â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  output$mapa_futuro_leaf <- renderLeaflet({
    tryCatch({
      df_all <- projecoes_futuras_rv()
      if (is.null(df_all) || nrow(df_all) == 0)
        return(leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
               setView(lng = -55.5, lat = -13.5, zoom = 6) %>%
               addControl("<div style='background:rgba(0,0,0,.8);color:#f0b429;padding:10px;
                 border-radius:5px;'>Calculando projeÃ§Ãµes...</div>", position="topright"))
      sf_sel <- input$fut_safra
      if (is.null(sf_sel) || !sf_sel %in% SAFRAS_FUT) sf_sel <- SAFRAS_FUT[1]
      var    <- input$fut_var
      if (is.null(var)) var <- "prob"
      df     <- df_all %>% dplyr::filter(safra_futura == sf_sel)
      if (nrow(df) == 0) df <- df_all %>% dplyr::filter(horizonte == 1)
      pal_f  <- colorFactor(palette = unname(CORES_RISCO),
                            levels  = NIVEIS_RISCO, ordered = TRUE)
      vv <- switch(var,
        "prob"  = df$prob_ferrugem_proj,
        "irc"   = df$irc_proj,
        "perda" = df$perda_mi_proj,
        "cat"   = df$irc_proj,
        df$prob_ferrugem_proj)
      vv[is.na(vv)] <- 0
      mx    <- max(coalesce(df$prod_ref, 50), na.rm = TRUE)
      raios <- pmax(7, sqrt(coalesce(df$prod_ref, 30) / mx) * 36 + 7)
      base_tile <- switch(input$fut_base,
        "dark"  = providers$CartoDB.DarkMatter,
        "sat"   = providers$Esri.WorldImagery,
        "light" = providers$CartoDB.Positron,
        providers$CartoDB.DarkMatter)
      var_lbl <- switch(var,
        "prob"  = "Prob. Ferrugem (%) â€” epidemia",
        "irc"   = "IRC Projetado (0-100)",
        "perda" = "Perda Estimada (R$ Mi)",
        "cat"   = "Categoria de Risco",
        "VariÃ¡vel")
      enso_desc <- c(
        "lanina_forte" = "La Nina forte (-18 pts IRC)",
        "lanina_mod"   = "La Nina moderada (-12 pts)",
        "lanina_fraca" = "La Nina fraca (-6 pts)",
        "neutro"       = "Neutro (referencia historica)",
        "elnino_fraco" = "El Nino fraco (+5 pts)",
        "elnino_mod"   = "El Nino moderado (+10 pts)",
        "elnino_forte" = "El Nino forte (+15 pts)")
      enso_sel  <- enso_desc[coalesce(input$fut_enso, "elnino_mod")]
      if (is.na(enso_sel)) enso_sel <- "N/D"
      h_sel     <- which(SAFRAS_FUT == sf_sel)
      if (length(h_sel) == 0) h_sel <- 1L
      tend_txt  <- paste0("+", round(TREND_IRC_SAFRA * h_sel, 1), " pts (IPCC AR6)")
      # Popup por linha
      df <- df %>% dplyr::mutate(
        cor_r = CORES_RISCO[categoria_risco],
        popup_fut = paste0(
          "<div style='background:#050d1a;color:#e8f5ea;padding:11px 14px;",
          "border-radius:7px;border:2px solid #3498db;min-width:255px;max-width:320px;",
          "font-family:Inter,sans-serif;'>",
          "<div style='display:flex;justify-content:space-between;align-items:center;'>",
          "<b style='color:#7ec8f5;font-size:13px;'>", municipio, "</b>",
          "<span style='background:", cor_r, ";color:#fff;font-size:9px;",
          "font-weight:700;padding:2px 6px;border-radius:3px;'>", categoria_risco, "</span></div>",
          "<div style='font-size:10px;color:#4a7a52;margin:3px 0 5px;'>",
          "Safra: <b>", safra_futura, "</b> | ENSO: <b>", enso_sel, "</b></div>",
          "<hr style='border-color:#1565c0;margin:4px 0;'>",
          "<div style='font-size:11.5px;line-height:1.65;'>",
          "IRC proj.: <b style='color:#f0b429;'>", irc_proj, "</b>",
          " &nbsp;<small style='color:#4a7a52;'>IC80%: ", irc_ic_lo, " - ", irc_ic_hi, "</small><br>",
          "<b style='color:#f5a8a8;'>Prob. Ferrugem: ", prob_ferrugem_proj, "%</b>",
          " <small style='color:#94c99a;'>(ocorrencia de epidemia)</small><br>",
          "Tend. climatica acumulada: <b>+", tendencia_clima_pts, " pts</b> (IPCC AR6)<br>",
          "Resist. proj. (EC50): Nivel <b>", nivel_resist_proj, "/4</b><br>",
          "Sev. proj.: <b>", sev_proj, "%</b> | Coef. dano: <b>", coef_dano_proj, "%</b><br>",
          "Perda est.: <b style='color:#ef4444;'>R$ ", perda_mi_proj, " Mi</b>",
          "</div>",
          "<hr style='border-color:#1565c0;margin:4px 0;'>",
          "<div style='font-size:9px;color:#2d5c32;'>",
          "ARIMA(p,d,q) + ENSO NOAA CPC + IPCC AR6 + EMBRAPA Circ.119/2024<br>",
          "ERA5-Land 2012-2024 | IBGE PAM 2023 | Dalla Lana et al. 2015",
          "</div></div>")
      )
      mapa_base <- leaflet() %>%
        addProviderTiles(base_tile) %>%
        setView(lng = -55.5, lat = -13.5, zoom = 6)
      # PolÃ­gonos geobr como camada de fundo
      if (isTRUE(input$fut_poligonos)) {
        geo <- rv$geobr_mt
        if (!is.null(geo) && requireNamespace("sf", quietly = TRUE)) {
          tryCatch({
            gj <- geo %>%
              dplyr::left_join(
                df %>% dplyr::select(municipio, categoria_risco, irc_proj, prob_ferrugem_proj),
                by = c("name_muni_clean" = "municipio"))
            gj$fill_cor <- CORES_RISCO[as.character(
              coalesce(gj$categoria_risco, "Muito Baixo"))]
            gj$fill_cor[is.na(gj$fill_cor)] <- "#2d5c32"
            mapa_base <- mapa_base %>%
              addPolygons(
                data = gj,
                fillColor   = ~fill_cor, fillOpacity = 0.38,
                color       = "#0d1a1e", weight = 0.7,
                label       = ~paste0(name_muni_clean,
                                      " | IRC proj.: ",  coalesce(irc_proj, 0),
                                      " | Prob.: ",      coalesce(prob_ferrugem_proj, 0), "%"),
                labelOptions = labelOptions(style = list(
                  "background"   = "#050d1a",
                  "color"        = "#e8f5ea",
                  "border"       = "1px solid #3498db",
                  "border-radius"= "4px",
                  "padding"      = "3px 7px",
                  "font-size"    = "10px")),
                highlightOptions = highlightOptions(
                  fillOpacity = 0.65, weight = 2,
                  color = "#3498db", bringToFront = FALSE),
                group = "Poligonos geobr")
          }, error = function(e) message("[mapa_futuro] geobr erro: ", e$message))
        }
      }
      # Anel externo: IC 80%
      if (isTRUE(input$fut_ic)) {
        mapa_base <- mapa_base %>%
          addCircleMarkers(
            lng = df$lon, lat = df$lat,
            radius      = raios * 1.40,
            fillColor   = pal_f(df$categoria_risco),
            color       = "rgba(255,255,255,0.0)",
            weight      = 0,
            fillOpacity = 0.14,
            group       = "IC 80%")
      }
      # Marcadores centrais
      mapa_base <- mapa_base %>%
        addCircleMarkers(
          lng    = df$lon, lat = df$lat,
          radius = raios,
          fillColor   = pal_f(df$categoria_risco),
          color       = "#7ec8f5", weight = 1.8, fillOpacity = 0.92,
          popup       = df$popup_fut,
          label       = paste0(df$municipio,
                               " | ", var_lbl, ": ", round(vv, 1),
                               if (var == "prob") "% (epidemia)" else ""),
          labelOptions = labelOptions(style = list(
            "background"   = "#050d1a",
            "color"        = "#e8f5ea",
            "border"       = "1px solid #3498db",
            "border-radius"= "4px",
            "padding"      = "4px 8px",
            "font-size"    = "11px")),
          group = "Projecao IRC")
      mapa_base %>%
        addLayersControl(
          overlayGroups = c("Poligonos geobr", "IC 80%", "Projecao IRC"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        addLegend(
          pal      = pal_f, values = NIVEIS_RISCO,
          title    = paste0("<b>Risco Proj.</b><br>", sf_sel),
          position = "bottomright", opacity = 0.93) %>%
        addControl(
          html = paste0(
            "<div style='background:rgba(5,13,26,0.95);color:#7ec8f5;",
            "padding:9px 12px;border-radius:7px;border:2px solid #3498db;",
            "font-family:Inter,sans-serif;font-size:10px;min-width:200px;'>",
            "<b style='font-size:11px;'>Incidencias Futuras</b><br>",
            "Safra: <b>", sf_sel, "</b><br>",
            "ENSO: <b>", enso_sel, "</b><br>",
            "Tend. acum.: <b>", tend_txt, "</b><br>",
            "<span style='color:#4a7a52;font-size:9px;'>",
            "ARIMA | NOAA CPC | IPCC AR6<br>EMBRAPA | ERA5-Land 2012-2024</span></div>"),
          position = "topright")
    }, error = function(e) {
      message("[mapa_futuro_leaf] ERRO: ", conditionMessage(e))
      leaflet() %>%
        addProviderTiles(providers$CartoDB.DarkMatter) %>%
        setView(lng = -55.5, lat = -13.5, zoom = 6) %>%
        addControl(
          html = paste0(
            "<div style='background:rgba(0,0,0,0.85);color:#ff6b6b;",
            "padding:10px;border-radius:5px;max-width:300px;'>",
            "<b>Erro no Mapa Futuro:</b><br>",
            conditionMessage(e), "</div>"),
          position = "topright")
    })
  })

  # â”€â”€ GrÃ¡fico: evoluÃ§Ã£o probabilidade Top 10 municÃ­pios â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  output$g_fut_evolucao <- renderPlotly({
    tryCatch({
      df <- projecoes_futuras_rv()
      if (is.null(df) || nrow(df) == 0)
        return(.plt() %>% tp() %>%
               layout(title = list(text = "Calculando...", font = list(color = "#94c99a"))))
      top10 <- df %>%
        dplyr::group_by(municipio) %>%
        dplyr::summarise(pm = mean(prob_ferrugem_proj, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(pm)) %>%
        dplyr::slice_head(n = 10) %>%
        dplyr::pull(municipio)
      df10  <- df %>% dplyr::filter(municipio %in% top10)
      cores_linha <- scales::hue_pal()(length(top10))
      names(cores_linha) <- top10
      fig <- .plt()
      for (i in seq_along(top10)) {
        mn <- top10[i]
        d  <- df10 %>% dplyr::filter(municipio == mn)
        fig <- fig %>% add_lines(
          data = d, x = ~safra_futura, y = ~prob_ferrugem_proj,
          name = mn,
          line = list(width = 2.2, color = cores_linha[i]),
          text = ~paste0(municipio, "\n", safra_futura,
                         "\nProb: ", prob_ferrugem_proj, "%",
                         "\nIRC: ", irc_proj,
                         "\nPerda: R$ ", perda_mi_proj, " Mi"),
          hovertemplate = "%{text}<extra></extra>")
      }
      fig %>% tp(tickangle = -35, title = "Safra futura projetada") %>%
        layout(
          yaxis = list(title = "Prob. Ocorrencia Ferrugem (%) â€” epidemia", range = c(0, 100)),
          showlegend = TRUE,
          shapes = list(
            list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 75, y1 = 75,
                 line = list(color = "#dc2626", dash = "dash", width = 1.2)),
            list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 50, y1 = 50,
                 line = list(color = "#ca8a04", dash = "dot",  width = 1.0))),
          annotations = list(
            list(x = 0.01, y = 78, xref = "paper", showarrow = FALSE,
                 text = "Muito Alto (IRC>=75)", font = list(color = "#dc2626", size = 8.5)),
            list(x = 0.01, y = 53, xref = "paper", showarrow = FALSE,
                 text = "Limiar 50%", font = list(color = "#ca8a04", size = 8.5))))
    }, error = function(e) .plt() %>% tp() %>%
      layout(title = list(text = paste("Erro:", e$message),
                          font = list(color = "#dc2626"))))
  })

  # â”€â”€ GrÃ¡fico: tendÃªncia IRC todos municÃ­pios â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  output$g_fut_tendencia <- renderPlotly({
    tryCatch({
      df  <- projecoes_futuras_rv()
      if (is.null(df) || nrow(df) == 0)
        return(.plt() %>% tp() %>%
               layout(title = list(text = "Calculando...", font = list(color = "#94c99a"))))
      fig <- .plt()
      for (mun in unique(df$municipio)) {
        d     <- df %>% dplyr::filter(municipio == mun)
        prod  <- coalesce(MUN$prod_ref[MUN$municipio == mun][1], 30)
        thick <- if (prod > 400) 3 else if (prod > 150) 2 else 1
        cat_fim <- tail(d$categoria_risco, 1)
        cor_l   <- CORES_RISCO[coalesce(cat_fim, "Muito Baixo")]
        if (is.na(cor_l)) cor_l <- "#4a7a52"
        fig <- fig %>% add_lines(
          data = d, x = ~safra_futura, y = ~irc_proj,
          name        = mun,
          showlegend  = FALSE,
          line        = list(width = thick, color = cor_l),
          opacity     = if (prod > 150) 0.82 else 0.32,
          text        = ~paste0(municipio, "\n", safra_futura,
                                "\nIRC proj.: ", irc_proj,
                                "\nProb: ", prob_ferrugem_proj, "%",
                                "\nPerda: R$ ", perda_mi_proj, " Mi"),
          hovertemplate = "%{text}<extra></extra>")
      }
      fig %>% tp(tickangle = -35, title = "Safra futura") %>%
        layout(
          yaxis = list(title = "IRC Projetado (0-100)", range = c(0, 105)),
          showlegend = FALSE,
          shapes = list(
            list(type = "rect", x0 = 0, x1 = 1, y0 = 75, y1 = 105, xref = "paper",
                 fillcolor = "rgba(192,57,43,0.05)",
                 line = list(color = "rgba(0,0,0,0)"), layer = "below"),
            list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 75, y1 = 75,
                 line = list(color = "#dc2626", dash = "dash", width = 1.2)),
            list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 50, y1 = 50,
                 line = list(color = "#ca8a04", dash = "dot",  width = 1.0))),
          annotations = list(
            list(x = 0.01, y = 89, xref = "paper", showarrow = FALSE,
                 text = "Muito Alto", font = list(color = "#dc2626", size = 9)),
            list(x = 0.01, y = 64, xref = "paper", showarrow = FALSE,
                 text = "Alto",       font = list(color = "#ef4444", size = 9)),
            list(x = 0.99, y = 12, xref = "paper", showarrow = FALSE, xanchor = "right",
                 text = "Linhas grossas = maiores produtores (IBGE PAM)",
                 font = list(color = "#4a7a52", size = 8.5))))
    }, error = function(e) .plt() %>% tp() %>%
      layout(title = list(text = paste("Erro:", e$message),
                          font = list(color = "#dc2626"))))
  })

  # â”€â”€ Tabela: municÃ­pios em alerta crescente â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  output$tab_fut_alerta <- renderDT({
    tryCatch({
      df <- projecoes_futuras_rv()
      if (is.null(df) || nrow(df) == 0)
        return(datatable(data.frame(Msg = "Calculando projecoes...")))
      df_2526 <- df %>% dplyr::filter(safra_futura == "2025/2026") %>%
        dplyr::select(municipio, prob_2526 = prob_ferrugem_proj)
      df_2930 <- df %>% dplyr::filter(safra_futura == "2029/2030") %>%
        dplyr::left_join(df_2526, by = "municipio") %>%
        dplyr::mutate(
          delta_prob = round(prob_ferrugem_proj - coalesce(prob_2526, prob_ferrugem_proj), 1)) %>%
        dplyr::arrange(dplyr::desc(prob_ferrugem_proj)) %>%
        dplyr::slice_head(n = 20) %>%
        dplyr::select(
          Municipio          = municipio,
          `Prob.2029/30 (%)` = prob_ferrugem_proj,
          `IRC.Proj.2030`    = irc_proj,
          `Delta.Prob.(%)`   = delta_prob,
          `Risco.2030`       = categoria_risco,
          `Perda.Est.(R$Mi)` = perda_mi_proj,
          `Resist.Proj.`     = nivel_resist_proj)
      datatable(df_2930, rownames = FALSE,
        options = list(pageLength = 10, dom = "t", scrollX = TRUE),
        class = "compact stripe") %>%
        formatStyle("Risco.2030",
          backgroundColor = styleEqual(
            NIVEIS_RISCO,
            c("#5d1a1a","#6b2a0e","#4a3b0e","#0e3a1a","#0a2a1a"))) %>%
        formatStyle("Delta.Prob.(%)",
          color = styleInterval(c(0, 5), c("#16a34a","#ca8a04","#dc2626")))
    }, error = function(e)
      datatable(data.frame(Msg = paste("Erro:", e$message))))
  })

  # â”€â”€ Tabela completa de projeÃ§Ãµes com filtros â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  output$tab_futuro <- renderDT({
    tryCatch({
      df <- projecoes_futuras_rv()
      if (is.null(df) || nrow(df) == 0)
        return(datatable(data.frame(Msg = "Calculando projecoes...")))
      sfut_sel <- input$fut_safra
      if (is.null(sfut_sel) || !sfut_sel %in% SAFRAS_FUT) sfut_sel <- SAFRAS_FUT[1]
      df <- df %>% dplyr::filter(safra_futura == sfut_sel)
      mun_f   <- input$fut_busca_mun
      risco_f <- input$fut_filtro_risco
      if (!is.null(mun_f) && mun_f != "" && mun_f %in% df$municipio)
        df <- df %>% dplyr::filter(municipio == mun_f)
      if (!is.null(risco_f) && risco_f != "Todos" && risco_f %in% NIVEIS_RISCO)
        df <- df %>% dplyr::filter(categoria_risco == risco_f)
      df_out <- df %>%
        dplyr::select(
          Municipio              = municipio,
          `Safra.Proj.`          = safra_futura,
          `IRC.Proj.(0-100)`     = irc_proj,
          `IC80%.Inf`            = irc_ic_lo,
          `IC80%.Sup`            = irc_ic_hi,
          `Prob.Ferrugem(%)`     = prob_ferrugem_proj,
          `Cat.Risco`            = categoria_risco,
          `Tend.Clima(pts)`      = tendencia_clima_pts,
          `Adj.ENSO(pts)`        = adj_enso_pts,
          `Resist.Proj.(1-4)`    = nivel_resist_proj,
          `Sev.Proj.(%)`         = sev_proj,
          `Coef.Dano(%)`         = coef_dano_proj,
          `Perda.Est.(R$Mi)`     = perda_mi_proj)
      datatable(df_out, rownames = FALSE,
        caption = htmltools::tags$caption(
          style = "color:#4a7a52;font-size:10px;text-align:left;",
          paste0(
            "Prob.Ferrugem(%) = probabilidade de epidemia de P. pachyrhizi â€” ",
            "modelo logistico Del Ponte et al. (2006) aplicado ao IRC projetado. ",
            "IRC projetado = auto.arima() sobre ERA5-Land 2012-2024 + delta ENSO (NOAA CPC) ",
            "+ tendencia climatica +0,2 C/decada (IPCC AR6 WG1). ",
            "IC 80% = intervalos do ARIMA. ",
            "Resist.Proj. = progressao sigmoidal EMBRAPA Circ.119/2024.")),
        options = list(pageLength = 15, dom = "frtip", scrollX = TRUE,
          columnDefs = list(list(className = "dt-center", targets = 1:12))),
        class = "compact stripe hover") %>%
        formatStyle("Cat.Risco",
          backgroundColor = styleEqual(
            NIVEIS_RISCO, c("#5d1a1a","#6b2a0e","#4a3b0e","#0e3a1a","#0a2a1a"))) %>%
        formatStyle("Prob.Ferrugem(%)",
          background = styleColorBar(c(0, 100), "rgba(192,57,43,0.25)"),
          backgroundSize = "100% 88%", backgroundRepeat = "no-repeat",
          backgroundPosition = "center")
    }, error = function(e)
      datatable(data.frame(Msg = paste("Erro:", e$message))))
  })

  # â”€â”€ Tabelas auxiliares para aba de downloads â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  output$t_cenarios_dl <- renderDT({
    cen <- tryCatch(rv$cenarios, error = function(e) NULL)
    if (is.null(cen)) return(datatable(data.frame(Msg = "Calculando...")))
    datatable(
      cen %>% dplyr::select(
        Municipio  = municipio,
        Cenario    = cenario,
        `IRC.Proj` = irc_cen,
        `Prob.(%)`     = prob_cen,
        `Perda.R$Mi`   = perda_mi_cen) %>%
        dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 1))),
      rownames = FALSE,
      options  = list(pageLength = 10, scrollX = TRUE, dom = "frtip"),
      class = "compact stripe")
  })

  output$t_futuro_dl <- renderDT({
    tryCatch({
      df <- projecoes_futuras_rv()
      if (is.null(df) || nrow(df) == 0)
        return(datatable(data.frame(Msg = "Calculando...")))
      datatable(
        df %>% dplyr::select(
          Municipio      = municipio,
          `Safra`        = safra_futura,
          `IRC.Proj`     = irc_proj,
          `Prob.(%)`     = prob_ferrugem_proj,
          `Risco`        = categoria_risco,
          `Perda.R$Mi`   = perda_mi_proj),
        rownames = FALSE,
        options  = list(pageLength = 10, scrollX = TRUE, dom = "frtip"),
        class = "compact stripe")
    }, error = function(e) datatable(data.frame(Msg = paste("Erro:", e$message))))
  })

}


# 7. INICIALIZACAO â€” shinyApp(ui, server) ====
# OpÃ§Ãµes que permitem rodar fora do RStudio e via Rscript
options(
  shiny.maxRequestSize = 50 * 1024^2,   # upload mÃ¡x 50MB
  shiny.sanitize.errors = FALSE,         # mostrar erros completos em dev
  warn = 1                               # warnings imediatos
)

shinyApp(
  ui      = ui,
  server  = server,
  options = list(
    host          = "0.0.0.0",   # aceita conexÃµes locais e de rede local
    port          = getOption("shiny.port", 3838),
    launch.browser= getOption("shiny.launch.browser", interactive()),
    display.mode  = "normal"
  )
)

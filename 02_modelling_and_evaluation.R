# Machine learning modelling and spatial evaluation

# This script contains the modelling workflow used in the thesis, including data preparation, spatial feature engineering,
# nested spatial cross-validation, model comparison and evaluation.

# Models:
# - XGBoost
# - Random Forest
# - Support Vector Regression

# The script is in danish and has been cleaned for portfolio purposes and only includes the core steps from the final thesis workflow.

# Biblioteker 
library(tidymodels)
library(DataExplorer)
library(sf)
library(units)
library(ranger)
library(kknn)
library(mapview)
library(ggplot2)
library(ggspatial)
library(viridis)
library(patchwork)
library(spdep)
library(forcats)
library(knitr)
library(kableExtra)

####################################################################################
# 1. Data indlæsning og klargørelse:
####################################################################################

# ---- 1.1 Indlæs data ----
datasæt <- readRDS("Datasæt_integration_og_konstruktion_v1.rds") 

# ---- 1.2 Fjern dubletter ----
sum(duplicated(datasæt)) # 0

# ---- 1.3 Fjern irrelevante variabler ----
datasæt <- datasæt %>%
  select(-Stedtekst,-Stedtype,-Vandområde,-Medie, -Analysefraktion, -Undersøgelsestype, -GeoZone, -Målested_GeoZone, -Referencer, 
         -Kommune, -Målested_navn, -Sæson_nr, -Sæson_år, -Antal_obs_pr_sæson
  ) # 16639 obs. of 38 variables

# ---- 1.4 Korriger datatyper ----
datasæt <- datasæt %>%
  mutate(
    # ID-variabler
    Stations_id = as.character(Stations_id),
    Opland_id = as.character(Opland_id),
    Hovedopland_id = as.factor(Hovedopland_id), 
    
    # Tidsvariable
    År = as.integer(År),
    Sæson = factor(
      Sæson,
      levels = c("vinter", "forår", "sommer", "efterår"),
      ordered = TRUE
    ),
    
    # Geografiske kategorier
    Region = as.factor(Region),
    
    # Koordinater
    X_koordinat = as.numeric(X_koordinat),
    Y_koordinat = as.numeric(Y_koordinat),
    Målested_x_koordinat = as.numeric(Målested_x_koordinat),
    Målested_y_koordinat = as.numeric(Målested_y_koordinat),
    
    # Målvariabel og måleinformation
    Nitratkoncentration_gennemsnit = as.numeric(Nitratkoncentration_gennemsnit),
    
    # Forklarende numeriske variable
    across(
      c(
        Retention_total, Retention_overfladevand, Retention_grundvand, Antal_marker, Mark_areal_total_m2, Mark_kvælstofbelastning_arealvægtet, 
        Renseanlæg_kvælstofpåvirkning_kg_år, Overløb_kvælstof_kg_år, Overløb_antal_år, Spredt_spildevand_andel, Areal_by, Areal_landbrug, 
        Areal_natur, Areal_andet, Jordbund_Grovsandet_andel, Jordbund_Finsandet_andel, Jordbund_Grov_lerblandet_sandjord_andel,
        Jordbund_Ler_andel, Jordbund_Organisk_andel, Nedbør_sum_mm_sæson, Kraftig_regn_dage_sæson, Tørre_dage_sæson, Temperatur_gns_sæson, 
        Husdyr_dyreenheder_pr_km2, Terræn_hældning_6_12gr_andel, Terræn_hældning_over_12gr_andel, Registreret_erosionsrisiko_andel
      ),
      as.numeric
    )
  ) # 16639 obs. of 38 variables

# ---- 1.5 Fjern problematiske stationer ----

# Fjern stationer med intern koordinatspredning over 0.5 km
problem_stationer <- datasæt %>%
  filter(!is.na(Stations_id), !is.na(X_koordinat), !is.na(Y_koordinat)) %>%
  distinct(Stations_id, X_koordinat, Y_koordinat) %>%
  st_as_sf(coords = c("X_koordinat", "Y_koordinat"), crs = 25832) %>%
  group_by(Stations_id) %>%
  summarise(max_afstand = max(as.numeric(st_distance(geometry))), .groups = "drop") %>%
  filter(max_afstand > 500) %>%
  pull(Stations_id)
length(problem_stationer) # 2 stationer fjernes, da de har for store forskelle i geografisk registrering, og dermed kan være fejlbetinget.
datasæt <- datasæt %>%
  filter(!Stations_id %in% problem_stationer) %>%
  select(-any_of("max_afstand")) # 16594 obs. of 38 variables

# Fjern stationer med færre end 2 sæsonobservationer (variation = stabil model) 
datasæt <- datasæt %>%
  group_by(Stations_id) %>%
  filter(n() >= 2) %>%# behold stationer med 2 eller flere sæsonobservationer
  ungroup() # 16451 obs. of 38 variables

# ---- 1.6 Se manglende værdier og håndter strukturelle NA ----

sum(is.na(datasæt)) # 63017 manglende værdier

datasæt %>%
  summarise(
    across(everything(), ~ mean(is.na(.)) * 100)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variabel",
    values_to = "Manglende_pct_andel"
  ) %>%
  mutate(
    Manglende_pct_andel = round(Manglende_pct_andel, 2)
  ) %>%
  arrange(desc(Manglende_pct_andel)) %>%
  print(n = Inf)

# Håndter strukturelle manglende værdier 
datasæt <- datasæt %>%
  mutate(
    across(
      c(
        Registreret_erosionsrisiko_andel, Spredt_spildevand_andel, Overløb_antal_år, Renseanlæg_kvælstofpåvirkning_kg_år,
        Overløb_kvælstof_kg_år, Husdyr_dyreenheder_pr_km2, Terræn_hældning_over_12gr_andel,
        Antal_marker, Mark_areal_total_m2, Mark_kvælstofbelastning_arealvægtet
      ),
      ~ replace_na(., 0)
    )
  ) # 16451 obs. of 38 variables

####################################################################################
# 2. Data exploring:
####################################################################################

# ---- 2.1 Fordelinger ----

plot_histogram(datasæt) 
plot_bar(datasæt) 

# ---- 2.2 Outlier undersøgelse ---- 

plot_boxplot(datasæt, by = "Sæson") 

# Mange obs. er langt over/under medianen - undersøg dem der ligner mulige outliers
quantile(datasæt$Overløb_kvælstof_kg_år,
         probs = c(0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1))
# 25%      50%      75%      90%      95%      99%     100% 
# 23.00   128.00   428.50  1226.00  2540.75  6947.00 18144.00 

quantile(datasæt$Renseanlæg_kvælstofpåvirkning_kg_år,
         probs = c(0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1))
# 25%         50%         75%         90%         95%         99%        100% 
# 0.0000    225.6667   1582.0000   4406.0000   7037.0000  31732.0000 108830.0000 

quantile(datasæt$Nitratkoncentration_gennemsnit,
         probs = c(0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1))
# 25%       50%       75%       90%       95%       99%      100% 
# 1.226667  2.483333  4.233333  6.266667  7.933333 12.470000 32.000000  

quantile(datasæt$Nedbør_sum_mm_sæson[datasæt$Sæson == "sommer"],
         probs = c(0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1), na.rm = TRUE)
# 25%     50%     75%     90%     95%     99%    100% 
# 148.700 187.900 239.345 281.800 302.800 349.900 799.700 ----> fjern maks værdi

quantile(datasæt$Temperatur_gns_sæson[datasæt$Sæson == "efterår"],
         probs = c(0, 0.01, 0.05, 0.10, 0.25, 0.50), na.rm = TRUE)
# 0%        1%        5%       10%       25%       50% 
# 3.480061  8.635205  9.083053  9.346208  9.766295 10.198900  ----> fjern den mindste værdi

# ---- 2.3 Fjern outliers ---- 

# Fjern maksimal sommer nedbør
maks_nedbør <- max(datasæt$Nedbør_sum_mm_sæson[datasæt$Sæson == "sommer"], na.rm = TRUE)
datasæt <- datasæt %>%
  filter(
    !(Sæson == "sommer" &
        !is.na(Nedbør_sum_mm_sæson) &
        Nedbør_sum_mm_sæson == maks_nedbør)
  )

# Fjern minimal efterår temperatur
min_temp <- min(datasæt$Temperatur_gns_sæson[datasæt$Sæson == "efterår"], na.rm = TRUE)
datasæt <- datasæt %>%
  filter(
    !(Sæson == "efterår" &
        !is.na(Temperatur_gns_sæson) &
        Temperatur_gns_sæson == min_temp)
  ) # 16449 obs. of 38 variables


####################################################################################
# 3. Konstruktion af rumlige features:
####################################################################################

# ---- 3.1 Funktioner ----

# Funktion til rækkevis minimum, hvor alle-NA bevares som NA
række_min <- function(x) {
  apply(x, 1, function(z) if (all(is.na(z))) NA_real_ else min(z, na.rm = TRUE))
}

# Funktion til meter
til_meter_matrix <- function(x) {
  as.matrix(x) %>%
    set_units("m") %>%
    drop_units()
}

# ---- 3.2 Konstruktion ----

# Indlæs oplande
id15_oplande_sf <- st_read("ID15_oplande_raw.gpkg", quiet = TRUE) %>%
  mutate(Opland_id = as.integer(Opland_id))

datasæt <- datasæt %>%
  mutate(Opland_id = as.integer(Opland_id))

# Én stabil geografisk profil pr. station
stationer_sf <- datasæt %>%
  filter(
    !is.na(Stations_id),
    !is.na(Opland_id),
    !is.na(Hovedopland_id),
    !is.na(X_koordinat),
    !is.na(Y_koordinat)
  ) %>%
  count(
    Stations_id,
    Opland_id,
    Hovedopland_id,
    X_koordinat,
    Y_koordinat,
    name = "antal"
  ) %>%
  slice_max(antal, n = 1, by = Stations_id, with_ties = FALSE) %>%
  select(-antal) %>%
  st_as_sf(
    coords = c("X_koordinat", "Y_koordinat"),
    crs = 25832,
    remove = FALSE
  )

# Gns. nitrat pr. station (over hele perioden) 
station_mean <- datasæt %>%
  group_by(Stations_id) %>%
  summarise(
    nitrat_gns = mean(Nitratkoncentration_gennemsnit, na.rm = TRUE),
    .groups = "drop"
  )

# Oplande med hovedopland
oplande_med_hovedopland <- id15_oplande_sf %>%
  semi_join(datasæt %>% distinct(Opland_id), by = "Opland_id") %>%
  left_join(
    datasæt %>% distinct(Opland_id, Hovedopland_id),
    by = "Opland_id"
  ) %>%
  filter(!is.na(Hovedopland_id))

# Oplandscentre og hovedoplande
oplandscentre_sf <- oplande_med_hovedopland %>%
  st_centroid() %>%
  select(Opland_id, Hovedopland_id)

hovedoplande_sf <- oplande_med_hovedopland %>%
  group_by(Hovedopland_id) %>%
  summarise(.groups = "drop")

hovedoplandscentre_sf <- hovedoplande_sf %>%
  st_centroid() %>%
  select(Hovedopland_id)

# Afstand til eget oplandscentrum
stationer_sf$Afstand_til_eget_oplandscentrum_m <- as.numeric(
  st_distance(
    stationer_sf,
    oplandscentre_sf[match(stationer_sf$Opland_id, oplandscentre_sf$Opland_id), ],
    by_element = TRUE
  )
)

# Afstand til eget hovedoplandscentrum
stationer_sf$Afstand_til_eget_hovedoplandscentrum_m <- as.numeric(
  st_distance(
    stationer_sf,
    hovedoplandscentre_sf[
      match(stationer_sf$Hovedopland_id, hovedoplandscentre_sf$Hovedopland_id),
    ],
    by_element = TRUE
  )
)

# Afstand til nærmeste station i samme hovedopland
afstand_stationer <- til_meter_matrix(st_distance(stationer_sf))

samme_hovedopland <- outer(
  stationer_sf$Hovedopland_id,
  stationer_sf$Hovedopland_id,
  FUN = "=="
)

diag(samme_hovedopland) <- FALSE
afstand_stationer[!samme_hovedopland] <- NA_real_

stationer_sf$Afstand_til_nærmeste_station_i_hovedopland_m <- række_min(
  afstand_stationer
)

# Find index for nærmeste station (ikke NA) 
nærmeste_idx <- apply(afstand_stationer, 1, function(x) {
  if (all(is.na(x))) return(NA_integer_)
  which.min(x)
})
# Map Stations_id for nærmeste nabo
stationer_sf$Nærmeste_station_id <- stationer_sf$Stations_id[nærmeste_idx]
# Join gennemsnit fra nabo
stationer_sf <- stationer_sf %>%
  left_join(
    station_mean,
    by = c("Nærmeste_station_id" = "Stations_id")
  ) %>%
  rename(
    nærmeste_station_i_hovedopland_gennemsnit = nitrat_gns
  )

# Antal oplande inden for 30 km i samme hovedopland
afstand_oplande <- til_meter_matrix(st_distance(stationer_sf, oplandscentre_sf))

samme_hovedopland_oplande <- outer(
  stationer_sf$Hovedopland_id,
  oplandscentre_sf$Hovedopland_id,
  FUN = "=="
)

afstand_oplande[!samme_hovedopland_oplande] <- NA_real_

stationer_sf$Antal_oplande_indenfor_30km_i_hovedopland <- rowSums(
  !is.na(afstand_oplande) & afstand_oplande <= 30000
)

# Afstand til nærmeste andet hovedopland
afstand_hovedoplande <- til_meter_matrix(st_distance(stationer_sf, hovedoplande_sf))

samme_hovedopland_polygon <- outer(
  stationer_sf$Hovedopland_id,
  hovedoplande_sf$Hovedopland_id,
  FUN = "=="
)

afstand_hovedoplande[samme_hovedopland_polygon] <- NA_real_

stationer_sf$Afstand_til_nærmeste_andet_hovedopland_m <- række_min(
  afstand_hovedoplande
)

# Saml og join rumlige features
rumlige_features <- stationer_sf %>%
  st_drop_geometry() %>%
  select(
    Stations_id,
    Antal_oplande_indenfor_30km_i_hovedopland,
    Afstand_til_eget_oplandscentrum_m,
    Afstand_til_nærmeste_station_i_hovedopland_m,
    Afstand_til_nærmeste_andet_hovedopland_m,
    Afstand_til_eget_hovedoplandscentrum_m,
    nærmeste_station_i_hovedopland_gennemsnit 
  )

datasæt <- datasæt %>%
  left_join(rumlige_features, by = "Stations_id")

####################################################################################
# 4. Deskriptiv statistik for endeligt datasæt:
####################################################################################

# ---- 4.2 Map over gennemsnitlig nitrat i analyseperioden ---- 

# ---- 4.2.1 Hjælpefunktion: flyt Bornholm i kort ----

flyt_bornholm <- function(sf_objekt, x_flyt = -175000, y_flyt = 220000) {
  
  bornholm_bbox <- st_bbox(
    c(
      xmin = 850000,
      xmax = 930000,
      ymin = 6080000,
      ymax = 6160000
    ),
    crs = st_crs(sf_objekt)
  )
  
  er_bornholm <- lengths(
    st_intersects(sf_objekt, st_as_sfc(bornholm_bbox))
  ) > 0
  
  sf_flyttet <- sf_objekt
  
  st_geometry(sf_flyttet)[er_bornholm] <-
    st_geometry(sf_flyttet)[er_bornholm] + c(x_flyt, y_flyt)
  
  sf_flyttet
}

# Flyttet version af ID15-oplande
id15_oplande_sf_kort <- flyt_bornholm(id15_oplande_sf)

# Ydre kant laves EFTER Bornholm er flyttet
dk_kant_kort <- st_union(id15_oplande_sf_kort) %>%
  st_as_sf()

# Ramme omkring flyttet Bornholm
bornholm_ramme <- id15_oplande_sf_kort %>%
  filter(
    lengths(
      st_intersects(
        .,
        st_as_sfc(
          st_bbox(
            c(
              xmin = 675000,
              xmax = 735000,
              ymin = 6280000,
              ymax = 6365000
            ),
            crs = st_crs(id15_oplande_sf_kort)
          )
        )
      )
    ) > 0
  ) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf() %>%
  st_buffer(5000)

####################################################################################
# 5. Rumlig autokorrelation i data inden modelanalyse:
####################################################################################

# ---- 5.1 Moran's I funktioner ---- 

# Find Queen-naboer inden for samme hovedopland
lav_queen_indenfor_hovedopland <- function(sf_data) {
  nabo1 <- poly2nb(sf_data, queen = TRUE)
  hovedopland <- sf_data$Hovedopland_id
  
  nabo2 <- lapply(seq_along(nabo1), function(i) {
    naboer <- nabo1[[i]]
    if (length(naboer) == 1 && naboer[1] == 0) return(0L)
    
    naboer <- naboer[hovedopland[naboer] == hovedopland[i]]
    if (length(naboer) == 0) return(0L)
    naboer
  })
  
  class(nabo2) <- "nb"
  attr(nabo2, "region.id") <- as.character(sf_data$Opland_id)
  attr(nabo2, "type") <- "queen within hovedopland"
  attr(nabo2, "sym") <- FALSE
  
  nb2listw(nabo2, style = "W", zero.policy = TRUE)
}

# Beregn Moran's I med queen-naboer inden for hovedopland
beregn_moran_queen <- function(sf_data, variabel) {
  lav_rumlige_vægte <- lav_queen_indenfor_hovedopland(sf_data)
  moran.test(
    sf_data[[variabel]],
    lav_rumlige_vægte,
    zero.policy = TRUE
  )
}

# Beregn Moran's I ved forskellige distancer 
beregn_moran_distance_bands <- function(sf_data, variabel, bands = seq(5000, 95000, 10000)) {
  coords <- st_coordinates(st_centroid(sf_data))
  
  map_dfr(bands, function(d) {
    nabo1 <- dnearneigh(coords, d1 = 0, d2 = d)
    lav_rumlige_vægte <- nb2listw(nabo1, style = "W", zero.policy = TRUE)
    
    test <- moran.test(
      sf_data[[variabel]],
      lav_rumlige_vægte,
      zero.policy = TRUE
    )
    
    tibble(
      distance_m = d,
      moran_I = test$estimate[["Moran I statistic"]],
      p_value = test$p.value
    )
  })
}

# ---- 5.2 Moran's I på målvariablen ---- 

# Klargør oplande med gennemsnitlig nitrat
oplande_moran_sf <- id15_oplande_sf %>%
  mutate(Opland_id = as.integer(Opland_id)) %>%
  semi_join(datasæt %>% distinct(Opland_id), by = "Opland_id") %>%
  left_join(
    datasæt %>%
      mutate(Opland_id = as.integer(Opland_id)) %>%
      group_by(Opland_id) %>%
      summarise(
        Hovedopland_id = first(as.character(Hovedopland_id)),
        nitrat_pr_opland = mean(Nitratkoncentration_gennemsnit, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "Opland_id"
  ) %>%
  filter(!is.na(Hovedopland_id), !is.na(nitrat_pr_opland)) %>%
  st_make_valid()

moran_målvariabel <- beregn_moran_queen(oplande_moran_sf, "nitrat_pr_opland")

# ---- 5.3 Moran's I ved forskellige distancer ---- 

moran_pr_distance <- beregn_moran_distance_bands(
  oplande_moran_sf,
  "nitrat_pr_opland",
  bands = seq(5000, 135000, 10000) 
)
moran_pr_distance

# plot
ggplot(moran_pr_distance, aes(x = distance_m / 1000, y = moran_I)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Moran's I ved forskellige distancer",
    x = "Distance (km)",
    y = "Moran's I (Målvariabel)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

####################################################################################
# 6. Nested CV:
####################################################################################

# ---- 6.1 Split data i fire ydre geografiske folde baseret på hovedoplande ---- 

folde <- tibble::tribble(
  ~Hovedopland_id, ~ydre_fold, 
  "DK1.1",  1,
  "DK1.2",  1,
  "DK1.3",  1, 
  "DK1.4",  1, 
  
  "DK1.5",  2,
  "DK1.6",  2,
  "DK1.7",  2,
  "DK1.8",  2,
  "DK1.9",  2,
  
  "DK4.1",  3,
  "DK1.10", 3,
  "DK1.11", 3,
  "DK1.12", 3,
  "DK1.13", 3,
  "DK1.14", 3,
  "DK1.15", 3,
  
  "DK2.1",  4,
  "DK2.2",  4,
  "DK2.3",  4,
  "DK2.4",  4,
  "DK2.5",  4,
  "DK2.6",  4,
  "DK3.1",  4
)

datasæt_folde <- datasæt %>%
  left_join(folde, by = "Hovedopland_id")

# Tabel over andele i hvert fold
tabel_over_folde <- datasæt_folde %>%
  group_by(ydre_fold) %>%
  summarise(
    antal_stationer = n_distinct(Stations_id),
    antal_oplande = n_distinct(Opland_id),
    .groups = "drop"
  ) %>%
  mutate(
    andel_stationer_pct = round(antal_stationer / sum(antal_stationer) * 100, 1),
    andel_oplande_pct = round(antal_oplande / sum(antal_oplande) * 100, 1)
  )

# ---- 6.1.1 Visualiser de fire folde og deres oplande ---- 

# Klargør oplande med fold
fold_kort <- id15_oplande_sf_kort %>%
  left_join(
    datasæt %>%
      distinct(Opland_id, Hovedopland_id) %>%
      mutate(Opland_id = as.integer(Opland_id)) %>%
      left_join(folde, by = "Hovedopland_id"),
    by = "Opland_id"
  ) %>%
  filter(!is.na(ydre_fold))

# Funktion til at lave fold map
plot_fold <- function(fold_nr) {
  ggplot() +
    geom_sf(
      data = fold_kort %>%
        mutate(fold_farve = if_else(ydre_fold == fold_nr, "Valgt fold", "Øvrige folde")),
      aes(fill = fold_farve),
      color = NA
    ) +
    geom_sf(data = dk_kant_kort, fill = NA, color = "grey45", linewidth = 0.35) +
    geom_sf(data = bornholm_ramme, fill = NA, color = "black", linewidth = 0.4) +
    scale_fill_manual(
      values = c("Valgt fold" = "#E69F00", "Øvrige folde" = "#56B4E9"),
      guide = "none"
    ) +
    coord_sf(
      crs = 25832,
      xlim = c(430000, 820000),
      ylim = c(6040000, 6410000),
      datum = NA,
      expand = FALSE
    ) +
    labs(title = paste("Rumlig CV Fold", fold_nr)) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
    )
}

# 2x2 samlet figur
(plot_fold(1) + plot_fold(2)) /
  (plot_fold(3) + plot_fold(4))

# Gør join permanent
datasæt <- datasæt_folde

# ---- 6.2 Funktion til nested CV ---- 

lav_nested_split <- function(data, ydre_test_fold) {
  
  ydre_træning <- data %>%
    filter(ydre_fold != ydre_test_fold)
  
  ydre_test <- data %>%
    filter(ydre_fold == ydre_test_fold)
  
  indre_validerings_folde <- sort(unique(ydre_træning$ydre_fold))
  
  indre_cv_splits <- map(
    indre_validerings_folde,
    ~ make_splits(
      list(
        analysis   = which(ydre_træning$ydre_fold != .x),
        assessment = which(ydre_træning$ydre_fold == .x)
      ),
      data = ydre_træning
    )
  )
  
  indre_cv <- manual_rset(
    splits = indre_cv_splits,
    ids = paste0("indre_validerings_fold_", indre_validerings_folde)
  )
  
  list(
    ydre_træning = ydre_træning,
    ydre_test = ydre_test,
    indre_cv = indre_cv
  )
}

# ---- 6.3 Anvend recipes() til datapreprocessing ---- 

# Samle id variabler
id_variabler <- c(
  "Målested_x_koordinat", "Målested_y_koordinat",
  "Stations_id", "Opland_id", "Hovedopland_id", "ydre_fold"
)

recipe_træbaseret <- recipe(Nitratkoncentration_gennemsnit ~ ., data = datasæt) %>% 
  update_role(all_of(id_variabler), new_role = "id") %>%
  step_impute_knn(all_predictors(), neighbors = 6) %>%
  step_dummy(all_nominal_predictors())

recipe_svr <- recipe(Nitratkoncentration_gennemsnit ~ ., data = datasæt) %>%
  update_role(all_of(id_variabler), new_role = "id") %>%
  step_zv(all_predictors()) %>%
  step_impute_knn(all_predictors(), neighbors = 6) %>%
  step_YeoJohnson(all_numeric_predictors()) %>% # normaliser fordelinger
  step_normalize(all_numeric_predictors()) %>% # skaler og standardiser 
  step_dummy(all_nominal_predictors())


# ---- 6.4 Funktion til modeller og workflows ---- 

modeller <- list(
  rf = list(
    workflow = workflow() %>%
      add_recipe(recipe_træbaseret) %>%
      add_model(
        rand_forest(
          mtry = tune(),
          trees = 1000, 
          min_n = tune() 
        ) %>%
          set_engine("ranger", importance = "impurity") %>%
          set_mode("regression")
      ),
    grid = NULL
  ),
  
  xgb = list(
    workflow = workflow() %>%
      add_recipe(recipe_træbaseret) %>%
      add_model(
        boost_tree(
          trees = 3000, # 1000/3000 træer
          tree_depth = tune(),
          learn_rate = tune(),
          min_n = tune(),
          loss_reduction = tune(),
          sample_size = tune(),
          mtry = tune() 
        ) %>%
          set_engine("xgboost", 
                     lambda = tune(),
                     ) %>% 
          set_mode("regression")
      ),
    grid = NULL
  ),
  
  svr = list(
    workflow = workflow() %>%
      add_recipe(recipe_svr) %>%
      add_model(
        svm_rbf( 
          cost = tune(),
          rbf_sigma = tune(),
          margin = tune()
        ) %>%
          set_engine("kernlab") %>%
          set_mode("regression")
      ),
    grid = NULL
  )
)  

# ---- 6.5 Funktion til tuning i indre krydsvalidering og test på ydre fold ---- 

kør_model_i_ydre_fold <- function(modelnavn, modelinfo, data, ydre_test_fold) {
  
  # Start tidstagning
  start_tid <- Sys.time()
  cat("\nStarter", toupper(modelnavn), "- ydre fold", ydre_test_fold, "\n")
  
  # Split data i ydre træning/test og indre CV til tuning
  split_objekt <- lav_nested_split(data, ydre_test_fold)
  ydre_træning <- split_objekt$ydre_træning
  ydre_test <- split_objekt$ydre_test
  indre_cv <- split_objekt$indre_cv
  
  # Tune hyperparametre på de indre folde
  tune_resultat <- tune_grid(
    modelinfo$workflow,
    resamples = indre_cv,
    grid = modelinfo$grid,
    metrics = metric_set(rmse, rsq, mae),
    control = control_grid(save_pred = TRUE)
  )
  
  # Vælg bedste hyperparametre ud fra laveste RMSE
  bedste_parametre <- select_best(tune_resultat, metric = "rmse")
  
  # Finalisér workflow med bedste hyperparametre
  final_workflow <- finalize_workflow(
    modelinfo$workflow,
    bedste_parametre
  )
  
  # Træn endelig model på hele den ydre træningsdel
  final_fit <- fit(
    final_workflow,
    data = ydre_træning
  )
  
  # Lav prædiktioner på den ydre testfold
  alle_test_prædiktioner <- predict(final_fit, new_data = ydre_test) %>%
    bind_cols(
      ydre_test %>%
        select(
          Nitratkoncentration_gennemsnit,
          Stations_id,
          Opland_id,
          Hovedopland_id,
          Sæson,
          År,
          ydre_fold
        )
    ) %>%
    mutate(
      model = modelnavn,
      ydre_test_fold = ydre_test_fold
    )
  
  # Beregn performance på ydre testfold
  evalueringsmetrikker_resultat <- alle_test_prædiktioner %>%
    metrics(
      truth = Nitratkoncentration_gennemsnit,
      estimate = .pred
    ) %>%
    mutate(
      model = modelnavn,
      ydre_test_fold = ydre_test_fold
    )
  
  # Beregn performance på træningsdata til overfitting-tjek
  træning_predictioner <- predict(final_fit, new_data = ydre_træning) %>%
    bind_cols(
      ydre_træning %>%
        select(Nitratkoncentration_gennemsnit)
    )
  
  træning_metrics <- træning_predictioner %>%
    metrics(
      truth = Nitratkoncentration_gennemsnit,
      estimate = .pred
    ) %>%
    mutate(
      model = modelnavn,
      ydre_test_fold = ydre_test_fold
    )
  
  # Beregn køretid
  slut_tid <- Sys.time()
  
  køretid_min <- as.numeric(
    difftime(slut_tid, start_tid, units = "mins")
  )
  
  cat(
    "Færdig med", toupper(modelnavn),
    "- ydre fold", ydre_test_fold,
    "- tid:", round(køretid_min, 1), "minutter\n"
  )
  
  # Returner alle centrale resultater
  list(
    model = modelnavn,
    ydre_test_fold = ydre_test_fold,
    tune = tune_resultat,
    bedste_parametre = bedste_parametre,
    final_fit = final_fit,
    alle_test_prædiktioner = alle_test_prædiktioner,
    evalueringsmetrikker_resultat = evalueringsmetrikker_resultat,
    træning_metrics = træning_metrics,
    køretid_min = køretid_min
  )
}

# ---- 6.6 Bredt hyperparametre grid ---- 

ydre_folde <- sort(unique(datasæt$ydre_fold))

modeller_bredt_grid <- modeller

modeller_bredt_grid$rf$grid <- tidyr::crossing(
  mtry = c(1, 2, 5, 8, 12, 14, 18, 20), 
  min_n = c(1, 2, 4, 7, 10, 12, 16, 20) 
) 

modeller_bredt_grid$xgb$grid <- tidyr::crossing(
  tree_depth = c(2, 5, 8),
  learn_rate = c(0.01, 0.05, 0.1),
  min_n = c(2, 10),
  loss_reduction = c(0, 1),
  sample_size = c(0.7, 1.0),
  mtry = c(4, 10),
  lambda = c(1, 10)
) 

modeller_bredt_grid$svr$grid <- tidyr::crossing(
  cost = c(0.05, 0.1, 0.25, 0.50, 4, 16), 
  rbf_sigma = c(0.0001, 0.005, 0.01, 0.05),
  margin = c(0.01, 0.1, 0.5)
) 

# ---- 6.6.1 Kør bredt hyperparametre grid ---- 

nested_resultater_bredt <- tidyr::crossing(
  modelnavn = names(modeller_bredt_grid),
  ydre_test_fold = ydre_folde
) %>%
  mutate(
    resultat = map2(
      modelnavn,
      ydre_test_fold,
      ~ kør_model_i_ydre_fold(
        modelnavn = .x,
        modelinfo = modeller_bredt_grid[[.x]],
        data = datasæt,
        ydre_test_fold = .y
      )
    )
  )

# ---- 6.6.2 Analyser resultater fra bredt hyperparametre grid ---- 

tuning_resultater_bredt <- nested_resultater_bredt %>%
  mutate(
    tune = map(resultat, "tune"),
    tuning_metrics = map(tune, collect_metrics)
  ) %>%
  select(modelnavn, ydre_test_fold, tuning_metrics) %>%
  unnest(tuning_metrics) %>%
  filter(.metric == "rmse")

rf_top10_bredt <- tuning_resultater_bredt %>%
  filter(modelnavn == "rf") %>%
  group_by(mtry, min_n) %>%
  summarise(mean_rmse = mean(mean), sd_rmse = sd(mean), .groups = "drop") %>%
  arrange(mean_rmse) %>%
  slice_head(n = 10)

xgb_top10_bredt <- tuning_resultater_bredt %>%
  filter(modelnavn == "xgb") %>%
  group_by(tree_depth, learn_rate, min_n, loss_reduction, sample_size, mtry, lambda) %>%
  summarise(mean_rmse = mean(mean), sd_rmse = sd(mean), .groups = "drop") %>%
  arrange(mean_rmse) %>%
  slice_head(n = 10)

svr_top10_bredt <- tuning_resultater_bredt %>%
  filter(modelnavn == "svr") %>%
  group_by(cost, rbf_sigma, margin) %>%
  summarise(mean_rmse = mean(mean), sd_rmse = sd(mean), .groups = "drop") %>%
  arrange(mean_rmse) %>%
  slice_head(n = 10)

# ---- 6.7 Smalt hyperparametre grid ---- 

modeller_smalt_grid <- modeller 

modeller_smalt_grid$rf$grid <- tidyr::crossing(
  mtry = c(2, 3, 4, 5, 6, 7, 8, 9, 10),  
  min_n = c(1, 2, 4, 5, 7, 10, 12, 14, 16, 20) 
) 

modeller_smalt_grid$xgb$grid <- tidyr::crossing(
  tree_depth = c(5, 7, 8), 
  learn_rate = c(0.003, 0.005, 0.01), 
  min_n = c(2, 5), 
  loss_reduction = c(0, 0.5), 
  sample_size = c(0.7, 0.90), 
  mtry = c(4, 7), 
  lambda = c(10, 30) 
) 

modeller_smalt_grid$svr$grid <- tidyr::crossing(
  cost = c(0.15, 0.25, 0.35, 0.50, 1, 2, 4),  
  rbf_sigma = c(0.003, 0.005, 0.008, 0.01, 0.02), 
  margin = c(0.1, 0.3, 0.5) 
) 

# ---- 6.7.1 Kør smalt hyperparametre grid ---- 

nested_resultater_smalt <- tidyr::crossing(
  modelnavn = names(modeller_smalt_grid),
  ydre_test_fold = ydre_folde
) %>%
  mutate(
    resultat = map2(
      modelnavn,
      ydre_test_fold,
      ~ kør_model_i_ydre_fold(
        modelnavn = .x,
        modelinfo = modeller_smalt_grid[[.x]],
        data = datasæt,
        ydre_test_fold = .y
      )
    )
  )

# ---- 6.7.2 Analyser resultaterne fra smalt hyperparametre grid ---- 

tuning_resultater_smalt <- nested_resultater_smalt %>%
  mutate(
    tune = map(resultat, "tune"),
    tuning_metrics = map(tune, collect_metrics)
  ) %>%
  select(modelnavn, ydre_test_fold, tuning_metrics) %>%
  unnest(tuning_metrics) %>%
  filter(.metric == "rmse")

rf_top10_smalt <- tuning_resultater_smalt %>%
  filter(modelnavn == "rf") %>%
  group_by(mtry, min_n) %>%
  summarise(mean_rmse = mean(mean), sd_rmse = sd(mean), .groups = "drop") %>%
  arrange(mean_rmse) %>%
  slice_head(n = 10)

xgb_top10_smalt <- tuning_resultater_smalt %>%
  filter(modelnavn == "xgb") %>%
  group_by(tree_depth, learn_rate, min_n, loss_reduction, sample_size, mtry, lambda) %>%
  summarise(mean_rmse = mean(mean), sd_rmse = sd(mean), .groups = "drop") %>%
  arrange(mean_rmse) %>%
  slice_head(n = 10)

svr_top10_smalt <- tuning_resultater_smalt %>%
  filter(modelnavn == "svr") %>%
  group_by(cost, rbf_sigma, margin) %>%
  summarise(mean_rmse = mean(mean), sd_rmse = sd(mean), .groups = "drop") %>%
  arrange(mean_rmse) %>%
  slice_head(n = 10)

####################################################################################
# 7. Modelsammenligning:
####################################################################################

# ---- 7.1 Saml metrikker, træning, prædiktioner og køretid for alle modellerne ---- 

samlet_metrics_alle_folds <- nested_resultater_smalt %>%
  mutate(metrics = map(resultat, "evalueringsmetrikker_resultat")) %>%
  select(modelnavn, metrics) %>%
  unnest(metrics)

samlet_træning_metrics_alle_folds <- nested_resultater_smalt %>%
  mutate(træning_metrics = map(resultat, "træning_metrics")) %>%
  select(modelnavn, træning_metrics) %>%
  unnest(træning_metrics)

samlet_predictions_alle_folds <- nested_resultater_smalt %>%
  mutate(predictioner = map(resultat, "alle_test_prædiktioner")) %>%
  select(modelnavn, predictioner) %>%
  unnest(predictioner)

samlet_køretid <- nested_resultater_smalt %>%
  mutate(køretid_min = map_dbl(resultat, "køretid_min")) %>%
  group_by(modelnavn) %>%
  summarise(
    total_tid_min = sum(køretid_min),
    gennemsnit_tid_pr_fold_min = mean(køretid_min),
    .groups = "drop"
  )

# ---- 7.2 Bedste hyperparametre kombination for hver model ---- 

vis_top_model <- function(data, model, parametre, nr = 1) {
  data %>%
    filter(modelnavn == model) %>%
    group_by(across(all_of(parametre))) %>%
    summarise(
      mean_rmse = mean(mean),
      sd_rmse = sd(mean),
      .groups = "drop"
    ) %>%
    arrange(mean_rmse, sd_rmse) %>%
    slice(nr)
}
vis_top_model(tuning_resultater_smalt, "rf",  c("mtry", "min_n"), nr = 1)
vis_top_model(tuning_resultater_smalt, "xgb", c("tree_depth", "learn_rate", "min_n", "loss_reduction", "sample_size", "mtry", "lambda"), nr = 1)
vis_top_model(tuning_resultater_smalt, "svr", c("cost", "rbf_sigma", "margin"), nr = 1)

# ---- 7.3 Samlet modelperformance med den bedste hyperparametre kombination for hver model ---- 

lav_performance_tabel <- function(data) {
  data %>%
    group_by(model, .metric) %>%
    summarise(
      resultat = paste0(
        round(mean(.estimate), 2),
        " ± ",
        round(sd(.estimate), 2)
      ),
      .groups = "drop"
    ) %>%
    mutate(
      model = toupper(model),
      .metric = recode(
        .metric,
        rmse = "RMSE",
        mae = "MAE",
        rsq = "R2"
      )
    ) %>%
    pivot_wider(
      names_from = .metric,
      values_from = resultat
    ) %>%
    arrange(RMSE)
}

test_performance_tabel <- lav_performance_tabel(
  samlet_metrics_alle_folds
)

træning_performance_tabel <- lav_performance_tabel(
  samlet_træning_metrics_alle_folds
)

test_performance_tabel

træning_performance_tabel

# ---- 7.4 Gem resultater fra smalt grid ----

best_params_smalt <- nested_resultater_smalt %>%
  mutate(bedste_parametre = map(resultat, "bedste_parametre")) %>%
  select(modelnavn, ydre_test_fold, bedste_parametre)

dir.create("resultater", showWarnings = FALSE)

saveRDS(
  list(
    predictions = samlet_predictions_alle_folds,
    test_metrics = samlet_metrics_alle_folds,
    train_metrics = samlet_træning_metrics_alle_folds,
    tuning_metrics = tuning_resultater_smalt,
    best_params = best_params_smalt,
    runtime = samlet_køretid,
    rf_top10 = rf_top10_smalt,
    xgb_top10 = xgb_top10_smalt,
    svr_top10 = svr_top10_smalt,
    test_performance = test_performance_tabel,
    train_performance = træning_performance_tabel
  ),
  "resultater/nested_cv_smalt_resultater.rds"
)

 # ---- 7.5 Boxplot over modelperformance på tværs af ydre fold ---- 

samlet_metrics_alle_folds %>%
  filter(.metric == "rmse") %>%
  ggplot(aes(x = model, y = .estimate)) +
  
  geom_boxplot(
    fill = "white",
    color = "black",
    alpha = 0.7,
    width = 0.6,
    outlier.shape = NA
  ) +
  
  geom_jitter(
    aes(color = factor(ydre_test_fold)),
    width = 0.08, 
    size = 3.5
  ) +
  
  scale_color_manual(
    values = c(
      "1" = "black",
      "2" = "grey35",
      "3" = "grey60",
      "4" = "grey85"
    ),
    labels = c(
      "1" = "CV fold 1",
      "2" = "CV fold 2",
      "3" = "CV fold 3",
      "4" = "CV fold 4"
    )
  ) +
  
  labs(
    title = "RMSE pr. model på tværs af ydre folds",
    x = "Model",
    y = "RMSE",
    color = "Fold"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

# ---- 7.5 Køretid for modellerne ----

samlet_køretid


####################################################################################
# 8. Feature importance på tvære af fold:
####################################################################################

# ---- 8.1 Vælg den bedste model ---- 

bedste_model <- "xgb" 

# Udtræk resultater for bedste model
bedste_resultater <- nested_resultater_smalt %>%
  filter(modelnavn == bedste_model)

bedste_predictions <- samlet_predictions_alle_folds %>%
  filter(model == bedste_model)

# ---- 8.2 Funktion til at beregne importance for en ydre fold ---- 

beregn_permutation_importance <- function(resultat_objekt, data) {
  
  # Udtræk modelnavn, testfold og den trænede model
  modelnavn <- resultat_objekt$model
  ydre_test_fold <- resultat_objekt$ydre_test_fold
  final_fit <- resultat_objekt$final_fit
  
  # Vælg testdata for den aktuelle ydre fold
  testdata <- data %>%
    filter(ydre_fold == ydre_test_fold)
  
  # Find prædiktorerne, dvs. fjern target og ID-variabler
  predictors <- testdata %>%
    select(-Nitratkoncentration_gennemsnit, -any_of(id_variabler)) %>%
    names()
  
  # Beregn modellens oprindelige RMSE på testdata
  baseline_pred <- predict(final_fit, new_data = testdata)$.pred
  
  baseline_rmse <- rmse_vec(
    truth = testdata$Nitratkoncentration_gennemsnit,
    estimate = baseline_pred
  )
  
  # Permutér én variabel ad gangen og mål hvor meget RMSE stiger
  map_dfr(predictors, function(var) {
    
    testdata_perm <- testdata
    testdata_perm[[var]] <- sample(testdata_perm[[var]])
    
    perm_pred <- predict(final_fit, new_data = testdata_perm)$.pred
    
    perm_rmse <- rmse_vec(
      truth = testdata_perm$Nitratkoncentration_gennemsnit,
      estimate = perm_pred
    )
    
    tibble(
      model = modelnavn,
      ydre_test_fold = ydre_test_fold,
      variabel = var,
      importance = perm_rmse - baseline_rmse
    )
  })
}

# Beregn permutation importance for bedste model på alle ydre folds
feature_importance <- bedste_resultater %>%
  mutate(
    importance = map(
      resultat,
      ~ beregn_permutation_importance(.x, datasæt)
    )
  ) %>%
  select(importance) %>%
  unnest(importance)

# Find de 15 vigtigste variabler målt på gennemsnitlig importance
top_variabler <- feature_importance %>%
  group_by(variabel) %>%
  summarise(
    gennemsnit_importance = mean(importance),
    .groups = "drop"
  ) %>%
  slice_max(gennemsnit_importance, n = 15) %>% 
  pull(variabel)

# Klargør data til plot
feature_importance_plotdata <- feature_importance %>%
  filter(variabel %in% top_variabler) %>%
  group_by(variabel) %>%
  mutate(
    gennemsnit_importance = mean(importance)
  ) %>%
  ungroup() %>%
  mutate(
    variabel = forcats::fct_reorder(variabel, gennemsnit_importance, .desc = TRUE),
    ydre_test_fold = factor(
      ydre_test_fold,
      levels = c(1, 2, 3, 4),
      labels = c("CV fold 1", "CV fold 2", "CV fold 3", "CV fold 4")
    )
  )

# Plot feature importance fordelt på ydre folds
ggplot(
  feature_importance_plotdata,
  aes(
    x = variabel,
    y = importance,
    fill = ydre_test_fold
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  scale_x_discrete(
    labels = \(x) stringr::str_trunc(x, width = 45) 
  ) +
  scale_fill_manual(
    values = c(
      "CV fold 1" = "black",
      "CV fold 2" = "grey35",
      "CV fold 3" = "grey60",
      "CV fold 4" = "grey85"
    )
  ) +
  labs(
    title = paste("Feature importance -", toupper(bedste_model)),
    x = NULL,
    y = "Stigning i RMSE ved permutation",
    fill = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(
      angle = 80,
      hjust = 1,
      size = 9
    ),
    axis.text.y = element_text(size = 11),
    legend.position = c(0.82, 0.82),
    legend.background = element_rect(
      fill = "white",
      color = "grey80"
    ),
    plot.title = element_text(
      face = "bold",
      hjust = 0
    )
  )

####################################################################################
# 9. Rumlig autokorrelation i modelresidualer:
####################################################################################

# ---- 9.1 Moran's I på bedste model ---- 
residualer_opland <- bedste_predictions %>%
  mutate(
    Opland_id = as.integer(Opland_id),
    residual = Nitratkoncentration_gennemsnit - .pred
  ) %>%
  group_by(Opland_id) %>%
  summarise(
    residual_opland = mean(residual, na.rm = TRUE),
    .groups = "drop"
  )

residualer_sf <- id15_oplande_sf %>%
  mutate(Opland_id = as.integer(Opland_id)) %>%
  left_join(
    datasæt %>%
      mutate(Opland_id = as.integer(Opland_id)) %>%
      distinct(Opland_id, Hovedopland_id),
    by = "Opland_id"
  ) %>%
  left_join(residualer_opland, by = "Opland_id") %>%
  filter(!is.na(Hovedopland_id), !is.na(residual_opland)) %>%
  st_make_valid()

moran_residualer <- beregn_moran_queen(residualer_sf, "residual_opland")

# ---- 9.2 Moran's I ved forskellige distancer ---- 

moran_residualer_distance <- beregn_moran_distance_bands(
  residualer_sf,
  "residual_opland",
  bands = seq(5000, 185000, 10000)
)

moran_residualer_distance

# plot
ggplot(moran_residualer_distance, aes(x = distance_m / 1000, y = moran_I)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = paste("Moran's I på residualer ved forskellige distancer -", toupper(bedste_model)),
    x = "Distance (km)",
    y = "Moran's I (XGBoost residualer)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

####################################################################################
# 10. Anvendelse af modellen:
####################################################################################

# ---- 10.1 Scatterplot over observerede versus prædikterede på målestationsniveau ---- 

akse_max <- max(
  bedste_predictions$Nitratkoncentration_gennemsnit,
  bedste_predictions$.pred,
  na.rm = TRUE
)

ggplot(
  bedste_predictions,
  aes(
    x = Nitratkoncentration_gennemsnit,
    y = .pred
  )
) +
  geom_point(
    color = "black",
    alpha = 0.7,
    size = 1.1
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "red",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  coord_equal(
    xlim = c(0, akse_max),
    ylim = c(0, akse_max),
    expand = FALSE
  ) +
  labs(
    title = paste(
      "Observeret versus prædikteret nitrat -",
      toupper(bedste_model)
    ),
    x = "Observeret nitrat (mg/L)",
    y = "Prædikteret nitrat (mg/L)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "plain",
      hjust = 0
    ),
    axis.line = element_line(linewidth = 0.5)
  )

# ---- 10.4 Beregn DI/AOA ---- 

# Øvre whisker bruges som AOA-tærskel:
# Q3 + 1.5 * IQR, men returnerer højeste DI-værdi inden for grænsen
øvre_whisker <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  
  grænse <- quantile(x, 0.75, na.rm = TRUE) + 1.5 * IQR(x, na.rm = TRUE)
  max(x[x <= grænse], na.rm = TRUE)
}

# Beregner feature vægte ud fra permutation importance
lav_vægte <- function(predictor_navne, importance_df) {
  
  importance_mean <- importance_df %>%
    group_by(variabel) %>%
    summarise(importance = mean(importance, na.rm = TRUE), .groups = "drop") %>%
    mutate(importance = if_else(is.na(importance) | importance < 0, 0, importance))
  
  vægte <- map_dbl(predictor_navne, function(x) {
    
    exact <- importance_mean %>%
      filter(variabel == x) %>%
      pull(importance)
    
    if (length(exact) == 1) return(exact)
    
    dummy_match <- importance_mean %>%
      filter(startsWith(x, paste0(variabel, "_"))) %>%
      arrange(desc(nchar(variabel))) %>%
      slice(1) %>%
      pull(importance)
    
    if (length(dummy_match) == 1) return(dummy_match)
    
    1
  })
  
  if (all(vægte == 0, na.rm = TRUE) || all(is.na(vægte))) {
    vægte <- rep(1, length(predictor_navne))
  }
  
  vægte / mean(vægte, na.rm = TRUE)
}

# Bake prædiktorer fra workflowets recipe
bake_predictors <- function(final_fit, new_data) {
  workflows::extract_recipe(final_fit, estimated = TRUE) %>%
    bake(new_data = new_data, all_predictors()) %>%
    select(where(is.numeric))
}

# Sikrer at træning og test har samme brugbare numeriske kolonner
rens_predictors <- function(træning_x, test_x) {
  
  fælles <- intersect(names(træning_x), names(test_x))
  
  træning_x <- træning_x %>% select(all_of(fælles))
  test_x <- test_x %>% select(all_of(fælles))
  
  gode_kolonner <- map_lgl(fælles, ~
                             all(is.finite(træning_x[[.x]])) &&
                             all(is.finite(test_x[[.x]])) &&
                             sd(træning_x[[.x]], na.rm = TRUE) > 0
  )
  
  list(
    træning = træning_x %>% select(all_of(fælles[gode_kolonner])),
    test = test_x %>% select(all_of(fælles[gode_kolonner]))
  )
}

# Z-score standardisering og vægtning med feature importance
standardiser_og_vægt <- function(træning_x, test_x, vægte) {
  
  mu <- map_dbl(træning_x, mean, na.rm = TRUE)
  sigma <- map_dbl(træning_x, sd, na.rm = TRUE)
  sigma[sigma == 0 | is.na(sigma)] <- 1
  
  list(
    træning = sweep(sweep(sweep(as.matrix(træning_x), 2, mu, "-"), 2, sigma, "/"), 2, vægte, "*"),
    test = sweep(sweep(sweep(as.matrix(test_x), 2, mu, "-"), 2, sigma, "/"), 2, vægte, "*")
  )
}

# Mindste euklidiske afstand fra hver testobservation til træningsdata
min_afstand_til_træning <- function(test_mat, træning_mat) {
  map_dbl(seq_len(nrow(test_mat)), function(i) {
    afstande <- sqrt(rowSums(sweep(træning_mat, 2, test_mat[i, ], "-")^2))
    min(afstande[is.finite(afstande)], na.rm = TRUE)
  })
}

# Beregner DI og AOA for én ydre fold
beregn_di_aoa_fold <- function(resultat_objekt, data, importance_df) {
  
  ydre_test_fold <- resultat_objekt$ydre_test_fold
  final_fit <- resultat_objekt$final_fit
  
  ydre_træning <- data %>% filter(ydre_fold != ydre_test_fold)
  ydre_test <- data %>% filter(ydre_fold == ydre_test_fold)
  
  renset <- rens_predictors(
    træning_x = bake_predictors(final_fit, ydre_træning),
    test_x = bake_predictors(final_fit, ydre_test)
  )
  
  if (ncol(renset$træning) == 0) {
    stop("Ingen brugbare predictor-kolonner tilbage efter rensning.")
  }
  
  vægte <- lav_vægte(
    predictor_navne = names(renset$træning),
    importance_df = importance_df
  )
  
  vægtet <- standardiser_og_vægt(
    træning_x = renset$træning,
    test_x = renset$test,
    vægte = vægte
  )
  
  træning_mat <- vægtet$træning
  test_mat <- vægtet$test
  
  # Gennemsnitlig afstand mellem alle træningsobservationer
  gennemsnit_afstand_træning <- mean(dist(træning_mat))
  
  # DI-tærskel beregnes på træningsdata via afstand til nærmeste observation i anden fold
  træning_folde <- ydre_træning$ydre_fold
  
  di_træning_cv <- map_dbl(seq_len(nrow(træning_mat)), function(i) {
    
    kandidater <- which(træning_folde != træning_folde[i])
    
    afstande <- sqrt(
      rowSums(
        sweep(træning_mat[kandidater, , drop = FALSE], 2, træning_mat[i, ], "-")^2
      )
    )
    
    min(afstande[is.finite(afstande)], na.rm = TRUE) / gennemsnit_afstand_træning
  })
  
  DI_tærskel <- øvre_whisker(di_træning_cv)
  
  # DI for testobservationer
  DI <- min_afstand_til_træning(test_mat, træning_mat) / gennemsnit_afstand_træning
  
  ydre_test %>%
    select(
      Nitratkoncentration_gennemsnit,
      Stations_id,
      Opland_id,
      Hovedopland_id,
      Sæson,
      År,
      ydre_fold
    ) %>%
    mutate(
      model = resultat_objekt$model,
      ydre_test_fold = ydre_test_fold,
      DI = DI,
      DI_tærskel = DI_tærskel,
      AOA = DI <= DI_tærskel
    )
}

# Beregn DI/AOA for bedste model på alle ydre folds
di_aoa_resultater <- bedste_resultater %>%
  mutate(
    di_aoa = map(
      resultat,
      ~ beregn_di_aoa_fold(
        resultat_objekt = .x,
        data = datasæt,
        importance_df = feature_importance
      )
    )
  ) %>%
  select(di_aoa) %>%
  unnest(di_aoa)

# ---- 10.5 DI: Kort opsummering ----

# Kort opsummering af DI og andel indenfor/udenfor AOA på observationsniveau
di_aoa_opsummering <- di_aoa_resultater %>%
  summarise(
    gennemsnit_DI = mean(DI, na.rm = TRUE),
    median_DI = median(DI, na.rm = TRUE), 
    min_DI = min(DI, na.rm = TRUE),
    max_DI = max(DI, na.rm = TRUE),
    gennemsnit_tærskel = mean(DI_tærskel, na.rm = TRUE),
    andel_indenfor_AOA = mean(AOA, na.rm = TRUE),
    andel_udenfor_AOA = mean(!AOA, na.rm = TRUE)
  )
di_aoa_opsummering

# ---- 10.6 AOA kort ----

# Saml DI/AOA på oplandsniveau så kort viser hvor stor en andel af observationerne i hvert opland der ligger indenfor AOA.
di_aoa_opland <- di_aoa_resultater %>%
  mutate(Opland_id = as.integer(Opland_id)) %>%
  group_by(Opland_id) %>%
  summarise(
    DI_gennemsnit = mean(DI, na.rm = TRUE),
    DI_maks = max(DI, na.rm = TRUE),
    AOA_andel = mean(AOA, na.rm = TRUE),
    AOA_opland = AOA_andel >= 0.5, 
    .groups = "drop"
  )

# Join til geometri
kort_di_aoa <- id15_oplande_sf_kort %>%
  mutate(Opland_id = as.integer(Opland_id)) %>%
  left_join(di_aoa_opland, by = "Opland_id") %>%
  filter(!is.na(DI_gennemsnit))

# DI-kort: høj DI betyder, at oplandet er mere forskelligt fra træningsdata
kort_DI <- ggplot(kort_di_aoa) +
  geom_sf(aes(fill = DI_gennemsnit), color = NA) +
  geom_sf(data = dk_kant_kort, fill = NA, color = "grey35", linewidth = 0.3) +
  geom_sf(data = bornholm_ramme, fill = NA, color = "black", linewidth = 0.4) +
  scale_fill_gradientn(
    colours = c("#EAF4F4", "#2A9D8F", "#073B3A"), 
    name = "DI",
    na.value = "grey90"
  ) +
  coord_sf(
    crs = 25832,
    xlim = c(430000, 820000),
    ylim = c(6040000, 6410000),
    datum = NA,
    expand = FALSE
  ) +
  labs(
    title = paste("Dissimilarity Index på ID15-oplande -", toupper(bedste_model))
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.text = element_text(size = 10)
  )
kort_DI
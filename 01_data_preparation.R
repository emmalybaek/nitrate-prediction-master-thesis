# Data preparation and integration

# This script constructs the modelling dataset used in the thesis. It combines nitrate measurements with spatial, hydrological,
# meteorological, agricultural and wastewater-related data.

# Main steps:
# 1. Clean and aggregate nitrate measurements
# 2. Link monitoring stations to catchments
# 3. Integrate external environmental data sources
# 4. Construct the final modelling dataset

# The script is in danish and has been cleaned for portfolio purposes and only includes the core steps from the final thesis workflow.

# Biblioteker
library(readxl)
library(dplyr)
library(sf)
library(mapview)
library(readr)
library(stringr)
library(tidyr)
library(httr)
library(jsonlite)
library(purrr)
library(lubridate)
library(igraph)
library(arrow)
library(units)

#___________________________________________________________________________________________________________________________
# Målinger af kvælstof (Nitrit+Nitrat-N)
# Link: https://kemidata.miljoeportal.dk/
# Output: target på sæsonniveau (2016-2023)
# Meteorologiske sæsoner:
# vinter = dec (forrige år) + jan + feb
# forår  = mar + apr + maj
# sommer = jun + jul + aug
# efterår = sep + okt + nov
#___________________________________________________________________________________________________________________________

# Excel-filer med rådata
filer <- c(
  "response_2016_2019.xlsx",
  "response_2020.xlsx",
  "response_2021_2023.xlsx"
)

# Koordinatkolonner der skal renses
koordinat_kolonner <- c(
  "x-koordinat",
  "y-koordinat",
  "Målested, x-koordinat",
  "Målested, y-koordinat"
)

# Funktion til rensning af koordinater
rens_koordinat <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  x <- gsub(",", ".", x, fixed = TRUE)
  as.numeric(x)
}

# Funktion til indlæsning af én fil
indlæs_målvariabel_fil <- function(file) {
  read_excel(file) %>%
    mutate(
      across(all_of(koordinat_kolonner), rens_koordinat)
    )
}

# Indlæs og saml alle filer
målvariabel_rå <- map_dfr(filer, indlæs_målvariabel_fil) %>%
  
  # Fjern observationer uden hovedkoordinater
  filter(
    !is.na(`x-koordinat`),
    !is.na(`y-koordinat`)
  ) # 63471 obs. of 35 variables

# Klargør datasæt og standardiser kolonnenavne
målvariabel <- målvariabel_rå %>%
  rename(
    Stedtype             = "﻿Stedtype",
    Målested_navn        = `Målested navn`,
    X_koordinat          = `x-koordinat`,
    Y_koordinat          = `y-koordinat`,
    Målested_x_koordinat = `Målested, x-koordinat`,
    Målested_y_koordinat = `Målested, y-koordinat`,
    Målested_GeoZone     = `Målested GeoZone`
  ) %>%
  
  # Opret dato- og sæsonvariable
  mutate(
    Dato = as.Date(Dato),
    År = year(Dato),
    Måned = month(Dato),
    
    Sæson = case_when(
      Måned %in% c(12, 1, 2)  ~ "vinter",
      Måned %in% c(3, 4, 5)   ~ "forår",
      Måned %in% c(6, 7, 8)   ~ "sommer",
      Måned %in% c(9, 10, 11) ~ "efterår"
    ),
    
    # December tilhører næste års vinter
    Sæson_år = if_else(Måned == 12L, År + 1L, År),
    
    # Numerisk sæsonrækkefølge
    Sæson_nr = case_when(
      Sæson == "vinter"  ~ 1L,
      Sæson == "forår"   ~ 2L,
      Sæson == "sommer"  ~ 3L,
      Sæson == "efterår" ~ 4L
    ),
    
    # Standardiser datatyper
    StedID = as.character(StedID),
    Resultat = as.numeric(Resultat),
    Prøvetagning = as.integer(Prøvetagning),
    Prøve = as.integer(Prøve),
    Delprøve = as.integer(Delprøve),
    GeoZone = as.character(GeoZone),
    Målested_GeoZone = as.character(Målested_GeoZone),
    År = as.integer(År),
    Måned = as.integer(Måned),
    Sæson_år = as.integer(Sæson_år),
    Sæson_nr = as.integer(Sæson_nr),
    
    # Gem originalt StedID
    StedID_original = StedID
  ) %>%
  
  # Afgræns analyseperiode
  filter(
    Sæson_år >= 2016,
    Sæson_år <= 2023,
    !(Sæson_år == 2016 & Sæson == "vinter")
  ) # 62537 obs. of 41 variables

# Opret stabilt stations_id baseret på fælles målestedskoordinater
stedid_stabil_map <- målvariabel %>%
  filter(
    !is.na(Målested_x_koordinat),
    !is.na(Målested_y_koordinat)
  ) %>%
  distinct(StedID, Målested_x_koordinat, Målested_y_koordinat) %>%
  group_by(Målested_x_koordinat, Målested_y_koordinat) %>%
  summarise(
    Stations_id = min(StedID),
    .groups = "drop"
  )

# Tilføj stabilt stations_id
målvariabel <- målvariabel %>%
  left_join(
    stedid_stabil_map,
    by = c("Målested_x_koordinat", "Målested_y_koordinat")
  ) %>%
  mutate(
    Stations_id = coalesce(Stations_id, StedID)
  )


# Metadata der bevares gennem aggregeringer
id_kolonner <- c(
  "Kommune", "Region", "Medie", "Vandområde", "Stedtype",
  "Stedtekst", "Referencer", "GeoZone",
  "X_koordinat", "Y_koordinat",
  "Målested_navn", "Målested_x_koordinat",
  "Målested_y_koordinat", "Målested_GeoZone",
  "Undersøgelsestype", "Analysefraktion"
)

# Saml delprøver til ét event
målvariabel_event <- målvariabel %>%
  group_by(
    Stations_id, Dato, Sæson_år, Sæson,
    Sæson_nr, Prøvetagning, Prøve
  ) %>%
  summarise(
    across(all_of(id_kolonner), first),
    Resultat_event = mean(Resultat, na.rm = TRUE),
    .groups = "drop"
  ) # 62342 obs. of 24 variables

# Saml flere målinger samme dag
målvariabel_dag <- målvariabel_event %>%
  group_by(
    Stations_id, Dato, Sæson_år,
    Sæson, Sæson_nr
  ) %>%
  summarise(
    across(all_of(id_kolonner), first),
    Resultat_dag = median(Resultat_event, na.rm = TRUE),
    .groups = "drop"
  ) # 60764 obs. of 22 variables

# Aggregér til sæsonniveau
målvariabel <- målvariabel_dag %>%
  group_by(
    Stations_id,
    Sæson_år,
    Sæson,
    Sæson_nr
  ) %>%
  summarise(
    across(all_of(id_kolonner), first),
    Nitratkoncentration_gennemsnit = mean(Resultat_dag, na.rm = TRUE),
    Antal_obs_pr_sæson    = sum(!is.na(Resultat_dag)),
    .groups = "drop"
  ) %>%
  mutate(
    Stations_id = as.character(Stations_id),
    År = as.integer(Sæson_år)
  ) %>% 
  arrange(Stations_id, År, Sæson_nr) # 16680 obs. of 23 variables

# Masterdatasæt starter med målvariablen 
data_master <- målvariabel

#___________________________________________________________________________________________________________________________
# Oplande (vandskel) - Kortlægning og geodata - Arealdata 
# Link: https://arealdata.miljoeportal.dk/datasets/urn:dmp:ds:oplande-vandskel
#___________________________________________________________________________________________________________________________

# Indlæs Oplands id 
id15_oplande_sf <- st_read("ID15_oplande_raw.gpkg", quiet = TRUE) %>%
  mutate(
    Opland_id = as.integer(Opland_id)
  ) # 3137 obs. of 5 variables


# Lav sf-punkter midlertidigt kun til oplandskobling
målvariabel_sf <- målvariabel %>%
  filter(
    !is.na(Målested_x_koordinat),
    !is.na(Målested_y_koordinat)
  ) %>%
  st_as_sf(
    coords = c("Målested_x_koordinat", "Målested_y_koordinat"),
    crs = 25832,
    remove = FALSE
  )

# Spatial join: punkt -> opland
målvariabel_opland <- målvariabel_sf %>%
  st_join(
    id15_oplande_sf %>% select(Opland_id),
    join = st_within,
    left = TRUE
  )

# Stabilt opland pr. station = mest hyppige Opland_id inden for Stations_id
opland_stabil <- målvariabel_opland %>%
  st_drop_geometry() %>%
  filter(!is.na(Opland_id)) %>%
  group_by(Stations_id, Opland_id) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(Stations_id, desc(n), Opland_id) %>%
  group_by(Stations_id) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  transmute(
    Stations_id = as.character(Stations_id),
    Opland_id = as.integer(Opland_id)
  ) # 1493 obs. of 2 variables. 1493 målestationer får tildelt et stabilt opland_id. Det vil sige, 6 målestationer rammer ikke et opland med st_join.

# Tilføj stabilt Opland_id til målvariablen/masterdatasæt
målvariabel <- målvariabel %>%
  left_join(opland_stabil, by = "Stations_id") %>%
  mutate(
    Stations_id = as.character(Stations_id),
    Opland_id = as.integer(Opland_id),
    År = as.integer(År),
    Sæson_år = as.integer(Sæson_år),
    Sæson_nr = as.integer(Sæson_nr)
  )

data_master <- målvariabel # 16680 obs. of 24 variables

# Rækker uden Opland_id fjernes fra analysen
data_master <- data_master %>%
  filter(!is.na(Opland_id)) # data_master består nu af 16639 obs. of 24 variables


#___________________________________________________________________________________________________________________________
# Kvælstofretention, version 2020 - Geologisk undergrund og råstofindvending - Arealdata 
# Link: https://arealdata.miljoeportal.dk/datasets/urn:dmp:ds:kvaelstofretention
#___________________________________________________________________________________________________________________________

retention <- st_read("Kvaelstofretention_2020_raw.gpkg", quiet = TRUE) # 3351 obs. of 10 variables

# Udtræk retentionstabellen uden geometri og omdøb centrale variable
retention_tabel <- retention %>%
  st_drop_geometry() %>%
  transmute(
    Opland_id = as.integer(id),
    Retention_total = redtot,
    Retention_overfladevand = redsurf,
    Retention_grundvand = redgw
  ) # 3351 obs. of 4 variables

# Tilføj retention til masterdatasæt
data_master <- data_master %>%
  left_join(retention_tabel, by = "Opland_id") # data_master består nu af 16639 obs. of 27 variables

#___________________________________________________________________________________________________________________________
# Markudledningskort 2025 - Landbrug og fiskeri - Arealdata 
# Link: https://arealdata.miljoeportal.dk/datasets/urn:dmp:ds:markudledningskort-2025
#___________________________________________________________________________________________________________________________

# Indlæs markudledningskort og transformer til samme koordinatsystem som oplande
markudledning <- st_read("Markudledningskort_2025.gpkg", quiet = TRUE) %>%
  st_transform(st_crs(id15_oplande_sf)) %>%
  mutate(
    mark_id = paste(CVR, Journalnr, Marknr, sep = "_"),
    Mark_areal_registreret = as.numeric(Indtareal),
    Markudledningsklasse = as.numeric(Markudledningsklasse)
  ) %>%
  select(mark_id, Mark_areal_registreret, Markudledningsklasse) # 611554 obs. of 4 variables

# Ret eventuelle ugyldige geometrier før rumlig overlap
markudledning <- st_make_valid(markudledning) # 611554 obs. of 4 variables

# Behold kun oplands-id til rumlig kobling
oplande_mark <- id15_oplande_sf %>%
  select(Opland_id)

# klip markpolygoner mod oplande 
mark_opland_klip <- st_intersection(markudledning, oplande_mark) %>%
  mutate(
    Areal_klippet_m2 = as.numeric(st_area(.))
  ) # 702968 obs. of 6 variables. Marker der ligger mellem to oplande, bliver til 2 rækker. Derfor er der nu 702968 mark oplands dele.

# Aggrerer markdata til oplandsniveau
mark_opland_aggregeret <- mark_opland_klip %>%
  st_drop_geometry() %>%
  group_by(Opland_id) %>%
  summarise(
    Antal_marker = n_distinct(mark_id),
    Mark_areal_total_m2 = sum(Areal_klippet_m2, na.rm = TRUE),
    Mark_kvælstofbelastning_arealvægtet =
      sum(Markudledningsklasse * Areal_klippet_m2, na.rm = TRUE) /
      sum(Areal_klippet_m2, na.rm = TRUE),
    .groups = "drop"
  ) # 3059 obs. of 4 variables. Der er 3059 oplande med markareal i markudledningskortet. Dem der matcher med oplande med vandløbsdata tilføjes.

# Tilføj oplandsaggregerede markdata til masterdatasættet
data_master <- data_master %>%
  left_join(mark_opland_aggregeret, by = "Opland_id") 

#___________________________________________________________________________________________________________________________
# VP3 - Renseanlæg brugt i Vandområdeplaner 2021-2027 - Naturbeskyttelse -  Arealdata 
# Link: https://arealdata.miljoeportal.dk/datasets/urn:dmp:ds:renseanlaeg-vandomraadeplaner-2021-2027
#___________________________________________________________________________________________________________________________

# Indlæs renseanlægspunkter og klargør kvælstofudledning
renseanlæg <- st_read( # aar kolonne: 2014-18 gennemsnit
  "VP3_renseanlaeg_vp3e2022_punkt_rens_saml_raw.gpkg",
  quiet = TRUE
) %>%
  st_transform(st_crs(id15_oplande_sf)) %>%
  mutate(
    Udledningspunkt_id = as.character(pkt_id),
    Total_udledning = as.numeric(udl_tn_sta)
  ) # 749 obs. of 37 variables

# Behold kun de oplande, som faktisk indgår i data_master
oplande_master_sf <- id15_oplande_sf %>%
  semi_join(
    data_master %>% distinct(Opland_id),
    by = "Opland_id"
  ) %>%
  select(Opland_id) # 1015 obs. of 2 variables

# Lav buffer omkring alle rensepunkter
renseanlæg_buffer <- st_buffer(renseanlæg, dist = 2000)

# Find alle oplande i data_master der ligger inden for bufferzonen
renseanlæg_opland <- st_join(
  renseanlæg_buffer,
  oplande_master_sf,
  join = st_intersects,
  left = TRUE
) %>%
  st_drop_geometry() %>%
  transmute(
    Udledningspunkt_id,
    Opland_id = as.integer(Opland_id),
    Total_udledning
  ) %>%
  filter(!is.na(Opland_id)) %>%
  distinct() # 1010 obs. of 3 variables. Der er 1010 unikke kombinationer af et opland og et renseanlæg. Et udledningspunkt kan godt ramme 2 oplande f.eks.

# Beregn hvor mange oplande hvert punkt rammer
punkt_opland_antal <- renseanlæg_opland %>%
  count(Udledningspunkt_id, name = "antal_oplande_ramt") # 554 obs. of 2 variables. Der er 554 renseanlæg, som rammer mindst et opland.  

# Fordel kvælstofudledning ligeligt mellem ramte oplande
rense_effekt <- renseanlæg_opland %>%
  left_join(punkt_opland_antal, by = "Udledningspunkt_id") %>%
  mutate(
    kvælstof_fordelt = Total_udledning / antal_oplande_ramt
  ) %>%
  group_by(Opland_id) %>%
  summarise(
    Renseanlæg_kvælstofpåvirkning_kg_år =
      if_else(
        all(is.na(kvælstof_fordelt)),
        NA_real_,
        sum(kvælstof_fordelt, na.rm = TRUE)
      ),
    .groups = "drop"
  ) # 597 obs. of 2 variables. Der er 597 oplande, som får tildelt en samlet renseanlægspåvirkning. 

# Tilføj rensepåvirkning til masterdatasættet
data_master <- data_master %>%
  left_join(rense_effekt, by = "Opland_id") 

#___________________________________________________________________________________________________________________________
# Regnbetingende udløb: Udledning + stamdata - Miljøbeskyttelse - Arealdata 
# Link: https://arealdata.miljoeportal.dk/datasets/urn:dmp:ds:regnbetingede-udloeb-udledning
# Link: https://arealdata.miljoeportal.dk/datasets/urn:dmp:ds:regnbetingede-udloeb-stamdata
#___________________________________________________________________________________________________________________________

# Indlæs stamdata og udledningsdata
overløb_stamdata <- read_excel("regnbetingende_udløb_stamdata.xlsx") # 67401 obs. of 18 variables
overløb_udledningsdata <- read_excel("regnbetingende_udløb_udledning.xlsx") # 273400 obs. of 13 variables

# Funktion til konvertering af danske talformater til numeriske værdier
konverter_dansk_tal <- function(x) {
  x <- str_trim(as.character(x))
  x <- na_if(x, "")
  x <- str_replace_all(x, "\\.", "")
  x <- str_replace_all(x, ",", ".")
  suppressWarnings(as.numeric(x))
}

# Klargør stamdata som punktgeometri
overløb_stamdata_sf <- overløb_stamdata %>%
  mutate(
    Udløbspunkt_id = as.character(OutfallId),
    Breddegrad = konverter_dansk_tal(Latitude),
    Længdegrad = konverter_dansk_tal(Longitude)
  ) %>%
  filter(!is.na(Breddegrad), !is.na(Længdegrad)) %>%
  st_as_sf(
    coords = c("Længdegrad", "Breddegrad"),
    crs = 4326,
    remove = FALSE
  ) %>%
  st_transform(st_crs(id15_oplande_sf)) %>%
  select(Udløbspunkt_id) # # 67401 obs. of 2 variables

# Klargør årlige udledningsdata
overløb_udledning <- overløb_udledningsdata %>%
  mutate(
    Udløbspunkt_id = as.character(OutfallId),
    År = as.integer(Year),
    Overløb_kvælstof_kg_år = as.numeric(Nitrogen),
    Overløb_antal_år = as.numeric(Overflows)
  ) %>%
  filter(
    År >= 2016,
    År <= 2023
  ) %>%
  select(Udløbspunkt_id, År, Overløb_kvælstof_kg_år, Overløb_antal_år) # 153951 obs. of 4 variables. Der er 153.951 udledningsobservationer i analyseperiode.

# Behold kun de oplande, som faktisk indgår i masterdatasættet
oplande_master_sf <- id15_oplande_sf %>%
  semi_join(
    data_master %>% distinct(Opland_id),
    by = "Opland_id"
  ) %>%
  select(Opland_id)

# Knyt årlige udledninger til punktgeometri
overløb_sf <- overløb_udledning %>%
  left_join(
    overløb_stamdata_sf,
    by = "Udløbspunkt_id"
  ) %>%
  st_as_sf() # 153951 obs. of 5 variables

# Lav buffer omkring udløbspunkter
overløb_buffer <- st_buffer(overløb_sf, dist = 1000)

# Find alle oplande i masterdatasættet, der rammes af bufferzonen
overløb_opland <- st_join(
  overløb_buffer,
  oplande_master_sf,
  join = st_intersects,
  left = TRUE
) %>%
  st_drop_geometry() %>%
  transmute(
    Udløbspunkt_id,
    År,
    Opland_id = as.integer(Opland_id),
    Overløb_kvælstof_kg_år,
    Overløb_antal_år
  ) %>%
  filter(!is.na(Opland_id)) %>%
  distinct() # 119408 obs. of 5 variables. Der er 119.408 kombinationer af udledningspunkt, år og opland. Nogle udløb fjernes, da de ikke rammer de 1015 oplande. 

# Beregn hvor mange oplande hvert udløbspunkt rammer pr. år
udløbspunkt_opland_antal <- overløb_opland %>%
  count(Udløbspunkt_id, År, name = "Antal_oplande_ramt") # 82932 obs. of 3 variables. Samme udløbspunkt kan ramme flere oplande. 

# Fordel påvirkningen ligeligt mellem ramte oplande
overløb_effekt <- overløb_opland %>%
  left_join(udløbspunkt_opland_antal, by = c("Udløbspunkt_id", "År")) %>%
  mutate(
    Overløb_kvælstof_fordelt_kg_år = Overløb_kvælstof_kg_år / Antal_oplande_ramt,
    Overløb_antal_fordelt_år = Overløb_antal_år / Antal_oplande_ramt
  ) %>%
  group_by(Opland_id, År) %>%
  summarise(
    Overløb_kvælstof_kg_år =
      if_else(
        all(is.na(Overløb_kvælstof_fordelt_kg_år)),
        NA_real_,
        sum(Overløb_kvælstof_fordelt_kg_år, na.rm = TRUE)
      ),
    Overløb_antal_år =
      if_else(
        all(is.na(Overløb_antal_fordelt_år)),
        NA_real_,
        sum(Overløb_antal_fordelt_år, na.rm = TRUE)
      ),
    .groups = "drop"
  ) # 7225 obs. of 4 variables. der er 7225 opland-år observationer i alt. 

# Tilføj overløbspåvirkning til masterdatasættet
data_master <- data_master %>%
  left_join(
    overløb_effekt,
    by = c("Opland_id", "År")
  ) 

#___________________________________________________________________________________________________________________________
# VP3 - Indsats: Ukloakerede ejendomme - Naturbeskyttelse - Arealdata 
# Link: https://arealdata.miljoeportal.dk/datasets/urn:dmp:ds:indsats-ukloakerede-ejendomme-punktkilder-vandomraadeplaner-2021-2027
#___________________________________________________________________________________________________________________________

# Indlæs polygoner for områder med spredt spildevand
spredt_spildevand <- st_read(
  "vp3_ukloakerede_ejendomme.gpkg",
  quiet = TRUE
) # 902 obs. of 10 variables

# Behold kun de oplande, som faktisk indgår i masterdatasættet
oplande_master_sf <- id15_oplande_sf %>%
  semi_join(
    data_master %>% distinct(Opland_id),
    by = "Opland_id"
  ) %>%
  select(Opland_id)

# Klip polygoner for spredt spildevand mod oplande
spredt_spildevand_klip <- st_intersection(
  spredt_spildevand,
  oplande_master_sf
) # 1277 obs. of 11 variables

# Beregn arealet med spredt spildevand inden for hvert opland
spredt_spildevand_areal <- spredt_spildevand_klip %>%
  mutate(
    Spredt_spildevand_areal_klippet_m2 = as.numeric(st_area(.))
  ) %>%
  st_drop_geometry() %>%
  group_by(Opland_id) %>%
  summarise(
    Spredt_spildevand_areal_m2 =
      if_else(
        all(is.na(Spredt_spildevand_areal_klippet_m2)),
        NA_real_,
        sum(Spredt_spildevand_areal_klippet_m2, na.rm = TRUE)
      ),
    .groups = "drop"
  ) # 213 obs. of 2 variables

# Beregn samlet oplandsareal
opland_areal <- oplande_master_sf %>%
  mutate(
    Opland_areal_m2 = as.numeric(st_area(.))
  ) %>%
  st_drop_geometry() # 1015 obs. of 2 variables

# Beregn andel af oplandet med spredt spildevand
spredt_spildevand_andel <- spredt_spildevand_areal %>%
  left_join(opland_areal, by = "Opland_id") %>%
  mutate(
    Spredt_spildevand_andel =
      Spredt_spildevand_areal_m2 / Opland_areal_m2
  ) %>%
  select(Opland_id, Spredt_spildevand_andel) # 213 obs. of 2 variables men nu i andele

# Tilføj andel med spredt spildevand til masterdatasættet
data_master <- data_master %>%
  left_join(spredt_spildevand_andel, by = "Opland_id") 

#___________________________________________________________________________________________________________________________
# VP3 - Arealanvendelse - Naturbeskyttelse - Arealdata 
# Link: https://arealdata.miljoeportal.dk/datasets/urn:dmp:ds:arealanvendelse-vandomraadeplaner-2021-2027
# Metadata: https://geodata-info.dk/srv/dan/catalog.search#/metadata/da22d09d-eec3-44f6-a470-c551e03d512b
#___________________________________________________________________________________________________________________________

# Indlæs arealanvendelsesandele på oplandsniveau
arealanvendelse_opland <- readRDS("lulc_vp3_2022_opland_feats.rds") # 2929 obs. of 23 variables

# Tilføj arealanvendelse til masterdatasættet
data_master <- data_master %>%
  left_join(arealanvendelse_opland, by = "Opland_id") %>%
  mutate(
    Areal_by =
      lulc_share_1 + lulc_share_2 + lulc_share_3 + lulc_share_4 +
      lulc_share_5 + lulc_share_6 + lulc_share_7 + lulc_share_8,
    
    Areal_landbrug =
      lulc_share_11 + lulc_share_12 + lulc_share_13,
    
    Areal_natur =
      lulc_share_16 + lulc_share_19 + lulc_share_20,
    
    Areal_andet =
      lulc_share_0 + lulc_share_9 + lulc_share_10 + lulc_share_14 +
      lulc_share_15 + lulc_share_17 + lulc_share_18 + lulc_share_21
  ) %>%
  select(-starts_with("lulc_share_")) 

#___________________________________________________________________________________________________________________________
# Jordbundskort 2019 - Landbrug og fiskeri - Arealdata 
# Link: https://arealdata.miljoeportal.dk/datasets/urn:dmp:ds:jordbundskort-2019
#___________________________________________________________________________________________________________________________

# Indlæs jordbundsandele på oplandsniveau
jordbund_opland <- readRDS("jb2019_opland_features.rds") # 3136 obs. of 12 variables

# Jordbundsvariable omdøbes
jordbund_opland_udvalgt <- jordbund_opland %>%
  transmute(
    Opland_id = as.integer(Opland_id),
    Jordbund_Grovsandet_andel = JB_share_1,
    Jordbund_Finsandet_andel = JB_share_2,
    Jordbund_Grov_lerblandet_sandjord_andel = JB_share_3,
    Jordbund_Ler_andel = JB_share_7,
    Jordbund_Organisk_andel = JB_share_11
  ) # 3136 obs. of 6 variables

# Tilføj jordbundsandele til masterdatasættet
data_master <- data_master %>%
  left_join(jordbund_opland_udvalgt, by = "Opland_id") 

#___________________________________________________________________________________________________________________________
# DMI data
# Link: https://opendataapi.dmi.dk/v2/metObs/bulk/
# Link: https://www.dmi.dk/friedata/dokumentation/download-data?
#___________________________________________________________________________________________________________________________

# For hvert målestation og hver dag vælges den nærmeste DMI-station med gyldig observation.
# Der anvendes op til 5 kandidatstationer pr. målested inden for afstandsgrænsen.

# Afstandsgrænser og datakrav
maks_afstand_nedbør_m <- 15000        # 15 km
maks_afstand_temperatur_m <- 20000    # 20 km
min_gyldige_dage_sæson <- 60          # Minimum antal gyldige dage for at beregne sæsonværdi
antal_kandidatstationer <- 5          # Antal nærmeste DMI-stationer der vurderes pr. målested

# Analyseår
år <- 2016:2023

# Filstier til DMI bulk-data
dmi_filer <- file.path(
  paste0("dmi_bulk_", år),
  paste0("daily_station_", år, ".parquet")
)

# Indlæs daglige DMI-observationer og klargør dato, nedbør, temperatur og sæsonvariable
dmi_daglig <- map_dfr(dmi_filer, read_parquet) %>%
  transmute(
    DMI_station_id = as.character(stationId),
    Dato = as.Date(date),
    Nedbør_mm_dag = as.numeric(precip_past1h_sum),
    Temperatur_gns_dag = as.numeric(temp_dry_mean)
  ) %>%
  mutate(
    # Behold kun realistiske daglige nedbørsværdier
    # Urealistiske nedbørsværdier sættes til NA før aggregation
    Nedbør_mm_dag = if_else(
      Nedbør_mm_dag >= 0 & Nedbør_mm_dag <= 40,
      Nedbør_mm_dag,
      NA_real_
    ),
    
    År = as.integer(year(Dato)),
    Måned = as.integer(month(Dato)),
    
    # Meteorologisk sæson
    Sæson = case_when(
      Måned %in% c(12, 1, 2)  ~ "vinter",
      Måned %in% c(3, 4, 5)   ~ "forår",
      Måned %in% c(6, 7, 8)   ~ "sommer",
      Måned %in% c(9, 10, 11) ~ "efterår"
    ),
    
    # December tilhører næste års vinter
    Sæson_år = if_else(Måned == 12L, År + 1L, År),
    
    # Numerisk sæsonrækkefølge
    Sæson_nr = case_when(
      Sæson == "vinter"  ~ 1L,
      Sæson == "forår"   ~ 2L,
      Sæson == "sommer"  ~ 3L,
      Sæson == "efterår" ~ 4L
    )
  ) %>%
  filter(
    Sæson_år >= 2016,
    Sæson_år <= 2023,
    !(Sæson_år == 2016 & Sæson == "vinter")
  ) # 463809 obs. of 9 variables

# Funktion til hentning af DMI-stationer fra DMI's API
hent_dmi_stationer <- function(grænse = 1000, maks_sider = 200) {
  basis_url <- "https://opendataapi.dmi.dk/v2/metObs/collections/station/items"
  stationer_liste <- list()
  start_række <- 0
  
  for (side in seq_len(maks_sider)) {
    url <- paste0(basis_url, "?limit=", grænse, "&offset=", start_række)
    
    svar <- GET(url)
    stop_for_status(svar)
    
    json_indhold <- fromJSON(
      content(svar, "text", encoding = "UTF-8"),
      flatten = TRUE
    )
    
    stationer_side <- json_indhold$features
    
    if (is.null(stationer_side)) break
    
    if (!is.data.frame(stationer_side)) {
      stationer_side <- as.data.frame(stationer_side)
    }
    
    if (NROW(stationer_side) == 0) break
    
    stationer_liste[[length(stationer_liste) + 1]] <- stationer_side
    start_række <- start_række + grænse
  }
  
  bind_rows(stationer_liste)
}

# Hent rå stationsmetadata fra DMI
dmi_stationer_rå <- hent_dmi_stationer() # 682 obs. of 23 variables

# Klargør DMI-stationer med stations-id og koordinater
dmi_stationer <- dmi_stationer_rå %>%
  transmute(
    DMI_station_id = as.character(properties.stationId),
    Land = as.character(properties.country),
    Koordinater = geometry.coordinates
  ) %>%
  filter(Land == "DNK") %>%
  mutate(
    Længdegrad = map_dbl(Koordinater, 1),
    Breddegrad = map_dbl(Koordinater, 2)
  ) %>%
  select(DMI_station_id, Længdegrad, Breddegrad) %>%
  filter(
    !is.na(DMI_station_id),
    !is.na(Længdegrad),
    !is.na(Breddegrad)
  ) %>%
  distinct(DMI_station_id, .keep_all = TRUE) # 234 obs. of 3 variables

# Udvælg DMI-stationer med mindst én gyldig nedbørsobservation
dmi_stationer_nedbør_sf <- dmi_stationer %>%
  semi_join(
    dmi_daglig %>%
      filter(!is.na(Nedbør_mm_dag)) %>%
      distinct(DMI_station_id),
    by = "DMI_station_id"
  ) %>%
  st_as_sf(
    coords = c("Længdegrad", "Breddegrad"),
    crs = 4326
  ) %>%
  st_transform(25832) # 112 obs. of 2 variables

# Udvælg DMI-stationer med mindst én gyldig temperaturobservation
dmi_stationer_temperatur_sf <- dmi_stationer %>%
  semi_join(
    dmi_daglig %>%
      filter(!is.na(Temperatur_gns_dag)) %>%
      distinct(DMI_station_id),
    by = "DMI_station_id"
  ) %>%
  st_as_sf(
    coords = c("Længdegrad", "Breddegrad"),
    crs = 4326
  ) %>%
  st_transform(25832) # 63 obs. of 2 variables

# Opret sf-punkter for målestationer i masterdatasættet
målesteder_sf <- data_master %>%
  distinct(
    Stations_id,
    Målested_x_koordinat,
    Målested_y_koordinat
  ) %>%
  filter(
    !is.na(Målested_x_koordinat),
    !is.na(Målested_y_koordinat)
  ) %>%
  st_as_sf(
    coords = c("Målested_x_koordinat", "Målested_y_koordinat"),
    crs = 25832,
    remove = FALSE
  ) 

# Beregn afstande fra hver målestation til DMI-stationer med nedbør
afstand_nedbør <- st_distance(målesteder_sf, dmi_stationer_nedbør_sf)

# Beregn afstande fra hvert målested til DMI-stationer med temperatur
afstand_temperatur <- st_distance(målesteder_sf, dmi_stationer_temperatur_sf)

# Konverter afstandsmatricer til meter uden units-klasse
afstand_nedbør <- drop_units(as.matrix(set_units(afstand_nedbør, "m")))
afstand_temperatur <- drop_units(as.matrix(set_units(afstand_temperatur, "m")))

# Navngiv rækker og kolonner i afstandsmatricerne
colnames(afstand_nedbør) <- dmi_stationer_nedbør_sf$DMI_station_id
rownames(afstand_nedbør) <- målesteder_sf$Stations_id

colnames(afstand_temperatur) <- dmi_stationer_temperatur_sf$DMI_station_id
rownames(afstand_temperatur) <- målesteder_sf$Stations_id

# Fjern DMI-stationer uden for afstandsgrænsen
afstand_nedbør[afstand_nedbør > maks_afstand_nedbør_m] <- NA_real_
afstand_temperatur[afstand_temperatur > maks_afstand_temperatur_m] <- NA_real_

# Find op til 5 nærmeste nedbørsstationer pr. målested
sted_nedbør_stationer <- as.data.frame(as.table(afstand_nedbør)) %>%
  rename(
    Stations_id = Var1,
    DMI_station_id = Var2,
    Afstand_m = Freq
  ) %>%
  filter(!is.na(Afstand_m)) %>%
  group_by(Stations_id) %>%
  arrange(Afstand_m, .by_group = TRUE) %>%
  slice_head(n = antal_kandidatstationer) %>%
  ungroup() # 2078 obs. of 3 variables

# Find op til 5 nærmeste temperaturstationer pr. målested
sted_temperatur_stationer <- as.data.frame(as.table(afstand_temperatur)) %>%
  rename(
    Stations_id = Var1,
    DMI_station_id = Var2,
    Afstand_m = Freq
  ) %>%
  filter(!is.na(Afstand_m)) %>%
  group_by(Stations_id) %>%
  arrange(Afstand_m, .by_group = TRUE) %>%
  slice_head(n = antal_kandidatstationer) %>%
  ungroup() # 1701 obs. of 3 variables

# Knyt daglig nedbør til hvert målested
# For hver dag vælges den nærmeste kandidatstation med gyldig nedbørsobservation
nedbør_dag_sted <- sted_nedbør_stationer %>%
  left_join(
    dmi_daglig %>%
      select(
        DMI_station_id,
        Dato,
        Sæson_år,
        Sæson,
        Sæson_nr,
        Nedbør_mm_dag
      ),
    by = "DMI_station_id",
    relationship = "many-to-many"
  ) %>%
  filter(!is.na(Nedbør_mm_dag)) %>%
  group_by(Stations_id, Dato, Sæson_år, Sæson, Sæson_nr) %>%
  arrange(Afstand_m, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  select(
    Stations_id,
    Dato,
    Sæson_år,
    Sæson,
    Sæson_nr,
    Nedbør_mm_dag
  ) # 3346794 obs. of 6 variables

# Knyt daglig temperatur til hvert målested
# For hver dag vælges den nærmeste kandidatstation med gyldig temperaturobservation
temperatur_dag_sted <- sted_temperatur_stationer %>%
  left_join(
    dmi_daglig %>%
      select(
        DMI_station_id,
        Dato,
        Sæson_år,
        Sæson,
        Sæson_nr,
        Temperatur_gns_dag
      ),
    by = "DMI_station_id",
    relationship = "many-to-many"
  ) %>%
  filter(!is.na(Temperatur_gns_dag)) %>%
  group_by(Stations_id, Dato, Sæson_år, Sæson, Sæson_nr) %>%
  arrange(Afstand_m, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  select(
    Stations_id,
    Dato,
    Sæson_år,
    Sæson,
    Sæson_nr,
    Temperatur_gns_dag
  ) # 2962148 obs. of 6 variables

# Saml daglig nedbør og temperatur pr. målested
vejr_dag_sted <- full_join(
  nedbør_dag_sted,
  temperatur_dag_sted,
  by = c(
    "Stations_id",
    "Dato",
    "Sæson_år",
    "Sæson",
    "Sæson_nr"
  )
) # 3920844 obs. of 7 variables

# Aggregér daglige vejrdata til sæsonniveau
# Sæsonværdier beregnes kun, hvis der er mindst 60 gyldige dage i sæsonen
dmi_sæson <- vejr_dag_sted %>%
  group_by(
    Stations_id,
    Sæson_år,
    Sæson,
    Sæson_nr
  ) %>%
  summarise(
    Gyldige_nedbørsdage = sum(!is.na(Nedbør_mm_dag)),
    Gyldige_temperaturdage = sum(!is.na(Temperatur_gns_dag)),
    
    Nedbør_sum_mm_sæson = if_else(
      Gyldige_nedbørsdage >= min_gyldige_dage_sæson,
      sum(Nedbør_mm_dag, na.rm = TRUE),
      NA_real_
    ),
    
    Kraftig_regn_dage_sæson = if_else(
      Gyldige_nedbørsdage >= min_gyldige_dage_sæson,
      sum(Nedbør_mm_dag >= 10, na.rm = TRUE),
      NA_real_
    ),
    
    Tørre_dage_sæson = if_else(
      Gyldige_nedbørsdage >= min_gyldige_dage_sæson,
      sum(Nedbør_mm_dag < 1, na.rm = TRUE),
      NA_real_
    ),
    
    Temperatur_gns_sæson = if_else(
      Gyldige_temperaturdage >= min_gyldige_dage_sæson,
      mean(Temperatur_gns_dag, na.rm = TRUE),
      NA_real_
    ),
    
    .groups = "drop"
  ) # 43492 obs. of 10 variables

# Tilføj sæsonaggregerede DMI-variable til masterdatasættet
data_master <- data_master %>%
  left_join(
    dmi_sæson %>%
      select(
        Stations_id,
        Sæson_år,
        Sæson,
        Sæson_nr,
        Nedbør_sum_mm_sæson,
        Kraftig_regn_dage_sæson,
        Tørre_dage_sæson,
        Temperatur_gns_sæson
      ),
    by = c(
      "Stations_id",
      "År" = "Sæson_år",
      "Sæson",
      "Sæson_nr"
    )
  ) 


#___________________________________________________________________________________________________________________________
# Det centrale husdyrbrugsregister (2021-2023)
# Link: https://landbrugsgeodata.fvm.dk/
#___________________________________________________________________________________________________________________________

# Indlæs CHR-data og standardisér datatyper
# CHRNR = besætningsnummer
# DE = dyreenheder
# chr_år = registreringsår i CHR-data
chr_data <- readRDS("chr_all_raw.rds") %>%
  mutate(
    CHRNR = as.character(CHRNR),
    Dyreenheder = as.numeric(DE),
    chr_år = as.integer(chr_year)
  ) # 199007 obs. of 23 variables

# Aggregér til én observation pr. besætning pr. år
# Hvis alle dyreenheder er NA for en besætning-år, bevares værdien som NA
chr_besætning_sf <- chr_data %>%
  group_by(chr_år, CHRNR) %>%
  summarise(
    Husdyr_dyreenheder =
      if_else(
        all(is.na(Dyreenheder)),
        NA_real_,
        sum(Dyreenheder, na.rm = TRUE)
      ),
    geometry = first(geometry),
    .groups = "drop"
  ) %>%
  st_as_sf() %>%
  st_transform(st_crs(id15_oplande_sf)) # 153927 obs. of 4 variables. En række = en unik besætning pr. år. 

# Beregn oplandsareal i km2
opland_areal <- id15_oplande_sf %>%
  transmute(
    Opland_id = as.integer(Opland_id),
    Opland_areal_km2 = as.numeric(st_area(.) / 1e6)
  ) %>%
  st_drop_geometry()

# Knyt besætninger til oplande og aggregér husdyrdata til opland-år
# Hvis alle dyreenheder er NA i et opland-år, bevares værdien som NA
chr_opland <- chr_besætning_sf %>%
  st_join(
    id15_oplande_sf %>% select(Opland_id),
    join = st_intersects,
    left = FALSE
  ) %>%
  st_drop_geometry() %>%
  group_by(Opland_id, chr_år) %>%
  summarise(
    Husdyr_total_dyreenheder =
      if_else(
        all(is.na(Husdyr_dyreenheder)),
        NA_real_,
        sum(Husdyr_dyreenheder, na.rm = TRUE)
      ),
    .groups = "drop"
  ) %>%
  mutate(
    Opland_id = as.integer(Opland_id),
    chr_år = as.integer(chr_år)
  ) %>%
  left_join(opland_areal, by = "Opland_id") %>%
  transmute(
    Opland_id,
    chr_år,
    Husdyr_dyreenheder_pr_km2 =
      Husdyr_total_dyreenheder / Opland_areal_km2
  ) # 10997 obs. of 3 variables. oplande i hvert år med samlet CHR data (unikke kombinationer af opland og år)

# CHR-data findes kun for 2021-2023, så tidligere analyseår kobles til 2021
data_master <- data_master %>%
  mutate(
    chr_år_tilkobling = case_when(
      År <= 2021 ~ 2021L,
      År == 2022 ~ 2022L,
      År == 2023 ~ 2023L
    )
  ) %>%
  left_join(
    chr_opland,
    by = c("Opland_id", "chr_år_tilkobling" = "chr_år")
  ) %>%
  select(-chr_år_tilkobling) 


#___________________________________________________________________________________________________________________________
# Hydrologiske hovedvandoplande 
# Link: https://mst.dk/erhverv/tilskud-miljoeviden-og-data/data-og-databaser/miljoegis-data-om-natur-og-miljoe-paa-webkort/hent-data-udstillet-paa-miljoegis
# Navn på den fil, som er downloadet: MiljøGIS for vandområdeplanerne for 2021-2027 (vp3endelig2022)
#___________________________________________________________________________________________________________________________

# Sti til udpakkede MiljøGIS-fil
sti_vp3_data <- "data/raw/vp3e2022_vandplan3_endelig_shp"

# Indlæs vandløbssegmenter og behold hovedvandoplands-id
vandløb <- st_read(
  file.path(sti_vp3_data, "vp3e2022_vandloeb_samlet.shp"),
  quiet = TRUE
) %>%
  st_make_valid() %>%
  select(
    Hovedopland_id = ho_id,
    geometry
  ) # 6703 obs. of 2 variables. Der er 6703 vandløbssegmenter. Flere segmenter kan tilhøre samme hovedopland.

# Opret unikke målesteder med koordinater
målesteder_sf <- data_master %>%
  distinct(
    Stations_id,
    Opland_id,
    Målested_x_koordinat,
    Målested_y_koordinat
  ) %>%
  filter(
    !is.na(Opland_id),
    !is.na(Målested_x_koordinat),
    !is.na(Målested_y_koordinat)
  ) %>%
  st_as_sf(
    coords = c("Målested_x_koordinat", "Målested_y_koordinat"),
    crs = 25832,
    remove = FALSE
  )

# Find nærmeste vandløbssegment til hvert målested
nærmeste_vandløb_indeks <- st_nearest_feature(
  målesteder_sf,
  vandløb
)

# Knyt hovedvandopland og afstand til nærmeste vandløbssegment på hvert målested
målested_hovedopland <- målesteder_sf %>%
  mutate(
    Hovedopland_id =
      vandløb$Hovedopland_id[nærmeste_vandløb_indeks],
    
    Afstand_til_vandløb_m = as.numeric(
      st_distance(
        geometry,
        vandløb[nærmeste_vandløb_indeks, ],
        by_element = TRUE
      )
    )
  ) %>%
  st_drop_geometry() 

# Vælg ét stabilt hovedvandopland pr. opland - Hvis flere målestationer findes i samme opland, vælges hovedvandoplandet fra målestedet tættest på et vandløbssegment
opland_hovedopland <- målested_hovedopland %>%
  filter(!is.na(Hovedopland_id)) %>%
  mutate(
    Afstand_rangering = if_else(
      is.na(Afstand_til_vandløb_m),
      Inf,
      Afstand_til_vandløb_m
    )
  ) %>%
  arrange(
    Opland_id,
    Afstand_rangering,
    Hovedopland_id
  ) %>%
  group_by(Opland_id) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(
    Opland_id = as.integer(Opland_id),
    Hovedopland_id = as.character(Hovedopland_id)
  ) # 1015 obs. of 2 variables. Alle oplande ender med et tilknyttet hovedopland. 

# Tilføj hovedvandopland til masterdatasættet
data_master <- data_master %>%
  left_join(
    opland_hovedopland,
    by = "Opland_id"
  ) 

#___________________________________________________________________________________________________________________________
# Terrænhældning - Kortlægning og geodata - Arealdata 
# Link: https://arealdata.miljoeportal.dk/datasets/urn:dmp:ds:terraenhaeldning-over-6-gr
#___________________________________________________________________________________________________________________________

# Indlæs polygoner for terrænhældning og behold kun relevante hældningsklasser
# GRID_KODE = 2 svarer til hældning på 6-12 grader
# GRID_KODE = 3 svarer til hældning over 12 grader
terrænhældning <- st_read("HAELDNING_GT_6GR.shp", quiet = TRUE) %>%
  st_make_valid() %>%
  mutate(
    Hældningsklasse = as.integer(GRID_KODE)
  ) %>%
  filter(Hældningsklasse %in% c(2L, 3L)) %>%
  select(Hældningsklasse) # 2310131 obs. of 2 variables

# Behold kun de oplande, som faktisk indgår i masterdatasættet og beregn samtidig samlet oplandsareal til andelsberegning
oplande_anvendt <- id15_oplande_sf %>%
  semi_join(
    data_master %>% distinct(Opland_id),
    by = "Opland_id"
  ) %>%
  mutate(
    Opland_areal_m2 = as.numeric(st_area(.))
  ) %>%
  select(Opland_id, Opland_areal_m2)

# Klip terrænhældningspolygoner mod oplande - Areal_klippet_m2 er arealet af den givne hældningsklasse inden for hvert opland
terrænhældning_klip <- st_intersection(
  terrænhældning,
  oplande_anvendt
) %>%
  mutate(
    Areal_klippet_m2 = as.numeric(st_area(.))
  ) # 1009495 obs. of 5 variables

# Aggregér hældningsarealer til oplandsniveau
terrænhældning_opland <- terrænhældning_klip %>%
  st_drop_geometry() %>%
  group_by(Opland_id, Opland_areal_m2) %>%
  summarise(
    Terræn_hældning_6_12gr_andel =
      if_else(
        all(is.na(Areal_klippet_m2[Hældningsklasse == 2L])),
        NA_real_,
        sum(Areal_klippet_m2[Hældningsklasse == 2L], na.rm = TRUE) / first(Opland_areal_m2)
      ),
    
    Terræn_hældning_over_12gr_andel =
      if_else(
        all(is.na(Areal_klippet_m2[Hældningsklasse == 3L])),
        NA_real_,
        sum(Areal_klippet_m2[Hældningsklasse == 3L], na.rm = TRUE) / first(Opland_areal_m2)
      ),
    
    .groups = "drop"
  ) %>%
  select(
    Opland_id,
    Terræn_hældning_6_12gr_andel,
    Terræn_hældning_over_12gr_andel
  ) # 1015 obs. of 3 variables

# Tilføj terrænhældning til masterdatasættet
data_master <- data_master %>%
  left_join(terrænhældning_opland, by = "Opland_id") 

#___________________________________________________________________________________________________________________________
# GLM5 Jorderosion - Landbrug og fiskeri - Arealdata 
# Link: https://arealdata.miljoeportal.dk/datasets/urn:dmp:ds:glm-jorderosion-2023
#___________________________________________________________________________________________________________________________

# Indlæs polygoner for registreret jorderosionsrisiko
glm_jorderosion <- st_read(
  "GLM_jorderosion_raw.gpkg",
  quiet = TRUE
) # 1670 obs. of 2 variables

# Behold kun de oplande, som faktisk indgår i masterdatasættet og beregn samtidig samlet oplandsareal til andelsberegning
oplande_anvendt <- id15_oplande_sf %>%
  semi_join(
    data_master %>% distinct(Opland_id),
    by = "Opland_id"
  ) %>%
  mutate(
    Opland_areal_m2 = as.numeric(st_area(.))
  ) %>%
  select(Opland_id, Opland_areal_m2)

# Klip erosionspolygoner mod oplande - Erosion_areal_klippet_m2 er arealet med registreret erosionsrisiko inden for hvert opland
erosion_klip <- st_intersection(
  glm_jorderosion,
  oplande_anvendt
) %>%
  mutate(
    Erosion_areal_klippet_m2 = as.numeric(st_area(.))
  ) # 843 obs. of 5 variables

# Aggregér registreret erosionsrisiko til oplandsniveau
erosion_opland <- erosion_klip %>%
  st_drop_geometry() %>%
  group_by(Opland_id, Opland_areal_m2) %>%
  summarise(
    Registreret_erosionsrisiko_andel =
      if_else(
        all(is.na(Erosion_areal_klippet_m2)),
        NA_real_,
        sum(Erosion_areal_klippet_m2, na.rm = TRUE) / first(Opland_areal_m2)
      ),
    .groups = "drop"
  ) %>%
  select(
    Opland_id,
    Registreret_erosionsrisiko_andel
  ) # 224 obs. of 2 variables

# Tilføj registreret erosionsrisiko til masterdatasættet
data_master <- data_master %>%
  left_join(erosion_opland, by = "Opland_id") # 16639 ibs. of 51 variables

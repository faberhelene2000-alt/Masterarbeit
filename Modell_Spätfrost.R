# Modell_2.csv, lt. Studie FrostStrat, Normierung, Maximalwert, Persistenzfaktor, Payout über die Region
#3 Tage-Fenster

library(data.table)

process_region_weighted <- function(file_bbch, file_sorten, file_out) {
  
  # 1) Bestehende BBCH-Progress Tabelle
  dt <- fread(file_bbch)
  
  dt[, date := as.Date(date, format = "%d.%m.%y")]
  dt[, threshold_date := as.Date(threshold_date, format = "%d.%m.%y")]
  
  # 2) Sortentabelle (nur Gewichtet relevant)
  sorten <- fread(file_sorten)
  names(sorten) <- trimws(names(sorten))
  
  # Nur relevante Spalten behalten
  sorten <- sorten[, .(BBCH, threshold = Gewichtet)]
  
  setorder(sorten, threshold)
  
  # 3) Funktion zur BBCH-Zuordnung
  assign_bbch_weighted <- function(x) {
    # finde höchstes BBCH wo threshold <= x
    valid <- sorten[threshold <= x]
    if (nrow(valid) == 0) return(9)  # default BBCH 9
    return(max(valid$BBCH))
  }
  
  # 4) Anwenden auf jede Zeile
  dt[, BBCH_weighted := sapply(cum_bbch, assign_bbch_weighted)]
  
  # 5) Finaler Output (nur relevante Spalten)
  result <- dt[, .(
    ID,
    year,
    threshold_date,
    date,
    TM,
    contribution,
    cum_bbch,
    BBCH_weighted
  )]
  
  # 6) Export
  fwrite(result, file_out, sep = ";", dec = ",")
  
  return(result)
}


# WESTSTEIERMARK
west_final <- process_region_weighted(
  "Weststeiermark_bbch_progress.csv",
  "Weststeiermark_Sorten_Skala_bereinigt_neu.csv",
  "Weststeiermark_final.csv"
)

# SÜDSTEIERMARK
sued_final <- process_region_weighted(
  "Suedsteiermark_bbch_progress.csv",
  "Suedsteiermark_Sorten_Skala_bereinigt.csv",
  "Suedsteiermark_final.csv"
)

# VULKANLAND
vulkan_final <- process_region_weighted(
  "Vulkanland_bbch_progress.csv",
  "Vulkanland_Sorten_Skala_bereinigt_neu.csv",
  "Vulkanland_final.csv"
)

lt <- fread("Modell_2.csv", sep = ";", dec = ",")

# Spaltennamen prüfen
names(lt)

# Spalten  umbenennen
setnames(lt,
         c("BBCH_Stadium", "LT10", "LT50", "LT90"),
         c("BBCH", "LT10", "LT50", "LT90"))


# Datentypen sicherstellen
lt[, BBCH := as.numeric(BBCH)]
lt[, LT10 := as.numeric(LT10)]
lt[, LT50 := as.numeric(LT50)]
lt[, LT90 := as.numeric(LT90)]


west_final[, Region := "Weststeiermark"]
sued_final[, Region := "Suedsteiermark"]
vulkan_final[, Region := "Vulkanland"]

data <- rbindlist(list(west_final, sued_final, vulkan_final), use.names = TRUE, fill = TRUE)


# Frostschaden berechnen

# 1. LT-Tabelle sortieren
setorder(lt, BBCH)

# 2. Datensatz sortieren (Für Rolling Join)
setorder(data, BBCH_weighted)

# 3. Rolling Join: Nimmt das letzte kleinere oder gleiche BBCH
data <- lt[data, on = .(BBCH = BBCH_weighted), roll = TRUE]

west_raw <- fread("Weststeiermark_gefiltert.csv", sep = ";", dec = ",")
sued_raw <- fread("Suedsteiermark_gefiltert.csv", sep = ";", dec = ",")
vulkan_raw <- fread("Vulkanland_gefiltert.csv", sep = ";", dec = ",")

west_raw <- west_raw[, .(ID, date, TN = `TN [degree_Celsius]`)]
sued_raw <- sued_raw[, .(ID, date, TN = `TN [degree_Celsius]`)]
vulkan_raw <- vulkan_raw[, .(ID, date, TN = `TN [degree_Celsius]`)]

west_raw[, date := as.Date(date, format = "%d.%m.%y")]
sued_raw[, date := as.Date(date, format = "%d.%m.%y")]
vulkan_raw[, date := as.Date(date, format = "%d.%m.%y")]

west_raw[, Region := "Weststeiermark"]
sued_raw[, Region := "Suedsteiermark"]
vulkan_raw[, Region := "Vulkanland"]

raw_all <- rbindlist(list(west_raw, sued_raw, vulkan_raw))

data <- merge(data, raw_all, by = c("ID", "date", "Region"), all.x = TRUE)

summary(data$TN)
data[, TN := as.numeric(TN)]
summary(data$TN)
data[, trigger := TN < LT10]

names(data)



# Normierung Schadensfunktion
data[, schaden :=
       fifelse(TN >= LT10, 0,
               fifelse(TN < LT10 & TN >= LT50,
                       0.5 * (LT10 - TN) / (LT10 - LT50),
                       fifelse(TN < LT50 & TN >= LT90,
                               0.5 + 0.5 * (LT50 - TN) / (LT50 - LT90),
                               fifelse(TN < LT90, 1,
                                       0
                               ))))]

summary(data$schaden)


# Persistenzfaktor

library(zoo)

# Sortierung wichtig für rollierende Fenster
setorder(data, ID, year, date)

# Nur schadensrelevante Frosttage berücksichtigen (tatsächlicher Schaden >0)
data[, frost_event := schaden > 0]

# Rollierende Summe innerhalb eines 3-Tage-Fensters (nur schadensrelevante Frosttage)
data[, frost_3day_count :=
       rollapply(
         as.numeric(frost_event),
         width = 3,
         FUN = sum,
         align = "right",
         fill = 0,
         partial = TRUE
       ),
     by = .(Region, ID, year)]

# Maximale Anzahl an Frosttagen je Station/Jahr
persistenz_dt <- data[, .(
  max_frosttage_3d = max(frost_3day_count, na.rm = TRUE)
), by = .(Region, ID, year)]


# Persistenzfaktor definieren

persistenz_dt[, persistenzfaktor :=
                fifelse(max_frosttage_3d == 0, 0,
                        fifelse(max_frosttage_3d == 1, 0.3,
                                1.0))]

# Maximalen Schaden seperat berechnen 

max_schaden_dt <- data[, .(
  max_schaden = max(schaden, na.rm = TRUE)
), by = .(ID, year, Region)]

write.xlsx(
  max_schaden_dt,
  file = "Schaden_Max_MA.xlsx",
  overwrite = TRUE
)

# Persistenzfaktor und Maximalschaden zusammenführen 

station_year <- merge(
  max_schaden_dt,
  persistenz_dt,
  by = c("ID", "year", "Region")
)

#  Severity je Station
station_year[, frost_schaden :=
               max_schaden * persistenzfaktor]



#  AGGREGATION

region_trigger <- station_year[, .(
  Trigger = any(frost_schaden > 0)
), by = .(year, Region)]

region_year <- station_year[, .(
  frost_schaden = mean(frost_schaden, na.rm = TRUE)
), by = .(year, Region)]

head(region_year)

write.xlsx(
  region_year,
  file = "Schaden_Final_Gebiet_MA.xlsx",
  overwrite = TRUE
)


# Ertragsdaten berechnen
west_ertrag <- fread("Ernte_Weststeiermark.csv", sep = ";", dec = ",")
sued_ertrag <- fread("Ernte_Suedsteiermark.csv", sep = ";", dec = ",")
vulkan_ertrag <- fread("Ernte_Vulkanland.csv", sep = ";", dec = ",")

west_ertrag[, Region := "Weststeiermark"]
sued_ertrag[, Region := "Suedsteiermark"]
vulkan_ertrag[, Region := "Vulkanland"]

ertrag <- rbindlist(list(west_ertrag, sued_ertrag, vulkan_ertrag), use.names = TRUE, fill = TRUE)

ertrag <- ertrag[, .(
  year = Jahr,
  Region = Weinbaugebiet,
  Detrended
)]

table(ertrag$Region)
head(ertrag)

unique(region_year$Region)
unique(ertrag$Region)
ertrag[Region == "Südsteiermark", Region := "Suedsteiermark"]

final <- merge(region_year, ertrag, by = c("year", "Region"))

table(final$Region)
head(final)



# Paneldaten-Regression

library(plm)

# Datentyp korrigieren
final$Detrended <- as.numeric(final$Detrended)

# Panelstruktur definieren
pdata <- pdata.frame(final, index = c("Region", "year"))

# Fixed Effects Modell
fe_model <- plm(Detrended ~ frost_schaden,
                data = pdata,
                model = "within")

summary(fe_model)

head(final)


#Payout

# Mittelwert der maximalen Schäden aller Stationen
severity <- station_year[, .(
  Severity = mean(frost_schaden, na.rm = TRUE)
), by = .(year, Region)]

# Spatial Extent, Anteil der Stationen mit Schaden > 0
spatial <- station_year[, .(
  Spatial_Extent = mean(frost_schaden > 0, na.rm = TRUE)
), by = .(year, Region)]

write.xlsx(
  spatial,
  file = "Spatial_MA.xlsx",
  overwrite = TRUE
)

# Zusammenführen
payout_dt <- merge(severity, spatial, by = c("year", "Region"))

# 1.) Payout noch ohne Deckel
payout_dt[, payout_raw := Severity * Spatial_Extent]

write.xlsx(
  payout_dt,
  file = "Auszahlung_MA.xlsx",
  overwrite = TRUE
)

# 2.) Deckel bei 100%
payout_dt[, payout_rate := pmin(1, payout_raw)]


# 3.) Mindestschwelle 5% 
payout_dt[, payout_rate :=
            ifelse(Severity > 0.05, payout_rate, 0)]

head(payout_dt)


# Mit Ertrag verbinden und Zusammenhang testen 

final2 <- merge(final, payout_dt, by = c("year", "Region"))

pdata2 <- pdata.frame(final2, index = c("Region", "year"))

summary(plm(Detrended ~ payout_rate, data = pdata2, model = "within"))


# Modellvalidierung 

# 1. Relevante Jahre definieren (bekannte Frostjahre)
validation_years <- final2[year %in% c(2016, 2017, 2024)]

# Sortieren für Übersicht
setorder(validation_years, year, Region)

print("VALIDIERUNGSJAHRE")
print(validation_years)


# 2. Basisrisiko berechnen

# Differenz zwischen tatsächlichem Ertragsverlust und Modell-Auszahlung

library(data.table)


sued <- fread("Ernte_Suedsteiermark.csv")
west <- fread("Ernte_Weststeiermark.csv")
vulkan <- fread("Ernte_Vulkanland.csv")

setnames(sued, c("Jahr", "Weinbaugebiet", "Ertrag/Ha"), 
         c("year", "Region", "hl_ha"))

setnames(west, c("Jahr", "Weinbaugebiet", "Ertrag/ha"), 
         c("year", "Region", "hl_ha"))

setnames(vulkan, c("Jahr", "Weinbaugebiet", "Ertrag/ha"), 
         c("year", "Region", "hl_ha"))

trend_data <- rbindlist(list(sued, west, vulkan), fill = TRUE)

trend_data <- trend_data[, .(year, Region, Trend, hl_ha)]

trend_data[, Region := chartr("ü", "ue", Region)]

normalize_region <- function(dt) {
  dt[, Region := chartr("üöä", "uoa", Region)]
  dt[, Region := gsub("Süd", "Sued", Region)]
}

normalize_region(trend_data)
normalize_region(final2)

final2[, Region := gsub("Sued", "Sued", Region)]  # bleibt gleich

trend_data[, Region := gsub("Sudsteiermark", "Suedsteiermark", Region)]


final3 <- merge(
  final2,
  trend_data,
  by = c("year", "Region"),
  all.x = TRUE
)


final3[, payout_percent := payout_rate * 100]

final3[, detrended_percent := (hl_ha - Trend) / Trend * 100]

final3[, loss_percent := pmax(0, -detrended_percent)]

final3[, basis_risk := loss_percent - payout_percent]

validation_years <- final3[year %in% c(2017, 2016, 2024)]

validation_years[, .(
  year,
  Region,
  detrended_percent,
  loss_percent,
  payout_percent,
  basis_risk
)]

summary(final3$detrended_percent)
summary(final3$loss_percent)
summary(final3$basis_risk)


print("Basisrisiko in Frostjahren")
print(validation_years[, .(
  year,
  Region,
  detrended_percent,
  loss_percent,
  payout_percent,
  basis_risk
)])

write.csv(
  validation_years,
  "Basisrisiko_final.csv",
  row.names = FALSE
)


# 3. Korrelation (Gesamtmodell)

correlation <- cor(final3$payout_rate, final3$Detrended, use = "complete.obs")

print("KORRELATION PAYOUT VS ERTRAG")
print(correlation)


# 4. Scatterplot 

plot(final3$payout_rate,
     final3$Detrended,
     main = "Payout vs. Ertragsabweichung",
     xlab = "Payout Rate",
     ylab = "Detrended Yield",
     pch = 19)

abline(lm(Detrended ~ payout_rate, data = final3), lwd = 2)


# 5. Fokus auf Frostjahre 

plot(validation_years$payout_rate,
     validation_years$Detrended,
     main = "Payout vs. Ertrag (Frostjahre)",
     xlab = "Payout Rate",
     ylab = "Detrended Yield",
     pch = 19)

text(validation_years$payout_rate,
     validation_years$Detrended,
     labels = paste(validation_years$year, validation_years$Region),
     pos = 4,
     cex = 0.7)

abline(lm(Detrended ~ payout_rate, data = validation_years), col = "red", lwd = 2)


# 6. Interpretation/Kennzahlen

# Durchschnittliches Basisrisiko
avg_basis_risk <- final3[, mean(basis_risk, na.rm = TRUE)]

# Durchschnittliches Basisrisiko in Frostjahren
avg_basis_risk_frost <- validation_years[, mean(basis_risk, na.rm = TRUE)]

print("BASISRISIKO IN FROSTJAHREN")
print(avg_basis_risk_frost)

# Durchschnittliches Basisrisiko in Frostjahren ohne 2017 
validation_years_no2017 <- validation_years[year != 2017]

avg_basis_risk_frost <- validation_years_no2017[, mean(basis_risk, na.rm = TRUE)]
print(avg_basis_risk_frost)


# 7. Trigger-Validierung

# Hat Modell korrekt ausgelöst?
trigger_check <- merge(region_trigger, payout_dt, by = c("year", "Region"))

print("TRIGGER VS PAYOUT")
print(trigger_check)


# 8. Over-/Undercompensation-

final3[, compensation_type :=
         ifelse(basis_risk > 0, "Undercompensation",
                ifelse(basis_risk < 0, "Overcompensation",
                       "Perfect"))]

comp_summary <- final3[, .N, by = compensation_type]

print("Kompensationsverteilung")
print(comp_summary)


# Burning Cost inkl. 2017
burning_cost_all <- payout_dt[, .(
  Burning_Cost = mean(payout_rate, na.rm = TRUE) * 100
)]

print(burning_cost_all)


# Burning Cost exkl. 2017
burning_cost_no2017 <- payout_dt[year != 2017, .(
  Burning_Cost = mean(payout_rate, na.rm = TRUE) * 100
)]

print(burning_cost_no2017)

burning_cost_summary <- data.table(
  Szenario = c(
    "Gesamter Beobachtungszeitraum",
    "Ohne Berücksichtigung des Jahres 2017"
  ),
  Reinpraemie = c(
    burning_cost_all$Burning_Cost,
    burning_cost_no2017$Burning_Cost
  )
)

# Runden
burning_cost_summary[, Reinpraemie := round(Reinpraemie, 2)]

print(burning_cost_summary)


# KRITISCHE FROSTSEQUENZEN 2017 KORREKT IDENTIFIZIEREN


frost_2017 <- data[
  year == 2017 & schaden > 0,
  .(
    ID,
    Region,
    date,
    TN,
    BBCH,
    schaden
  )
]


# Nach ID + Region sortieren
setorder(frost_2017, Region, ID, date)

# Abstand zum vorherigen Frosttag
frost_2017[, diff_days :=
             as.numeric(date - shift(date)),
           by = .(Region, ID)]

# Neue Sequenz starten wenn Abstand > 1 Tag
frost_2017[, seq_id :=
             cumsum(is.na(diff_days) | diff_days > 1),
           by = .(Region, ID)]

# Länge der Sequenzen
frost_2017[, seq_length := .N,
           by = .(Region, ID, seq_id)]

# Nur Sequenzen >= 2 Tage
kritische_sequenzen <- frost_2017[seq_length >= 2]

# Ausgabe
print(
  kritische_sequenzen[
    order(Region, ID, date),
    .(
      Region,
      ID,
      date,
      TN,
      BBCH,
      schaden,
      seq_length
    )
  ]
)


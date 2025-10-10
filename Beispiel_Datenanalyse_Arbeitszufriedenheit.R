# Titel: Datenanalyse der Umfrage zur Arbeitszufridenheit
# Autor: Aurora Monticelli
# Datum: 2025-09-24
# Beschreibung: Dieses Skript analysiert die Umfragedaten zur Arbeitszufriedenheit von Gesundheitsfachpersonen in Basel-Stadt und Basel-Land 

### 1. Pakete laden ---------------------------------------
library(tidyverse)  # Sammlung von Paketen für Datenwissenschaft
library(readxl)     # Für Excel-Dateien
library(vcd)

### 2. Daten importieren ---------------------------------
# Daten aus CSV-Datei laden
data <- read.csv("healthcare_job_satisfaction.csv")
data <- read.csv("healthcare_data.csv")

# 3. Daten anschuaen ---------------------------------
# Erste Zeilen anzeigen
head(data)

# Datenstruktur untersuchen
print(data)

# Zusammenfassende Statistiken
summary(data)

# Fehlende Werte
sum(is.na(data))



### 3. Visualisierung durch plots ---------------------------------
# Barplot für Anzahl TeilnehmerInnen nach Geschlecht
ggplot(data, aes(x = factor(gender))) + 
  geom_bar(fill = "#b9aadf") +
  labs(title = "Anzahl von TeilnehmerInnen nach Geschlecht", 
       x = "Geschlecht", y = "Häufigkeit")

# Boxplot für Arbeitszufriedenheit nach Geschlecht
ggplot(data, aes(x = factor(gender), y = job_satisfaction)) + 
  geom_boxplot(fill = "#B6CEB4") +
  labs(title = "Arbeitszufriedenheit nach Geschlecht", 
       x = "Geschlecht", y = "Arbeitszufriedenheit")

# Histogram für Arbeitszufriedenheit
ggplot(data, aes(x = job_satisfaction)) + 
  geom_bar(fill = "#BBDCE5") +
  labs(title = "Verteilung der Arbeitszufriedenheit", 
       x = "Arbeitszufriedenheit", y = "Häufigkeit")


### 4. Ästhetische Anpassungen ------------------------------------
ggplot(data, aes(x = profession, fill = profession)) +
  geom_bar() + #Basis des Balkendiagrams
  labs(
    title = "Anzahl der Beschäftigten nach Beruf",
    x = "Beruf", # X-Achsen-Beschriftung
    y = "Häufigkeit", # Y-Achsen-Beschriftung
    fill = "Profession"  # Legendentitel 
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),  # Titel-Stil
    axis.title = element_text(size = 12), # Achsen-Titel-Stil
    axis.text = element_text(size = 10), # Achsen-Titel-Stil
    panel.background = element_rect(fill = "white"), # Weisser Hintergrund
    panel.grid.major = element_line(color = "grey90") # Heller Gitternetz
  ) +
  scale_fill_brewer(palette = "Blues")  # Farbpalette für die Balken



### 5. Descriptive Analyse   ----------------------------------------
# Grundlegende Datenexploration (bereits bekannt)
str(data)
head(data)
summary(data)

# Häufigkeiten kategorialer Variablen
table(data$gender)
table(data$profession)

# Deskriptive Statistiken für numerische Variablen
mean(data$job_satisfaction)    # Mittelwert
sd(data$job_satisfaction)      # Standardabweichung
median(data$job_satisfaction)   # Median


### 6. Inferenzstatistik   ----------------------------------------
## Chi-Quadrat
# Kreuztabelle erstellen: Geschlecht vs. Beruf
table(data$gender, data$profession)

# Chi-Quadrat Test durchführen
chisq_result <- chisq.test(table(data$gender, data$profession))
print(chisq_result)

# Erwartete Häufigkeiten anzeigen
chisq_result$expected

# Standardisierte Residuen analysieren
chisq_result$residuals

# Cramér's V als Effektstärke berechnen
assocstats(table(data$gender, data$profession))

## Proportionen Test
# Ein-Stichproben Test: Anteil weiblicher Beschäftigter vs. 50%
female_count <- sum(data$gender == "Female")
total_count <- nrow(data)
prop.test(female_count, total_count, p = 0.5)

# Zwei-Stichproben Test: Vergleich der Zufriedenheitsanteile (≥4) zwischen Geschlechtern
female_high_satisfaction <- sum(data$gender == "Female" & data$job_satisfaction >= 4)
male_high_satisfaction <- sum(data$gender == "Male" & data$job_satisfaction >= 4)
female_total <- sum(data$gender == "Female")
male_total <- sum(data$gender == "Male")

prop.test(c(female_high_satisfaction, male_high_satisfaction), 
          c(female_total, male_total))

# Exakter Test nach Fisher für kleine Stichproben
fisher.test(matrix(c(female_high_satisfaction, female_total - female_high_satisfaction,
                     male_high_satisfaction, male_total - male_high_satisfaction), nrow = 2))
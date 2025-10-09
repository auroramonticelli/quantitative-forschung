# Titel: Datenanalyse der Umfrage zur Arbeitszufridenheit
# Autor: Aurora Monticelli
# Datum: 2025-09-24
# Beschreibung: Dieses Skript analysiert die Umfragedaten zur Arbeitszufriedenheit von Gesundheitsfachpersonen in Basel-Stadt und Basel-Land 

# 1. Pakete laden ---------------------------------------
library(tidyverse)  # Sammlung von Paketen für Datenwissenschaft
library(readxl)     # Für Excel-Dateien

# 2. Daten importieren ---------------------------------
# Daten aus CSV-Datei laden
data <- read.csv("healthcare_job_satisfaction.csv")

# 3. Daten anschuaen ---------------------------------
# Erste Zeilen anzeigen
head(data)

# Datenstruktur untersuchen
print(data)

# Zusammenfassende Statistiken
summary(data)

# Fehlende Werte
sum(is.na(data))



# 3. Visualisierung durch plots ---------------------------------
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

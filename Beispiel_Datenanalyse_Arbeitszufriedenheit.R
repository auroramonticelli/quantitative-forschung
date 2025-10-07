# Titel: Datenanalyse der Umfrage zur Arbeitszufridenheit
# Autor: Aurora Monticelli
# Datum: 2025-09-24
# Beschreibung: Dieses Skript analysiert die Umfragedaten zur Arbeitszufriedenheit von Gesundheitsfachpersonen in Basel-Stadt und Basel-Land 

# 1. Pakete laden ---------------------------------------
library(tidyverse)  # Sammlung von Paketen für Datenwissenschaft
library(readxl)     # Für Excel-Dateien

# 2. Daten importieren ---------------------------------
# Daten aus CSV-Datei laden
data <- read.csv("Desktop/healthcare_job_satisfaction.csv")

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
## Barplot 
ggplot(data, aes(x = factor(cyl))) + 
  geom_bar(fill = "#b9aadf") +
  labs(title = "Anzahl der Autos nach Zylinderzahl", 
       x = "Zylinder", y = "Anzahl")

# Boxplot 
ggplot(data, aes(x = factor(cyl), y = mpg)) + 
  geom_boxplot(fill = "#b9aadf") +
  labs(title = "MPG Verteilung nach Zylinderzahl", 
       x = "Zylinder", y = "MPG")

# Histogram 
ggplot(data, aes(x = mpg)) + 
  geom_histogram(fill = "#b9aadf", bins = 10, color = "white") +
  labs(title = "Verteilung der MPG Werte", 
       x = "MPG", y = "Häufigkeit")

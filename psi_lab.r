data <- read.table("revenues2023.txt", header=FALSE, dec=",")

colnames(data) <- c("Week", "Trekking", "MTB", "Strada", "Abbigliamento", "Accessori", 
                    "Componenti", "Assistenza", "Riparazioni",
                    "R_Trekking", "R_MTB", "R_Strada", 
                    "R_Abbigliamento", "R_Accessori", 
                    "R_Assistenza", "R_Riparazioni")


#
#
# ISTOGRAMMA VENDITE
vendite_per_reparto <- colSums(data[, c("Trekking", "MTB", "Strada", "Abbigliamento", "Accessori", "Componenti", "Assistenza", "Riparazioni")])

# Calcolo della media e mediana
media_vendite <- mean(vendite_per_reparto)
mediana_vendite <- median(vendite_per_reparto)

# Creazione dell'istogramma
barplot(vendite_per_reparto,
        main = "Vendite per Reparto (2023)",
        xlab = "Reparto",
        ylab = "Quantità Vendute",
        col = "skyblue",
        cex.names = 0.8,
        cex.main = 0.9,
        cex.lab = 0.8)

# Aggiunta della griglia
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")

# Aggiunta delle linee per media e mediana
abline(h = media_vendite, col = "red", lwd = 2, lty = "dashed")  # Linea per la media
abline(h = mediana_vendite, col = "green", lwd = 2, lty = "dotted")  # Linea per la mediana

# Aggiunta della legenda
legend("topright", legend = c("Media", "Mediana"), 
       col = c("red", "green"), lty = c("dashed", "dotted"), lwd = 2)


#
#
# ISTOGRAMMA RICAVI
ricavi_per_reparto <- colSums(data[, c("R_Trekking", "R_MTB", "R_Strada", "R_Abbigliamento", "R_Accessori", "R_Assistenza", "R_Riparazioni")])

# Calcolo della media e mediana
media_ricavi <- mean(ricavi_per_reparto)
mediana_ricavi <- median(ricavi_per_reparto)

# Creazione dell'istogramma
barplot(ricavi_per_reparto,
        main = "Ricavi per Reparto (2023)",
        xlab = "Reparto",
        ylab = "Ricavi (€)",
        col = "orange",
        cex.names = 0.8,
        cex.main = 0.9,
        cex.lab = 0.8)

# Aggiunta della griglia
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")

# Aggiunta delle linee per media e mediana
abline(h = media_ricavi, col = "red", lwd = 2, lty = "dashed")  # Linea per la media
abline(h = mediana_ricavi, col = "green", lwd = 2, lty = "dotted")  # Linea per la mediana

# Aggiunta della legenda
legend("topright", legend = c("Media", "Mediana"), 
       col = c("red", "green"), lty = c("dashed", "dotted"), lwd = 2)


#
#
# MISURE STATISTICHE PER DIPARTIMENTO

# Dati per Assistenza e Riparazioni
vendite_assistenza_riparazioni <- rowSums(data[, c("Assistenza", "Riparazioni")])
ricavi_assistenza_riparazioni <- rowSums(data[, c("R_Assistenza", "R_Riparazioni")])

# Dati per tutti gli altri reparti
vendite_altri <- rowSums(data[, c("Trekking", "MTB", "Strada", "Abbigliamento", "Accessori", "Componenti")])
ricavi_altri <- rowSums(data[, c("R_Trekking", "R_MTB", "R_Strada", "R_Abbigliamento", "R_Accessori")])

# Funzione per calcolare statistiche descrittive
calcola_statistiche <- function(x) {
  c(
    Minimo = min(x),
    Quartile_1 = quantile(x, 0.25),
    Mediana = median(x),
    Media = mean(x),
    Quartile_3 = quantile(x, 0.75),
    SD = sd(x),
    Massimo = max(x),
    Totale = sum(x)
  )
}

# Calcolo delle statistiche per vendite
statistiche_vendite <- data.frame(
  Categoria = c("Assistenza e Riparazioni", "Altri Reparti"),
  t(rbind(
    calcola_statistiche(vendite_assistenza_riparazioni),
    calcola_statistiche(vendite_altri)
  ))
)

# Calcolo delle statistiche per ricavi
statistiche_ricavi <- data.frame(
  Categoria = c("Assistenza e Riparazioni", "Altri Reparti"),
  t(rbind(
    calcola_statistiche(ricavi_assistenza_riparazioni),
    calcola_statistiche(ricavi_altri)
  ))
)

# Visualizzazione dei risultati
print("Statistiche per le Vendite:")
print(statistiche_vendite)

print("Statistiche per i Ricavi:")
print(statistiche_ricavi)


#
#
# REGRESSIONE LINEARE

# Estrazione dei dati
vendite_assistenza_riparazioni <- rowSums(data[, c("Componenti_Assistenza", "Riparazioni")])
ricavi_assistenza_riparazioni <- rowSums(data[, c("Ricavi_Componenti", "Ricavi_Riparazioni")])

# Creazione del modello di regressione lineare
modello <- lm(ricavi_assistenza_riparazioni ~ vendite_assistenza_riparazioni)

# Riassunto del modello
summary(modello)

# Creazione del grafico con linea di regressione e griglia
plot(vendite_assistenza_riparazioni, ricavi_assistenza_riparazioni,
     main = "Regressione Lineare (Assistenza e Riparazioni)",
     xlab = "Vendite (Assistenza e Riparazioni)",
     ylab = "Ricavi (Assistenza e Riparazioni)",
     col = "blue", pch = 16)

# Aggiunta della griglia
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")

# Aggiunta della linea di regressione
abline(modello, col = "red", lwd = 2)

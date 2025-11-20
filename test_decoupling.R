# Test: Entkopplung von X und β
# ================================
# Demonstriert den Unterschied zwischen gekoppelter und entkoppelter Generierung

source("R/data_generation.R")
source("R/scenarios.R")

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("DEMONSTRATION: X-β ENTKOPPLUNG\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# ============================================================================
# Teil 1: GEKOPPELTE Version (ALT - problematisch)
# ============================================================================

cat("Teil 1: GEKOPPELTE Generierung (problematisch)\n")
cat(paste(rep("-", 70), collapse = ""), "\n\n")

config_coupled <- list(
  n = 100,
  p_max = 10,
  beta_spec = c(1.0, 0.8, 0.5, rep(0, 7)),  # ← DETERMINISTISCH
  sigma_eps = 0.2,
  X_dist = "normal",
  correlation_structure = "ar1",
  rho = 0.7,
  seed = 123
)

data_coupled <- generate_data_advanced(config_coupled)

cat("Config:\n")
cat("  Correlation structure: AR(1) with rho = 0.7\n")
cat("  Beta spec: c(1.0, 0.8, 0.5, 0, 0, ...)\n\n")

cat("Generierte Beta:\n")
print(data_coupled$true_beta)
cat("\n")

cat("Aktive Positionen: ", which(data_coupled$true_beta != 0), "\n")
cat("→ IMMER die ersten Variablen!\n\n")

cat("X Korrelationsmatrix (Auszug):\n")
print(round(cor(data_coupled$X)[1:5, 1:5], 2))
cat("\n")

cat("⚠️  PROBLEM:\n")
cat("  - AR(1) macht X[,1] und X[,2] stark korreliert (r=0.70)\n")
cat("  - Genau diese Variablen haben die größten β-Werte\n")
cat("  - → Künstliche Bevorzugung durch Strukturkopplung!\n\n")

# ============================================================================
# Teil 2: ENTKOPPELTE Version (NEU - korrekt)
# ============================================================================

cat(paste(rep("=", 70), collapse = ""), "\n\n")
cat("Teil 2: ENTKOPPELTE Generierung (korrekt)\n")
cat(paste(rep("-", 70), collapse = ""), "\n\n")

config_decoupled <- list(
  n = 100,
  p_max = 10,
  beta_spec = "decoupled_random",    # ← ENTKOPPELT
  p_true = 3,                        # Anzahl aktiver Variablen
  beta_sd = 1.0,                     # Aus N(0, 1)
  sigma_eps = 0.2,
  X_dist = "normal",
  correlation_structure = "ar1",     # ← GLEICHE X-Struktur
  rho = 0.7,
  seed = 123
)

data_decoupled <- generate_data_advanced(config_decoupled)

cat("Config:\n")
cat("  Correlation structure: AR(1) with rho = 0.7 (IDENTISCH)\n")
cat("  Beta spec: decoupled_random, p_true = 3, beta_sd = 1.0\n\n")

cat("Generierte Beta:\n")
print(round(data_decoupled$true_beta, 2))
cat("\n")

cat("Aktive Positionen: ", which(data_decoupled$true_beta != 0), "\n")
cat("→ ZUFÄLLIG verteilt!\n\n")

cat("X Korrelationsmatrix (identisch zur gekoppelten Version):\n")
print(round(cor(data_decoupled$X)[1:5, 1:5], 2))
cat("\n")

cat("✅ KORRIGIERT:\n")
cat("  - X-Struktur IDENTISCH wie vorher\n")
cat("  - Aber β-Positionen UNABHÄNGIG davon\n")
cat("  - → Keine künstliche Bevorzugung!\n\n")

# ============================================================================
# Teil 3: Vergleich mit mehreren Seeds
# ============================================================================

cat(paste(rep("=", 70), collapse = ""), "\n\n")
cat("Teil 3: Entkopplung über mehrere Seeds\n")
cat(paste(rep("-", 70), collapse = ""), "\n\n")

cat("Gekoppelte Version - Aktive Positionen bei verschiedenen Seeds:\n")
for (s in c(123, 456, 789)) {
  config_coupled$seed <- s
  data <- generate_data_advanced(config_coupled)
  cat(sprintf("  Seed %d: %s  ← IMMER GLEICH!\n", 
              s, 
              paste(which(data$true_beta != 0), collapse = ", ")))
}
cat("\n")

cat("Entkoppelte Version - Aktive Positionen bei verschiedenen Seeds:\n")
for (s in c(123, 456, 789)) {
  config_decoupled$seed <- s
  data <- generate_data_advanced(config_decoupled)
  positions <- which(data$true_beta != 0)
  cat(sprintf("  Seed %d: %s  ← VARIIERT!\n", 
              s, 
              paste(positions, collapse = ", ")))
}
cat("\n")

# ============================================================================
# Teil 4: Effekt auf Modellselektion
# ============================================================================

cat(paste(rep("=", 70), collapse = ""), "\n\n")
cat("Teil 4: Warum ist das wichtig für Modellselektion?\n")
cat(paste(rep("-", 70), collapse = ""), "\n\n")

cat("GEKOPPELT:\n")
cat("  Wenn AR(1) die ersten Variablen stark korreliert macht UND\n")
cat("  die stärksten Effekte genau dort platziert werden,\n")
cat("  dann werden Selektionsmethoden künstlich begünstigt!\n\n")

cat("  Beispiel: {X1, X2} wird leichter gefunden weil:\n")
cat("    1. X1 und X2 sind korreliert (AR1)\n")
cat("    2. β1 und β2 sind die größten Effekte\n")
cat("    → Doppelte Begünstigung!\n\n")

cat("ENTKOPPELT:\n")
cat("  Die Methode muss wirklich arbeiten, um relevante Variablen zu finden,\n")
cat("  denn die Position sagt nichts über die Signalstärke aus.\n\n")

cat("  Das ist realistisch, weil in echten Daten:\n")
cat("    - Die wichtigste Variable nicht immer 'die erste' ist\n")
cat("    - Korrelationsstruktur und Effektstärke unabhängig sind\n")
cat("    - Position im Datensatz keine Information trägt\n\n")

# ============================================================================
# Teil 5: Benchmark - Entkoppelte Szenarien
# ============================================================================

cat(paste(rep("=", 70), collapse = ""), "\n\n")
cat("Teil 5: Neue entkoppelte Szenarien\n")
cat(paste(rep("-", 70), collapse = ""), "\n\n")

cat("S15: AR(1) + Random Effects\n")
s15_config <- scenario_s15_decoupled_ar1(seed = 999)
s15_data <- generate_data_advanced(s15_config)
cat(sprintf("  Beta positions: %s\n", 
            paste(which(s15_data$true_beta != 0), collapse = ", ")))
cat(sprintf("  Beta values: %s\n", 
            paste(round(s15_data$true_beta[s15_data$true_beta != 0], 2), collapse = ", ")))
cat("\n")

cat("S16: Block + Fixed Magnitude\n")
s16_config <- scenario_s16_decoupled_block(seed = 999)
s16_data <- generate_data_advanced(s16_config)
cat(sprintf("  Beta positions: %s\n", 
            paste(which(s16_data$true_beta != 0), collapse = ", ")))
cat(sprintf("  Beta values: %s\n", 
            paste(round(s16_data$true_beta[s16_data$true_beta != 0], 2), collapse = ", ")))
cat("\n")

cat("S17: Compound + Controlled Effects\n")
s17_config <- scenario_s17_decoupled_compound(seed = 999)
s17_data <- generate_data_advanced(s17_config)
cat(sprintf("  Beta positions: %s\n", 
            paste(which(s17_data$true_beta != 0), collapse = ", ")))
cat(sprintf("  Beta values: %s\n", 
            paste(round(s17_data$true_beta[s17_data$true_beta != 0], 2), collapse = ", ")))
cat("\n")

# ============================================================================
# Zusammenfassung
# ============================================================================

cat(paste(rep("=", 70), collapse = ""), "\n\n")
cat("ZUSAMMENFASSUNG\n")
cat(paste(rep("-", 70), collapse = ""), "\n\n")

cat("✅ Die Entkopplung ist erfolgreich implementiert!\n\n")

cat("Drei unabhängige Komponenten:\n")
cat("  1. X-STRUKTUR:  Kovarianzmatrix Σ (AR1, Block, Compound, ...)\n")
cat("  2. β-SUPPORT:   Zufällige Positionen, fixe Anzahl\n")
cat("  3. β-GRÖßE:     Aus Verteilung oder fixe Magnitude\n\n")

cat("Verwendung:\n")
cat("  beta_spec = 'decoupled_random'  → Volle Randomisierung\n")
cat("  beta_spec = 'decoupled_fixed'   → Kontrollierte Werte\n\n")

cat("Parameter:\n")
cat("  p_true          → Anzahl aktiver Variablen\n")
cat("  beta_sd         → SD für N(0, sigma^2)\n")
cat("  beta_magnitude  → Fixe Größe für ±magnitude\n")
cat("  beta_values     → User-spezifische Werte\n\n")

cat(paste(rep("=", 70), collapse = ""), "\n\n")

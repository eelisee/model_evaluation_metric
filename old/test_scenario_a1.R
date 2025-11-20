# Test: Szenario A1 - Baseline Uncorrelated
# ===========================================
# Minimal test case: Unkorrelierte Prädiktoren, sparse beta, Gaussian noise
# ERWARTUNG: M_p findet den true support (absolute Minimum-Anforderung)

source("R/data_generation.R")
source("R/model_evaluation.R")
source("R/selection_rules.R")
source("R/scenarios.R")
source("R/recovery_metrics.R")

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("TEST: SZENARIO A1 - BASELINE UNCORRELATED\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# ============================================================================
# Setup
# ============================================================================

cat("Setup:\n")
cat("  X ~ N(0, I)           → Unkorrelierte Prädiktoren\n")
cat("  β sparse, random      → Entkoppelter Support\n")
cat("  ε ~ N(0, σ²)          → Gaussian Noise\n\n")

cat("Erwartung:\n")
cat("  ✅ M_p sollte den TRUE SUPPORT finden\n")
cat("  ✅ Wenn das nicht klappt, ist alles andere egal!\n\n")

cat(paste(rep("-", 70), collapse = ""), "\n\n")

# ============================================================================
# Test mit verschiedenen Seeds
# ============================================================================

cat("Test mit 5 verschiedenen Seeds:\n\n")

results <- list()

for (i in 1:5) {
  seed <- 1000 + i
  
  # Szenario generieren (JETZT mit fixierten beta_values)
  config <- scenario_a1_baseline_uncorrelated(
    n = 100,
    p_max = 10,
    p_true = 3,
    sigma_eps = 0.2,
    beta_values = c(1.0, 0.8, 0.5),  # ← Fixierte Werte
    seed = seed
  )
  
  # Daten generieren
  data <- generate_data_advanced(config)
  
  # Modelle evaluieren
  subsets <- enumerate_models(p_max = data$meta$p_max, strategy = "full")
  models <- evaluate_all_models_batch(data$X, data$y, subsets, verbose = FALSE)
  
  # Selektion anwenden
  selection <- apply_selection_rule(models)
  
  # Recovery Metrics berechnen
  true_support <- which(data$true_beta != 0)
  recovery <- compute_recovery_metrics(
    selected = selection$mp$selected_model$subset_str,
    true = true_support,
    p_max = data$meta$p_max
  )
  
  # Speichern
  results[[i]] <- list(
    seed = seed,
    true_support = true_support,
    true_beta_values = round(data$true_beta[true_support], 2),
    p_star = selection$mp$p_star,
    selected = selection$mp$selected_model$subset_str,
    F1 = recovery$F1,
    precision = recovery$precision,
    recall = recovery$recall,
    exact_match = recovery$exact_match
  )
  
  # Output
  cat(sprintf("Seed %d:\n", seed))
  cat(sprintf("  True support:    {%s}  (β = %s)\n", 
              paste(true_support, collapse = ", "),
              paste(round(data$true_beta[true_support], 2), collapse = ", ")))
  cat(sprintf("  M_p selected:    p* = %d, subset = {%s}\n",
              selection$mp$p_star,
              selection$mp$selected_model$subset_str))
  cat(sprintf("  Performance:     F1 = %.3f, Precision = %.3f, Recall = %.3f\n",
              recovery$F1, recovery$precision, recovery$recall))
  cat(sprintf("  Exact match:     %s\n", ifelse(recovery$exact_match, "✅ YES", "❌ NO")))
  cat("\n")
}

# ============================================================================
# Zusammenfassung
# ============================================================================

cat(paste(rep("=", 70), collapse = ""), "\n\n")
cat("ZUSAMMENFASSUNG über 5 Wiederholungen:\n\n")

# Statistiken berechnen
f1_scores <- sapply(results, function(r) r$F1)
precision_scores <- sapply(results, function(r) r$precision)
recall_scores <- sapply(results, function(r) r$recall)
exact_matches <- sapply(results, function(r) r$exact_match)
p_stars <- sapply(results, function(r) r$p_star)

cat(sprintf("F1 Score:        Mean = %.3f, SD = %.3f, Range = [%.3f, %.3f]\n",
            mean(f1_scores), sd(f1_scores), min(f1_scores), max(f1_scores)))
cat(sprintf("Precision:       Mean = %.3f, SD = %.3f\n",
            mean(precision_scores), sd(precision_scores)))
cat(sprintf("Recall:          Mean = %.3f, SD = %.3f\n",
            mean(recall_scores), sd(recall_scores)))
cat(sprintf("Exact matches:   %d / 5  (%.0f%%)\n",
            sum(exact_matches), 100 * mean(exact_matches)))
cat(sprintf("Selected p*:     %s\n",
            paste(table(p_stars), collapse = " | ")))

cat("\n")

# ============================================================================
# Diagnose
# ============================================================================

cat(paste(rep("=", 70), collapse = ""), "\n\n")
cat("DIAGNOSE:\n\n")

if (mean(f1_scores) >= 0.9) {
  cat("✅ EXCELLENT: M_p findet fast immer den korrekten Support!\n")
  cat("   → Die Metrik funktioniert im Ideal-Fall wie erwartet.\n\n")
} else if (mean(f1_scores) >= 0.7) {
  cat("⚠️  GOOD BUT NOT PERFECT: M_p findet meist gute Modelle.\n")
  cat("   → Prüfe, ob systematische Fehler vorliegen.\n\n")
} else if (mean(f1_scores) >= 0.5) {
  cat("⚠️  MEDIOCRE: M_p hat Schwierigkeiten im Basis-Szenario.\n")
  cat("   → Das ist besorgniserregend - untersuche die Selektionsregel!\n\n")
} else {
  cat("❌ FAILED: M_p findet nicht den korrekten Support!\n")
  cat("   → KRITISCHES PROBLEM - alles andere ist irrelevant!\n\n")
}

# Detaillierte Diagnose
if (mean(precision_scores) < 0.8) {
  cat("⚠️  Niedrige Precision → Zu viele False Positives (over-selection)\n")
}
if (mean(recall_scores) < 0.8) {
  cat("⚠️  Niedrige Recall → Zu viele False Negatives (under-selection)\n")
}

cat("\n")

# ============================================================================
# Visualisierung (optional)
# ============================================================================

if (require(graphics, quietly = TRUE)) {
  cat(paste(rep("=", 70), collapse = ""), "\n\n")
  cat("VISUALISIERUNG:\n\n")
  
  # Letztes Beispiel für Plot
  config <- scenario_a1_baseline_uncorrelated(seed = 1005)
  data <- generate_data_advanced(config)
  subsets <- enumerate_models(p_max = 10, strategy = "full")
  models <- evaluate_all_models_batch(data$X, data$y, subsets, verbose = FALSE)
  selection <- apply_selection_rule(models)
  
  # Best models by p
  best_models <- selection$best_models
  
  # Plot M_p curve
  par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
  
  # Panel 1: M_p curve
  plot(best_models$p, best_models$Mp, type = "b", pch = 19,
       xlab = "Model Complexity (p)", ylab = "M_p",
       main = "A1: M_p Efficiency Curve",
       col = "steelblue", lwd = 2)
  abline(v = selection$mp$p_star, col = "red", lty = 2, lwd = 2)
  abline(v = data$meta$p_true, col = "darkgreen", lty = 2, lwd = 2)
  legend("topright", 
         legend = c(sprintf("M_p choice (p*=%d)", selection$mp$p_star),
                    sprintf("True (p=%d)", data$meta$p_true)),
         col = c("red", "darkgreen"), lty = 2, lwd = 2, cex = 0.8)
  
  # Panel 2: R² curve
  plot(best_models$p, best_models$R2, type = "b", pch = 19,
       xlab = "Model Complexity (p)", ylab = "R²",
       main = "A1: R² vs Complexity",
       col = "coral", lwd = 2, ylim = c(0, 1))
  abline(v = selection$mp$p_star, col = "red", lty = 2, lwd = 2)
  abline(v = data$meta$p_true, col = "darkgreen", lty = 2, lwd = 2)
  
  cat("Plots erstellt!\n\n")
}

# ============================================================================
# Fazit
# ============================================================================

cat(paste(rep("=", 70), collapse = ""), "\n\n")
cat("FAZIT:\n\n")

cat("Szenario A1 ist der MINIMAL TEST für jede Selektionsmethode:\n")
cat("  • Keine Korrelationen (Σ = I)\n")
cat("  • Keine Heteroskedastizität\n")
cat("  • Lineares Modell\n")
cat("  • Gaussian Noise\n")
cat("  • Sparse β mit entkoppeltem Support\n\n")

if (mean(f1_scores) >= 0.9 && mean(exact_matches) >= 0.6) {
  cat("✅ BESTANDEN: M_p funktioniert im Ideal-Fall!\n")
  cat("   → Bereit für komplexere Szenarien (A2-A7).\n\n")
} else {
  cat("❌ NICHT BESTANDEN: M_p hat Probleme im Basis-Fall!\n")
  cat("   → Behebe dies, bevor du weitermachst.\n\n")
}

cat(paste(rep("=", 70), collapse = ""), "\n\n")

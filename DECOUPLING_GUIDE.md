# Entkopplung von X und β - Implementation Guide

## Problem: Warum war die alte Implementierung gekoppelt?

### ❌ Alte (unrealistische) Implementierung:

```r
# PROBLEM 1: Deterministische β-Positionierung
beta_spec = c(1.0, 0.8, 0.5, 0, 0, 0, ...)
# → Erste Variable hat IMMER den stärksten Effekt

# PROBLEM 2: X-Struktur trivial
correlation_structure = "identity"
# → Keine realistische Redundanz oder Information

# ERGEBNIS: 
# - Die Position im Index bestimmt die Signalstärke
# - Bei AR(1) sind vordere Variablen automatisch bevorzugt
# - Unrealistische Kopplung zwischen Datenstruktur und Effekten
```

### ✅ Warum ist das problematisch?

1. **Unrealistische Szenarien**: In echten Daten ist die wichtigste Variable nicht immer "die erste"
2. **Verzerrte Evaluierung**: Methoden werden an künstlich einfachen Fällen getestet
3. **Strukturelle Kopplung**: AR(1)-Korrelation begünstigt automatisch die ersten Variablen

---

## Lösung: Drei unabhängige Komponenten

### 🎯 Prinzip der Entkopplung

```
┌─────────────────────────────────────────────────────────────┐
│  Drei Dinge müssen UNABHÄNGIG variiert werden:              │
│                                                              │
│  1. STRUKTUR VON X  (Σ - Kovarianzmatrix)                   │
│     → AR(1), Block, Compound, Identity                      │
│     → Bestimmt Redundanz und Informationszuwachs            │
│                                                              │
│  2. SUPPORT VON β  (welche Variablen aktiv)                 │
│     → Zufällige Positionen, fixe Anzahl                     │
│     → KEINE Kopplung mit X-Index                            │
│                                                              │
│  3. GRÖSSE VON β  (Effektstärken)                           │
│     → Aus Verteilung oder fixe Magnitude                    │
│     → NICHT deterministisch nach Position sortiert          │
└─────────────────────────────────────────────────────────────┘
```

---

## Neue Implementation

### ✅ Methode 1: `beta_spec = "decoupled_random"`

**Vollständig randomisierte Effekte**

```r
scenario_example <- function(seed = 123) {
  list(
    scenario_name = "decoupled_ar1",
    beta_spec = "decoupled_random",    # ← NEU
    p_true = 5,                        # Anzahl aktiver Variablen
    beta_sd = 1.0,                     # Effekte aus N(0, 1)
    correlation_structure = "ar1",
    rho = 0.7,
    seed = seed
  )
}
```

**Was passiert intern:**

```r
# Schritt 1: Support zufällig wählen
beta_support <- sample(1:p_max, p_true, replace = FALSE)
# z.B. [3, 7, 2, 9, 5] - KEINE Sortierung!

# Schritt 2: Effektstärken generieren
beta_values <- rnorm(p_true, mean = 0, sd = beta_sd)
# z.B. [0.82, -1.34, 0.45, 1.12, -0.67]

# Schritt 3: β-Vektor konstruieren
true_beta <- rep(0, p_max)
true_beta[beta_support] <- beta_values
# → [0, 0.45, 0.82, 0, -0.67, 0, -1.34, 0, 1.12, 0]
```

**Eigenschaften:**
- ✅ Support-Positionen unabhängig von Σ
- ✅ Effektgrößen realistisch variabel
- ✅ Vorzeichen zufällig
- ⚠️ Jede Wiederholung hat andere β-Werte

---

### ✅ Methode 2: `beta_spec = "decoupled_fixed"`

**Kontrollierte Effekte mit zufälligen Positionen**

```r
scenario_example <- function(seed = 123) {
  list(
    beta_spec = "decoupled_fixed",
    p_true = 3,
    beta_values = c(1.0, 0.8, 0.5),   # ← Fixe Werte
    beta_magnitude = NULL,             # Alternative: beta_magnitude = 1.0
    correlation_structure = "compound",
    rho = 0.5,
    seed = seed
  )
}
```

**Was passiert intern:**

```r
# Schritt 1: Support zufällig
beta_support <- sample(1:10, 3, replace = FALSE)
# z.B. [7, 2, 9]

# Schritt 2: User-spezifizierte Werte zuweisen
beta_values <- c(1.0, 0.8, 0.5)

# Schritt 3: Randomisierte Vorzeichen (optional)
beta_values <- beta_values * sample(c(-1, 1), 3, replace = TRUE)
# z.B. [-1.0, 0.8, -0.5]

# Schritt 4: β-Vektor
true_beta[7] <- -1.0
true_beta[2] <- 0.8
true_beta[9] <- -0.5
```

**Eigenschaften:**
- ✅ Effektgrößen kontrolliert (reproduzierbar)
- ✅ Positionen randomisiert (entkoppelt)
- ✅ Optional: Vorzeichen randomisiert
- ✅ Vergleichbare Signalstärke über Seeds

---

## Vergleich: Alt vs. Neu

| Aspekt | ❌ Alt (`"descending"`) | ✅ Neu (`"decoupled_random"`) |
|--------|------------------------|-------------------------------|
| β-Positionen | Immer [1,2,3,...] | Zufällig aus 1:p_max |
| Effektgrößen | Deterministisch [1.0, 0.8, 0.5] | Aus N(0, σ²) oder ±magnitude |
| Vorzeichen | Immer positiv | Randomisiert |
| Kopplung mit Σ | ⚠️ Stark gekoppelt | ✅ Vollständig entkoppelt |
| Realismus | Künstlich | Realistisch |
| AR(1)-Bias | Ja (erste Variablen bevorzugt) | Nein |

---

## Beispiel-Szenarien

### S15: AR(1) mit randomisierten Effekten

```r
scenario_s15_decoupled_ar1(
  n = 100,
  p_max = 10,
  p_true = 5,           # 5 aktive Variablen
  rho = 0.7,            # AR(1) Korrelation
  beta_sd = 1.0,        # Effekte aus N(0,1)
  sigma_eps = 0.2
)
```

**Resultat:**
- X hat AR(1)-Struktur: Σ[i,j] = 0.7^|i-j|
- 5 zufällige Positionen sind aktiv
- Effekte sind N(0,1)-verteilt
- KEINE Bevorzugung früher Indizes

### S16: Block-Struktur mit fixer Magnitude

```r
scenario_s16_decoupled_block(
  p_true = 5,
  rho = 0.6,
  block_size = 3,
  beta_magnitude = 1.0  # Alle Effekte ±1.0
)
```

**Resultat:**
- X hat Block-Struktur (3er-Gruppen mit ρ=0.6)
- 5 Variablen aktiv an zufälligen Positionen
- Alle Effekte haben Größe 1.0
- Vorzeichen ±1 zufällig

### S17: Compound Symmetry mit kontrollierten Werten

```r
scenario_s17_decoupled_compound(
  p_true = 3,
  rho = 0.5,
  beta_values = c(1.0, 0.8, 0.5)
)
```

**Resultat:**
- X hat Compound Symmetry (alle Paare ρ=0.5)
- Genau 3 Variablen aktiv
- Effekte exakt [1.0, 0.8, 0.5]
- Aber an zufälligen Positionen!

---

## Seeds und Reproduzierbarkeit

### ⚠️ Wichtig: Seed-Management

```r
# Für identische Vergleiche zwischen Metriken (M_p, AIC, BIC):
config <- list(
  seed = 123,              # ← Hauptseed für X-Generierung
  beta_spec = "decoupled_random"
)

# Intern:
# 1. set.seed(123) → X generiert
# 2. set.seed(123) → β-Support und Werte generiert
# 3. Gleicher Seed = gleiche Daten für alle Metriken
```

### ✅ Best Practice für Experimente

```r
# Experiment mit 10 Wiederholungen
results <- list()
for (rep in 1:10) {
  config <- scenario_s15_decoupled_ar1(seed = 1000 + rep)
  results[[rep]] <- run_experiment(config)
}

# Jede Wiederholung hat:
# - Unterschiedliche X-Realisierung
# - Unterschiedliche β-Positionen
# - Aber konsistente Vergleichbarkeit innerhalb jeder Rep
```

---

## Migration von alten Szenarien

### Schritt 1: Identifiziere gekoppelte Szenarien

```r
# ❌ ALT - GEKOPPELT
scenario_s6_collinearity <- function(...) {
  list(
    beta_spec = c(1.0, 0.8, 0.5, rep(0, 7)),  # ← Deterministisch
    correlation_structure = "ar1",
    rho = 0.8
  )
}
```

### Schritt 2: Konvertiere zu entkoppelt

```r
# ✅ NEU - ENTKOPPELT
scenario_s6_decoupled <- function(...) {
  list(
    beta_spec = "decoupled_fixed",            # ← Entkoppelt
    p_true = 3,
    beta_values = c(1.0, 0.8, 0.5),           # Gleiche Werte
    correlation_structure = "ar1",
    rho = 0.8
  )
}
```

### Schritt 3: Vergleiche Ergebnisse

```r
# Alte Version
result_old <- run_experiment(scenario_s6_collinearity(seed = 123))

# Neue Version  
result_new <- run_experiment(scenario_s6_decoupled(seed = 123))

# Erwartung:
# - Neue Version: realistischere Herausforderung
# - F1-Scores könnten unterschiedlich sein
# - Aber: faire Evaluierung der Methoden
```

---

## Checkliste: Ist mein Szenario entkoppelt?

✅ **Ein Szenario ist korrekt entkoppelt, wenn:**

- [ ] Σ-Struktur kommt allein aus `correlation_structure` + `rho`
- [ ] β-Support ist NICHT an Index-Positionen gekoppelt
- [ ] β-Größen sind NICHT deterministisch sortiert
- [ ] AR(1)-Struktur bevorzugt KEINE frühen Indizes
- [ ] Seeds garantieren Reproduzierbarkeit
- [ ] Vergleiche zwischen Metriken verwenden identische Daten

❌ **Warnsignale für Kopplung:**

- [ ] `beta_spec = c(1.0, 0.8, 0.5, ...)` mit fixen Positionen
- [ ] Erste Variablen haben immer stärkste Effekte
- [ ] AR(1) oder Block-Struktur korreliert mit β-Größe
- [ ] Effekte haben keine Vorzeichenvariabilität

---

## Zusammenfassung

**Das Kernprinzip:**

> **X trägt die Struktur, β trägt das Signal - aber beide sind unabhängig!**

**Die drei Schlüsseländerungen:**

1. **`beta_spec = "decoupled_random"`** oder **`"decoupled_fixed"`**
2. **`p_true`** gibt Supportgröße an
3. **`beta_sd`**, **`beta_magnitude`**, oder **`beta_values`** kontrollieren Effekte

**Das Ergebnis:**

- ✅ Realistische Simulationen
- ✅ Faire Methodenevaluierung
- ✅ Keine künstlichen Vorteile durch Indexstruktur
- ✅ Vergleichbarkeit bleibt erhalten

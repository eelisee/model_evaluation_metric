# Entkoppelte β-Generierung - Quick Start

## Problem

Die ursprüngliche Implementierung hatte eine **unrealistische Kopplung**:

```r
# ❌ PROBLEM: Deterministische β-Positionen
beta_spec = c(1.0, 0.8, 0.5, 0, 0, ...)  # Erste Variablen immer am stärksten
correlation_structure = "ar1", rho = 0.7  # AR(1) korreliert erste Variablen stark
```

→ **Doppelte Begünstigung**: Stärkste Effekte UND stärkste Korrelation bei gleichen Variablen!

## Lösung

Drei unabhängige Komponenten:

1. **X-Struktur** (Σ): AR(1), Block, Compound → bestimmt Redundanz
2. **β-Support**: Zufällige Positionen → keine Index-Kopplung  
3. **β-Größe**: Aus Verteilung → keine deterministische Sortierung

## Verwendung

### Option 1: Vollständig randomisiert

```r
scenario <- list(
  beta_spec = "decoupled_random",
  p_true = 5,              # 5 aktive Variablen
  beta_sd = 1.0,           # Effekte aus N(0, 1)
  correlation_structure = "ar1",
  rho = 0.7
)
```

### Option 2: Kontrollierte Effekte

```r
scenario <- list(
  beta_spec = "decoupled_fixed",
  p_true = 3,
  beta_values = c(1.0, 0.8, 0.5),  # Fixe Werte, random Positionen
  correlation_structure = "block",
  rho = 0.6
)
```

### Option 3: Fixe Magnitude

```r
scenario <- list(
  beta_spec = "decoupled_random",
  p_true = 5,
  beta_magnitude = 1.0,    # Alle Effekte ±1.0
  correlation_structure = "compound",
  rho = 0.5
)
```

## Neue Szenarien

```r
# S15: AR(1) + Random Effects
source("R/scenarios.R")
config <- scenario_s15_decoupled_ar1(rho = 0.7, beta_sd = 1.0)

# S16: Block + Fixed Magnitude  
config <- scenario_s16_decoupled_block(rho = 0.6, beta_magnitude = 1.0)

# S17: Compound + Controlled Effects
config <- scenario_s17_decoupled_compound(rho = 0.5, beta_values = c(1, 0.8, 0.5))
```

## Test

```bash
Rscript test_decoupling.R
```

## Dokumentation

- **Vollständige Anleitung**: `DECOUPLING_GUIDE.md`
- **Implementation**: `R/data_generation.R` (Zeilen 163-232)
- **Szenarien**: `R/scenarios.R` (S15, S16, S17)

## Vergleich

| Feature | Alt | Neu |
|---------|-----|-----|
| β-Positionen | Immer [1,2,3] | Zufällig |
| Effektgrößen | [1.0, 0.8, 0.5] fix | Aus N(0,σ²) |
| Vorzeichen | Immer + | ±1 random |
| Kopplung mit Σ | ⚠️ Stark | ✅ Keine |
| Realismus | Künstlich | Realistisch |

## Checkliste

✅ Ein Szenario ist entkoppelt wenn:
- [ ] `beta_spec = "decoupled_random"` oder `"decoupled_fixed"`
- [ ] β-Support zufällig via `sample(1:p_max, p_true)`
- [ ] Effekte aus Verteilung oder mit random signs
- [ ] X-Struktur unabhängig von β-Positionen

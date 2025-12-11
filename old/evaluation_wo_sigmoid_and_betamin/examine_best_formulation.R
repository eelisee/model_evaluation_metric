# Examine the best formulation in detail

detailed <- read.csv('mp_formulation_analysis_detailed.csv')

# Filter for best formulation: delta3_central + zero_crossing_ceil
best <- detailed[detailed$formulation == 'delta3_central' & 
                 detailed$selection_rule == 'zero_crossing_ceil', ]

cat('\n=== DELTA3_CENTRAL + ZERO_CROSSING_CEIL (Best Formulation) ===\n\n')
cat('Results by scenario:\n\n')
print(best[, c('scenario', 'p_selected', 'p_true', 'p_bic', 'is_correct', 'error_true')])

cat('\n\nSuccess scenarios:\n')
print(best[best$is_correct, 'scenario'])

cat('\n\nFailure scenarios:\n')
failures <- best[!best$is_correct, ]
print(failures[, c('scenario', 'p_selected', 'p_true', 'p_bic')])

cat('\n\nNote: p_selected = -1 means NA (no zero crossing found)\n')

# Also check what the current implementation selects
cat('\n\n=== COMPARISON WITH CURRENT IMPLEMENTATION ===\n\n')

# The current implementation uses: argmin(delta3) 
# Let's check that one too
current <- detailed[detailed$formulation == 'delta3_central' & 
                    detailed$selection_rule == 'min', ]

cat('Current implementation (delta3_central + min):\n\n')
print(current[, c('scenario', 'p_selected', 'p_true', 'p_bic', 'is_correct', 'error_true')])

cat('\n\nComparison:\n')
cat(sprintf('Best formulation (zero_crossing_ceil): %.1f%% correct\n', mean(best$is_correct) * 100))
cat(sprintf('Current implementation (min): %.1f%% correct\n', mean(current$is_correct) * 100))




## estimate percentage Fishoil from industry reports
# EWOS Cargill Aqua Nutrition for total marine ingredients globally	32.30%
# Biomar for fish oil globally 	42%
# Mowi for fish oil in Scotland	4.90%

fo <- data.frame(source = c('cargill', 'biomar', 'mowi'), 
				estimate = c(0.323, 0.42, 0.049), 
				contribution = c(0.4, 0.4, 0.2))

# weighted average for Scottish salmon
weighted.mean(fo$estimate, fo$contribution) ## 0.31


## update from Karen 
fo <- data.frame(source = c('cargill', 'biomar', 'mowi'), 
				estimate = c(0.28, 0.295, 0.049), 
				contribution = c(0.4, 0.4, 0.2))

# weighted average for Scottish salmon
weighted.mean(fo$estimate, fo$contribution) ## 0.24
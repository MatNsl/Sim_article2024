### Premier essai avec ChatGPT -----

# Générer la séquence
sequence <- seq(200, 300, by = .01)

# Tester l'appartenance
is_in_sequence <- 297.21 %in% sequence

# Afficher le résultat
print(is_in_sequence)

### Second essai avec ChatGPT -----

# Générer la séquence
sequence <- seq(234, 300, by = .01)

# Tester l'appartenance
is_in_sequence <- 297.21 %in% sequence

# Afficher le résultat
print(is_in_sequence) # TRUE with 234, FALSE with 233

# Afficher une partie de la séquence autour de 297.21 pour vérifier la présence
nearby_values <- sequence[which(abs(sequence - 297.21) < .001)]
print(nearby_values) # Dans tous les cas, affiche 297.21


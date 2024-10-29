# Liste de 50 mots en français
mots_francais <- c("bonjour", "chocolat", "maison", "école", "soleil", "ordinateur", "voiture", "chat", "fleur", "livre", 
                   "montagne", "plage", "oiseau", "musique", "bouteille", "fenêtre", "table", "amour", "ciel", "eau",
                   "arbre", "forêt", "lumière", "chaise", "jardin", "chemin", "route", "étoile", "verre", "clé",
                   "horloge", "château", "pont", "rivière", "mer", "montre", "téléphone", "parapluie", "chaussure", "lampe",
                   "village", "feuille", "bureau", "camion", "pain", "gâteau", "roue", "ballon", "crayon", "papillon")

# Générer 10 mots aléatoires
mots_aleatoires <- sample(mots_francais, 10, replace = FALSE)

# Afficher les mots aléatoires
print(mots_aleatoires)
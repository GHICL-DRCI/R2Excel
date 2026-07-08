# Pour faire un schéma fonctionnel des fonctions qui appelle les fonctions... 
# pour expliquer les dépendances entre les fonctions dans le pkg et
# aider quand on fait un changement, à aller voir les répercutions

pkg_path <- "X:/DRCI Methodologie/BIOSTATISTIQUES/R2Excel"

####  foodweb ####
## du package mvbutils

# install.packages("mvbutils")
library(mvbutils)

# Charger ton package
library(R2Excel)
packageVersion("R2Excel") # ‘0.2.0’
# Générer le schéma fonctionnel
?foodweb()

# Partir d'une fonction spécifique
foodweb(
  where = "package:R2Excel",
  prune = "save_excel_results",  
  descendents = TRUE
)
foodweb(
  where = "package:R2Excel",
  prune = "save_excel_paired_results",
  descendents = TRUE
)
foodweb(
  where = "package:R2Excel",
  descendents = TRUE, 
  recursive = TRUE
)
foodweb(
  where = "package:R2Excel"
)

## A) Ajouter dans quel script se trouve quel fonction 

# Lancer foodweb et récupérer l'objet
fw <- foodweb(
  where = "package:R2Excel",
  descendents = TRUE,
  plotting = FALSE  # Ne pas plotter encore
)
# Créer un mapping fonction : fichier
r_files <- list.files(
  file.path(pkg_path, "R/"), pattern = "\\.R$",
  full.names = TRUE
)

# Construire la table de correspondance
func_file_map <- data.table::rbindlist(lapply(r_files, function(file) {
  lines <- readLines(file)
  # Détecter les définitions de fonctions
  func_lines <- grep("^[a-zA-Z0-9_.]+\\s*<-\\s*function\\s*\\(", lines, value = TRUE)
  func_names <- sub("\\s*<-\\s*function.*", "", func_lines)
  func_names <- trimws(func_names)
  
  if (length(func_names) > 0) {
    data.table::data.table(
      func = func_names,
      file = basename(file)
    )
  }
}))

print(func_file_map)
func_file_map[order(file), ]

# B) Ajouter les labels au graphe foodweb

# Mapping couleur par fichier

dput(unique(func_file_map$file))
# c("compute_date_tables.R", "compute_OR_tables.R", "compute_paired_tables.R", 
#   "compute_SMD_tables.R", "compute_tables.R", "excel_file_paired_production.R", 
#   "excel_file_production.R", "tests_stats.R", "utils.R")

file_colors <- c(
  "excel_file_production.R"        = "#E74C3C",  # Rouge
  "excel_file_paired_production.R" = "#E67E22",  # Orange
  "compute_tables.R"               = "#3498DB",  # Bleu
  "compute_paired_tables.R"        = "#2980B9",  # Bleu foncé
  "compute_SMD_tables.R"           = "#27AE60",  # Vert
  "compute_OR_tables.R"            = "#2ECC71",  # Vert clair
  "compute_date_tables.R"          = "#9B59B6",  # Violet
  "tests_stats.R"                  = "#F39C12",  # Jaune
  "utils.R"                        = "#95A5A6"   # Gris
)


# Construire le mapping fonction → fichier + couleur
r_files <- list.files(
  file.path(pkg_path, "R/"), pattern = "\\.R$", 
  full.names = TRUE
)

# Récupérer les noms de fonctions dans le graphe

func_file_map <- data.table::rbindlist(lapply(r_files, function(file) {
  lines <- readLines(file)
  func_lines <- grep("^[a-zA-Z0-9_.]+\\s*<-\\s*function\\s*\\(", lines, value = TRUE)
  func_names <- trimws(sub("\\s*<-\\s*function.*", "", func_lines))
  if (length(func_names) > 0) {
    data.table::data.table(
      func  = func_names,
      file  = basename(file),
      color = file_colors[basename(file)]
    )
  }
}))

# Générer le foodweb SANS plot
fw <- foodweb(
  where    = "package:R2Excel",
  descendents = TRUE,
  plotting = FALSE
)
# Utiliser dimnames(fw$funmat) ? 
funcs_in_graph <- dimnames(fw$funmat)$MASTER
# cat("Fonctions dans le graphe :\n")
# print(funcs_in_graph)

# Assigner les couleurs aux noeuds
node_colors <- sapply(funcs_in_graph, function(fn) {
  match_idx <- which(func_file_map$func == fn)
  if (length(match_idx) > 0) {
    func_file_map$color[match_idx[1]]
  } else {
    "#BDC3C7"  # Gris par défaut si non trouvé
  }
})

# Vérifier le mapping
mapping_check <- data.frame(
  func  = funcs_in_graph,
  color = node_colors,
  file  = sapply(funcs_in_graph, function(fn) {
    match_idx <- which(func_file_map$func == fn)
    if (length(match_idx) > 0) func_file_map$file[match_idx[1]] else "NOT FOUND"
  })
)
print(mapping_check)

# # Plotter avec les couleurs
# plot(
#   fw,
#   boxcolor = node_colors
# )
# # Ajouter la légende
# legend(
#   x      = "bottomleft",
#   legend = names(file_colors),
#   fill   = file_colors,
#   title  = "Script source",
#   cex    = 0.7,
#   bty    = "n"
# )

png(
  filename = "R2Excel_foodweb.png",
  width    = 1800,
  height   = 1200,  # Augmenter la hauteur pour la légende
  res      = 150
)
# Layout : 2 zones (graphe + légende)
layout(
  mat    = matrix(c(1, 2), nrow = 2),
  heights = c(0.85, 0.15)  # 85% graphe, 15% légende
)
# zone 1
par(mar = c(1, 1, 2, 1))
foodweb(
  where = "package:R2Excel",
  descendents = TRUE,
  plotting = TRUE,
  boxcolor = node_colors
)
# zone 2
par(mar = c(0, 0, 0, 0))
# plot.new()
legend(
  x      = "bottomleft",
  legend = names(file_colors),
  fill   = file_colors,
  title  = "Script source",
  cex    = 0.6,
  bty    = "n"
)
dev.off()

message("Graph sauvegardé : R2Excel_foodweb.png. ici : ")
message(getwd())

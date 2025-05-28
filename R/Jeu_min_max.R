# script.R# Importations

# Données
CHEMIN_DOSSIER <- if(length(grep("--file=", commandArgs(trailingOnly = FALSE))) > 0){
	dirname(normalizePath(sub("--file=", "", commandArgs(trailingOnly = FALSE)[grep("--file=", commandArgs(trailingOnly = FALSE))[1]])))
}else{
	getwd()
}
FICHIER_OPTIONS <- file.path(CHEMIN_DOSSIER, "minmax_options.txt")

# Fonctions utilitaires
## Lecture d'un argument de commande
#' Lit un entier sécurisé entre min et max
#' @param message Message d'invite
#' @param min Valeur minimale
#' @param max Valeur maximale
lire_entier <- function(message, min, max){
	while(TRUE){
		cat(message)
		flush.console()
		entree <- tryCatch(readLines(file("stdin"), 1), error = function(exception_entree){""})
		entree <- trimws(entree)
		if(grepl("^\\d+$", entree) &&
		   as.integer(entree) >= min &&
		   as.integer(entree) <= max){
			return(as.integer(entree))
		}
		cat("\nVeuillez entrer un entier entre ", min, " et ", max, ".\n", sep = "")
	}
}

## Chargement des options sauvegardées
#' Charge les options ou renvoie les valeurs par défaut
charger_options <- function(){
	if(file.exists(FICHIER_OPTIONS)){
		donnees <- as.integer(strsplit(readLines(FICHIER_OPTIONS, warn = FALSE), ",")[[1]])
		if(length(donnees) == 3 && !any(is.na(donnees))){
			return(donnees)
		}
	}
	c(1, 100, 5)
}

## Sauvegarde des options
#' Sauvegarde les paramètres
#' @param min Valeur minimale
#' @param max Valeur maximale
#' @param tours Nombre de tours
sauvegarder_options <- function(min, max, tours){
	writeLines(paste(c(min, max, tours), collapse = ","), FICHIER_OPTIONS, useBytes = TRUE)
}

# Fonctions principales
#' Affiche le menu principal
afficher_menu_principal <- function(){
	cat("\n#### Menu ####\n1 : Jouer\n2 : Options\n3 : Quitter\n")
}

#' Affiche le menu d'options
afficher_menu_options <- function(min, max, tours){
	cat("\n#### Menu options ####\n1 : Choisir les limites (actuellement : ", min, " - ", max,
	    ")\n2 : Nombre de tours max (actuellement : ", tours, ")\n", sep = "")
}

#' Joue une partie et indique plus grand/petit
jouer_partie <- function(min, max, tours){
	cible <- sample(min:max, 1)
	cat("\nTrouvez entre ", min, " et ", max, " en ", tours, " tours\n", sep = "")
	for(essai_numero in 1:tours){
		choix <- lire_entier(paste0("Tour ", essai_numero, " : "), min, max)
		if(choix == cible){
			cat("\nGagné !\n")
			return(invisible(NULL))
		}else if(choix > cible){
			cat("-\n")
		}else{
			cat("+\n")
		}
	}
	cat("\nPerdu ! La réppnse était ", cible, ".\n", sep = "")
}

#' Gère les options et sauvegarde si modifiées
gerer_options <- function(min, max, tours){
	afficher_menu_options(min, max, tours)
	choix_options <- lire_entier("? = ", 1, 2)
	if(choix_options == 1){
		min <- lire_entier("\nMin = ", 1, max)
		max <- lire_entier("\nMax = ", min, 10000)
	}else if(choix_options == 2){
		tours <- lire_entier("\nTours max = ", 1, 100)
	}
	sauvegarder_options(min, max, tours)
	c(min, max, tours)
}

# Main
main <- function(){
	parametres <- charger_options()
	min <- parametres[1]; max <- parametres[2]; tours <- parametres[3]
	repeat{
		afficher_menu_principal()
		choix_menu <- lire_entier("? = ", 1, 3)
		if(choix_menu == 1){
			jouer_partie(min, max, tours)
		}else if(choix_menu == 2){
			variables <- gerer_options(min, max, tours)
			min <- variables[1]; max <- variables[2]; tours <- variables[3]
		}else if(choix_menu == 3){
			break
		}
	}
}

# Lancement du programme
main()

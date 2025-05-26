#!/usr/bin/env bash

# Données
CHEMIN_DOSSIER=$(dirname "$0")
FICHIER_OPTIONS="${CHEMIN_DOSSIER}/minmax_options.txt"

# Importations

# Fonctions utilitaires
## Lecture d'un entier utilisateur sécurisé
#"""
#	Lit un entier dans une plage
#	Invite jusqu’à une saisie valide
#	@message	texte affiché
#	@min		borne minimale
#	@max		borne maximale
#"""
lire_entier() {
	local message="$1" min="$2" max="$3" entree
	while true; do
		printf "%s" "$message" >&2
		read -r entree
		if [[ "$entree" =~ ^[0-9]+$ ]] && ((entree >= min && entree <= max)); then
			echo "$entree"
			return
		fi
		echo -e "\nVeuillez entrer un entier entre $min et $max." >&2
	done
}

## Chargement des options sauvegardées
#"""
#	Charge les paramètres depuis fichier, sinon défaut
#"""
charger_options() {
	if [[ -f "$FICHIER_OPTIONS" ]]; then
		IFS=',' read -r min max tours <"$FICHIER_OPTIONS"
		if [[ -n "$tours" ]]; then
			OPTIONS_MIN=$min
			OPTIONS_MAX=$max
			OPTIONS_TOURS=$tours
			return
		fi
	fi
	OPTIONS_MIN=1
	OPTIONS_MAX=100
	OPTIONS_TOURS=5
}

## Sauvegarde des options
#"""
#	Sauvegarde les paramètres dans le fichier
#"""
sauvegarder_options() {
	local min="$1" max="$2" tours="$3"
	echo "${min},${max},${tours}" >"$FICHIER_OPTIONS"
}

# Fonctions principales

# Affiche le menu principal
#"""
#	Affiche le menu principal du jeu
#"""
afficher_menu_principal() {
	echo -e "\n#### Menu ####\n1 : Jouer\n2 : Options\n3 : Quitter"
}

# Affiche le menu d'options
#"""
#	Affiche le menu d’options actuelles
#"""
afficher_menu_options() {
	local min="$1" max="$2" tours="$3"
	echo -e "\n#### Menu options ####\n1 : Choisir les limites (actuellement : ${min} - ${max})\n2 : Nombre de tours max (actuellement : ${tours})"
}

# Joue une partie
#"""
#	Fait deviner un nombre avec indication + ou -
#"""
jouer_partie() {
	local min="$1" max="$2" tours="$3"
	local cible=$((RANDOM % (max - min + 1) + min))
	echo -e "\nTrouvez entre ${min} et ${max} en ${tours} tours"
	for ((essai = 1; essai <= tours; essai++)); do
		local choix
		choix=$(lire_entier "Tour ${essai} : " "$min" "$max")
		if ((choix == cible)); then
			echo -e "\nGagné !"
			return
		elif ((choix > cible)); then
			echo "-"
		else
			echo "+"
		fi
	done
	echo -e "\nPerdu ! La réppnse était ${cible}."
}

# Gère les options
#"""
#	Gère les modifications des options
#"""
gerer_options() {
	local min="$1" max="$2" tours="$3" choix
	afficher_menu_options "$min" "$max" "$tours"
	choix=$(lire_entier "? = " 1 2)
	if [[ "$choix" == 1 ]]; then
		echo
		min=$(lire_entier "Min = " 1 "$max")
		max=$(lire_entier "Max = " "$min" 10000)
	elif [[ "$choix" == 2 ]]; then
		echo
		tours=$(lire_entier "Tours max = " 1 100)
	fi
	sauvegarder_options "$min" "$max" "$tours"
	OPTIONS_MIN=$min
	OPTIONS_MAX=$max
	OPTIONS_TOURS=$tours
}

# Main
#"""
#	Fonction principale exécutée au lancement
#"""
main() {
	charger_options
	local min=$OPTIONS_MIN max=$OPTIONS_MAX tours=$OPTIONS_TOURS
	while true; do
		afficher_menu_principal
		local choix
		choix=$(lire_entier "? = " 1 3)
		# echo "DEBUG: choix='$choix'" # <-- pour vérifier la saisie
		case "$choix" in
		1) jouer_partie "$min" "$max" "$tours" ;;
		2)
			gerer_options "$min" "$max" "$tours"
			min=$OPTIONS_MIN
			max=$OPTIONS_MAX
			tours=$OPTIONS_TOURS
			;;
		3) break ;;
		*) echo "Option invalide" ;;
		esac
	done
}

# Lancement du programme
main

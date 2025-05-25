

# Importations
import os
import random
import re

# Données
CHEMIN_DOSSIER = os.path.dirname(os.path.abspath(__file__))
FICHIER_OPTIONS = os.path.join(CHEMIN_DOSSIER, "minmax_options.txt")

# Fonctions utilitaires
## Lecture d'un argument de commande

"""
	Lecture sécurisée d'un entier depuis l'utilisateur
	Contrôle l'appartenance à un intervalle [minimum, maximum]
	@message message à afficher
	@minimum borne inférieure
	@maximum borne supérieure
"""
def lire_entier(message, minimum, maximum):
	while True:
		print(message, end='')
		entree = input().strip()
		if re.fullmatch(r"\d+", entree) and minimum <= int(entree) <= maximum:
			return int(entree)
		print(f"\nVeuillez entrer un entier entre {minimum} et {maximum}.")

# Fonctions utilitaires principales
## Création d'une tâche

"""
	Charge les options sauvegardées depuis le fichier
	Retourne une liste de trois entiers [minimum, maximum, nombre_tours]
	@return liste d'options
"""
def charger_options():
	if os.path.exists(FICHIER_OPTIONS):
		with open(FICHIER_OPTIONS, "r", encoding="utf-8") as fichier_options:
			donnees = fichier_options.read().split(",")
		options = [int(valeur) for valeur in donnees if valeur.isdigit()]
		if len(options) == 3:
			return options
	return [1, 100, 5]

"""
	Sauvegarde les options (minimum, maximum, nombre_tours) dans le fichier
	Remplace l'ancien contenu
	@minimum borne inférieure
	@maximum borne supérieure
	@nombre_tours nombre de tours
"""
def sauvegarder_options(minimum, maximum, nombre_tours):
	with open(FICHIER_OPTIONS, "w", encoding="utf-8") as fichier_options:
		fichier_options.write(f"{minimum},{maximum},{nombre_tours}")

# Fonctions principales

"""
	Affiche le menu principal du programme
"""
def afficher_menu_principal():
	print("\n#### Menu ####\n1 : Jouer\n2 : Options\n3 : Quitter")

"""
	Affiche le menu d'options avec valeurs courantes
	@minimum borne inférieure
	@maximum borne supérieure
	@nombre_tours nombre de tours
"""
def afficher_menu_options(minimum, maximum, nombre_tours):
	print(f"\n#### Menu options ####\n1 : Choisir les limites (actuellement : {minimum} - {maximum})\n2 : Nombre de tours max (actuellement : {nombre_tours})")

"""
	Joue une partie, affiche + ou - selon la réponse
	@minimum borne inférieure
	@maximum borne supérieure
	@nombre_tours nombre de tours
"""
def jouer_partie(minimum, maximum, nombre_tours):
	cible = random.randint(minimum, maximum)
	print(f"\nTrouvez entre {minimum} et {maximum} en {nombre_tours} tours")
	for numero_essai in range(1, nombre_tours + 1):
		choix = lire_entier(f"Tour {numero_essai} : ", minimum, maximum)
		if choix == cible:
			print("\nGagné !")
			return
		elif choix > cible:
			print("-")
		else:
			print("+")
	print(f"\nPerdu ! La réppnse était {cible}.")

"""
	Gère le menu des options et sauvegarde si modifiées
	@minimum borne inférieure
	@maximum borne supérieure
	@nombre_tours nombre de tours
	@return tuple (minimum, maximum, nombre_tours)
"""
def gerer_options(minimum, maximum, nombre_tours):
	afficher_menu_options(minimum, maximum, nombre_tours)
	choix_options = lire_entier("? = ", 1, 2)
	if choix_options == 1:
		minimum = lire_entier("\nMin = ", 1, maximum)
		maximum = lire_entier("\nMax = ", minimum, 10000)
	elif choix_options == 2:
		nombre_tours = lire_entier("\nTours max = ", 1, 100)
	sauvegarder_options(minimum, maximum, nombre_tours)
	return minimum, maximum, nombre_tours

# Main

"""
	Boucle principale du programme
	Gère la navigation entre menu, jeu et options
	@none aucun
"""
def main():
	minimum, maximum, nombre_tours = charger_options()
	while True:
		afficher_menu_principal()
		choix_menu = lire_entier("? = ", 1, 3)
		if choix_menu == 1:
			jouer_partie(minimum, maximum, nombre_tours)
		elif choix_menu == 2:
			minimum, maximum, nombre_tours = gerer_options(minimum, maximum, nombre_tours)
		elif choix_menu == 3:
			break

# Lancement du programme
main()

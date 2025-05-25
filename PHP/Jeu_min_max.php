<?php
# Données
define('CHEMIN_DOSSIER', __DIR__);
define('FICHIER_OPTIONS', CHEMIN_DOSSIER . DIRECTORY_SEPARATOR . 'minmax_options.txt');

# Fonctions utilitaires
## Lecture d'un argument de commande
/**
	Lit un entier et le valide
	Boucle jusqu'à un entier compris dans l’intervalle
	@message invite utilisateur
	@minimum borne basse
	@maximum borne haute
*/
function lire_entier(string $message, int $minimum, int $maximum): int {
	while (true) {
		echo $message;
		$entree = trim(fgets(STDIN));
		if (preg_match('/^\d+$/', $entree)) {
			$valeur = (int)$entree;
			if ($valeur >= $minimum && $valeur <= $maximum) {
				return $valeur;
			}
		}
		echo "\nVeuillez entrer un entier entre {$minimum} et {$maximum}.\n";
	}
}

## Chargement des options sauvegardées
/**
	Charge les options depuis le fichier
	Renvoie [min,max,tours] ou valeurs par défaut
*/
function charger_options(): array {
	if (file_exists(FICHIER_OPTIONS)) {
		$contenu = file_get_contents(FICHIER_OPTIONS);
		$donnees = array_map('intval', explode(',', $contenu));
		if (count($donnees) === 3) {
			return $donnees;
		}
	}
	return [1, 100, 5];
}

## Sauvegarde des options
/**
	Sauvegarde les options dans le fichier
	@minimum borne basse
	@maximum borne haute
	@tours nombre de tours
*/
function sauvegarder_options(int $minimum, int $maximum, int $tours): void {
	$ligne = "{$minimum},{$maximum},{$tours}";
	file_put_contents(FICHIER_OPTIONS, $ligne);
}

# Fonctions principales
## Affiche le menu principal
/**
	Affiche le menu principal
*/
function afficher_menu_principal(): void {
	echo "\n#### Menu ####\n1 : Jouer\n2 : Options\n3 : Quitter\n";
}

## Affiche le menu d'options
/**
	Affiche le menu d’options courant
	@minimum borne basse
	@maximum borne haute
	@tours nombre de tours
*/
function afficher_menu_options(int $minimum, int $maximum, int $tours): void {
	echo "\n#### Menu options ####\n1 : Choisir les limites (actuellement : {$minimum} - {$maximum})\n2 : Nombre de tours max (actuellement : {$tours})\n";
}

## Joue une partie
/**
	Lance une partie ; indique +/- selon la réponse
	@minimum borne basse
	@maximum borne haute
	@tours nombre de tours
*/
function jouer_partie(int $minimum, int $maximum, int $tours): void {
	$cible = random_int($minimum, $maximum);
	echo "\nTrouvez entre {$minimum} et {$maximum} en {$tours} tours\n";
	for ($attemptNumber = 1; $attemptNumber <= $tours; $attemptNumber++) {
		$choix = lire_entier("Tour {$attemptNumber} : ", $minimum, $maximum);
		if ($choix === $cible) { echo "\nGagné !\n"; return; }
		echo ($choix > $cible ? "-\n" : "+\n");
	}
	echo "\nPerdu ! La réppnse était {$cible}.\n";
}

## Gère les options
/**
	Gère la modification des options et les sauvegarde
	@minimum borne basse
	@maximum borne haute
	@tours nombre de tours
*/
function gerer_options(int $minimum, int $maximum, int $tours): array {
	afficher_menu_options($minimum, $maximum, $tours);
	$choixOptions = lire_entier("? = ", 1, 2);
	if ($choixOptions === 1) {
		$minimum = lire_entier("\nMin = ", 1, $maximum);
		$maximum = lire_entier("\nMax = ", $minimum, 10000);
	} elseif ($choixOptions === 2) {
		$tours = lire_entier("\nTours max = ", 1, 100);
	}
	sauvegarder_options($minimum, $maximum, $tours);
	return [$minimum, $maximum, $tours];
}

# Main
/**
	Point d’entrée du programme
*/
function main(): void {
	[$minimum, $maximum, $tours] = charger_options();
	while (true) {
		afficher_menu_principal();
		$choixMenu = lire_entier("? = ", 1, 3);
		switch ($choixMenu) {
			case 1: jouer_partie($minimum, $maximum, $tours); break;
			case 2: [$minimum, $maximum, $tours] = gerer_options($minimum, $maximum, $tours); break;
			case 3: return;
		}
	}
}

# Lancement du programme
main();

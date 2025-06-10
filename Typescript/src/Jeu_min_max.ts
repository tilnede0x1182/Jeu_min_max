// # Importations
import * as fs from 'fs';
import * as path from 'path';
import * as readlineSync from 'readline-sync';

// # Données
const CHEMIN_DOSSIER: string = __dirname;
const FICHIER_OPTIONS: string = path.join(CHEMIN_DOSSIER, '..', 'data', 'minmax_options.txt');

// # Fonctions utilitaires
// ## Lecture d'un argument de commande
/**
 * Lit un entier sécurisé auprès de l'utilisateur
 * Répète la lecture tant que la saisie n'est pas valide
 * @param message Message d'invite
 * @param min     Limite basse incluse
 * @param max     Limite haute incluse
 */
function lireEntier(message: string, min: number, max: number): number {
	while(true) {
		const entree: string = readlineSync.question(message).trim();
		const valeur: number = Number(entree);
		if (/^\d+$/.test(entree) && valeur >= min && valeur <= max) {
			return valeur;
		}
		console.log(`\nVeuillez entrer un entier entre ${min} et ${max}.`);
	}
}

// ## Chargement des options sauvegardées
/**
 * Charge les options ou renvoie les valeurs par défaut
 */
function chargerOptions(): [number, number, number] {
	if (fs.existsSync(FICHIER_OPTIONS)) {
		const donnees: number[] = fs.readFileSync(FICHIER_OPTIONS, 'utf8').split(',').map(Number);
		if (donnees.length === 3) {
			return [donnees[0], donnees[1], donnees[2]];
		}
	}
	return [1, 100, 5];
}

// ## Sauvegarde des options
/**
 * Sauvegarde les options sur disque
 * @param min   Valeur minimale
 * @param max   Valeur maximale
 * @param tours Nombre de tours
 */
function sauvegarderOptions(min: number, max: number, tours: number): void {
	fs.writeFileSync(FICHIER_OPTIONS, [min, max, tours].join(','));
}

// # Fonctions principales
/**
 * Affiche le menu principal
 */
function afficherMenuPrincipal(): void {
	console.log('\n#### Menu ####\n1 : Jouer\n2 : Options\n3 : Quitter');
}

/**
 * Affiche le menu des options
 * @param min   Limite basse actuelle
 * @param max   Limite haute actuelle
 * @param tours Tours actuels
 */
function afficherMenuOptions(min: number, max: number, tours: number): void {
	console.log(`\n#### Menu options ####\n1 : Choisir les limites (actuellement : ${min} - ${max})\n2 : Nombre de tours max (actuellement : ${tours})`);
}

/**
 * Lance une partie « Plus / Moins »
 * @param min   Limite basse
 * @param max   Limite haute
 * @param tours Nombre de tours
 */
function jouerPartie(min: number, max: number, tours: number): void {
	const cible: number = Math.floor(Math.random() * (max - min + 1)) + min;
	console.log(`\nTrouvez entre ${min} et ${max} en ${tours} tours`);
	for (let essaiNumero: number = 1; essaiNumero <= tours; essaiNumero++) {
		const choix: number = lireEntier(`Tour ${essaiNumero} : `, min, max);
		if (choix === cible) {
			console.log('\nGagné !');
			return;
		}
		console.log(choix > cible ? '-' : '+');
	}
	console.log(`\nPerdu ! La réppnse était ${cible}.`);
}

/**
 * Gère et sauvegarde les options
 * @param min   Limite basse
 * @param max   Limite haute
 * @param tours Nombre de tours
 */
function gererOptions(min: number, max: number, tours: number): [number, number, number] {
	afficherMenuOptions(min, max, tours);
	const choixOptions: number = lireEntier('? = ', 1, 2);
	if (choixOptions === 1) {
		min = lireEntier('\nMin = ', 1, max);
		max = lireEntier('\nMax = ', min, 10_000);
	} else if (choixOptions === 2) {
		tours = lireEntier('\nTours max = ', 1, 100);
	}
	sauvegarderOptions(min, max, tours);
	return [min, max, tours];
}

// # Main
/**
 * Point d'entrée du programme
 */
function main(): void {
	// Crée le répertoire Data s'il n'existe pas.
	fs.mkdirSync(path.join(CHEMIN_DOSSIER, '..', 'data'), { recursive: true });
	let [min, max, tours] = chargerOptions();
	while (true) {
		afficherMenuPrincipal();
		const choixMenu: number = lireEntier('? = ', 1, 3);
		switch (choixMenu) {
			case 1:
				jouerPartie(min, max, tours);
				break;
			case 2:
				[min, max, tours] = gererOptions(min, max, tours);
				break;
			case 3:
				return;
		}
	}
}

// # Lancement du programme
main();

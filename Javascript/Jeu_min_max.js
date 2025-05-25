// # Importations
const fs = require("fs");
const path = require("path");

// # Données
const CHEMIN_DOSSIER = __dirname;
const FICHIER_OPTIONS = path.join(CHEMIN_DOSSIER, "minmax_options.txt");

// # Fonctions utilitaires
// ## Lecture d'une ligne synchronisée
/**
	Lit une ligne sur STDIN de façon synchrone
	@returns ligne lue (sans « \n »)
*/
function readLineSync() {
	const tampon = Buffer.alloc(1);
	let ligne = "";
	while (true) {
		const octetsLus = fs.readSync(0, tampon, 0, 1, null);
		if (octetsLus === 0) break;
		const caractere = tampon.toString();
		if (caractere === "\n") break;
		ligne += caractere;
	}
	return ligne;
}

// ## Lecture d'un entier utilisateur sécurisé
/**
	Lit un entier entre min et max inclus, boucle jusqu'à validité
	@message prompt
	@min borne basse
	@max borne haute
*/
function lireEntier(message, min, max) {
	while (true) {
		process.stdout.write(message);
		const entree = readLineSync().trim();
		if (/^\d+$/.test(entree)) {
			const nombre = Number(entree);
			if (nombre >= min && nombre <= max) return nombre;
		}
		console.log(`\nVeuillez entrer un entier entre ${min} et ${max}.`);
	}
}

// ## Chargement des options sauvegardées
/**
	Charge le fichier d’options ou renvoie les valeurs par défaut
*/
function chargerOptions() {
	if (fs.existsSync(FICHIER_OPTIONS)) {
		const donnees = fs
			.readFileSync(FICHIER_OPTIONS, "utf8")
			.split(",")
			.map(Number);
		if (donnees.length === 3) return donnees;
	}
	return [1, 100, 5];
}

// ## Sauvegarde des options
/**
	Enregistre les options actuelles
*/
function sauvegarderOptions(min, max, tours) {
	fs.writeFileSync(FICHIER_OPTIONS, [min, max, tours].join(","));
}

// # Fonctions principales
/**
	Affiche le menu principal
*/
function afficherMenuPrincipal() {
	console.log("\n#### Menu ####\n1 : Jouer\n2 : Options\n3 : Quitter");
}

/**
	Affiche le menu d’options
*/
function afficherMenuOptions(min, max, tours) {
	console.log(
		`\n#### Menu options ####\n1 : Choisir les limites (actuellement : ${min} - ${max})\n2 : Nombre de tours max (actuellement : ${tours})`
	);
}

/**
	Lance une partie « plus-grand / plus-petit »
*/
function jouerPartie(min, max, tours) {
	const cible = Math.floor(Math.random() * (max - min + 1) + min);
	console.log(`\nTrouvez entre ${min} et ${max} en ${tours} tours`);
	for (let tourNumero = 1; tourNumero <= tours; tourNumero++) {
		const choix = lireEntier(`Tour ${tourNumero} : `, min, max);
		if (choix === cible) {
			console.log("\nGagné !");
			return;
		}
		console.log(choix > cible ? "-" : "+");
	}
	console.log(`\nPerdu ! La réppnse était ${cible}.`);
}

/**
	Gère le menu Options puis sauvegarde
*/
function gererOptions(min, max, tours) {
	afficherMenuOptions(min, max, tours);
	const choixOptions = lireEntier("? = ", 1, 2);
	if (choixOptions === 1) {
		min = lireEntier("\nMin = ", 1, max);
		max = lireEntier("\nMax = ", min, 10000);
	} else if (choixOptions === 2) {
		tours = lireEntier("\nTours max = ", 1, 100);
	}
	sauvegarderOptions(min, max, tours);
	return [min, max, tours];
}

// # Main
/**
	Point d’entrée principal
*/
function main() {
	let [min, max, tours] = chargerOptions();
	while (true) {
		afficherMenuPrincipal();
		const choixMenu = lireEntier("? = ", 1, 3);
		if (choixMenu === 1) {
			jouerPartie(min, max, tours);
		} else if (choixMenu === 2) {
			[min, max, tours] = gererOptions(min, max, tours);
		} else if (choixMenu === 3) {
			break;
		}
	}
}

// # Lancement du programme
main();

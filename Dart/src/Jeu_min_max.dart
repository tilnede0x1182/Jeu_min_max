// # Importations
import 'dart:io';
import 'dart:math';

// # Données
final RegExp motifEntier = RegExp(r'^\d+$');
final File fichierOptions = File('${Directory.current.path}/data/minmax_options.txt');

// # Fonctions utilitaires
// ## Lecture d'un argument de commande
/**
 * Lecture sécurisée d'un entier depuis l'utilisateur
 * Contrôle l'appartenance à un intervalle [minimum, maximum]
 * @param message  message à afficher
 * @param minimum  borne inférieure
 * @param maximum  borne supérieure
 * @return entier validé
 */
int lireEntier(String message, int minimum, int maximum) {
	while (true) {
		stdout.write(message);
		String? entree = stdin.readLineSync()?.trim();
		if (entree != null && motifEntier.hasMatch(entree)) {
			int valeur = int.parse(entree);
			if (valeur >= minimum && valeur <= maximum) return valeur;
		}
		print('\nVeuillez entrer un entier entre $minimum et $maximum.');
	}
}

// # Fonctions utilitaires principales
// ## Création d'une tâche
/**
 * Charge les options sauvegardées depuis le fichier
 * @return tableau [minimum, maximum, nombreTours]
 */
List<int> chargerOptions() {
	try {
		if (fichierOptions.existsSync()) {
			String contenu = fichierOptions.readAsStringSync();
			List<int> valeurs = contenu
				.split(',')
				.where((fragment) => motifEntier.hasMatch(fragment))
				.map(int.parse)
				.toList();
			if (valeurs.length == 3) return valeurs;
		}
	} on FileSystemException catch (exception) {
		// ignore et valeurs par défaut
	}
	return [1, 100, 5];
}

/**
 * Sauvegarde les options dans le fichier
 * @param minimum     borne inférieure
 * @param maximum     borne supérieure
 * @param nombreTours nombre de tours
 */
void sauvegarderOptions(int minimum, int maximum, int nombreTours) {
	try {
		fichierOptions.parent.createSync(recursive: true);
		fichierOptions.writeAsStringSync('$minimum,$maximum,$nombreTours');
	} on FileSystemException catch (exception) {
		stderr.writeln('Erreur de sauvegarde : ${exception.message}');
	}
}

// # Fonctions principales
/**
 * Affiche le menu principal
 */
void afficherMenuPrincipal() {
	print('\n#### Menu ####\n1 : Jouer\n2 : Options\n3 : Quitter');
}

/**
 * Affiche le menu d'options
 */
void afficherMenuOptions(int minimum, int maximum, int nombreTours) {
	print(
		'\n#### Menu options ####\n1 : Choisir les limites (actuellement : $minimum - $maximum)\n2 : Nombre de tours max (actuellement : $nombreTours)');
}

/**
 * Lance une partie et affiche + ou -
 */
void jouerPartie(int minimum, int maximum, int nombreTours) {
	int cible = minimum + Random().nextInt(maximum - minimum + 1);
	print('\nTrouvez entre $minimum et $maximum en $nombreTours tours');
	for (int numeroEssai = 1; numeroEssai <= nombreTours; numeroEssai++) {
		int choix = lireEntier('Tour $numeroEssai : ', minimum, maximum);
		if (choix == cible) {
			print('\nGagné !');
			return;
		}
		print(choix > cible ? '-' : '+');
	}
	print('\nPerdu ! La réppnse était $cible.');
}

/**
 * Gère le menu Options et sauvegarde
 */
List<int> gererOptions(int minimum, int maximum, int nombreTours) {
	afficherMenuOptions(minimum, maximum, nombreTours);
	int choixOptions = lireEntier('? = ', 1, 2);
	if (choixOptions == 1) {
		minimum = lireEntier('\nMin = ', 1, maximum);
		maximum = lireEntier('\nMax = ', minimum, 10000);
	} else if (choixOptions == 2) {
		nombreTours = lireEntier('\nTours max = ', 1, 100);
	}
	sauvegarderOptions(minimum, maximum, nombreTours);
	return [minimum, maximum, nombreTours];
}

/**
 * Boucle principale du programme
 */
void boucleProgramme() {
	List<int> options = chargerOptions();
	int minimum = options[0], maximum = options[1], nombreTours = options[2];
	while (true) {
		afficherMenuPrincipal();
		int choixMenu = lireEntier('? = ', 1, 3);
		if (choixMenu == 1) {
			jouerPartie(minimum, maximum, nombreTours);
		} else if (choixMenu == 2) {
			var nouvellesOptions = gererOptions(minimum, maximum, nombreTours);
			minimum = nouvellesOptions[0];
			maximum = nouvellesOptions[1];
			nombreTours = nouvellesOptions[2];
		} else {
			break;
		}
	}
}

// # Main
/**
 * Lancement du programme
 */
void main(List<String> arguments) {
	boucleProgramme();
}

// # Lancement du programme
// (exécuté via main())

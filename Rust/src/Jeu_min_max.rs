//! # Jeu MinMax
//! Dépendance externe requise : rand = "0.8"

// # Importations
use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;
use rand::Rng;

// # Données

// # Fonctions utilitaires
// ## Structure de données

/// Structure des paramètres du jeu
/// 
/// - `min` : borne inférieure
/// - `max` : borne supérieure
/// - `tours` : nombre d’essais
struct Options {
	min: i32,
	max: i32,
	tours: i32,
}

// ## Chemin du fichier d’options

/// Retourne le chemin absolu vers « ./data/minmax_options.txt »
/// Crée le dossier ./data s’il n’existe pas
fn chemin_options() -> PathBuf {
	let mut chemin = std::env::current_dir().expect("Impossible d’obtenir le dossier courant");
	chemin.push("data");
	fs::create_dir_all(&chemin).expect("Impossible de créer ./data");
	chemin.push("minmax_options.txt");
	chemin
}

// ## Lecture d’un entier utilisateur sécurisé

/// Boucle jusqu’à obtenir un entier compris dans l’intervalle donné
///
/// @message Message affiché à l’utilisateur
/// @borne_min Borne minimale autorisée
/// @borne_max Borne maximale autorisée
fn lire_entier(message: &str, borne_min: i32, borne_max: i32) -> i32 {
	loop {
		print!("{message}");
		io::stdout().flush().unwrap();
		let mut entree = String::new(); 
		io::stdin().read_line(&mut entree).unwrap();
		let trimmed = entree.trim();
		if !trimmed.is_empty() && trimmed.chars().all(|c| c.is_ascii_digit()) {
			let valeur = trimmed.parse::<i32>().unwrap();
			if (borne_min..=borne_max).contains(&valeur) {
				return valeur;
			}
		}
		println!("\nVeuillez entrer un entier entre {borne_min} et {borne_max}.");
	}
}

// ## Chargement des options sauvegardées

/// Lit les options depuis le fichier, ou renvoie des valeurs par défaut
fn charger_options() -> Options {
	let chemin = chemin_options();
	if let Ok(contenu) = fs::read_to_string(chemin) {
		let morceaux: Vec<i32> = contenu.split(',').filter_map(|s| s.trim().parse().ok()).collect();
		if morceaux.len() == 3 {
			return Options { min: morceaux[0], max: morceaux[1], tours: morceaux[2] };
		}
	}
	Options { min: 1, max: 100, tours: 5 }
}

// ## Sauvegarde des options

/// Écrit les paramètres actuels dans le fichier d’options
///
/// @options Référence à la structure contenant min, max, tours
fn sauvegarder_options(options: &Options) {
	let chemin = chemin_options();
	let contenu = format!("{},{},{}", options.min, options.max, options.tours);
	let _ = fs::write(chemin, contenu);
}

// # Fonctions principales
// ## Affiche le menu principal

/// Affiche les choix disponibles à l’utilisateur
fn afficher_menu_principal() {
	println!("\n#### Menu ####\n1 : Jouer\n2 : Options\n3 : Quitter");
}

// ## Affiche le menu d’options

/// Affiche les paramètres actuels dans le menu d’options
///
/// @options Référence à la structure de configuration
fn afficher_menu_options(options: &Options) {
	println!(
		"\n#### Menu options ####\n1 : Choisir les limites (actuellement : {} - {})\n2 : Nombre de tours max (actuellement : {})",
		options.min, options.max, options.tours
	);
}

// ## Joue une partie (+ / -)

/// Lance une partie avec un nombre d’essais limité
///
/// Affiche « + » ou « - » selon la comparaison avec la cible.
/// @options Référence aux limites et nombre de tours
fn jouer_partie(options: &Options) {
	let cible = rand::thread_rng().gen_range(options.min..=options.max);
	println!("\nTrouvez entre {} et {} en {} tours", options.min, options.max, options.tours);
	for tentative in 1..=options.tours {
		let choix = lire_entier(&format!("Tour {} : ", tentative), options.min, options.max);
		if choix == cible {
			println!("\nGagné !");
			return;
		}
		println!("{}", if choix > cible { "-" } else { "+" });
	}
	println!("\nPerdu ! La réppnse était {}.", cible);
}

// ## Gère les options et sauvegarde si modifiées

/// Modifie les paramètres selon l’utilisateur et les sauvegarde
///
/// @options Référence mutable à la configuration actuelle
fn gerer_options(options: &mut Options) {
	afficher_menu_options(options);
	let choix_options = lire_entier("? = ", 1, 2);
	if choix_options == 1 {
		options.min = lire_entier("\nMin = ", 1, options.max);
		options.max = lire_entier("\nMax = ", options.min, 10_000);
	} else {
		options.tours = lire_entier("\nTours max = ", 1, 100);
	}
	sauvegarder_options(options);
}

// # Main

/// Point d’entrée principal du programme
fn main() {
	let mut options = charger_options();
	loop {
		afficher_menu_principal();
		match lire_entier("? = ", 1, 3) {
			1 => jouer_partie(&options),
			2 => gerer_options(&mut options),
			3 => break,
			_ => {}
		}
	}
}

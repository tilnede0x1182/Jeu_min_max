
// # Importations
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class MinMax {
	// # Données
	private static final Scanner scanner = new Scanner(System.in);
	private static final Pattern motifEntier = Pattern.compile("\\d+");
	private static final Path fichierOptions = Paths.get(System.getProperty("user.dir"), "data", "minmax_options.txt");

	// # Fonctions utilitaires
	// ## Lecture d'un argument de commande
	/**
	 * Lecture sécurisée d'un entier depuis l'utilisateur
	 * Contrôle l'appartenance à un intervalle [minimum, maximum]
	 * 
	 * @param message message à afficher
	 * @param minimum borne inférieure
	 * @param maximum borne supérieure
	 * @return entier validé
	 */
	private static int lireEntier(String message, int minimum, int maximum) {
		while (true) {
			System.out.print(message);
			String entree = scanner.nextLine().trim();
			if (motifEntier.matcher(entree).matches()) {
				int valeur = Integer.parseInt(entree);
				if (valeur >= minimum && valeur <= maximum)
					return valeur;
			}
			System.out.println("\nVeuillez entrer un entier entre " + minimum + " et " + maximum + ".");
		}
	}

	// # Fonctions utilitaires principales
	// ## Création d'une tâche
	/**
	 * Charge les options sauvegardées depuis le fichier
	 * 
	 * @return tableau [minimum, maximum, nombreTours]
	 */
	private static int[] chargerOptions() {
		if (Files.exists(fichierOptions)) {
			try (BufferedReader lecteur = Files.newBufferedReader(fichierOptions)) {
				List<Integer> valeurs = Arrays.stream(lecteur.readLine().split(","))
						.filter(motifEntier.asPredicate())
						.map(Integer::parseInt)
						.collect(Collectors.toList());
				if (valeurs.size() == 3)
					return valeurs.stream().mapToInt(Integer::intValue).toArray();
			} catch (IOException exceptionLecture) {
				// ignore et valeurs par défaut
			}
		}
		return new int[] { 1, 100, 5 };
	}

	/**
	 * Sauvegarde les options dans le fichier
	 * 
	 * @param minimum     borne inférieure
	 * @param maximum     borne supérieure
	 * @param nombreTours nombre de tours
	 */
	private static void sauvegarderOptions(int minimum, int maximum, int nombreTours) {
		try (BufferedWriter ecriture = Files.newBufferedWriter(fichierOptions)) {
			ecriture.write(minimum + "," + maximum + "," + nombreTours);
		} catch (IOException exceptionEcriture) {
			System.err.println("Erreur de sauvegarde : " + exceptionEcriture.getMessage());
		}
	}

	// # Fonctions principales
	/**
	 * Affiche le menu principal
	 */
	private static void afficherMenuPrincipal() {
		System.out.println("\n#### Menu ####\n1 : Jouer\n2 : Options\n3 : Quitter");
	}

	/**
	 * Affiche le menu d'options
	 */
	private static void afficherMenuOptions(int minimum, int maximum, int nombreTours) {
		System.out.println("\n#### Menu options ####\n1 : Choisir les limites (actuellement : "
				+ minimum + " - " + maximum + ")\n2 : Nombre de tours max (actuellement : " + nombreTours + ")");
	}

	/**
	 * Lance une partie et affiche + ou -
	 */
	private static void jouerPartie(int minimum, int maximum, int nombreTours) {
		int cible = minimum + (int) (Math.random() * (maximum - minimum + 1));
		System.out.println("\nTrouvez entre " + minimum + " et " + maximum + " en " + nombreTours + " tours");
		for (int numeroEssai = 1; numeroEssai <= nombreTours; numeroEssai++) {
			int choix = lireEntier("Tour " + numeroEssai + " : ", minimum, maximum);
			if (choix == cible) {
				System.out.println("\nGagné !");
				return;
			}
			System.out.println(choix > cible ? "-" : "+");
		}
		System.out.println("\nPerdu ! La réppnse était " + cible + ".");
	}

	/**
	 * Gère le menu Options et sauvegarde
	 */
	private static int[] gererOptions(int minimum, int maximum, int nombreTours) {
		afficherMenuOptions(minimum, maximum, nombreTours);
		int choixOptions = lireEntier("? = ", 1, 2);
		if (choixOptions == 1) {
			minimum = lireEntier("\nMin = ", 1, maximum);
			maximum = lireEntier("\nMax = ", minimum, 10000);
		} else if (choixOptions == 2) {
			nombreTours = lireEntier("\nTours max = ", 1, 100);
		}
		sauvegarderOptions(minimum, maximum, nombreTours);
		return new int[] { minimum, maximum, nombreTours };
	}

	// # Main
	/**
	 * Boucle principale du programme
	 */
	public static void main(String[] args) {
		int[] options = chargerOptions();
		int minimum = options[0], maximum = options[1], nombreTours = options[2];
		while (true) {
			afficherMenuPrincipal();
			int choixMenu = lireEntier("? = ", 1, 3);
			if (choixMenu == 1) {
				jouerPartie(minimum, maximum, nombreTours);
			} else if (choixMenu == 2) {
				int[] nouvellesOptions = gererOptions(minimum, maximum, nombreTours);
				minimum = nouvellesOptions[0];
				maximum = nouvellesOptions[1];
				nombreTours = nouvellesOptions[2];
			} else if (choixMenu == 3) {
				break;
			}
		}
	}
}

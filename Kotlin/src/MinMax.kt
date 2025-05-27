// # Importations
import java.io.BufferedWriter
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.Scanner
import java.util.regex.Pattern

// # Données
object MinMax {
	@JvmStatic
	private val scanner = Scanner(System.`in`)
	@JvmStatic
	private val motifEntier: Pattern = Pattern.compile("\\d+")
	@JvmStatic
	private val fichierOptions: Path = Paths.get(System.getProperty("user.dir"), "data", "minmax_options.txt")

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
	private fun lireEntier(message: String, minimum: Int, maximum: Int): Int {
		while (true) {
			print(message)
			val entree = scanner.nextLine().trim()
			if (motifEntier.matcher(entree).matches()) {
				val valeur = entree.toInt()
				if (valeur >= minimum && valeur <= maximum) return valeur
			}
			println("\nVeuillez entrer un entier entre $minimum et $maximum.")
		}
	}

	// # Fonctions utilitaires principales
	// ## Création d'une tâche
	/**
	 * Charge les options sauvegardées depuis le fichier
	 *
	 * @return tableau [minimum, maximum, nombreTours]
	 */
	private fun chargerOptions(): IntArray {
		if (Files.exists(fichierOptions)) {
			try {
				Files.newBufferedReader(fichierOptions).use { lecteur ->
					val valeurs = lecteur.readLine()
						.split(',')
						.filter { motifEntier.matcher(it).matches() }
						.map { it.toInt() }
					if (valeurs.size == 3) return valeurs.toIntArray()
				}
			} catch (exceptionLecture: IOException) {
				// ignore et valeurs par défaut
			}
		}
		return intArrayOf(1, 100, 5)
	}

	/**
	 * Sauvegarde les options dans le fichier
	 *
	 * @param minimum     borne inférieure
	 * @param maximum     borne supérieure
	 * @param nombreTours nombre de tours
	 */
	private fun sauvegarderOptions(minimum: Int, maximum: Int, nombreTours: Int) {
		try {
			Files.newBufferedWriter(fichierOptions).use { ecriture ->
				ecriture.write("$minimum,$maximum,$nombreTours")
			}
		} catch (exceptionEcriture: IOException) {
			System.err.println("Erreur de sauvegarde : ${exceptionEcriture.message}")
		}
	}

	// # Fonctions principales
	/**
	 * Affiche le menu principal
	 */
	private fun afficherMenuPrincipal() {
		println("\n#### Menu ####\n1 : Jouer\n2 : Options\n3 : Quitter")
	}

	/**
	 * Affiche le menu d'options
	 */
	private fun afficherMenuOptions(minimum: Int, maximum: Int, nombreTours: Int) {
		println(
			"\n#### Menu options ####\n1 : Choisir les limites (actuellement : " +
					"$minimum - $maximum)\n2 : Nombre de tours max (actuellement : $nombreTours)"
		)
	}

	/**
	 * Lance une partie et affiche + ou -
	 */
	private fun jouerPartie(minimum: Int, maximum: Int, nombreTours: Int) {
		val cible = minimum + (Math.random() * (maximum - minimum + 1)).toInt()
		println("\nTrouvez entre $minimum et $maximum en $nombreTours tours")
		for (numeroEssai in 1..nombreTours) {
			val choix = lireEntier("Tour $numeroEssai : ", minimum, maximum)
			if (choix == cible) {
				println("\nGagné !")
				return
			}
			println(if (choix > cible) "-" else "+")
		}
		println("\nPerdu ! La réppnse était $cible.")
	}

	/**
	 * Gère le menu Options et sauvegarde
	 */
	private fun gererOptions(minimumInitial: Int, maximumInitial: Int, toursInitial: Int): IntArray {
		var minimumLocal = minimumInitial
		var maximumLocal = maximumInitial
		var toursLocal = toursInitial
		afficherMenuOptions(minimumLocal, maximumLocal, toursLocal)
		when (lireEntier("? = ", 1, 2)) {
			1 -> {
				minimumLocal = lireEntier("\nMin = ", 1, maximumLocal)
				maximumLocal = lireEntier("\nMax = ", minimumLocal, 10000)
			}
			2 -> toursLocal = lireEntier("\nTours max = ", 1, 100)
		}
		sauvegarderOptions(minimumLocal, maximumLocal, toursLocal)
		return intArrayOf(minimumLocal, maximumLocal, toursLocal)
	}

	// # Main
	/**
	 * Boucle principale du programme
	 */
	@JvmStatic
	fun main(args: Array<String>) {
		var options = chargerOptions()
		var minimum = options[0]
		var maximum = options[1]
		var nombreTours = options[2]
		while (true) {
			afficherMenuPrincipal()
			when (lireEntier("? = ", 1, 3)) {
				1 -> jouerPartie(minimum, maximum, nombreTours)
				2 -> {
					options = gererOptions(minimum, maximum, nombreTours)
					minimum = options[0]
					maximum = options[1]
					nombreTours = options[2]
				}
				3 -> return
			}
		}
	}
}

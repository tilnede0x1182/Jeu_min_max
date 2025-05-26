// Importations
using System;
using System.IO;

class Program
{
	// Données
	private static readonly string cheminDossier = Directory.GetCurrentDirectory();

	private static readonly string fichierOptions = Path.Combine(cheminDossier, "data", "minmax_options.txt");

	// Création automatique du dossier data
	static Program()
	{
		string dossierData = Path.Combine(cheminDossier, "data");
		if (!Directory.Exists(dossierData))
		{
			Directory.CreateDirectory(dossierData);
		}
	}

	// Fonctions utilitaires
	// ## Lecture d'un entier utilisateur sécurisé
	/**
		Lecture d'un entier utilisateur sécurisé
		Boucle jusqu'à obtenir un entier valide entre min et max.
		@message invite à l'utilisateur
		@min borne inférieure
		@max borne supérieure
		@return entier validé
	*/
	static int LireEntier(string message, int min, int max)
	{
		while (true)
		{
			Console.Write(message);
			string entree = Console.ReadLine().Trim();
			if (int.TryParse(entree, out int nombre) && nombre >= min && nombre <= max) return nombre;
			Console.WriteLine($"\nVeuillez entrer un entier entre {min} et {max}.");
		}
	}

	// Fonctions utilitaires principales
	// ## Chargement et sauvegarde des options
	/**
		Charge les options ou renvoie les valeurs par défaut.
		@return tuple (min,max,tours)
	*/
	static (int min, int max, int tours) ChargerOptions()
	{
		if (File.Exists(fichierOptions))
		{
			string[] donnees = File.ReadAllText(fichierOptions).Split(',');
			if (donnees.Length == 3 &&
				int.TryParse(donnees[0], out int min) &&
				int.TryParse(donnees[1], out int max) &&
				int.TryParse(donnees[2], out int tours)) return (min, max, tours);
		}
		return (1, 100, 5);
	}

	/**
		Sauvegarde les options actuelles.
	*/
	static void SauvegarderOptions(int min, int max, int tours)
	{
		File.WriteAllText(fichierOptions, $"{min},{max},{tours}");
	}

	// Fonctions principales
	/**
		Affiche le menu principal.
	*/
	static void AfficherMenuPrincipal()
	{
		Console.WriteLine("\n#### Menu ####\n1 : Jouer\n2 : Options\n3 : Quitter");
	}

	/**
		Affiche le menu d'options.
	*/
	static void AfficherMenuOptions(int min, int max, int tours)
	{
		Console.WriteLine($"\n#### Menu options ####\n1 : Choisir les limites (actuellement : {min} - {max})\n2 : Nombre de tours max (actuellement : {tours})");
	}

	/**
		Joue une partie, indique si plus grand/petit.
	*/
	static void JouerPartie(int min, int max, int tours)
	{
		Random generateur = new Random();
		int cible = generateur.Next(min, max + 1);
		Console.WriteLine($"\nTrouvez entre {min} et {max} en {tours} tours");
		for (int essaiNumero = 1; essaiNumero <= tours; essaiNumero++)
		{
			int choix = LireEntier($"Tour {essaiNumero} : ", min, max);
			if (choix == cible) { Console.WriteLine("\nGagné !"); return; }
			Console.WriteLine(choix > cible ? "-" : "+");
		}
		Console.WriteLine($"\nPerdu ! La réppnse était {cible}.");
	}

	/**
		Gère les options et les sauvegarde si modifiées.
		@return tuple (min,max,tours)
	*/
	static (int min, int max, int tours) GererOptions(int min, int max, int tours)
	{
		AfficherMenuOptions(min, max, tours);
		int choixOptions = LireEntier("? = ", 1, 2);
		if (choixOptions == 1)
		{
			min = LireEntier("\nMin = ", 1, max);
			max = LireEntier("\nMax = ", min, 10000);
		}
		else if (choixOptions == 2) { tours = LireEntier("\nTours max = ", 1, 100); }
		SauvegarderOptions(min, max, tours);
		return (min, max, tours);
	}

	// Main
	/**
		Boucle principale : menu, options, jeu.
	*/
	static void main()
	{
		var (min, max, tours) = ChargerOptions();
		while (true)
		{
			AfficherMenuPrincipal();
			switch (LireEntier("? = ", 1, 3))
			{
				case 1: JouerPartie(min, max, tours); break;
				case 2: (min, max, tours) = GererOptions(min, max, tours); break;
				case 3: return;
			}
		}
	}

	// Lancement du programme
	static void Main(string[] args) => main();
}

// # Importations
#ifdef _WIN32
	#include <windows.h>
#else
	#include <sys/stat.h>
	#include <sys/types.h>
#endif
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <regex>
#include <random>
#include <tuple>

// # Données
static const std::string FICHIER_OPTIONS = "./data/minmax_options.txt";

// # Fonctions utilitaires
// ## Lecture d'un argument de commande
/**
	Lit un entier sécurisé dans [min,max]
	@message invite utilisateur
	@min borne inférieure
	@max borne supérieure
*/
static int lireEntier(const std::string &message, int min, int max)
{
	std::regex motif("^\\d+$");
	std::string entree;
	while (true)
	{
		std::cout << message;
		if (!std::getline(std::cin, entree))
			std::exit(0);
		if (std::regex_match(entree, motif))
		{
			int valeur = std::stoi(entree);
			if (valeur >= min && valeur <= max)
				return valeur;
		}
		std::cout << "\nVeuillez entrer un entier entre " << min << " et " << max << ".\n";
	}
}

// # Fonctions utilitaires principales
// ## Gestion des options persistance
static std::tuple<int, int, int> chargerOptions()
{
	std::ifstream fichier(FICHIER_OPTIONS);
	if (fichier)
	{
		std::string contenu;
		std::getline(fichier, contenu);
		std::stringstream flux(contenu);
		std::string seg;
		int valeurs[3]{};
		for (int index = 0; index < 3 && std::getline(flux, seg, ','); ++index)
			valeurs[index] = std::stoi(seg);
		if (valeurs[2])
			return {valeurs[0], valeurs[1], valeurs[2]};
	}
	return {1, 100, 5};
}

/**
	Crée le dossier ./data si nécessaire (Windows et POSIX)
*/
static void creerDossierData()
{
	#ifdef _WIN32
		CreateDirectoryA("./data", nullptr);
	#else
		mkdir("./data", 0775);
	#endif
}

static void sauvegarderOptions(int min, int max, int tours)
{
	creerDossierData();
	std::ofstream(FICHIER_OPTIONS) << min << ',' << max << ',' << tours;
}

// # Fonctions principales
class JeuMinMax
{
private:
	int borneMin, borneMax, nombreTours;
	std::mt19937 moteur{std::random_device{}()};

public:
	JeuMinMax()
	{
		std::tie(borneMin, borneMax, nombreTours) = chargerOptions();
	}
	/**
		Affiche le menu principal
	*/
	void afficherMenuPrincipal() const
	{
		std::cout << "\n#### Menu ####\n1 : Jouer\n2 : Options\n3 : Quitter\n";
	}
	/**
		Affiche le menu Options
	*/
	void afficherMenuOptions() const
	{
		std::cout << "\n#### Menu options ####\n1 : Choisir les limites (actuellement : "
				  << borneMin << " - " << borneMax
				  << ")\n2 : Nombre de tours max (actuellement : "
				  << nombreTours << ")\n";
	}
	/**
		Lance une partie Plus/Moins
	*/
	void jouerPartie()
	{
		std::uniform_int_distribution<int> dist(borneMin, borneMax);
		int cible = dist(moteur);
		std::cout << "\nTrouvez entre " << borneMin << " et " << borneMax
				  << " en " << nombreTours << " tours\n";
		for (int tour = 1; tour <= nombreTours; ++tour)
		{
			int choix = lireEntier("Tour " + std::to_string(tour) + " : ",
								   borneMin, borneMax);
			if (choix == cible)
			{
				std::cout << "\nGagné !\n";
				return;
			}
			std::cout << (choix > cible ? "-" : "+") << '\n';
		}
		std::cout << "\nPerdu ! La réppnse était " << cible << ".\n";
	}
	/**
		Gère le sous-menu Options
	*/
	void gererOptions()
	{
		afficherMenuOptions();
		switch (lireEntier("? = ", 1, 2))
		{
		case 1:
			borneMin = lireEntier("\nMin = ", 1, borneMax);
			borneMax = lireEntier("\nMax = ", borneMin, 10000);
			break;
		case 2:
			nombreTours = lireEntier("\nTours max = ", 1, 100);
			break;
		}
		sauvegarderOptions(borneMin, borneMax, nombreTours);
	}
	/**
		Boucle principale
	*/
	void boucle()
	{
		while (true)
		{
			afficherMenuPrincipal();
			switch (lireEntier("? = ", 1, 3))
			{
			case 1:
				jouerPartie();
				break;
			case 2:
				gererOptions();
				break;
			case 3:
				return;
			}
		}
	}
};

// # Main
int main()
{
	JeuMinMax jeu;
	jeu.boucle();
	return 0;
}

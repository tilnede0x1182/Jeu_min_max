// # Importations
#include <iostream>
#include <fstream>
#include <filesystem>
#include <regex>
#include <random>
#include <array>

// # Données
static const std::regex MOTIF_ENTIER("^\\d+$");
using Options = std::array<int, 3>; // [minimum,maximum,tours]

// # Classes utilitaires
// ## Gestion du fichier options
class OptionManager
{
public:
	/** Charge les options depuis ./data/minmax_options.txt */
	static Options load()
	{
		std::ifstream flux(path());
		if (flux)
		{
			Options options{};
			char separateur;
			if (flux >> options[0] >> separateur >> options[1] >> separateur >> options[2])
				return options;
		}
		return {1, 100, 5};
	}
	/** Sauvegarde les options */
	static void save(int minimum, int maximum, int nombreTours)
	{
		std::ofstream(path()) << minimum << ',' << maximum << ',' << nombreTours;
	}

private:
	static std::filesystem::path path()
	{
		auto dossier = std::filesystem::current_path() / "data";
		std::filesystem::create_directories(dossier); // Windows & POSIX
		return dossier / "minmax_options.txt";
	}
};

// # Classe principale
class JeuMinMax
{
private:
	int borneMinimum, borneMaximum, limiteTours;
	std::mt19937 generateur{std::random_device{}()};
	/** Lecture sécurisée d'un entier */
	int lireEntier(const std::string &invite, int intervalMin, int intervalMax) const
	{
		std::string saisie;
		while (true)
		{
			std::cout << invite;
			if (!std::getline(std::cin, saisie))
				std::exit(0);
			if (std::regex_match(saisie, MOTIF_ENTIER))
			{
				int valeur = std::stoi(saisie);
				if (valeur >= intervalMin && valeur <= intervalMax)
					return valeur;
			}
			std::cout << "\nVeuillez entrer un entier entre " << intervalMin
					  << " et " << intervalMax << ".\n";
		}
	}
	void afficherMenuPrincipal() const
	{
		std::cout << "\n#### Menu ####\n1 : Jouer\n2 : Options\n3 : Quitter\n";
	}
	void afficherMenuOptions() const
	{
		std::cout << "\n#### Menu options ####\n1 : Choisir les limites (actuellement : "
				  << borneMinimum << " - " << borneMaximum
				  << ")\n2 : Nombre de tours max (actuellement : "
				  << limiteTours << ")\n";
	}
	void jouerPartie()
	{
		std::uniform_int_distribution<int> distribution(borneMinimum, borneMaximum);
		int nombreCible = distribution(generateur);
		std::cout << "\nTrouvez entre " << borneMinimum << " et " << borneMaximum
				  << " en " << limiteTours << " tours\n";
		for (int indiceTour = 1; indiceTour <= limiteTours; ++indiceTour)
		{
			int choix = lireEntier("Tour " + std::to_string(indiceTour) + " : ",
								   borneMinimum, borneMaximum);
			if (choix == nombreCible)
			{
				std::cout << "\nGagné !\n";
				return;
			}
			std::cout << (choix > nombreCible ? '-' : '+') << '\n';
		}
		std::cout << "\nPerdu ! La réppnse était " << nombreCible << ".\n";
	}
	void menuOptions()
	{
		afficherMenuOptions();
		int choixOption = lireEntier("? = ", 1, 2);
		if (choixOption == 1)
		{
			borneMinimum = lireEntier("\nMin = ", 1, borneMaximum);
			borneMaximum = lireEntier("\nMax = ", borneMinimum, 10000);
		}
		else
		{
			limiteTours = lireEntier("\nTours max = ", 1, 100);
		}
		OptionManager::save(borneMinimum, borneMaximum, limiteTours);
	}

public:
	JeuMinMax()
	{
		Options opts = OptionManager::load();
		borneMinimum = opts[0];
		borneMaximum = opts[1];
		limiteTours = opts[2];
	}
	/** Boucle principale */
	void run()
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
				menuOptions();
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
	JeuMinMax().run();
	return 0;
}
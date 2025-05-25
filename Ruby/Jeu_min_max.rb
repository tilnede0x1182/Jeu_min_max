# Données
CHEMIN_DOSSIER = File.dirname(__FILE__)
FICHIER_OPTIONS = File.join(CHEMIN_DOSSIER, "minmax_options.txt")

# Importations

# Fonctions utilitaires
## Lecture d'un entier utilisateur sécurisé
def lire_entier(message, min, max)
	loop do
		print message
		entree = gets.strip
		return entree.to_i if entree =~ /^\d+$/ && entree.to_i.between?(min, max)
		puts "\nVeuillez entrer un entier entre #{min} et #{max}."
	end
end

## Chargement des options sauvegardées
def charger_options()
	if File.exist?(FICHIER_OPTIONS)
		donnees = File.read(FICHIER_OPTIONS).split(",").map(&:to_i)
		return donnees if donnees.size == 3
	end
	[1, 100, 5]
end

## Sauvegarde des options
def sauvegarder_options(min, max, tours)
	File.write(FICHIER_OPTIONS, [min, max, tours].join(","))
end

# Fonctions principales

# Affiche le menu principal
def afficher_menu_principal()
	puts "\n#### Menu ####\n1 : Jouer\n2 : Options\n3 : Quitter"
end

# Affiche le menu d'options
def afficher_menu_options(min, max, tours)
	puts "\n#### Menu options ####\n1 : Choisir les limites (actuellement : #{min} - #{max})\n2 : Nombre de tours max (actuellement : #{tours})"
end

# Joue une partie, indique si plus grand/petit
def jouer_partie(min, max, tours)
	cible = rand(min..max)
	puts "\nTrouvez entre #{min} et #{max} en #{tours} tours"
	1.upto(tours) do |essai_numero|
		choix = lire_entier("Tour #{essai_numero} : ", min, max)
		if choix == cible
			puts "\nGagné !"
			return
		elsif choix > cible
			puts "-"
		else
			puts "+"
		end
	end
	puts "\nPerdu ! La réppnse était #{cible}."
end

# Gère les options et sauvegarde si modifiées
def gerer_options(min, max, tours)
	afficher_menu_options(min, max, tours)
	choix_options = lire_entier("? = ", 1, 2)
	if choix_options == 1
		min = lire_entier("\nMin = ", 1, max)
		max = lire_entier("\nMax = ", min, 10_000)
	elsif choix_options == 2
		tours = lire_entier("\nTours max = ", 1, 100)
	end
	sauvegarder_options(min, max, tours)
	[min, max, tours]
end

# Main
def main()
	min, max, tours = charger_options()
	loop do
		afficher_menu_principal()
		choix_menu = lire_entier("? = ", 1, 3)
		case choix_menu
		when 1
			jouer_partie(min, max, tours)
		when 2
			variables = gerer_options(min, max, tours)
			min, max, tours = variables
		when 3
			break
		end
	end
end

# Lancement du programme
main()

# Importations
use strict;
use warnings;
use utf8;
use FindBin;
use File::Spec;

binmode STDOUT, ":utf8";
binmode STDERR, ":utf8";

# Données
my $CHEMIN_DOSSIER    = $FindBin::Bin;
my $FICHIER_OPTIONS   = File::Spec->catfile($CHEMIN_DOSSIER, 'minmax_options.txt');

# Fonctions utilitaires
## Lecture d'un entier utilisateur sécurisé
#"""
# Demande un entier à l'utilisateur et vérifie qu'il se situe dans l'intervalle
# Affiche un message d'erreur jusqu'à obtention d'une valeur valide
# @message Texte de l'invite
# @min     Valeur minimale inclusive
# @max     Valeur maximale inclusive
#"""
sub lire_entier {
	my ($message, $min, $max) = @_;
	while (1) {
		print $message;
		my $entree = <STDIN>;
		next unless defined $entree;
		chomp $entree;
		return int $entree if $entree =~ /^\d+$/ && $entree >= $min && $entree <= $max;
		print "\nVeuillez entrer un entier entre $min et $max.\n";
	}
}

## Chargement des options sauvegardées
#"""
# Lit le fichier d'options et retourne les valeurs si valides,
# sinon applique les valeurs par défaut.
#"""
sub charger_options {
	if (-e $FICHIER_OPTIONS) {
		open my $flux_lecture, '<', $FICHIER_OPTIONS or die $!;
		local $/;
		my $contenu = <$flux_lecture>;
		close $flux_lecture;
		my @donnees = map { int $_ } split /,/, $contenu;
		return @donnees if @donnees == 3;
	}
	return (1, 100, 5);
}

## Sauvegarde des options
#"""
# Écrit les options courantes dans le fichier de configuration
#"""
sub sauvegarder_options {
	my ($min, $max, $tours) = @_;
	open my $flux_ecriture, '>', $FICHIER_OPTIONS or die $!;
	print $flux_ecriture join ',', $min, $max, $tours;
	close $flux_ecriture;
}

# Fonctions principales
## Affiche le menu principal
#"""
# Affiche le menu principal du jeu
#"""
sub afficher_menu_principal {
	print "\n#### Menu ####\n1 : Jouer\n2 : Options\n3 : Quitter\n";
}

## Affiche le menu d'options
#"""
# Présente les paramètres actuels et propose les choix de modification
#"""
sub afficher_menu_options {
	my ($min, $max, $tours) = @_;
	print "\n#### Menu options ####\n1 : Choisir les limites (actuellement : $min - $max)\n2 : Nombre de tours max (actuellement : $tours)\n";
}

## Joue une partie
#"""
# Lance une partie de « plus ou moins »
#"""
sub jouer_partie {
	my ($min, $max, $tours) = @_;
	my $cible = int rand($max - $min + 1) + $min;
	print "\nTrouvez entre $min et $max en $tours tours\n";
	for my $num_tour (1 .. $tours) {
		my $choix = lire_entier("Tour $num_tour : ", $min, $max);
		if ($choix == $cible) {
			print "\nGagné !\n";
			return;
		} elsif ($choix > $cible) {
			print "-\n";
		} else {
			print "+\n";
		}
	}
	print "\nPerdu ! La réppnse était $cible.\n";
}

## Gère les options
#"""
# Permet de modifier et de sauvegarder les paramètres du jeu
#"""
sub gerer_options {
	my ($min, $max, $tours) = @_;
	afficher_menu_options($min, $max, $tours);
	my $choix_options = lire_entier("? = ", 1, 2);
	if ($choix_options == 1) {
		$min   = lire_entier("\nMin = ", 1, $max);
		$max   = lire_entier("\nMax = ", $min, 10_000);
	} elsif ($choix_options == 2) {
		$tours = lire_entier("\nTours max = ", 1, 100);
	}
	sauvegarder_options($min, $max, $tours);
	return ($min, $max, $tours);
}

# Main
#"""
# Point d'entrée principal du programme
#"""
sub main {
	my ($min, $max, $tours) = charger_options();
	while (1) {
		afficher_menu_principal();
		my $choix_menu = lire_entier("? = ", 1, 3);
		if    ($choix_menu == 1) { jouer_partie($min, $max, $tours);       }
		elsif ($choix_menu == 2) { ($min, $max, $tours) = gerer_options($min, $max, $tours); }
		else                     { last; }
	}
}

# Lancement du programme
main();

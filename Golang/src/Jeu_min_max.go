package main

// # Importations
import (
	"bufio"
	"fmt"
	"math/rand"
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"time"
)

// # Données
var	lecteur		= bufio.NewReader(os.Stdin)
var	motifEntier	= regexp.MustCompile(`^\d+$`)
var	fichierOptions	string

// # Fonctions utilitaires
// ## Lecture d'un argument de commande
/**
 Lecture sécurisée d'un entier depuis l'utilisateur
 Contrôle l'appartenance à un intervalle [minimum, maximum]
 @param	message		message à afficher
 @param	minimum		borne inférieure
 @param	maximum		borne supérieure
 @return	entier validé
*/
func	lireEntier(message string, minimum, maximum int) int {
	for {
		fmt.Print(message)
		entree, _ := lecteur.ReadString('\n')
		entree = strings.TrimSpace(entree)
		if motifEntier.MatchString(entree) {
			valeur, _ := strconv.Atoi(entree)
			if valeur >= minimum && valeur <= maximum {
				return valeur
			}
		}
		fmt.Println("\nVeuillez entrer un entier entre", minimum, "et", maximum, ".")
	}
}

// # Fonctions utilitaires principales
// ## Création d'une tâche
/**
 Charge les options sauvegardées depuis le fichier
 @return	tableau [min, max, tours]
*/
func	chargerOptions() [3]int {
	if fichierExiste(fichierOptions) {
		contenu, exceptionLecture := os.ReadFile(fichierOptions)
		if exceptionLecture == nil {
			segments := strings.Split(strings.TrimSpace(string(contenu)), ",")
			if len(segments) == 3 &&
				motifEntier.MatchString(segments[0]) &&
				motifEntier.MatchString(segments[1]) &&
				motifEntier.MatchString(segments[2]) {
				minimum, _ := strconv.Atoi(segments[0])
				maximum, _ := strconv.Atoi(segments[1])
				nombreTours, _ := strconv.Atoi(segments[2])
				return [3]int{minimum, maximum, nombreTours}
			}
		}
	}
	return [3]int{1, 100, 5}
}

/**
 Sauvegarde les options dans le fichier
 @param	minimum		borne inférieure
 @param	maximum		borne supérieure
 @param	nombreTours	nombre de tours
*/
func	sauvegarderOptions(minimum, maximum, nombreTours int) {
	contenu := fmt.Sprintf("%d,%d,%d", minimum, maximum, nombreTours)
	exceptionEcriture := os.WriteFile(fichierOptions, []byte(contenu), 0644)
	if exceptionEcriture != nil {
		fmt.Fprintln(os.Stderr, "Erreur de sauvegarde :", exceptionEcriture.Error())
	}
}

// ### Fonction d'existence de fichier
/**
 Vérifie l'existence d'un fichier
 @param	chemin	chemin du fichier
 @return	booléen d'existence
*/
func	fichierExiste(chemin string) bool {
	_, erreurStat := os.Stat(chemin)
	return erreurStat == nil
}

// # Fonctions principales
/**
 Affiche le menu principal
*/
func	afficherMenuPrincipal() {
	fmt.Println("\n#### Menu ####\n1 : Jouer\n2 : Options\n3 : Quitter")
}

/**
 Affiche le menu d'options
*/
func	afficherMenuOptions(minimum, maximum, nombreTours int) {
	fmt.Printf("\n#### Menu options ####\n1 : Choisir les limites (actuellement : %d - %d)\n2 : Nombre de tours max (actuellement : %d)\n",
		minimum, maximum, nombreTours)
}

/**
 Lance une partie et affiche + ou -
*/
func	jouerPartie(minimum, maximum, nombreTours int) {
	cible := minimum + rand.Intn(maximum-minimum+1)
	fmt.Println("\nTrouvez entre", minimum, "et", maximum, "en", nombreTours, "tours")
	for numeroEssai := 1; numeroEssai <= nombreTours; numeroEssai++ {
		choix := lireEntier(fmt.Sprintf("Tour %d : ", numeroEssai), minimum, maximum)
		if choix == cible {
			fmt.Println("\nGagné !")
			return
		}
		if choix > cible {
			fmt.Println("-")
		} else {
			fmt.Println("+")
		}
	}
	fmt.Println("\nPerdu ! La réppnse était", cible, ".")
}

/**
 Gère le menu Options et sauvegarde
 @return	tableau [min, max, tours]
*/
func	gererOptions(minimum, maximum, nombreTours int) [3]int {
	afficherMenuOptions(minimum, maximum, nombreTours)
	choixOptions := lireEntier("? = ", 1, 2)
	if choixOptions == 1 {
		minimum = lireEntier("\nMin = ", 1, maximum)
		maximum = lireEntier("\nMax = ", minimum, 10000)
	} else if choixOptions == 2 {
		nombreTours = lireEntier("\nTours max = ", 1, 100)
	}
	sauvegarderOptions(minimum, maximum, nombreTours)
	return [3]int{minimum, maximum, nombreTours}
}

// # Main
/**
 Boucle principale du programme
*/
func	main() {
	rand.Seed(time.Now().UnixNano())
	repertoireCourant, _ := os.Getwd()
	fichierOptions = filepath.Join(repertoireCourant, "data", "minmax_options.txt")
	options := chargerOptions()
	minimum, maximum, nombreTours := options[0], options[1], options[2]
	for {
		afficherMenuPrincipal()
		choixMenu := lireEntier("? = ", 1, 3)
		if choixMenu == 1 {
			jouerPartie(minimum, maximum, nombreTours)
		} else if choixMenu == 2 {
			nouvellesOptions := gererOptions(minimum, maximum, nombreTours)
			minimum, maximum, nombreTours = nouvellesOptions[0], nouvellesOptions[1], nouvellesOptions[2]
		} else if choixMenu == 3 {
			break
		}
	}
}

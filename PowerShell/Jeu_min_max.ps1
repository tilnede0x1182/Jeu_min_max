# Importations

# Données
$CHEMIN_DOSSIER		= Split-Path -Parent $MyInvocation.MyCommand.Definition
$FICHIER_OPTIONS	= Join-Path $CHEMIN_DOSSIER "minmax_options.txt"

# Fonctions utilitaires
## Lecture d'un argument de commande
<#
	Lit un entier sécurisé dans un intervalle
	Boucle jusqu'à entrée valide
	@message	Invite affichée
	@min		Borne inférieure
	@max		Borne supérieure
#>
function lire_entier {
	param(
		[string]$message,
		[int]$min,
		[int]$max
	)
	while ($true) {
		Write-Host -NoNewline $message
		$entree	= Read-Host
		if ($entree -match '^\d+$') {
			$valeur	= [int]$entree
			if ($valeur -ge $min -and $valeur -le $max) { return $valeur }
		}
		Write-Host ""
		Write-Host "Veuillez entrer un entier entre $min et $max."
	}
}

## Chargement des options sauvegardées
<#
	Charge les limites et le nombre de tours depuis le fichier
	Renvoie [min,max,tours] ou valeurs par défaut
#>
function charger_options {
	if (Test-Path $FICHIER_OPTIONS) {
		$données = (Get-Content $FICHIER_OPTIONS -Raw).Split(',') | ForEach-Object { [int]$_ }
		if ($données.Count -eq 3) { return $données }
	}
	return 1,100,5
}

## Sauvegarde des options
<#
	Enregistre les limites et le nombre de tours
#>
function sauvegarder_options {
	param(
		[int]$min,
		[int]$max,
		[int]$tours
	)
	"$min,$max,$tours" | Set-Content $FICHIER_OPTIONS
}

# Fonctions principales
<#
	Affiche le menu principal
#>
function afficher_menu_principal {
	Write-Host ""
	Write-Host "#### Menu ####"
	Write-Host "1 : Jouer"
	Write-Host "2 : Options"
	Write-Host "3 : Quitter"
}

<#
	Affiche le menu des options
#>
function afficher_menu_options {
	param(
		[int]$min,
		[int]$max,
		[int]$tours
	)
	Write-Host ""
	Write-Host "#### Menu options ####"
	Write-Host "1 : Choisir les limites (actuellement : $min - $max)"
	Write-Host "2 : Nombre de tours max (actuellement : $tours)"
}

<#
	Lance une partie et guide l'utilisateur
#>
function jouer_partie {
	param(
		[int]$min,
		[int]$max,
		[int]$tours
	)
	$cible	= Get-Random -Minimum $min -Maximum ($max + 1)
	Write-Host ""
	Write-Host "Trouvez entre $min et $max en $tours tours"
	for ($essaiNumero = 1; $essaiNumero -le $tours; $essaiNumero++) {
		$choix = lire_entier "Tour $essaiNumero : " $min $max
		if ($choix -eq $cible) {
			Write-Host ""
			Write-Host "Gagné !"
			return
		} elseif ($choix -gt $cible) {
			Write-Host "-"
		} else {
			Write-Host "+"
		}
	}
	Write-Host ""
	Write-Host "Perdu ! La réppnse était $cible."
}

<#
	Gère le sous-menu Options et persiste les choix
#>
function gerer_options {
	param(
		[int]$min,
		[int]$max,
		[int]$tours
	)
	afficher_menu_options $min $max $tours
	$choixOptions = lire_entier "? = " 1 2
	if ($choixOptions -eq 1) {
		$min	= lire_entier "`nMin = " 1 $max
		$max	= lire_entier "`nMax = " $min 10000
	} elseif ($choixOptions -eq 2) {
		$tours	= lire_entier "`nTours max = " 1 100
	}
	sauvegarder_options $min $max $tours
	return $min,$max,$tours
}

# Main
<#
	Point d'entrée
#>
function main {
	$min,$max,$tours = charger_options
	while ($true) {
		afficher_menu_principal
		$choixMenu = lire_entier "? = " 1 3
		if ($choixMenu -eq 3) { break }
		switch ($choixMenu) {
			1 { jouer_partie $min $max $tours }
			2 { $vars = gerer_options $min $max $tours; $min,$max,$tours = $vars }
		}
	}
}

# Lancement du programme
main

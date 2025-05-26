@echo off
:: # Lancement du programme, doit être fait au début.
goto :main

REM === UTILITAIRES AVEC VARIABLES DE SUIVI ===
:: /**
:: * Demande un entier à l'utilisateur, contrôle les bornes, redirige selon le contexte.
:: * @param %~1 prompt
:: * @param %~2 min
:: * @param %~3 max
:: * @param %~4 mode_retour
:: */
:lire_entier
	REM Paramètres: prompt, min, max, mode_retour
	set "promptTexte=%~1"
	set "minVal=%~2"
	set "maxVal=%~3"
	set "mode_retour=%~4"

:: /**
:: * Boucle de saisie pour lire_entier, recommence tant que la valeur n'est pas correcte.
:: */
:lire_entier_loop
	set "nonChiffre="
	set /p saisie=%promptTexte%
	if "%saisie%"=="" goto lire_entier_erreur
	for /f "delims=0123456789" %%C in ("%saisie%") do set nonChiffre=%%C
	if defined nonChiffre goto lire_entier_erreur
	set /a nombre=%saisie% 2>nul
	if %nombre% GEQ %minVal% if %nombre% LEQ %maxVal% (
		set lireEntierRet=%nombre%
		REM Traitement selon le mode de retour
		if "%mode_retour%"=="menu_principal" goto traiter_menu_principal
		if "%mode_retour%"=="menu_options" goto traiter_menu_options
		if "%mode_retour%"=="config_min" goto traiter_config_min
		if "%mode_retour%"=="config_max" goto traiter_config_max
		if "%mode_retour%"=="config_tours" goto traiter_config_tours
		if "%mode_retour%"=="jeu" goto traiter_jeu
		exit /b
	)

:: /**
:: * Message d'erreur et relance de la boucle de saisie dans lire_entier.
:: */
:lire_entier_erreur
	echo.
	echo Veuillez entrer un entier entre %minVal% et %maxVal%.
	echo.
	goto lire_entier_loop

REM === LECTURE ENTIER SANS LIMITE (pour config) ===

:: /**
:: * Demande un entier sans limite pour la configuration, puis redirige.
:: * @param %~1 prompt
:: * @param %~2 mode_retour
:: */
:lire_entier_libre
	echo.
	set "promptTexte=%~1"
	set "mode_retour=%~2"

:: /**
:: * Boucle de saisie pour lire_entier_libre, recommence tant que la valeur n'est pas correcte.
:: */
:lire_entier_libre_loop
	set "nonChiffre="
	set /p saisie=%promptTexte%
	if "%saisie%"=="" goto lire_entier_libre_erreur
	for /f "delims=0123456789" %%C in ("%saisie%") do set nonChiffre=%%C
	if defined nonChiffre goto lire_entier_libre_erreur
	set /a nombre=%saisie% 2>nul
	set lireEntierRet=%nombre%
	REM Traitement selon le mode de retour
	if "%mode_retour%"=="config_min" goto traiter_config_min
	if "%mode_retour%"=="config_max" goto traiter_config_max
	if "%mode_retour%"=="config_tours" goto traiter_config_tours
	exit /b

:: /**
:: * Message d'erreur et relance de la boucle de saisie dans lire_entier_libre.
:: */
:lire_entier_libre_erreur
	echo.
	echo Veuillez entrer un entier valide.
	echo.
	goto lire_entier_libre_loop

REM === FONCTION AVEC RÉAFFICHAGE MENU ===

:: /**
:: * Affiche un menu, puis demande un entier borné à l'utilisateur, redirige selon la saisie.
:: * @param %~1 menuFunction
:: * @param %~2 prompt
:: * @param %~3 min
:: * @param %~4 max
:: * @param %~5 param1
:: * @param %~6 param2
:: * @param %~7 param3
:: */
:lire_entier_avec_menu
	set "menuFunction=%~1"
	set "promptTexte=%~2"
	set "minVal=%~3"
	set "maxVal=%~4"
	set "param1=%~5"
	set "param2=%~6"
	set "param3=%~7"

:: /**
:: * Boucle de saisie pour lire_entier_avec_menu, réaffiche le menu à chaque essai incorrect.
:: */
:lemenu_loop
	call :%menuFunction% %param1% %param2% %param3%
	set "nonChiffre="
	set /p saisie=%promptTexte%
	if "%saisie%"=="" goto lemenu_mauvais
	for /f "delims=0123456789" %%C in ("%saisie%") do set nonChiffre=%%C
	if defined nonChiffre goto lemenu_mauvais
	set /a nombre=%saisie% 2>nul
	if %nombre% GEQ %minVal% if %nombre% LEQ %maxVal% (
		set lireEntierRet=%nombre%
		exit /b
	)

:: /**
:: * Message d'erreur et relance de la boucle de saisie dans lire_entier_avec_menu.
:: */
:lemenu_mauvais
	echo.
	echo Veuillez entrer un entier entre %minVal% et %maxVal%.
	echo.
	goto lemenu_loop

:: /**
:: * Affiche le menu principal du programme.
:: */
:afficher_menu_principal
	echo.
	echo #### Menu ####
	echo 1 : Jouer
	echo 2 : Options
	echo 3 : Quitter
	exit /b

:: /**
:: * Affiche le menu des options, incluant les valeurs actuelles.
:: * @param %1 min
:: * @param %2 max
:: * @param %3 tours
:: */
:afficher_menu_options
	echo.
	echo #### Menu options ####
	echo 1 : Limites (actuellement : %1 - %2)
	echo 2 : Nombre de tours max (actuellement : %3)
	exit /b

:: /**
:: * Gère la configuration : lecture ou écriture des valeurs dans le fichier.
:: * @param %~1 action ('lire' ou 'ecrire')
:: */
:gestion_config
	set "action=%~1"
	set "FICHIER_OPTIONS=%~dp0minmax_options.txt"
	
	if "%action%"=="lire" (
		REM Réinitialisation des variables
		set "min="
		set "max="
		set "tours="
		
		if exist "%FICHIER_OPTIONS%" (
			for /f "usebackq tokens=1-3 delims=," %%A in ("%FICHIER_OPTIONS%") do (
				set min=%%A & set max=%%B & set tours=%%C
			)
		)
		
		REM Valeurs par défaut si fichier inexistant ou variables vides
		if not defined min set min=1 & set max=100 & set tours=5
		
	) else if "%action%"=="ecrire" (
		(echo %min%,%max%,%tours%) > "%FICHIER_OPTIONS%"
	)
	exit /b

REM === TRAITEMENTS SELON CONTEXTE ===

:: /**
:: * Traite le choix du menu principal (jouer, options, quitter).
:: */
:traiter_menu_principal
	if "%lireEntierRet%"=="1" goto lancer_jeu
	if "%lireEntierRet%"=="2" goto menu_options_loop
	if "%lireEntierRet%"=="3" goto fin
	goto menu_principal_loop

:: /**
:: * Traite le choix du menu des options (limites, nombre de tours).
:: */
:traiter_menu_options
	if "%lireEntierRet%"=="1" goto configurer_limites
	if "%lireEntierRet%"=="2" goto configurer_tours_solo
	goto fin_options

:: /**
:: * Enregistre la nouvelle valeur minimale puis demande la valeur maximale.
:: */
:traiter_config_min
	set min=%lireEntierRet%
	call :lire_entier_libre "Max = " config_max
	exit /b

:: /**
:: * Vérifie que max > min, sinon relance la saisie, sinon retourne aux options.
:: */
:traiter_config_max
	REM Vérification que max > min
	if %lireEntierRet% LEQ %min% (
		echo.
		echo Le maximum doit etre superieur au minimum (%min%^).
		echo.
		call :lire_entier_libre "Max = " config_max
		exit /b
	)
	set max=%lireEntierRet%
	goto fin_options

:: /**
:: * Enregistre le nombre de tours maximum puis retourne aux options.
:: */
:traiter_config_tours
	set tours=%lireEntierRet%
	goto fin_options

:: /**
:: * Gère la logique d'un tour de jeu (comparaison, victoire, poursuite ou fin).
:: */
:traiter_jeu
	set choix=%lireEntierRet%
	setlocal EnableDelayedExpansion
	if !choix! EQU !cible! (
		echo. & echo Gagné^^ ^^! & endlocal & goto menu_principal_loop
	)
	if !choix! GTR !cible! (echo -) else (echo +)
	set /a jeu_tour+=1
	if !jeu_tour! GTR !tours! (
		echo. & echo Perdu^^ ^^! La reponse etait !cible!. & endlocal & goto menu_principal_loop
	)
	call :lire_entier "Tour !jeu_tour! : " !min! !max! jeu
	exit /b

REM === BOUCLES PRINCIPALES ===

:: /**
:: * Boucle principale du menu, attend et traite le choix de l'utilisateur.
:: */
:menu_principal_loop
	call :lire_entier_avec_menu afficher_menu_principal "? = " 1 3
	set choixMenu=%lireEntierRet%
	if "%choixMenu%"=="1" goto lancer_jeu
	if "%choixMenu%"=="2" goto menu_options_loop
	if "%choixMenu%"=="3" goto fin
	goto menu_principal_loop

:: /**
:: * Boucle du menu d'options, traite la navigation dans les options.
:: */
:menu_options_loop
	call :lire_entier_avec_menu afficher_menu_options "? = " 1 2 %min% %max% %tours%
	set choixOpt=%lireEntierRet%
	if "%choixOpt%"=="1" goto configurer_limites
	if "%choixOpt%"=="2" goto configurer_tours_solo
	goto fin_options

:: /**
:: * Lance la saisie des nouvelles limites min et max.
:: */
:configurer_limites
	call :lire_entier_libre "Min = " config_min
	exit /b

:: /**
:: * Lance la saisie du nombre maximal de tours.
:: */
:configurer_tours_solo
	call :lire_entier_libre "Tours max = " config_tours
	exit /b

:: /**
:: * Enregistre la configuration puis retourne au menu principal.
:: */
:fin_options
	call :gestion_config ecrire
	goto menu_principal_loop

:: /**
:: * Initialise la partie (tirage aléatoire, compteur de tours), puis lance la saisie.
:: */
:lancer_jeu
	setlocal EnableDelayedExpansion
	REM Conversion en entier pour éliminer les espaces
	set /a minClean=!min!
	set /a maxClean=!max!
	set /a toursClean=!tours!
	set /a intervalle=maxClean-minClean+1
	set /a cible=!RANDOM! %% !intervalle! + !minClean!
	set jeu_tour=1
	echo.
	echo Trouvez entre !minClean! et !maxClean! en !toursClean! tours
	call :lire_entier "Tour !jeu_tour! : " !min! !max! jeu
	exit /b
	
REM === PROGRAMME PRINCIPAL ===

:: /**
:: * Point d'entrée du programme, charge la config puis lance le menu principal.
:: */
:main
	call :gestion_config lire
	goto menu_principal_loop

:: /**
:: * Termine le programme proprement.
:: */
:fin
	exit /b